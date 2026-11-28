{-# LANGUAGE ScopedTypeVariables #-}

-- | Where a build runs, and how its directory comes to exist.
--
-- == Where
--
-- A build directory is under the store's state directory, in
-- @.nova-nix\/builds@, where upstream's has been by default since 2.30.0
-- (@stateDir + "\/builds"@ in @LocalBuildStoreConfig::getBuildDir@,
-- local-store.cc).  At 2.24.9 upstream roots it at @TMPDIR@, which on
-- Linux is usually @\/tmp@, where any local user can create the name a
-- build is about to take.  Whoever can write the state directory can
-- already rewrite the store's database there, so a build directory under
-- it is open to no one the store is not.  A store has its own state
-- directory, so builds into two stores never share one.
--
-- The directory is named by the hash of the derivation's first output,
-- whose path lock the build holds from before the directory is made until
-- after it is removed (@withOutputLocks@ in "Nix.Builder").  No two builds
-- into one store use one name at a time, so whatever is found at the name
-- was left by an earlier build that could not remove it, one that was
-- killed for instance, and is removed, or moved aside where it cannot be
-- ('clearLeftover').  Upstream instead names each directory
-- @nix-build-\<drv name\>-\<n\>@ and counts up until a name is free,
-- which leaves such a directory where it is.
--
-- == Creation
--
-- The directory is created by a call that fails when anything already
-- answers to the name, so a build never runs in a directory it did not
-- create.  It is open to the user nova-nix runs as alone
-- ('OwnerAccess'), so other users can neither read what the build
-- unpacks nor change it while it runs; a build that runs as a build user
-- gives the directory its builder works in to that user
-- ("Nix.Builder.BuildUser").  On POSIX it is created with mode
-- 0700, as upstream's @createTempDir@ makes its build directories
-- (file-system.cc).  On Windows it is created with a protected access control
-- list granting that user alone full control, which what the build makes in
-- it inherits ("Nix.Store.Access").
--
-- == When a build fails
--
-- A build's directory is removed when the build ends, however it ends,
-- unless the build did not succeed and @keep-failed@ is on ('KeepFailed'):
-- then it stays, and the directory the builder worked in is named to the
-- logger, as upstream keeps and names it.  A build that does not
-- succeed is one that stopped short of registering its outputs, or a
-- check that found one different, whether it stopped with a failure or an
-- exception, an interrupt included, as upstream keeps the directory of
-- every build that does not succeed.
--
-- Three things differ from upstream.  The directory keeps mode 0700, where
-- upstream opens it, and the directory its builder worked in, with 0755,
-- so that the user who asked for the build can read what a build user
-- wrote there: here the build runs as the user nova-nix runs as, who can
-- read it, or as a build user for root, who can read anything.  A kept directory
-- stays until the next build of the same derivation, which removes it as
-- it removes any directory an earlier build left at the name
-- ('clearLeftover'), where upstream's stays until it is removed by hand,
-- its name being one no later build takes.  The name is what makes a
-- rebuild see the paths the first build saw, which a check relies on
-- ("Nix.Builder"), so keeping a directory cannot be what moves the next
-- build elsewhere.  And on Windows a failed build's directory is kept too,
-- where upstream's Windows builds remove theirs whatever @keep-failed@
-- says.
module Nix.Builder.BuildDir
  ( -- * The directory
    buildDirFor,
    sandboxedBuildDir,
    WorkDir (..),
    workDirIn,
    FailedBuilds (..),
    BuildEnding (..),
    withBuildDir,
    clearLeftover,
    createBuildDir,
  )
where

import Control.Exception (IOException, catch, displayException, mask, onException, throwIO)
import qualified Data.Text as T
import Nix.Logger (Logger (..), logWarning)
import Nix.Store.Access (DirAccess (OwnerAccess), createDirectoryWith)
import Nix.Store.DB (metaDirName)
import Nix.Store.Path (StoreDir (..), StorePath (spHash))
import System.Directory (createDirectoryIfMissing, doesPathExist, pathIsSymbolicLink, removePathForcibly, renamePath)
import System.FilePath (takeDirectory, (</>))

-- | The directory under the store's state directory that holds every
-- build directory: upstream's name for its own under its state
-- directory.
buildsDirName :: FilePath
buildsDirName = "builds"

-- | Where a build of the derivation whose first output is this path runs.
buildDirFor :: StoreDir -> StorePath -> FilePath
buildDirFor dir firstOutput =
  unStoreDir dir </> metaDirName </> buildsDirName </> T.unpack (spHash firstOutput)

-- | Where in its build directory a sandboxed build's own directory is,
-- the one bound where the builder sees it, as upstream puts its @tmpDir@
-- at @topTmpDir + "/build"@ in a chroot build (@startBuilder@ in
-- local-derivation-goal.cc at 2.24.9).  The builder owns what it sees and can
-- open it to anyone; the build directory above it, which it cannot reach,
-- stays closed to every other user.
sandboxedBuildDir :: FilePath -> FilePath
sandboxedBuildDir dir = dir </> "build"

-- | Where in its build directory a build's builder works.
data WorkDir
  = -- | In the build directory itself, as an unsandboxed builder does and
    -- one in macOS's sandbox, which sees the directory where it is.
    WorksInBuildDir
  | -- | In the directory inside it that a sandboxed build on Linux shows
    -- the builder ('sandboxedBuildDir').
    WorksInSandboxedBuildDir
  deriving (Eq, Show)

-- | The directory a builder works in, given its build directory.
workDirIn :: WorkDir -> FilePath -> FilePath
workDirIn workDir dir = case workDir of
  WorksInBuildDir -> dir
  WorksInSandboxedBuildDir -> sandboxedBuildDir dir

-- | What becomes of a build's directory when the build does not succeed:
-- upstream's @keep-failed@.
data FailedBuilds
  = -- | It is removed, as it is when the build succeeds.
    RemoveFailed
  | -- | It is kept, and the directory the builder worked in is named to
    -- the logger.
    KeepFailed
  deriving (Eq, Show)

-- | How a build ended, which decides whether its directory may be kept.
data BuildEnding
  = -- | It registered its outputs, or its check found them the same.
    BuildSucceeded
  | -- | It did not.
    BuildFailed
  deriving (Eq, Show)

-- | Remove what an earlier build left at a path, or where it cannot be
-- removed, move it aside, to the first free name of the path with
-- @.leftover.@ and a number after it, beside it in the state directory,
-- and warn the logger, so the build goes on.  A build can leave
-- what its user cannot remove: a directory another user made in it,
-- through a descriptor a fixed-output builder handed out, which the user
-- may neither empty nor change, and which removal would otherwise trip on
-- at every build of the derivation, the name being the derivation's.
-- What cannot be moved either fails with the removal's error.
clearLeftover :: Logger -> FilePath -> IO ()
clearLeftover logger path = removePathForcibly path `catch` setAside
  where
    setAside (failure :: IOException) = do
      aside <- freeName (1 :: Int)
      renamePath path aside `catch` \(_ :: IOException) -> throwIO failure
      logWarning logger ("cannot remove '" <> T.pack path <> "', which an earlier build left (" <> T.pack (displayException failure) <> "); it is now '" <> T.pack aside <> "'")
    freeName number = do
      let candidate = path <> ".leftover." <> show number
      taken <- (||) <$> doesPathExist candidate <*> pathIsSymbolicLink candidate `catch` \(_ :: IOException) -> pure False
      if taken then freeName (number + 1) else pure candidate

-- | Run a build in its directory ('buildDirFor'), given the derivation's
-- first output, whose path lock the caller holds, where in the directory
-- its builder works, what becomes of the directory if the build fails, and
-- how the build's result says it ended.  What an earlier build left at the
-- name is removed, or moved aside ('clearLeftover'), and the directory is
-- created ('createBuildDir'), with the one the builder works in when that
-- is inside it, so the directory a failed build names is always there.
-- The build is given the build directory.  When it ends, however it ends,
-- the directory is removed, unless the build failed under 'KeepFailed';
-- an exception is a failure.  When the creation fails, whatever took the
-- name first is left as it is, since this build did not make it.
-- What it says goes to the logger.
withBuildDir :: Logger -> StoreDir -> StorePath -> WorkDir -> FailedBuilds -> (a -> BuildEnding) -> (FilePath -> IO a) -> IO a
withBuildDir logger storeDir firstOutput workDir failedBuilds ending build = do
  createDirectoryIfMissing True (takeDirectory dir)
  clearLeftover logger dir
  mask $ \restore -> do
    createBuildDir dir
    createWorkDir `onException` removeBuildDir dir
    result <- restore (build dir) `onException` leave BuildFailed
    leave (ending result)
    pure result
  where
    dir = buildDirFor storeDir firstOutput
    createWorkDir = case workDir of
      WorksInBuildDir -> pure ()
      WorksInSandboxedBuildDir -> createBuildDir (sandboxedBuildDir dir)
    leave BuildSucceeded = removeBuildDir dir
    leave BuildFailed = case failedBuilds of
      RemoveFailed -> removeBuildDir dir
      KeepFailed -> logMessage logger (keptMessage (workDirIn workDir dir))

-- | The note naming a kept build directory, in upstream's words, the path
-- in the double quotes upstream puts a path in.
keptMessage :: FilePath -> T.Text
keptMessage path = "note: keeping build directory \"" <> T.pack path <> "\""

-- | Create a build directory, open to the user nova-nix runs as alone
-- ('OwnerAccess'), failing with an
-- 'System.IO.Error.isAlreadyExistsError' exception when anything already
-- answers to the name, a symlink included, which is not followed.  On
-- POSIX its mode is 0700, narrowed by the umask.
createBuildDir :: FilePath -> IO ()
createBuildDir = createDirectoryWith OwnerAccess

-- | Remove a build directory, ignoring a filesystem failure to remove it:
-- it runs as the build ends, where a directory that will not go away
-- must not replace the build's own outcome, and the next build of the
-- derivation removes what is left.  'removePathForcibly' makes a
-- read-only directory writable before emptying it, as upstream's
-- @deletePath@ does, so a tree the builder copied read-only out of the
-- store goes too.
removeBuildDir :: FilePath -> IO ()
removeBuildDir dir = removePathForcibly dir `catch` ignore
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()
