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
-- unpacks nor change it while it runs.  On POSIX it is created with mode
-- 0700, as upstream's @createTempDir@ makes its build directories
-- (file-system.cc).  On Windows it is created with a protected access control
-- list granting that user alone full control, which what the build makes in
-- it inherits ("Nix.Store.Access").
module Nix.Builder.BuildDir
  ( -- * The directory
    buildDirFor,
    sandboxedBuildDir,
    withBuildDir,
    clearLeftover,
    createBuildDir,
  )
where

import Control.Exception (IOException, bracket_, catch, displayException, throwIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Store.Access (DirAccess (OwnerAccess), createDirectoryWith)
import Nix.Store.DB (metaDirName)
import Nix.Store.Path (StoreDir (..), StorePath (spHash))
import System.Directory (createDirectoryIfMissing, doesPathExist, pathIsSymbolicLink, removePathForcibly, renamePath)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

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

-- | Remove what an earlier build left at a path, or where it cannot be
-- removed, move it aside, to the first free name of the path with
-- @.leftover.@ and a number after it, beside it in the state directory,
-- and say so on standard error, so the build goes on.  A build can leave
-- what its user cannot remove: a directory another user made in it,
-- through a descriptor a fixed-output builder handed out, which the user
-- may neither empty nor change, and which removal would otherwise trip on
-- at every build of the derivation, the name being the derivation's.
-- What cannot be moved either fails with the removal's error.
clearLeftover :: FilePath -> IO ()
clearLeftover path = removePathForcibly path `catch` setAside
  where
    setAside (failure :: IOException) = do
      aside <- freeName (1 :: Int)
      renamePath path aside `catch` \(_ :: IOException) -> throwIO failure
      TIO.hPutStrLn stderr ("warning: cannot remove '" <> T.pack path <> "', which an earlier build left (" <> T.pack (displayException failure) <> "); it is now '" <> T.pack aside <> "'")
    freeName number = do
      let candidate = path <> ".leftover." <> show number
      taken <- (||) <$> doesPathExist candidate <*> pathIsSymbolicLink candidate `catch` \(_ :: IOException) -> pure False
      if taken then freeName (number + 1) else pure candidate

-- | Run a build in its directory ('buildDirFor'), given the derivation's
-- first output, whose path lock the caller holds: what an earlier build
-- left at the name is removed, or moved aside ('clearLeftover'), the
-- directory is created ('createBuildDir'), and it is removed when the
-- build ends, however it ends.  When the creation fails, whatever took the name first is left as
-- it is, since this build did not make it.
withBuildDir :: StoreDir -> StorePath -> (FilePath -> IO a) -> IO a
withBuildDir storeDir firstOutput build = do
  createDirectoryIfMissing True (takeDirectory dir)
  clearLeftover dir
  bracket_ (createBuildDir dir) (removeBuildDir dir) (build dir)
  where
    dir = buildDirFor storeDir firstOutput

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
