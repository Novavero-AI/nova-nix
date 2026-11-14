{-# LANGUAGE CPP #-}

-- | Where a build runs, and how its directory comes to exist.
--
-- == Where
--
-- A build directory is under the store's state directory, in
-- @.nova-nix\/builds@, where upstream's has been by default since 2.30.0
-- (@stateDir + "\/builds"@ in @LocalBuildStoreConfig::getBuildDir@,
-- local-store.cc:80-88).  At 2.24.9 upstream roots it at @TMPDIR@, which
-- on Linux is usually @\/tmp@, where any local user can create the name a
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
-- killed for instance, and is removed.  Upstream
-- instead names each directory @nix-build-\<drv name\>-\<n\>@ and counts
-- up until a name is free, which leaves such a directory where it is.
--
-- == Creation
--
-- The directory is created by a call that fails when anything already
-- answers to the name, so a build never runs in a directory it did not
-- create.  On POSIX it is created with mode 0700, so other users cannot
-- read what the build unpacks: upstream's @createTempDir@ makes its build
-- directories the same way (file-system.cc:499-531 at 2.24.9).  On
-- Windows it takes the access control list it inherits from the store
-- directory.
module Nix.Builder.BuildDir
  ( -- * The directory
    buildDirFor,
    withBuildDir,
    createBuildDir,
  )
where

import Control.Exception (IOException, bracket_, catch)
import qualified Data.Text as T
import Nix.Store.DB (metaDirName)
import Nix.Store.Path (StoreDir (..), StorePath (spHash))
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
#if defined(mingw32_HOST_OS)
import System.Directory (createDirectory)
#else
import System.Posix.Directory (createDirectory)
import System.Posix.Files (ownerModes)
#endif

-- | The directory under the store's state directory that holds every
-- build directory: upstream's name for its own under its state
-- directory.
buildsDirName :: FilePath
buildsDirName = "builds"

-- | Where a build of the derivation whose first output is this path runs.
buildDirFor :: StoreDir -> StorePath -> FilePath
buildDirFor dir firstOutput =
  unStoreDir dir </> metaDirName </> buildsDirName </> T.unpack (spHash firstOutput)

-- | Run a build in its directory ('buildDirFor'), given the derivation's
-- first output, whose path lock the caller holds: what an earlier build
-- left at the name is removed, the directory is created
-- ('createBuildDir'), and it is removed when the build ends, however it
-- ends.  When the creation fails, whatever took the name first is left as
-- it is, since this build did not make it.
withBuildDir :: StoreDir -> StorePath -> (FilePath -> IO a) -> IO a
withBuildDir storeDir firstOutput build = do
  createDirectoryIfMissing True (takeDirectory dir)
  removePathForcibly dir
  bracket_ (createBuildDir dir) (removeBuildDir dir) (build dir)
  where
    dir = buildDirFor storeDir firstOutput

-- | Create a build directory, failing with an
-- 'System.IO.Error.isAlreadyExistsError' exception when anything already
-- answers to the name, a symlink included, which is not followed.  On
-- POSIX its mode is 0700, narrowed by the umask.
createBuildDir :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createBuildDir = createDirectory
#else
createBuildDir dir = createDirectory dir ownerModes
#endif

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
