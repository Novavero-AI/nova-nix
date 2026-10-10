{-# LANGUAGE ScopedTypeVariables #-}

-- | Symlink-aware filesystem primitives shared by the store walks.
--
-- Two things every walk over store content needs and nothing in
-- @directory@ gives directly: classifying a node WITHOUT following a
-- link, and creating a link whose Windows flavour (file or directory)
-- is read off the target.  They live in a leaf module so the collector's
-- root walk and the out-link creation ("Nix.Store.GC") share them with
-- the materializers in "Nix.Store" without importing either.
module Nix.Store.Symlink
  ( -- * Classification
    WalkNode (..),
    classifyWalkNode,

    -- * Creation
    createSymlinkOfKind,
  )
where

import Control.Exception (IOException, catch, try)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Store.Exclusive (symlinkTakenMessage)
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isAlreadyExistsError)

-- | A node kind for store walks, classified WITHOUT following symlinks:
-- the link test runs first because 'Dir.doesDirectoryExist' and
-- 'Dir.doesFileExist' follow links and would report a link as its target.
-- A dangling link still classifies as 'WalkSymlink'; a probe failure
-- classifies as 'WalkAbsent' rather than throwing mid-walk.
data WalkNode = WalkSymlink | WalkDirectory | WalkRegular | WalkAbsent
  deriving (Eq, Show)

-- | Classify one path for a store walk.  The walks dispatch on this (or
-- run the same link-first probe order) so no store walk follows a
-- symlink: following one reads or mutates content outside the tree
-- being walked, and does not terminate on a link cycle.
classifyWalkNode :: FilePath -> IO WalkNode
classifyWalkNode path = do
  isLink <- Dir.pathIsSymbolicLink path `catch` \(_ :: IOException) -> pure False
  if isLink
    then pure WalkSymlink
    else do
      isDir <- Dir.doesDirectoryExist path
      if isDir
        then pure WalkDirectory
        else do
          isFile <- Dir.doesFileExist path
          pure (if isFile then WalkRegular else WalkAbsent)

-- | Create one symlink, choosing the Windows flavour from the target's
-- kind: the target is resolved relative to the link's directory (an
-- absolute target resolves to itself) and probed once.  A creation
-- failure is loud rather than approximated: writing the target text as a
-- regular file once registered a tree whose NAR hash differed from the
-- signed narinfo's, silent store corruption that a later push refused to
-- publish.  A name already taken is refused in upstream's words, as a
-- folded sibling or a raced root meets it.
-- The parent directory is not created here; a caller that wants that
-- does it first.
createSymlinkOfKind :: FilePath -> FilePath -> IO (Either Text ())
createSymlinkOfKind linkPath target = do
  targetIsDir <- Dir.doesDirectoryExist (takeDirectory linkPath </> target)
  result <-
    try $
      if targetIsDir
        then Dir.createDirectoryLink target linkPath
        else Dir.createFileLink target linkPath
  pure $ case result of
    Right () -> Right ()
    Left (e :: IOException)
      | isAlreadyExistsError e -> Left (symlinkTakenMessage linkPath target)
      | otherwise ->
          Left
            ( "cannot create symlink "
                <> T.pack linkPath
                <> " -> "
                <> T.pack target
                <> ": "
                <> T.pack (show e)
                <> " (on Windows this needs Developer Mode or elevation)"
            )
