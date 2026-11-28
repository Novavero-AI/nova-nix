{-# LANGUAGE LambdaCase #-}

-- | A fetched tree, as a fetcher builds it before anything is written to
-- the store: every name in it, what each names, and where a file's bytes
-- are.  The tree is never extracted to a directory and copied, since a
-- volume that folds case would merge two names that differ only in case;
-- its NAR is streamed instead, once to give its hash and once into the
-- store, whose restore spells each name for its volume.
module Nix.Fetch.Tree
  ( -- * Trees
    Tree (..),
    dereferenceSingleton,

    -- * NARs
    NarPiece (..),
    narPieces,
    Blob (..),
    withTreeNarSource,
    treeNarHash,
  )
where

import Control.Exception (bracketOnError, finally, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR.Stream as Stream
import System.IO (Handle, IOMode (ReadMode), hClose, openBinaryFile)

-- ---------------------------------------------------------------------------
-- Trees
-- ---------------------------------------------------------------------------

-- | A tree whose files' bytes are where @blob@ says.  A directory's
-- entries are keyed by their names' bytes, which is the order a NAR
-- lists them in.
data Tree blob
  = -- | A regular file: whether it is executable, and its bytes.
    TreeFile !Bool !blob
  | TreeSymlink !ByteString
  | TreeDirectory !(Map ByteString (Tree blob))
  deriving (Eq, Show)

-- | The tree a fetched archive gives: the one directory at its top, when
-- it holds a directory and nothing else, as upstream's tarball cache
-- dereferences a singleton directory (@dereferenceSingletonDirectory@ in
-- git-utils.cc at 2.36.0), and otherwise the archive's own top.
dereferenceSingleton :: Tree blob -> Tree blob
dereferenceSingleton tree = case tree of
  TreeDirectory entries
    | [(_, inner@(TreeDirectory _))] <- Map.toList entries -> inner
  _ -> tree

-- ---------------------------------------------------------------------------
-- NARs
-- ---------------------------------------------------------------------------

-- | A piece of a tree's NAR: structural bytes, or a file's contents of a
-- size, framed by the pieces around them.
data NarPiece blob
  = NarBytes !ByteString
  | NarContents !Word64 !blob
  deriving (Eq, Show)

-- | A tree's NAR, as the pieces it is made of, with each file's size
-- read off its blob.  Consecutive structural bytes are one piece.
narPieces :: (blob -> Word64) -> Tree blob -> [NarPiece blob]
narPieces sizeOf tree = coalesce (bytesOf (narString Stream.tokMagic) : node tree [])
  where
    node current rest = case current of
      TreeFile executable blob ->
        let size = sizeOf blob
         in bytesOf
              ( open
                  <> narString Stream.tokRegular
                  <> (if executable then narString Stream.tokExecutable <> narString BS.empty else mempty)
                  <> narString Stream.tokContents
                  <> B.word64LE size
              )
              : NarContents size blob
              : bytesOf (padding size <> close)
              : rest
      TreeSymlink target ->
        bytesOf (open <> narString Stream.tokSymlink <> narString Stream.tokTarget <> narString target <> close) : rest
      TreeDirectory entries ->
        bytesOf (open <> narString Stream.tokDirectory)
          : Map.foldrWithKey entry (bytesOf close : rest) entries
    entry name sub rest =
      bytesOf (narString Stream.tokEntry <> narString Stream.tokLParen <> narString Stream.tokName <> narString name <> narString Stream.tokNode)
        : node sub (bytesOf close : rest)
    open = narString Stream.tokLParen <> narString Stream.tokType
    close = narString Stream.tokRParen
    padding size = B.byteString (BS.replicate (Stream.narPadOf size) 0)
    bytesOf = NarBytes . BL.toStrict . B.toLazyByteString
    coalesce pieces = case pieces of
      NarBytes first : NarBytes second : rest -> coalesce (NarBytes (first <> second) : rest)
      piece : rest -> piece : coalesce rest
      [] -> []

-- | A NAR wire string: its length, its bytes and its padding.
narString :: ByteString -> B.Builder
narString bytes =
  B.word64LE (fromIntegral (BS.length bytes))
    <> B.byteString bytes
    <> B.byteString (BS.replicate (Stream.narPad (BS.length bytes)) 0)

-- | A file a fetcher wrote, holding a fetched file's bytes, and its size.
data Blob = Blob
  { blobFile :: !FilePath,
    blobSize :: !Word64
  }
  deriving (Eq, Show)

-- | How far a NAR source has got: the pieces still to give, or a blob
-- open part way with the bytes it still owes.
data SourceState
  = SourcePieces ![NarPiece Blob]
  | SourceBlob !Handle !FilePath !Word64 ![NarPiece Blob]

-- | The bytes read from a blob at a time.
blobChunkBytes :: Int
blobChunkBytes = 131072

-- | A tree's NAR as a pull source of chunks, the empty chunk ending it,
-- with no blob's bytes held whole.  A blob holding fewer bytes than its
-- size fails the pull.  A blob still open when the continuation returns
-- is closed.  This source and its state are one deliberate boundary of
-- mutable state, as the store's own NAR sources are.
withTreeNarSource :: Tree Blob -> (IO ByteString -> IO a) -> IO a
withTreeNarSource tree consume = do
  stateRef <- newIORef (SourcePieces (narPieces blobSize tree))
  consume (pull stateRef) `finally` (closeHeld =<< readIORef stateRef)
  where
    closeHeld state = case state of
      SourceBlob handle _ _ _ -> hClose handle
      SourcePieces _ -> pure ()

-- | The next chunk of a NAR source.
pull :: IORef SourceState -> IO ByteString
pull stateRef =
  readIORef stateRef >>= \case
    SourcePieces [] -> pure BS.empty
    SourcePieces (NarBytes bytes : rest) -> bytes <$ writeIORef stateRef (SourcePieces rest)
    SourcePieces (NarContents 0 _ : rest) -> writeIORef stateRef (SourcePieces rest) >> pull stateRef
    -- The handle is the state's as soon as it is open, so the
    -- continuation's exit closes it whatever interrupts the pull.
    SourcePieces (NarContents size blob : rest) -> do
      bracketOnError (openBinaryFile (blobFile blob) ReadMode) hClose $ \handle ->
        writeIORef stateRef (SourceBlob handle (blobFile blob) size rest)
      pull stateRef
    SourceBlob handle file owed rest -> do
      chunk <- BS.hGet handle (fromIntegral (min owed (fromIntegral blobChunkBytes)))
      if BS.null chunk
        then throwIO (userError ("the fetched file " <> file <> " ends before its size"))
        else do
          let remaining = owed - fromIntegral (BS.length chunk)
          if remaining == 0
            then hClose handle >> writeIORef stateRef (SourcePieces rest)
            else writeIORef stateRef (SourceBlob handle file remaining rest)
          pure chunk

-- | A tree's NAR hash, its NAR streamed once.
treeNarHash :: Tree Blob -> IO Hash.NixHash
treeNarHash tree = withTreeNarSource tree (go Hash.hashInit)
  where
    go !ctx source = do
      chunk <- source
      if BS.null chunk
        then pure (Hash.hashFinalize ctx)
        else go (Hash.hashUpdate ctx chunk) source
