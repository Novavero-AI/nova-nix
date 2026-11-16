-- | Source positions as the evaluator keeps them: a t'PosIdx' stored beside
-- an attribute, read back as a file, line and column on demand.
--
-- A position is recorded when the construct it belongs to is compiled, as
-- three words in the bytecode data buffer: the file it is in, one past the
-- index of its path among the code's strings ('noFile' for text with no
-- file behind it), its line and its column.  The path is a string and not
-- a symbol, as upstream keeps a file in a position's origin and not in its
-- symbol table (@PosTable::Origin@, pos-table.hh:15-38 at 2.24.9).
-- Its index is one past the record's offset, so 0 is free to mean no
-- position, the bias upstream's @PosIdx@ carries for the same reason
-- (@noPos@ in pos-idx.hh, @PosTable::add@ in pos-table.hh, at 2.24.9).
-- The record lives as long as the bytecode that refers to it, and is
-- never written again.
module Nix.Eval.SourcePos
  ( -- * Indices
    PosIdx (..),
    noPos,

    -- * Recording
    PosRecorder,
    newPosRecorder,
    recordPos,

    -- * Reading back
    RecordedPos (..),
    lookupPos,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word32)
import Nix.Eval.CBytecode (cbcData, cbcEmitData, cbcEmitText, cbcStringText)
import Nix.Eval.CData (MonadCData)
import Nix.Expr.Types (SourceOrigin (..), SrcPos (..))

-- | Where something was defined: 'noPos', or the index of a recorded
-- position.
newtype PosIdx = PosIdx {unPosIdx :: Word32}
  deriving (Eq, Show)

-- | Defined nowhere in source: a builtin's attribute, or one a builtin
-- made.
noPos :: PosIdx
noPos = PosIdx 0

-- | The file word of a position in text with no file.  A file's word is
-- one past its path's string index, so it cannot be mistaken for one.
noFile :: Word32
noFile = 0

-- | Records the positions of one compilation.  It remembers the last
-- origin it recorded and that origin's file word, because the positions
-- of one parse share their origin, and storing the path again for each
-- one would copy it as many times.
newtype PosRecorder = PosRecorder (IORef (Maybe SeenOrigin))

-- | An origin and its file word.
data SeenOrigin = SeenOrigin !SourceOrigin !Word32

-- | A recorder that has seen no origin yet.
newPosRecorder :: IO PosRecorder
newPosRecorder = PosRecorder <$> newIORef Nothing

-- | Record a position for the evaluator to read back.  A line or column
-- past 32 bits records 'noPos', as upstream's position table cannot
-- address one either.
recordPos :: PosRecorder -> SrcPos -> IO PosIdx
recordPos (PosRecorder seen) (SrcPos origin line column) = case (toWord32 line, toWord32 column) of
  (Just lineWord, Just columnWord) -> do
    file <- fileWord
    offset <- cbcEmitData file
    _ <- cbcEmitData lineWord
    _ <- cbcEmitData columnWord
    pure (PosIdx (offset + 1))
  _ -> pure noPos
  where
    fileWord = do
      remembered <- readIORef seen
      case remembered of
        Just (SeenOrigin known word) | known == origin -> pure word
        _ -> do
          word <- storedFile origin
          writeIORef seen (Just (SeenOrigin origin word))
          pure word
    storedFile (SourceFile path) = (+ 1) <$> cbcEmitText path
    storedFile (SourceText _) = pure noFile
    toWord32 n
      | n >= 0 && toInteger n <= toInteger (maxBound :: Word32) = Just (fromIntegral n)
      | otherwise = Nothing

-- | A recorded position, read back.
data RecordedPos
  = -- | A line and column in the file at this path.
    PosInFile !Text !Int !Int
  | -- | A line and column in text with no file behind it.
    PosInText !Int !Int
  deriving (Eq, Show)

-- | Read a position back, or 'Nothing' for 'noPos'.
lookupPos :: (MonadCData m) => PosIdx -> m (Maybe RecordedPos)
lookupPos (PosIdx 0) = pure Nothing
lookupPos (PosIdx idx) = do
  let offset = idx - 1
  file <- cbcData offset
  line <- fromIntegral <$> cbcData (offset + 1)
  column <- fromIntegral <$> cbcData (offset + 2)
  Just
    <$> if file == noFile
      then pure (PosInText line column)
      else (\path -> PosInFile path line column) <$> cbcStringText (file - 1)
