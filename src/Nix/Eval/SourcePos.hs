-- | Source positions as the evaluator keeps them: a 'PosIdx' stored beside
-- an attribute, read back as a file, line and column on demand.
--
-- A position is recorded when the construct it belongs to is compiled, as
-- three words in the bytecode data buffer: the interned path of the file it
-- is in ('noFile' for text with no file behind it), its line and its column.
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
import Nix.Eval.CBytecode (cbcData, cbcEmitData)
import Nix.Eval.Symbol (Symbol (..), symbolIntern, symbolText)
import Nix.Expr.Types (SourceOrigin (..), SrcPos (..))
import System.IO.Unsafe (unsafePerformIO)

-- | Where something was defined: 'noPos', or the index of a recorded
-- position.
newtype PosIdx = PosIdx {unPosIdx :: Word32}
  deriving (Eq, Show)

-- | Defined nowhere in source: a builtin's attribute, or one a builtin
-- made.
noPos :: PosIdx
noPos = PosIdx 0

-- | The file word of a position in text with no file.  No symbol is 0
-- (@NN_SYMBOL_INVALID@), so it cannot be mistaken for a path.
noFile :: Word32
noFile = 0

-- | Records the positions of one compilation.  It remembers the last
-- origin it recorded and that origin's file word, because the positions
-- of one parse share their origin, and interning the path again for each
-- one would copy and hash it as many times.
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
          word <- internedFile origin
          writeIORef seen (Just (SeenOrigin origin word))
          pure word
    internedFile (SourceFile path) = unSymbol <$> symbolIntern path
    internedFile (SourceText _) = pure noFile
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

-- | Read a position back, or 'Nothing' for 'noPos'.  'unsafePerformIO' is
-- safe here: the record was written once, when its index was made, and
-- no later write touches it, so the read answers the same every time.
lookupPos :: PosIdx -> Maybe RecordedPos
lookupPos (PosIdx 0) = Nothing
lookupPos (PosIdx idx) = Just $ unsafePerformIO $ do
  let offset = idx - 1
  file <- cbcData offset
  line <- fromIntegral <$> cbcData (offset + 1)
  column <- fromIntegral <$> cbcData (offset + 2)
  pure $
    if file == noFile
      then PosInText line column
      else PosInFile (symbolText (Symbol file)) line column
