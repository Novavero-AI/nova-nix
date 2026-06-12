-- | Interned string symbols backed by a C hash table.
--
-- Attribute names are the most repeated data in Nix evaluation.
-- This module interns them via a C-side hash table ('cbits/nn_symbol.c'),
-- replacing O(n) string comparison with O(1) integer comparison.
--
-- @
-- symbolInit 8192
-- sym <- symbolIntern "name"
-- symbolText sym   -- "name"
-- symbolInit destroys and re-creates; call once per evaluation.
-- @
module Nix.Eval.Symbol
  ( -- * Symbol type
    Symbol (..),

    -- * Lifecycle
    symbolInit,
    symbolDestroy,

    -- * Core operations
    symbolIntern,
    symbolText,
    symbolLen,

    -- * Diagnostics
    symbolCount,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import Data.Word (Word32)
import Foreign.C.Types (CChar, CSize (..))
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

-- | An interned symbol - a 'Word32' index into the global symbol table.
-- Two symbols are equal iff their indices are equal (O(1) comparison).
-- 0 is the invalid sentinel.
newtype Symbol = Symbol {unSymbol :: Word32}
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- FFI imports (unsafe - these never call back to Haskell)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_symbol_init"
  c_nn_symbol_init :: Word32 -> IO ()

foreign import ccall unsafe "nn_symbol_destroy"
  c_nn_symbol_destroy :: IO ()

foreign import ccall unsafe "nn_symbol_intern"
  c_nn_symbol_intern :: Ptr CChar -> CSize -> IO Word32

foreign import ccall unsafe "nn_symbol_text"
  c_nn_symbol_text :: Word32 -> IO (Ptr CChar)

foreign import ccall unsafe "nn_symbol_len"
  c_nn_symbol_len :: Word32 -> IO CSize

foreign import ccall unsafe "nn_symbol_count"
  c_nn_symbol_count :: IO Word32

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize the global symbol table.  Call once before evaluation.
-- @capacity@ is a hint for the expected number of unique symbols.
-- Pass 0 to use the default (4096).
symbolInit :: Word32 -> IO ()
symbolInit = c_nn_symbol_init

-- | Destroy the global symbol table, freeing all C-side memory.
-- All 'Symbol' values become invalid after this call.
symbolDestroy :: IO ()
symbolDestroy = c_nn_symbol_destroy

-- ---------------------------------------------------------------------------
-- Core operations
-- ---------------------------------------------------------------------------

-- | Intern a 'Text' value, returning its 'Symbol'.
-- If the string was already interned, returns the existing symbol.
-- This is the canonical entry point for Text to C conversion.
symbolIntern :: Text -> IO Symbol
symbolIntern txt =
  TF.withCStringLen txt $ \(ptr, len) -> do
    sid <- c_nn_symbol_intern ptr (fromIntegral len)
    pure (Symbol sid)

-- | Retrieve the text of an interned symbol.
-- Returns the original string.  The result is safe to use - it copies
-- from the C arena into a fresh 'Text'.
symbolText :: Symbol -> Text
symbolText (Symbol sid)
  | sid == 0 = T.empty
  | otherwise = unsafePerformIO $ do
      ptr <- c_nn_symbol_text sid
      if ptr == nullPtr
        then pure T.empty
        else do
          len <- c_nn_symbol_len sid
          TF.peekCStringLen (ptr, fromIntegral len)

-- | Byte length of a symbol's string.
symbolLen :: Symbol -> Int
symbolLen (Symbol sid) = fromIntegral (unsafePerformIO (c_nn_symbol_len sid))

-- ---------------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------------

-- | Number of unique symbols currently interned.
symbolCount :: IO Word32
symbolCount = c_nn_symbol_count
