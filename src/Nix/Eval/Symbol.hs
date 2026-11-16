-- | Interned names backed by a C hash table.
--
-- Attribute names are the most repeated data in Nix evaluation.
-- This module interns them via a C-side hash table ('cbits/nn_symbol.c'),
-- replacing O(n) string comparison with O(1) integer comparison.
--
-- The table holds names alone: attribute names, formals, variables and
-- the names interned at startup, as upstream's symbol table does
-- (@SymbolTable@, symbol-table.hh at 2.24.9).  A name's place is the order
-- it was interned in, which decides the order of a set's attributes, so
-- a string value is never interned: its bytes live in "Nix.Eval.CStr".
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
    invalidSymbolId,

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
import Nix.Eval.CData (MonadCData, cdataIO, readImmutable)
import Nix.Eval.CStatus (cStatusFailure)

-- | An interned symbol - a 'Word32' index into the global symbol table.
-- Two symbols are equal iff their indices are equal (O(1) comparison).
-- 0 is the invalid sentinel.
newtype Symbol = Symbol {unSymbol :: Word32}
  deriving (Eq, Ord, Show)

-- | @NN_SYMBOL_INVALID@: the interner's failure sentinel, and the index
-- no symbol ever has.  Must stay in lockstep with @cbits\/nn_symbol.h@.
invalidSymbolId :: Word32
invalidSymbolId = 0

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

-- | Initialize the global symbol table, destroying a live one first.
-- Call once before evaluation.  @capacity@ is a hint for the expected
-- number of unique symbols.  Pass 0 to use the default (4096).
symbolInit :: Word32 -> IO ()
symbolInit = c_nn_symbol_init

-- | Destroy the global symbol table, freeing all C-side memory.
-- All t'Symbol' values become invalid after this call.
symbolDestroy :: IO ()
symbolDestroy = c_nn_symbol_destroy

-- ---------------------------------------------------------------------------
-- Core operations
-- ---------------------------------------------------------------------------

-- | Intern a name, returning its t'Symbol'.
-- If the name was already interned, returns the existing symbol.  The
-- table stores the name's UTF-8 bytes.
symbolIntern :: (MonadCData m) => Text -> m Symbol
symbolIntern txt =
  cdataIO . TF.withCStringLen txt $ \(ptr, len) ->
    checkedSymbol =<< c_nn_symbol_intern ptr (fromIntegral len)

-- | Reject the interner's failure sentinel before it can become a
-- t'Symbol': wrapped it would read back as the empty string.  The table
-- answers it outside its init .. destroy window, and inside it for a
-- string it cannot hold: one that would take the string arena past its
-- 4 GiB of uint32 offsets, or a growth the allocator refuses.
checkedSymbol :: Word32 -> IO Symbol
checkedSymbol sid
  | sid == invalidSymbolId = cStatusFailure "nn_symbol_intern" "the symbol table cannot hold the string (allocation failure or its 4 GiB arena limit)"
  | otherwise = pure (Symbol sid)

-- The reads below are pure ('readImmutable'): a symbol's bytes do not
-- change once interned, until 'symbolDestroy'.  They can move, since the
-- table grows its byte storage with @realloc@ as later strings are
-- interned, and a read takes their address before it copies them, so no
-- intern may run between the two: the layer is used from one thread at a
-- time.

-- | Retrieve the text of an interned symbol.
-- Returns the original string.  The result is safe to use - it copies
-- from the C arena into a fresh 'Text'.
symbolText :: Symbol -> Text
symbolText (Symbol sid)
  | sid == invalidSymbolId = T.empty
  | otherwise = readImmutable . cdataIO $ do
      ptr <- c_nn_symbol_text sid
      if ptr == nullPtr
        then pure T.empty
        else do
          len <- c_nn_symbol_len sid
          TF.peekCStringLen (ptr, fromIntegral len)

-- | Byte length of a symbol's string.
symbolLen :: Symbol -> Int
symbolLen (Symbol sid) = fromIntegral (readImmutable (cdataIO (c_nn_symbol_len sid)))

-- ---------------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------------

-- | Number of unique symbols currently interned.
symbolCount :: IO Word32
symbolCount = c_nn_symbol_count
