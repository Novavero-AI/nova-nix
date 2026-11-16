-- | The bytes of string and path values in the C data layer
-- (@cbits\/nn_str.c@).
--
-- A cell holding a string or a path, and a string with context, points at
-- an @nn_str_t@: a length and the bytes after it, made once and never
-- moved or written again until the arena is destroyed.  A cell takes a
-- copy of a new value's bytes ('cstrNew'), and shares the object of a
-- value read from another cell or of a literal in the compiled code
-- ("Nix.Eval.CBytecode"), which the value carries
-- ('Nix.Eval.Types.HeldBytes').  A read gives back a copy ('cstrBytes'),
-- so a t'ByteString' in a 'Nix.Eval.Types.NixValue' never points into C
-- memory.  Nothing is interned: the symbol table
-- ("Nix.Eval.Symbol") holds names alone, so a string value never takes a
-- symbol, and never moves the place of a name interned after it.
module Nix.Eval.CStr
  ( -- * Opaque handle
    NnStr,
    CStrPtr,

    -- * Lifecycle
    cstrInit,
    cstrDestroy,

    -- * Making
    cstrNew,
    cstrFromText,

    -- * Reading
    cstrBytes,
    cstrText,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import Data.Word (Word32)
import Foreign.C.Types (CChar, CSize (..))
import Foreign.Ptr (Ptr, nullPtr)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (cInvariantViolated, cStatusFailure)

-- | Phantom type for C-side @nn_str_t@.
data NnStr

-- | Pointer to a string's bytes in C.
type CStrPtr = Ptr NnStr

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_str_init"
  c_nn_str_init :: IO ()

foreign import ccall unsafe "nn_str_destroy"
  c_nn_str_destroy :: IO ()

foreign import ccall unsafe "nn_str_new"
  c_nn_str_new :: Ptr CChar -> CSize -> IO CStrPtr

foreign import ccall unsafe "nn_str_len"
  c_nn_str_len :: CStrPtr -> IO Word32

foreign import ccall unsafe "nn_str_bytes"
  c_nn_str_bytes :: CStrPtr -> IO (Ptr CChar)

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Start the value arena, ending a live one first.
cstrInit :: IO ()
cstrInit = c_nn_str_init

-- | End the value arena, freeing every string it made.
cstrDestroy :: IO ()
cstrDestroy = c_nn_str_destroy

-- ---------------------------------------------------------------------------
-- Making
-- ---------------------------------------------------------------------------

-- | Copy bytes into a new string.  'BSU.unsafeUseAsCStringLen' is safe
-- here: @nn_str_new@ copies the bytes and keeps no pointer to them.
cstrNew :: (MonadCData m) => ByteString -> m CStrPtr
cstrNew bytes =
  cdataIO . BSU.unsafeUseAsCStringLen bytes $ \(ptr, len) ->
    checkedStr =<< c_nn_str_new ptr (fromIntegral len)

-- | Copy text's UTF-8 bytes into a new string: a path, or a store path's
-- hash or name or an output name in a string's context.
cstrFromText :: (MonadCData m) => Text -> m CStrPtr
cstrFromText text =
  cdataIO . TF.withCStringLen text $ \(ptr, len) ->
    checkedStr =<< c_nn_str_new ptr (fromIntegral len)

-- | Reject the NULL @nn_str_new@ answers, outside the arena's window, for
-- a string longer than its uint32 length can say, or when the allocator
-- refuses a block.
checkedStr :: CStrPtr -> IO CStrPtr
checkedStr s
  | s == nullPtr = cStatusFailure "nn_str_new" "the string cannot be held (allocation failure, or longer than 4 GiB)"
  | otherwise = pure s

-- ---------------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------------

-- | A copy of a string's bytes.
cstrBytes :: (MonadCData m) => CStrPtr -> m ByteString
cstrBytes s = cdataIO $ do
  len <- c_nn_str_len s
  if len == 0
    then pure BS.empty
    else do
      bytes <- c_nn_str_bytes s
      BS.packCStringLen (bytes, fromIntegral len)

-- | The text of a string made from text ('cstrFromText').  Its bytes are
-- decoded where they lie and copied once, into the result, before the
-- action returns.  Bytes that are not UTF-8 are a string no text made,
-- which only a defect in this library stores where text is read back.
cstrText :: (MonadCData m) => CStrPtr -> m Text
cstrText s = cdataIO $ do
  len <- c_nn_str_len s
  if len == 0
    then pure T.empty
    else do
      bytes <- c_nn_str_bytes s
      view <- BSU.unsafePackCStringLen (bytes, fromIntegral len)
      case TE.decodeUtf8' view of
        Right text -> pure text
        Left _ -> cInvariantViolated "nn_str_bytes" "a string stored from text holds bytes that are not UTF-8"
