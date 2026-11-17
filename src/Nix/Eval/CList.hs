-- | C-backed contiguous list of thunk pointers via FFI.
--
-- Wraps @cbits/nn_list.c@: a flat array of @nn_thunk_t*@ pointers, 8
-- bytes an element against the ~48 of the cons cells of a @[Thunk]@.
--
-- Lists are immutable after construction: 'clistFromThunks' builds one
-- and 'clistThunks' reads its items.  The headers are freed in bulk at
-- arena teardown, and the items arrays with the env pages that hold them.
--
-- 'nullPtr' is the empty list, so the C constructors report failure
-- through a status of their own rather than a NULL, and every one is
-- checked here: a list the C side cannot allocate raises
-- 'Nix.Eval.CStatus.CStatusError', never reads back as empty.
module Nix.Eval.CList
  ( -- * Opaque handle
    NnList,
    CListPtr,

    -- * CList newtype
    CList (..),
    emptyCList,
    clistFromThunks,
    clistThunks,
    clistLen,
    clistIndex,
    clistDrop,

    -- * Conversion
    thunkListToCList,
  )
where

import Control.Monad (unless)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (cStatusFailure)
import Nix.Eval.CThunk (CThunkPtr)

-- | Phantom type for C-side @nn_list_t@.
data NnList

-- | Pointer to a C-allocated list.
type CListPtr = Ptr NnList

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_list_new"
  c_nn_list_new :: Ptr CListPtr -> Word32 -> IO CInt

foreign import ccall unsafe "nn_list_count"
  c_nn_list_count :: CListPtr -> IO Word32

foreign import ccall unsafe "nn_list_get"
  c_nn_list_get :: CListPtr -> Word32 -> IO CThunkPtr

foreign import ccall unsafe "nn_list_set"
  c_nn_list_set :: CListPtr -> Word32 -> CThunkPtr -> IO ()

foreign import ccall unsafe "nn_list_drop"
  c_nn_list_drop :: Ptr CListPtr -> CListPtr -> Word32 -> IO CInt

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a new list with space for @count@ thunk pointers, or
-- 'nullPtr' (the empty list) when count is 0.  Raises
-- 'Nix.Eval.CStatus.CStatusError' when the C side cannot allocate it.
clistNew :: (MonadCData m) => Word32 -> m CListPtr
clistNew count = cdataIO $ alloca $ \out -> do
  checkedListStatus "nn_list_new" =<< c_nn_list_new out count
  peek out

-- | Raise the 'Nix.Eval.CStatus.CStatusError' for a list constructor's
-- failure status.
checkedListStatus :: String -> CInt -> IO ()
checkedListStatus site status =
  unless (status == 0) (cStatusFailure site "list allocation failed")

-- ---------------------------------------------------------------------------
-- Access
-- ---------------------------------------------------------------------------

-- | Number of elements in the list.
clistCount :: (MonadCData m) => CListPtr -> m Word32
clistCount = cdataIO . c_nn_list_count

-- | Get the thunk pointer at the given index.
clistGet :: (MonadCData m) => CListPtr -> Word32 -> m CThunkPtr
clistGet list idx = cdataIO (c_nn_list_get list idx)

-- | Set the thunk pointer at the given index (for construction).
clistSet :: (MonadCData m) => CListPtr -> Word32 -> CThunkPtr -> m ()
clistSet list idx ptr = cdataIO (c_nn_list_set list idx ptr)

-- ---------------------------------------------------------------------------
-- Conversion helpers
-- ---------------------------------------------------------------------------

-- | Convert a Haskell list of 'CThunkPtr' to a C list.
-- Allocates a new nn_list_t and fills it with the thunk pointers.
-- Returns 'nullPtr' for empty lists.  A list longer than the C count
-- can hold is refused rather than wrapped to a shorter allocation that
-- the fill would then run past.
thunkListToCList :: (MonadCData m) => [CThunkPtr] -> m CListPtr
thunkListToCList [] = pure nullPtr
thunkListToCList ptrs
  | len > fromIntegral (maxBound :: Word32) =
      cdataIO (cStatusFailure "nn_list_new" ("a list of " ++ show len ++ " elements is past the uint32_t count a C list holds"))
  | otherwise = do
      clist <- clistNew (fromIntegral len)
      fillList clist 0 ptrs
      pure clist
  where
    len = length ptrs
    fillList _ _ [] = pure ()
    fillList cl !i (p : ps) = do
      clistSet cl i p
      fillList cl (i + 1) ps

-- | Convert a C list back to a Haskell list of 'CThunkPtr'.
-- Returns @[]@ for 'nullPtr'.
clistToThunkList :: (MonadCData m) => CListPtr -> m [CThunkPtr]
clistToThunkList cl
  | cl == nullPtr = pure []
  | otherwise = do
      n <- clistCount cl
      mapM (clistGet cl) [0 .. n - 1]

-- ---------------------------------------------------------------------------
-- CList newtype (wraps CListPtr for use in NixValue ADT)
-- ---------------------------------------------------------------------------

-- | A C list, the payload of @VList@ in t'Nix.Eval.Types.NixValue': an
-- @nn_list_t@ that lives until the evaluation session ends, or null for
-- the empty list.
--
-- Two lists are equal when they are one C list, and 'show' gives its
-- address.  Neither reads the list; 'clistThunks' reads its items.
newtype CList = CList {unCList :: CListPtr}
  deriving (Eq, Show)

-- | The empty CList (null pointer, zero elements).
emptyCList :: CList
emptyCList = CList nullPtr

-- | Convert a Haskell list of 'CThunkPtr' to a t'CList'.
clistFromThunks :: (MonadCData m) => [CThunkPtr] -> m CList
clistFromThunks ptrs = CList <$> thunkListToCList ptrs

-- | Convert a t'CList' back to a Haskell list of 'CThunkPtr'.
clistThunks :: (MonadCData m) => CList -> m [CThunkPtr]
clistThunks (CList p) = clistToThunkList p

-- | Number of elements in the list.
clistLen :: (MonadCData m) => CList -> m Int
clistLen (CList p)
  | p == nullPtr = pure 0
  | otherwise = fromIntegral <$> clistCount p

-- | The element at an index, or 'Nothing' outside the list, read from the
-- C array without materializing the list.
clistIndex :: (MonadCData m) => CList -> Int -> m (Maybe CThunkPtr)
clistIndex cl@(CList p) i = do
  len <- clistLen cl
  if i < 0 || i >= len
    then pure Nothing
    else Just <$> clistGet p (fromIntegral i)

-- | The list without its first @n@ elements, as 'drop' is on lists.  The
-- result is a new header over this list's C array (@nn_list_drop@), so it
-- costs the same whatever the length; sharing the array cannot be
-- observed, because a list is never written after construction.
-- @nn_list_drop@ answers the empty list for a count at or past the
-- length, so a count past its @uint32_t@ is clamped to the largest one,
-- at or past every length.
clistDrop :: (MonadCData m) => Int -> CList -> m CList
clistDrop n cl@(CList p)
  | n <= 0 = pure cl
  | otherwise = CList <$> cdataIO (alloca dropInto)
  where
    count = fromIntegral (min n countCeiling)
    countCeiling = fromIntegral (maxBound :: Word32)
    dropInto out = do
      checkedListStatus "nn_list_drop" =<< c_nn_list_drop out p count
      peek out
