-- | C-backed contiguous list of thunk pointers via FFI.
--
-- Wraps @cbits/nn_list.c@ - a flat array of @nn_thunk_t*@ pointers.
-- Replaces Haskell's @[Thunk]@ (linked-list cons cells, ~48 bytes per
-- element) with a contiguous C array (8 bytes per element).
--
-- Lists are immutable after construction: allocate with 'clistNew',
-- fill with 'clistSet', then read with 'clistGet'/'clistCount'.
-- All memory is freed in bulk via 'clistFreeAll' at evaluation end.
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

    -- * Lifecycle
    clistNew,
    clistFreeAll,

    -- * Access
    clistCount,
    clistGet,
    clistSet,

    -- * Conversion
    thunkListToCList,
    clistToThunkList,
  )
where

import Control.Monad (unless)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Nix.Eval.CStatus (cStatusFailure)
import Nix.Eval.CThunk (CThunkPtr)
import System.IO.Unsafe (unsafePerformIO)

-- | Phantom type for C-side @nn_list_t@.
data NnList

-- | Pointer to a C-allocated list.
type CListPtr = Ptr NnList

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_list_new"
  c_nn_list_new :: Ptr CListPtr -> Word32 -> IO CInt

foreign import ccall unsafe "nn_list_free_all"
  c_nn_list_free_all :: IO ()

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
clistNew :: Word32 -> IO CListPtr
clistNew count = alloca $ \out -> do
  checkedListStatus "nn_list_new" =<< c_nn_list_new out count
  peek out

-- | Raise the 'Nix.Eval.CStatus.CStatusError' for a list constructor's
-- failure status.
checkedListStatus :: String -> CInt -> IO ()
checkedListStatus site status =
  unless (status == 0) (cStatusFailure site "list allocation failed")

-- | Free all tracked list headers (arena-style cleanup).
-- Items arrays are freed by the env page allocator.
clistFreeAll :: IO ()
clistFreeAll = c_nn_list_free_all

-- ---------------------------------------------------------------------------
-- Access
-- ---------------------------------------------------------------------------

-- | Number of elements in the list.
clistCount :: CListPtr -> IO Word32
clistCount = c_nn_list_count

-- | Get the thunk pointer at the given index.
clistGet :: CListPtr -> Word32 -> IO CThunkPtr
clistGet = c_nn_list_get

-- | Set the thunk pointer at the given index (for construction).
clistSet :: CListPtr -> Word32 -> CThunkPtr -> IO ()
clistSet = c_nn_list_set

-- ---------------------------------------------------------------------------
-- Conversion helpers
-- ---------------------------------------------------------------------------

-- | Convert a Haskell list of 'CThunkPtr' to a C list.
-- Allocates a new nn_list_t and fills it with the thunk pointers.
-- Returns 'nullPtr' for empty lists.  A list longer than the C count
-- can hold is refused rather than wrapped to a shorter allocation that
-- the fill would then run past.
thunkListToCList :: [CThunkPtr] -> IO CListPtr
thunkListToCList [] = pure nullPtr
thunkListToCList ptrs
  | len > fromIntegral (maxBound :: Word32) =
      cStatusFailure "nn_list_new" ("a list of " ++ show len ++ " elements is past the uint32_t count a C list holds")
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
clistToThunkList :: CListPtr -> IO [CThunkPtr]
clistToThunkList cl
  | cl == nullPtr = pure []
  | otherwise = do
      n <- clistCount cl
      mapM (clistGet cl) [0 .. n - 1]

-- ---------------------------------------------------------------------------
-- CList newtype (wraps CListPtr for use in NixValue ADT)
-- ---------------------------------------------------------------------------

-- | Opaque wrapper around a C-backed list.  Used as the payload of
-- @VList@ in the 'NixValue' ADT.  The underlying @nn_list_t*@ is
-- arena-allocated and valid until evaluation end.
newtype CList = CList {unCList :: CListPtr}

-- | Pointer equality - two CLists are the same if they point to the
-- same C struct.  Deep equality is handled by the evaluator.
instance Eq CList where
  CList a == CList b = a == b

instance Show CList where
  show (CList p)
    | p == nullPtr = "<list 0>"
    | otherwise =
        let n = unsafePerformIO (clistCount p)
         in "<list " ++ show n ++ ">"

-- | The empty CList (null pointer, zero elements).
emptyCList :: CList
emptyCList = CList nullPtr

-- | Convert a Haskell list of 'CThunkPtr' to a 'CList'.
-- Uses 'unsafePerformIO' - safe because allocation is idempotent
-- per call (no shared mutable state beyond the tracking array).
{-# NOINLINE clistFromThunks #-}
clistFromThunks :: [CThunkPtr] -> CList
clistFromThunks ptrs = CList (unsafePerformIO (thunkListToCList ptrs))

-- | Convert a 'CList' back to a Haskell list of 'CThunkPtr'.
clistThunks :: CList -> [CThunkPtr]
clistThunks (CList p) = unsafePerformIO (clistToThunkList p)

-- | Number of elements in the list.
clistLen :: CList -> Int
clistLen (CList p)
  | p == nullPtr = 0
  | otherwise = fromIntegral (unsafePerformIO (clistCount p))

-- | The element at an index, or 'Nothing' outside the list, read from the
-- C array without materializing the list.  Pure because a list is never
-- written after construction.
clistIndex :: CList -> Int -> Maybe CThunkPtr
clistIndex cl@(CList p) i
  | i < 0 || i >= clistLen cl = Nothing
  | otherwise = Just $! unsafePerformIO (clistGet p (fromIntegral i))

-- | The list without its first @n@ elements, as 'drop' is on lists.  The
-- result is a new header over this list's C array (@nn_list_drop@), so it
-- costs the same whatever the length.  Allocating it under
-- 'unsafePerformIO' is safe because the array is immutable: a header
-- built twice or shared between calls describes the same elements.
clistDrop :: Int -> CList -> CList
clistDrop n cl@(CList p)
  | n <= 0 = cl
  | n >= clistLen cl = emptyCList
  | otherwise = CList (unsafePerformIO (alloca dropInto))
  where
    dropInto out = do
      checkedListStatus "nn_list_drop" =<< c_nn_list_drop out p (fromIntegral n)
      peek out
