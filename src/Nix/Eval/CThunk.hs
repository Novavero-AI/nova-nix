-- | C-backed thunk memoization cells via FFI.
--
-- Wraps @cbits/nn_thunk.c@ - an arena-allocated mutable cell that
-- holds either pending code (a bytecode index and the C env it runs in)
-- or a computed value (an inline scalar or builtin index, or a pointer to
-- a C object).  Each cell is a 16-byte C struct in an arena, invisible to
-- GHC's GC.
--
-- Includes blackhole detection: a thunk being forced is marked
-- BLACKHOLE; if forced again before completion, infinite recursion
-- is detected (matching C++ Nix behavior).
--
-- @
-- cthunkInit 0
-- ptr <- cthunkNewBc bcIdx envPtr
-- cthunkState ptr  -- 0 (PENDING)
-- cthunkMarkBlackhole ptr  -- True (was PENDING)
-- cthunkSetComputedInt ptr 42  -- True (was not yet COMPUTED)
-- cthunkState ptr  -- 1 (COMPUTED)
-- cthunkDestroy
-- @
module Nix.Eval.CThunk
  ( -- * Opaque handle
    NnThunk,
    CThunkPtr,

    -- * Lifecycle
    cthunkInit,
    cthunkDestroy,

    -- * Allocation
    cthunkNewBc,
    cthunkNewComputedInt,
    cthunkNewComputedFloat,
    cthunkNewComputedStr,
    cthunkNewComputedPath,
    cthunkNewComputedList,
    cthunkNewComputedAttrs,
    cthunkNewComputedCtxStr,
    cthunkNewComputedLambda,
    cthunkNewComputedPrimop,
    cthunkNewComputedPrimopApp,

    -- * Shared constants
    cthunkNull,
    cthunkBool,
    cthunkEmptyList,
    cthunkPrimop,

    -- * State queries
    cthunkState,
    cthunkPayload,
    cthunkValueTag,
    cthunkGetBcIdx,
    cthunkGetInt,
    cthunkGetFloat,
    cthunkGetBool,
    cthunkGetStr,
    cthunkGetPath,
    cthunkGetList,
    cthunkGetAttrs,
    cthunkGetCtxStr,
    cthunkGetLambda,
    cthunkGetPrimop,
    cthunkGetPrimopApp,

    -- * State transitions
    cthunkMarkBlackhole,
    cthunkMarkPending,
    cthunkSetComputedInt,
    cthunkSetComputedFloat,
    cthunkSetComputedBool,
    cthunkSetComputedNull,
    cthunkSetComputedStr,
    cthunkSetComputedPath,
    cthunkSetComputedList,
    cthunkSetComputedAttrs,
    cthunkSetComputedCtxStr,
    cthunkSetComputedLambda,
    cthunkSetComputedPrimop,
    cthunkSetComputedPrimopApp,

    -- * Arena diagnostics / cleanup
    cthunkCount,
    cthunkGet,
  )
where

import Data.Int (Int64)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (cInvariantViolated, checkedCPtr)
import Nix.Eval.CStr (CStrPtr)

-- | Phantom type for C-side @nn_thunk_t@.
data NnThunk

-- | Pointer to a C-allocated thunk (lives in arena).
type CThunkPtr = Ptr NnThunk

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast data access)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_thunk_init"
  c_nn_thunk_init :: Word32 -> IO ()

foreign import ccall unsafe "nn_thunk_destroy"
  c_nn_thunk_destroy :: IO ()

foreign import ccall unsafe "nn_thunk_new_bc"
  c_nn_thunk_new_bc :: Word32 -> Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_get_bc_idx"
  c_nn_thunk_get_bc_idx :: CThunkPtr -> IO Word32

foreign import ccall unsafe "nn_thunk_new_computed_int"
  c_nn_thunk_new_computed_int :: Int64 -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_float"
  c_nn_thunk_new_computed_float :: CDouble -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_str"
  c_nn_thunk_new_computed_str :: CStrPtr -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_path"
  c_nn_thunk_new_computed_path :: CStrPtr -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_list"
  c_nn_thunk_new_computed_list :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_attrs"
  c_nn_thunk_new_computed_attrs :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_ctxstr"
  c_nn_thunk_new_computed_ctxstr :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_lambda"
  c_nn_thunk_new_computed_lambda :: Ptr () -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_primop"
  c_nn_thunk_new_computed_primop :: Word32 -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_new_computed_primop_app"
  c_nn_thunk_new_computed_primop_app :: Ptr () -> IO CThunkPtr

-- The shared cells are C statics, one address for the life of the
-- process, so these imports are pure: no arena window, nor its absence,
-- changes what they return.

-- | The shared COMPUTED @null@ thunk.
foreign import ccall unsafe "nn_thunk_null"
  cthunkNull :: CThunkPtr

-- | The shared COMPUTED empty list thunk.
foreign import ccall unsafe "nn_thunk_empty_list"
  cthunkEmptyList :: CThunkPtr

foreign import ccall unsafe "nn_thunk_bool"
  c_nn_thunk_bool :: Word8 -> CThunkPtr

-- Fills the static cell the first time it is asked for, so an action.
foreign import ccall unsafe "nn_thunk_primop"
  c_nn_thunk_primop :: Word32 -> IO CThunkPtr

foreign import ccall unsafe "nn_thunk_state"
  c_nn_thunk_state :: CThunkPtr -> IO Word8

foreign import ccall unsafe "nn_thunk_payload"
  c_nn_thunk_payload :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_value_tag"
  c_nn_thunk_value_tag :: CThunkPtr -> IO Word8

foreign import ccall unsafe "nn_thunk_get_int"
  c_nn_thunk_get_int :: CThunkPtr -> IO Int64

foreign import ccall unsafe "nn_thunk_get_float"
  c_nn_thunk_get_float :: CThunkPtr -> IO CDouble

foreign import ccall unsafe "nn_thunk_get_bool"
  c_nn_thunk_get_bool :: CThunkPtr -> IO Word8

foreign import ccall unsafe "nn_thunk_get_str"
  c_nn_thunk_get_str :: CThunkPtr -> IO CStrPtr

foreign import ccall unsafe "nn_thunk_get_path"
  c_nn_thunk_get_path :: CThunkPtr -> IO CStrPtr

foreign import ccall unsafe "nn_thunk_get_list"
  c_nn_thunk_get_list :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_get_attrs"
  c_nn_thunk_get_attrs :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_get_ctxstr"
  c_nn_thunk_get_ctxstr :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_get_lambda"
  c_nn_thunk_get_lambda :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_get_primop"
  c_nn_thunk_get_primop :: CThunkPtr -> IO Word32

foreign import ccall unsafe "nn_thunk_get_primop_app"
  c_nn_thunk_get_primop_app :: CThunkPtr -> IO (Ptr ())

foreign import ccall unsafe "nn_thunk_mark_blackhole"
  c_nn_thunk_mark_blackhole :: CThunkPtr -> IO CInt

foreign import ccall unsafe "nn_thunk_mark_pending"
  c_nn_thunk_mark_pending :: CThunkPtr -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_int"
  c_nn_thunk_set_computed_int :: CThunkPtr -> Int64 -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_float"
  c_nn_thunk_set_computed_float :: CThunkPtr -> CDouble -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_bool"
  c_nn_thunk_set_computed_bool :: CThunkPtr -> Word8 -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_null"
  c_nn_thunk_set_computed_null :: CThunkPtr -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_str"
  c_nn_thunk_set_computed_str :: CThunkPtr -> CStrPtr -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_path"
  c_nn_thunk_set_computed_path :: CThunkPtr -> CStrPtr -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_list"
  c_nn_thunk_set_computed_list :: CThunkPtr -> Ptr () -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_attrs"
  c_nn_thunk_set_computed_attrs :: CThunkPtr -> Ptr () -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_ctxstr"
  c_nn_thunk_set_computed_ctxstr :: CThunkPtr -> Ptr () -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_lambda"
  c_nn_thunk_set_computed_lambda :: CThunkPtr -> Ptr () -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_primop"
  c_nn_thunk_set_computed_primop :: CThunkPtr -> Word32 -> IO CInt

foreign import ccall unsafe "nn_thunk_set_computed_primop_app"
  c_nn_thunk_set_computed_primop_app :: CThunkPtr -> Ptr () -> IO CInt

foreign import ccall unsafe "nn_thunk_count"
  c_nn_thunk_count :: IO Word32

foreign import ccall unsafe "nn_thunk_get"
  c_nn_thunk_get :: Word32 -> IO CThunkPtr

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize the global thunk arena.  Call once before evaluation.
-- @capacity@ is a hint for thunks per block (0 uses default: 65536).
cthunkInit :: Word32 -> IO ()
cthunkInit = c_nn_thunk_init

-- | Destroy the global thunk arena, freeing its blocks.  Every
-- 'CThunkPtr' into the arena becomes invalid after this call; the shared
-- cells do not.
cthunkDestroy :: IO ()
cthunkDestroy = c_nn_thunk_destroy

-- ---------------------------------------------------------------------------
-- Allocation
-- ---------------------------------------------------------------------------

-- Every allocator below rejects the C side's NULL through 'checkedCPtr':
-- outside the 'cthunkInit' .. 'cthunkDestroy' window that is the only
-- answer the arena gives, and a NULL that flowed on would fault at the
-- next state read instead of raising 'Nix.Eval.CStatus.ArenaNotInitialized'.

-- | Allocate a new PENDING thunk with a bytecode index + C env pointer.
-- No Haskell heap references - zero GC pressure for pending thunks.
-- The pointer is an @nn_env_t *@, untyped here because "Nix.Eval.CEnv"
-- imports this module for the slots it stores.
cthunkNewBc :: (MonadCData m) => Word32 -> Ptr () -> m CThunkPtr
cthunkNewBc bcIdx envPtr = cdataIO (checkedCPtr "nn_thunk_new_bc" =<< c_nn_thunk_new_bc bcIdx envPtr)

-- | Read the bytecode index from a PENDING thunk.
cthunkGetBcIdx :: (MonadCData m) => CThunkPtr -> m Word32
cthunkGetBcIdx = cdataIO . c_nn_thunk_get_bc_idx

-- | Allocate a pre-COMPUTED thunk with an inline int64.
cthunkNewComputedInt :: (MonadCData m) => Int64 -> m CThunkPtr
cthunkNewComputedInt n = cdataIO (checkedCPtr "nn_thunk_new_computed_int" =<< c_nn_thunk_new_computed_int n)

-- | Allocate a pre-COMPUTED thunk with an inline double.
cthunkNewComputedFloat :: (MonadCData m) => Double -> m CThunkPtr
cthunkNewComputedFloat d = cdataIO (checkedCPtr "nn_thunk_new_computed_float" =<< c_nn_thunk_new_computed_float (CDouble d))

-- | Allocate a pre-COMPUTED thunk pointing at a string's bytes.  For
-- context-free strings only (tag 4).
cthunkNewComputedStr :: (MonadCData m) => CStrPtr -> m CThunkPtr
cthunkNewComputedStr str = cdataIO (checkedCPtr "nn_thunk_new_computed_str" =<< c_nn_thunk_new_computed_str str)

-- | Allocate a pre-COMPUTED thunk pointing at a path's bytes.
cthunkNewComputedPath :: (MonadCData m) => CStrPtr -> m CThunkPtr
cthunkNewComputedPath str = cdataIO (checkedCPtr "nn_thunk_new_computed_path" =<< c_nn_thunk_new_computed_path str)

-- | Allocate a pre-COMPUTED thunk with a CList pointer.
cthunkNewComputedList :: (MonadCData m) => Ptr () -> m CThunkPtr
cthunkNewComputedList list = cdataIO (checkedCPtr "nn_thunk_new_computed_list" =<< c_nn_thunk_new_computed_list list)

-- | Allocate a pre-COMPUTED thunk with a CAttrSet pointer.
cthunkNewComputedAttrs :: (MonadCData m) => Ptr () -> m CThunkPtr
cthunkNewComputedAttrs attrs = cdataIO (checkedCPtr "nn_thunk_new_computed_attrs" =<< c_nn_thunk_new_computed_attrs attrs)

-- | Allocate a pre-COMPUTED thunk with a CCtxStr pointer.  For strings
-- with non-empty context (tag 8).
cthunkNewComputedCtxStr :: (MonadCData m) => Ptr () -> m CThunkPtr
cthunkNewComputedCtxStr ctxstr = cdataIO (checkedCPtr "nn_thunk_new_computed_ctxstr" =<< c_nn_thunk_new_computed_ctxstr ctxstr)

-- | Allocate a pre-COMPUTED thunk with a CLambda pointer.  For lambda
-- closures (tag 9).
cthunkNewComputedLambda :: (MonadCData m) => Ptr () -> m CThunkPtr
cthunkNewComputedLambda lambda = cdataIO (checkedCPtr "nn_thunk_new_computed_lambda" =<< c_nn_thunk_new_computed_lambda lambda)

-- | Allocate a pre-COMPUTED thunk holding a builtin's index inline, for a
-- builtin with no argument taken (tag 10).
cthunkNewComputedPrimop :: (MonadCData m) => Word32 -> m CThunkPtr
cthunkNewComputedPrimop primop = cdataIO (checkedCPtr "nn_thunk_new_computed_primop" =<< c_nn_thunk_new_computed_primop primop)

-- | Allocate a pre-COMPUTED thunk with a CPrimopApp pointer, for a
-- builtin's partial application (tag 11).
cthunkNewComputedPrimopApp :: (MonadCData m) => Ptr () -> m CThunkPtr
cthunkNewComputedPrimopApp app = cdataIO (checkedCPtr "nn_thunk_new_computed_primop_app" =<< c_nn_thunk_new_computed_primop_app app)

-- ---------------------------------------------------------------------------
-- Shared constants
-- ---------------------------------------------------------------------------

-- | The shared COMPUTED @true@ or @false@ thunk.
cthunkBool :: Bool -> CThunkPtr
cthunkBool b = if b then trueCell else falseCell

trueCell, falseCell :: CThunkPtr
trueCell = c_nn_thunk_bool 1
falseCell = c_nn_thunk_bool 0

-- | The shared COMPUTED cell of the builtin at an index: a C static, one
-- per builtin, the same in every arena window and outside all of them.
-- An index from @NN_PRIMOP_CELLS@ on has no cell, which only a builtin
-- table grown past that bound would ask for.
cthunkPrimop :: (MonadCData m) => Word32 -> m CThunkPtr
cthunkPrimop primop = cdataIO $ do
  cell <- c_nn_thunk_primop primop
  if cell == nullPtr
    then cInvariantViolated "nn_thunk_primop" ("builtin index " ++ show primop ++ " is past the static cells (NN_PRIMOP_CELLS)")
    else pure cell

-- ---------------------------------------------------------------------------
-- State queries
-- ---------------------------------------------------------------------------

-- | Read the current state: 0 = PENDING, 1 = COMPUTED, 2 = BLACKHOLE.
cthunkState :: (MonadCData m) => CThunkPtr -> m Word8
cthunkState = cdataIO . c_nn_thunk_state

-- | Read the payload pointer.  Returns 'Foreign.Ptr.nullPtr' for NULL payloads.
cthunkPayload :: (MonadCData m) => CThunkPtr -> m (Ptr ())
cthunkPayload = cdataIO . c_nn_thunk_payload

-- | Read the value tag (valid when state == COMPUTED): the
-- @NN_VALUE_*@ constants of @cbits\/nn_thunk.h@.
cthunkValueTag :: (MonadCData m) => CThunkPtr -> m Word8
cthunkValueTag = cdataIO . c_nn_thunk_value_tag

-- | Read an inline int64 from a COMPUTED thunk (val_tag == 0).
cthunkGetInt :: (MonadCData m) => CThunkPtr -> m Int64
cthunkGetInt = cdataIO . c_nn_thunk_get_int

-- | Read an inline double from a COMPUTED thunk (val_tag == 1).
cthunkGetFloat :: (MonadCData m) => CThunkPtr -> m Double
cthunkGetFloat ptr = cdataIO $ do
  CDouble d <- c_nn_thunk_get_float ptr
  pure d

-- | Read an inline bool from a COMPUTED thunk (val_tag == 2).
cthunkGetBool :: (MonadCData m) => CThunkPtr -> m Word8
cthunkGetBool = cdataIO . c_nn_thunk_get_bool

-- | Read a string's bytes pointer from a COMPUTED thunk (val_tag == 4).
cthunkGetStr :: (MonadCData m) => CThunkPtr -> m CStrPtr
cthunkGetStr = cdataIO . c_nn_thunk_get_str

-- | Read a path's bytes pointer from a COMPUTED thunk (val_tag == 5).
cthunkGetPath :: (MonadCData m) => CThunkPtr -> m CStrPtr
cthunkGetPath = cdataIO . c_nn_thunk_get_path

-- | Read a CList pointer from a COMPUTED thunk (val_tag == 6).
cthunkGetList :: (MonadCData m) => CThunkPtr -> m (Ptr ())
cthunkGetList = cdataIO . c_nn_thunk_get_list

-- | Read a CAttrSet pointer from a COMPUTED thunk (val_tag == 7).
cthunkGetAttrs :: (MonadCData m) => CThunkPtr -> m (Ptr ())
cthunkGetAttrs = cdataIO . c_nn_thunk_get_attrs

-- | Read a CCtxStr pointer from a COMPUTED thunk (val_tag == 8).
cthunkGetCtxStr :: (MonadCData m) => CThunkPtr -> m (Ptr ())
cthunkGetCtxStr = cdataIO . c_nn_thunk_get_ctxstr

-- | Read a CLambda pointer from a COMPUTED thunk (val_tag == 9).
cthunkGetLambda :: (MonadCData m) => CThunkPtr -> m (Ptr ())
cthunkGetLambda = cdataIO . c_nn_thunk_get_lambda

-- | Read a builtin's index from a COMPUTED thunk (val_tag == 10).
cthunkGetPrimop :: (MonadCData m) => CThunkPtr -> m Word32
cthunkGetPrimop = cdataIO . c_nn_thunk_get_primop

-- | Read a CPrimopApp pointer from a COMPUTED thunk (val_tag == 11).
cthunkGetPrimopApp :: (MonadCData m) => CThunkPtr -> m (Ptr ())
cthunkGetPrimopApp = cdataIO . c_nn_thunk_get_primop_app

-- ---------------------------------------------------------------------------
-- State transitions
-- ---------------------------------------------------------------------------

-- | Mark a PENDING thunk as BLACKHOLE (being evaluated).
-- Returns 'True' on success, 'False' if not PENDING.
cthunkMarkBlackhole :: (MonadCData m) => CThunkPtr -> m Bool
cthunkMarkBlackhole ptr = cdataIO (accepted <$> c_nn_thunk_mark_blackhole ptr)

-- | Restore a BLACKHOLE thunk to PENDING after a caught throw during its force,
-- so a later force re-evaluates instead of misreporting infinite recursion.
-- Returns 'True' on success, 'False' if the thunk was not BLACKHOLE.
cthunkMarkPending :: (MonadCData m) => CThunkPtr -> m Bool
cthunkMarkPending ptr = cdataIO (accepted <$> c_nn_thunk_mark_pending ptr)

-- The setters below store a value in a thunk that is not yet COMPUTED and
-- answer 'True', or leave a COMPUTED thunk as it was and answer 'False'.
-- The pending payload they overwrite is an env pointer the env arena
-- owns, so nothing is handed back.

-- | Store an inline int64.
cthunkSetComputedInt :: (MonadCData m) => CThunkPtr -> Int64 -> m Bool
cthunkSetComputedInt ptr n = cdataIO (accepted <$> c_nn_thunk_set_computed_int ptr n)

-- | Store an inline double.
cthunkSetComputedFloat :: (MonadCData m) => CThunkPtr -> Double -> m Bool
cthunkSetComputedFloat ptr d = cdataIO (accepted <$> c_nn_thunk_set_computed_float ptr (CDouble d))

-- | Store an inline bool.
cthunkSetComputedBool :: (MonadCData m) => CThunkPtr -> Word8 -> m Bool
cthunkSetComputedBool ptr b = cdataIO (accepted <$> c_nn_thunk_set_computed_bool ptr b)

-- | Store null.
cthunkSetComputedNull :: (MonadCData m) => CThunkPtr -> m Bool
cthunkSetComputedNull ptr = cdataIO (accepted <$> c_nn_thunk_set_computed_null ptr)

-- | Store a pointer to a string's bytes.
cthunkSetComputedStr :: (MonadCData m) => CThunkPtr -> CStrPtr -> m Bool
cthunkSetComputedStr ptr str = cdataIO (accepted <$> c_nn_thunk_set_computed_str ptr str)

-- | Store a pointer to a path's bytes.
cthunkSetComputedPath :: (MonadCData m) => CThunkPtr -> CStrPtr -> m Bool
cthunkSetComputedPath ptr str = cdataIO (accepted <$> c_nn_thunk_set_computed_path ptr str)

-- | Store a CList pointer.
cthunkSetComputedList :: (MonadCData m) => CThunkPtr -> Ptr () -> m Bool
cthunkSetComputedList ptr list = cdataIO (accepted <$> c_nn_thunk_set_computed_list ptr list)

-- | Store a CAttrSet pointer.
cthunkSetComputedAttrs :: (MonadCData m) => CThunkPtr -> Ptr () -> m Bool
cthunkSetComputedAttrs ptr attrs = cdataIO (accepted <$> c_nn_thunk_set_computed_attrs ptr attrs)

-- | Store a CCtxStr pointer.
cthunkSetComputedCtxStr :: (MonadCData m) => CThunkPtr -> Ptr () -> m Bool
cthunkSetComputedCtxStr ptr ctxstr = cdataIO (accepted <$> c_nn_thunk_set_computed_ctxstr ptr ctxstr)

-- | Store a CLambda pointer.
cthunkSetComputedLambda :: (MonadCData m) => CThunkPtr -> Ptr () -> m Bool
cthunkSetComputedLambda ptr lambda = cdataIO (accepted <$> c_nn_thunk_set_computed_lambda ptr lambda)

-- | Store a builtin's index inline.
cthunkSetComputedPrimop :: (MonadCData m) => CThunkPtr -> Word32 -> m Bool
cthunkSetComputedPrimop ptr primop = cdataIO (accepted <$> c_nn_thunk_set_computed_primop ptr primop)

-- | Store a CPrimopApp pointer.
cthunkSetComputedPrimopApp :: (MonadCData m) => CThunkPtr -> Ptr () -> m Bool
cthunkSetComputedPrimopApp ptr app = cdataIO (accepted <$> c_nn_thunk_set_computed_primop_app ptr app)

-- | A transition's C answer: 1 when it ran, 0 when the thunk's state
-- refused it.
accepted :: CInt -> Bool
accepted = (/= 0)

-- ---------------------------------------------------------------------------
-- Arena diagnostics / cleanup
-- ---------------------------------------------------------------------------

-- | Total thunks allocated in the arena.
cthunkCount :: IO Word32
cthunkCount = c_nn_thunk_count

-- | Get a thunk by global index, in allocation order.
-- Returns 'Foreign.Ptr.nullPtr' if index is out of range.
cthunkGet :: Word32 -> IO CThunkPtr
cthunkGet = c_nn_thunk_get
