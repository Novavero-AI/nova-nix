-- | C-backed evaluation environments.
--
-- Wraps @cbits/nn_env.c@ - arena-allocated @nn_env_t@ structs that
-- hold slot arrays, lazy scopes, parent pointers, and with-scopes.
-- All memory is freed in bulk via 'cenvDestroy' at evaluation end.
--
-- Each @nn_env_t@ is 48 bytes, arena-allocated off the GHC heap.  Every
-- allocation names its 'EnvSite', under which the census counts it.
module Nix.Eval.CEnv
  ( -- * Opaque C type
    NnEnv,

    -- * Lifecycle
    cenvInit,
    cenvDestroy,

    -- * Slot allocation
    cenvAllocSlots,

    -- * Env constructors
    cenvEmpty,
    cenvNew,
    cenvFromSlots,
    cenvPushWith,
    cenvNewMinimal,

    -- * Accessors
    cenvSlots,
    cenvSlotCount,
    cenvLazyScope,
    cenvParent,
    cenvWithScopes,
    cenvWithCount,

    -- * Lookup
    cenvLookupResolved,
    cenvRootScope,

    -- * With-scopes array
    cenvAllocWithScopes,
  )
where

import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Nix.Eval.AllocSite (EnvSite, envSiteIndex)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CThunk (CThunkPtr)

-- ---------------------------------------------------------------------------
-- Opaque C type (phantom - never constructed on the Haskell side)
-- ---------------------------------------------------------------------------

-- | Phantom type representing the C @nn_env_t@ struct.
data NnEnv

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast operations)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_env_init"
  c_nn_env_init :: IO ()

foreign import ccall unsafe "nn_env_destroy"
  c_nn_env_destroy :: IO ()

foreign import ccall unsafe "nn_env_alloc_slots"
  c_nn_env_alloc_slots :: Word32 -> Word32 -> IO (Ptr CThunkPtr)

-- The empty env is a C static, one address for the life of the process,
-- so this import is pure, as the null and Boolean cells of
-- "Nix.Eval.CThunk" are: no arena window, nor its absence, changes what
-- it returns.

-- | The global empty env (all fields zero or NULL).  Not to be modified.
foreign import ccall unsafe "nn_env_empty"
  cenvEmpty :: Ptr NnEnv

foreign import ccall unsafe "nn_env_new"
  c_nn_env_new ::
    Ptr CThunkPtr ->
    Word32 ->
    Ptr () ->
    Ptr NnEnv ->
    Ptr (Ptr ()) ->
    Word32 ->
    Word32 ->
    IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_from_slots"
  c_nn_env_from_slots ::
    Ptr CThunkPtr -> Word32 -> Ptr NnEnv -> Word32 -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_push_with"
  c_nn_env_push_with :: Ptr NnEnv -> Ptr () -> Word32 -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_new_minimal"
  c_nn_env_new_minimal :: Ptr CThunkPtr -> Word32 -> Word32 -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_slots"
  c_nn_env_slots :: Ptr NnEnv -> IO (Ptr CThunkPtr)

foreign import ccall unsafe "nn_env_slot_count"
  c_nn_env_slot_count :: Ptr NnEnv -> IO Word32

foreign import ccall unsafe "nn_env_lazy_scope"
  c_nn_env_lazy_scope :: Ptr NnEnv -> IO (Ptr ())

foreign import ccall unsafe "nn_env_parent"
  c_nn_env_parent :: Ptr NnEnv -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_with_scopes"
  c_nn_env_with_scopes :: Ptr NnEnv -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "nn_env_with_count"
  c_nn_env_with_count :: Ptr NnEnv -> IO Word32

foreign import ccall unsafe "nn_env_lookup_resolved"
  c_nn_env_lookup_resolved :: Ptr NnEnv -> CInt -> CInt -> IO CThunkPtr

foreign import ccall unsafe "nn_env_root_scope"
  c_nn_env_root_scope :: Ptr NnEnv -> IO (Ptr ())

foreign import ccall unsafe "nn_env_alloc_with_scopes"
  c_nn_env_alloc_with_scopes :: Word32 -> Word32 -> IO (Ptr (Ptr ()))

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize the global env allocator.  Call once before evaluation.
cenvInit :: IO ()
cenvInit = c_nn_env_init

-- | Destroy the global env allocator, freeing all page memory.
-- All env and slot array pointers become invalid after this call.
cenvDestroy :: IO ()
cenvDestroy = c_nn_env_destroy

-- ---------------------------------------------------------------------------
-- Slot allocation
-- ---------------------------------------------------------------------------

-- | Allocate a C array of @count@ 'CThunkPtr' slots for an env at a
-- site.  O(1) amortized.  Returns 'Foreign.Ptr.nullPtr' if count is 0.
-- All slots are zero-initialized.
cenvAllocSlots :: (MonadCData m) => EnvSite -> Word32 -> m (Ptr CThunkPtr)
cenvAllocSlots site count = cdataIO (c_nn_env_alloc_slots count (envSiteIndex site))

-- ---------------------------------------------------------------------------
-- Env constructors
-- ---------------------------------------------------------------------------

-- | Full constructor: set all fields explicitly.  Arena-allocated.
cenvNew ::
  (MonadCData m) =>
  EnvSite ->
  Ptr CThunkPtr ->
  Word32 ->
  Ptr () ->
  Ptr NnEnv ->
  Ptr (Ptr ()) ->
  Word32 ->
  m (Ptr NnEnv)
cenvNew site slots slotCount lazyScope parent withs withCount =
  cdataIO (c_nn_env_new slots slotCount lazyScope parent withs withCount (envSiteIndex site))

-- | Child env with positional slots, inheriting parent's with-scopes.
cenvFromSlots :: (MonadCData m) => EnvSite -> Ptr CThunkPtr -> Word32 -> Ptr NnEnv -> m (Ptr NnEnv)
cenvFromSlots site slots slotCount parent = cdataIO (c_nn_env_from_slots slots slotCount parent (envSiteIndex site))

-- | Copy base env with a new with-scope prepended.
cenvPushWith :: (MonadCData m) => EnvSite -> Ptr NnEnv -> Ptr () -> m (Ptr NnEnv)
cenvPushWith site base scope = cdataIO (c_nn_env_push_with base scope (envSiteIndex site))

-- | Minimal env: slots only, no parent, no with-scopes.
cenvNewMinimal :: (MonadCData m) => EnvSite -> Ptr CThunkPtr -> Word32 -> m (Ptr NnEnv)
cenvNewMinimal site slots slotCount = cdataIO (c_nn_env_new_minimal slots slotCount (envSiteIndex site))

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | Read the slot array pointer from a C env.
cenvSlots :: (MonadCData m) => Ptr NnEnv -> m (Ptr CThunkPtr)
cenvSlots = cdataIO . c_nn_env_slots

-- | Read the slot count from a C env.
cenvSlotCount :: (MonadCData m) => Ptr NnEnv -> m Word32
cenvSlotCount = cdataIO . c_nn_env_slot_count

-- | Read the lazy scope pointer from a C env (NULL if none).
cenvLazyScope :: (MonadCData m) => Ptr NnEnv -> m (Ptr ())
cenvLazyScope = cdataIO . c_nn_env_lazy_scope

-- | Read the parent pointer from a C env (NULL if root).
cenvParent :: (MonadCData m) => Ptr NnEnv -> m (Ptr NnEnv)
cenvParent = cdataIO . c_nn_env_parent

-- | Read the with-scopes array pointer from a C env.
cenvWithScopes :: (MonadCData m) => Ptr NnEnv -> m (Ptr (Ptr ()))
cenvWithScopes = cdataIO . c_nn_env_with_scopes

-- | Read the with-scope count from a C env.
cenvWithCount :: (MonadCData m) => Ptr NnEnv -> m Word32
cenvWithCount = cdataIO . c_nn_env_with_count

-- ---------------------------------------------------------------------------
-- Lookup
-- ---------------------------------------------------------------------------

-- | Resolved variable lookup: walk @level@ parent hops, then read
-- @slots[idx]@.  Single FFI call - O(level) in C.
cenvLookupResolved :: (MonadCData m) => Ptr NnEnv -> Int -> Int -> m CThunkPtr
cenvLookupResolved env level idx =
  -- C @int@ params: convert at the boundary (CInt), not HsInt (64-bit).
  cdataIO $ c_nn_env_lookup_resolved env (fromIntegral level) (fromIntegral idx)

-- | Walk parent chain to root, return root's lazy scope (NULL if none).
cenvRootScope :: (MonadCData m) => Ptr NnEnv -> m (Ptr ())
cenvRootScope = cdataIO . c_nn_env_root_scope

-- ---------------------------------------------------------------------------
-- With-scopes array allocation
-- ---------------------------------------------------------------------------

-- | Allocate an arena array for the with-scopes of an env at a site.
-- Zero-initialized.  Returns 'Foreign.Ptr.nullPtr' if the count is 0 or
-- the allocation fails - the caller must check.
cenvAllocWithScopes :: (MonadCData m) => EnvSite -> Word32 -> m (Ptr (Ptr ()))
cenvAllocWithScopes site count = cdataIO (c_nn_env_alloc_with_scopes count (envSiteIndex site))
