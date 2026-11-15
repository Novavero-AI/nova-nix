-- | C-backed evaluation environments.
--
-- Wraps @cbits/nn_env.c@ - arena-allocated @nn_env_t@ envs that hold
-- their slots inline, and a parent, a lazy scope and with-scopes when
-- they have them.  All memory is freed in bulk via 'cenvDestroy' at
-- evaluation end.
--
-- An env is a header word and its slots, followed by its parent when it
-- has one and by its scopes when it has them (@cbits/nn_env.h@): a
-- call's env of one slot takes 24 bytes, none of them on GHC's heap.
-- Every allocation names its 'EnvSite', under which the census counts
-- it.
module Nix.Eval.CEnv
  ( -- * Opaque C type
    NnEnv,

    -- * Lifecycle
    cenvInit,
    cenvDestroy,

    -- * Env constructors
    cenvEmpty,
    cenvNew,
    cenvNewScoped,
    cenvPushWith,

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
    cenvWithOwner,

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

-- The empty env is a C static, one address for the life of the process,
-- so this import is pure, as the null and Boolean cells of
-- "Nix.Eval.CThunk" are: no arena window, nor its absence, changes what
-- it returns.

-- | The global empty env: no slots, no parent, no scopes.  Not to be
-- modified.
foreign import ccall unsafe "nn_env_empty"
  cenvEmpty :: Ptr NnEnv

foreign import ccall unsafe "nn_env_new"
  c_nn_env_new :: Word32 -> Ptr NnEnv -> Word32 -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_new_scoped"
  c_nn_env_new_scoped ::
    Word32 ->
    Ptr NnEnv ->
    Ptr () ->
    Ptr (Ptr ()) ->
    Word32 ->
    Word32 ->
    IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_push_with"
  c_nn_env_push_with :: Ptr NnEnv -> Ptr () -> Word32 -> IO (Ptr NnEnv)

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

foreign import ccall unsafe "nn_env_with_owner"
  c_nn_env_with_owner :: Ptr NnEnv -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_alloc_with_scopes"
  c_nn_env_alloc_with_scopes :: Word32 -> Word32 -> IO (Ptr (Ptr ()))

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize the global env allocator.  Call once before evaluation.
cenvInit :: IO ()
cenvInit = c_nn_env_init

-- | Destroy the global env allocator, freeing all page memory.
-- All env and array pointers become invalid after this call.
cenvDestroy :: IO ()
cenvDestroy = c_nn_env_destroy

-- ---------------------------------------------------------------------------
-- Env constructors
-- ---------------------------------------------------------------------------

-- | An env of @count@ empty slots under a parent, or under none for
-- 'Foreign.Ptr.nullPtr', made at a site.  Arena-allocated;
-- 'Foreign.Ptr.nullPtr' when it cannot be allocated.
cenvNew :: (MonadCData m) => EnvSite -> Word32 -> Ptr NnEnv -> m (Ptr NnEnv)
cenvNew site slotCount parent = cdataIO (c_nn_env_new slotCount parent (envSiteIndex site))

-- | 'cenvNew' for an env with scopes: a lazy scope (or
-- 'Foreign.Ptr.nullPtr') and with-scopes of its own (or
-- 'Foreign.Ptr.nullPtr' and 0, to see its parent's).
-- 'Foreign.Ptr.nullPtr' for a count above 0 without an array.
cenvNewScoped ::
  (MonadCData m) =>
  EnvSite ->
  Word32 ->
  Ptr NnEnv ->
  Ptr () ->
  Ptr (Ptr ()) ->
  Word32 ->
  m (Ptr NnEnv)
cenvNewScoped site slotCount parent lazyScope withs withCount =
  cdataIO (c_nn_env_new_scoped slotCount parent lazyScope withs withCount (envSiteIndex site))

-- | The env a with's body runs in, made at a site: a copy of the base
-- with a new with-scope prepended to those it sees.
cenvPushWith :: (MonadCData m) => EnvSite -> Ptr NnEnv -> Ptr () -> m (Ptr NnEnv)
cenvPushWith site base scope = cdataIO (c_nn_env_push_with base scope (envSiteIndex site))

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | The slots of a C env, inline in it.
cenvSlots :: (MonadCData m) => Ptr NnEnv -> m (Ptr CThunkPtr)
cenvSlots = cdataIO . c_nn_env_slots

-- | Read the slot count from a C env.
cenvSlotCount :: (MonadCData m) => Ptr NnEnv -> m Word32
cenvSlotCount = cdataIO . c_nn_env_slot_count

-- | Read the lazy scope pointer from a C env (NULL if none).
cenvLazyScope :: (MonadCData m) => Ptr NnEnv -> m (Ptr ())
cenvLazyScope = cdataIO . c_nn_env_lazy_scope

-- | Read the parent pointer from a C env (NULL if none).
cenvParent :: (MonadCData m) => Ptr NnEnv -> m (Ptr NnEnv)
cenvParent = cdataIO . c_nn_env_parent

-- | A C env's own with-scopes array (NULL if it has none of its own;
-- 'cenvWithOwner' finds the env whose with-scopes it sees).
cenvWithScopes :: (MonadCData m) => Ptr NnEnv -> m (Ptr (Ptr ()))
cenvWithScopes = cdataIO . c_nn_env_with_scopes

-- | The count of a C env's own with-scopes (0 if it has none of its own).
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

-- | The env whose with-scopes a C env sees: the first on its parent
-- chain, itself included, with with-scopes of its own (NULL if none).
cenvWithOwner :: (MonadCData m) => Ptr NnEnv -> m (Ptr NnEnv)
cenvWithOwner = cdataIO . c_nn_env_with_owner

-- ---------------------------------------------------------------------------
-- With-scopes array allocation
-- ---------------------------------------------------------------------------

-- | Allocate an arena array for the with-scopes of an env at a site.
-- Zero-initialized.  Returns 'Foreign.Ptr.nullPtr' if the count is 0 or
-- the allocation fails - the caller must check.
cenvAllocWithScopes :: (MonadCData m) => EnvSite -> Word32 -> m (Ptr (Ptr ()))
cenvAllocWithScopes site count = cdataIO (c_nn_env_alloc_with_scopes count (envSiteIndex site))
