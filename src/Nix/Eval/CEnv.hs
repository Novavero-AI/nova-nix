-- | C-backed evaluation environments.
--
-- Wraps @cbits/nn_env.c@ - arena-allocated @nn_env_t@ envs that hold
-- their slots inline, and a parent, a name table and with-scopes when
-- they have them.  All memory is freed in bulk at arena teardown.
--
-- An env is a word and its slots (@cbits/nn_env.h@).  One of one to
-- seven slots and no scopes keeps its parent in that word, as upstream's
-- @Env@ is a parent and its values: a call's env of one slot takes 16
-- bytes, none of them on GHC's heap.  Any other env's word is a header,
-- followed after the slots by its parent when it has one and by its
-- scopes when it has them.
-- Every allocation names its 'EnvSite', under which the census counts
-- it.
module Nix.Eval.CEnv
  ( -- * Opaque C type
    NnEnv,

    -- * Env constructors
    cenvEmpty,
    cenvNew,
    cenvNewScoped,
    cenvPushWith,

    -- * Accessors
    cenvSlots,
    cenvSlotCount,
    cenvNameTable,
    cenvParent,
    cenvWithScopes,
    cenvWithCount,

    -- * Lookup
    cenvLookupResolved,
    cenvWithOwner,
  )
where

import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)
import Nix.Eval.AllocSite (EnvSite, envSiteIndex)
import Nix.Eval.CData (MonadCData, cdataIO)
import Nix.Eval.CStatus (cInvariantViolated, checkedCPtr)
import Nix.Eval.CThunk (CThunkPtr)

-- ---------------------------------------------------------------------------
-- Opaque C type (phantom - never constructed on the Haskell side)
-- ---------------------------------------------------------------------------

-- | Phantom type representing the C @nn_env_t@ struct.
data NnEnv

-- ---------------------------------------------------------------------------
-- FFI imports (all unsafe - no callbacks, fast operations)
-- ---------------------------------------------------------------------------

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

foreign import ccall unsafe "nn_env_name_table"
  c_nn_env_name_table :: Ptr NnEnv -> IO (Ptr ())

foreign import ccall unsafe "nn_env_parent"
  c_nn_env_parent :: Ptr NnEnv -> IO (Ptr NnEnv)

foreign import ccall unsafe "nn_env_with_scopes"
  c_nn_env_with_scopes :: Ptr NnEnv -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "nn_env_with_count"
  c_nn_env_with_count :: Ptr NnEnv -> IO Word32

foreign import ccall unsafe "nn_env_lookup_resolved"
  c_nn_env_lookup_resolved :: Ptr NnEnv -> Word32 -> Word32 -> IO CThunkPtr

foreign import ccall unsafe "nn_env_with_owner"
  c_nn_env_with_owner :: Ptr NnEnv -> IO (Ptr NnEnv)

-- ---------------------------------------------------------------------------
-- Env constructors
-- ---------------------------------------------------------------------------

-- | An env of @count@ empty slots under a parent, or under none for
-- 'Foreign.Ptr.nullPtr', made at a site.  Arena-allocated; the C side's
-- NULL, when it cannot allocate the env, is raised as
-- 'Nix.Eval.CStatus.CStatusError'.  A parent no env could be (an address
-- with any bit of @NN_ENV_SHORT_MASK@ set) aborts in C.
cenvNew :: (MonadCData m) => EnvSite -> Word32 -> Ptr NnEnv -> m (Ptr NnEnv)
cenvNew site slotCount parent = cdataIO (checkedCPtr "nn_env_new" =<< c_nn_env_new slotCount parent (envSiteIndex site))

-- | 'cenvNew' for an env with scopes: a name table (or
-- 'Foreign.Ptr.nullPtr') and with-scopes of its own (or
-- 'Foreign.Ptr.nullPtr' and 0, to see its parent's).  The env holds the
-- with-scopes array itself, so envs may share one.  A with-scope count
-- above 0 without an array aborts in C.
cenvNewScoped ::
  (MonadCData m) =>
  EnvSite ->
  Word32 ->
  Ptr NnEnv ->
  Ptr () ->
  Ptr (Ptr ()) ->
  Word32 ->
  m (Ptr NnEnv)
cenvNewScoped site slotCount parent nameTable withs withCount =
  cdataIO (checkedCPtr "nn_env_new_scoped" =<< c_nn_env_new_scoped slotCount parent nameTable withs withCount (envSiteIndex site))

-- | The env a with's body runs in, made at a site: a copy of the base
-- with a new with-scope prepended to those it sees.
cenvPushWith :: (MonadCData m) => EnvSite -> Ptr NnEnv -> Ptr () -> m (Ptr NnEnv)
cenvPushWith site base scope = cdataIO (checkedCPtr "nn_env_push_with" =<< c_nn_env_push_with base scope (envSiteIndex site))

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | The slots of a C env, inline in it.
cenvSlots :: (MonadCData m) => Ptr NnEnv -> m (Ptr CThunkPtr)
cenvSlots = cdataIO . c_nn_env_slots

-- | How many slots a C env has.
cenvSlotCount :: (MonadCData m) => Ptr NnEnv -> m Word32
cenvSlotCount = cdataIO . c_nn_env_slot_count

-- | Read the name table pointer from a C env (NULL if none).
cenvNameTable :: (MonadCData m) => Ptr NnEnv -> m (Ptr ())
cenvNameTable = cdataIO . c_nn_env_name_table

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
-- @slots[idx]@.  Single FFI call - O(level) in C.  The resolver chose the
-- level and index for this env's shape, so the NULL the C side answers
-- for one outside the env, or for a slot a frame has not filled yet, is a
-- defect in this library, raised as
-- 'Nix.Eval.CStatus.CInvariantViolated' rather than handed on as a cell
-- the next force would dereference.
cenvLookupResolved :: (MonadCData m) => Ptr NnEnv -> Word32 -> Word32 -> m CThunkPtr
cenvLookupResolved env level idx = cdataIO $ do
  cell <- c_nn_env_lookup_resolved env level idx
  if cell == nullPtr
    then cInvariantViolated "nn_env_lookup_resolved" ("no thunk at level " ++ show level ++ ", index " ++ show idx)
    else pure cell

-- | The env whose with-scopes a C env sees: the first on its parent
-- chain, itself included, with with-scopes of its own (NULL if none).
cenvWithOwner :: (MonadCData m) => Ptr NnEnv -> m (Ptr NnEnv)
cenvWithOwner = cdataIO . c_nn_env_with_owner
