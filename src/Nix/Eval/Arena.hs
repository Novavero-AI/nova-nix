-- | The C data layer's arena lifecycle, which an evaluation session owns
-- ('Nix.Eval.IO.withEvalSession'): 'arenaInit' sets up every C sub-arena
-- (symbol table, string arena, thunk arena, env pages, bytecode store) as
-- the outermost session starts, and 'arenaDestroy' tears them down as it
-- ends.  Every value lives in C, so the teardown frees a session's values
-- whole, with nothing in GHC's heap to release.  The module is internal to
-- the library: no caller sets the arena up or tears it down.
--
-- Outside a session the sub-arenas have no memory to hand out, and every
-- path that allocates from one ('Nix.Builtins.builtinEnv', 'Nix.Eval.eval',
-- 'Nix.Eval.force', the value constructors that make a cell or intern a
-- name) raises 'Nix.Eval.CStatus.ArenaNotInitialized' naming the C entry
-- point that refused, rather than dereferencing state that does not
-- exist.  The tracked objects (sets, list headers, lambdas, strings with
-- context, partial applications) are malloc'd, so the binding modules'
-- own constructors for them work outside a session, and the next
-- teardown frees what they made.  Parsing ("Nix.Parser") is pure and
-- needs no arena.  The @null@, @true@, @false@ and empty list cells and each
-- builtin's own cell are C statics outside every arena, so forcing one
-- needs no session and survives every one.
module Nix.Eval.Arena
  ( arenaInit,
    arenaDestroy,
  )
where

import Data.Word (Word32)
import Nix.Eval.StartupSymbols (startupSymbols)
import Nix.Eval.Symbol (symbolIntern)

-- ---------------------------------------------------------------------------
-- The C lifecycle, which only this module calls
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_symbol_init" c_nn_symbol_init :: Word32 -> IO ()

foreign import ccall unsafe "nn_symbol_destroy" c_nn_symbol_destroy :: IO ()

foreign import ccall unsafe "nn_str_init" c_nn_str_init :: IO ()

foreign import ccall unsafe "nn_str_destroy" c_nn_str_destroy :: IO ()

foreign import ccall unsafe "nn_thunk_init" c_nn_thunk_init :: Word32 -> IO ()

foreign import ccall unsafe "nn_thunk_destroy" c_nn_thunk_destroy :: IO ()

foreign import ccall unsafe "nn_env_init" c_nn_env_init :: IO ()

foreign import ccall unsafe "nn_env_destroy" c_nn_env_destroy :: IO ()

foreign import ccall unsafe "nn_bytecode_init" c_nn_bytecode_init :: Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nn_bytecode_destroy" c_nn_bytecode_destroy :: IO ()

foreign import ccall unsafe "nn_roots_clear" c_nn_roots_clear :: IO ()

foreign import ccall unsafe "nn_ctxstr_free_all" c_nn_ctxstr_free_all :: IO ()

foreign import ccall unsafe "nn_lambda_free_all" c_nn_lambda_free_all :: IO ()

foreign import ccall unsafe "nn_primop_app_free_all" c_nn_primop_app_free_all :: IO ()

foreign import ccall unsafe "nn_list_free_all" c_nn_list_free_all :: IO ()

foreign import ccall unsafe "nn_attrset_free_all" c_nn_attrset_free_all :: IO ()

-- | The capacity hint that asks a C arena for its default size.
defaultCapacity :: Word32
defaultCapacity = 0

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize all C data layer arenas, as the outermost evaluation
-- session starts: symbol table, string arena, thunk arena, env slot
-- allocator, bytecode store.  The new symbol table interns upstream's
-- startup names ('startupSymbols') before anything else can, so that a
-- name gets the symbol upstream's would.
arenaInit :: IO ()
arenaInit = do
  c_nn_symbol_init defaultCapacity
  mapM_ symbolIntern startupSymbols
  c_nn_str_init
  c_nn_thunk_init defaultCapacity
  c_nn_env_init
  c_nn_bytecode_init defaultCapacity defaultCapacity

-- | Destroy all C arenas: the registered roots, the tracked objects
-- (strings with context, lambdas, partial applications, lists, sets),
-- then the bytecode store
-- with the code's strings, the env pages, the thunk blocks, the string
-- arena and the symbol table.
--
-- A second 'arenaInit' starts the evaluator afresh: no constant of the
-- program points into the memory freed here, since an evaluation
-- session builds its base environment and @derivation@ wrapper as it
-- starts ('Nix.Eval.IO.withEvalSession'), and the @null@, @true@, @false@
-- and empty list cells and the builtins' own cells are C statics that
-- survive every cycle.  A session's own values do not: one started
-- before the teardown is not used after it.
arenaDestroy :: IO ()
arenaDestroy = do
  c_nn_roots_clear
  c_nn_ctxstr_free_all
  c_nn_lambda_free_all
  c_nn_primop_app_free_all
  c_nn_list_free_all
  c_nn_attrset_free_all
  c_nn_bytecode_destroy
  c_nn_env_destroy
  c_nn_thunk_destroy
  c_nn_str_destroy
  c_nn_symbol_destroy
