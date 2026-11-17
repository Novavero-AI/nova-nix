-- | The C data layer's arena lifecycle, which an evaluation session owns
-- ('Nix.Eval.IO.withEvalSession'): 'arenaInit' sets up every C sub-arena
-- (symbol table, string arena, thunk arena, env pages, bytecode store) as
-- the outermost session starts, and 'arenaDestroy' tears them down as it
-- ends.  Every value lives in C, so the teardown frees a session's values
-- whole, with nothing in GHC's heap to release.  The module is internal to
-- the library: no caller sets the arena up or tears it down.
--
-- Outside a session the C layer has no memory to hand out, and every path
-- into it ('Nix.Builtins.builtinEnv', 'Nix.Eval.eval', 'Nix.Eval.force',
-- the value constructors) raises 'Nix.Eval.CStatus.ArenaNotInitialized'
-- naming the C entry point that refused, rather than dereferencing state
-- that does not exist.  Parsing ("Nix.Parser") is pure and needs no
-- arena.  The @null@, @true@, @false@ and empty list cells and each
-- builtin's own cell are C statics outside every arena, so forcing one
-- needs no session and survives every one.
module Nix.Eval.Arena
  ( arenaInit,
    arenaDestroy,
  )
where

import Nix.Eval.CBytecode (cbcDestroy, cbcInit)
import Nix.Eval.CCtxStr (cctxstrFreeAll)
import Nix.Eval.CEnv (cenvDestroy, cenvInit)
import Nix.Eval.CLambda (clambdaFreeAll)
import Nix.Eval.CList (clistFreeAll)
import Nix.Eval.CPrimop (cprimopAppFreeAll)
import Nix.Eval.CRoots (crootsClear)
import Nix.Eval.CStr (cstrDestroy, cstrInit)
import Nix.Eval.CThunk (cthunkDestroy, cthunkInit)
import Nix.Eval.StartupSymbols (startupSymbols)
import Nix.Eval.Symbol (symbolDestroy, symbolInit, symbolIntern)

-- ---------------------------------------------------------------------------
-- FFI import for bulk CAttrSet cleanup
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_attrset_free_all"
  cattrsetFreeAll :: IO ()

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Initialize all C data layer arenas.  Call once before evaluation.
-- Initializes: symbol table, string arena, thunk arena, env slot
-- allocator, bytecode store.  The new symbol table interns upstream's
-- startup names ('startupSymbols') before anything else can, so that a
-- name gets the symbol upstream's would.
arenaInit :: IO ()
arenaInit = do
  symbolInit 0
  mapM_ symbolIntern startupSymbols
  cstrInit
  cthunkInit 0
  cenvInit
  cbcInit 0 0

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
  crootsClear
  cctxstrFreeAll
  clambdaFreeAll
  cprimopAppFreeAll
  clistFreeAll
  cattrsetFreeAll
  cbcDestroy
  cenvDestroy
  cthunkDestroy
  cstrDestroy
  symbolDestroy
