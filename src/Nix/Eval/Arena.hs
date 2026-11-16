-- | Unified arena lifecycle for the C data layer.
--
-- Initializes and destroys all C sub-arenas (symbol table, string arena,
-- thunk arena, env slot allocator, bytecode store) with a single pair of
-- calls.  Every value lives in C, so tearing the arenas down frees an
-- evaluation's values whole, with nothing in GHC's heap to release.
--
-- Everything that evaluates runs between the two calls:
--
-- @
-- bracket_ arenaInit arenaDestroy $ do
--   ... evaluation ...
-- @
--
-- Outside that window the C layer has no memory to hand out, and every
-- path into it ('Nix.Builtins.builtinEnv', 'Nix.Eval.eval',
-- 'Nix.Eval.force', the value constructors) raises
-- 'ArenaNotInitialized' naming the C entry point that refused, rather
-- than dereferencing state that does not exist.  Parsing ("Nix.Parser")
-- is pure and needs no arena.
--
-- 'ArenaNotInitialized' is a setup diagnosis: add the bracket and start
-- the process again.  It is not an error to catch and retry in the same
-- process, because GHC updates a thunk whose evaluation raised with that
-- exception: any shared value whose evaluation reached C before the
-- window re-raises it inside a later, live arena.  The @null@, @true@
-- and @false@ thunks and each builtin's own cell are C statics outside
-- every arena, so forcing one needs no window and survives every cycle.
module Nix.Eval.Arena
  ( -- * Lifecycle
    arenaInit,
    arenaDestroy,
    arenaLive,

    -- * Errors
    CStatusError (..),
  )
where

import Nix.Eval.CBytecode (cbcDestroy, cbcInit)
import Nix.Eval.CCtxStr (cctxstrFreeAll)
import Nix.Eval.CEnv (cenvDestroy, cenvInit)
import Nix.Eval.CLambda (clambdaFreeAll)
import Nix.Eval.CList (clistFreeAll)
import Nix.Eval.CPrimop (cprimopAppFreeAll)
import Nix.Eval.CRoots (crootsClear)
import Nix.Eval.CStatus (CStatusError (..), arenaLive)
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
-- starts ('Nix.Eval.IO.newEvalState'), and the @null@, @true@, @false@
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
