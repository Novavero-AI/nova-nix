-- | Failure statuses at the C data-layer boundary.
--
-- Every C allocator reports failure through a sentinel or a status
-- instead of aborting: a NULL pointer, @UINT32_MAX@ from the bytecode
-- emitters, @NN_SYMBOL_INVALID@ from the symbol table, @-1@ from the
-- list constructors and the attribute set insert.  Each covers two
-- causes a caller needs to tell apart: the sub-arena is outside its
-- init .. destroy window (nothing ran 'Nix.Eval.Arena.arenaInit', or
-- 'Nix.Eval.Arena.arenaDestroy' already ran), or it is live and could
-- not grow.  Only the C side knows which, so the failure path asks it
-- once, through @nn_arena_live@, and raises a 'CStatusError' that names
-- the C entry point.  The question is asked only after a failure, so
-- the hot path pays the sentinel comparison and nothing more.
--
-- The thunk state transitions report a third kind of refusal: a thunk
-- not in the state the transition needs.  The boundary only asks for a
-- transition it has just made valid, so that refusal is a defect in this
-- library, raised as 'CInvariantViolated' rather than carried on.
module Nix.Eval.CStatus
  ( -- * Errors
    CStatusError (..),

    -- * Checks
    checkedCPtr,
    cStatusFailure,
    cInvariantViolated,

    -- * Arena state
    arenaLive,
  )
where

import Control.Exception (Exception (..), throwIO)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)

-- ---------------------------------------------------------------------------
-- FFI import
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nn_arena_live"
  c_nn_arena_live :: IO CInt

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Why a C data-layer call reported failure.  Each constructor carries
-- the C entry point that refused.
data CStatusError
  = -- | The call ran outside the 'Nix.Eval.Arena.arenaInit' ..
    -- 'Nix.Eval.Arena.arenaDestroy' window.  A setup diagnosis to act
    -- on by adding the bracket and starting the process again, not an
    -- error to catch and retry in place: GHC updates a thunk whose
    -- evaluation raised with that exception, so any shared value whose
    -- evaluation reached C before the window re-raises this inside a
    -- later, live arena.  The @null@, @true@ and @false@ thunks are C
    -- statics rather than arena cells, so they are not among them.
    ArenaNotInitialized !String
  | -- | The arena is live and the allocation itself failed: exhaustion,
    -- or a size the C side rejects.  The second field is the detail.
    CAllocationFailed !String !String
  | -- | The C side refused a state transition the boundary had just made
    -- valid, or reported a state no transition produces.  A defect in
    -- this library, never a resource failure; the second field says
    -- which state was wrong.
    CInvariantViolated !String !String
  deriving (Eq, Show)

instance Exception CStatusError where
  displayException (ArenaNotInitialized site) =
    site
      ++ ": the C data layer is not initialized; evaluation must run between "
      ++ "Nix.Eval.Arena.arenaInit and Nix.Eval.Arena.arenaDestroy"
  displayException (CAllocationFailed site detail) = site ++ ": " ++ detail
  displayException (CInvariantViolated site detail) =
    site ++ ": internal invariant violated: " ++ detail

-- ---------------------------------------------------------------------------
-- Checks
-- ---------------------------------------------------------------------------

-- | Reject a NULL from a C allocator before it can flow onward as a
-- pointer: the C side signals failure deliberately, and the next
-- dereference would be undefined behaviour in a release build.
checkedCPtr :: String -> Ptr a -> IO (Ptr a)
checkedCPtr site ptr
  | ptr == nullPtr = cStatusFailure site "C allocation failed"
  | otherwise = pure ptr

-- | Raise the 'CStatusError' for a sentinel @site@ just returned.  Asked
-- right after the failing call and before anything else touches the
-- arena, so the answer describes the state that call saw.
cStatusFailure :: String -> String -> IO a
cStatusFailure site detail = do
  live <- arenaLive
  throwIO (if live then CAllocationFailed site detail else ArenaNotInitialized site)

-- | Raise 'CInvariantViolated' for a transition @site@ refused.  Unlike
-- 'cStatusFailure' it does not ask whether the arena is live: the caller
-- holds a cell the arena handed out, so liveness is not the question.
cInvariantViolated :: String -> String -> IO a
cInvariantViolated site detail = throwIO (CInvariantViolated site detail)

-- ---------------------------------------------------------------------------
-- Arena state
-- ---------------------------------------------------------------------------

-- | Whether every C sub-arena is inside its init .. destroy window.
arenaLive :: IO Bool
arenaLive = (/= 0) <$> c_nn_arena_live
