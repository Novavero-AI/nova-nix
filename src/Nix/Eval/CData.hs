{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The evaluator's boundary with the C data layer.
--
-- Thunks, environments, attribute sets, lists, lambdas, the bytes of
-- strings and paths, strings with context, interned names and the
-- bytecode the evaluator runs all live in C (@cbits/@), reached through
-- the bindings in the @Nix.Eval.C*@ modules, "Nix.Eval.Symbol" and
-- "Nix.Eval.SourcePos".  Every read of,
-- write to and allocation in that memory the evaluator makes is a
-- t'CData' action, and the evaluator runs one in its own monad through
-- 'MonadCData'; the arena's lifecycle, the regions, the census and the
-- counters ('Nix.Eval.Symbol.symbolCount') call the layer in 'IO'.  An action
-- runs where the evaluation reaches it, in order with the actions around
-- it: a read sees memory as it is at that point and not whenever a lazy
-- value happens to be demanded, and an allocation happens once each time
-- the evaluation passes it, never shared by GHC between two passes.
-- A collector of the layer's memory needs that order: nothing still
-- unevaluated can read a cell at a point the evaluation cannot see.
--
-- Two functions read the layer outside a monad, each for one kind of
-- use, with the invariant that makes it sound:
--
-- * 'readImmutable' reads an interned symbol's bytes ("Nix.Eval.Symbol"),
--   which do not change once interned, until the arena is destroyed.
--   They can move: the table grows its byte storage with @realloc@ as
--   later strings are interned, and a read takes the bytes' address
--   before it copies them.  So no intern may run between the two, which
--   holds while one thread at a time uses the layer.
-- * 'runCDataPure' runs a whole computation as a value: a pure
--   evaluation ('Nix.Eval.Types.runPureEval').  Such a computation
--   writes only memory it allocated, the with-scope array entries it
--   resolves ('Nix.Eval.Types.resolveWithScopeAt'), each from a set's
--   thunk to the set the thunk evaluates to, and the static cell
--   of a builtin it binds ('Nix.Eval.CThunk.cthunkPrimop'), which it
--   fills with the one value that cell ever holds.  Two runs that resolve
--   one entry make two sets with the same contents in different cells, so
--   a value carried from one run into another can meet the other run's
--   set where it would have met its own, which the repeated marker and
--   @==@'s shortcut for one cell tell apart.  Only an environment whose
--   with-scope is still unresolved when it passes between runs can show
--   this, and only tests pass one.
--
-- Whatever runs, reads or allocates through these outside an evaluation
-- session ('Nix.Eval.IO.withEvalSession') raises
-- 'Nix.Eval.CStatus.ArenaNotInitialized' or reads freed memory, as any
-- other action does.
module Nix.Eval.CData
  ( -- * Actions
    CData,
    runCData,
    MonadCData (..),
    cdataIO,

    -- * Outside a monad
    readImmutable,
    runCDataPure,
  )
where

import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

-- | An action on the C data layer: an FFI call into @cbits/@, or a peek
-- or poke of memory the layer handed out.  Nothing else is wrapped, so a
-- function whose result is a t'CData' touches nothing but that memory.
-- The constructor is not exported: the modules that bind the layer make
-- an action with 'cdataIO', and 'runCData', 'readImmutable' and
-- 'runCDataPure' run one.
newtype CData a = CData (IO a)
  deriving (Functor, Applicative, Monad)

-- | Run an action in 'IO', for the 'MonadCData' instance of a monad over
-- 'IO'.
runCData :: CData a -> IO a
runCData (CData io) = io
{-# INLINE runCData #-}

-- | A monad that runs C data-layer actions in sequence with its own.
-- The evaluator's monads ('Nix.Eval.Types.MonadEval') are instances, and
-- so is 'IO', for the executable and the tests.
class (Monad m) => MonadCData m where
  -- | Run the action at this point of the computation.
  liftCData :: CData a -> m a

instance MonadCData IO where
  liftCData = runCData
  {-# INLINE liftCData #-}

instance MonadCData CData where
  liftCData = id
  {-# INLINE liftCData #-}

-- | Run a binding's IO action as a t'CData' action in any 'MonadCData'.
-- For the modules that bind the layer, whose IO actions are FFI calls and
-- peeks and pokes of its memory.
cdataIO :: (MonadCData m) => IO a -> m a
cdataIO = liftCData . CData
{-# INLINE cdataIO #-}

-- | Read memory whose contents do not change while the value naming it
-- exists, until the arena is destroyed, and which nothing moves while the
-- read runs (see the module header).  The read may run any number of
-- times, or never: every run answers the same.
readImmutable :: CData a -> a
readImmutable (CData io) = unsafeDupablePerformIO io
{-# INLINE readImmutable #-}

-- | Run a computation that allocates as a value.  Sound for a computation
-- that writes only memory it allocated, with-scope entries it resolves,
-- with what two runs resolving one entry can show, and builtins' static
-- cells, which every run fills alike (see the module header): each run
-- allocates memory of its own, and a run GHC shares or never makes is not
-- observable except through the addresses of the values it returns.
runCDataPure :: CData a -> a
runCDataPure (CData io) = unsafePerformIO io
