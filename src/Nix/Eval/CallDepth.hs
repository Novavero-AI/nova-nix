-- | The function-call nesting ceiling: upstream's @max-call-depth@.
--
-- @EvalState::callFunction@ keeps a count of the frames active around the
-- current evaluation and refuses a new call once that count exceeds
-- @settings.maxCallDepth@ (eval.cc at 2.24.9).  The check runs on entry,
-- before the callee is even forced, so the count is the number of frames
-- ALREADY active: with a ceiling of @n@, @n + 1@ nested calls succeed and
-- the @n + 2@-th is refused.  The default is 10000 (eval-settings.hh).
--
-- What costs a frame is whatever upstream routes through @callFunction@:
-- every application of a lambda, a builtin (also when partially applied),
-- a functor (two frames, the inner call being a @callFunction@ of its own),
-- a @__toString@ coercion, and the operators upstream's parser desugars
-- into builtin calls (@-@, @*@, @/@, @<@, @<=@, @>@, @>=@ and unary @-@).
-- Forcing a thunk costs nothing, so a value forced inside a frame is
-- evaluated at that frame's depth.  A builtin's deferred application
-- ('Nix.Expr.Types.EDeferredApp', upstream's app value as @map@,
-- @genList@, @mapAttrs@ and @zipAttrsWith@ build it) is applied where its
-- result is forced, and the frame opens before its function is forced, so
-- @mapAttrs@'s @f name value@, two nested app values upstream, runs the
-- name application a frame below the value application.  A
-- @builtins.sort@ whose comparator is @builtins.lessThan@ itself opens no
-- frame per comparison: upstream's @prim_sort@ bypasses @callFunction@
-- for it.
--
-- One divergence, by choice: upstream's @derivation@ is a Nix-language
-- wrapper (derivation.nix) whose body reaches an output through
-- @builtins.head@ over a @map@, three frames, and whose @outPath@ is a
-- @getAttr@ over @derivationStrict@, two; here @derivation@ is a builtin,
-- one frame, and @outPath@ selects from the one @derivationStrict@
-- application, one.  Those calls are the wrapper's implementation, not
-- the language, and upstream has already rewritten it once since 2.24,
-- so they are not imitated.
--
-- The count is evaluator state, carried per frame by each 'Nix.Eval.Types.MonadEval'
-- instance; this module holds the rule and the setting's default so the
-- evaluators and the config cascade agree on both.
module Nix.Eval.CallDepth
  ( CallDepth (..),
    defaultMaxCallDepth,
    topLevelCallDepth,
    enterCallFrame,
    maxCallDepthExceeded,
  )
where

import Data.Text (Text)
import Data.Word (Word32)

-- | The frames active around the current evaluation, and the ceiling they
-- may not exceed.  'Word32' because upstream's setting is an
-- @unsigned int@, so a configured ceiling has exactly that range.
data CallDepth = CallDepth
  { cdActive :: !Word32,
    cdCeiling :: !Word32
  }
  deriving (Eq, Show)

-- | Upstream's default @max-call-depth@.
defaultMaxCallDepth :: Word32
defaultMaxCallDepth = 10000

-- | No frames active, under the given ceiling: where an evaluation starts.
topLevelCallDepth :: Word32 -> CallDepth
topLevelCallDepth limit = CallDepth {cdActive = 0, cdCeiling = limit}

-- | The depth a new call frame runs at, or upstream's error when the frames
-- already active exceed the ceiling.
enterCallFrame :: CallDepth -> Either Text CallDepth
enterCallFrame depth@(CallDepth active limit)
  | active > limit = Left maxCallDepthExceeded
  | otherwise = Right depth {cdActive = active + 1}

-- | Upstream's message, verbatim.
maxCallDepthExceeded :: Text
maxCallDepthExceeded = "stack overflow; max-call-depth exceeded"
