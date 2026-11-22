{-# LANGUAGE LambdaCase #-}

-- | Calling the function a command line's target evaluates to, with the
-- values @--arg@ and @--argstr@ give: upstream's @autoCallFunction@
-- (eval.cc:1767-1814 at 2.24.9).
--
-- Only a function with a set pattern is called.  One with an ellipsis is
-- passed every argument; any other is passed the arguments its formals
-- name, and a formal with neither an argument nor a default fails before
-- the call, with upstream's message.  A set with @__functor@ is the
-- function its functor gives, called the same way, and anything else is
-- left as it is.
--
-- Upstream points that failure at the formal; a formal here carries no
-- position, so the message stands alone.
module Nix.Eval.AutoCall
  ( AutoArgs,
    autoCallFunction,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval (MonadEval (..), NixValue (..), Thunk, attrSetFromMap, attrSetLookup, deferApply, evaluated, force)
import Nix.Eval.AllocSite (SetSite (SetRoot))
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..), hasDefault)
import Nix.Eval.Symbol (symbolText)

-- | The arguments by name.  A call shares each cell, so an argument the
-- function never uses is never evaluated.
type AutoArgs = Map Text Thunk

-- | The attribute that makes a set callable.
functorName :: Text
functorName = "__functor"

-- | The value the function a target evaluates to gives when called with
-- the arguments, or the value itself when it is no function with a set
-- pattern.
autoCallFunction :: (MonadEval m) => AutoArgs -> NixValue -> m NixValue
autoCallFunction args fun = case fun of
  VAttrs attrs ->
    attrSetLookup functorName attrs >>= \case
      Just functorThunk -> do
        functor <- force functorThunk
        self <- evaluated fun
        deferApply functor self >>= force >>= autoCallFunction args
      Nothing -> pure fun
  VLambda _ (EFSet _ formals ellipsis) _ -> callWith formals ellipsis
  VLambda _ (EFNamedSet _ _ formals ellipsis) _ -> callWith formals ellipsis
  _ -> pure fun
  where
    callWith formals ellipsis = do
      passed <- if ellipsis then pure args else taken formals
      argSet <- attrSetFromMap SetRoot passed
      evaluated (VAttrs argSet) >>= deferApply fun >>= force
    -- Upstream walks the formals in symbol order and fails at the first
    -- it cannot fill (eval.cc:1798-1810).
    taken formals = case [efName formal | formal <- formals, not (Map.member (symbolText (efName formal)) args), not (hasDefault (efDefault formal))] of
      [] -> pure (Map.restrictKeys args (Set.fromList (map (symbolText . efName) formals)))
      missing : more -> throwEvalError (missingArgumentMessage (symbolText (minimum (missing :| more))))

-- | Upstream's @MissingArgumentError@ for a formal no argument or default
-- fills (eval.cc:1803-1807 at 2.24.9).
missingArgumentMessage :: Text -> Text
missingArgumentMessage name =
  T.intercalate
    "\n"
    [ "cannot evaluate a function that has an argument without a value ('" <> name <> "')",
      "Nix attempted to evaluate a function as a top level expression; in",
      "this case it must have its arguments supplied either by default",
      "values, or passed explicitly with '--arg' or '--argstr'. See",
      "https://nixos.org/manual/nix/stable/language/constructs.html#functions."
    ]
