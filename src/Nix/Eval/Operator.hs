-- | Binary and unary operator evaluation for Nix.
--
-- Short-circuiting operators ('OpAnd', 'OpOr', 'OpImpl') are handled
-- directly in @Nix.Eval.eval@ because they must not evaluate both
-- operands.  Everything else lives here.
module Nix.Eval.Operator
  ( evalBinary,
    evalUnary,
    nixCompare,
    nixEqual,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Eval.Types
  ( MonadEval (..),
    NixValue (..),
    Thunk,
    typeName,
  )
import Nix.Expr.Types (BinaryOp (..), UnaryOp (..))

-- | Force function passed by the caller to break the import cycle.
-- Needed for deep equality on lists and attribute sets.
type Force m = Thunk -> m NixValue

-- | Evaluate a binary operator on two forced values.
--
-- The caller must handle short-circuit operators ('OpAnd', 'OpOr',
-- 'OpImpl') before calling this.  The 'Force' function is used only
-- for deep structural equality on compound values.
evalBinary :: (MonadEval m) => Force m -> BinaryOp -> NixValue -> NixValue -> m NixValue
evalBinary forceThunk op left right = case op of
  OpAdd -> evalAdd left right
  OpSub -> evalArith "subtraction" (-) (-) left right
  OpMul -> evalArith "multiplication" (*) (*) left right
  OpDiv -> evalDiv left right
  OpEq -> VBool <$> nixEqual forceThunk left right
  OpNeq -> VBool . not <$> nixEqual forceThunk left right
  OpLt -> VBool <$> nixCompare left right
  OpLte -> do
    lt <- nixCompare left right
    eq <- nixEqual forceThunk left right
    pure (VBool (lt || eq))
  OpGt -> VBool <$> nixCompare right left
  OpGte -> do
    gt <- nixCompare right left
    eq <- nixEqual forceThunk left right
    pure (VBool (gt || eq))
  OpConcat -> evalConcat left right
  OpUpdate -> evalUpdate left right
  -- Short-circuit ops must be handled by the caller
  OpAnd -> throwEvalError "internal error: OpAnd should be handled by eval"
  OpOr -> throwEvalError "internal error: OpOr should be handled by eval"
  OpImpl -> throwEvalError "internal error: OpImpl should be handled by eval"

-- | Evaluate a unary operator on a forced value.
evalUnary :: (MonadEval m) => UnaryOp -> NixValue -> m NixValue
evalUnary OpNot val = case val of
  VBool b -> pure (VBool (not b))
  other -> throwEvalError ("cannot apply ! to " <> typeName other)
evalUnary OpNegate val = case val of
  VInt n -> pure (VInt (negate n))
  VFloat n -> pure (VFloat (negate n))
  other -> throwEvalError ("cannot negate " <> typeName other)

-- | Addition: int/float arithmetic, string concatenation, path append.
evalAdd :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalAdd (VInt a) (VInt b) = pure (VInt (a + b))
evalAdd (VInt a) (VFloat b) = pure (VFloat (fromInteger a + b))
evalAdd (VFloat a) (VInt b) = pure (VFloat (a + fromInteger b))
evalAdd (VFloat a) (VFloat b) = pure (VFloat (a + b))
evalAdd (VStr a ctxA) (VStr b ctxB) = pure (VStr (a <> b) (ctxA <> ctxB))
evalAdd (VPath a) (VStr b _) = pure (VPath (a <> b))
evalAdd left right =
  throwEvalError ("cannot add " <> typeName left <> " and " <> typeName right)

-- | Generic arithmetic for subtraction and multiplication.
evalArith ::
  (MonadEval m) =>
  Text ->
  (Integer -> Integer -> Integer) ->
  (Double -> Double -> Double) ->
  NixValue ->
  NixValue ->
  m NixValue
evalArith name intOp floatOp left right = case (left, right) of
  (VInt a, VInt b) -> pure (VInt (intOp a b))
  (VInt a, VFloat b) -> pure (VFloat (floatOp (fromInteger a) b))
  (VFloat a, VInt b) -> pure (VFloat (floatOp a (fromInteger b)))
  (VFloat a, VFloat b) -> pure (VFloat (floatOp a b))
  _ ->
    throwEvalError
      ( "cannot apply "
          <> name
          <> " to "
          <> typeName left
          <> " and "
          <> typeName right
      )

-- | Division with zero check.  Integer division uses 'quot'.
evalDiv :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalDiv left right = case (left, right) of
  (VInt _, VInt 0) -> throwEvalError "division by zero"
  (VInt a, VInt b) -> pure (VInt (quot a b))
  (VInt a, VFloat b)
    | b == 0 -> throwEvalError "division by zero"
    | otherwise -> pure (VFloat (fromInteger a / b))
  (VFloat _, VInt 0) -> throwEvalError "division by zero"
  (VFloat a, VInt b) -> pure (VFloat (a / fromInteger b))
  (VFloat a, VFloat b)
    | b == 0 -> throwEvalError "division by zero"
    | otherwise -> pure (VFloat (a / b))
  _ ->
    throwEvalError
      ( "cannot divide "
          <> typeName left
          <> " by "
          <> typeName right
      )

-- ---------------------------------------------------------------------------
-- Comparison and equality
-- ---------------------------------------------------------------------------

-- | Ordering comparison for < (reused for >, <=, >= via argument swap).
nixCompare :: (MonadEval m) => NixValue -> NixValue -> m Bool
nixCompare (VInt a) (VInt b) = pure (a < b)
nixCompare (VInt a) (VFloat b) = pure (fromInteger a < b)
nixCompare (VFloat a) (VInt b) = pure (a < fromInteger b)
nixCompare (VFloat a) (VFloat b) = pure (a < b)
-- String comparison ignores context (matching real Nix).
nixCompare (VStr a _) (VStr b _) = pure (a < b)
nixCompare left right =
  throwEvalError
    ( "cannot compare "
        <> typeName left
        <> " and "
        <> typeName right
    )

-- | Deep structural equality.  Forces thunks inside lists and
-- attribute sets as needed.
nixEqual :: (MonadEval m) => Force m -> NixValue -> NixValue -> m Bool
nixEqual _ (VInt a) (VInt b) = pure (a == b)
nixEqual _ (VInt a) (VFloat b) = pure (fromInteger a == b)
nixEqual _ (VFloat a) (VInt b) = pure (a == fromInteger b)
nixEqual _ (VFloat a) (VFloat b) = pure (a == b)
nixEqual _ (VBool a) (VBool b) = pure (a == b)
nixEqual _ VNull VNull = pure True
-- String equality ignores context (matching real Nix).
nixEqual _ (VStr a _) (VStr b _) = pure (a == b)
nixEqual _ (VPath a) (VPath b) = pure (a == b)
nixEqual forceThunk (VList as) (VList bs)
  | length as /= length bs = pure False
  | otherwise = listEqual forceThunk as bs
nixEqual forceThunk (VAttrs as) (VAttrs bs)
  | Map.keys as /= Map.keys bs = pure False
  | otherwise = do
      let pairs = zip (Map.elems as) (Map.elems bs)
      results <- mapM (thunkPairEqual forceThunk) pairs
      pure (and results)
nixEqual _ _ _ = pure False

-- | Pairwise equality of two thunk lists (for list comparison).
listEqual :: (MonadEval m) => Force m -> [Thunk] -> [Thunk] -> m Bool
listEqual _ [] [] = pure True
listEqual forceThunk (a : as) (b : bs) = do
  va <- forceThunk a
  vb <- forceThunk b
  eq <- nixEqual forceThunk va vb
  if eq then listEqual forceThunk as bs else pure False
listEqual _ _ _ = pure False

-- | Compare two thunks for equality by forcing both.
thunkPairEqual :: (MonadEval m) => Force m -> (Thunk, Thunk) -> m Bool
thunkPairEqual forceThunk (a, b) = do
  va <- forceThunk a
  vb <- forceThunk b
  nixEqual forceThunk va vb

-- ---------------------------------------------------------------------------
-- List / attrset operators
-- ---------------------------------------------------------------------------

-- | List concatenation (++).
evalConcat :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalConcat (VList as) (VList bs) = pure (VList (as ++ bs))
evalConcat left right =
  throwEvalError ("cannot concatenate " <> typeName left <> " and " <> typeName right)

-- | Attribute set merge (//).  Right-biased: keys in the right
-- operand shadow keys in the left.
evalUpdate :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalUpdate (VAttrs as) (VAttrs bs) = pure (VAttrs (Map.union bs as))
evalUpdate left right =
  throwEvalError ("cannot merge " <> typeName left <> " and " <> typeName right)
