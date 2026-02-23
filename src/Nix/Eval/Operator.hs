-- | Binary and unary operator evaluation for Nix.
--
-- Short-circuiting operators ('OpAnd', 'OpOr', 'OpImpl') are handled
-- directly in 'Nix.Eval.eval' because they must not evaluate both
-- operands.  Everything else lives here.
module Nix.Eval.Operator
  ( evalBinary,
    evalUnary,
    nixEqual,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Eval.Types
  ( NixValue (..),
    Thunk,
    typeName,
  )
import Nix.Expr.Types (BinaryOp (..), UnaryOp (..))

-- | Force function passed by the caller to break the import cycle.
-- Needed for deep equality on lists and attribute sets.
type Force = Thunk -> Either Text NixValue

-- | Evaluate a binary operator on two forced values.
--
-- The caller must handle short-circuit operators ('OpAnd', 'OpOr',
-- 'OpImpl') before calling this.  The 'Force' function is used only
-- for deep structural equality on compound values.
evalBinary :: Force -> BinaryOp -> NixValue -> NixValue -> Either Text NixValue
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
  OpAnd -> Left "internal error: OpAnd should be handled by eval"
  OpOr -> Left "internal error: OpOr should be handled by eval"
  OpImpl -> Left "internal error: OpImpl should be handled by eval"

-- | Evaluate a unary operator on a forced value.
evalUnary :: UnaryOp -> NixValue -> Either Text NixValue
evalUnary OpNot val = case val of
  VBool b -> Right (VBool (not b))
  other -> Left ("cannot apply ! to " <> typeName other)
evalUnary OpNegate val = case val of
  VInt n -> Right (VInt (negate n))
  VFloat n -> Right (VFloat (negate n))
  other -> Left ("cannot negate " <> typeName other)

-- | Addition: int/float arithmetic, string concatenation, path append.
evalAdd :: NixValue -> NixValue -> Either Text NixValue
evalAdd (VInt a) (VInt b) = Right (VInt (a + b))
evalAdd (VInt a) (VFloat b) = Right (VFloat (fromInteger a + b))
evalAdd (VFloat a) (VInt b) = Right (VFloat (a + fromInteger b))
evalAdd (VFloat a) (VFloat b) = Right (VFloat (a + b))
evalAdd (VStr a) (VStr b) = Right (VStr (a <> b))
evalAdd (VPath a) (VStr b) = Right (VPath (a <> b))
evalAdd left right =
  Left ("cannot add " <> typeName left <> " and " <> typeName right)

-- | Generic arithmetic for subtraction and multiplication.
evalArith ::
  Text ->
  (Integer -> Integer -> Integer) ->
  (Double -> Double -> Double) ->
  NixValue ->
  NixValue ->
  Either Text NixValue
evalArith name intOp floatOp left right = case (left, right) of
  (VInt a, VInt b) -> Right (VInt (intOp a b))
  (VInt a, VFloat b) -> Right (VFloat (floatOp (fromInteger a) b))
  (VFloat a, VInt b) -> Right (VFloat (floatOp a (fromInteger b)))
  (VFloat a, VFloat b) -> Right (VFloat (floatOp a b))
  _ ->
    Left
      ( "cannot apply "
          <> name
          <> " to "
          <> typeName left
          <> " and "
          <> typeName right
      )

-- | Division with zero check.  Integer division uses 'quot'.
evalDiv :: NixValue -> NixValue -> Either Text NixValue
evalDiv left right = case (left, right) of
  (VInt _, VInt 0) -> Left "division by zero"
  (VInt a, VInt b) -> Right (VInt (quot a b))
  (VInt a, VFloat b)
    | b == 0 -> Left "division by zero"
    | otherwise -> Right (VFloat (fromInteger a / b))
  (VFloat _, VInt 0) -> Left "division by zero"
  (VFloat a, VInt b) -> Right (VFloat (a / fromInteger b))
  (VFloat a, VFloat b)
    | b == 0 -> Left "division by zero"
    | otherwise -> Right (VFloat (a / b))
  _ ->
    Left
      ( "cannot divide "
          <> typeName left
          <> " by "
          <> typeName right
      )

-- ---------------------------------------------------------------------------
-- Comparison and equality
-- ---------------------------------------------------------------------------

-- | Ordering comparison for < (reused for >, <=, >= via argument swap).
nixCompare :: NixValue -> NixValue -> Either Text Bool
nixCompare (VInt a) (VInt b) = Right (a < b)
nixCompare (VInt a) (VFloat b) = Right (fromInteger a < b)
nixCompare (VFloat a) (VInt b) = Right (a < fromInteger b)
nixCompare (VFloat a) (VFloat b) = Right (a < b)
nixCompare (VStr a) (VStr b) = Right (a < b)
nixCompare left right =
  Left
    ( "cannot compare "
        <> typeName left
        <> " and "
        <> typeName right
    )

-- | Deep structural equality.  Forces thunks inside lists and
-- attribute sets as needed.
nixEqual :: Force -> NixValue -> NixValue -> Either Text Bool
nixEqual _ (VInt a) (VInt b) = Right (a == b)
nixEqual _ (VInt a) (VFloat b) = Right (fromInteger a == b)
nixEqual _ (VFloat a) (VInt b) = Right (a == fromInteger b)
nixEqual _ (VFloat a) (VFloat b) = Right (a == b)
nixEqual _ (VBool a) (VBool b) = Right (a == b)
nixEqual _ VNull VNull = Right True
nixEqual _ (VStr a) (VStr b) = Right (a == b)
nixEqual _ (VPath a) (VPath b) = Right (a == b)
nixEqual forceThunk (VList as) (VList bs)
  | length as /= length bs = Right False
  | otherwise = listEqual forceThunk as bs
nixEqual forceThunk (VAttrs as) (VAttrs bs)
  | Map.keys as /= Map.keys bs = Right False
  | otherwise = do
      let pairs = zip (Map.elems as) (Map.elems bs)
      results <- mapM (thunkPairEqual forceThunk) pairs
      pure (and results)
nixEqual _ _ _ = Right False

-- | Pairwise equality of two thunk lists (for list comparison).
listEqual :: Force -> [Thunk] -> [Thunk] -> Either Text Bool
listEqual _ [] [] = Right True
listEqual forceThunk (a : as) (b : bs) = do
  va <- forceThunk a
  vb <- forceThunk b
  eq <- nixEqual forceThunk va vb
  if eq then listEqual forceThunk as bs else Right False
listEqual _ _ _ = Right False

-- | Compare two thunks for equality by forcing both.
thunkPairEqual :: Force -> (Thunk, Thunk) -> Either Text Bool
thunkPairEqual forceThunk (a, b) = do
  va <- forceThunk a
  vb <- forceThunk b
  nixEqual forceThunk va vb

-- ---------------------------------------------------------------------------
-- List / attrset operators
-- ---------------------------------------------------------------------------

-- | List concatenation (++).
evalConcat :: NixValue -> NixValue -> Either Text NixValue
evalConcat (VList as) (VList bs) = Right (VList (as ++ bs))
evalConcat left right =
  Left ("cannot concatenate " <> typeName left <> " and " <> typeName right)

-- | Attribute set merge (//).  Right-biased: keys in the right
-- operand shadow keys in the left.
evalUpdate :: NixValue -> NixValue -> Either Text NixValue
evalUpdate (VAttrs as) (VAttrs bs) = Right (VAttrs (Map.union bs as))
evalUpdate left right =
  Left ("cannot merge " <> typeName left <> " and " <> typeName right)
