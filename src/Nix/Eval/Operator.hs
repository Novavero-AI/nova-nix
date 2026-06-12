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

import Data.Int (Int64)
import Data.Text (Text)
import Nix.Eval.CAttrSet (cattrsetUnion)
import Nix.Eval.CList (clistFromThunks, clistLen, clistThunks)
import Nix.Eval.Types
  ( AttrSet (..),
    MonadEval (..),
    NixValue (..),
    Thunk (..),
    attrSetElems,
    attrSetKeys,
    thunkSameRef,
    typeName,
  )
import Nix.Expr.Types (BinaryOp (..), UnaryOp (..))
import System.IO.Unsafe (unsafePerformIO)

-- | Force function passed by the caller to break the import cycle.
-- Needed for deep equality on lists and attribute sets.
type Force m = Thunk -> m NixValue

-- | Evaluate a binary operator on two forced values.
--
-- The caller must handle short-circuit operators ('OpAnd', 'OpOr',
-- 'OpImpl') before calling this.  The @Force@ function is used only
-- for deep structural equality on compound values.
evalBinary :: (MonadEval m) => Force m -> BinaryOp -> NixValue -> NixValue -> m NixValue
evalBinary forceFn op left right = case op of
  OpAdd -> evalAdd left right
  OpSub -> evalArith "subtraction" (-) (-) left right
  OpMul -> evalArith "multiplication" (*) (*) left right
  OpDiv -> evalDiv left right
  OpEq -> VBool <$> nixEqual forceFn left right
  OpNeq -> VBool . not <$> nixEqual forceFn left right
  OpLt -> VBool <$> nixCompare forceFn left right
  OpLte -> do
    lt <- nixCompare forceFn left right
    eq <- nixEqual forceFn left right
    pure (VBool (lt || eq))
  OpGt -> VBool <$> nixCompare forceFn right left
  OpGte -> do
    gt <- nixCompare forceFn right left
    eq <- nixEqual forceFn left right
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
evalAdd (VInt a) (VFloat b) = pure (VFloat (fromIntegral a + b))
evalAdd (VFloat a) (VInt b) = pure (VFloat (a + fromIntegral b))
evalAdd (VFloat a) (VFloat b) = pure (VFloat (a + b))
evalAdd (VStr a ctxA) (VStr b ctxB) = pure (VStr (a <> b) (ctxA <> ctxB))
evalAdd (VPath a) (VStr b _) = pure (VPath (a <> b))
evalAdd (VPath a) (VPath b) = pure (VPath (a <> b))
evalAdd (VStr a ctxA) (VPath b) = pure (VStr (a <> b) ctxA)
evalAdd left right =
  throwEvalError ("cannot add " <> typeName left <> " and " <> typeName right)

-- | Generic arithmetic for subtraction and multiplication.
evalArith ::
  (MonadEval m) =>
  Text ->
  (Int64 -> Int64 -> Int64) ->
  (Double -> Double -> Double) ->
  NixValue ->
  NixValue ->
  m NixValue
evalArith name intOp floatOp left right = case (left, right) of
  (VInt a, VInt b) -> pure (VInt (intOp a b))
  (VInt a, VFloat b) -> pure (VFloat (floatOp (fromIntegral a) b))
  (VFloat a, VInt b) -> pure (VFloat (floatOp a (fromIntegral b)))
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

-- | Division with zero check.  Integer division uses 'quot'
-- (truncation toward zero, matching C++ Nix semantics).
evalDiv :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalDiv left right = case (left, right) of
  (VInt _, VInt 0) -> throwEvalError "division by zero"
  (VInt a, VInt b)
    -- quot minBound (-1) throws arithmetic overflow in Haskell;
    -- C++ Nix wraps silently (undefined behavior, wraps to minBound).
    | a == minBound && b == -1 -> pure (VInt minBound)
    | otherwise -> pure (VInt (quot a b))
  (VInt a, VFloat b)
    | b == 0 -> throwEvalError "division by zero"
    | otherwise -> pure (VFloat (fromIntegral a / b))
  (VFloat _, VInt 0) -> throwEvalError "division by zero"
  (VFloat a, VInt b) -> pure (VFloat (a / fromIntegral b))
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
nixCompare :: (MonadEval m) => Force m -> NixValue -> NixValue -> m Bool
nixCompare _ (VInt a) (VInt b) = pure (a < b)
nixCompare _ (VInt a) (VFloat b) = pure (fromIntegral a < b)
nixCompare _ (VFloat a) (VInt b) = pure (a < fromIntegral b)
nixCompare _ (VFloat a) (VFloat b) = pure (a < b)
-- String comparison ignores context (matching real Nix).
nixCompare _ (VStr a _) (VStr b _) = pure (a < b)
-- Paths compare as their string representation (Nix semantics).
nixCompare _ (VPath a) (VPath b) = pure (a < b)
-- Lists compare lexicographically, element by element (Nix semantics).
nixCompare forceFn (VList clA) (VList clB) =
  listCompare forceFn (map Thunk (clistThunks clA)) (map Thunk (clistThunks clB))
nixCompare _ left right =
  throwEvalError
    ( "cannot compare "
        <> typeName left
        <> " and "
        <> typeName right
    )

-- | Lexicographic comparison of two thunk lists for the @<@ operator: the
-- first differing element decides; a proper prefix is less than the longer
-- list.  Mirrors 'listEqual'.
listCompare :: (MonadEval m) => Force m -> [Thunk] -> [Thunk] -> m Bool
listCompare _ [] [] = pure False
listCompare _ [] (_ : _) = pure True
listCompare _ (_ : _) [] = pure False
listCompare forceFn (a : as) (b : bs)
  | thunkSameRef a b = listCompare forceFn as bs
  | otherwise = do
      va <- forceFn a
      vb <- forceFn b
      ltAB <- nixCompare forceFn va vb
      if ltAB
        then pure True
        else do
          ltBA <- nixCompare forceFn vb va
          if ltBA then pure False else listCompare forceFn as bs

-- | Deep structural equality.  Forces thunks inside lists and
-- attribute sets as needed.
nixEqual :: (MonadEval m) => Force m -> NixValue -> NixValue -> m Bool
nixEqual _ (VInt a) (VInt b) = pure (a == b)
nixEqual _ (VInt a) (VFloat b) = pure (fromIntegral a == b)
nixEqual _ (VFloat a) (VInt b) = pure (a == fromIntegral b)
nixEqual _ (VFloat a) (VFloat b) = pure (a == b)
nixEqual _ (VBool a) (VBool b) = pure (a == b)
nixEqual _ VNull VNull = pure True
-- String equality ignores context (matching real Nix).
nixEqual _ (VStr a _) (VStr b _) = pure (a == b)
nixEqual _ (VPath a) (VPath b) = pure (a == b)
nixEqual forceFn (VList clA) (VList clB)
  | clistLen clA /= clistLen clB = pure False
  | otherwise = listEqual forceFn (map Thunk (clistThunks clA)) (map Thunk (clistThunks clB))
nixEqual forceFn (VAttrs as) (VAttrs bs)
  | attrSetKeys as /= attrSetKeys bs = pure False
  | otherwise = do
      let pairs = zip (attrSetElems as) (attrSetElems bs)
      results <- mapM (thunkPairEqual forceFn) pairs
      pure (and results)
nixEqual _ _ _ = pure False

-- | Pairwise equality of two thunk lists (for list comparison).
listEqual :: (MonadEval m) => Force m -> [Thunk] -> [Thunk] -> m Bool
listEqual _ [] [] = pure True
listEqual forceFn (a : as) (b : bs)
  | thunkSameRef a b = listEqual forceFn as bs
  | otherwise = do
      va <- forceFn a
      vb <- forceFn b
      eq <- nixEqual forceFn va vb
      if eq then listEqual forceFn as bs else pure False
listEqual _ _ _ = pure False

-- | Compare two thunks for equality by forcing both.
-- Short-circuits on thunk identity (same IORef = same value).
thunkPairEqual :: (MonadEval m) => Force m -> (Thunk, Thunk) -> m Bool
thunkPairEqual forceFn (a, b)
  | thunkSameRef a b = pure True
  | otherwise = do
      va <- forceFn a
      vb <- forceFn b
      nixEqual forceFn va vb

-- ---------------------------------------------------------------------------
-- List / attrset operators
-- ---------------------------------------------------------------------------

-- | List concatenation (++).
evalConcat :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalConcat (VList clA) (VList clB) =
  pure (VList (clistFromThunks (clistThunks clA ++ clistThunks clB)))
evalConcat left right =
  throwEvalError ("cannot concatenate " <> typeName left <> " and " <> typeName right)

-- | Attribute set merge (//).  Right-biased: keys in the right
-- operand shadow keys in the left.
--
-- When one side is a 'LazyAttrs', avoid full materialization by
-- merging binding recipes directly.  This is critical for nixpkgs
-- where the overlay system does @big_set // small_set@.
evalUpdate :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalUpdate (VAttrs as) (VAttrs bs) = pure (VAttrs (mergeAttrSets as bs))
evalUpdate left right =
  throwEvalError ("cannot merge " <> typeName left <> " and " <> typeName right)

-- | Merge two 'AttrSet's, right-biased (@//@).
-- Delegates to C-side @nn_attrset_union@ which performs a linear merge
-- of two sorted arrays - O(n+m) on contiguous, cache-friendly memory.
--
-- 'unsafePerformIO' safety: @nn_attrset_union@ is a pure C function
-- that allocates a new result set from its two inputs without side
-- effects, callbacks to Haskell, or dependency on mutable state
-- beyond the C allocator.  The NOINLINE pragma prevents float-out
-- from sharing results across distinct call sites.
{-# NOINLINE mergeAttrSets #-}
mergeAttrSets :: AttrSet -> AttrSet -> AttrSet
mergeAttrSets (AttrSet a) (AttrSet b) =
  AttrSet (unsafePerformIO (cattrsetUnion a b))
