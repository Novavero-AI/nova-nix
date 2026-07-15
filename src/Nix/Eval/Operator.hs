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
    checkedAdd,
    checkedSub,
    checkedMul,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.CAttrSet (cattrsetUnion)
import Nix.Eval.CList (clistFromThunks, clistLen, clistThunks)
import Nix.Eval.Types
  ( AttrSet (..),
    MonadEval (..),
    NixValue (..),
    Thunk (..),
    attrSetElems,
    attrSetKeys,
    attrSetLookup,
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
  OpSub -> evalArith "subtraction" checkedSub (-) left right
  OpMul -> evalArith "multiplication" checkedMul (*) left right
  OpDiv -> evalDiv left right
  OpEq -> VBool <$> nixEqual forceFn left right
  OpNeq -> VBool . not <$> nixEqual forceFn left right
  OpLt -> VBool <$> nixCompare forceFn left right
  -- <= and >= are negated swapped <, never (< or ==): upstream's parser
  -- desugars them that way (parser.y: a <= b becomes !(b < a)), which
  -- fixes NaN (nan <= x is true), matches the swapped operand order in
  -- incomparable-type errors, and needs one comparison instead of two.
  OpLte -> VBool . not <$> nixCompare forceFn right left
  OpGt -> VBool <$> nixCompare forceFn right left
  OpGte -> VBool . not <$> nixCompare forceFn left right
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
  -- negate minBound has no Int64 representation; upstream desugars unary
  -- minus to 0 - n, so it reports the same checked-subtraction overflow.
  VInt n -> either throwEvalError (pure . VInt) (checkedSub 0 n)
  VFloat n -> pure (VFloat (negate n))
  other -> throwEvalError ("cannot negate " <> typeName other)

-- | Addition: int/float arithmetic and string concatenation.  Path
-- operands never reach here - @Nix.Eval.evalAddWithCoercion@ handles them
-- (store-copy coercion for @string + path@, context checks for
-- @path + string@) before delegating.
evalAdd :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalAdd (VInt a) (VInt b) = either throwEvalError (pure . VInt) (checkedAdd a b)
evalAdd (VInt a) (VFloat b) = pure (VFloat (fromIntegral a + b))
evalAdd (VFloat a) (VInt b) = pure (VFloat (a + fromIntegral b))
evalAdd (VFloat a) (VFloat b) = pure (VFloat (a + b))
evalAdd (VStr a ctxA) (VStr b ctxB) = pure (VStr (a <> b) (ctxA <> ctxB))
evalAdd left right =
  throwEvalError ("cannot add " <> typeName left <> " and " <> typeName right)

-- | Checked Int64 arithmetic: integer overflow is an eval error (Nix
-- 2.24 semantics), never a two's-complement wrap.  Computed in Integer
-- and bounds-checked.
checkedIntOp :: Text -> (Integer -> Integer -> Integer) -> Int64 -> Int64 -> Either Text Int64
checkedIntOp verb op a b
  | wide < toInteger (minBound :: Int64) || wide > toInteger (maxBound :: Int64) =
      Left
        ( "integer overflow in "
            <> verb
            <> " "
            <> T.pack (show a)
            <> " and "
            <> T.pack (show b)
        )
  | otherwise = Right (fromInteger wide)
  where
    wide = op (toInteger a) (toInteger b)

checkedAdd :: Int64 -> Int64 -> Either Text Int64
checkedAdd = checkedIntOp "adding" (+)

checkedSub :: Int64 -> Int64 -> Either Text Int64
checkedSub = checkedIntOp "subtracting" (-)

checkedMul :: Int64 -> Int64 -> Either Text Int64
checkedMul = checkedIntOp "multiplying" (*)

-- | Generic arithmetic for subtraction and multiplication.  The integer
-- side is a checked op ('checkedSub' / 'checkedMul').
evalArith ::
  (MonadEval m) =>
  Text ->
  (Int64 -> Int64 -> Either Text Int64) ->
  (Double -> Double -> Double) ->
  NixValue ->
  NixValue ->
  m NixValue
evalArith name checkedOp floatOp left right = case (left, right) of
  (VInt a, VInt b) -> either throwEvalError (pure . VInt) (checkedOp a b)
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
    -- The one overflowing division: |minBound| has no representation.
    | a == minBound && b == -1 ->
        throwEvalError
          ("integer overflow in dividing " <> T.pack (show a) <> " and " <> T.pack (show b))
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
nixEqual forceFn (VAttrs as) (VAttrs bs) = do
  drvOutPaths <- derivationOutPathPair forceFn as bs
  case drvOutPaths of
    Just outPathPair -> thunkPairEqual forceFn outPathPair
    Nothing
      | attrSetKeys as /= attrSetKeys bs -> pure False
      | otherwise ->
          -- Short-circuit on the first mismatch: later pairs are never
          -- forced, so errors past the deciding pair cannot surface
          -- (upstream stops comparing there too).
          allPairsEqual (zip (attrSetElems as) (attrSetElems bs))
  where
    allPairsEqual [] = pure True
    allPairsEqual (pair : rest) = do
      eq <- thunkPairEqual forceFn pair
      if eq then allPairsEqual rest else pure False
nixEqual _ _ _ = pure False

-- | When both attr sets are derivations (a @type@ attr forcing to the string
-- @"derivation"@) and both carry an @outPath@, the pair of outPath thunks.
--
-- C++ Nix's eqValues compares derivations by outPath ALONE, before any
-- key-set comparison: two mkDerivation results with the same outPath are
-- equal even though their lambda attrs (override, overrideAttrs) never are,
-- and distinct self-referential finalAttrs packages would otherwise recurse
-- forever.  If either set lacks an outPath, fall through to deep comparison,
-- exactly as upstream does.
derivationOutPathPair :: (MonadEval m) => Force m -> AttrSet -> AttrSet -> m (Maybe (Thunk, Thunk))
derivationOutPathPair forceFn as bs = do
  leftIsDrv <- isDerivationSet forceFn as
  if not leftIsDrv
    then pure Nothing
    else do
      rightIsDrv <- isDerivationSet forceFn bs
      pure $
        if rightIsDrv
          then (,) <$> attrSetLookup "outPath" as <*> attrSetLookup "outPath" bs
          else Nothing

-- | Does the set carry @type = "derivation"@?  Forces only the @type@ attr
-- (as upstream's isDerivation does); a non-string type is simply not a
-- derivation, not an error.
isDerivationSet :: (MonadEval m) => Force m -> AttrSet -> m Bool
isDerivationSet forceFn attrs =
  case attrSetLookup "type" attrs of
    Nothing -> pure False
    Just typeThunk -> do
      typeVal <- forceFn typeThunk
      case typeVal of
        VStr tag _ -> pure (tag == "derivation")
        _ -> pure False

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
