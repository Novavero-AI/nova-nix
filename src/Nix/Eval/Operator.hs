-- | Binary and unary operator evaluation for Nix.
--
-- Short-circuiting operators ('OpAnd', 'OpOr', 'OpImpl') are handled
-- directly in @Nix.Eval.eval@ because they must not evaluate both
-- operands.  Everything else lives here.
module Nix.Eval.Operator
  ( evalBinary,
    evalUnary,
    evalUpdate,
    addToInteger,
    addToFloat,
    nixCompare,
    nixEqual,
    primAdd,
    primSub,
    primMul,
    primDiv,
    expectInt,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.CAttrSet (cattrsetUnion)
import Nix.Eval.CList (clistFromThunks, clistLen, clistThunks)
import Nix.Eval.Print (PrintOptions (..), printValue)
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
    typeOfValue,
  )
import Nix.Expr.Types (BinaryOp (..), UnaryOp (..))
import System.IO.Unsafe (unsafePerformIO)

-- | Force function passed by the caller to break the import cycle.
-- Needed for deep equality on lists and attribute sets.
type Force m = Thunk -> m NixValue

-- | Evaluate a binary operator on two forced values.
--
-- The caller must handle 'OpAdd' and the short-circuit operators
-- ('OpAnd', 'OpOr', 'OpImpl') before calling this: each looks at its left
-- operand before the right is evaluated, and @+@ coerces through the
-- evaluator.  The @Force@ function is used only for deep structural
-- equality on compound values.
evalBinary :: (MonadEval m) => Force m -> BinaryOp -> NixValue -> NixValue -> m NixValue
evalBinary forceFn op left right = case op of
  OpSub -> primSub left right
  OpMul -> primMul left right
  OpDiv -> primDiv left right
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
  OpUpdate -> evalUpdate (pure left) (pure right)
  -- Left-first ops must be handled by the caller
  OpAdd -> throwEvalError "internal error: OpAdd should be handled by eval"
  OpAnd -> throwEvalError "internal error: OpAnd should be handled by eval"
  OpOr -> throwEvalError "internal error: OpOr should be handled by eval"
  OpImpl -> throwEvalError "internal error: OpImpl should be handled by eval"

-- | Evaluate a unary operator on a forced value.
evalUnary :: (MonadEval m) => UnaryOp -> NixValue -> m NixValue
evalUnary OpNot val = case val of
  VBool b -> pure (VBool (not b))
  other -> throwEvalError ("cannot apply ! to " <> typeName other)
-- Upstream's parser turns -e into __sub 0 e (parser.y at 2.24.9), so
-- negation is builtins.sub with 0 on the left in every respect: minBound
-- reports the checked-subtraction overflow, a float zero comes out positive
-- (0 - 0.0, where negate would give -0.0), and a non-number fails sub's
-- forceInt on its second argument.
evalUnary OpNegate val = primSub (VInt 0) val

-- | @+@ once its left operand is an integer, given the right: upstream's
-- @ExprConcatStrings::eval@ (eval.cc at 2.24.9) adds a number and refuses
-- anything else, naming the right operand's type.  A float on the right
-- makes the sum a float.
addToInteger :: (MonadEval m) => Int64 -> NixValue -> m NixValue
addToInteger a right = case right of
  VInt b -> either throwEvalError (pure . VInt) (checkedAdd a b)
  VFloat b -> pure (VFloat (fromIntegral a + b))
  other -> throwEvalError ("cannot add " <> typeName other <> " to an integer")

-- | @+@ once its left operand is a float, as 'addToInteger'.
addToFloat :: (MonadEval m) => Double -> NixValue -> m NixValue
addToFloat a right = case right of
  VInt b -> pure (VFloat (a + fromIntegral b))
  VFloat b -> pure (VFloat (a + b))
  other -> throwEvalError ("cannot add " <> typeName other <> " to a float")

-- | Checked Int64 arithmetic: integer overflow is an eval error, never a
-- two's-complement wrap.  Computed in Integer and bounds-checked.  2.24.9
-- leaves overflow to C++ signed arithmetic, which is undefined; upstream
-- checks it from 2.25, and its message is followed.  Division is the
-- exception, see 'divisionOverflowMessage'.
checkedIntOp :: Text -> Text -> (Integer -> Integer -> Integer) -> Int64 -> Int64 -> Either Text Int64
checkedIntOp verb symbol op a b
  | wide < toInteger (minBound :: Int64) || wide > toInteger (maxBound :: Int64) =
      Left (overflowMessage verb symbol a b)
  | otherwise = Right (fromInteger wide)
  where
    wide = op (toInteger a) (toInteger b)

-- | Upstream's overflow message: @integer overflow in adding a + b@.
overflowMessage :: Text -> Text -> Int64 -> Int64 -> Text
overflowMessage verb symbol a b =
  "integer overflow in " <> verb <> " " <> T.pack (show a) <> " " <> symbol <> " " <> T.pack (show b)

-- | @minBound / -1@, the one overflowing division.  2.24.9 checks it
-- itself (prim_div), so its message wins over 2.25's
-- @integer overflow in dividing a / b@.
divisionOverflowMessage :: Text
divisionOverflowMessage = "overflow in integer division"

checkedAdd :: Int64 -> Int64 -> Either Text Int64
checkedAdd = checkedIntOp "adding" "+" (+)

checkedSub :: Int64 -> Int64 -> Either Text Int64
checkedSub = checkedIntOp "subtracting" "-" (-)

checkedMul :: Int64 -> Int64 -> Either Text Int64
checkedMul = checkedIntOp "multiplying" "*" (*)

-- | Upstream's @forceInt@ on a forced value (eval.cc at 2.24.9).
expectInt :: (MonadEval m) => NixValue -> m Int64
expectInt (VInt n) = pure n
expectInt other = throwEvalError (expectedMessage "an integer" other)

-- | Upstream's @forceFloat@ on a forced value: an integer widens.
expectFloat :: (MonadEval m) => NixValue -> m Double
expectFloat (VInt n) = pure (fromIntegral n)
expectFloat (VFloat x) = pure x
expectFloat other = throwEvalError (expectedMessage "a float" other)

expectedMessage :: Text -> NixValue -> Text
expectedMessage wanted other =
  "expected " <> wanted <> " but found " <> typeName other <> ": " <> printValue PrintForError other

isFloat :: NixValue -> Bool
isFloat (VFloat _) = True
isFloat _ = False

-- | @-@, @*@, @builtins.add@, @builtins.sub@ and @builtins.mul@: upstream's
-- @prim_add@, @prim_sub@ and @prim_mul@ (primops.cc at 2.24.9).  A float on
-- either side sends both operands through @forceFloat@, and otherwise both
-- go through @forceInt@, the left first.  @+@ is not among them: upstream
-- evaluates it as @ExprConcatStrings@ ('addToInteger', 'addToFloat').
primArith ::
  (MonadEval m) =>
  (Int64 -> Int64 -> Either Text Int64) ->
  (Double -> Double -> Double) ->
  NixValue ->
  NixValue ->
  m NixValue
primArith checkedOp floatOp left right
  | isFloat left || isFloat right = VFloat <$> (floatOp <$> expectFloat left <*> expectFloat right)
  | otherwise = either throwEvalError (pure . VInt) =<< (checkedOp <$> expectInt left <*> expectInt right)

primAdd, primSub, primMul :: (MonadEval m) => NixValue -> NixValue -> m NixValue
primAdd = primArith checkedAdd (+)
primSub = primArith checkedSub (-)
primMul = primArith checkedMul (*)

-- | @/@ and @builtins.div@, upstream's @prim_div@: the divisor goes through
-- @forceFloat@ and is checked for zero before the dividend is looked at,
-- so @{ } / 0@ is a division by zero.  Integer division truncates toward
-- zero, as C++ does.
primDiv :: (MonadEval m) => NixValue -> NixValue -> m NixValue
primDiv left right = expectFloat right >>= divideBy
  where
    divideBy divisor
      | divisor == 0 = throwEvalError divisionByZeroMessage
      | isFloat left || isFloat right = VFloat . (/ divisor) <$> expectFloat left
      | otherwise = either throwEvalError (pure . VInt) =<< (checkedDiv <$> expectInt left <*> expectInt right)

checkedDiv :: Int64 -> Int64 -> Either Text Int64
checkedDiv a b
  | b == 0 = Left divisionByZeroMessage
  | a == minBound && b == -1 = Left divisionOverflowMessage
  | otherwise = Right (quot a b)

divisionByZeroMessage :: Text
divisionByZeroMessage = "division by zero"

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
-- Upstream's @CompareValues@ (primops.cc at 2.24.9).  2.33.2 appends the
-- two values; 2.24.9 does not.
nixCompare _ left right
  | typeOfValue left == typeOfValue right =
      throwEvalError (incomparable <> "; values of that type are incomparable")
  | otherwise = throwEvalError incomparable
  where
    incomparable = "cannot compare " <> typeName left <> " with " <> typeName right

-- | Lexicographic comparison of two thunk lists for the @<@ operator:
-- the first NON-EQUAL element pair decides via @<@ on that pair, as
-- upstream does (eqValues, then CompareValues on the first difference).
-- An unequal pair where @<@ holds in neither direction (NaN) therefore
-- decides False rather than being skipped as equal.  A proper prefix is
-- less than the longer list.  Mirrors 'listEqual'.
listCompare :: (MonadEval m) => Force m -> [Thunk] -> [Thunk] -> m Bool
listCompare _ [] [] = pure False
listCompare _ [] (_ : _) = pure True
listCompare _ (_ : _) [] = pure False
listCompare forceFn (a : as) (b : bs)
  | thunkSameRef a b = listCompare forceFn as bs
  | otherwise = do
      va <- forceFn a
      vb <- forceFn b
      equal <- nixEqual forceFn va vb
      if equal
        then listCompare forceFn as bs
        else nixCompare forceFn va vb

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
-- Each operand is evaluated and checked for a set in turn, the left
-- first, as @ExprOpUpdate::eval@ calls @evalAttrs@ on each (eval.cc at
-- 2.24.9): a non-set left fails before the right is evaluated at all, and
-- the refusal is @evalAttrs@'s message.  2.33.2 checks the right first;
-- 2.24.9 is followed.  Upstream adds a trace line naming the operand ("in
-- the left operand of the update (//) operator") above the message;
-- nova-nix's errors carry no trace, so only the final line is reproduced.
evalUpdate :: (MonadEval m) => m NixValue -> m NixValue -> m NixValue
evalUpdate evalLeft evalRight = do
  leftSet <- evalLeft >>= expectSet
  rightSet <- evalRight >>= expectSet
  pure (VAttrs (mergeAttrSets leftSet rightSet))
  where
    expectSet (VAttrs set) = pure set
    expectSet other =
      throwEvalError ("expected a set but found " <> typeName other <> ": " <> printValue PrintForError other)

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
