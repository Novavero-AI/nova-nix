{-# LANGUAGE LambdaCase #-}

-- | Binary and unary operator evaluation for Nix.
--
-- Short-circuiting operators ('OpAnd', 'OpOr', 'OpImpl') are handled in
-- "Nix.Eval" because they must not evaluate both operands, and so is @+@
-- ('OpAdd'), which coerces strings and paths through the evaluator and
-- comes here for its numeric cases ('addToInteger', 'addToFloat').
-- Everything else lives here.
module Nix.Eval.Operator
  ( evalBinary,
    evalUnary,
    evalUpdate,
    evalUpdateChain,
    addToInteger,
    addToFloat,
    nixCompare,
    nixEqual,
    primAdd,
    primSub,
    primMul,
    primDiv,
    expectInt,
    expectedFailure,
  )
where

import Control.Monad (foldM)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.AllocSite (SetSite (SetUpdate))
import Nix.Eval.CAttrSet (cattrsetUpdate)
import Nix.Eval.CData (MonadCData)
import Nix.Eval.CList (clistFromThunks, clistLen, clistThunks)
import Nix.Eval.Print (PrintOptions (..), printValue)
import Nix.Eval.Types
  ( AttrSet (..),
    MonadEval (..),
    NixValue (..),
    Thunk (..),
    attrSetAllPairs,
    attrSetLookup,
    attrSetSize,
    typeName,
    typeOfValue,
  )
import Nix.Expr.Types (BinaryOp (..), UnaryOp (..))

-- | Force function passed by the caller to break the import cycle.
-- Needed for deep equality on lists and attribute sets.
type Force m = Thunk -> m NixValue

-- | Evaluate a binary operator on two forced values.
--
-- The caller must handle 'OpAdd' and the short-circuit operators
-- ('OpAnd', 'OpOr', 'OpImpl') before calling this: each looks at its left
-- operand before the right is evaluated, and @+@ coerces through the
-- evaluator.  The @Force@ function is used only to force the elements of
-- compound values: for deep structural equality, and for the element-wise
-- ordering of lists.
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
expectInt other = expectedFailure "an integer" other

-- | Upstream's @forceFloat@ on a forced value: an integer widens.
expectFloat :: (MonadEval m) => NixValue -> m Double
expectFloat (VInt n) = pure (fromIntegral n)
expectFloat (VFloat x) = pure x
expectFloat other = expectedFailure "a float" other

-- | Upstream's refusal of a value of the wrong type: the type wanted, the
-- type found, and the value as an error prints it.
expectedFailure :: (MonadEval m) => Text -> NixValue -> m a
expectedFailure wanted other = do
  shown <- printValue PrintForError other
  throwEvalError ("expected " <> wanted <> " but found " <> typeName other <> ": " <> shown)

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

-- | 'primArith' with 64-bit integer overflow an evaluation error:
-- @builtins.add@; @-@ and @builtins.sub@; @*@ and @builtins.mul@.
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
nixCompare forceFn (VList clA) (VList clB) = do
  as <- map Thunk <$> clistThunks clA
  bs <- map Thunk <$> clistThunks clB
  listCompare forceFn as bs
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
-- upstream does (eqValues, then CompareValues on the first difference,
-- in primops.cc).  An unequal pair where @<@ holds in neither
-- direction (NaN) therefore decides False rather than being skipped as
-- equal.  A proper prefix is less than the longer list.  Mirrors
-- 'listEqual'.
listCompare :: (MonadEval m) => Force m -> [Thunk] -> [Thunk] -> m Bool
listCompare _ [] [] = pure False
listCompare _ [] (_ : _) = pure True
listCompare _ (_ : _) [] = pure False
listCompare forceFn (a : as) (b : bs) = do
  va <- forceFn a
  vb <- forceFn b
  equal <- forcedPairEqual forceFn (a, va) (b, vb)
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
nixEqual forceFn (VList clA) (VList clB) = do
  lenA <- clistLen clA
  lenB <- clistLen clB
  if lenA /= lenB
    then pure False
    else do
      as <- map Thunk <$> clistThunks clA
      bs <- map Thunk <$> clistThunks clB
      listEqual forceFn as bs
-- Upstream's @eqValues@ on two sets (eval.cc): two derivations by their
-- outPaths, then sets of different sizes unequal with no value forced, then
-- 'attrsEqual'.
nixEqual forceFn (VAttrs as) (VAttrs bs) = do
  drvOutPaths <- derivationOutPathPair forceFn as bs
  case drvOutPaths of
    Just outPathPair -> thunkPairEqual forceFn outPathPair
    Nothing -> do
      sizeA <- attrSetSize as
      sizeB <- attrSetSize bs
      if sizeA /= sizeB then pure False else attrsEqual forceFn as bs
nixEqual _ _ _ = pure False

-- | Upstream's walk over two sets of one size (@eqValues@ in eval.cc):
-- both in symbol order, each pair's names compared and then its
-- values, stopping at the first difference, so no value past it is forced
-- ('attrSetAllPairs').  Upstream's @Bindings@ are sorted by @Symbol@,
-- whose id is the order its table interned the name in (attr-set.hh and
-- symbol-table.hh), and a set here walks in symbol id order the same way.
-- Two sets therefore compare in upstream's order wherever the two tables
-- interned the sets' names in one order: the startup names first
-- ('Nix.Eval.StartupSymbols'), each name in source where upstream's
-- parser interns it, and each name @listToAttrs@, @groupBy@ and
-- @fromJSON@ make where those builtins intern it upstream.  No string
-- value takes a symbol in either.
attrsEqual :: (MonadEval m) => Force m -> AttrSet -> AttrSet -> m Bool
attrsEqual forceFn = attrSetAllPairs samePair
  where
    samePair (nameA, a) (nameB, b)
      | nameA /= nameB = pure False
      | otherwise = thunkPairEqual forceFn (a, b)

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
      if rightIsDrv
        then do
          outPathA <- attrSetLookup "outPath" as
          outPathB <- attrSetLookup "outPath" bs
          pure ((,) <$> outPathA <*> outPathB)
        else pure Nothing

-- | Does the set carry @type = "derivation"@?  Forces only the @type@ attr
-- (as upstream's isDerivation does); a non-string type is simply not a
-- derivation, not an error.
isDerivationSet :: (MonadEval m) => Force m -> AttrSet -> m Bool
isDerivationSet forceFn attrs =
  attrSetLookup "type" attrs >>= \case
    Nothing -> pure False
    Just typeThunk -> do
      typeVal <- forceFn typeThunk
      case typeVal of
        VStr tag _ -> pure (tag == "derivation")
        _ -> pure False

-- | Pairwise equality of two thunk lists (for list comparison).
listEqual :: (MonadEval m) => Force m -> [Thunk] -> [Thunk] -> m Bool
listEqual _ [] [] = pure True
listEqual forceFn (a : as) (b : bs) = do
  eq <- thunkPairEqual forceFn (a, b)
  if eq then listEqual forceFn as bs else pure False
listEqual _ _ _ = pure False

-- | Upstream's @eqValues@ on two thunks (eval.cc): both are forced, the left
-- first, before one is taken as equal to itself, so an error in a value both
-- sides share surfaces.
thunkPairEqual :: (MonadEval m) => Force m -> (Thunk, Thunk) -> m Bool
thunkPairEqual forceFn (a, b) = do
  va <- forceFn a
  vb <- forceFn b
  forcedPairEqual forceFn (a, va) (b, vb)

-- | Equality of two thunks already forced, each with its value.  One thunk
-- is equal to itself whatever its value, as upstream's @&v1 == &v2@ makes
-- a function or NaN that both sides share equal; two thunks compare their
-- values.
forcedPairEqual :: (MonadEval m) => Force m -> (Thunk, NixValue) -> (Thunk, NixValue) -> m Bool
forcedPairEqual forceFn (a, va) (b, vb)
  | a == b = pure True
  | otherwise = nixEqual forceFn va vb

-- ---------------------------------------------------------------------------
-- List / attrset operators
-- ---------------------------------------------------------------------------

-- | List concatenation (++).
evalConcat :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalConcat (VList clA) (VList clB) = do
  as <- clistThunks clA
  bs <- clistThunks clB
  VList <$> clistFromThunks (as ++ bs)
evalConcat left right =
  throwEvalError ("cannot concatenate " <> typeName left <> " and " <> typeName right)

-- | Attribute set merge (//).  Right-biased: keys in the right
-- operand shadow keys in the left.  The chain of the two
-- ('evalUpdateChain').
evalUpdate :: (MonadEval m) => m NixValue -> m NixValue -> m NixValue
evalUpdate evalLeft evalRight = evalUpdateChain (evalLeft :| [evalRight])

-- | A chain of @//@, given its operands first to last.
--
-- Each operand is evaluated and checked for a set in turn, last to
-- first, as upstream Nix gathers a chain: each update evaluates its right
-- operand and then its left.  A non-set fails before an operand before it
-- is evaluated at all, and the refusal is upstream's message for a value
-- that is not a set.  Older upstream releases evaluate and check the left
-- operand first; the newest pinned release is followed.  Upstream adds a
-- trace line naming the operand ("in the left operand of the update (//)
-- operator") above the message; this refusal adds no line of context, so
-- only the final line is reproduced.
--
-- Once every operand is a set, the sets are merged from the first, each
-- into the result of those before it, as the update queue folds a chain
-- from its leftmost operand once it has evaluated them all
-- (@ExprOpUpdate::eval@ in eval.cc): each small operand layers on the sets
-- before it ('mergeAttrSets'), and a chain one of whose operands fails
-- merges none.
evalUpdateChain :: (MonadEval m) => NonEmpty (m NixValue) -> m NixValue
evalUpdateChain operands = do
  lastFirst <- traverse (>>= expectSet) (NE.reverse operands)
  case NE.reverse lastFirst of
    start :| rest -> VAttrs <$> foldM mergeAttrSets start rest
  where
    expectSet (VAttrs set) = pure set
    expectSet other = expectedFailure "a set" other

-- | Merge two t'AttrSet's, right-biased (@//@).  An empty operand gives
-- the other operand's set itself, the left checked first, as upstream's
-- @ExprOpUpdate::eval@ copies the other operand's @Value@ (eval.cc): no set
-- is made, and a printer, which enters a set by its bindings, finds the
-- operand's.  Otherwise C-side @nn_attrset_update@ makes the set as
-- upstream 2.34 does: a layer on the left holding the right's entries alone
-- when the right is small and the left's chain of layers short, else a
-- merge of the two into a new set, O(n+m)
-- ('Nix.Eval.CAttrSet.cattrsetUpdate').  The census counts the entries
-- either copies, as upstream adds them to @nrOpUpdateValuesCopied@
-- (@ExprOpUpdate::eval@ in eval.cc at 2.34.9).  An empty operand copies and
-- counts nothing in either.
mergeAttrSets :: (MonadCData m) => AttrSet -> AttrSet -> m AttrSet
mergeAttrSets left@(AttrSet a) right@(AttrSet b) = do
  leftSize <- attrSetSize left
  rightSize <- attrSetSize right
  case (leftSize, rightSize) of
    (0, _) -> pure right
    (_, 0) -> pure left
    _ -> AttrSet <$> cattrsetUpdate SetUpdate a b
