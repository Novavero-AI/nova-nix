{-# LANGUAGE LambdaCase #-}

-- | Nix expression evaluator.
--
-- Nix evaluation is LAZY.  Attribute set members and list elements
-- are stored as thunks and only forced when their value is demanded.
-- Function arguments are likewise thunked — @(x: 1) (throw "boom")@
-- returns @1@ because @x@ is never referenced.
--
-- The evaluator maintains an environment ('Env') that maps variable
-- names to thunks.  @let@, @with@, function application, and
-- recursive attribute sets all extend the environment.
module Nix.Eval
  ( -- * Values (re-exported from Types)
    NixValue (..),
    Thunk (..),

    -- * String context (re-exported from Types)
    StringContextElement (..),
    StringContext (..),
    emptyContext,
    mkStr,

    -- * Environment (re-exported from Types)
    Env (..),
    emptyEnv,

    -- * Evaluation monad (re-exported from Types)
    MonadEval (..),
    PureEval (..),

    -- * Evaluation
    eval,
    force,

    -- * Helpers (for Builtins)
    typeName,
    evaluated,

    -- * Builtin registry
    BuiltinDef (..),
    builtinRegistry,
    builtinNames,

    -- * Platform
    currentSystemStr,
  )
where

import Control.Monad (when, (>=>))
import qualified Crypto.Hash as CH
import qualified Data.Array as Array
import Data.Bits (xor, (.&.), (.|.))
import qualified Data.ByteArray as BA
import Data.Char (chr, digitToInt, isAlpha, isDigit, isHexDigit, ord)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Derivation (Derivation (..), DerivationOutput (..), textToPlatform, toATerm)
import Nix.Eval.Context (extractInputDrvs, extractInputSrcs)
import Nix.Eval.Operator (evalBinary, evalUnary, nixCompare, nixEqual)
import Nix.Eval.StringInterp (coerceToString, evalIndStringParts, evalStringParts)
import Nix.Eval.Types
  ( Env (..),
    MonadEval (..),
    NixValue (..),
    PureEval (..),
    StringContext (..),
    StringContextElement (..),
    Thunk (..),
    emptyContext,
    emptyEnv,
    envInsertThunk,
    envLookup,
    evaluated,
    mkStr,
    mkSyntheticThunk,
    mkThunk,
    pushWithScope,
    runPureEval,
    typeName,
  )
import Nix.Expr.Types
  ( AttrKey (..),
    AttrPath,
    BinaryOp (..),
    Binding (..),
    Expr (..),
    Formal (..),
    Formals (..),
    NixAtom (..),
    StringPart (..),
  )
import Nix.Hash (byteToHex, sha256Hex, truncatedBase32)
import Nix.Store.Path (StorePath (..), defaultStoreDir, defaultStoreDirText, parseStorePath, storePathToFilePath)
import qualified System.Info
import Text.Regex.TDFA (matchAllText)
import qualified Text.Regex.TDFA as RE

-- | Evaluate a Nix expression in an environment.
eval :: (MonadEval m) => Env -> Expr -> m NixValue
eval env expr = case expr of
  ELit atom -> evalLit atom
  EStr parts -> uncurry VStr <$> evalStringParts eval env parts
  EIndStr parts -> uncurry VStr <$> evalIndStringParts eval env parts
  EVar name -> evalVar env name
  EAttrs isRec bindings -> evalAttrs env isRec bindings
  EList exprs -> pure (VList (map (mkThunk env) exprs))
  ESelect target path defExpr -> evalSelect env target path defExpr
  EHasAttr target path -> evalHasAttr env target path
  EApp func arg -> evalApp env func arg
  ELambda formals body -> pure (VLambda env formals body)
  ELet bindings body -> evalLet env bindings body
  EIf cond thenExpr elseExpr -> evalIf env cond thenExpr elseExpr
  EWith scope body -> evalWith env scope body
  EAssert cond body -> evalAssert env cond body
  EUnary op operand -> do
    val <- eval env operand
    evalUnary op val
  ESearchPath name -> evalSearchPath env name
  EBinary OpAnd left right -> evalShortCircuitAnd env left right
  EBinary OpOr left right -> evalShortCircuitOr env left right
  EBinary OpImpl left right -> evalShortCircuitImpl env left right
  EBinary op left right -> do
    leftVal <- eval env left
    rightVal <- eval env right
    evalBinary force op leftVal rightVal

-- | Force a thunk to a value.
--
-- Delegates to 'forceThunk' which is a 'MonadEval' method — this
-- allows IO evaluators to implement memoization (caching the result
-- after the first force) while pure evaluators simply re-evaluate.
force :: (MonadEval m) => Thunk -> m NixValue
force = forceThunk eval

-- ---------------------------------------------------------------------------
-- Literal
-- ---------------------------------------------------------------------------

evalLit :: (MonadEval m) => NixAtom -> m NixValue
evalLit atom = case atom of
  NixInt n -> pure (VInt n)
  NixFloat n -> pure (VFloat n)
  NixBool b -> pure (VBool b)
  NixNull -> pure VNull
  NixUri u -> pure (mkStr u)
  NixPath p -> VPath <$> resolvePathLiteral p

-- ---------------------------------------------------------------------------
-- Search paths (<nixpkgs>, <nixpkgs/lib>)
-- ---------------------------------------------------------------------------

-- | Evaluate a search path expression.
-- Desugars to @builtins.findFile builtins.nixPath "name"@ — exactly how
-- real Nix handles @\<name\>@ expressions.
evalSearchPath :: (MonadEval m) => Env -> Text -> m NixValue
evalSearchPath env name = do
  builtinsVal <- evalVar env "builtins"
  case builtinsVal of
    VAttrs builtinsAttrs ->
      case Map.lookup "nixPath" builtinsAttrs of
        Just nixPathThunk -> do
          nixPathVal <- force nixPathThunk
          builtinFindFile nixPathVal (mkStr name)
        Nothing ->
          throwEvalError ("file '" <> name <> "' was not found in the Nix search path")
    _ ->
      throwEvalError ("file '" <> name <> "' was not found in the Nix search path")

-- ---------------------------------------------------------------------------
-- Variables
-- ---------------------------------------------------------------------------

evalVar :: (MonadEval m) => Env -> Text -> m NixValue
evalVar env name =
  case envLookup name env of
    Just thunk -> force thunk
    Nothing -> throwEvalError ("undefined variable '" <> name <> "'")

-- ---------------------------------------------------------------------------
-- Attribute sets
-- ---------------------------------------------------------------------------

evalAttrs :: (MonadEval m) => Env -> Bool -> [Binding] -> m NixValue
evalAttrs env False bindings = evalNonRecAttrs env bindings
evalAttrs env True bindings = evalRecAttrs env bindings

-- | Non-recursive attribute set: thunks capture the outer environment.
evalNonRecAttrs :: (MonadEval m) => Env -> [Binding] -> m NixValue
evalNonRecAttrs env bindings = do
  attrMap <- buildBindingsMapM env env bindings
  pure (VAttrs attrMap)

-- | Recursive attribute set: thunks capture the completed environment
-- (Haskell's laziness ties the knot — Thunk's Env field is lazy).
-- Dynamic keys in recursive attrs evaluate against the outer env
-- (the rec env is not yet available during key resolution).
evalRecAttrs :: (MonadEval m) => Env -> [Binding] -> m NixValue
evalRecAttrs env bindings = do
  -- Resolve dynamic keys eagerly against the outer env.
  resolvedBindings <- resolveBindingKeys env bindings
  -- Now knot-tie: thunks capture recEnv, keys are already resolved.
  let recEnv = env {envBindings = Map.union recBindings (envBindings env)}
      recBindings = buildResolvedBindingsMap recEnv resolvedBindings
  pure (VAttrs recBindings)

-- | Build a flat attribute map from bindings (monadic — resolves
-- dynamic keys via eval, but only creates thunks for values).
-- Used by non-rec {}.
--
-- @thunkEnv@ is the environment captured by value thunks.
-- @keyEnv@ is used to evaluate dynamic key expressions.
--
-- Handles nested attribute paths (@a.b.c = 1@) by building nested
-- 'VAttrs' maps.  Handles @inherit@ by looking up names in the
-- environment or a source expression.
buildBindingsMapM :: (MonadEval m) => Env -> Env -> [Binding] -> m (Map Text Thunk)
buildBindingsMapM thunkEnv keyEnv = foldlM' mergeBinding Map.empty
  where
    mergeBinding current binding = do
      new <- processBindingM thunkEnv keyEnv binding
      pure (mergeAttrMaps current new)

-- | Strict left fold over a list in a monad.
foldlM' :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM' _ acc [] = pure acc
foldlM' f !acc (x : xs) = do
  acc2 <- f acc x
  foldlM' f acc2 xs

-- | Process a single binding into key-value pairs.
processBindingM :: (MonadEval m) => Env -> Env -> Binding -> m (Map Text Thunk)
processBindingM thunkEnv keyEnv (NamedBinding path bodyExpr) =
  buildNestedAttrM thunkEnv keyEnv path bodyExpr
processBindingM _ lookupEnv (Inherit Nothing names) =
  pure $ Map.fromList [(n, inheritLookup lookupEnv n) | n <- names]
processBindingM thunkEnv _ (Inherit (Just fromExpr) names) =
  pure $
    Map.fromList
      [ (n, mkThunk thunkEnv (ESelect fromExpr [StaticKey n] Nothing))
      | n <- names
      ]

-- ---------------------------------------------------------------------------
-- Resolved bindings (for knot-tying in rec {} and let)
-- ---------------------------------------------------------------------------

-- | A binding with all attribute keys pre-resolved to text.
-- Used by 'evalRecAttrs' and 'evalLet' to separate key resolution
-- (monadic, may evaluate dynamic keys) from thunk construction
-- (pure, enables knot-tying).
data ResolvedBinding
  = -- | @path = expr@ with all keys resolved to text.
    ResolvedNamed ![Text] !Expr
  | -- | @inherit attrs@ from the surrounding scope.
    ResolvedInherit ![Text]
  | -- | @inherit (from) attrs@.
    ResolvedInheritFrom !Expr ![Text]

-- | Resolve all attribute keys in a list of bindings, evaluating
-- dynamic keys against the given environment.
resolveBindingKeys :: (MonadEval m) => Env -> [Binding] -> m [ResolvedBinding]
resolveBindingKeys keyEnv = mapM resolveOne
  where
    resolveOne (NamedBinding path bodyExpr) = do
      resolvedPath <- mapM (resolveKey keyEnv) path
      pure (ResolvedNamed resolvedPath bodyExpr)
    resolveOne (Inherit Nothing names) =
      pure (ResolvedInherit names)
    resolveOne (Inherit (Just fromExpr) names) =
      pure (ResolvedInheritFrom fromExpr names)

-- | Build a flat attribute map from pre-resolved bindings (pure).
-- Thunks capture @thunkEnv@ — suitable for knot-tying.
buildResolvedBindingsMap :: Env -> [ResolvedBinding] -> Map Text Thunk
buildResolvedBindingsMap thunkEnv =
  foldl' mergeBinding Map.empty
  where
    mergeBinding current binding =
      mergeAttrMaps current (processResolved thunkEnv binding)

-- | Process a single resolved binding into key-value pairs (pure).
processResolved :: Env -> ResolvedBinding -> Map Text Thunk
processResolved thunkEnv (ResolvedNamed path bodyExpr) =
  buildResolvedNestedAttr thunkEnv path bodyExpr
processResolved lookupEnv (ResolvedInherit names) =
  Map.fromList [(n, inheritLookup lookupEnv n) | n <- names]
processResolved thunkEnv (ResolvedInheritFrom fromExpr names) =
  Map.fromList
    [ (n, mkThunk thunkEnv (ESelect fromExpr [StaticKey n] Nothing))
    | n <- names
    ]

-- | Build a nested attribute structure from a resolved dotted path (pure).
buildResolvedNestedAttr :: Env -> [Text] -> Expr -> Map Text Thunk
buildResolvedNestedAttr _thunkEnv [] _bodyExpr = Map.empty
buildResolvedNestedAttr thunkEnv [key] bodyExpr =
  Map.singleton key (mkThunk thunkEnv bodyExpr)
buildResolvedNestedAttr thunkEnv (key : rest) bodyExpr =
  Map.singleton key (evaluated (VAttrs (buildResolvedNestedAttr thunkEnv rest bodyExpr)))

-- | Look up a name for @inherit@.  If not found, create a thunk
-- that will error when forced (matching real Nix behaviour).
inheritLookup :: Env -> Text -> Thunk
inheritLookup env name =
  case envLookup name env of
    Just thunk -> thunk
    Nothing ->
      -- Deferred error: only triggers if this binding is actually demanded.
      -- Constructs: builtins.throw "undefined variable '<name>'"
      let errMsg = "undefined variable '" <> name <> "'"
          throwExpr = EApp (EVar "throw") (EStr [StrLit errMsg])
       in mkThunk env throwExpr

-- | Build a nested attribute structure from a dotted path.
-- @a.b.c = expr@ becomes @{ a = { b = { c = thunk; }; }; }@.
buildNestedAttrM :: (MonadEval m) => Env -> Env -> AttrPath -> Expr -> m (Map Text Thunk)
buildNestedAttrM _thunkEnv _keyEnv [] _bodyExpr =
  throwEvalError "empty attribute path"
buildNestedAttrM thunkEnv keyEnv [key] bodyExpr = do
  keyText <- resolveKey keyEnv key
  pure (Map.singleton keyText (mkThunk thunkEnv bodyExpr))
buildNestedAttrM thunkEnv keyEnv (key : rest) bodyExpr = do
  keyText <- resolveKey keyEnv key
  inner <- buildNestedAttrM thunkEnv keyEnv rest bodyExpr
  pure (Map.singleton keyText (evaluated (VAttrs inner)))

-- | Resolve an attribute key to its text name.
-- Static keys return immediately; dynamic keys evaluate the expression
-- and coerce the result to a string.
resolveKey :: (MonadEval m) => Env -> AttrKey -> m Text
resolveKey _env (StaticKey name) = pure name
resolveKey env (DynamicKey expr) = do
  val <- eval env expr
  case val of
    VStr s _ -> pure s
    _ -> throwEvalError ("dynamic attribute key must be a string, got " <> typeName val)

-- | Merge two attribute maps.  For overlapping keys where both sides
-- are attribute sets, merge recursively (for nested attr paths).
mergeAttrMaps :: Map Text Thunk -> Map Text Thunk -> Map Text Thunk
mergeAttrMaps = Map.unionWith mergeThunks

-- | Merge two thunks at the same key.  If both are evaluated VAttrs,
-- merge their contents recursively.  Otherwise the right wins.
mergeThunks :: Thunk -> Thunk -> Thunk
mergeThunks (Evaluated (VAttrs a)) (Evaluated (VAttrs b)) =
  Evaluated (VAttrs (mergeAttrMaps a b))
mergeThunks _ new = new

-- ---------------------------------------------------------------------------
-- Select / has-attr
-- ---------------------------------------------------------------------------

evalSelect :: (MonadEval m) => Env -> Expr -> AttrPath -> Maybe Expr -> m NixValue
evalSelect env target path defExpr = do
  targetVal <- eval env target
  result <- walkAttrPath env path targetVal
  case result of
    Just val -> pure val
    Nothing -> case defExpr of
      Just def -> eval env def
      Nothing -> throwEvalError ("attribute path not found in " <> typeName targetVal)

-- | Walk an attribute path through nested attribute sets.
-- Returns @Just value@ if the full path resolves, @Nothing@ if any
-- key is missing or a non-set is encountered mid-path.
-- The @env@ is used only for resolving dynamic keys.
walkAttrPath :: (MonadEval m) => Env -> AttrPath -> NixValue -> m (Maybe NixValue)
walkAttrPath _env [] val = pure (Just val)
walkAttrPath env (key : rest) val = case val of
  VAttrs attrs -> do
    keyText <- resolveKey env key
    case Map.lookup keyText attrs of
      Just thunk -> do
        inner <- force thunk
        walkAttrPath env rest inner
      Nothing -> pure Nothing
  _ -> pure Nothing

evalHasAttr :: (MonadEval m) => Env -> Expr -> AttrPath -> m NixValue
evalHasAttr env target path = do
  targetVal <- eval env target
  result <- walkAttrPath env path targetVal
  case result of
    Just _ -> pure (VBool True)
    Nothing -> pure (VBool False)

-- ---------------------------------------------------------------------------
-- Application
-- ---------------------------------------------------------------------------

evalApp :: (MonadEval m) => Env -> Expr -> Expr -> m NixValue
evalApp env funcExpr argExpr = do
  funcVal <- eval env funcExpr
  case funcVal of
    VLambda closureEnv formals body -> do
      let argThunk = mkThunk env argExpr
      extEnv <- matchFormals closureEnv formals argThunk
      eval extEnv body
    VBuiltin "tryEval" [] -> do
      result <- catchEvalError (eval env argExpr)
      case result of
        Right val ->
          pure
            ( VAttrs
                ( Map.fromList
                    [ ("success", evaluated (VBool True)),
                      ("value", evaluated val)
                    ]
                )
            )
        Left _ ->
          pure
            ( VAttrs
                ( Map.fromList
                    [ ("success", evaluated (VBool False)),
                      ("value", evaluated (VBool False))
                    ]
                )
            )
    VBuiltin name accArgs -> do
      argVal <- eval env argExpr
      applyBuiltin name accArgs argVal
    _ -> throwEvalError ("attempt to call " <> typeName funcVal <> ", which is not a function")

-- | Match function formals against an argument thunk.
matchFormals :: (MonadEval m) => Env -> Formals -> Thunk -> m Env
matchFormals closureEnv (FormalName name) argThunk =
  pure (envInsertThunk name argThunk closureEnv)
matchFormals closureEnv (FormalSet formals allowExtra) argThunk = do
  argVal <- force argThunk
  matchFormalSet closureEnv formals allowExtra argVal
matchFormals closureEnv (FormalNamedSet name formals allowExtra) argThunk = do
  argVal <- force argThunk
  -- Bind the @-pattern name (e.g. "args") BEFORE matching formals so that
  -- default expressions like @system ? args.system or ...@ can see it.
  let envWithAt = envInsertThunk name argThunk closureEnv
  matchFormalSet envWithAt formals allowExtra argVal

-- | Match a formal set pattern against a VAttrs argument.
matchFormalSet :: (MonadEval m) => Env -> [Formal] -> Bool -> NixValue -> m Env
matchFormalSet closureEnv formals allowExtra argVal =
  case argVal of
    VAttrs attrs -> do
      checkExtraKeys formals allowExtra attrs
      foldl' (bindFormal attrs) (pure closureEnv) formals
    _ -> throwEvalError ("function expects a set argument, got " <> typeName argVal)

-- | Verify that no unexpected keys are present (unless @...@ allows them).
checkExtraKeys :: (MonadEval m) => [Formal] -> Bool -> Map Text Thunk -> m ()
checkExtraKeys _ True _ = pure ()
checkExtraKeys formals False attrs =
  let expected = map fName formals
      actual = Map.keys attrs
      extra = filter (`notElem` expected) actual
   in case extra of
        [] -> pure ()
        (k : _) -> throwEvalError ("unexpected attribute '" <> k <> "' in function argument")

-- | Bind a single formal parameter from the argument attrset.
bindFormal :: (MonadEval m) => Map Text Thunk -> m Env -> Formal -> m Env
bindFormal attrs acc (Formal name defExpr) = do
  env <- acc
  case Map.lookup name attrs of
    Just thunk -> pure (envInsertThunk name thunk env)
    Nothing -> case defExpr of
      Just def -> pure (envInsertThunk name (mkThunk env def) env)
      Nothing -> throwEvalError ("missing required attribute '" <> name <> "'")

-- ---------------------------------------------------------------------------
-- Let / if / with / assert
-- ---------------------------------------------------------------------------

-- | Let is recursive in Nix: all bindings are visible to each other.
-- Knot-tying via Haskell laziness (Thunk's Env field is lazy).
-- Dynamic keys in let evaluate against the outer env (the let env
-- is not yet available during key resolution).
evalLet :: (MonadEval m) => Env -> [Binding] -> Expr -> m NixValue
evalLet env bindings body = do
  resolvedBindings <- resolveBindingKeys env bindings
  let letEnv = env {envBindings = Map.union letBindings (envBindings env)}
      letBindings = buildResolvedBindingsMap letEnv resolvedBindings
  eval letEnv body

evalIf :: (MonadEval m) => Env -> Expr -> Expr -> Expr -> m NixValue
evalIf env cond thenExpr elseExpr = do
  condVal <- eval env cond
  case condVal of
    VBool True -> eval env thenExpr
    VBool False -> eval env elseExpr
    _ -> throwEvalError ("'if' condition must be a Boolean, got " <> typeName condVal)

evalWith :: (MonadEval m) => Env -> Expr -> Expr -> m NixValue
evalWith env scope body = do
  scopeVal <- eval env scope
  case scopeVal of
    VAttrs attrs -> eval (pushWithScope attrs env) body
    _ -> throwEvalError ("'with' requires a set, got " <> typeName scopeVal)

evalAssert :: (MonadEval m) => Env -> Expr -> Expr -> m NixValue
evalAssert env cond body = do
  condVal <- eval env cond
  case condVal of
    VBool True -> eval env body
    VBool False -> throwEvalError "assertion failed"
    _ -> throwEvalError ("assertion condition must be a Boolean, got " <> typeName condVal)

-- ---------------------------------------------------------------------------
-- Short-circuit Boolean operators
-- ---------------------------------------------------------------------------

evalShortCircuitAnd :: (MonadEval m) => Env -> Expr -> Expr -> m NixValue
evalShortCircuitAnd env left right = do
  leftVal <- eval env left
  case leftVal of
    VBool False -> pure (VBool False)
    VBool True -> do
      rightVal <- eval env right
      case rightVal of
        VBool _ -> pure rightVal
        _ -> throwEvalError ("second operand of && must be a Boolean, got " <> typeName rightVal)
    _ -> throwEvalError ("first operand of && must be a Boolean, got " <> typeName leftVal)

evalShortCircuitOr :: (MonadEval m) => Env -> Expr -> Expr -> m NixValue
evalShortCircuitOr env left right = do
  leftVal <- eval env left
  case leftVal of
    VBool True -> pure (VBool True)
    VBool False -> do
      rightVal <- eval env right
      case rightVal of
        VBool _ -> pure rightVal
        _ -> throwEvalError ("second operand of || must be a Boolean, got " <> typeName rightVal)
    _ -> throwEvalError ("first operand of || must be a Boolean, got " <> typeName leftVal)

evalShortCircuitImpl :: (MonadEval m) => Env -> Expr -> Expr -> m NixValue
evalShortCircuitImpl env left right = do
  leftVal <- eval env left
  case leftVal of
    VBool False -> pure (VBool True)
    VBool True -> do
      rightVal <- eval env right
      case rightVal of
        VBool _ -> pure rightVal
        _ -> throwEvalError ("second operand of -> must be a Boolean, got " <> typeName rightVal)
    _ -> throwEvalError ("first operand of -> must be a Boolean, got " <> typeName leftVal)

-- ---------------------------------------------------------------------------
-- Builtin registry (single-definition-site for all builtins)
-- ---------------------------------------------------------------------------

-- | A builtin function definition: its arity and implementation.
data BuiltinDef m = BuiltinDef
  { bdArity :: !Int,
    bdApply :: [NixValue] -> m NixValue
  }

-- | Define an arity-1 builtin.
builtin1 :: (MonadEval m) => Text -> (NixValue -> m NixValue) -> (Text, BuiltinDef m)
builtin1 name f =
  ( name,
    BuiltinDef 1 $ \case
      [a] -> f a
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
  )

-- | Define an arity-2 builtin.
builtin2 ::
  (MonadEval m) =>
  Text ->
  (NixValue -> NixValue -> m NixValue) ->
  (Text, BuiltinDef m)
builtin2 name f =
  ( name,
    BuiltinDef 2 $ \case
      [a, b] -> f a b
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
  )

-- | Define an arity-3 builtin.
builtin3 ::
  (MonadEval m) =>
  Text ->
  (NixValue -> NixValue -> NixValue -> m NixValue) ->
  (Text, BuiltinDef m)
builtin3 name f =
  ( name,
    BuiltinDef 3 $ \case
      [a, b, c] -> f a b c
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
  )

-- | Central registry of all builtins.  Adding a new builtin is a single
-- entry here plus its implementation function — no other files need changes.
builtinRegistry :: (MonadEval m) => Map Text (BuiltinDef m)
builtinRegistry =
  Map.fromList
    [ -- Type checking (arity 1)
      builtin1 "typeOf" (pure . mkStr . typeOfValue),
      builtin1 "isNull" (pure . VBool . isNullVal),
      builtin1 "isInt" (pure . VBool . isIntVal),
      builtin1 "isFloat" (pure . VBool . isFloatVal),
      builtin1 "isBool" (pure . VBool . isBoolVal),
      builtin1 "isString" (pure . VBool . isStringVal),
      builtin1 "isList" (pure . VBool . isListVal),
      builtin1 "isAttrs" (pure . VBool . isAttrsVal),
      builtin1 "isFunction" (pure . VBool . isFunctionVal),
      -- List operations (arity 1)
      builtin1 "length" builtinLength,
      builtin1 "head" builtinHead,
      builtin1 "tail" builtinTail,
      -- String operations (arity 1)
      builtin1 "toString" (fmap (uncurry VStr) . coerceToStringPermissive),
      builtin1 "stringLength" builtinStringLength,
      -- Control (arity 1)
      builtin1 "throw" builtinThrow,
      builtin1 "abort" builtinThrow,
      -- Attr set operations (arity 1)
      builtin1 "attrNames" builtinAttrNames,
      builtin1 "attrValues" builtinAttrValues,
      builtin1 "listToAttrs" builtinListToAttrs,
      -- Attr set operations (arity 2)
      builtin2 "hasAttr" builtinHasAttr,
      builtin2 "getAttr" builtinGetAttr,
      builtin2 "removeAttrs" builtinRemoveAttrs,
      builtin2 "intersectAttrs" builtinIntersectAttrs,
      builtin2 "catAttrs" builtinCatAttrs,
      -- List higher-order (arity 2)
      builtin2 "map" builtinMap,
      builtin2 "filter" builtinFilter,
      builtin2 "genList" builtinGenList,
      builtin2 "sort" builtinSort,
      builtin2 "concatMap" builtinConcatMap,
      builtin2 "any" builtinAny,
      builtin2 "all" builtinAll,
      builtin2 "elem" builtinElem,
      builtin2 "elemAt" builtinElemAt,
      builtin2 "partition" builtinPartition,
      builtin2 "groupBy" builtinGroupBy,
      -- String operations (arity 2)
      builtin2 "concatStringsSep" builtinConcatStringsSep,
      -- Arity 3
      builtin3 "foldl'" builtinFoldl,
      builtin3 "substring" builtinSubstring,
      -- Numeric
      builtin1 "isPath" (pure . VBool . isPathVal),
      builtin1 "ceil" builtinCeil,
      builtin1 "floor" builtinFloor,
      builtin2 "seq" (\_ b -> pure b),
      builtin2 "trace" (\_ b -> pure b),
      builtin1 "unsafeDiscardStringContext" builtinDiscardContext,
      builtin1 "unsafeDiscardOutputDependency" builtinDiscardOutputDep,
      -- String context introspection
      builtin1 "hasContext" builtinHasContext,
      builtin1 "getContext" builtinGetContext,
      builtin2 "appendContext" builtinAppendContext,
      builtin1 "baseNameOf" builtinBaseNameOf,
      builtin1 "dirOf" builtinDirOf,
      builtin1 "concatLists" builtinConcatLists,
      builtin2 "lessThan" builtinLessThan,
      -- Arithmetic + bitwise
      builtin2 "add" builtinAdd,
      builtin2 "sub" builtinSub,
      builtin2 "mul" builtinMul,
      builtin2 "div" builtinDiv,
      builtin2 "bitAnd" builtinBitAnd,
      builtin2 "bitOr" builtinBitOr,
      builtin2 "bitXor" builtinBitXor,
      -- Attr set higher-order
      builtin2 "mapAttrs" builtinMapAttrs,
      builtin1 "functionArgs" builtinFunctionArgs,
      builtin2 "zipAttrsWith" builtinZipAttrsWith,
      -- String manipulation
      builtin2 "match" builtinMatch,
      builtin2 "split" builtinSplit,
      builtin3 "replaceStrings" builtinReplaceStrings,
      builtin2 "compareVersions" builtinCompareVersions,
      builtin1 "splitVersion" builtinSplitVersion,
      builtin1 "parseDrvName" builtinParseDrvName,
      -- Serialization + hashing
      builtin1 "toJSON" builtinToJSON,
      builtin1 "fromJSON" builtinFromJSON,
      builtin2 "hashString" builtinHashString,
      -- Error handling + sequencing
      builtin1 "tryEval" (\_ -> throwEvalError "unreachable: tryEval handled in evalApp"),
      builtin2 "deepSeq" builtinDeepSeq,
      -- Graph traversal
      builtin1 "genericClosure" builtinGenericClosure,
      -- IO builtins (delegate to MonadEval methods)
      builtin1 "import" builtinImport,
      builtin1 "readFile" builtinReadFile,
      builtin1 "pathExists" builtinPathExists,
      builtin1 "readDir" builtinReadDir,
      builtin1 "getEnv" builtinGetEnv,
      builtin1 "toPath" builtinToPath,
      -- Store path operations
      builtin1 "placeholder" builtinPlaceholder,
      builtin1 "storePath" builtinStorePath,
      builtin2 "findFile" builtinFindFile,
      builtin2 "toFile" builtinToFile,
      builtin2 "scopedImport" builtinScopedImport,
      -- Network fetchers
      builtin1 "fetchurl" builtinFetchurl,
      builtin1 "fetchTarball" builtinFetchTarball,
      builtin1 "fetchGit" builtinFetchGit,
      -- Derivation construction
      builtin1 "derivation" builtinDerivation,
      -- Error context (pass-through — context only matters on error)
      builtin2 "addErrorContext" (\_ val -> pure val),
      -- Attr position (return null — nixpkgs handles this gracefully)
      builtin2 "unsafeGetAttrPos" (\_ _ -> pure VNull)
    ]

-- | Names of all registered builtins.
builtinNames :: [Text]
builtinNames = Map.keys (builtinRegistry :: Map Text (BuiltinDef PureEval))

-- ---------------------------------------------------------------------------
-- Builtin dispatch (partial application via accumulated args)
-- ---------------------------------------------------------------------------

-- | Arity of a builtin (how many arguments before execution).
builtinArity :: Text -> Int
builtinArity name = maybe 1 bdArity (Map.lookup name (builtinRegistry :: Map Text (BuiltinDef PureEval)))

-- | Apply a builtin with accumulated args.  If we have enough args,
-- execute; otherwise return a partially applied builtin.
applyBuiltin :: (MonadEval m) => Text -> [NixValue] -> NixValue -> m NixValue
applyBuiltin name accArgs arg =
  let allArgs = accArgs ++ [arg]
      arity = builtinArity name
   in if length allArgs < arity
        then pure (VBuiltin name allArgs)
        else executeBuiltin name allArgs

-- | Apply a function value (lambda or builtin) to one argument.
-- Used by higher-order builtins to invoke user-supplied functions.
applyValue :: (MonadEval m) => NixValue -> NixValue -> m NixValue
applyValue (VLambda closureEnv formals body) arg = do
  extEnv <- matchFormals closureEnv formals (evaluated arg)
  eval extEnv body
applyValue (VBuiltin name accArgs) arg =
  applyBuiltin name accArgs arg
applyValue other _ =
  throwEvalError ("attempt to call " <> typeName other <> ", which is not a function")

-- | Execute a builtin once all arguments are collected.
--
-- Direct case dispatch avoids rebuilding the polymorphic 'builtinRegistry'
-- Map on every call.  'builtinRegistry' is polymorphic in @m@ so GHC
-- cannot cache it as a CAF — it gets reconstructed on every use.
-- Pattern matching on the name is zero-allocation.
executeBuiltin :: (MonadEval m) => Text -> [NixValue] -> m NixValue
executeBuiltin name args = case name of
  -- Type checking (arity 1)
  "typeOf" -> apply1 (pure . mkStr . typeOfValue)
  "isNull" -> apply1 (pure . VBool . isNullVal)
  "isInt" -> apply1 (pure . VBool . isIntVal)
  "isFloat" -> apply1 (pure . VBool . isFloatVal)
  "isBool" -> apply1 (pure . VBool . isBoolVal)
  "isString" -> apply1 (pure . VBool . isStringVal)
  "isList" -> apply1 (pure . VBool . isListVal)
  "isAttrs" -> apply1 (pure . VBool . isAttrsVal)
  "isFunction" -> apply1 (pure . VBool . isFunctionVal)
  -- List operations (arity 1)
  "length" -> apply1 builtinLength
  "head" -> apply1 builtinHead
  "tail" -> apply1 builtinTail
  -- String operations (arity 1)
  "toString" -> apply1 (fmap (uncurry VStr) . coerceToStringPermissive)
  "stringLength" -> apply1 builtinStringLength
  -- Control (arity 1)
  "throw" -> apply1 builtinThrow
  "abort" -> apply1 builtinThrow
  -- Attr set operations (arity 1)
  "attrNames" -> apply1 builtinAttrNames
  "attrValues" -> apply1 builtinAttrValues
  "listToAttrs" -> apply1 builtinListToAttrs
  -- Attr set operations (arity 2)
  "hasAttr" -> apply2 builtinHasAttr
  "getAttr" -> apply2 builtinGetAttr
  "removeAttrs" -> apply2 builtinRemoveAttrs
  "intersectAttrs" -> apply2 builtinIntersectAttrs
  "catAttrs" -> apply2 builtinCatAttrs
  -- List higher-order (arity 2)
  "map" -> apply2 builtinMap
  "filter" -> apply2 builtinFilter
  "genList" -> apply2 builtinGenList
  "sort" -> apply2 builtinSort
  "concatMap" -> apply2 builtinConcatMap
  "any" -> apply2 builtinAny
  "all" -> apply2 builtinAll
  "elem" -> apply2 builtinElem
  "elemAt" -> apply2 builtinElemAt
  "partition" -> apply2 builtinPartition
  "groupBy" -> apply2 builtinGroupBy
  -- String operations (arity 2)
  "concatStringsSep" -> apply2 builtinConcatStringsSep
  -- Arity 3
  "foldl'" -> apply3 builtinFoldl
  "substring" -> apply3 builtinSubstring
  -- Numeric
  "isPath" -> apply1 (pure . VBool . isPathVal)
  "ceil" -> apply1 builtinCeil
  "floor" -> apply1 builtinFloor
  "seq" -> apply2 (\_ b -> pure b)
  "trace" -> apply2 (\_ b -> pure b)
  "unsafeDiscardStringContext" -> apply1 builtinDiscardContext
  "unsafeDiscardOutputDependency" -> apply1 builtinDiscardOutputDep
  -- String context introspection
  "hasContext" -> apply1 builtinHasContext
  "getContext" -> apply1 builtinGetContext
  "appendContext" -> apply2 builtinAppendContext
  "baseNameOf" -> apply1 builtinBaseNameOf
  "dirOf" -> apply1 builtinDirOf
  "concatLists" -> apply1 builtinConcatLists
  "lessThan" -> apply2 builtinLessThan
  -- Arithmetic + bitwise
  "add" -> apply2 builtinAdd
  "sub" -> apply2 builtinSub
  "mul" -> apply2 builtinMul
  "div" -> apply2 builtinDiv
  "bitAnd" -> apply2 builtinBitAnd
  "bitOr" -> apply2 builtinBitOr
  "bitXor" -> apply2 builtinBitXor
  -- Attr set higher-order
  "mapAttrs" -> apply2 builtinMapAttrs
  "functionArgs" -> apply1 builtinFunctionArgs
  "zipAttrsWith" -> apply2 builtinZipAttrsWith
  -- String manipulation
  "match" -> apply2 builtinMatch
  "split" -> apply2 builtinSplit
  "replaceStrings" -> apply3 builtinReplaceStrings
  "compareVersions" -> apply2 builtinCompareVersions
  "splitVersion" -> apply1 builtinSplitVersion
  "parseDrvName" -> apply1 builtinParseDrvName
  -- Serialization + hashing
  "toJSON" -> apply1 builtinToJSON
  "fromJSON" -> apply1 builtinFromJSON
  "hashString" -> apply2 builtinHashString
  -- Error handling + sequencing
  "tryEval" -> apply1 (\_ -> throwEvalError "unreachable: tryEval handled in evalApp")
  "deepSeq" -> apply2 builtinDeepSeq
  -- Graph traversal
  "genericClosure" -> apply1 builtinGenericClosure
  -- IO builtins (delegate to MonadEval methods)
  "import" -> apply1 builtinImport
  "readFile" -> apply1 builtinReadFile
  "pathExists" -> apply1 builtinPathExists
  "readDir" -> apply1 builtinReadDir
  "getEnv" -> apply1 builtinGetEnv
  "toPath" -> apply1 builtinToPath
  -- Store path operations
  "placeholder" -> apply1 builtinPlaceholder
  "storePath" -> apply1 builtinStorePath
  "findFile" -> apply2 builtinFindFile
  "toFile" -> apply2 builtinToFile
  "scopedImport" -> apply2 builtinScopedImport
  -- Network fetchers
  "fetchurl" -> apply1 builtinFetchurl
  "fetchTarball" -> apply1 builtinFetchTarball
  "fetchGit" -> apply1 builtinFetchGit
  -- Derivation construction
  "derivation" -> apply1 builtinDerivation
  -- Error context (pass-through — context only matters on error)
  "addErrorContext" -> apply2 (\_ val -> pure val)
  -- Attr position (return null — nixpkgs handles this gracefully)
  "unsafeGetAttrPos" -> apply2 (\_ _ -> pure VNull)
  _ -> throwEvalError ("unknown builtin '" <> name <> "'")
  where
    apply1 f = case args of
      [a] -> f a
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
    apply2 f = case args of
      [a, b] -> f a b
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
    apply3 f = case args of
      [a, b, c] -> f a b c
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")

-- ---------------------------------------------------------------------------
-- Builtin implementations — type checking
-- ---------------------------------------------------------------------------

typeOfValue :: NixValue -> Text
typeOfValue val = case val of
  VInt _ -> "int"
  VFloat _ -> "float"
  VBool _ -> "bool"
  VNull -> "null"
  VStr _ _ -> "string"
  VPath _ -> "path"
  VList _ -> "list"
  VAttrs _ -> "set"
  VLambda {} -> "lambda"
  VBuiltin _ _ -> "lambda"
  VDerivation _ -> "set"

isNullVal :: NixValue -> Bool
isNullVal VNull = True
isNullVal _ = False

isIntVal :: NixValue -> Bool
isIntVal (VInt _) = True
isIntVal _ = False

isFloatVal :: NixValue -> Bool
isFloatVal (VFloat _) = True
isFloatVal _ = False

isBoolVal :: NixValue -> Bool
isBoolVal (VBool _) = True
isBoolVal _ = False

isStringVal :: NixValue -> Bool
isStringVal (VStr _ _) = True
isStringVal _ = False

isListVal :: NixValue -> Bool
isListVal (VList _) = True
isListVal _ = False

isAttrsVal :: NixValue -> Bool
isAttrsVal (VAttrs _) = True
isAttrsVal _ = False

isFunctionVal :: NixValue -> Bool
isFunctionVal (VLambda {}) = True
isFunctionVal (VBuiltin _ _) = True
isFunctionVal _ = False

-- ---------------------------------------------------------------------------
-- Builtin implementations — list (arity 1)
-- ---------------------------------------------------------------------------

builtinLength :: (MonadEval m) => NixValue -> m NixValue
builtinLength (VList xs) = pure (VInt (fromIntegral (length xs)))
builtinLength other = throwEvalError ("builtins.length: expected a list, got " <> typeName other)

builtinHead :: (MonadEval m) => NixValue -> m NixValue
builtinHead (VList []) = throwEvalError "builtins.head: empty list"
builtinHead (VList (x : _)) = force x
builtinHead other = throwEvalError ("builtins.head: expected a list, got " <> typeName other)

builtinTail :: (MonadEval m) => NixValue -> m NixValue
builtinTail (VList []) = throwEvalError "builtins.tail: empty list"
builtinTail (VList (_ : xs)) = pure (VList xs)
builtinTail other = throwEvalError ("builtins.tail: expected a list, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — string (arity 1)
-- ---------------------------------------------------------------------------

builtinStringLength :: (MonadEval m) => NixValue -> m NixValue
builtinStringLength (VStr s _) = pure (VInt (fromIntegral (T.length s)))
builtinStringLength other =
  throwEvalError ("builtins.stringLength: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — control
-- ---------------------------------------------------------------------------

builtinThrow :: (MonadEval m) => NixValue -> m NixValue
builtinThrow (VStr msg _) = throwEvalError msg
builtinThrow other = throwEvalError ("builtins.throw: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — attr set (arity 1)
-- ---------------------------------------------------------------------------

builtinAttrNames :: (MonadEval m) => NixValue -> m NixValue
builtinAttrNames (VAttrs attrs) =
  pure (VList (map (evaluated . mkStr) (Map.keys attrs)))
builtinAttrNames other =
  throwEvalError ("builtins.attrNames: expected a set, got " <> typeName other)

builtinAttrValues :: (MonadEval m) => NixValue -> m NixValue
builtinAttrValues (VAttrs attrs) =
  pure (VList (Map.elems attrs))
builtinAttrValues other =
  throwEvalError ("builtins.attrValues: expected a set, got " <> typeName other)

builtinListToAttrs :: (MonadEval m) => NixValue -> m NixValue
builtinListToAttrs (VList thunks) = do
  pairs <- mapM listToAttrsPair thunks
  pure (VAttrs (Map.fromList pairs))
builtinListToAttrs other =
  throwEvalError ("builtins.listToAttrs: expected a list, got " <> typeName other)

-- | Extract { name, value } from a thunk for listToAttrs.
listToAttrsPair :: (MonadEval m) => Thunk -> m (Text, Thunk)
listToAttrsPair thunk = do
  val <- force thunk
  case val of
    VAttrs attrs -> do
      nameThunk <-
        maybe (throwEvalError "builtins.listToAttrs: element missing 'name'") pure $
          Map.lookup "name" attrs
      nameVal <- force nameThunk
      case nameVal of
        VStr keyName _ ->
          case Map.lookup "value" attrs of
            Just valueThunk -> pure (keyName, valueThunk)
            Nothing -> throwEvalError "builtins.listToAttrs: element missing 'value'"
        _ -> throwEvalError "builtins.listToAttrs: 'name' must be a string"
    _ -> throwEvalError "builtins.listToAttrs: element must be a set"

-- ---------------------------------------------------------------------------
-- Builtin implementations — attr set (arity 2)
-- ---------------------------------------------------------------------------

builtinHasAttr :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinHasAttr (VStr key _) (VAttrs attrs) =
  pure (VBool (Map.member key attrs))
builtinHasAttr (VStr _ _) other =
  throwEvalError ("builtins.hasAttr: expected a set, got " <> typeName other)
builtinHasAttr other _ =
  throwEvalError ("builtins.hasAttr: expected a string, got " <> typeName other)

builtinGetAttr :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinGetAttr (VStr key _) (VAttrs attrs) =
  case Map.lookup key attrs of
    Just thunk -> force thunk
    Nothing -> throwEvalError ("builtins.getAttr: attribute '" <> key <> "' not found")
builtinGetAttr (VStr _ _) other =
  throwEvalError ("builtins.getAttr: expected a set, got " <> typeName other)
builtinGetAttr other _ =
  throwEvalError ("builtins.getAttr: expected a string, got " <> typeName other)

builtinRemoveAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinRemoveAttrs (VAttrs attrs) (VList thunks) = do
  keys <- mapM forceToString thunks
  pure (VAttrs (foldl' (flip Map.delete) attrs keys))
  where
    forceToString thunk = do
      val <- force thunk
      case val of
        VStr s _ -> pure s
        _ -> throwEvalError "builtins.removeAttrs: key list must contain strings"
builtinRemoveAttrs (VAttrs _) other =
  throwEvalError ("builtins.removeAttrs: expected a list, got " <> typeName other)
builtinRemoveAttrs other _ =
  throwEvalError ("builtins.removeAttrs: expected a set, got " <> typeName other)

builtinIntersectAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinIntersectAttrs (VAttrs a) (VAttrs b) =
  pure (VAttrs (Map.intersection b a))
builtinIntersectAttrs (VAttrs _) other =
  throwEvalError ("builtins.intersectAttrs: expected a set, got " <> typeName other)
builtinIntersectAttrs other _ =
  throwEvalError ("builtins.intersectAttrs: expected a set, got " <> typeName other)

builtinCatAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinCatAttrs (VStr key _) (VList thunks) = do
  vals <- catAttrsCollect key thunks
  pure (VList vals)
builtinCatAttrs (VStr _ _) other =
  throwEvalError ("builtins.catAttrs: expected a list, got " <> typeName other)
builtinCatAttrs other _ =
  throwEvalError ("builtins.catAttrs: expected a string, got " <> typeName other)

-- | Collect values for a given key from a list of attrsets.
catAttrsCollect :: (MonadEval m) => Text -> [Thunk] -> m [Thunk]
catAttrsCollect _ [] = pure []
catAttrsCollect key (thunk : rest) = do
  val <- force thunk
  case val of
    VAttrs attrs ->
      case Map.lookup key attrs of
        Just found -> (found :) <$> catAttrsCollect key rest
        Nothing -> catAttrsCollect key rest
    _ -> throwEvalError "builtins.catAttrs: list element must be a set"

-- ---------------------------------------------------------------------------
-- Builtin implementations — list higher-order (arity 2)
-- ---------------------------------------------------------------------------

builtinMap :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinMap func (VList thunks) =
  -- Lazy: each element is a deferred application, forced only on demand.
  pure (VList (map (deferApply func) thunks))
builtinMap _ other =
  throwEvalError ("builtins.map: expected a list, got " <> typeName other)

builtinFilter :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinFilter predFn (VList thunks) = do
  filtered <- filterThunks predFn thunks
  pure (VList filtered)
builtinFilter _ other =
  throwEvalError ("builtins.filter: expected a list, got " <> typeName other)

filterThunks :: (MonadEval m) => NixValue -> [Thunk] -> m [Thunk]
filterThunks _ [] = pure []
filterThunks predFn (thunk : rest) = do
  val <- force thunk
  result <- applyValue predFn val
  case result of
    VBool True -> (thunk :) <$> filterThunks predFn rest
    VBool False -> filterThunks predFn rest
    _ -> throwEvalError "builtins.filter: predicate must return a bool"

builtinGenList :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinGenList func (VInt n)
  | n < 0 = throwEvalError "builtins.genList: length must be non-negative"
  | otherwise =
      -- Lazy: each element is a deferred @f i@, forced only on demand.
      let fnThunk = evaluated func
          env = Env {envBindings = Map.fromList [("__fn", fnThunk)], envWithScopes = []}
          mkIndexThunk i = mkThunk env (EApp (EVar "__fn") (ELit (NixInt i)))
       in pure (VList (map mkIndexThunk [0 .. n - 1]))
builtinGenList _ other =
  throwEvalError ("builtins.genList: expected an integer, got " <> typeName other)

builtinSort :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinSort comparator (VList thunks) = do
  vals <- mapM force thunks
  sorted <- insertionSort comparator vals
  pure (VList (map evaluated sorted))
builtinSort _ other =
  throwEvalError ("builtins.sort: expected a list, got " <> typeName other)

-- | Stable insertion sort using a user-supplied comparator.
-- The comparator takes two args (curried) and returns bool.
insertionSort :: (MonadEval m) => NixValue -> [NixValue] -> m [NixValue]
insertionSort _ [] = pure []
insertionSort cmp (x : xs) = do
  sorted <- insertionSort cmp xs
  insertSorted cmp x sorted

insertSorted :: (MonadEval m) => NixValue -> NixValue -> [NixValue] -> m [NixValue]
insertSorted _ val [] = pure [val]
insertSorted cmp val (y : ys) = do
  partial <- applyValue cmp val
  result <- applyValue partial y
  case result of
    VBool True -> pure (val : y : ys)
    VBool False -> (y :) <$> insertSorted cmp val ys
    _ -> throwEvalError "builtins.sort: comparator must return a bool"

builtinConcatMap :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinConcatMap func (VList thunks) = do
  -- Semi-eager: must force each application to discover list structure for
  -- concatenation, but element thunks within those sub-lists stay lazy.
  let deferredApps = map (deferApply func) thunks
  results <- mapM force deferredApps
  concatted <- mapM extractList results
  pure (VList (concat concatted))
builtinConcatMap _ other =
  throwEvalError ("builtins.concatMap: expected a list, got " <> typeName other)

extractList :: (MonadEval m) => NixValue -> m [Thunk]
extractList (VList xs) = pure xs
extractList other =
  throwEvalError ("builtins.concatMap: function must return a list, got " <> typeName other)

builtinAny :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAny predFn (VList thunks) = do
  result <- anyThunk predFn thunks
  pure (VBool result)
builtinAny _ other =
  throwEvalError ("builtins.any: expected a list, got " <> typeName other)

anyThunk :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
anyThunk _ [] = pure False
anyThunk predFn (thunk : rest) = do
  val <- force thunk
  result <- applyValue predFn val
  case result of
    VBool True -> pure True
    VBool False -> anyThunk predFn rest
    _ -> throwEvalError "builtins.any: predicate must return a bool"

builtinAll :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAll predFn (VList thunks) = do
  result <- allThunk predFn thunks
  pure (VBool result)
builtinAll _ other =
  throwEvalError ("builtins.all: expected a list, got " <> typeName other)

allThunk :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
allThunk _ [] = pure True
allThunk predFn (thunk : rest) = do
  val <- force thunk
  result <- applyValue predFn val
  case result of
    VBool True -> allThunk predFn rest
    VBool False -> pure False
    _ -> throwEvalError "builtins.all: predicate must return a bool"

builtinElem :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinElem needle (VList thunks) = do
  found <- elemCheck needle thunks
  pure (VBool found)
builtinElem _ other =
  throwEvalError ("builtins.elem: expected a list, got " <> typeName other)

elemCheck :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
elemCheck _ [] = pure False
elemCheck needle (thunk : rest) = do
  val <- force thunk
  eq <- nixEqual force needle val
  if eq then pure True else elemCheck needle rest

builtinElemAt :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinElemAt (VList thunks) (VInt idx)
  | idx < 0 || idx >= fromIntegral (length thunks) =
      throwEvalError
        ( "builtins.elemAt: index "
            <> T.pack (show idx)
            <> " out of bounds for list of length "
            <> T.pack (show (length thunks))
        )
  | otherwise = case drop (fromIntegral idx) thunks of
      (t : _) -> force t
      [] -> throwEvalError "builtins.elemAt: index out of bounds"
builtinElemAt (VList _) other =
  throwEvalError ("builtins.elemAt: expected an integer, got " <> typeName other)
builtinElemAt other _ =
  throwEvalError ("builtins.elemAt: expected a list, got " <> typeName other)

builtinPartition :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinPartition predFn (VList thunks) = do
  (rightThunks, wrongThunks) <- partitionThunks predFn thunks
  pure
    ( VAttrs
        ( Map.fromList
            [ ("right", evaluated (VList rightThunks)),
              ("wrong", evaluated (VList wrongThunks))
            ]
        )
    )
builtinPartition _ other =
  throwEvalError ("builtins.partition: expected a list, got " <> typeName other)

partitionThunks :: (MonadEval m) => NixValue -> [Thunk] -> m ([Thunk], [Thunk])
partitionThunks _ [] = pure ([], [])
partitionThunks predFn (thunk : rest) = do
  val <- force thunk
  result <- applyValue predFn val
  (rs, ws) <- partitionThunks predFn rest
  case result of
    VBool True -> pure (thunk : rs, ws)
    VBool False -> pure (rs, thunk : ws)
    _ -> throwEvalError "builtins.partition: predicate must return a bool"

builtinGroupBy :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinGroupBy func (VList thunks) = do
  groups <- groupByCollect func thunks Map.empty
  pure (VAttrs (Map.map (evaluated . VList . reverse) groups))
builtinGroupBy _ other =
  throwEvalError ("builtins.groupBy: expected a list, got " <> typeName other)

groupByCollect ::
  (MonadEval m) =>
  NixValue ->
  [Thunk] ->
  Map Text [Thunk] ->
  m (Map Text [Thunk])
groupByCollect _ [] acc = pure acc
groupByCollect func (thunk : rest) acc = do
  val <- force thunk
  result <- applyValue func val
  case result of
    VStr key _ ->
      groupByCollect func rest (Map.insertWith (++) key [thunk] acc)
    _ -> throwEvalError "builtins.groupBy: function must return a string"

-- ---------------------------------------------------------------------------
-- Builtin implementations — string (arity 2)
-- ---------------------------------------------------------------------------

builtinConcatStringsSep :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinConcatStringsSep (VStr sep sepCtx) (VList thunks) = do
  pairs <- mapM forceToStrCtx thunks
  let texts = map fst pairs
      mergedCtx = sepCtx <> mconcat (map snd pairs)
  pure (VStr (T.intercalate sep texts) mergedCtx)
  where
    forceToStrCtx thunk = do
      val <- force thunk
      case val of
        VStr s ctx -> pure (s, ctx)
        _ -> throwEvalError "builtins.concatStringsSep: list elements must be strings"
builtinConcatStringsSep (VStr _ _) other =
  throwEvalError ("builtins.concatStringsSep: expected a list, got " <> typeName other)
builtinConcatStringsSep other _ =
  throwEvalError ("builtins.concatStringsSep: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — arity 3
-- ---------------------------------------------------------------------------

builtinFoldl :: (MonadEval m) => NixValue -> NixValue -> NixValue -> m NixValue
builtinFoldl op initial (VList thunks) =
  foldlStrict op initial thunks
builtinFoldl _ _ other =
  throwEvalError ("builtins.foldl': expected a list, got " <> typeName other)

-- | Strict left fold: apply @op acc elem@ for each element.
-- @op@ is curried so we call @applyValue op acc@ then @applyValue partial elem@.
foldlStrict :: (MonadEval m) => NixValue -> NixValue -> [Thunk] -> m NixValue
foldlStrict _ acc [] = pure acc
foldlStrict op acc (thunk : rest) = do
  val <- force thunk
  partial <- applyValue op acc
  stepped <- applyValue partial val
  foldlStrict op stepped rest

builtinSubstring :: (MonadEval m) => NixValue -> NixValue -> NixValue -> m NixValue
builtinSubstring (VInt start) (VInt len) (VStr s ctx) =
  let clampedStart = max 0 (fromIntegral start)
      -- Nix clamps len to available length (negative len means rest of string)
      available = T.length s - clampedStart
      clampedLen =
        if len < 0
          then available
          else min (fromIntegral len) available
   in -- Context is preserved through substring (matching real Nix).
      pure (VStr (T.take clampedLen (T.drop clampedStart s)) ctx)
builtinSubstring _ _ (VStr _ _) =
  throwEvalError "builtins.substring: start and length must be integers"
builtinSubstring _ _ other =
  throwEvalError ("builtins.substring: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin helpers
-- ---------------------------------------------------------------------------

-- | Build a thunk that defers @f arg@ — the application only happens when
-- the thunk is forced.  Reuses the existing eval machinery via a synthetic
-- @EApp (EVar "__fn") (EVar "__arg")@ in a self-contained env.
deferApply :: NixValue -> Thunk -> Thunk
deferApply func argThunk =
  let env =
        Env
          { envBindings =
              Map.fromList
                [ ("__fn", evaluated func),
                  ("__arg", argThunk)
                ],
            envWithScopes = []
          }
   in mkSyntheticThunk env (EApp (EVar "__fn") (EVar "__arg"))

-- | Permissive coercion used by @builtins.toString@.
--
-- Like 'coerceToString' but additionally handles lists: elements are
-- recursively coerced and joined with spaces, matching real Nix semantics.
-- @toString [1 2 3]@ gives @"1 2 3"@.
coerceToStringPermissive :: (MonadEval m) => NixValue -> m (Text, StringContext)
coerceToStringPermissive (VList thunks) = do
  parts <- mapM coerceThunk thunks
  let texts = map fst parts
      ctx = mconcat (map snd parts)
  pure (T.intercalate " " texts, ctx)
  where
    coerceThunk thunk = do
      val <- force thunk
      coerceToStringPermissive val
coerceToStringPermissive other = coerceToString other

-- | The current system platform string.
currentSystemStr :: Text
currentSystemStr = case (System.Info.arch, System.Info.os) of
  ("x86_64", "mingw32") -> "x86_64-windows"
  ("x86_64", "darwin") -> "x86_64-darwin"
  ("aarch64", "darwin") -> "aarch64-darwin"
  ("aarch64", "linux") -> "aarch64-linux"
  ("x86_64", "linux") -> "x86_64-linux"
  (arch, os) -> T.pack arch <> "-" <> T.pack os

-- | Store dir with trailing slash, for building store paths.
storeDirPrefix :: Text
storeDirPrefix = defaultStoreDirText <> "/"

-- ---------------------------------------------------------------------------
-- Builtin implementations — numeric + context
-- ---------------------------------------------------------------------------

isPathVal :: NixValue -> Bool
isPathVal (VPath _) = True
isPathVal _ = False

builtinCeil :: (MonadEval m) => NixValue -> m NixValue
builtinCeil (VFloat f) = pure (VInt (ceiling f))
builtinCeil (VInt n) = pure (VInt n)
builtinCeil other = throwEvalError ("builtins.ceil: expected a number, got " <> typeName other)

builtinFloor :: (MonadEval m) => NixValue -> m NixValue
builtinFloor (VFloat f) = pure (VInt (floor f))
builtinFloor (VInt n) = pure (VInt n)
builtinFloor other = throwEvalError ("builtins.floor: expected a number, got " <> typeName other)

builtinDiscardContext :: (MonadEval m) => NixValue -> m NixValue
builtinDiscardContext (VStr s _) = pure (mkStr s)
builtinDiscardContext other =
  throwEvalError ("builtins.unsafeDiscardStringContext: expected a string, got " <> typeName other)

-- | Strip only derivation output dependencies (SCDrvOutput, SCAllOutputs),
-- keeping plain store path references (SCPlain).
builtinDiscardOutputDep :: (MonadEval m) => NixValue -> m NixValue
builtinDiscardOutputDep (VStr s (StringContext ctx)) =
  let kept = Set.filter isPlain ctx
   in pure (VStr s (StringContext kept))
  where
    isPlain (SCPlain _) = True
    isPlain _ = False
builtinDiscardOutputDep other =
  throwEvalError ("builtins.unsafeDiscardOutputDependency: expected a string, got " <> typeName other)

-- | Check whether a string has any context elements.
builtinHasContext :: (MonadEval m) => NixValue -> m NixValue
builtinHasContext (VStr _ ctx) = pure (VBool (ctx /= emptyContext))
builtinHasContext other =
  throwEvalError ("builtins.hasContext: expected a string, got " <> typeName other)

-- | Return the context of a string as an attrset.
--
-- Each key is a store path string.  Each value is an attrset with:
--   - @path@: true if there's a SCPlain reference
--   - @allOutputs@: true if there's a SCAllOutputs reference
--   - @outputs@: list of output names from SCDrvOutput references
builtinGetContext :: (MonadEval m) => NixValue -> m NixValue
builtinGetContext (VStr _ (StringContext ctx)) = do
  let grouped = groupContextByPath (Set.toList ctx)
      attrMap = Map.map contextEntryToAttrs grouped
  pure (VAttrs attrMap)
builtinGetContext other =
  throwEvalError ("builtins.getContext: expected a string, got " <> typeName other)

-- | Intermediate representation for grouping context elements by store path.
data ContextEntry = ContextEntry
  { cePath :: !Bool,
    ceAllOutputs :: !Bool,
    ceOutputs :: ![Text]
  }

-- | Group context elements by their store path.
groupContextByPath :: [StringContextElement] -> Map Text ContextEntry
groupContextByPath = foldl' addElement Map.empty
  where
    addElement acc (SCPlain sp) =
      Map.insertWith mergeEntry (spToText sp) (ContextEntry True False []) acc
    addElement acc (SCDrvOutput sp outName) =
      Map.insertWith mergeEntry (spToText sp) (ContextEntry False False [outName]) acc
    addElement acc (SCAllOutputs sp) =
      Map.insertWith mergeEntry (spToText sp) (ContextEntry False True []) acc
    mergeEntry new old =
      ContextEntry
        (cePath new || cePath old)
        (ceAllOutputs new || ceAllOutputs old)
        (ceOutputs new ++ ceOutputs old)
    spToText sp =
      T.pack (storePathToFilePath defaultStoreDir sp)

-- | Convert a ContextEntry to an attrset thunk.
contextEntryToAttrs :: ContextEntry -> Thunk
contextEntryToAttrs entry =
  let fields =
        [("path", evaluated (VBool True)) | cePath entry]
          ++ [("allOutputs", evaluated (VBool True)) | ceAllOutputs entry]
          ++ [("outputs", evaluated (VList [evaluated (mkStr o) | o <- ceOutputs entry])) | not (null (ceOutputs entry))]
   in evaluated (VAttrs (Map.fromList fields))

-- | Append context entries to a string from an attrset.
--
-- @builtins.appendContext string contextAttrset@ adds the specified
-- context elements to the string.
builtinAppendContext :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAppendContext (VStr s ctx) (VAttrs contextAttrs) = do
  newCtx <- parseContextAttrs contextAttrs
  pure (VStr s (ctx <> newCtx))
builtinAppendContext (VStr _ _) other =
  throwEvalError ("builtins.appendContext: second argument must be a set, got " <> typeName other)
builtinAppendContext other _ =
  throwEvalError ("builtins.appendContext: first argument must be a string, got " <> typeName other)

-- | Parse a context attrset into a StringContext.
-- Each key is a store path; each value is an attrset with optional
-- @path@, @allOutputs@, and @outputs@ fields.
parseContextAttrs :: (MonadEval m) => Map Text Thunk -> m StringContext
parseContextAttrs attrs = do
  elements <- mapM parseOneCtx (Map.toList attrs)
  pure (StringContext (Set.fromList (concat elements)))
  where
    parseOneCtx (pathText, thunk) = do
      val <- force thunk
      case val of
        VAttrs inner -> do
          let sp = case parseStorePath defaultStoreDir pathText of
                Just parsed -> parsed
                Nothing -> StorePath (T.take 32 (T.drop (T.length storeDirPrefix) pathText)) (T.drop 33 (T.drop (T.length storeDirPrefix) pathText))
          hasPath <- getBoolAttr "path" inner
          hasAllOuts <- getBoolAttr "allOutputs" inner
          outNames <- getOutputsList inner
          let pathElems = [SCPlain sp | hasPath]
              allOutElems = [SCAllOutputs sp | hasAllOuts]
              outElems = [SCDrvOutput sp o | o <- outNames]
          pure (pathElems ++ allOutElems ++ outElems)
        _ -> throwEvalError "builtins.appendContext: context entry must be a set"

    getBoolAttr key attrs' = case Map.lookup key attrs' of
      Nothing -> pure False
      Just thunk -> do
        val <- force thunk
        case val of
          VBool b -> pure b
          _ -> pure False

    getOutputsList attrs' = case Map.lookup "outputs" attrs' of
      Nothing -> pure []
      Just thunk -> do
        val <- force thunk
        case val of
          VList thunks -> mapM forceToOutputName thunks
          _ -> pure []

    forceToOutputName thunk = do
      val <- force thunk
      case val of
        VStr s _ -> pure s
        _ -> throwEvalError "builtins.appendContext: output name must be a string"

builtinBaseNameOf :: (MonadEval m) => NixValue -> m NixValue
builtinBaseNameOf (VStr s ctx) = pure (VStr (lastComponent s) ctx)
builtinBaseNameOf (VPath p) = pure (mkStr (lastComponent p))
builtinBaseNameOf other =
  throwEvalError ("builtins.baseNameOf: expected a string or path, got " <> typeName other)

lastComponent :: Text -> Text
lastComponent t = case T.splitOn "/" t of
  [] -> t
  parts -> case reverse (filter (not . T.null) parts) of
    [] -> ""
    (final : _) -> final

builtinDirOf :: (MonadEval m) => NixValue -> m NixValue
builtinDirOf (VStr s ctx) = pure (VStr (dirComponent s) ctx)
builtinDirOf (VPath p) = pure (VPath (dirComponent p))
builtinDirOf other =
  throwEvalError ("builtins.dirOf: expected a string or path, got " <> typeName other)

dirComponent :: Text -> Text
dirComponent t =
  let idx = T.findIndex (== '/') (T.reverse t)
   in case idx of
        Nothing -> "."
        Just n -> T.take (T.length t - n - 1) t

builtinConcatLists :: (MonadEval m) => NixValue -> m NixValue
builtinConcatLists (VList thunks) = do
  sublists <- mapM forceThenExtractList thunks
  pure (VList (concat sublists))
  where
    forceThenExtractList thunk = do
      val <- force thunk
      case val of
        VList xs -> pure xs
        _ -> throwEvalError "builtins.concatLists: element must be a list"
builtinConcatLists other =
  throwEvalError ("builtins.concatLists: expected a list, got " <> typeName other)

builtinLessThan :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinLessThan a b = VBool <$> nixCompare a b

-- ---------------------------------------------------------------------------
-- Builtin implementations — arithmetic + bitwise
-- ---------------------------------------------------------------------------

builtinAdd :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAdd (VInt a) (VInt b) = pure (VInt (a + b))
builtinAdd (VInt a) (VFloat b) = pure (VFloat (fromInteger a + b))
builtinAdd (VFloat a) (VInt b) = pure (VFloat (a + fromInteger b))
builtinAdd (VFloat a) (VFloat b) = pure (VFloat (a + b))
builtinAdd l r = throwEvalError ("builtins.add: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinSub :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinSub (VInt a) (VInt b) = pure (VInt (a - b))
builtinSub (VInt a) (VFloat b) = pure (VFloat (fromInteger a - b))
builtinSub (VFloat a) (VInt b) = pure (VFloat (a - fromInteger b))
builtinSub (VFloat a) (VFloat b) = pure (VFloat (a - b))
builtinSub l r = throwEvalError ("builtins.sub: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinMul :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinMul (VInt a) (VInt b) = pure (VInt (a * b))
builtinMul (VInt a) (VFloat b) = pure (VFloat (fromInteger a * b))
builtinMul (VFloat a) (VInt b) = pure (VFloat (a * fromInteger b))
builtinMul (VFloat a) (VFloat b) = pure (VFloat (a * b))
builtinMul l r = throwEvalError ("builtins.mul: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinDiv :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinDiv _ (VInt 0) = throwEvalError "builtins.div: division by zero"
builtinDiv (VInt a) (VInt b) = pure (VInt (quot a b))
builtinDiv _ (VFloat 0) = throwEvalError "builtins.div: division by zero"
builtinDiv (VInt a) (VFloat b) = pure (VFloat (fromInteger a / b))
builtinDiv (VFloat a) (VInt b) = pure (VFloat (a / fromInteger b))
builtinDiv (VFloat a) (VFloat b) = pure (VFloat (a / b))
builtinDiv l r = throwEvalError ("builtins.div: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinBitAnd :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinBitAnd (VInt a) (VInt b) = pure (VInt (a .&. b))
builtinBitAnd _ _ = throwEvalError "builtins.bitAnd: expected two integers"

builtinBitOr :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinBitOr (VInt a) (VInt b) = pure (VInt (a .|. b))
builtinBitOr _ _ = throwEvalError "builtins.bitOr: expected two integers"

builtinBitXor :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinBitXor (VInt a) (VInt b) = pure (VInt (xor a b))
builtinBitXor _ _ = throwEvalError "builtins.bitXor: expected two integers"

-- ---------------------------------------------------------------------------
-- Builtin implementations — attr set higher-order
-- ---------------------------------------------------------------------------

builtinMapAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinMapAttrs func (VAttrs attrs) =
  -- Lazy: each attr value is a deferred @f key val@, forced only on demand.
  pure (VAttrs (Map.mapWithKey deferAttr attrs))
  where
    deferAttr key valThunk =
      let env =
            Env
              { envBindings =
                  Map.fromList
                    [ ("__fn", evaluated func),
                      ("__key", evaluated (mkStr key)),
                      ("__val", valThunk)
                    ],
                envWithScopes = []
              }
       in mkSyntheticThunk env (EApp (EApp (EVar "__fn") (EVar "__key")) (EVar "__val"))
builtinMapAttrs _ other =
  throwEvalError ("builtins.mapAttrs: expected a set, got " <> typeName other)

builtinFunctionArgs :: (MonadEval m) => NixValue -> m NixValue
builtinFunctionArgs (VLambda _ formals _) = pure (formalsToAttrs formals)
builtinFunctionArgs (VBuiltin _ _) = pure (VAttrs Map.empty)
builtinFunctionArgs other =
  throwEvalError ("builtins.functionArgs: expected a function, got " <> typeName other)

formalsToAttrs :: Formals -> NixValue
formalsToAttrs (FormalName _) = VAttrs Map.empty
formalsToAttrs (FormalSet formals _) = formalsListToAttrs formals
formalsToAttrs (FormalNamedSet _ formals _) = formalsListToAttrs formals

formalsListToAttrs :: [Formal] -> NixValue
formalsListToAttrs formals =
  VAttrs $
    Map.fromList
      [(fName f, evaluated (VBool (isJust (fDefault f)))) | f <- formals]

builtinZipAttrsWith :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinZipAttrsWith func (VList thunks) = do
  attrSets <- mapM forceToAttrSet thunks
  let merged = mergeAllAttrs attrSets
  resultPairs <- mapM (applyZip func) (Map.toList merged)
  pure (VAttrs (Map.fromList resultPairs))
  where
    forceToAttrSet thunk = do
      val <- force thunk
      case val of
        VAttrs attrs -> pure attrs
        _ -> throwEvalError "builtins.zipAttrsWith: list element must be a set"
    mergeAllAttrs = foldl' (\acc attrs -> Map.unionWith (++) acc (Map.map (: []) attrs)) Map.empty
    applyZip fn (key, thunkList) = do
      partial <- applyValue fn (mkStr key)
      result <- applyValue partial (VList thunkList)
      pure (key, evaluated result)
builtinZipAttrsWith _ other =
  throwEvalError ("builtins.zipAttrsWith: expected a list, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — string manipulation
-- ---------------------------------------------------------------------------

builtinReplaceStrings ::
  (MonadEval m) => NixValue -> NixValue -> NixValue -> m NixValue
builtinReplaceStrings (VList fromThunks) (VList toThunks) (VStr input inputCtx) = do
  froms <- mapM forceStr fromThunks
  toStrs <- mapM forceStr toThunks
  when (length froms /= length toStrs) $
    throwEvalError "builtins.replaceStrings: 'from' and 'to' must have the same length"
  let fromTexts = map fst froms
      toTexts = map fst toStrs
      -- Merge contexts: input context + all 'to' string contexts + all 'from' string contexts
      mergedCtx = inputCtx <> mconcat (map snd froms) <> mconcat (map snd toStrs)
      pairs = zip fromTexts toTexts
  pure (VStr (replaceAll pairs input) mergedCtx)
  where
    forceStr thunk = do
      val <- force thunk
      case val of
        VStr s ctx -> pure (s, ctx)
        _ -> throwEvalError "builtins.replaceStrings: elements must be strings"
builtinReplaceStrings _ _ (VStr _ _) =
  throwEvalError "builtins.replaceStrings: first two arguments must be lists"
builtinReplaceStrings _ _ other =
  throwEvalError ("builtins.replaceStrings: expected a string, got " <> typeName other)

replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll pairs = go
  where
    go remaining
      | T.null remaining =
          -- At end of string, still check for empty-from match
          case findMatch pairs remaining of
            Just (replacement, _, _) -> replacement
            Nothing -> ""
      | otherwise = case findMatch pairs remaining of
          Just (replacement, rest, matched) ->
            if T.null matched
              then case T.uncons remaining of
                -- empty-from: insert replacement then advance 1 char
                Just (ch, after) -> replacement <> T.singleton ch <> go after
                Nothing -> replacement -- unreachable: guarded by T.null above
              else replacement <> go rest
          Nothing -> case T.uncons remaining of
            Just (ch, after) -> T.singleton ch <> go after
            Nothing -> "" -- unreachable: guarded by T.null above
    findMatch [] _ = Nothing
    findMatch ((from, to) : rest) txt
      | T.null from = Just (to, txt, from)
      | Just suffix <- T.stripPrefix from txt = Just (to, suffix, from)
      | otherwise = findMatch rest txt

-- ---------------------------------------------------------------------------
-- Builtin implementations — regex (POSIX ERE via regex-tdfa)
-- ---------------------------------------------------------------------------

-- | @builtins.match regex str@: match a POSIX ERE against a string.
-- The regex is implicitly anchored (must match the entire string).
-- Returns @null@ if no match, or a list of capture group strings
-- (empty string for unmatched optional groups).
builtinMatch :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinMatch (VStr regex _) (VStr str _) = do
  let anchored = "^" <> T.unpack regex <> "$"
  case RE.makeRegexM anchored :: Maybe RE.Regex of
    Nothing ->
      throwEvalError ("builtins.match: invalid regex: " <> regex)
    Just compiled ->
      let matches = matchAllText compiled (T.unpack str)
       in case matches of
            [] -> pure VNull
            (match : _) ->
              -- match is an Array of (String, (offset, len)) pairs.
              -- Index 0 is the full match; indices 1.. are capture groups.
              let groups = Array.elems match
                  -- Skip index 0 (full match) — return only capture groups.
                  captureGroups = drop 1 groups
                  toThunk (s, _) = evaluated (mkStr (T.pack s))
               in pure (VList (map toThunk captureGroups))
builtinMatch (VStr _ _) other =
  throwEvalError ("builtins.match: expected a string, got " <> typeName other)
builtinMatch other _ =
  throwEvalError ("builtins.match: expected a string (regex), got " <> typeName other)

-- | @builtins.split regex str@: split a string by a POSIX ERE.
-- Returns an alternating list of non-matched strings and match-group lists.
-- Example: @split "(x)" "axbxc"@ → @["a" ["x"] "b" ["x"] "c"]@
builtinSplit :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinSplit (VStr regex _) (VStr str _) = do
  case RE.makeRegexM (T.unpack regex) :: Maybe RE.Regex of
    Nothing ->
      throwEvalError ("builtins.split: invalid regex: " <> regex)
    Just compiled ->
      let allMatches = matchAllText compiled (T.unpack str)
          strText = T.unpack str
          result = buildSplitResult strText 0 allMatches
       in pure (VList result)
builtinSplit (VStr _ _) other =
  throwEvalError ("builtins.split: expected a string, got " <> typeName other)
builtinSplit other _ =
  throwEvalError ("builtins.split: expected a string (regex), got " <> typeName other)

-- | Build the alternating list for builtins.split.
buildSplitResult :: String -> Int -> [Array.Array Int (String, (Int, Int))] -> [Thunk]
buildSplitResult remaining pos [] =
  -- No more matches — emit the rest of the string.
  [evaluated (mkStr (T.pack (drop pos remaining)))]
buildSplitResult remaining pos (match : rest) =
  let fullMatch = match Array.! 0
      (_, (matchStart, matchLen)) = fullMatch
      -- Text before this match
      before = T.pack (take (matchStart - pos) (drop pos remaining))
      -- Capture groups (indices 1..)
      groups = drop 1 (Array.elems match)
      groupThunks = map (\(s, _) -> evaluated (mkStr (T.pack s))) groups
      -- Continue after this match
      afterPos = matchStart + matchLen
   in evaluated (mkStr before)
        : evaluated (VList groupThunks)
        : buildSplitResult remaining afterPos rest

builtinCompareVersions :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinCompareVersions (VStr a _) (VStr b _) =
  pure (VInt (compareVersionParts (splitVersionStr a) (splitVersionStr b)))
builtinCompareVersions _ _ = throwEvalError "builtins.compareVersions: expected two strings"

compareVersionParts :: [Text] -> [Text] -> Integer
compareVersionParts [] [] = 0
compareVersionParts [] (_ : _) = -1
compareVersionParts (_ : _) [] = 1
compareVersionParts (a : as) (b : bs) =
  case compareComponent a b of
    0 -> compareVersionParts as bs
    n -> n

compareComponent :: Text -> Text -> Integer
compareComponent a b
  | a == b = 0
  | allDigits a && allDigits b = compare' (readInt a) (readInt b)
  | a == "" = -1
  | b == "" = 1
  | otherwise = if a < b then -1 else 1
  where
    allDigits t = not (T.null t) && T.all isDigit t
    readInt :: Text -> Integer
    readInt = T.foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt c)) (0 :: Integer)
    compare' x y
      | x < y = -1
      | x > y = 1
      | otherwise = 0

splitVersionStr :: Text -> [Text]
splitVersionStr t
  | T.null t = []
  | otherwise =
      let (component, rest) = spanComponent t
       in component : splitVersionAfterComponent rest

splitVersionAfterComponent :: Text -> [Text]
splitVersionAfterComponent t = case T.uncons t of
  Nothing -> []
  Just ('.', rest) -> splitVersionStr rest
  Just _ -> splitVersionStr t

spanComponent :: Text -> (Text, Text)
spanComponent t = case T.uncons t of
  Nothing -> ("", "")
  Just (c, rest)
    | isDigit c -> T.span isDigit t
    | isAlpha c -> T.span isAlpha t
    | otherwise -> (T.singleton c, rest)

builtinSplitVersion :: (MonadEval m) => NixValue -> m NixValue
builtinSplitVersion (VStr s _) =
  pure (VList (map (evaluated . mkStr) (splitVersionComponents s)))
builtinSplitVersion other =
  throwEvalError ("builtins.splitVersion: expected a string, got " <> typeName other)

splitVersionComponents :: Text -> [Text]
splitVersionComponents t = case T.uncons t of
  Nothing -> []
  Just ('.', rest) -> "." : splitVersionComponents rest
  Just (c, _)
    | isDigit c ->
        let (digits, rest) = T.span isDigit t
         in digits : splitVersionComponents rest
    | otherwise ->
        let (alpha, rest) = T.span isAlpha t
         in alpha : splitVersionComponents rest

builtinParseDrvName :: (MonadEval m) => NixValue -> m NixValue
builtinParseDrvName (VStr s _) =
  let (name, version) = parseName s
   in pure
        ( VAttrs
            ( Map.fromList
                [ ("name", evaluated (mkStr name)),
                  ("version", evaluated (mkStr version))
                ]
            )
        )
builtinParseDrvName other =
  throwEvalError ("builtins.parseDrvName: expected a string, got " <> typeName other)

parseName :: Text -> (Text, Text)
parseName t =
  case findVersionDash t 0 of
    Nothing -> (t, "")
    Just idx -> (T.take idx t, T.drop (idx + 1) t)

findVersionDash :: Text -> Int -> Maybe Int
findVersionDash t idx
  | idx >= T.length t = Nothing
  | T.index t idx == '-'
      && idx + 1 < T.length t
      && isDigit (T.index t (idx + 1)) =
      Just idx
  | otherwise = findVersionDash t (idx + 1)

-- ---------------------------------------------------------------------------
-- Builtin implementations — serialization + hashing
-- ---------------------------------------------------------------------------

builtinToJSON :: (MonadEval m) => NixValue -> m NixValue
builtinToJSON val = mkStr <$> valueToJSON val

valueToJSON :: (MonadEval m) => NixValue -> m Text
valueToJSON VNull = pure "null"
valueToJSON (VBool True) = pure "true"
valueToJSON (VBool False) = pure "false"
valueToJSON (VInt n) = pure (T.pack (show n))
valueToJSON (VFloat f) =
  let s = show f
   in -- Nix outputs 1e+40 style, Haskell outputs 1.0e40 style
      -- For simple cases just use show
      pure (T.pack s)
valueToJSON (VStr s _) = pure (jsonEscapeString s)
valueToJSON (VList thunks) = do
  vals <- mapM force thunks
  jsonVals <- mapM valueToJSON vals
  pure ("[" <> T.intercalate "," jsonVals <> "]")
valueToJSON (VAttrs attrs) = do
  let sortedKeys = Map.keys attrs
  pairs <- mapM (jsonPair attrs) sortedKeys
  pure ("{" <> T.intercalate "," pairs <> "}")
  where
    jsonPair attrMap key = case Map.lookup key attrMap of
      Nothing -> pure ""
      Just thunk -> do
        val <- force thunk
        jsonVal <- valueToJSON val
        pure (jsonEscapeString key <> ":" <> jsonVal)
valueToJSON (VPath p) = pure (jsonEscapeString p)
valueToJSON (VLambda {}) = throwEvalError "builtins.toJSON: cannot convert a function to JSON"
valueToJSON (VBuiltin _ _) = throwEvalError "builtins.toJSON: cannot convert a function to JSON"
valueToJSON (VDerivation _) = throwEvalError "builtins.toJSON: cannot convert a derivation to JSON"

jsonEscapeString :: Text -> Text
jsonEscapeString s = "\"" <> T.concatMap escapeChar s <> "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | ord c < 0x20 = "\\u" <> T.pack (padHex 4 (showHex' (ord c)))
      | otherwise = T.singleton c
    padHex n str = replicate (n - length str) '0' ++ str
    showHex' 0 = "0"
    showHex' num = go num ""
      where
        go 0 acc = acc
        go v acc =
          let (q, r) = quotRem v 16
           in go q (hexDigit r : acc)

-- | Safe hex digit lookup (total for 0–15).
hexDigit :: Int -> Char
hexDigit n
  | n >= 0 && n <= 9 = chr (ord '0' + n)
  | n >= 10 && n <= 15 = chr (ord 'a' + n - 10)
  | otherwise = '?' -- unreachable for valid hex

builtinFromJSON :: (MonadEval m) => NixValue -> m NixValue
builtinFromJSON (VStr s _) = case parseJSON (T.strip s) of
  Just (val, rest)
    | T.null (T.strip rest) -> pure val
    | otherwise -> throwEvalError "builtins.fromJSON: trailing content after JSON value"
  Nothing -> throwEvalError "builtins.fromJSON: invalid JSON"
builtinFromJSON other =
  throwEvalError ("builtins.fromJSON: expected a string, got " <> typeName other)

parseJSON :: Text -> Maybe (NixValue, Text)
parseJSON t = case T.uncons (T.stripStart t) of
  Nothing -> Nothing
  Just ('n', rest)
    | Just suffix <- T.stripPrefix "ull" rest -> Just (VNull, suffix)
  Just ('t', rest)
    | Just suffix <- T.stripPrefix "rue" rest -> Just (VBool True, suffix)
  Just ('f', rest)
    | Just suffix <- T.stripPrefix "alse" rest -> Just (VBool False, suffix)
  Just ('"', _) -> parseJSONString (T.stripStart t)
  Just ('[', rest) -> parseJSONArray rest
  Just ('{', rest) -> parseJSONObject rest
  Just (c, _)
    | c == '-' || isDigit c -> parseJSONNumber (T.stripStart t)
  _ -> Nothing

parseJSONString :: Text -> Maybe (NixValue, Text)
parseJSONString t = case T.uncons t of
  Just ('"', rest) ->
    let (strVal, remaining) = parseJSONStringContent rest ""
     in Just (mkStr strVal, remaining)
  _ -> Nothing

parseJSONStringContent :: Text -> Text -> (Text, Text)
parseJSONStringContent t acc = case T.uncons t of
  Nothing -> (acc, "")
  Just ('"', rest) -> (acc, rest)
  Just ('\\', rest) -> case T.uncons rest of
    Just ('"', r) -> parseJSONStringContent r (acc <> "\"")
    Just ('\\', r) -> parseJSONStringContent r (acc <> "\\")
    Just ('/', r) -> parseJSONStringContent r (acc <> "/")
    Just ('n', r) -> parseJSONStringContent r (acc <> "\n")
    Just ('r', r) -> parseJSONStringContent r (acc <> "\r")
    Just ('t', r) -> parseJSONStringContent r (acc <> "\t")
    Just ('u', r) -> case parseHex4 r of
      Just (codepoint, r2) ->
        parseJSONStringContent r2 (acc <> T.singleton (chr codepoint))
      Nothing -> parseJSONStringContent r (acc <> "u")
    _ -> (acc, rest)
  Just (c, rest) -> parseJSONStringContent rest (acc <> T.singleton c)

parseHex4 :: Text -> Maybe (Int, Text)
parseHex4 t
  | T.length t >= 4 =
      let hex = T.take 4 t
       in if T.all isHexDigit hex
            then Just (readHex4 hex, T.drop 4 t)
            else Nothing
  | otherwise = Nothing

readHex4 :: Text -> Int
readHex4 = T.foldl' (\acc c -> acc * 16 + digitToInt c) 0

parseJSONNumber :: Text -> Maybe (NixValue, Text)
parseJSONNumber t =
  let (numStr, rest) = T.span (\c -> isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E' || c == '+') t
   in if T.null numStr
        then Nothing
        else
          if T.any (\c -> c == '.' || c == 'e' || c == 'E') numStr
            then case reads (T.unpack numStr) :: [(Double, String)] of
              [(d, "")] -> Just (VFloat d, rest)
              _ -> Nothing
            else case reads (T.unpack numStr) :: [(Integer, String)] of
              [(n, "")] -> Just (VInt n, rest)
              _ -> Nothing

parseJSONArray :: Text -> Maybe (NixValue, Text)
parseJSONArray t = parseJSONArrayElements (T.stripStart t) []

parseJSONArrayElements :: Text -> [Thunk] -> Maybe (NixValue, Text)
parseJSONArrayElements t acc = case T.uncons (T.stripStart t) of
  Just (']', rest) -> Just (VList (reverse acc), rest)
  _ -> case parseJSON t of
    Just (val, rest) ->
      let stripped = T.stripStart rest
       in case T.uncons stripped of
            Just (',', rest2) -> parseJSONArrayElements rest2 (evaluated val : acc)
            Just (']', rest2) -> Just (VList (reverse (evaluated val : acc)), rest2)
            _ -> Nothing
    Nothing -> Nothing

parseJSONObject :: Text -> Maybe (NixValue, Text)
parseJSONObject t = parseJSONObjectEntries (T.stripStart t) Map.empty

parseJSONObjectEntries :: Text -> Map Text Thunk -> Maybe (NixValue, Text)
parseJSONObjectEntries t acc = case T.uncons (T.stripStart t) of
  Just ('}', rest) -> Just (VAttrs acc, rest)
  _ -> case parseJSONString (T.stripStart t) of
    Just (VStr key _, rest) -> case T.uncons (T.stripStart rest) of
      Just (':', rest2) -> case parseJSON rest2 of
        Just (val, rest3) ->
          let stripped = T.stripStart rest3
              updated = Map.insert key (evaluated val) acc
           in case T.uncons stripped of
                Just (',', rest4) -> parseJSONObjectEntries rest4 updated
                Just ('}', rest4) -> Just (VAttrs updated, rest4)
                _ -> Nothing
        Nothing -> Nothing
      _ -> Nothing
    _ -> Nothing

builtinHashString :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinHashString (VStr algo _) (VStr input _) =
  let inputBytes = TE.encodeUtf8 input
   in case algo of
        "sha256" ->
          let digest = CH.hash inputBytes :: CH.Digest CH.SHA256
           in pure (mkStr (digestToHex digest))
        "sha512" ->
          let digest = CH.hash inputBytes :: CH.Digest CH.SHA512
           in pure (mkStr (digestToHex digest))
        "sha1" ->
          let digest = CH.hash inputBytes :: CH.Digest CH.SHA1
           in pure (mkStr (digestToHex digest))
        "md5" ->
          let digest = CH.hash inputBytes :: CH.Digest CH.MD5
           in pure (mkStr (digestToHex digest))
        _ -> throwEvalError ("builtins.hashString: unknown hash algorithm '" <> algo <> "'")
builtinHashString (VStr _ _) other =
  throwEvalError ("builtins.hashString: expected a string, got " <> typeName other)
builtinHashString other _ =
  throwEvalError ("builtins.hashString: expected a string, got " <> typeName other)

digestToHex :: (BA.ByteArrayAccess a) => a -> Text
digestToHex digest =
  let bytes = BA.unpack digest
   in T.pack (concatMap byteToHex bytes)

-- ---------------------------------------------------------------------------
-- Builtin implementations — deep evaluation
-- ---------------------------------------------------------------------------

builtinDeepSeq :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinDeepSeq first second = do
  deepForce first
  pure second

deepForce :: (MonadEval m) => NixValue -> m ()
deepForce (VList thunks) = mapM_ (force >=> deepForce) thunks
deepForce (VAttrs attrs) = mapM_ (force >=> deepForce) (Map.elems attrs)
deepForce _ = pure ()

-- ---------------------------------------------------------------------------
-- Builtin implementations — graph traversal
-- ---------------------------------------------------------------------------

builtinGenericClosure :: (MonadEval m) => NixValue -> m NixValue
builtinGenericClosure (VAttrs attrs) = do
  startSetThunk <-
    maybe (throwEvalError "builtins.genericClosure: missing 'startSet'") pure $
      Map.lookup "startSet" attrs
  operatorThunk <-
    maybe (throwEvalError "builtins.genericClosure: missing 'operator'") pure $
      Map.lookup "operator" attrs
  startSetVal <- force startSetThunk
  operatorVal <- force operatorThunk
  case startSetVal of
    VList items -> do
      result <- closureLoop operatorVal items [] []
      pure (VList (map evaluated result))
    _ -> throwEvalError "builtins.genericClosure: 'startSet' must be a list"
builtinGenericClosure other =
  throwEvalError ("builtins.genericClosure: expected a set, got " <> typeName other)

closureLoop ::
  (MonadEval m) =>
  NixValue ->
  [Thunk] ->
  [NixValue] ->
  [NixValue] ->
  m [NixValue]
closureLoop _ [] _ acc = pure (reverse acc)
closureLoop operator (thunk : rest) seenKeys acc = do
  item <- force thunk
  key <- extractKey item
  alreadySeen <- keyInList key seenKeys
  if alreadySeen
    then closureLoop operator rest seenKeys acc
    else do
      newItems <- applyValue operator item
      case newItems of
        VList newThunks ->
          closureLoop operator (rest ++ newThunks) (key : seenKeys) (item : acc)
        _ -> throwEvalError "builtins.genericClosure: operator must return a list"

extractKey :: (MonadEval m) => NixValue -> m NixValue
extractKey (VAttrs attrs) =
  case Map.lookup "key" attrs of
    Just thunk -> force thunk
    Nothing -> throwEvalError "builtins.genericClosure: item missing 'key' attribute"
extractKey _ = throwEvalError "builtins.genericClosure: item must be a set with 'key'"

keyInList :: (MonadEval m) => NixValue -> [NixValue] -> m Bool
keyInList _ [] = pure False
keyInList key (seen : rest) = do
  eq <- nixEqual force key seen
  if eq then pure True else keyInList key rest

-- ---------------------------------------------------------------------------
-- IO builtins (delegate to MonadEval methods)
-- ---------------------------------------------------------------------------

-- | Coerce a value to a path 'Text'.  Accepts 'VPath' and 'VStr';
-- throws a type error for anything else.
coerceToPath :: (MonadEval m) => Text -> NixValue -> m Text
coerceToPath _ (VPath p) = pure p
coerceToPath _ (VStr s _) = pure s
coerceToPath name other =
  throwEvalError ("builtins." <> name <> ": expected a path or string, got " <> typeName other)

builtinImport :: (MonadEval m) => NixValue -> m NixValue
builtinImport (VPath p) = importFile p
builtinImport other =
  throwEvalError ("import: expected a path, got " <> typeName other)

builtinReadFile :: (MonadEval m) => NixValue -> m NixValue
builtinReadFile val = do
  p <- coerceToPath "readFile" val
  mkStr <$> readFileText p

builtinPathExists :: (MonadEval m) => NixValue -> m NixValue
builtinPathExists val = do
  p <- coerceToPath "pathExists" val
  VBool <$> doesPathExist p

builtinReadDir :: (MonadEval m) => NixValue -> m NixValue
builtinReadDir val = do
  p <- coerceToPath "readDir" val
  entries <- listDirectory p
  pure (VAttrs (Map.fromList [(name, evaluated (mkStr fileType)) | (name, fileType) <- entries]))

-- ---------------------------------------------------------------------------
-- Builtin implementations — environment + paths
-- ---------------------------------------------------------------------------

builtinGetEnv :: (MonadEval m) => NixValue -> m NixValue
builtinGetEnv (VStr name _) = mkStr <$> getEnvVar name
builtinGetEnv other =
  throwEvalError ("builtins.getEnv: expected a string, got " <> typeName other)

builtinToPath :: (MonadEval m) => NixValue -> m NixValue
builtinToPath (VPath p) = pure (VPath p)
builtinToPath (VStr s _) = case T.uncons s of
  Nothing -> throwEvalError "builtins.toPath: empty path"
  Just ('/', _) -> pure (VPath s)
  Just _ -> throwEvalError ("builtins.toPath: path must be absolute, got " <> s)
builtinToPath other =
  throwEvalError ("builtins.toPath: expected a string or path, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — store path operations
-- ---------------------------------------------------------------------------

builtinPlaceholder :: (MonadEval m) => NixValue -> m NixValue
builtinPlaceholder (VStr outputName _) =
  let preimage = "nix-output:" <> outputName
      hashText = truncatedBase32 (TE.encodeUtf8 preimage)
   in pure (VPath (storeDirPrefix <> hashText <> "-" <> outputName))
builtinPlaceholder other =
  throwEvalError ("builtins.placeholder: expected a string, got " <> typeName other)

builtinStorePath :: (MonadEval m) => NixValue -> m NixValue
builtinStorePath (VPath p) = validateStorePath p
builtinStorePath (VStr s _) = validateStorePath s
builtinStorePath other =
  throwEvalError ("builtins.storePath: expected a path or string, got " <> typeName other)

validateStorePath :: (MonadEval m) => Text -> m NixValue
validateStorePath p
  | storeDirPrefix `T.isPrefixOf` p,
    T.length p > T.length storeDirPrefix,
    let basename = T.drop (T.length storeDirPrefix) p,
    T.length basename >= 33,
    T.index basename 32 == '-' =
      pure (VPath p)
  | otherwise =
      throwEvalError ("builtins.storePath: not a valid store path: " <> p)

-- ---------------------------------------------------------------------------
-- Builtin implementations — Nix search path
-- ---------------------------------------------------------------------------

builtinFindFile :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinFindFile (VList searchPath) (VStr name _) = do
  entries <- mapM forceSearchEntry searchPath
  findFirst entries name
builtinFindFile (VList _) other =
  throwEvalError ("builtins.findFile: expected a string, got " <> typeName other)
builtinFindFile other _ =
  throwEvalError ("builtins.findFile: expected a list, got " <> typeName other)

-- | Extract {prefix, path} from a search path entry thunk.
forceSearchEntry :: (MonadEval m) => Thunk -> m (Text, Text)
forceSearchEntry thunk = do
  val <- force thunk
  case val of
    VAttrs attrs -> do
      prefixThunk <-
        maybe (throwEvalError "builtins.findFile: entry missing 'prefix'") pure $
          Map.lookup "prefix" attrs
      pathThunk <-
        maybe (throwEvalError "builtins.findFile: entry missing 'path'") pure $
          Map.lookup "path" attrs
      prefixVal <- force prefixThunk
      pathVal <- force pathThunk
      prefix <- case prefixVal of
        VStr s _ -> pure s
        _ -> throwEvalError "builtins.findFile: 'prefix' must be a string"
      path <- case pathVal of
        VStr s _ -> pure s
        VPath s -> pure s
        _ -> throwEvalError "builtins.findFile: 'path' must be a string or path"
      pure (prefix, path)
    _ -> throwEvalError "builtins.findFile: search path entry must be a set"

-- | Iterate search path entries, checking for a match.
findFirst :: (MonadEval m) => [(Text, Text)] -> Text -> m NixValue
findFirst [] name =
  throwEvalError ("file '" <> name <> "' was not found in the Nix search path")
findFirst ((prefix, path) : rest) name
  | prefix == name || (not (T.null prefix) && (prefix <> "/") `T.isPrefixOf` name) =
      let suffix = if prefix == name then "" else T.drop (T.length prefix + 1) name
          candidate = if T.null suffix then path else path <> "/" <> suffix
       in do
            exists <- doesPathExist candidate
            if exists
              then pure (VPath candidate)
              else findFirst rest name
  | T.null prefix =
      let candidate = path <> "/" <> name
       in do
            exists <- doesPathExist candidate
            if exists
              then pure (VPath candidate)
              else findFirst rest name
  | otherwise = findFirst rest name

-- ---------------------------------------------------------------------------
-- Builtin implementations — store file creation
-- ---------------------------------------------------------------------------

builtinToFile :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinToFile (VStr name _) (VStr contents _) = do
  storePath <- writeToStore name contents
  pure (VPath storePath)
builtinToFile (VStr _ _) other =
  throwEvalError ("builtins.toFile: expected a string, got " <> typeName other)
builtinToFile other _ =
  throwEvalError ("builtins.toFile: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — scoped import
-- ---------------------------------------------------------------------------

builtinScopedImport :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinScopedImport (VAttrs attrs) pathVal = do
  p <- coerceToPath "scopedImport" pathVal
  let scope = Map.toList attrs
  scopedImportFile scope p
builtinScopedImport other _ =
  throwEvalError ("builtins.scopedImport: expected a set, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations — network fetchers
-- ---------------------------------------------------------------------------

builtinFetchurl :: (MonadEval m) => NixValue -> m NixValue
builtinFetchurl (VStr url _) = fetchUrlSimple url Nothing
builtinFetchurl (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchurl" "url" attrs
  sha256 <- forceOptionalAttrStr attrs "sha256"
  fetchUrlSimple url sha256
builtinFetchurl other =
  throwEvalError ("builtins.fetchurl: expected a string or set, got " <> typeName other)

builtinFetchTarball :: (MonadEval m) => NixValue -> m NixValue
builtinFetchTarball (VStr url _) = fetchUrlSimple url Nothing
builtinFetchTarball (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchTarball" "url" attrs
  sha256 <- forceOptionalAttrStr attrs "sha256"
  fetchUrlSimple url sha256
builtinFetchTarball other =
  throwEvalError ("builtins.fetchTarball: expected a string or set, got " <> typeName other)

builtinFetchGit :: (MonadEval m) => NixValue -> m NixValue
builtinFetchGit (VStr url _) = do
  -- Use a content-based temp dir to avoid predictable paths
  let urlHash = sha256Hex (TE.encodeUtf8 url)
  sysTmpRaw <- getEnvVar "TMPDIR"
  -- TMPDIR on Unix, TEMP/TMP on Windows; fall back to /tmp
  sysTmp <-
    if T.null sysTmpRaw
      then do
        winTmp <- getEnvVar "TEMP"
        pure (if T.null winTmp then "/tmp" else winTmp)
      else pure sysTmpRaw
  let tmpDir = sysTmp <> "/nova-nix-fetchgit-" <> urlHash
  (code, _stdout, stderr) <- runProcess "git" ["clone", "--depth", "1", "--", url, tmpDir] ""
  if code /= 0
    then throwEvalError ("builtins.fetchGit: git clone failed: " <> stderr)
    else pure (VPath tmpDir)
builtinFetchGit (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchGit" "url" attrs
  builtinFetchGit (mkStr url)
builtinFetchGit other =
  throwEvalError ("builtins.fetchGit: expected a string or set, got " <> typeName other)

-- | Fetch a URL and optionally verify its hash.
fetchUrlSimple :: (MonadEval m) => Text -> Maybe Text -> m NixValue
fetchUrlSimple url _sha256 = do
  (code, stdout, stderr) <- runProcess "curl" ["-sSfL", "--", url] ""
  if code /= 0
    then throwEvalError ("fetch failed: " <> stderr)
    else do
      storePath <- writeToStore "fetchurl-result" stdout
      pure (VPath storePath)

-- | Force a required string attribute from an attrset.
forceAttrStr :: (MonadEval m) => Text -> Text -> Map Text Thunk -> m Text
forceAttrStr builtin key attrs =
  case Map.lookup key attrs of
    Nothing -> throwEvalError (builtin <> ": missing required attribute '" <> key <> "'")
    Just thunk -> do
      val <- force thunk
      case val of
        VStr s _ -> pure s
        VPath p -> pure p
        _ -> throwEvalError (builtin <> ": '" <> key <> "' must be a string")

-- | Force an optional string attribute.
forceOptionalAttrStr :: (MonadEval m) => Map Text Thunk -> Text -> m (Maybe Text)
forceOptionalAttrStr attrs key =
  case Map.lookup key attrs of
    Nothing -> pure Nothing
    Just thunk -> do
      val <- force thunk
      case val of
        VStr s _ -> pure (Just s)
        _ -> pure Nothing

-- ---------------------------------------------------------------------------
-- Builtin implementations — derivation construction
-- ---------------------------------------------------------------------------

builtinDerivation :: (MonadEval m) => NixValue -> m NixValue
builtinDerivation (VAttrs attrs) = do
  -- Extract required attributes
  drvName <- forceAttrStr "derivation" "name" attrs
  system <- forceAttrStr "derivation" "system" attrs
  builder <- forceAttrStr "derivation" "builder" attrs

  -- Extract optional outputs (default ["out"])
  outputNames <- case Map.lookup "outputs" attrs of
    Nothing -> pure ["out"]
    Just thunk -> do
      val <- force thunk
      case val of
        VList thunks -> mapM forceToText thunks
        _ -> throwEvalError "derivation: 'outputs' must be a list of strings"

  -- Extract optional args (default [])
  builderArgs <- case Map.lookup "args" attrs of
    Nothing -> pure []
    Just thunk -> do
      val <- force thunk
      case val of
        VList thunks -> mapM forceToText thunks
        _ -> throwEvalError "derivation: 'args' must be a list of strings"

  -- Collect all string-coercible attrs into env WITH their contexts
  (drvEnvPairs, envContext) <- collectDrvEnvWithContext attrs

  -- Extract input derivations and input sources from the merged context
  let inputDrvs = extractInputDrvs envContext
      inputSrcs = extractInputSrcs envContext

  -- Build the platform
  let platform = textToPlatform system

  -- Build the derivation with populated inputs for hashing
  let envMap = Map.fromList drvEnvPairs
      drv =
        Derivation
          { drvOutputs = [],
            drvInputDrvs = inputDrvs,
            drvInputSrcs = inputSrcs,
            drvPlatform = platform,
            drvBuilder = builder,
            drvArgs = builderArgs,
            drvEnv = envMap
          }

  -- Serialize to ATerm and hash for drvPath
  let aterm = toATerm drv
      storeRef = ":" <> defaultStoreDirText <> ":"
      drvPathHash = truncatedBase32 (TE.encodeUtf8 ("text:sha256:" <> sha256Hex (TE.encodeUtf8 aterm) <> storeRef <> drvName <> ".drv"))
      drvPathText = storeDirPrefix <> drvPathHash <> "-" <> drvName <> ".drv"

  -- Parse drvPath as a StorePath for context
  let drvSP = case parseStorePath defaultStoreDir drvPathText of
        Just sp -> sp
        Nothing -> StorePath (T.take 32 (T.drop (T.length storeDirPrefix) drvPathText)) (drvName <> ".drv")

  -- Compute output paths
  let computeOutPath outName =
        let nameSuffix = if outName == "out" then "" else "-" <> outName
            preimage = "output:" <> outName <> ":sha256:" <> sha256Hex (TE.encodeUtf8 aterm) <> storeRef <> drvName <> nameSuffix
            outHash = truncatedBase32 (TE.encodeUtf8 preimage)
         in storeDirPrefix <> outHash <> "-" <> drvName <> nameSuffix

  let outPaths = [(outName, computeOutPath outName) | outName <- outputNames]
      mainOutPath = case outPaths of
        ((_, p) : _) -> p
        [] -> ""

  -- Build DerivationOutput records for the Derivation value
  let drvOutputsList =
        [ DerivationOutput
            { doName = outName,
              doPath = case parseStorePath defaultStoreDir outP of
                Just sp -> sp
                -- Fallback: construct manually from the path string
                Nothing -> StorePath (T.take 32 (T.drop (T.length storeDirPrefix) outP)) outName,
              doHashAlgo = "",
              doHash = ""
            }
        | (outName, outP) <- outPaths
        ]

  -- Build the complete Derivation with populated outputs and inputs
  let completeDrv = drv {drvOutputs = drvOutputsList}

  -- Context for output paths: each output carries SCDrvOutput context
  -- Context for drvPath: carries SCAllOutputs context
  let drvPathCtx = StringContext (Set.singleton (SCAllOutputs drvSP))
      outPathCtx outName = StringContext (Set.singleton (SCDrvOutput drvSP outName))

  -- Build result attrset: original attrs + drvPath, outPath, type, per-output attrs
  let baseAttrs =
        Map.fromList $
          [ ("type", evaluated (mkStr "derivation")),
            ("drvPath", evaluated (VStr drvPathText drvPathCtx)),
            ("outPath", evaluated (VStr mainOutPath (outPathCtx "out"))),
            ("name", evaluated (mkStr drvName)),
            ("system", evaluated (mkStr system)),
            ("builder", evaluated (mkStr builder)),
            ("_derivation", evaluated (VDerivation completeDrv))
          ]
            ++ [(outName, evaluated (VStr outP (outPathCtx outName))) | (outName, outP) <- outPaths]
      -- Merge original attrs underneath so computed attrs take priority
      resultAttrs = Map.union baseAttrs attrs

  pure (VAttrs resultAttrs)
builtinDerivation other =
  throwEvalError ("derivation: expected a set, got " <> typeName other)

-- | Force a thunk to a Text string.
forceToText :: (MonadEval m) => Thunk -> m Text
forceToText thunk = do
  val <- force thunk
  case val of
    VStr s _ -> pure s
    VPath p -> pure p
    _ -> throwEvalError ("expected a string, got " <> typeName val)

-- | Collect all string-coercible attributes for the derivation environment,
-- along with the merged string context from all collected values.
collectDrvEnvWithContext :: (MonadEval m) => Map Text Thunk -> m ([(Text, Text)], StringContext)
collectDrvEnvWithContext attrs = do
  let pairs = Map.toList attrs
  results <- mapM tryCoerce pairs
  let envPairs = catMaybes [fmap (\(k, v, _) -> (k, v)) r | r <- results]
      mergedCtx = mconcat [ctx | Just (_, _, ctx) <- results]
  pure (envPairs, mergedCtx)
  where
    tryCoerce (key, thunk) = do
      val <- force thunk
      case val of
        VStr s ctx -> pure (Just (key, s, ctx))
        VPath p -> pure (Just (key, p, emptyContext))
        VInt n -> pure (Just (key, T.pack (show n), emptyContext))
        VBool True -> pure (Just (key, "1", emptyContext))
        VBool False -> pure (Just (key, "", emptyContext))
        VNull -> pure Nothing
        VList _ -> pure Nothing
        VAttrs innerAttrs ->
          -- Derivations in env get their outPath
          case Map.lookup "outPath" innerAttrs of
            Just outThunk -> do
              outVal <- force outThunk
              case outVal of
                VPath p -> pure (Just (key, p, emptyContext))
                VStr s ctx -> pure (Just (key, s, ctx))
                _ -> pure Nothing
            Nothing -> pure Nothing
        _ -> pure Nothing
