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

    -- * Environment (re-exported from Types)
    Env (..),
    emptyEnv,

    -- * Evaluation
    eval,
    force,

    -- * Helpers (for Builtins)
    typeName,
    evaluated,
  )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.Operator (evalBinary, evalUnary)
import Nix.Eval.StringInterp (coerceToString, evalIndStringParts, evalStringParts)
import Nix.Eval.Types
  ( Env (..),
    NixValue (..),
    Thunk (..),
    emptyEnv,
    envInsertThunk,
    envLookup,
    evaluated,
    mkThunk,
    pushWithScope,
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
  )
import qualified System.Info

-- | Evaluate a Nix expression in an environment.
eval :: Env -> Expr -> Either Text NixValue
eval env expr = case expr of
  ELit atom -> evalLit atom
  EStr parts -> VStr <$> evalStringParts eval env parts
  EIndStr parts -> VStr <$> evalIndStringParts eval env parts
  EVar name -> evalVar env name
  EAttrs isRec bindings -> evalAttrs env isRec bindings
  EList exprs -> Right (VList (map (mkThunk env) exprs))
  ESelect target path defExpr -> evalSelect env target path defExpr
  EHasAttr target path -> evalHasAttr env target path
  EApp func arg -> evalApp env func arg
  ELambda formals body -> Right (VLambda env formals body)
  ELet bindings body -> evalLet env bindings body
  EIf cond thenExpr elseExpr -> evalIf env cond thenExpr elseExpr
  EWith scope body -> evalWith env scope body
  EAssert cond body -> evalAssert env cond body
  EUnary op operand -> do
    val <- eval env operand
    evalUnary op val
  EBinary OpAnd left right -> evalShortCircuitAnd env left right
  EBinary OpOr left right -> evalShortCircuitOr env left right
  EBinary OpImpl left right -> evalShortCircuitImpl env left right
  EBinary op left right -> do
    leftVal <- eval env left
    rightVal <- eval env right
    evalBinary force op leftVal rightVal

-- | Force a thunk to a value.
force :: Thunk -> Either Text NixValue
force (Evaluated val) = Right val
force (Thunk thunkExpr thunkEnv) = eval thunkEnv thunkExpr

-- ---------------------------------------------------------------------------
-- Literal
-- ---------------------------------------------------------------------------

evalLit :: NixAtom -> Either Text NixValue
evalLit atom = case atom of
  NixInt n -> Right (VInt n)
  NixFloat n -> Right (VFloat n)
  NixBool b -> Right (VBool b)
  NixNull -> Right VNull
  NixUri u -> Right (VStr u)
  NixPath p -> Right (VPath p)

-- ---------------------------------------------------------------------------
-- Variables
-- ---------------------------------------------------------------------------

evalVar :: Env -> Text -> Either Text NixValue
evalVar env name =
  case envLookup name env of
    Just thunk -> force thunk
    Nothing -> Left ("undefined variable '" <> name <> "'")

-- ---------------------------------------------------------------------------
-- Attribute sets
-- ---------------------------------------------------------------------------

evalAttrs :: Env -> Bool -> [Binding] -> Either Text NixValue
evalAttrs env False bindings = evalNonRecAttrs env bindings
evalAttrs env True bindings = evalRecAttrs env bindings

-- | Non-recursive attribute set: thunks capture the outer environment.
evalNonRecAttrs :: Env -> [Binding] -> Either Text NixValue
evalNonRecAttrs env bindings = do
  attrMap <- evalBindings env env bindings
  pure (VAttrs attrMap)

-- | Recursive attribute set: thunks capture the completed environment
-- (Haskell's laziness ties the knot — Thunk's Env field is lazy).
evalRecAttrs :: Env -> [Binding] -> Either Text NixValue
evalRecAttrs env bindings =
  let recEnv = env {envBindings = Map.union recBindings (envBindings env)}
      recBindings = case evalBindings recEnv recEnv bindings of
        Right m -> m
        Left _ -> Map.empty
   in Right (VAttrs recBindings)

-- | Process bindings into a flat attribute map.
--
-- Handles nested attribute paths (@a.b.c = 1@) by building nested
-- 'VAttrs' maps.  Handles @inherit@ by looking up names in the
-- environment or a source expression.
evalBindings :: Env -> Env -> [Binding] -> Either Text (Map Text Thunk)
evalBindings thunkEnv lookupEnv =
  foldl' mergeBinding (Right Map.empty)
  where
    mergeBinding acc binding = do
      current <- acc
      new <- processBinding thunkEnv lookupEnv binding
      pure (mergeAttrMaps current new)

-- | Process a single binding into key-value pairs.
processBinding :: Env -> Env -> Binding -> Either Text (Map Text Thunk)
processBinding thunkEnv _ (NamedBinding path bodyExpr) =
  buildNestedAttr thunkEnv path bodyExpr
processBinding _ lookupEnv (Inherit Nothing names) =
  Right $ Map.fromList [(n, inheritLookup lookupEnv n) | n <- names]
processBinding thunkEnv _ (Inherit (Just fromExpr) names) =
  Right $
    Map.fromList
      [ (n, mkThunk thunkEnv (ESelect fromExpr [StaticKey n] Nothing))
      | n <- names
      ]

-- | Look up a name for @inherit@.  If not found, create a thunk
-- that will error when forced.
inheritLookup :: Env -> Text -> Thunk
inheritLookup env name =
  case envLookup name env of
    Just thunk -> thunk
    Nothing -> evaluated (VStr ("<undefined: " <> name <> ">"))

-- | Build a nested attribute structure from a dotted path.
-- @a.b.c = expr@ becomes @{ a = { b = { c = thunk; }; }; }@.
buildNestedAttr :: Env -> AttrPath -> Expr -> Either Text (Map Text Thunk)
buildNestedAttr thunkEnv path bodyExpr = case path of
  [] -> Left "empty attribute path"
  [key] -> do
    keyText <- resolveStaticKey key
    pure (Map.singleton keyText (mkThunk thunkEnv bodyExpr))
  (key : rest) -> do
    keyText <- resolveStaticKey key
    inner <- buildNestedAttr thunkEnv rest bodyExpr
    pure (Map.singleton keyText (evaluated (VAttrs inner)))

-- | Resolve a static attribute key to its text name.
resolveStaticKey :: AttrKey -> Either Text Text
resolveStaticKey (StaticKey name) = Right name
resolveStaticKey (DynamicKey _) = Left "dynamic attribute keys not yet supported"

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

evalSelect :: Env -> Expr -> AttrPath -> Maybe Expr -> Either Text NixValue
evalSelect env target path defExpr = do
  targetVal <- eval env target
  result <- walkAttrPath path targetVal
  case result of
    Just val -> Right val
    Nothing -> case defExpr of
      Just def -> eval env def
      Nothing -> Left ("attribute path not found in " <> typeName targetVal)

-- | Walk an attribute path through nested attribute sets.
-- Returns @Just value@ if the full path resolves, @Nothing@ if any
-- key is missing or a non-set is encountered mid-path.
walkAttrPath :: AttrPath -> NixValue -> Either Text (Maybe NixValue)
walkAttrPath [] val = Right (Just val)
walkAttrPath (key : rest) val = case val of
  VAttrs attrs -> do
    keyText <- resolveStaticKey key
    case Map.lookup keyText attrs of
      Just thunk -> do
        inner <- force thunk
        walkAttrPath rest inner
      Nothing -> Right Nothing
  _ -> Right Nothing

evalHasAttr :: Env -> Expr -> AttrPath -> Either Text NixValue
evalHasAttr env target path = do
  targetVal <- eval env target
  result <- walkAttrPath path targetVal
  case result of
    Just _ -> Right (VBool True)
    Nothing -> Right (VBool False)

-- ---------------------------------------------------------------------------
-- Application
-- ---------------------------------------------------------------------------

evalApp :: Env -> Expr -> Expr -> Either Text NixValue
evalApp env funcExpr argExpr = do
  funcVal <- eval env funcExpr
  case funcVal of
    VLambda closureEnv formals body -> do
      let argThunk = mkThunk env argExpr
      extEnv <- matchFormals closureEnv formals argThunk
      eval extEnv body
    VBuiltin name -> do
      argVal <- eval env argExpr
      applyBuiltin name argVal
    _ -> Left ("attempt to call " <> typeName funcVal <> ", which is not a function")

-- | Match function formals against an argument thunk.
matchFormals :: Env -> Formals -> Thunk -> Either Text Env
matchFormals closureEnv (FormalName name) argThunk =
  Right (envInsertThunk name argThunk closureEnv)
matchFormals closureEnv (FormalSet formals allowExtra) argThunk = do
  argVal <- force argThunk
  matchFormalSet closureEnv formals allowExtra argVal
matchFormals closureEnv (FormalNamedSet name formals allowExtra) argThunk = do
  argVal <- force argThunk
  matched <- matchFormalSet closureEnv formals allowExtra argVal
  pure (envInsertThunk name argThunk matched)

-- | Match a formal set pattern against a VAttrs argument.
matchFormalSet :: Env -> [Formal] -> Bool -> NixValue -> Either Text Env
matchFormalSet closureEnv formals allowExtra argVal =
  case argVal of
    VAttrs attrs -> do
      checkExtraKeys formals allowExtra attrs
      foldl' (bindFormal attrs) (Right closureEnv) formals
    _ -> Left ("function expects a set argument, got " <> typeName argVal)

-- | Verify that no unexpected keys are present (unless @...@ allows them).
checkExtraKeys :: [Formal] -> Bool -> Map Text Thunk -> Either Text ()
checkExtraKeys _ True _ = Right ()
checkExtraKeys formals False attrs =
  let expected = map fName formals
      actual = Map.keys attrs
      extra = filter (`notElem` expected) actual
   in case extra of
        [] -> Right ()
        (k : _) -> Left ("unexpected attribute '" <> k <> "' in function argument")

-- | Bind a single formal parameter from the argument attrset.
bindFormal :: Map Text Thunk -> Either Text Env -> Formal -> Either Text Env
bindFormal attrs acc (Formal name defExpr) = do
  env <- acc
  case Map.lookup name attrs of
    Just thunk -> Right (envInsertThunk name thunk env)
    Nothing -> case defExpr of
      Just def -> Right (envInsertThunk name (mkThunk env def) env)
      Nothing -> Left ("missing required attribute '" <> name <> "'")

-- ---------------------------------------------------------------------------
-- Let / if / with / assert
-- ---------------------------------------------------------------------------

-- | Let is recursive in Nix: all bindings are visible to each other.
-- Knot-tying via Haskell laziness (Thunk's Env field is lazy).
evalLet :: Env -> [Binding] -> Expr -> Either Text NixValue
evalLet env bindings body =
  let letEnv = env {envBindings = Map.union letBindings (envBindings env)}
      letBindings = case evalBindings letEnv letEnv bindings of
        Right m -> m
        Left _ -> Map.empty
   in eval letEnv body

evalIf :: Env -> Expr -> Expr -> Expr -> Either Text NixValue
evalIf env cond thenExpr elseExpr = do
  condVal <- eval env cond
  case condVal of
    VBool True -> eval env thenExpr
    VBool False -> eval env elseExpr
    _ -> Left ("'if' condition must be a Boolean, got " <> typeName condVal)

evalWith :: Env -> Expr -> Expr -> Either Text NixValue
evalWith env scope body = do
  scopeVal <- eval env scope
  case scopeVal of
    VAttrs attrs -> eval (pushWithScope attrs env) body
    _ -> Left ("'with' requires a set, got " <> typeName scopeVal)

evalAssert :: Env -> Expr -> Expr -> Either Text NixValue
evalAssert env cond body = do
  condVal <- eval env cond
  case condVal of
    VBool True -> eval env body
    VBool False -> Left "assertion failed"
    _ -> Left ("assertion condition must be a Boolean, got " <> typeName condVal)

-- ---------------------------------------------------------------------------
-- Short-circuit Boolean operators
-- ---------------------------------------------------------------------------

evalShortCircuitAnd :: Env -> Expr -> Expr -> Either Text NixValue
evalShortCircuitAnd env left right = do
  leftVal <- eval env left
  case leftVal of
    VBool False -> Right (VBool False)
    VBool True -> do
      rightVal <- eval env right
      case rightVal of
        VBool _ -> Right rightVal
        _ -> Left ("second operand of && must be a Boolean, got " <> typeName rightVal)
    _ -> Left ("first operand of && must be a Boolean, got " <> typeName leftVal)

evalShortCircuitOr :: Env -> Expr -> Expr -> Either Text NixValue
evalShortCircuitOr env left right = do
  leftVal <- eval env left
  case leftVal of
    VBool True -> Right (VBool True)
    VBool False -> do
      rightVal <- eval env right
      case rightVal of
        VBool _ -> Right rightVal
        _ -> Left ("second operand of || must be a Boolean, got " <> typeName rightVal)
    _ -> Left ("first operand of || must be a Boolean, got " <> typeName leftVal)

evalShortCircuitImpl :: Env -> Expr -> Expr -> Either Text NixValue
evalShortCircuitImpl env left right = do
  leftVal <- eval env left
  case leftVal of
    VBool False -> Right (VBool True)
    VBool True -> do
      rightVal <- eval env right
      case rightVal of
        VBool _ -> Right rightVal
        _ -> Left ("second operand of -> must be a Boolean, got " <> typeName rightVal)
    _ -> Left ("first operand of -> must be a Boolean, got " <> typeName leftVal)

-- ---------------------------------------------------------------------------
-- Builtin dispatch
-- ---------------------------------------------------------------------------

-- | Apply a built-in function to a single argument.
applyBuiltin :: Text -> NixValue -> Either Text NixValue
applyBuiltin name arg = case name of
  -- Type checking
  "typeOf" -> Right (VStr (typeOfValue arg))
  "isNull" -> Right (VBool (isNullVal arg))
  "isInt" -> Right (VBool (isIntVal arg))
  "isFloat" -> Right (VBool (isFloatVal arg))
  "isBool" -> Right (VBool (isBoolVal arg))
  "isString" -> Right (VBool (isStringVal arg))
  "isList" -> Right (VBool (isListVal arg))
  "isAttrs" -> Right (VBool (isAttrsVal arg))
  "isFunction" -> Right (VBool (isFunctionVal arg))
  -- List operations
  "length" -> builtinLength arg
  "head" -> builtinHead arg
  "tail" -> builtinTail arg
  -- String operations
  "toString" -> VStr <$> coerceToString arg
  "stringLength" -> builtinStringLength arg
  -- Control
  "throw" -> builtinThrow arg
  "abort" -> builtinThrow arg
  -- System
  "currentSystem" -> Right (VStr currentSystemStr)
  _ -> Left ("unknown builtin '" <> name <> "'")

-- ---------------------------------------------------------------------------
-- Builtin implementations
-- ---------------------------------------------------------------------------

typeOfValue :: NixValue -> Text
typeOfValue val = case val of
  VInt _ -> "int"
  VFloat _ -> "float"
  VBool _ -> "bool"
  VNull -> "null"
  VStr _ -> "string"
  VPath _ -> "path"
  VList _ -> "list"
  VAttrs _ -> "set"
  VLambda {} -> "lambda"
  VBuiltin _ -> "lambda"
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
isStringVal (VStr _) = True
isStringVal _ = False

isListVal :: NixValue -> Bool
isListVal (VList _) = True
isListVal _ = False

isAttrsVal :: NixValue -> Bool
isAttrsVal (VAttrs _) = True
isAttrsVal _ = False

isFunctionVal :: NixValue -> Bool
isFunctionVal (VLambda {}) = True
isFunctionVal (VBuiltin _) = True
isFunctionVal _ = False

builtinLength :: NixValue -> Either Text NixValue
builtinLength (VList xs) = Right (VInt (fromIntegral (length xs)))
builtinLength other = Left ("builtins.length: expected a list, got " <> typeName other)

builtinHead :: NixValue -> Either Text NixValue
builtinHead (VList []) = Left "builtins.head: empty list"
builtinHead (VList (x : _)) = force x
builtinHead other = Left ("builtins.head: expected a list, got " <> typeName other)

builtinTail :: NixValue -> Either Text NixValue
builtinTail (VList []) = Left "builtins.tail: empty list"
builtinTail (VList (_ : xs)) = Right (VList xs)
builtinTail other = Left ("builtins.tail: expected a list, got " <> typeName other)

builtinStringLength :: NixValue -> Either Text NixValue
builtinStringLength (VStr s) = Right (VInt (fromIntegral (T.length s)))
builtinStringLength other =
  Left ("builtins.stringLength: expected a string, got " <> typeName other)

builtinThrow :: NixValue -> Either Text NixValue
builtinThrow (VStr msg) = Left msg
builtinThrow other = Left ("builtins.throw: expected a string, got " <> typeName other)

-- | The current system platform string.
currentSystemStr :: Text
currentSystemStr = case (System.Info.arch, System.Info.os) of
  ("x86_64", "mingw32") -> "x86_64-windows"
  ("x86_64", "darwin") -> "x86_64-darwin"
  ("aarch64", "darwin") -> "aarch64-darwin"
  ("aarch64", "linux") -> "aarch64-linux"
  ("x86_64", "linux") -> "x86_64-linux"
  (arch, os) -> T.pack arch <> "-" <> T.pack os
