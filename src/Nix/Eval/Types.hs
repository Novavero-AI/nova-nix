-- | Shared types for the Nix evaluator.
--
-- Extracted into its own module so that 'Nix.Eval.Operator',
-- 'Nix.Eval.StringInterp', and 'Nix.Builtins' can all reference
-- 'NixValue', 'Thunk', and 'Env' without creating import cycles.
module Nix.Eval.Types
  ( -- * Values
    NixValue (..),
    Thunk (..),

    -- * Environment
    Env (..),
    emptyEnv,
    envLookup,
    envInsert,
    envInsertThunk,
    pushWithScope,

    -- * Thunk operations
    mkThunk,
    evaluated,

    -- * Display
    typeName,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Derivation (Derivation)
import Nix.Expr.Types (Expr, Formals)

-- | A thunk: either an unevaluated expression paired with its
-- capturing environment, or an already-forced value.
data Thunk
  = -- | Unevaluated: Expr is strict, Env is LAZY to allow
    -- knot-tying in recursive let and rec { }.
    Thunk !Expr Env
  | Evaluated !NixValue
  deriving (Show)

-- | Structural equality — compares expressions for unevaluated thunks,
-- values for evaluated ones.  Will diverge on recursive environments;
-- only used in tests on non-recursive structures.
instance Eq Thunk where
  (Evaluated v1) == (Evaluated v2) = v1 == v2
  (Thunk e1 _) == (Thunk e2 _) = e1 == e2
  _ == _ = False

-- | A Nix value — the result of evaluating an expression.
data NixValue
  = -- | Integer.
    VInt !Integer
  | -- | Floating-point.
    VFloat !Double
  | -- | Boolean.
    VBool !Bool
  | -- | The null value.
    VNull
  | -- | String.
    VStr !Text
  | -- | Path.
    VPath !Text
  | -- | List of thunks (lazy elements).
    VList ![Thunk]
  | -- | Attribute set: name -> thunk (lazy values).
    VAttrs !(Map Text Thunk)
  | -- | Lambda closure: captures environment, formals, body.
    VLambda !Env !Formals !Expr
  | -- | A realized derivation (build recipe).
    VDerivation !Derivation
  | -- | Built-in function, dispatched by name.
    VBuiltin !Text
  deriving (Eq, Show)

-- | Evaluation environment.
--
-- Variable lookup checks 'envBindings' first (lexical scope always wins),
-- then walks 'envWithScopes' innermost-first.  This correctly handles
-- nested @with@ expressions where inner scopes shadow outer ones, and
-- lexical bindings always take priority.
data Env = Env
  { envBindings :: !(Map Text Thunk),
    envWithScopes :: ![Map Text Thunk]
  }
  deriving (Eq, Show)

-- | Empty environment (no variables in scope).
emptyEnv :: Env
emptyEnv = Env Map.empty []

-- | Look up a variable: lexical bindings first, then with-scopes.
envLookup :: Text -> Env -> Maybe Thunk
envLookup name (Env bindings withs) =
  case Map.lookup name bindings of
    Just val -> Just val
    Nothing -> lookupWithScopes name withs

-- | Walk with-scopes innermost to outermost.
lookupWithScopes :: Text -> [Map Text Thunk] -> Maybe Thunk
lookupWithScopes _ [] = Nothing
lookupWithScopes name (scope : rest) =
  case Map.lookup name scope of
    Just val -> Just val
    Nothing -> lookupWithScopes name rest

-- | Insert an already-forced value into the lexical bindings.
envInsert :: Text -> NixValue -> Env -> Env
envInsert name val env =
  env {envBindings = Map.insert name (Evaluated val) (envBindings env)}

-- | Insert a thunk into the lexical bindings.
envInsertThunk :: Text -> Thunk -> Env -> Env
envInsertThunk name thunk env =
  env {envBindings = Map.insert name thunk (envBindings env)}

-- | Push a with-scope onto the scope chain (innermost position).
pushWithScope :: Map Text Thunk -> Env -> Env
pushWithScope scope env =
  env {envWithScopes = scope : envWithScopes env}

-- | Create an unevaluated thunk.
mkThunk :: Env -> Expr -> Thunk
mkThunk env thunkExpr = Thunk thunkExpr env

-- | Wrap an already-computed value as a thunk.
evaluated :: NixValue -> Thunk
evaluated = Evaluated

-- | Human-readable type name for error messages.
typeName :: NixValue -> Text
typeName val = case val of
  VInt _ -> "an integer"
  VFloat _ -> "a float"
  VBool _ -> "a Boolean"
  VNull -> "null"
  VStr _ -> "a string"
  VPath _ -> "a path"
  VList _ -> "a list"
  VAttrs _ -> "a set"
  VLambda {} -> "a function"
  VDerivation _ -> "a derivation"
  VBuiltin _ -> "a built-in function"
