-- | Nix expression evaluator.
--
-- == How evaluation works
--
-- Nix evaluation is LAZY.  When you write:
--
-- @
-- let
--   pkgs = import \<nixpkgs\> {};
--   hello = pkgs.hello;
--   gcc = pkgs.gcc;
-- in hello
-- @
--
-- The evaluator does NOT evaluate @gcc@.  It's never referenced by the
-- result, so its thunk is never forced.  This is critical because
-- nixpkgs has 80,000+ packages — evaluating all of them would take
-- forever.
--
-- Evaluation proceeds in 3 phases:
--
-- 1. __Parse__: source text → AST ('Nix.Expr.Types.Expr')
-- 2. __Evaluate__: AST → value ('NixValue').  Lazy — thunks everywhere.
-- 3. __Realize__: when a value is a derivation, extract the '.drv' recipe.
--
-- The evaluator maintains an ENVIRONMENT (scope) that maps variable
-- names to values.  @let@, @with@, function application, and @import@
-- all extend the environment.  Attribute set members are thunks that
-- close over their defining environment (enabling @rec { }@).
--
-- == builtins
--
-- Nix has ~100 built-in functions: @builtins.map@, @builtins.filter@,
-- @builtins.hashString@, @builtins.fetchurl@, etc.  These are implemented
-- in 'Nix.Builtins' and injected into the top-level scope.  The most
-- important one is @builtins.derivation@ — it takes an attribute set and
-- produces a derivation value (which, when forced, writes a .drv file).
module Nix.Eval
  ( -- * Values
    NixValue (..),
    Thunk,

    -- * Evaluation
    eval,

    -- * Environment
    Env (..),
    emptyEnv,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Derivation (Derivation)
import Nix.Expr.Types (Expr)

-- | A thunk: an unevaluated expression paired with its environment.
-- Forced on demand (lazy evaluation).
data Thunk = Thunk
  { thExpr :: !Expr,
    thEnv :: !Env
  }
  deriving (Eq, Show)

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
  | -- | String (with context tracking for store path references).
    VStr !Text
  | -- | Path.
    VPath !Text
  | -- | List of thunks (lazy elements).
    VList ![Thunk]
  | -- | Attribute set: name → thunk (lazy values).
    VAttrs !(Map Text Thunk)
  | -- | Lambda closure: captures environment.
    VLambda !Env !Expr
  | -- | A realized derivation (the build recipe).
    VDerivation !Derivation
  deriving (Eq, Show)

-- | Evaluation environment: variable name → value.
newtype Env = Env {unEnv :: Map Text NixValue}
  deriving (Eq, Show)

-- | Empty environment (no variables in scope).
emptyEnv :: Env
emptyEnv = Env Map.empty

-- | Evaluate a Nix expression in an environment.
--
-- This is the core evaluation loop.  Pattern matching on each 'Expr'
-- constructor, extending the environment as needed, forcing thunks
-- when values are demanded.
eval :: Env -> Expr -> Either Text NixValue
eval _env _expr = Left "Evaluator not yet implemented"
