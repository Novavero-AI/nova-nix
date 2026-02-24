{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Shared types for the Nix evaluator.
--
-- Extracted into its own module so that 'Nix.Eval.Operator',
-- 'Nix.Eval.StringInterp', and 'Nix.Builtins' can all reference
-- 'NixValue', 'Thunk', and 'Env' without creating import cycles.
module Nix.Eval.Types
  ( -- * Values
    NixValue (..),
    Thunk (..),

    -- * String context
    StringContextElement (..),
    StringContext (..),
    emptyContext,
    mkStr,

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

    -- * Evaluation monad
    MonadEval (..),
    PureEval (..),
  )
where

import Data.IORef (IORef, newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import Nix.Derivation (Derivation)
import Nix.Expr.Types (Expr, Formals)
import Nix.Store.Path (StorePath)
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- String context
-- ---------------------------------------------------------------------------

-- | A single element of string context, tracking where a string
-- references store paths.
data StringContextElement
  = -- | Plain store path reference (inputSrcs).
    SCPlain !StorePath
  | -- | Derivation output reference (inputDrvs): .drv path + output name.
    SCDrvOutput !StorePath !Text
  | -- | All outputs of a derivation (for drvPath itself).
    SCAllOutputs !StorePath
  deriving (Eq, Ord, Show)

-- | Context carried by Nix strings, tracking store path dependencies.
newtype StringContext = StringContext {unStringContext :: Set StringContextElement}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Empty string context (alias for 'mempty').
emptyContext :: StringContext
emptyContext = mempty

-- | Smart constructor for context-free strings.
mkStr :: Text -> NixValue
mkStr t = VStr t emptyContext

-- ---------------------------------------------------------------------------
-- Thunks
-- ---------------------------------------------------------------------------

-- | A thunk: either an unevaluated expression paired with its
-- capturing environment, or an already-forced value.
--
-- Each unevaluated thunk carries an 'IORef' memoization cell that
-- caches the result after the first force — matching real Nix, which
-- mutates thunks in-place.  The IORef is allocated lazily via
-- 'unsafePerformIO' in 'mkThunk' so that knot-tying in recursive
-- @let@ and @rec {}@ works unchanged (the IORef doesn't participate
-- in the knot).  Memory is naturally bounded: when a thunk is no
-- longer reachable, both the thunk and its cached value are GC'd.
data Thunk
  = -- | Unevaluated: Expr is strict, Env is LAZY to allow
    -- knot-tying in recursive let and rec { }.  The IORef cell
    -- is written once on first force, then read on subsequent forces.
    Thunk !Expr Env !(IORef (Maybe NixValue))
  | Evaluated !NixValue

instance Show Thunk where
  show (Evaluated val) = "Evaluated (" ++ show val ++ ")"
  show (Thunk expr _ _) = "Thunk (" ++ show expr ++ ") <env> <ref>"

-- | Structural equality — compares expressions for unevaluated thunks,
-- values for evaluated ones.  Ignores the IORef cell and environment.
-- Will diverge on recursive environments; only used in tests on
-- non-recursive structures.
instance Eq Thunk where
  (Evaluated v1) == (Evaluated v2) = v1 == v2
  (Thunk e1 _ _) == (Thunk e2 _ _) = e1 == e2
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
  | -- | String with dependency context.
    VStr !Text !StringContext
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
    -- Accumulated args support curried partial application.
    VBuiltin !Text ![NixValue]
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

-- | Create an unevaluated thunk with a fresh memoization cell.
--
-- Each thunk gets its own 'IORef' for in-place memoization, matching
-- real Nix which mutates @Value@ structs on first force.  The cell is
-- allocated via 'newMemoCell' which uses 'unsafePerformIO' — safe
-- because 'newIORef' is a pure allocation with no observable side
-- effects, and the 'NOINLINE' + @seq@ pattern prevents GHC from
-- floating the allocation to a shared top-level CAF.
mkThunk :: Env -> Expr -> Thunk
mkThunk env thunkExpr =
  Thunk thunkExpr env (newMemoCell thunkExpr)

-- | Allocate a fresh memoization cell for a thunk.
--
-- @NOINLINE@ prevents inlining so GHC can't see inside or CSE calls.
-- @seq@ on the argument creates a data dependency that prevents
-- float-out to a top-level CAF — without this, GHC would hoist
-- @unsafePerformIO (newIORef Nothing)@ and share ONE cell across
-- ALL thunks.
{-# NOINLINE newMemoCell #-}
newMemoCell :: Expr -> IORef (Maybe NixValue)
newMemoCell expr = unsafePerformIO (expr `seq` newIORef Nothing)

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
  VStr _ _ -> "a string"
  VPath _ -> "a path"
  VList _ -> "a list"
  VAttrs _ -> "a set"
  VLambda {} -> "a function"
  VDerivation _ -> "a derivation"
  VBuiltin _ _ -> "a built-in function"

-- ---------------------------------------------------------------------------
-- Evaluation monad
-- ---------------------------------------------------------------------------

-- | Effect class for Nix evaluation.  Core logic is polymorphic in @m@
-- so the same evaluator composes into 'PureEval' for tests or @IO@ for
-- real file-system access (e.g. @import@, @readFile@).
class (Monad m) => MonadEval m where
  throwEvalError :: Text -> m a
  catchEvalError :: m a -> m (Either Text a)
  readFileText :: Text -> m Text
  doesPathExist :: Text -> m Bool

  -- | List a directory, returning @(name, fileType)@ pairs.
  -- @fileType@ is one of @"regular"@, @"directory"@, or @"symlink"@
  -- (matching Nix's @builtins.readDir@ semantics).
  listDirectory :: Text -> m [(Text, Text)]

  importFile :: Text -> m NixValue

  -- | Look up an environment variable.  Returns @""@ if unset.
  getEnvVar :: Text -> m Text

  -- | Get the current epoch time (seconds since 1970-01-01).
  getCurrentTime :: m Integer

  -- | Write a named file to the store, returning the store path.
  writeToStore :: Text -> Text -> m Text

  -- | Import a file with a custom scope overlaid on builtins.
  scopedImportFile :: [(Text, Thunk)] -> Text -> m NixValue

  -- | Run an external process: @(command, args, stdin) -> (exitCode, stdout, stderr)@.
  runProcess :: Text -> [Text] -> Text -> m (Int, Text, Text)

  -- | Resolve a path literal to an absolute path.
  -- In IO evaluation, relative paths are resolved against the current
  -- file's directory ('esBaseDir').  In pure evaluation, paths are
  -- returned unchanged.  This ensures that path values captured in
  -- closures remain valid after the import scope ends.
  resolvePathLiteral :: Text -> m Text

  -- | Force a thunk to a value, with memoization.
  -- IO evaluators should cache results (e.g. via 'StableName').
  -- Pure evaluators re-evaluate each time.
  -- The first argument is the evaluation function (to break the
  -- Eval.Types → Eval circular dependency).
  forceThunk :: (Env -> Expr -> m NixValue) -> Thunk -> m NixValue

-- | Pure evaluation monad — wraps 'Either Text'.
-- IO builtins ('readFile', 'import') are unavailable;
-- everything else evaluates identically to the IO version.
newtype PureEval a = PureEval {runPureEval :: Either Text a}
  deriving (Functor, Applicative, Monad)

instance MonadEval PureEval where
  throwEvalError msg = PureEval (Left msg)
  catchEvalError (PureEval action) = PureEval (Right action)
  readFileText _ = throwEvalError "readFile: not available in pure evaluation"
  doesPathExist _ = pure False
  listDirectory _ = throwEvalError "builtins.readDir: not available in pure evaluation"
  importFile _ = throwEvalError "import: not available in pure evaluation"
  getEnvVar _ = pure ""
  getCurrentTime = pure 0
  writeToStore _ _ = throwEvalError "toFile: not available in pure evaluation"
  scopedImportFile _ _ = throwEvalError "scopedImport: not available in pure evaluation"
  runProcess _ _ _ = throwEvalError "runProcess: not available in pure evaluation"
  resolvePathLiteral = pure
  forceThunk _ (Evaluated val) = pure val
  forceThunk evalFn (Thunk expr env _) = evalFn env expr
