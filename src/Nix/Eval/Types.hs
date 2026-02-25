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
    ThunkCell (..),

    -- * Attribute sets (lazy/eager unified abstraction)
    AttrSet (..),
    LazyBinding (..),
    attrSetLookup,
    attrSetKeys,
    attrSetToMap,
    attrSetFromMap,
    attrSetMember,
    attrSetNull,
    attrSetSize,
    attrSetElems,
    attrSetToAscList,
    attrSetMapWithKey,
    attrSetUnionWith,
    newLazyAttrCache,

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
    mkSyntheticThunk,
    evaluated,
    thunkSameRef,

    -- * Display
    typeName,

    -- * Evaluation monad
    MonadEval (..),
    PureEval (..),
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import Nix.Derivation (Derivation)
import Nix.Expr.Types (AttrKey (..), Expr (..), Formals, StringPart (..))
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

-- | Contents of a thunk's memoization cell.  When forced, 'Pending'
-- is overwritten with 'Computed', dropping the Expr and Env — they
-- become unreachable and are GC'd.  This matches C++ Nix, which
-- overwrites Value structs in place to release closures.
data ThunkCell
  = -- | Not yet forced: expression + capturing environment.
    -- Expr is strict; Env is LAZY for knot-tying in rec {} and let.
    Pending !Expr Env
  | -- | Forced: the computed result.  Expr + Env are gone.
    Computed !NixValue
  deriving (Show)

-- | A thunk: either an IORef-backed memoization cell (deferred
-- evaluation), or an already-known value (no IORef needed).
--
-- Each 'ThunkRef' carries an 'IORef ThunkCell'.  On first force,
-- the cell is overwritten from @Pending expr env@ to @Computed val@,
-- which drops all references to the Expr and Env — matching real Nix
-- which mutates thunks in-place.  The IORef is allocated via
-- 'unsafePerformIO' in 'mkThunk' so that knot-tying works unchanged.
data Thunk
  = -- | Deferred thunk with memoization cell.
    ThunkRef !(IORef ThunkCell)
  | -- | Already-known value (no IORef overhead).
    Evaluated !NixValue

instance Show Thunk where
  show (Evaluated val) = "Evaluated (" ++ show val ++ ")"
  show (ThunkRef _) = "ThunkRef <ref>"

-- | Equality: compares values for 'Evaluated', IORef identity for
-- 'ThunkRef'.  Only used in tests on non-recursive structures.
instance Eq Thunk where
  (Evaluated v1) == (Evaluated v2) = v1 == v2
  (ThunkRef ref1) == (ThunkRef ref2) = ref1 == ref2
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
  | -- | Attribute set: unified lazy/eager representation.
    VAttrs !AttrSet
  | -- | Lambda closure: captures environment, formals, body.
    VLambda !Env !Formals !Expr
  | -- | A realized derivation (build recipe).
    VDerivation !Derivation
  | -- | Built-in function, dispatched by name.
    -- Accumulated args support curried partial application.
    VBuiltin !Text ![NixValue]
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Attribute sets (lazy/eager unified abstraction)
-- ---------------------------------------------------------------------------

-- | Per-key lazy binding recipe.  Used by 'LazyAttrs' to defer
-- thunk construction until first access.  Only ~50 of nixpkgs' 30k
-- packages are touched for a typical eval — storing recipes instead
-- of pre-built thunks avoids ~30k IORef allocations.
data LazyBinding
  = -- | Simple @key = expr@ — deferred @mkThunk recEnv expr@.
    LazyExpr !Expr
  | -- | @inherit key@ — deferred @inheritLookup recEnv key@.
    LazyInherit
  | -- | @inherit (from) key@ — deferred @mkThunk env (ESelect from [StaticKey key] Nothing)@.
    LazyInheritFrom !Expr
  | -- | Pre-built thunk (for merged nested paths or other complex cases).
    PreBuilt !Thunk
  deriving (Eq, Show)

-- | Attribute set representation: either an eagerly-materialized map
-- of thunks, or a lazy binding map that defers thunk construction.
--
-- The 'LazyAttrs' constructor is used for @rec {}@ and @let@ bindings
-- in large attribute sets (e.g. nixpkgs' 30k-entry package set).
-- Thunks + IORefs are only allocated for keys that are actually accessed.
data AttrSet
  = -- | Eagerly-materialized attribute set (small sets, non-rec, builtins).
    EagerAttrs !(Map Text Thunk)
  | -- | Lazy attribute set: thunks built on demand from binding recipes.
    -- The 'Env' field is INTENTIONALLY LAZY for knot-tying in rec {}.
    LazyAttrs
      -- | Knot-tied recEnv (lazy for knot-tying)
      Env
      -- | Key → binding recipe (O(log n) lookup)
      !(Map Text LazyBinding)
      -- | Materialized thunk cache (written via unsafePerformIO)
      !(IORef (Map Text Thunk))

instance Eq AttrSet where
  EagerAttrs a == EagerAttrs b = a == b
  a == b = attrSetToMap a == attrSetToMap b

instance Show AttrSet where
  show (EagerAttrs m) = show m
  show (LazyAttrs _ bindings _) =
    "<lazy " ++ show (Map.size bindings) ++ " bindings>"

-- | Look up a single key.  For 'EagerAttrs', pure 'Map.lookup'.
-- For 'LazyAttrs', checks the cache first, then materializes a
-- single thunk from the binding recipe on cache miss.
--
-- Uses 'unsafePerformIO' for cache writes — safe because this is
-- idempotent memoization with no observable side effects beyond
-- caching (same rationale as thunk memoization via 'newMemoCell').
attrSetLookup :: Text -> AttrSet -> Maybe Thunk
attrSetLookup key (EagerAttrs m) = Map.lookup key m
attrSetLookup key (LazyAttrs env bindings cacheRef) =
  -- Check cache first, then binding map
  let cached = unsafePerformIO (readIORef cacheRef)
   in case Map.lookup key cached of
        Just thunk -> Just thunk
        Nothing -> case Map.lookup key bindings of
          Nothing -> Nothing
          Just recipe ->
            let thunk = materializeBinding env key recipe
             in -- Cache the materialized thunk
                unsafePerformIO $ do
                  atomicModifyIORef' cacheRef (\c -> (Map.insert key thunk c, ()))
                  pure (Just thunk)

-- | Materialize a single 'LazyBinding' into a 'Thunk'.
materializeBinding :: Env -> Text -> LazyBinding -> Thunk
materializeBinding env _key (LazyExpr expr) = mkThunk env expr
materializeBinding env key LazyInherit = inheritLookupThunk env key
materializeBinding env key (LazyInheritFrom fromExpr) =
  mkThunk env (ESelect fromExpr [StaticKey key] Nothing)
materializeBinding _env _key (PreBuilt thunk) = thunk

-- | Look up a name for lazy @inherit@.  If not found, create a thunk
-- that will error when forced (matching real Nix behaviour).
inheritLookupThunk :: Env -> Text -> Thunk
inheritLookupThunk env name =
  case envLookup name env of
    Just thunk -> thunk
    Nothing ->
      let errMsg = "undefined variable '" <> name <> "'"
          throwExpr = EApp (EVar "throw") (EStr [StrLit errMsg])
       in mkThunk env throwExpr

-- | All keys in the attribute set.  For 'LazyAttrs', reads keys from
-- the binding map — zero thunk allocation.
attrSetKeys :: AttrSet -> [Text]
attrSetKeys (EagerAttrs m) = Map.keys m
attrSetKeys (LazyAttrs _ bindings _) = Map.keys bindings

-- | Check key membership without materializing thunks.
attrSetMember :: Text -> AttrSet -> Bool
attrSetMember key (EagerAttrs m) = Map.member key m
attrSetMember key (LazyAttrs _ bindings _) = Map.member key bindings

-- | Check if the attribute set is empty.
attrSetNull :: AttrSet -> Bool
attrSetNull (EagerAttrs m) = Map.null m
attrSetNull (LazyAttrs _ bindings _) = Map.null bindings

-- | Number of attributes.
attrSetSize :: AttrSet -> Int
attrSetSize (EagerAttrs m) = Map.size m
attrSetSize (LazyAttrs _ bindings _) = Map.size bindings

-- | Full materialization: build a 'Map Text Thunk' from all bindings.
-- For 'LazyAttrs', this allocates thunks + IORefs for every key —
-- expensive on large sets, avoid on the hot path.
--
-- Uses 'unsafePerformIO' to update the cache atomically.
attrSetToMap :: AttrSet -> Map Text Thunk
attrSetToMap (EagerAttrs m) = m
attrSetToMap (LazyAttrs env bindings cacheRef) =
  unsafePerformIO $ do
    cached <- readIORef cacheRef
    if Map.size cached == Map.size bindings
      then pure cached
      else do
        let full = Map.mapWithKey (materializeOne env cached) bindings
        atomicModifyIORef' cacheRef (const (full, ()))
        pure full
  where
    materializeOne lazyEnv cache key recipe =
      case Map.lookup key cache of
        Just thunk -> thunk
        Nothing -> materializeBinding lazyEnv key recipe

-- | All thunk values (materialized).
attrSetElems :: AttrSet -> [Thunk]
attrSetElems = Map.elems . attrSetToMap

-- | Sorted key-value pairs (materialized).
attrSetToAscList :: AttrSet -> [(Text, Thunk)]
attrSetToAscList = Map.toAscList . attrSetToMap

-- | Map a function over all key-value pairs (materializes lazy sets).
attrSetMapWithKey :: (Text -> Thunk -> Thunk) -> AttrSet -> AttrSet
attrSetMapWithKey f attrs = EagerAttrs (Map.mapWithKey f (attrSetToMap attrs))

-- | Union two attribute sets with a combining function (materializes both).
attrSetUnionWith :: (Thunk -> Thunk -> Thunk) -> AttrSet -> AttrSet -> AttrSet
attrSetUnionWith f a b = EagerAttrs (Map.unionWith f (attrSetToMap a) (attrSetToMap b))

-- | Wrap an eager map as an 'AttrSet'.
attrSetFromMap :: Map Text Thunk -> AttrSet
attrSetFromMap = EagerAttrs

-- | Allocate a fresh materialization cache for 'LazyAttrs'.
-- Uses 'unsafePerformIO' with @NOINLINE@ + @seq@ to ensure each
-- call site gets its own IORef (same pattern as 'newMemoCell').
{-# NOINLINE newLazyAttrCache #-}
newLazyAttrCache :: Map Text LazyBinding -> IORef (Map Text Thunk)
newLazyAttrCache bindings = unsafePerformIO (bindings `seq` newIORef Map.empty)

-- | Evaluation environment.
--
-- Variable lookup checks 'envBindings' first (lexical scope always wins),
-- then walks 'envWithScopes' innermost-first.  This correctly handles
-- nested @with@ expressions where inner scopes shadow outer ones, and
-- lexical bindings always take priority.
data Env = Env
  { envBindings :: !(Map Text Thunk),
    envWithScopes :: ![AttrSet]
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
-- Uses 'attrSetLookup' so that 'LazyAttrs' with-scopes only
-- materialize the accessed key, not the entire set.
lookupWithScopes :: Text -> [AttrSet] -> Maybe Thunk
lookupWithScopes _ [] = Nothing
lookupWithScopes name (scope : rest) =
  case attrSetLookup name scope of
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
-- Accepts 'AttrSet' directly so 'LazyAttrs' with-scopes stay lazy.
pushWithScope :: AttrSet -> Env -> Env
pushWithScope scope env =
  env {envWithScopes = scope : envWithScopes env}

-- | Create an unevaluated thunk with a fresh memoization cell.
--
-- Each thunk gets its own 'IORef ThunkCell'.  On first force the
-- cell is overwritten from @Pending@ to @Computed@, dropping the
-- Expr and Env.  The cell is allocated via @newMemoCell@ which uses
-- 'unsafePerformIO' — safe because 'newIORef' is a pure allocation,
-- and the @NOINLINE@ + @seq@ pattern prevents GHC from floating the
-- allocation to a shared top-level CAF.
mkThunk :: Env -> Expr -> Thunk
mkThunk env thunkExpr =
  ThunkRef (newMemoCell thunkExpr env)

-- | Like 'mkThunk' but for synthetic thunks that reuse the same 'Expr'
-- (e.g. @EApp (EVar "__fn") (EVar "__arg")@ in 'deferApply').  Uses the
-- 'Env' bindings map for cell uniqueness instead of the expression, since
-- GHC's full-laziness transform would otherwise float the shared expr to
-- a CAF and all thunks would get the same IORef.
--
-- Must only be called with freshly-constructed envs (not knot-tied
-- recursive envs), since it forces the bindings map to WHNF.
mkSyntheticThunk :: Env -> Expr -> Thunk
mkSyntheticThunk env thunkExpr =
  ThunkRef (newSyntheticCell (envBindings env) thunkExpr env)

-- | Allocate a fresh memoization cell for a thunk.
--
-- @NOINLINE@ prevents inlining so GHC can't see inside or CSE calls.
-- @seq@ on the expr creates a data dependency that prevents float-out
-- to a top-level CAF — without this, GHC would hoist the
-- @unsafePerformIO (newIORef ...)@ and share ONE cell across ALL thunks.
--
-- The cell starts as @Pending expr env@, putting both references inside
-- the IORef.  On force, it's overwritten to @Computed val@, dropping
-- the Expr and Env so they become unreachable.
{-# NOINLINE newMemoCell #-}
newMemoCell :: Expr -> Env -> IORef ThunkCell
newMemoCell expr env = unsafePerformIO (expr `seq` newIORef (Pending expr env))

-- | Like 'newMemoCell' but keyed on a bindings map instead of an expression.
-- Used by 'mkSyntheticThunk' where multiple thunks share the same expression.
{-# NOINLINE newSyntheticCell #-}
newSyntheticCell :: Map Text Thunk -> Expr -> Env -> IORef ThunkCell
newSyntheticCell bindings expr env = unsafePerformIO (bindings `seq` newIORef (Pending expr env))

-- | Wrap an already-computed value as a thunk.
evaluated :: NixValue -> Thunk
evaluated = Evaluated

-- | Check if two thunks share the same memoization cell (IORef pointer
-- equality).  When true, both thunks will always produce the same value,
-- so deep equality can short-circuit to 'True' without forcing.
-- Matching real Nix which uses pointer identity for same-thunk comparison.
thunkSameRef :: Thunk -> Thunk -> Bool
thunkSameRef (ThunkRef ref1) (ThunkRef ref2) = ref1 == ref2
thunkSameRef _ _ = False

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
  -- file's directory (@esBaseDir@).  In pure evaluation, paths are
  -- returned unchanged.  This ensures that path values captured in
  -- closures remain valid after the import scope ends.
  resolvePathLiteral :: Text -> m Text

  -- | Force a thunk to a value, with memoization.
  -- IO evaluators should cache results (via per-thunk @IORef@).
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
  forceThunk evalFn (ThunkRef ref) =
    -- Read the cell via unsafePerformIO — safe because readIORef has no
    -- side effects and PureEval never writes back (no memoization).
    case unsafePerformIO (readIORef ref) of
      Computed val -> pure val
      Pending expr env -> evalFn env expr
