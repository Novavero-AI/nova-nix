{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IO-based Nix evaluator.
--
-- Provides t'EvalIO', a concrete 'MonadEval' instance that performs
-- real file-system access.  The @import@ builtin reads, parses, and
-- evaluates @.nix@ files with a per-session import cache.  Source parsed
-- by 'Nix.Parser.parseNix' evaluates in a session ('withEvalSession'):
--
-- @
-- withEvalSession (SessionSettings unrestrictedPolicy nixPathSearchPath storeDir "/path/to/project") $ \\st ->
--   runEvalIO st (evalParsed (pure (evalBaseEnv st)) parsed)
-- @
module Nix.Eval.IO
  ( -- * Evaluator
    EvalIO,
    runEvalIO,
    runEvalIOTraced,

    -- * Sessions
    SessionSettings (..),
    withEvalSession,
    nixPathSearchPath,

    -- * State
    EvalState
      ( esDrvModuloCache,
        esDrvClosure,
        esSourcePathCache,
        esStoreWriteCache,
        esBaseDir,
        esStoreDir,
        esCallDepth,
        esAllowedPaths
      ),
    evalBaseEnv,
    evalSearchPath,
    newEvalStateWith,
    nixPathValue,
    allowEvalPath,

    -- * Errors
    EvalErrorKind (..),
    EvalFailure (..),
    renderEvalFailure,
    NixEvalError (..),
    NixAbortError (..),
  )
where

import Control.Exception (Exception, IOException, SomeAsyncException, SomeException, bracket, bracket_, catch, catchJust, displayException, fromException, onException, throwIO, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask, asks, local)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.Either (fromRight, isRight)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)
import Foreign.C.Error (Errno (..), eACCES, ePERM)
import Foreign.Ptr (castPtr)
import GHC.Exts (oneShot)
import GHC.IO.Exception (ioe_errno)
import Nix.Builtins (builtinEnv, builtinEnvWithScope, parseNixPath, rootScopeNames)
import Nix.Derivation (fromATerm)
import Nix.Environment (EnvLookup (..), lookupEnvBytes)
import Nix.Eval (deepForce, eval, evalBytecode, evalParsed)
import Nix.Eval.Arena (arenaDestroy, arenaInit)
import Nix.Eval.AttrPath (selectAttrPath)
import Nix.Eval.CData (MonadCData (..), runCData)
import Nix.Eval.CList (CList (..))
import Nix.Eval.CRoots (crootsAddEnv, crootsAddThunk, crootsMark, crootsRelease)
import Nix.Eval.CStatus (arenaLive, cInvariantViolated)
import Nix.Eval.CThunk (CThunkPtr, cthunkMarkBlackhole, cthunkMarkPending, cthunkSetComputedAttrs, cthunkSetComputedBool, cthunkSetComputedCtxStr, cthunkSetComputedFloat, cthunkSetComputedInt, cthunkSetComputedLambda, cthunkSetComputedList, cthunkSetComputedNull, cthunkSetComputedPath, cthunkSetComputedPrimop, cthunkSetComputedPrimopApp, cthunkSetComputedStr, cthunkState)
import Nix.Eval.CallDepth (CallDepth, defaultMaxCallDepth, enterCallFrame, topLevelCallDepth)
import Nix.Eval.CanonPath (canonBaseName, canonPath, canonPathValue)
import Nix.Eval.Policy (AllowedPaths, EvalPolicy (..), allowPathIn, forbiddenPathMessage, isAbsolutePath, isAllowedPath, isAllowedPrefix, joinComponents, noAllowedPaths, pathComponents, pathsRestricted, uriAccess)
import Nix.Eval.SourceError (SourceAccess (..), sourceErrorMessage)
import Nix.Eval.Types (AttrSet (..), Env (..), MonadEval (..), NixValue (..), PathExistence (..), PrimopCell (..), StringCell (..), Thunk (..), attrSetSize, bytesToTextLossy, evaluated, marshalLambda, pathCell, primopCell, readComputed, readPending, storePathOrThrow, stringCell, pattern ThunkBlackhole, pattern ThunkComputed, pattern ThunkPending)
import Nix.Expr.Resolve (undefinedVariableMessage)
import Nix.Expr.Types (Expr)
import Nix.Hash (FileIngestionMethod (..), bytesToHexText, makeFixedOutputPath, makeTextPath, sha256Digest)
import Nix.Parser (ParsedSource, SourceError (..), SourceOrigin (..), parseNixWithScope)
import Nix.Store (UpstreamHazard (..), canonicalizeMetadata, unpackNarEntry, upstreamManagedRefusal)
import Nix.Store.CaseSensitive (probeCaseSensitivity, processCaseHack, volumeCaseHack)
import qualified Nix.Store.ExecBit as ExecBit
import qualified Nix.Store.Path as SP
import qualified NovaCache.NAR as NAR
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isPathSeparator, isRelative, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import qualified System.Process as Proc

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | How an eval-time failure interacts with @builtins.tryEval@:
-- 'ErrorThrown' (@builtins.throw@, a failed @assert@) is catchable,
-- matching upstream's ThrownError\/AssertionError; 'ErrorUncatchable'
-- (type errors, missing attributes, IO failures) escapes tryEval like
-- every other upstream EvalError.
data EvalErrorKind = ErrorThrown | ErrorUncatchable
  deriving (Eq, Show)

-- | A failure's message and the lines of context it gathered unwinding
-- ('addErrorTrace'), outermost first: upstream's @ErrorInfo@ message and
-- traces, without positions.
data EvalFailure = EvalFailure
  { failureTrace :: ![Text],
    failureMessage :: !Text
  }
  deriving (Eq, Show)

-- | A failure with nothing gathered yet.
untracedFailure :: Text -> EvalFailure
untracedFailure = EvalFailure []

-- | Evaluation error surfaced as an IO exception.
data NixEvalError = NixEvalError !EvalErrorKind !EvalFailure
  deriving (Show)

instance Exception NixEvalError

-- | Abort error - NOT catchable by tryEval (matches real Nix semantics).
newtype NixAbortError = NixAbortError EvalFailure
  deriving (Show)

instance Exception NixAbortError

-- | A failure as upstream's @showErrorInfo@ prints it (error.cc at
-- 2.24.9) when no line has a position: each line of context after an
-- ellipsis and a blank line, then the message after an @error:@ of its
-- own.  An empty line of context prints nothing, as upstream passes over
-- a trace whose text is empty (error.cc:386), and the message still
-- follows an @error:@ of its own, as upstream's follows one whenever the
-- failure has a trace (error.cc:376, 416).  The report is upstream's
-- @indent@: the first line follows @error: @, every later line is
-- indented to align with it, and each line loses its trailing whitespace.
renderEvalFailure :: EvalFailure -> Text
renderEvalFailure (EvalFailure trace msg) =
  T.intercalate "\n" (zipWith indentLine (errorPrefix : repeat continuation) (T.splitOn "\n" (chomp report)))
  where
    shown = filter (not . T.null) trace
    report = foldMap traceLine shown <> (if null trace then "" else "\n" <> errorPrefix) <> msg
    traceLine line = "\n\x2026 " <> line <> "\n"
    indentLine prefix line = chomp (prefix <> line)
    continuation = T.replicate (T.length errorPrefix) " "
    chomp = T.dropWhileEnd (`elem` [' ', '\n', '\r', '\t'])

-- | What upstream prints before an error's message.
errorPrefix :: Text
errorPrefix = "error: "

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

-- | Shared state for IO evaluation: an evaluation session.
--
-- The policy, the time and the search path are fixed as the session
-- starts ('withEvalSession'), when its base environment is built over them,
-- as upstream's @EvalState@ takes its settings and lookup path as it is
-- constructed; the module exports no way to change one after.
--
-- 'esImportCache' is a shared mutable cache (global across all frames),
-- holding each file's value in a cell registered as a root
-- ("Nix.Eval.CRoots").  Single-threaded only - switch to @MVar@ or
-- @TVar@ if concurrent evaluation is ever added.
--
-- 'esBaseDir' is immutable per frame - @import@ uses 'local' to set it
-- for nested evaluations, so it is exception-safe with no save\/restore.
data EvalState = EvalState
  { esImportCache :: !(IORef (Map FilePath Thunk)),
    -- | The base environment every file the session evaluates is
    -- evaluated in, @builtins.nixPath@ the search path in it.
    esBaseEnv :: !Env,
    -- | Cache of derivation modulo-hashes (drv store path to hex), populated
    -- bottom-up by @builtinDerivationStrict@ so input derivations can be
    -- substituted by their content hashes when computing output paths.
    esDrvModuloCache :: !(IORef (Map Text Text)),
    -- | Accumulated @.drv@ closure (drv store path text to its ATerm bytes),
    -- recorded bottom-up by @builtinDerivationStrict@.  The build driver reads
    -- this after evaluation to write every input @.drv@ to the store before
    -- building.
    esDrvClosure :: !(IORef (Map Text BS.ByteString)),
    -- | Cache of source path to its store path (recursive NAR hash), so a path
    -- literal used across many derivations is hashed only once.
    esSourcePathCache :: !(IORef (Map Text Text)),
    -- | Every store object evaluation wrote (store path to its
    -- references), for the build driver to register before building -
    -- eval has no store DB handle.  All of them, not only
    -- @builtins.toFile@: an unrecorded write reaches @drvInputSrcs@ as
    -- an unregistered path and fails the build that names it.
    esStoreWriteCache :: !(IORef (Map Text ([SP.StorePath], SP.StoreWriteMode))),
    esBaseDir :: !FilePath,
    -- | Where store objects live on this machine, so eval's own reads and
    -- writes honor @--store@ the same way the builder does.
    esStoreDir :: !SP.StoreDir,
    esTimestamp :: !Int64,
    esSearchPaths :: ![Thunk],
    -- | The call frames active around the current evaluation and the
    -- @max-call-depth@ ceiling.  Per frame like 'esBaseDir':
    -- 'withCallFrame' scopes it with 'local', so a failure unwinding
    -- through a frame leaves the count it found, upstream's RAII guard
    -- with nothing to restore.
    esCallDepth :: !CallDepth,
    -- | What this evaluation may reach: upstream's @restrict-eval@ and
    -- @pure-eval@ settings and the @allowed-uris@ list, and its
    -- @trace-verbose@ setting.  Unrestricted unless the caller says
    -- otherwise, as upstream's defaults are.
    esPolicy :: !EvalPolicy,
    -- | The allowed path prefixes under a restricting policy: the search
    -- path roots the caller seeded with 'allowEvalPath', plus every store
    -- object this evaluation copies, fetches or writes (upstream's
    -- @allowPath@ after each such step), plus the fetchers' scratch
    -- directories.  Mutable because it grows as evaluation proceeds, like
    -- the import cache.
    esAllowedPaths :: !(IORef AllowedPaths)
  }

-- | What an evaluation session starts with: upstream's settings and
-- lookup path, which its @EvalState@ takes as it is constructed.
data SessionSettings = SessionSettings
  { -- | What the evaluation may reach.
    sessionPolicy :: !EvalPolicy,
    -- | The search path, made once the session's arena is live; for
    -- the one @NIX_PATH@ gives, 'nixPathSearchPath'.
    sessionSearchPath :: !(IO [Thunk]),
    -- | Where store objects live.
    sessionStoreDir :: !SP.StoreDir,
    -- | The directory relative paths in the evaluated source resolve
    -- against.
    sessionBaseDir :: !FilePath
  }

-- | Run an action in an evaluation session: the C data layer's arena is
-- set up when no session holds it already, the session's state is made
-- over the settings ('newEvalStateWith'), and the action runs with it.
-- As the session ends, by returning or by an exception, it forgets the
-- roots it registered, and the outermost session tears the arena down,
-- freeing every C object its evaluations made.  A nested session shares
-- the arena of the session around it.  Nothing made in a session is used
-- after it ends: no value, environment or thunk outlives its arena.
withEvalSession :: SessionSettings -> (EvalState -> IO a) -> IO a
withEvalSession settings action = withArena (bracket crootsMark crootsRelease (const session))
  where
    session = do
      searchPaths <- sessionSearchPath settings
      newEvalStateWith (sessionPolicy settings) searchPaths (sessionStoreDir settings) (sessionBaseDir settings) >>= action
    withArena body = do
      live <- arenaLive
      if live then body else bracket_ arenaInit arenaDestroy body

-- | The search path @NIX_PATH@ gives ('nixPathValue').
nixPathSearchPath :: IO [Thunk]
nixPathSearchPath = nixPathValue >>= parseNixPath

-- | The base environment a session evaluates a file in: every name
-- upstream's base environment binds ('Nix.Builtins.builtinEnv').
evalBaseEnv :: EvalState -> Env
evalBaseEnv = esBaseEnv

-- | The search path the session was started with, the one
-- @builtins.nixPath@ holds.
evalSearchPath :: EvalState -> [Thunk]
evalSearchPath = esSearchPaths

-- | The bytes of @NIX_PATH@, unchanged, as upstream's @initGC@ reads them
-- (src/libexpr/eval-gc.cc at Nix 2.24.9), and empty when it is unset.  The
-- one refusal is a Windows @NIX_PATH@ holding an unpaired UTF-16
-- surrogate, which has no UTF-8 form and raises an 'IOException'.
nixPathValue :: IO BS.ByteString
nixPathValue =
  lookupEnvBytes nixPathVar >>= \case
    EnvUnset -> pure BS.empty
    EnvValue value -> pure value
    EnvUnpairedSurrogate ->
      ioError (userError "NIX_PATH holds an unpaired UTF-16 surrogate, which has no UTF-8 form")

-- | The state of a session, made in the session already open: what
-- 'withEvalSession' makes for a new one, and a caller already inside one
-- can make another with.  The call-depth ceiling starts at upstream's
-- default; a caller with a configured one replaces 'esCallDepth'.
--
-- Under a restricting policy, the roots it should admit are seeded with
-- 'allowEvalPath' before evaluation starts, as upstream's constructor
-- allows its lookup path.  The base environment is built here, in the C
-- data layer, so this runs in a session ('withEvalSession'), whose roots
-- the base environment joins, and the search path with it, through
-- @builtins.nixPath@.
newEvalStateWith :: EvalPolicy -> [Thunk] -> SP.StoreDir -> FilePath -> IO EvalState
newEvalStateWith policy searchPaths storeDir baseDir = do
  cache <- newIORef Map.empty
  drvCache <- newIORef Map.empty
  drvClosure <- newIORef Map.empty
  srcCache <- newIORef Map.empty
  storeWriteCache <- newIORef Map.empty
  allowedPaths <- newIORef noAllowedPaths
  now <- floor <$> getPOSIXTime :: IO Int64
  baseEnv@(Env baseEnvPtr) <- builtinEnv policy now searchPaths
  crootsAddEnv baseEnvPtr
  pure
    EvalState
      { esImportCache = cache,
        esBaseEnv = baseEnv,
        esDrvModuloCache = drvCache,
        esDrvClosure = drvClosure,
        esSourcePathCache = srcCache,
        esStoreWriteCache = storeWriteCache,
        esBaseDir = baseDir,
        esStoreDir = storeDir,
        esTimestamp = now,
        esSearchPaths = searchPaths,
        esCallDepth = topLevelCallDepth defaultMaxCallDepth,
        esPolicy = policy,
        esAllowedPaths = allowedPaths
      }

-- | Grant access to a path prefix (upstream @EvalState::allowPath@): the
-- caller seeds the search path roots before evaluation, and the instance
-- adds every store object the evaluation produces.  Like upstream's, this
-- is a no-op when nothing restricts access, so an unrestricted evaluation
-- does not accumulate a set it never consults.  The prefix is allowed on
-- the drive its reads open ('locatedPath'), the form every check uses.
allowEvalPath :: EvalState -> Text -> IO ()
allowEvalPath st path =
  when (pathsRestricted (esPolicy st)) $
    modifyIORef' (esAllowedPaths st) (allowPathIn (locatedPath st path))

-- | Path-value text as the policy checks it: canonical, and on the drive
-- this evaluation's store directory puts it on ('SP.qualifyPathText'),
-- so a check and the read it guards ('evalStoreTextPath') name one file.
locatedPath :: EvalState -> Text -> Text
locatedPath st = SP.qualifyPathText (esStoreDir st) . canonPathValue

-- ---------------------------------------------------------------------------
-- EvalIO newtype
-- ---------------------------------------------------------------------------

-- | IO evaluation monad.  Wraps @ReaderT EvalState IO@.
newtype EvalIO a = MkEvalIO (ReaderT EvalState IO a)

-- Every action is built through this pattern, never through MkEvalIO
-- itself, so that its state argument is marked one-shot: an evaluator
-- action runs once, so GHC may take the state and the IO world as
-- arguments together instead of returning a closure that waits for them,
-- and may move work into the action instead of keeping it outside to share
-- between runs that never happen.  This is the one-shot state monad trick
-- GHC's own IOEnv uses (Note [The one-shot state monad trick] in
-- GHC.Utils.Monad).  A derived instance would bypass the builder, so the
-- instances below are written out.  Under the specialisation below it
-- cuts the time of the benchmarks the cabal file names by 8 to 12% and
-- their allocation by 12 to 13%; on the unspecialised evaluator it
-- changes nothing, since every bind there is an unknown call through the
-- instance dictionary.
pattern EvalIO :: ReaderT EvalState IO a -> EvalIO a
pattern EvalIO {unEvalIO} <- MkEvalIO unEvalIO
  where
    EvalIO (ReaderT run) = MkEvalIO (ReaderT (oneShot run))

{-# COMPLETE EvalIO #-}

instance Functor EvalIO where
  fmap f (EvalIO m) = EvalIO (fmap f m)
  a <$ EvalIO m = EvalIO (a <$ m)

instance Applicative EvalIO where
  pure a = EvalIO (pure a)
  EvalIO f <*> EvalIO m = EvalIO (f <*> m)
  EvalIO m *> EvalIO k = EvalIO (m *> k)

instance Monad EvalIO where
  EvalIO m >>= k = EvalIO (m >>= unEvalIO . k)

  -- Not the default, which goes through '>>=' and loses its one-shot
  -- marking unless '>>=' is INLINE (Note [INLINE pragmas and (>>)] in
  -- GHC.Utils.Monad).
  (>>) = (*>)

-- ---------------------------------------------------------------------------
-- Specialisation
-- ---------------------------------------------------------------------------

-- The evaluator is written over any MonadEval and compiled that way, so
-- each of its binds and class methods is a call through the instance
-- dictionary.  These compile the entry points a caller runs in EvalIO
-- (evalParsed for this module's import and scopedImport and for the
-- executable's evaluation, evalBytecode for the executable's evaluation
-- that keeps its root environment for a census, eval for an expression
-- built by hand, and --strict and attribute selection) for EvalIO alone, and the library's
-- specialisation flags carry that through everything they reach.  Each
-- pragma exports a rule that sends a call at EvalIO in any module
-- importing this one to that code.  The flags alone leave no rule for an
-- entry point small enough to be inlined here rather than specialised, as
-- eval is, so a caller in another module, the executable among them, would
-- run the dictionary-passing evaluator.
{-# SPECIALIZE evalParsed :: EvalIO Env -> ParsedSource -> EvalIO NixValue #-}

{-# SPECIALIZE eval :: Env -> Expr -> EvalIO NixValue #-}

{-# SPECIALIZE evalBytecode :: Env -> Word32 -> EvalIO NixValue #-}

{-# SPECIALIZE deepForce :: NixValue -> EvalIO () #-}

{-# SPECIALIZE selectAttrPath :: Text -> NixValue -> EvalIO (Either Text NixValue) #-}

-- The environment each scopedImport builds over the base one, here too:
-- left to the flags, it ran on the dictionary-passing code.
{-# SPECIALIZE builtinEnvWithScope :: Env -> [(Text, Thunk)] -> EvalIO Env #-}

-- ---------------------------------------------------------------------------
-- MonadEval instance
-- ---------------------------------------------------------------------------

instance MonadCData EvalIO where
  liftCData act = EvalIO (liftIO (runCData act))
  {-# INLINE liftCData #-}

instance MonadEval EvalIO where
  throwEvalError msg = EvalIO (liftIO (throwIO (NixEvalError ErrorUncatchable (untracedFailure msg))))
  throwCatchableError msg = EvalIO (liftIO (throwIO (NixEvalError ErrorThrown (untracedFailure msg))))
  abortEvaluation msg = EvalIO (liftIO (throwIO (NixAbortError (untracedFailure msg))))

  -- tryEval semantics: recover from a throw/assert only; an uncatchable
  -- eval error is rethrown (aborts are a separate exception type and
  -- never enter the 'try').
  catchEvalError (EvalIO action) = EvalIO $ do
    st <- ask
    result <- liftIO (try (runReaderT action st))
    case result of
      Left (NixEvalError ErrorThrown failure) -> pure (Left (failureMessage failure))
      Left err@(NixEvalError ErrorUncatchable _) -> liftIO (throwIO err)
      Right val -> pure (Right val)

  -- Over IO exceptions, so eval errors, aborts, and IO failures
  -- crossing 'wrapIO' all trigger the cleanup before propagating.
  onEvalError (EvalIO action) (EvalIO cleanup) = EvalIO $ do
    st <- ask
    liftIO (runReaderT action st `onException` runReaderT cleanup st)

  -- The line is made after the failure is caught, not in a handler,
  -- which would run it with asynchronous exceptions masked.
  addErrorTrace (EvalIO line) (EvalIO action) = EvalIO $ do
    st <- ask
    result <- liftIO (try (try (runReaderT action st)))
    case result of
      Right (Right val) -> pure val
      Right (Left (NixAbortError failure)) -> retrace (throwIO . NixAbortError) failure
      Left (NixEvalError kind failure) -> retrace (throwIO . NixEvalError kind) failure
    where
      retrace rethrow (EvalFailure trace msg) = do
        !context <- line
        liftIO (rethrow (EvalFailure (context : trace) msg))

  withCallFrame (EvalIO action) = EvalIO $ do
    depth <- asks esCallDepth
    either
      (unEvalIO . throwEvalError)
      (\deeper -> local (\s -> s {esCallDepth = deeper}) action)
      (enterCallFrame depth)

  -- A refused path reads as absent, as upstream's pathExists turns a
  -- RestrictedPathError into false.  The query decides the walk and the
  -- stat as prim_pathExists does: a plain path is resolved through its
  -- ancestors only and lstat'ed in place, so a symlink exists whatever
  -- it points at; a path that must be a directory is resolved fully and
  -- must stat as one.
  doesPathExist query path = do
    allowed <- pathAllowed (existenceResolution query) path
    if allowed
      then evalStoreTextPath path >>= \resolved -> wrapIO (existsAs query resolved)
      else pure False

  listDirectory path = do
    accessPath path
    dir <- evalStoreTextPath path
    sourceIOWith ReadDirectory (resolvedForMessage path dir) $ do
      entries <- Dir.listDirectory dir
      mapM (classifyEntry dir) entries

  importFile rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    baseEnv <- EvalIO (asks esBaseEnv)
    policy <- EvalIO (asks esPolicy)
    -- Resolved, and so checked, before the cache is consulted, as
    -- upstream resolves the file before its fileEvalCache lookup: a
    -- cached import is no more readable than a fresh one.
    (target, ioTarget) <- resolveImportTarget baseDir rawPath
    -- Check import cache (readIORef cannot throw, no wrapIO needed)
    cacheRef <- EvalIO (asks esImportCache)
    cache <- EvalIO (liftIO (readIORef cacheRef))
    case Map.lookup target cache of
      Just (Thunk cell) -> readComputed cell
      Nothing -> do
        source <- sourceIO ReadFile (T.pack target) (BS.readFile ioTarget)
        let fileDir = takeDirectory target
        case parseNixWithScope (rootScopeNames policy) fileDir (SourceFile (T.pack target)) source of
          Left err -> rejectSource "import" target err
          Right parsed -> do
            -- local sets new base dir for nested imports - pure, exception-safe
            let nested =
                  EvalIO
                    ( local
                        (\s -> s {esBaseDir = fileDir})
                        (unEvalIO (evalParsed (pure baseEnv) parsed))
                    )
            result <- nested
            -- Skip caching very large attr sets (e.g. all-packages.nix
            -- with 30k+ entries): the cache keeps what it holds for the
            -- whole session, and nixpkgs imports all-packages.nix once,
            -- at the top level, so skipping it costs nothing.  The value
            -- is kept in a cell registered as a root.
            shouldCache <- case result of
              VAttrs attrs -> (< importCacheMaxAttrs) <$> attrSetSize attrs
              _ -> pure True
            when shouldCache $ do
              cell <- evaluated result
              crootsAddThunk (unThunk cell)
              wrapIO (modifyIORef' cacheRef (Map.insert target cell))
            pure result

  -- Upstream's getEnv answers empty under either setting
  -- (@prim_getEnv@: @restrictEval || pureEval ? "" : getEnv(name)@).
  -- The read cannot throw, so it needs no wrapIO.
  getEnvVar name = do
    policy <- EvalIO (asks esPolicy)
    if pathsRestricted policy
      then pure BS.empty
      else do
        found <- EvalIO (liftIO (lookupEnvBytes name))
        case found of
          EnvUnset -> pure BS.empty
          EnvValue value -> pure value
          EnvUnpairedSurrogate ->
            throwEvalError ("builtins.getEnv: the value of '" <> bytesToTextLossy name <> "' holds an unpaired UTF-16 surrogate, which has no UTF-8 form")

  lookupDrvHash key = EvalIO $ do
    ref <- asks esDrvModuloCache
    liftIO (Map.lookup key <$> readIORef ref)

  cacheDrvHash key val = EvalIO $ do
    ref <- asks esDrvModuloCache
    liftIO (modifyIORef' ref (Map.insert key val))

  recordDrvAterm key aterm = EvalIO $ do
    ref <- asks esDrvClosure
    liftIO (modifyIORef' ref (Map.insert key aterm))

  -- Read a .drv from the store on a modulo-hash cache miss (a cross-session or
  -- appendContext reference).  Mirrors the build side's readDrvFromStore: map
  -- to the on-disk path, read the raw bytes, parse the ATerm byte-level (env
  -- values keep arbitrary bytes).  A file that cannot be read or does not
  -- parse is 'Nothing', which the caller turns into a loud modulo-hash error.
  readStoreDerivation sp = EvalIO $ do
    filePath <- asks ((`storeFilePath` sp) . esStoreDir)
    result <- liftIO (try (BS.readFile filePath) :: IO (Either IOException BS.ByteString))
    pure $ case result of
      Left _ -> Nothing
      Right bytes -> either (const Nothing) Just (fromATerm bytes)

  -- Look up a derivation recorded earlier this session by its .drv path,
  -- reusing the esDrvClosure ATerm map (populated bottom-up by recordDrvAterm).
  -- This recovers an in-session all-outputs reference's output names without a
  -- disk read - the .drv is not written to the store until after evaluation.
  lookupSessionDrv drvPathText = EvalIO $ do
    ref <- asks esDrvClosure
    closure <- liftIO (readIORef ref)
    pure (Map.lookup drvPathText closure >>= either (const Nothing) Just . fromATerm)

  lookupSessionReferences sp = EvalIO $ do
    ref <- asks esStoreWriteCache
    writes <- liftIO (readIORef ref)
    pure (fst <$> Map.lookup (canonicalStorePathText sp) writes)

  storeSourcePath rawPath = do
    ref <- EvalIO (asks esSourcePathCache)
    cached <- EvalIO (liftIO (Map.lookup rawPath <$> readIORef ref))
    case cached of
      Just hit -> pure hit
      Nothing -> do
        -- Upstream names the copy baseNameOf(canonicalized path); path
        -- values arrive canonicalized, so the last segment is the name.
        -- A root, which has none, is refused before its tree is read.
        -- A name the store-path rules refuse is refused only once the tree
        -- is hashed, as upstream's is: the refusal names the path the copy
        -- would have had.
        let name = canonBaseName rawPath
        when (T.null name) $
          throwEvalError ("cannot copy '" <> rawPath <> "' to the store: the path has no base name")
        accessPath rawPath
        -- The tree behind the path's symlinks, under the path's own name:
        -- upstream's copyPathToStore stores path.resolveSymlinks() under
        -- path.baseName() (eval.cc:2393 at 2.24.9), so a path that is a
        -- link copies what it points to, not the link.
        resolvedText <- resolveSymlinks rawPath
        resolvedSource <- evalStoreTextPath resolvedText
        -- A path already in the store never reaches here (it coerces to
        -- itself), so this is a source, read as the build driver's
        -- restore reads it again.
        entry <- serialiseSource resolvedText processCaseHack resolvedSource
        let narDigest = sha256Digest (NAR.serialise entry)
        sp <- storePathOrThrow (makeFixedOutputPath name "sha256" NarIngestion narDigest)
        let spText = canonicalStorePathText sp
        EvalIO (liftIO (modifyIORef' ref (Map.insert rawPath spText)))
        -- The copy is readable afterwards, as upstream's copyPathToStore
        -- allows its destination.
        allowPath spText
        pure spText

  getCurrentTime = EvalIO (asks esTimestamp)

  writeToStore name contents refs = do
    -- Upstream's text-path scheme via makeTextPath - the same scheme .drv
    -- paths use, so it is parity-validated: type @text:<refs>@, flat
    -- sha256 of the contents, canonical store dir.  Construction also
    -- validates the name, so the write below never targets a path
    -- outside the store root.  The contents are the string's RAW BYTES,
    -- hashed and written as-is: no encoding step, and no text-mode IO
    -- (which would CRLF-translate on Windows and store bytes that no
    -- longer match the hash that named the path).
    sp <- storePathOrThrow (makeTextPath name (sha256Digest contents) refs)
    filePath <- evalFilePath sp
    let storePath = canonicalStorePathText sp
    wrapIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory filePath)
      -- Adopt an existing file only when its bytes are these bytes: an
      -- interrupted earlier run can leave a truncated file here, and
      -- skipping on bare existence adopted it under this path's hash.
      -- 'Dir.removePathForcibly' clears read-only marks and accepts a
      -- missing path, so a sealed or squatting leftover rewrites too.
      existing <- readBytesIfPresent filePath
      unless (existing == Just contents) $ do
        Dir.removePathForcibly filePath
        BS.writeFile filePath contents
      -- Adopted or written, the file takes the metadata upstream's
      -- addToStoreFromDump gives what it adds (local-store.cc:1342 at
      -- 2.24.9), which builtins.toFile calls (primops.cc:2317).
      canonicalizeMetadata filePath
    recordStoreWrite storePath refs SP.WriteText
    -- Upstream's toFile returns the path through allowAndSetStorePathString.
    allowPath storePath
    pure storePath

  scopedImportFile scope rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    baseEnv <- EvalIO (asks esBaseEnv)
    policy <- EvalIO (asks esPolicy)
    (target, ioTarget) <- resolveImportTarget baseDir rawPath
    source <- sourceIO ReadFile (T.pack target) (BS.readFile ioTarget)
    let fileDir = takeDirectory target
    case parseNixWithScope (Set.union (Set.fromList (map fst scope)) (rootScopeNames policy)) fileDir (SourceFile (T.pack target)) source of
      Left err -> rejectSource "scopedImport" target err
      Right parsed -> do
        -- No import cache for scoped imports (different scopes = different results)
        EvalIO
          ( local
              (\s -> s {esBaseDir = fileDir})
              (unEvalIO (evalParsed (builtinEnvWithScope baseEnv scope) parsed))
          )

  readFileBytes path = do
    accessPath path
    evalStoreTextPath path >>= \resolved -> sourceIOWith ReadFile (resolvedForMessage path resolved) (BS.readFile resolved)

  -- An lstat of the path itself, no symlink walk: upstream's readFileType
  -- realises its argument without resolution, the allow list checks the
  -- path it is handed (so a refusal names the path asked for), and the
  -- posix accessor beneath refuses a symlink anywhere above it, in every
  -- mode, so the type of a path reached through a symlinked directory is
  -- never revealed.
  getFileType path = do
    accessPathDirect path
    refuseSymlinkedAncestor path
    evalStoreTextPath path >>= \resolved -> sourceIO StatPath path (classifyPath resolved)

  runProcess cmd cmdArgs stdinText = wrapIO $ do
    let cp =
          (Proc.proc (T.unpack cmd) (map T.unpack cmdArgs))
            { Proc.std_in = Proc.CreatePipe,
              Proc.std_out = Proc.CreatePipe,
              Proc.std_err = Proc.CreatePipe
            }
    (exitCode, stdoutStr, stderrStr) <-
      Proc.readCreateProcessWithExitCode cp (T.unpack stdinText)
    let code = case exitCode of
          ExitSuccess -> 0
          ExitFailure n -> n
    pure (code, T.pack stdoutStr, T.pack stderrStr)

  createScratchDir prefix = do
    dir <- wrapIO $ do
      tmpBase <- Dir.getTemporaryDirectory
      suffix <- getRandomBytes scratchSuffixBytes
      -- Forward-slash join: the scratch path feeds sh pipelines (tar -C)
      -- and store copies, both of which accept '/' on every host.
      let dir = tmpBase <> "/" <> T.unpack (prefix <> bytesToHexText suffix)
      -- createDirectory is exclusive: an already-existing path fails the
      -- fetch rather than being silently adopted.
      Dir.createDirectory dir
      pure (T.pack dir)
    -- The evaluator's own scratch area, not ambient filesystem: what a
    -- fetch lands there is read back under the same rule that makes a
    -- fetched store path readable afterwards.  Allowed under its resolved
    -- spelling too, because the walk follows symlinks and a temp dir can
    -- sit behind one (macOS's /var is /private/var), and under the given
    -- spelling so the walk may traverse the link's own ancestors.
    resolved <- wrapIO (Dir.canonicalizePath (T.unpack dir))
    allowPath dir
    allowPath (T.pack resolved)
    pure dir

  removeScratchDir dir = wrapIO (Dir.removePathForcibly (T.unpack dir))

  copyPathToStore srcPath name pin = do
    -- Content-addressed like upstream addToStore: recursive NAR sha256
    -- under the caller's name.  Same content means same path, so the
    -- existence check in 'unpackToStoreVerified' is sound: changed source
    -- content can never serve stale bytes from an earlier copy.
    -- The tree behind the root's symlinks, under the root's own name, as
    -- upstream's copyPathToStore and addPath store path.resolveSymlinks()
    -- under path.baseName(); the walk checks the policy on the way.
    resolvedText <- resolveSymlinks srcPath
    caseHack <- readCaseHack resolvedText
    resolvedSource <- evalStoreTextPath resolvedText
    entry <- serialiseSource resolvedText caseHack resolvedSource
    let narDigest = sha256Digest (NAR.serialise entry)
    case pin of
      Just (expected, mismatch) | expected /= narDigest -> throwEvalError (mismatch narDigest)
      _ -> pure ()
    -- The name is refused only once the tree is hashed, as upstream's is:
    -- the refusal names the path the copy would have had.
    sp <- storePathOrThrow (makeFixedOutputPath name "sha256" NarIngestion narDigest)
    destFilePath <- evalFilePath sp
    let destPath = canonicalStorePathText sp
    -- Restored from the entry just hashed, not copied from the source:
    -- the store's volume may fold a pair the source's keeps apart, and
    -- the tree that lands is exactly the one the address names.
    wrapIO (unpackToStoreVerified destFilePath entry narDigest) >>= either throwEvalError pure
    recordStoreWrite destPath [] SP.WriteRecursive
    allowPath destPath
    pure destPath

  narHashOfPath path = do
    accessPath path
    caseHack <- readCaseHack path
    resolved <- evalStoreTextPath path
    wrapIO (sha256Digest . NAR.serialise <$> ExecBit.serialiseFromPath caseHack resolved)

  isExecutableFile path = do
    accessPath path
    evalStoreTextPath path >>= \resolved -> wrapIO (ExecBit.isExecutable resolved)

  setExecutableFile path = do
    accessPath path
    evalStoreTextPath path >>= \resolved -> wrapIO (ExecBit.markExecutable resolved)

  lookupFetchCache key = wrapIO $ do
    file <- fetchCacheFile key
    there <- Dir.doesFileExist file
    if not there
      then pure Nothing
      else do
        recorded <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
        pure $ case recorded of
          Left _ -> Nothing
          Right bytes -> either (const Nothing) Just (TE.decodeUtf8' bytes)

  adoptStorePath path = do
    resolved <- evalStoreTextPath path
    there <- wrapIO (Dir.doesPathExist resolved)
    -- Same recording the uncached fetch's copyPathToStore would have done.
    -- materializeEvalStoreWrites skips a path that is already valid, so
    -- re-recording one costs nothing and closes the case where the row is
    -- missing.
    when there $ do
      -- What is on disk may carry other modes and times than a fetch
      -- writes, so the remembered tree is made canonical as the uncached
      -- fetch's copyPathToStore makes the tree it writes.
      wrapIO (canonicalizeMetadata resolved)
      recordStoreWrite path [] SP.WriteRecursive
      -- A fetch result, whether fetched now or remembered: upstream
      -- allows the store path every fetcher returns.
      allowPath path
    pure there

  writeFetchCache key value = wrapIO $ do
    file <- fetchCacheFile key
    -- The cache is an optimisation: a machine that cannot write one still
    -- has to be able to build.
    _ <-
      try
        ( do
            Dir.createDirectoryIfMissing True (takeDirectory file)
            -- Written beside the entry and renamed onto it, so a reader
            -- sees either the whole entry or none of it. A plain write can
            -- be cut short and leave a torn file behind, and an entry is
            -- trusted for as long as its store path survives.
            let staging = file ++ ".tmp"
            BS.writeFile staging (TE.encodeUtf8 value)
            Dir.renameFile staging file
        ) ::
        IO (Either IOException ())
    pure ()

  -- The two checks of getFileType: upstream's readLink asserts no
  -- symlink above the path it is handed as well.
  readSymlinkTarget path = do
    accessPathDirect path
    refuseSymlinkedAncestor path
    evalStoreTextPath path >>= \resolved -> wrapIO (T.pack <$> Dir.getSymbolicLinkTarget resolved)

  addSourceNar name narBytes =
    case NAR.deserialise narBytes of
      -- Unreachable in practice: the bytes come from NAR.serialise of a
      -- tree this process just built.  Kept total for the FFI-adjacent
      -- boundary rather than trusting the round trip.
      Left err -> throwEvalError ("builtins.path: internal NAR round-trip error: " <> T.pack err)
      Right entry -> do
        sp <- storePathOrThrow (makeFixedOutputPath name "sha256" NarIngestion (sha256Digest narBytes))
        destFilePath <- evalFilePath sp
        let destPath = canonicalStorePathText sp
        -- Raised here rather than inside 'wrapIO', which would render
        -- the refusal through an exception's own text ("user error (...)").
        wrapIO (unpackToStoreVerified destFilePath entry (sha256Digest narBytes)) >>= either throwEvalError pure
        recordStoreWrite destPath [] SP.WriteRecursive
        allowPath destPath
        pure destPath

  addFixedOutputFile name bytes = do
    -- Canonical fixed-output path: a sha256-pinned fetch must land at the same
    -- store path C++ Nix computes, so it stays reproducible and cache-compatible.
    sp <- storePathOrThrow (makeFixedOutputPath name "sha256" FlatIngestion (sha256Digest bytes))
    filePath <- evalFilePath sp
    let storePath = canonicalStorePathText sp
    wrapIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory filePath)
      -- Same verified adoption as the other writers: an unconditional
      -- write would crash on a sealed leftover (the write hits
      -- read-only).
      existing <- readBytesIfPresent filePath
      unless (existing == Just bytes) $ do
        Dir.removePathForcibly filePath
        BS.writeFile filePath bytes
      -- The metadata upstream gives a downloaded file it adds
      -- (LocalStore::addToStore, local-store.cc:1190 at 2.24.9).
      canonicalizeMetadata filePath
    recordStoreWrite storePath [] SP.WriteFlat
    allowPath storePath
    pure storePath

  traceMessage msg = EvalIO (liftIO (hPutStrLn stderr (T.unpack msg)))

  evalPolicy = EvalIO (asks esPolicy)

  checkUri uri = do
    st <- EvalIO ask
    allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
    either throwEvalError pure (uriAccess (esStoreDir st) (esPolicy st) allowed uri)

  qualifyPath path = EvalIO (asks ((`SP.qualifyPathText` path) . esStoreDir))

  -- Store text stays canonical in the value domain, as it does for an
  -- import target ('resolveImportTarget' says why); every other path
  -- resolves as the platform path it opens, the policy walk having
  -- followed the same links first.
  resolveSymlinks path = do
    accessPath path
    if SP.isCanonicalStoreText path
      then pure (canonPath path)
      else evalStoreTextPath path >>= \native -> wrapIO (canonPathValue . T.pack <$> Dir.canonicalizePath native)

  resolvePathLiteral path = do
    baseDir <- EvalIO (asks esBaseDir)
    policy <- EvalIO (asks esPolicy)
    -- ~/x resolves against the home directory (upstream lexes HPATH and
    -- expands it at eval); everything else relative joins the base dir.
    -- Both end at the producer gate ('canonPathValue'): the value is
    -- absolute, lexically canonical, and slash-spelled regardless of
    -- the base dir's native spelling - platform separators exist only
    -- at the filesystem boundary.  Under pure-eval the home directory is
    -- ambient state, and upstream's parser refuses the literal outright
    -- (parser.y, the HPATH rule); the same refusal lands here, where the
    -- expansion is.
    expanded <- case T.stripPrefix "~/" path of
      Just below
        | epPureEval policy ->
            throwEvalError ("the path '" <> path <> "' can not be resolved in pure mode")
        | otherwise -> do
            home <- wrapIO Dir.getHomeDirectory
            pure (home </> T.unpack below)
      Nothing -> pure (T.unpack path)
    let absolute = if isRelative expanded then baseDir </> expanded else expanded
    pure (canonPathValue (T.pack absolute))

  forceThunk evalFn (Thunk ptr) = do
    -- Force protocol: PENDING to BLACKHOLE to COMPUTED with memoization.
    -- Scalar values (int/float/bool/null) are stored inline in the
    -- thunk payload, dispatched via val_tag.
    --
    -- Blackhole detection: PENDING thunks are marked BLACKHOLE before
    -- evaluation begins.  If evaluation re-enters the same thunk, it
    -- sees BLACKHOLE and reports infinite recursion.  This is safe with
    -- knot-tying (evalBcRecAttrs, evalBcLet, matchFormalSet) because those
    -- patterns create distinct thunks sharing an env - no thunk ever
    -- forces itself.
    state <- cthunkState ptr
    case state of
      ThunkComputed -> readComputed ptr
      ThunkBlackhole ->
        -- Infinite recursion is non-catchable (like abort), matching C++ Nix.
        -- tryEval must NOT catch blackholes - using abortEvaluation ensures
        -- the error propagates through tryEval/catchEvalError.
        abortEvaluation "infinite recursion encountered"
      ThunkPending -> do
        pending <- readPending ptr
        -- Mark blackhole BEFORE evaluation - any re-entry hits the
        -- BLACKHOLE branch above.
        marked <- cthunkMarkBlackhole ptr
        unless marked $
          EvalIO (liftIO (cInvariantViolated "nn_thunk_mark_blackhole" "a thunk read as PENDING was not PENDING"))
        -- If the force throws (a builtins.throw caught by an upstream tryEval,
        -- a type error, a failed import), restore the thunk to PENDING and
        -- rethrow, so a later force of this shared thunk re-evaluates instead of
        -- taking the BLACKHOLE branch above and aborting with a bogus "infinite
        -- recursion".  Mirrors C++ Nix forceValue: catch (...) { restore; throw }.
        -- A genuine self-recursion still rethrows its NixAbortError, which
        -- escapes tryEval exactly as before.  Storing the value is inside the
        -- restore too, since copying or marshalling it can fail.  The
        -- restore's own answer is not needed: it refuses only a thunk that is
        -- no longer BLACKHOLE, which a later force already handles, and the
        -- exception in flight is the one to report.
        EvalIO $ do
          st <- ask
          liftIO
            ( ( do
                  result <- runReaderT (unEvalIO (evalFn pending)) st
                  storeComputed ptr result
                  pure result
              )
                `onException` cthunkMarkPending ptr
            )
      other ->
        EvalIO (liftIO (cInvariantViolated "nn_thunk_state" ("no thunk has state " ++ show other)))

-- | Store a forced thunk's NixValue in its C cell.
-- Scalars (int, float, bool, null) and a builtin with no argument taken
-- are stored inline; strings and paths (over the C object their bytes are
-- held in, or a copy), lists, sets, lambdas and partial applications as
-- their C objects.  The cell is BLACKHOLE while its force runs, which
-- keeps every other path from computing it, so a setter's refusal is an
-- invariant violation rather than a value to drop.
storeComputed :: CThunkPtr -> NixValue -> IO ()
storeComputed ptr val = do
  stored <- case val of
    VInt n -> cthunkSetComputedInt ptr n
    VFloat d -> cthunkSetComputedFloat ptr d
    VBool b -> cthunkSetComputedBool ptr (if b then 1 else 0)
    VNull -> cthunkSetComputedNull ptr
    VAttrs (AttrSet cset) -> cthunkSetComputedAttrs ptr (castPtr cset)
    VPathHeld p held -> pathCell p held >>= cthunkSetComputedPath ptr
    VStrHeld t ctx held ->
      stringCell t ctx held >>= \case
        StrCell str -> cthunkSetComputedStr ptr str
        CtxStrCell csptr -> cthunkSetComputedCtxStr ptr (castPtr csptr)
    VList (CList clistPtr) -> cthunkSetComputedList ptr (castPtr clistPtr)
    VLambda (Env envPtr) formals bodyBcIdx -> do
      lamPtr <- marshalLambda envPtr formals bodyBcIdx
      cthunkSetComputedLambda ptr lamPtr
    VBuiltin builtin args ->
      primopCell builtin args >>= \case
        PrimopCell primop -> cthunkSetComputedPrimop ptr primop
        PrimopAppCell app -> cthunkSetComputedPrimopApp ptr (castPtr app)
  unless stored (cInvariantViolated "nn_thunk_set_computed_*" "a thunk being forced was already COMPUTED")

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Maximum attribute set size for import caching.  Results larger than this
-- are not cached, allowing GC to reclaim them.  Prevents the import cache
-- from retaining huge attr sets like nixpkgs' 30k-entry all-packages.nix.
importCacheMaxAttrs :: Int
importCacheMaxAttrs = 1000

-- | The variable upstream reads its lookup path from.
nixPathVar :: BS.ByteString
nixPathVar = "NIX_PATH"

-- | Random bytes in a scratch-dir name suffix (hex-encoded).  128 bits:
-- unguessable by another local process, collision-free in practice.
scratchSuffixBytes :: Int
scratchSuffixBytes = 16

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Access policy
-- ---------------------------------------------------------------------------

-- | Admit a store object this evaluation produced (upstream's @allowPath@
-- after a copy, fetch or write).
allowPath :: Text -> EvalIO ()
allowPath path = do
  st <- EvalIO ask
  EvalIO (liftIO (allowEvalPath st path))

-- | Refuse a read unless the whole path, symlinks resolved, lies in the
-- allowed prefixes.  Upstream's accessor checks every prefix as its
-- symlink walk pushes it (@SourceAccessor::resolveSymlinks@ through
-- @FilteringSourceAccessor::maybeLstat@, which throws at 2.24.9), so the
-- refusal names the first prefix outside the allow list - @/etc@ for
-- @/etc/passwd@ when nothing under @/etc@ is allowed - and a symlink's
-- target is checked where the walk follows it.  A no-op when nothing
-- restricts access.
accessPath :: Text -> EvalIO ()
accessPath path = do
  st <- EvalIO ask
  when (pathsRestricted (esPolicy st)) $ do
    allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
    walkAllowed ResolveFull st allowed path >>= either throwEvalError pure

-- | Refuse a read unless the path itself is allowed, no symlink walk: the
-- check upstream's accessor makes on an lstat or readlink of the path it
-- is handed.  The refusal names the path asked for.
accessPathDirect :: Text -> EvalIO ()
accessPathDirect path = do
  st <- EvalIO ask
  when (pathsRestricted (esPolicy st)) $ do
    allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
    let canonical = locatedPath st path
    unless (isAbsolutePath canonical) $ throwEvalError (relativePathMessage path)
    unless (isAllowedPath allowed canonical) $
      throwEvalError (forbiddenPathMessage (esPolicy st) canonical)

-- | Whether the walk would admit the path, for the one reader that
-- answers a Boolean instead of refusing.
pathAllowed :: SymlinkResolution -> Text -> EvalIO Bool
pathAllowed mode path = do
  st <- EvalIO ask
  if pathsRestricted (esPolicy st)
    then do
      allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
      isRight <$> walkAllowed mode st allowed path
    else pure True

-- | Refuse a path with a symlink anywhere above it, naming the deepest
-- one: upstream's posix accessor asserts this on an lstat or readlink of
-- the path it is handed (@PosixSourceAccessor::assertNoSymlinks@ over
-- the parent, at 2.24.9), in every mode, and names the first symlink it
-- meets walking up from the parent.
refuseSymlinkedAncestor :: Text -> EvalIO ()
refuseSymlinkedAncestor path = do
  st <- EvalIO ask
  mapM_ (check st) (strictAncestors (canonPathValue path))
  where
    check st ancestor = do
      target <- EvalIO (liftIO (symlinkTargetAt st ancestor))
      when (isJust target) $
        throwEvalError ("path '" <> ancestor <> "' is a symlink")

-- | A canonical path's ancestors below the root, deepest first.
strictAncestors :: Text -> [Text]
strictAncestors path =
  [joinComponents (take n components) | n <- [length components - 1, length components - 2 .. 1]]
  where
    components = pathComponents path

-- | How a 'PathExistence' query walks the path: a directory is demanded
-- through the final symlink, an entry is found in place.
existenceResolution :: PathExistence -> SymlinkResolution
existenceResolution ExistsAsDirectory = ResolveFull
existenceResolution ExistsAsEntry = ResolveAncestors

-- | The stat a 'PathExistence' query ends in: a directory through every
-- symlink, or an entry of any kind in place.
existsAs :: PathExistence -> FilePath -> IO Bool
existsAs ExistsAsDirectory = Dir.doesDirectoryExist
existsAs ExistsAsEntry = lstatExists

-- | Whether an entry is at the path, a dangling symlink included: an
-- lstat, as upstream's @maybeLstat@ is.  'Dir.pathIsSymbolicLink' is the
-- lstat the directory package exposes, and it throws for a missing path,
-- so any failure is absence, as it is for 'Dir.doesPathExist'.
lstatExists :: FilePath -> IO Bool
lstatExists fp = do
  outcome <- try (Dir.pathIsSymbolicLink fp) :: IO (Either IOException Bool)
  pure (isRight outcome)

-- | Upstream's @coerceToPath@ refusal of a relative string.  The builtins
-- refuse one before it gets here; the gate repeats the refusal because
-- its soundness depends on it.
relativePathMessage :: Text -> Text
relativePathMessage path = "string '" <> path <> "' doesn't represent an absolute path"

-- | Which symlinks a walk follows, upstream's @SymlinkResolution@: every
-- one, or every one but the last component's, which is then checked in
-- place.
data SymlinkResolution = ResolveFull | ResolveAncestors
  deriving (Eq, Show)

-- | The symlink-resolving prefix walk of upstream's
-- @SourceAccessor::resolveSymlinks@ at 2.24.9, with the allow list
-- consulted where its @maybeLstat@ would be: each component is pushed,
-- the prefix so far must be allowed, and a symlink at that prefix has
-- its target spliced in front of what remains (an absolute target
-- restarts at the root).  'Left' carries the refusal for the first
-- prefix outside the allow list.  Under 'ResolveAncestors' the last
-- component's own symlink is left alone, as upstream consults
-- @maybeLstat@ only while something remains to resolve.  The path is
-- walked on the drive its read opens ('locatedPath'), the form the allow
-- list holds.
walkAllowed :: SymlinkResolution -> EvalState -> AllowedPaths -> Text -> EvalIO (Either Text ())
walkAllowed mode st allowed path
  | not (isAbsolutePath canonical) = pure (Left (relativePathMessage path))
  | otherwise = go symlinkFollowLimit [] (pathComponents canonical)
  where
    canonical = locatedPath st path
    policy = esPolicy st
    go _ _ [] = pure (Right ())
    go linksLeft done (component : rest)
      | component == "." = go linksLeft done rest
      | component == ".." = go linksLeft (drop 1 done) rest
      | not (isAllowedPrefix allowed prefix) = pure (Left (forbiddenPathMessage policy prefixText))
      | otherwise = do
          target <- if followsLinkAt rest then EvalIO (liftIO (symlinkTargetAt st prefixText)) else pure Nothing
          case target of
            Nothing -> go linksLeft prefixReversed rest
            Just _
              | linksLeft == 0 ->
                  throwEvalError ("infinite symlink recursion in path '" <> canonical <> "'")
            Just link ->
              let linkComponents = filter (not . T.null) (T.split isPathSeparator link)
                  -- Spliced in front of the remainder; the walk resumes from
                  -- the root for an absolute target and from the link's own
                  -- directory otherwise.
                  resumeFrom = if isAbsoluteTarget link then [] else done
               in go (linksLeft - 1) resumeFrom (linkComponents ++ rest)
      where
        prefixReversed = component : done
        prefix = reverse prefixReversed
        prefixText = joinComponents prefix
    followsLinkAt rest = mode == ResolveFull || not (null rest)
    isAbsoluteTarget link = T.isPrefixOf "/" link || isAbsolute (T.unpack link)

-- | The target of the symlink at a path value, read through the store
-- mapping, or 'Nothing' when there is no symlink there.  A path that
-- does not exist is simply not a symlink; the read that follows reports
-- it missing.
symlinkTargetAt :: EvalState -> Text -> IO (Maybe Text)
symlinkTargetAt st pathText = do
  let fsPath = SP.storeTextToFilePath (esStoreDir st) pathText
  outcome <-
    try
      ( do
          isLink <- Dir.pathIsSymbolicLink fsPath
          if isLink then Just . T.pack <$> Dir.getSymbolicLinkTarget fsPath else pure Nothing
      ) ::
      IO (Either IOException (Maybe Text))
  pure (fromRight Nothing outcome)

-- | Upstream's bound on symlinks followed while resolving one path.
symlinkFollowLimit :: Int
symlinkFollowLimit = 1024

-- | Where a recorded fetch is kept: one file per key, under this user's
-- cache directory, named by the key's own hash. Outside the store on
-- purpose - it's a note about work already done, not a derivation input.
fetchCacheFile :: Text -> IO FilePath
fetchCacheFile key = do
  dir <- Dir.getXdgDirectory Dir.XdgCache "nova-nix/fetch"
  pure (dir </> T.unpack (bytesToHexText (sha256Digest (TE.encodeUtf8 key))))

-- | Where a store path lives on this machine: this evaluation's store dir
-- mapped to a filesystem path.  This is the 'SP.StorePath' direction;
-- path-value text goes through 'evalStoreTextPath'.  Both must read the
-- same store dir, since a store honored on one side and not the other
-- writes to one directory and reads from another.
storeFilePath :: SP.StoreDir -> SP.StorePath -> FilePath
storeFilePath = SP.storePathToFilePath

-- | Record a store object evaluation just wrote, so the build driver
-- registers it before a derivation naming it is built.  Every eval-time
-- writer goes through this: a write that skips it lands in
-- @drvInputSrcs@ with no registration row and fails the build with
-- \"path \'...\' is not valid\".  The mode names the scheme that
-- constructed the path, so materialization can verify the on-disk
-- content reproduces it before registering.
recordStoreWrite :: Text -> [SP.StorePath] -> SP.StoreWriteMode -> EvalIO ()
recordStoreWrite storePath refs mode = do
  cacheRef <- EvalIO (asks esStoreWriteCache)
  EvalIO (liftIO (modifyIORef' cacheRef (Map.insert storePath (refs, mode))))

-- | The file's bytes, 'Nothing' when nothing readable is there: absent,
-- or something unreadable squatting on the name; either way the writer
-- must replace rather than adopt.
readBytesIfPresent :: FilePath -> IO (Maybe BS.ByteString)
readBytesIfPresent path = do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure (either (const Nothing) Just result)

-- | The NAR serialisation of what is on disk, 'Nothing' when nothing
-- serialisable is there.  Through 'ExecBit.serialiseIfReadable', the
-- same walk every producer and verifier uses, so the comparison sees
-- the executable flags the store's model records, not the platform's
-- permission guesses.
narBytesIfPresent :: NAR.CaseHack -> FilePath -> IO (Maybe BS.ByteString)
narBytesIfPresent caseHack path = do
  onDisk <- Dir.doesPathExist path
  if not onDisk
    then pure Nothing
    else fmap NAR.serialise <$> ExecBit.serialiseIfReadable caseHack path

-- | The case-hack mode a read of path-value text runs under.  Store
-- text names a tree on the store's volume, read back as the store
-- reads its own paths ('volumeCaseHack' of the probed volume); any
-- other path is a source, read under the process's setting
-- ('processCaseHack').
readCaseHack :: Text -> EvalIO NAR.CaseHack
readCaseHack path
  | SP.isCanonicalStoreText path = do
      storeRoot <- EvalIO (asks (SP.unStoreDir . esStoreDir))
      wrapIO (volumeCaseHack <$> probeCaseSensitivity storeRoot)
  | otherwise = pure processCaseHack

-- | Where an eval-time store write of a path lands: 'storeFilePath'
-- against the store this evaluation was given.  Every eval-time writer
-- asks here before it writes, so a store upstream Nix's database may
-- also manage is refused at the first write, uncatchably
-- ("Nix.Store.Upstream"): a writer clears a tree there that does not
-- reproduce its address, which may be one upstream registered.  An
-- evaluation that writes nothing never asks.
evalFilePath :: SP.StorePath -> EvalIO FilePath
evalFilePath sp = do
  storeDir <- EvalIO (asks esStoreDir)
  refusal <- wrapIO (upstreamManagedRefusal ReplacesUpstreamPaths storeDir)
  maybe (pure (storeFilePath storeDir sp)) throwEvalError refusal

-- | Resolve path-value text to a filesystem location against the store
-- this evaluation was given.  The read counterpart of 'evalFilePath':
-- both sides of every eval-time store access must agree on the store
-- dir, or a redirected store is written to and read from two places.
evalStoreTextPath :: Text -> EvalIO FilePath
evalStoreTextPath txt = EvalIO (asks ((`SP.storeTextToFilePath` txt) . esStoreDir))

-- | A store path's identity: the canonical @/nix/store@ spelling every
-- platform shares.  Hashes and eval-visible strings carry this form; it
-- never names a location on disk.
canonicalStorePathText :: SP.StorePath -> Text
canonicalStorePathText = SP.storePathToText SP.defaultStoreDir

-- | Fail an import whose file does not parse or bind.  Both are uncatchable,
-- as upstream's ParseError and UndefinedVarError are to @builtins.tryEval@.
-- An unbound variable reads exactly as upstream's message does; a syntax
-- error keeps the parser's detail behind the builtin and file it came from.
rejectSource :: Text -> FilePath -> SourceError -> EvalIO a
rejectSource builtinName target err = throwEvalError $ case err of
  SyntaxError syntax -> builtinName <> " " <> T.pack target <> ": " <> T.pack (show syntax)
  UndefinedVariable name -> undefinedVariableMessage name

-- | Resolve an import's raw path to the value-domain target (the import
-- cache key, the parse name, and the base dir the file's relative path
-- literals resolve against) and the filesystem location to read, with
-- both the path named and the file resolved checked against the policy:
-- upstream's @resolveExprPath@ walks the path it is handed, and
-- @parseExprFromFile@ walks the file it reads after @default.nix@ has
-- been appended, so a directory whose @default.nix@ is a symlink is
-- refused at the link's target.
--
-- Store text stays canonical in the value domain: 'Dir.canonicalizePath'
-- would attach the working drive to the rooted @/nix@ prefix on Windows,
-- so store text gets lexical canonicalization ('canonPath') only, and
-- its reads resolve through 'evalStoreTextPath'.  Every other path
-- resolves and canonicalizes as the platform path 'evalStoreTextPath'
-- gives it, where the two returned forms coincide.
resolveImportTarget :: FilePath -> Text -> EvalIO (FilePath, FilePath)
resolveImportTarget baseDir rawPath = do
  accessPath rawPath
  (target, ioTarget) <- locate
  accessPath (T.pack target)
  pure (target, ioTarget)
  where
    locate
      | SP.isCanonicalStoreText rawPath = do
          let valueBase = T.unpack (canonPath rawPath)
          resolvedBase <- evalStoreTextPath (T.pack valueBase)
          isDir <- sourceIO StatPath (T.pack valueBase) (statSource resolvedBase >> Dir.doesDirectoryExist resolvedBase)
          -- The value-domain join stays "/" so the canonical spelling survives.
          let valueTarget = if isDir then valueBase <> "/default.nix" else valueBase
          resolvedTarget <- evalStoreTextPath (T.pack valueTarget)
          pure (valueTarget, resolvedTarget)
      | otherwise = do
          raw <- evalStoreTextPath rawPath
          let resolved = if isRelative raw then baseDir </> raw else raw
          canonical <- wrapIO (Dir.canonicalizePath resolved)
          -- Directory import: append /default.nix if target is a
          -- directory, which upstream's resolveExprPath learns from an
          -- lstat, so an absent target is a stat failure.
          isDir <- sourceIO StatPath (T.pack canonical) (statSource canonical >> Dir.doesDirectoryExist canonical)
          let target = if isDir then canonical </> "default.nix" else canonical
          pure (target, target)

-- | Classify a filesystem path as @"regular"@, @"directory"@, @"symlink"@,
-- or @"unknown"@ - matching Nix's @builtins.readDir@ / @readFileType@.
classifyPath :: FilePath -> IO Text
classifyPath fp =
  firstMatch
    "unknown"
    [ (Dir.pathIsSymbolicLink fp, "symlink"),
      (Dir.doesDirectoryExist fp, "directory"),
      (Dir.doesFileExist fp, "regular")
    ]

-- | Return the label of the first predicate that holds, or the default.
firstMatch :: Text -> [(IO Bool, Text)] -> IO Text
firstMatch def [] = pure def
firstMatch def ((test, label) : rest) =
  test >>= \case
    True -> pure label
    False -> firstMatch def rest

-- | Classify a directory entry (name relative to parent) as upstream's
-- @readDirectory@ types one (posix-source-accessor.cc:136-163 at
-- 2.24.9): an entry whose lstat is refused with @EACCES@ or @EPERM@
-- (@permission_denied@, @operation_not_permitted@) has no type, and any
-- other failure fails the listing.  A directory that can be read but not
-- searched refuses every entry's lstat this way.
classifyEntry :: FilePath -> FilePath -> IO (Text, Maybe Text)
classifyEntry parentDir name = do
  ty <- catchJust refusedStat (Just <$> classifyPath (parentDir </> name)) (\() -> pure Nothing)
  pure (T.pack name, ty)
  where
    refusedStat err
      | fmap Errno (ioe_errno err) `elem` [Just eACCES, Just ePERM] = Just ()
      | otherwise = Nothing

-- | Convert IO exceptions to eval errors.
-- Guards against double-wrapping: if the exception is already a
-- t'NixEvalError', it is re-thrown as-is.
wrapIO :: IO a -> EvalIO a
wrapIO action = EvalIO $ liftIO $ do
  result <- try action
  case result of
    Right val -> pure val
    Left (err :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      | Just abortErr <- fromException err -> throwIO (abortErr :: NixAbortError)
      | Just nixErr <- fromException err -> throwIO (nixErr :: NixEvalError)
      | otherwise -> throwIO (NixEvalError ErrorUncatchable (untracedFailure (T.pack (displayException err))))

-- | 'wrapIO' for an access to a source path: an 'IOException' becomes
-- the message upstream gives that access ('sourceErrorMessage'), naming
-- the path as given.
sourceIO :: SourceAccess -> Text -> IO a -> EvalIO a
sourceIO access path = sourceIOWith access (pure path)

-- | 'sourceIO' with the path to name computed once the access has failed,
-- so a successful read pays nothing for it.
sourceIOWith :: SourceAccess -> IO Text -> IO a -> EvalIO a
sourceIOWith access shownPath action =
  wrapIO $
    action `catch` \(err :: IOException) -> do
      shown <- shownPath
      throwIO (NixEvalError ErrorUncatchable (untracedFailure (sourceErrorMessage access shown err)))

-- | A path with its symlinks resolved, as upstream names one it reached
-- through realisePath (readFile, hashFile and readDir resolve fully),
-- given the value and the native path it opens.  A path the resolution
-- itself cannot walk keeps the spelling given.
resolvedForMessage :: Text -> FilePath -> IO Text
resolvedForMessage path native
  | SP.isCanonicalStoreText path = pure (canonPath path)
  | otherwise =
      (canonPathValue . T.pack <$> Dir.canonicalizePath native)
        `catch` \(_ :: IOException) -> pure path

-- | An lstat that raises for an absent path: 'Dir.pathIsSymbolicLink' is
-- the portable lstat, as 'lstatExists' says.
statSource :: FilePath -> IO ()
statSource = void . Dir.pathIsSymbolicLink

-- | A source path's NAR, its root lstat'ed first as upstream's dumpPath
-- begins.  A regular file's only access in the walk is its read, so a
-- failure there is the read's.  A failure below a directory keeps GHC's
-- text until nova-cache's walk names the access that failed
-- (Novavero-AI/nova-cache#80).
serialiseSource :: Text -> NAR.CaseHack -> FilePath -> EvalIO NAR.NarEntry
serialiseSource path caseHack resolved = do
  isLink <- sourceIO StatPath path (Dir.pathIsSymbolicLink resolved)
  isFile <- wrapIO (Dir.doesFileExist resolved)
  let walk = ExecBit.serialiseFromPath caseHack resolved
  if not isLink && isFile then sourceIO ReadFile path walk else wrapIO walk

-- | Run an IO evaluation, returning @Left@ with the failure's message on
-- error.
runEvalIO :: EvalState -> EvalIO a -> IO (Either Text a)
runEvalIO st action = either (Left . failureMessage) Right <$> runEvalIOTraced st action

-- | Run an IO evaluation, returning @Left@ with the whole failure on
-- error, its lines of context included, for a caller that reports it
-- ('renderEvalFailure').
--
-- Catches t'NixEvalError' (throw) and t'NixAbortError' (abort).
-- Async exceptions (@StackOverflow@, @ThreadKilled@, etc.) propagate uncaught.
runEvalIOTraced :: EvalState -> EvalIO a -> IO (Either EvalFailure a)
runEvalIOTraced st (EvalIO action) = do
  result <- try (runReaderT action st)
  case result of
    Right val -> pure (Right val)
    Left (err :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      | Just (NixEvalError _ failure) <- fromException err -> pure (Left failure)
      | Just (NixAbortError failure) <- fromException err -> pure (Left failure)
      | otherwise -> pure (Left (untracedFailure (T.pack (displayException err))))

-- ---------------------------------------------------------------------------
-- Store copy helpers
-- ---------------------------------------------------------------------------

-- | Unpack a NAR entry at its content-addressed destination and make the
-- tree's metadata canonical ('canonicalizeMetadata'), as upstream's
-- @addToStoreFromDump@ does to what it restores (local-store.cc:1342 at
-- 2.24.9).  An existing destination is adopted only when its NAR digest
-- is the expected one - same content means same path, so a matching tree
-- is byte-identical by construction; anything else (an interrupted
-- earlier unpack, a squatter) is cleared and unpacked afresh.  The
-- entry is the one evaluation hashed to name the path, so the tree
-- that lands is the one the address names, with the store volume's
-- own sibling-name handling.
unpackToStoreVerified :: FilePath -> NAR.NarEntry -> BS.ByteString -> IO (Either Text ())
unpackToStoreVerified dest entry expectedDigest = do
  Dir.createDirectoryIfMissing True (takeDirectory dest)
  -- Probed per write rather than carried in 'EvalState': the store
  -- directory need not exist when evaluation starts, and the probe
  -- answers for a path on disk.  It was created just above, and the
  -- cost is one pathconf call per tree.
  sensitivity <- probeCaseSensitivity (takeDirectory dest)
  onDiskNar <- narBytesIfPresent (volumeCaseHack sensitivity) dest
  placed <-
    if (sha256Digest <$> onDiskNar) == Just expectedDigest
      then pure (Right ())
      else do
        Dir.removePathForcibly dest
        unpackNarEntry sensitivity dest entry
  traverse (\() -> canonicalizeMetadata dest) placed
