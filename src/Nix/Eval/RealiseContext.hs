{-# LANGUAGE LambdaCase #-}

-- | Realising a string's context before a path reader reads its path, as
-- upstream's @EvalState::realiseContext@ does (primops.cc): what a context
-- asks of the store, and what a session does about it.
--
-- Every path the context names must be valid, checked in upstream's
-- order, and the first that is not stops the read with upstream's
-- @path \'...\' is not valid@.  Then, unless @allow-import-from-derivation@
-- refuses a context that names an output, each output it names is built or
-- substituted (@EvalState::realiseContext@ in primops.cc).
--
-- A read-write session, a build's, adds what evaluation has fetched,
-- written or named and the store does not hold yet before that, so all
-- of it counts as valid there.  A read-only session, an evaluation as
-- @nix-instantiate --eval@ runs one, adds only what a fetcher fetched, as
-- it is fetched.  What evaluation computed (a coerced source, a
-- @builtins.toFile@, @builtins.path@ or @builtins.filterSource@ result, a
-- derivation) is not valid there unless the store already holds it, since
-- upstream's read-only evaluation computes those paths without writing
-- them.  What a fetcher fetched is valid there, since upstream's fetchers
-- add their results whatever the mode.  A read-only session realises an
-- output as
-- upstream's read-only evaluation does: every substitution is refused, and
-- an output that may not be substituted is built.
module Nix.Eval.RealiseContext
  ( -- * Sessions
    SessionMode (..),
    PendingOrigin (..),

    -- * What a context asks
    ContextRequest (..),

    -- * Planning
    readableIn,
    firstInvalidPath,
    Plan (..),
    planRealisation,
    referenceClosure,

    -- * Refusals
    ContextRefusal (..),
    contextRefusalMessage,
    findFileRefusalMessage,
  )
where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.Eval.Policy (ImportFromDerivation (..))
import Nix.Store.Path (StorePath, defaultStoreDir, storePathBasename, storePathToText)

-- ---------------------------------------------------------------------------
-- Sessions
-- ---------------------------------------------------------------------------

-- | What an evaluation session may do to its store.
data SessionMode
  = -- | Add only what a fetcher fetched, as it is fetched, and realise an
    -- output as upstream's read-only evaluation does, as
    -- @nix-instantiate --eval@ does without @--read-write-mode@.
    ReadOnly
  | -- | Add what evaluation made and build what its contexts name, as a
    -- build does.
    ReadWrite
  deriving (Eq, Show)

-- | How an object evaluation made and the store does not hold yet came
-- to be, which decides whether a read-only session can read it.
data PendingOrigin
  = -- | A fetcher fetched it, which upstream adds to its store in any
    -- mode.
    Fetched
  | -- | Evaluation computed it, which upstream's read-only evaluation
    -- names without writing.
    Computed
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- What a context asks
-- ---------------------------------------------------------------------------

-- | What a string's context asks of the store, in the order upstream walks
-- its context set (@NixStringContext@, value/context.hh at 2.24.9): plain
-- paths, then derivations named whole, then outputs, each by path.
data ContextRequest = ContextRequest
  { -- | Each path that must be valid: a plain path, a derivation named
    -- whole, and the derivation of each output named, as upstream's
    -- @ensureValid@ asks about each element in turn (primops.cc).
    crPaths :: ![StorePath],
    -- | Each output named, by its derivation and its name.
    crOutputs :: ![(StorePath, Text)]
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Planning
-- ---------------------------------------------------------------------------

-- | What a session does for a context it can realise.
data Plan = Plan
  { -- | Whether to add what evaluation has made and the store does not
    -- hold yet first: a read-write session adds it before it builds, and
    -- before a read of something evaluation made.
    planAdd :: !Bool,
    -- | The derivations to realise, each for the outputs named of it that
    -- are not valid, or that it may not have, which the realisation
    -- refuses as upstream's worker does.
    planBuilds :: ![(StorePath, Set Text)],
    -- | Every output the context names, whose closures become readable
    -- once they are realised, as upstream allows them (@allowClosure@ in
    -- @EvalState::realiseContext@, primops.cc, at 2.34.9).
    planOutputs :: ![StorePath],
    -- | The warning @trace-import-from-derivation@ prints before anything
    -- is realised, where it is on and an output is named.
    planWarning :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | Plan the realisation of a context, given the session's mode, what
-- evaluation has made that the store may not hold, the paths of those
-- asked about that the store holds, and each output's path.  A session
-- asks the store about every path the request names that it cannot
-- answer itself: a read-write session about those evaluation did not
-- make, a read-only one about all but what a fetcher fetched, and about
-- the outputs.
--
-- A path that is not valid is refused first, in the request's order
-- ('PathNotValid').  A request that names an output is then refused when
-- import from derivation is off ('ImportFromDerivationOff'), whether or
-- not the output is valid, and warned of when it is traced, each naming
-- the first output in upstream's order.  Each derivation is then realised
-- for its outputs that are not valid, and for any output name whose path is not known,
-- which the realisation refuses as upstream's worker refuses an output a
-- derivation does not have.  A read-write session adds what evaluation
-- made first when it realises anything or the request names any of it,
-- so the read finds it.
planRealisation ::
  ImportFromDerivation ->
  SessionMode ->
  Map StorePath PendingOrigin ->
  Set StorePath ->
  Map (StorePath, Text) StorePath ->
  ContextRequest ->
  Either ContextRefusal Plan
planRealisation imports mode pending valid outputPaths request =
  case firstInvalidPath mode pending valid request of
    Just path -> Left (PathNotValid path)
    Nothing -> case (imports, crOutputs request) of
      (_, []) -> Right (planned Nothing)
      (RefuseImportFromDerivation, (drv, output) : _) -> Left (ImportFromDerivationOff drv output)
      (AllowImportFromDerivation, _) -> Right (planned Nothing)
      (TraceImportFromDerivation, (drv, output) : _) -> Right (planned (Just (importFromDerivationWarning drv output)))
  where
    planned = Plan (mode == ReadWrite && (namesPending || not (null builds))) builds known
    readable = readableIn mode pending valid
    namesPending = any (`Map.member` pending) (crPaths request)
    resolved = [(named, Map.lookup named outputPaths) | named <- crOutputs request]
    known = mapMaybe snd resolved
    builds =
      Map.toList
        ( Map.fromListWith
            Set.union
            [(drv, Set.singleton output) | ((drv, output), path) <- resolved, maybe True (not . readable) path]
        )

-- | The first path of the request that is not valid, in the request's
-- order, given the session's mode, what evaluation has made and the
-- store's answer for the paths asked about: what upstream's
-- @ensureValid@ refuses first (@EvalState::realiseContext@ in primops.cc).
firstInvalidPath :: SessionMode -> Map StorePath PendingOrigin -> Set StorePath -> ContextRequest -> Maybe StorePath
firstInvalidPath mode pending valid = find (not . readableIn mode pending valid) . crPaths

-- | Whether a session can read a path: the store holds it, a fetcher
-- fetched it, or evaluation computed it and a read-write session adds it
-- before the read.
readableIn :: SessionMode -> Map StorePath PendingOrigin -> Set StorePath -> StorePath -> Bool
readableIn mode pending valid path =
  path `Set.member` valid || case Map.lookup path pending of
    Just Fetched -> True
    Just Computed -> mode == ReadWrite
    Nothing -> False

-- | Each path with every path it references, through the store's answer
-- for one path's references, asking about each once.
referenceClosure :: (Monad m) => (StorePath -> m (Either Text (Set StorePath))) -> [StorePath] -> m (Either Text (Set StorePath))
referenceClosure references = go Set.empty
  where
    go seen [] = pure (Right seen)
    go seen (path : rest)
      | path `Set.member` seen = go seen rest
      | otherwise =
          references path >>= \case
            Left failure -> pure (Left failure)
            Right referenced -> go (Set.insert path seen) (Set.toList referenced <> rest)

-- ---------------------------------------------------------------------------
-- Refusals
-- ---------------------------------------------------------------------------

-- | Why a context could not be realised.
data ContextRefusal
  = -- | This path is not valid, and the session cannot make it so.
    PathNotValid !StorePath
  | -- | This output of this derivation is the first the context names,
    -- and @allow-import-from-derivation@ is off.
    ImportFromDerivationOff !StorePath !Text
  | -- | Adding or realising failed, with the message the failure reports.
    RealisationFailed !Text
  deriving (Eq, Show)

-- | A refusal as upstream words it.  A path is named under the logical
-- store directory, as upstream's @printStorePath@ names it
-- (@InvalidPathError@ in eval-error.cc, 2.35.4 and later; 2.34.9 names the
-- path's base name alone).
contextRefusalMessage :: ContextRefusal -> Text
contextRefusalMessage = \case
  PathNotValid path -> "path '" <> storePathToText defaultStoreDir path <> "' is not valid"
  ImportFromDerivationOff drv output -> "cannot build '" <> builtText drv output <> "' during evaluation because the option 'allow-import-from-derivation' is disabled"
  RealisationFailed message -> message

-- | The warning @trace-import-from-derivation@ gives, naming the first
-- output a context names (@EvalState::realiseContext@ in primops.cc at
-- 2.34.9).
importFromDerivationWarning :: StorePath -> Text -> Text
importFromDerivationWarning drv output = "built '" <> builtText drv output <> "' during evaluation due to an import from derivation"

-- | An output of a derivation as upstream renders a built derived path,
-- the derivation's path, a caret and the output's name
-- (@DerivedPath::Built::to_string@ in derived-path.cc).
builtText :: StorePath -> Text -> Text
builtText drv output = storePathToText defaultStoreDir drv <> "^" <> output

-- | A refusal of a search path entry's context as @builtins.findFile@
-- words it: a path that is not valid names the entry and the path's base
-- name (@prim_findFile@ in primops.cc, 2.34.9 and later, where 2.24.9 names
-- the whole path), and any other refusal is worded as it is anywhere else.
findFileRefusalMessage :: Text -> ContextRefusal -> Text
findFileRefusalMessage entry refusal = case refusal of
  PathNotValid path -> "cannot find '" <> entry <> "', since path '" <> storePathBasename path <> "' is not valid"
  other -> contextRefusalMessage other
