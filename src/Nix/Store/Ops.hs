{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | The store a command works against, as a record of its operations:
-- what it holds, adding what an evaluation wrote, building, checking, and
-- rooting an out-link.  A command runs over the record rather than an open
-- t'Nix.Store.Handle.Store', so it runs as well over a store reached some
-- other way, and over a fake one in tests, in the pattern of
-- "Nix.Realise"'s 'Nix.Realise.RealiseEffects'.
--
-- 'withLocalStore' gives the operations of the store in a directory,
-- which it opens when an operation first needs it and closes once, when
-- the command is done with it.  'localQueries' answers what that store
-- holds for a session that only reads: it makes no store and writes no
-- row, though SQLite can make its wal-index files beside the database
-- ('Nix.Store.DB.openStoreDBReadOnly').
--
-- In a store upstream Nix's database also manages, nova-nix holds no
-- rows, so the queries count a path there as valid when its tree exists
-- and know no references for it ("Nix.Store.Upstream").  That holds until
-- validity there comes from the store's owner, through a client of
-- upstream's daemon.
module Nix.Store.Ops
  ( -- * The operations
    StoreQueries (..),
    StoreOps (..),
    EvalObjects (..),
    nothingToAdd,
    BuildStop (..),

    -- * The local store
    withLocalStore,
    localQueries,

    -- * Opening on first use
    withOpenedOnFirstUse,
  )
where

import Control.Concurrent.MVar (modifyMVarMasked, newMVar, swapMVar)
import Control.Exception (bracket)
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.Builder (BuildConfig (..), BuildStop (..), SubstitutionAccess (..), buildRequests, checkStored, realiseWithoutWriting)
import Nix.Store (Store (..), StoreDB, UpstreamHazard (..), addOutLinkRoot, canonicalStoreDir, closeStore, closeStoreDB, dbFileName, isValidPath, materializeEvalSources, materializeEvalStoreWrites, metaDirName, openStore, openStoreDBReadOnly, queryReferences, storeTablesExist, upstreamManagedRefusal, writeDrvClosure)
import Nix.Store.Path (StoreDir (..), StorePath, StoreWriteMode, parseStorePath, storePathToFilePath)
import Nix.Store.Symlink (WalkNode (..), classifyWalkNode)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Error (illegalOperationErrorType, ioeSetErrorString, mkIOError)

-- ---------------------------------------------------------------------------
-- The operations
-- ---------------------------------------------------------------------------

-- | What a store answers about what it holds.
data StoreQueries m = StoreQueries
  { -- | Which of these paths are valid.
    sqValidPaths :: !(Set StorePath -> m (Set StorePath)),
    -- | The references the store records for a path, none for one it does
    -- not hold, failing on a recorded reference that names no store path.
    sqReferences :: !(StorePath -> m (Either Text (Set StorePath)))
  }

-- | What a command reads from, writes to and builds in a store.
data StoreOps m = StoreOps
  { -- | What the store holds, asked through the same handle as the
    -- operations below, so a store that is not there yet is made by the
    -- first question.
    soQueries :: !(StoreQueries m),
    -- | Add what an evaluation wrote or named, references first: its
    -- sources, then the trees it wrote, each passed over when it is
    -- valid, then every derivation it recorded, written unless its file
    -- already holds those bytes and registered again whether or not it
    -- was valid.  A store that refuses the add says why before anything
    -- is written; an add of nothing asks nothing of the store and so
    -- refuses nothing.
    soAddEvalObjects :: !(EvalObjects -> m (Either Text ())),
    -- | Realise each derivation for the outputs a request names, with the
    -- message and status of a build that stops.
    soBuild :: !([(StorePath, Set Text)] -> m (Either BuildStop ())),
    -- | Check one derivation, every output of it, as @nova-nix build
    -- --check@ does, with the message and status of a check that stops.
    soCheck :: !(StorePath -> m (Either BuildStop ())),
    -- | Link a path to a store path and register the link as a root,
    -- answering the link as the root records it, or why it was refused.
    soAddOutLinkRoot :: !(FilePath -> StorePath -> m (Either Text FilePath))
  }

-- | What an evaluation named or wrote in the store without registering
-- it, as its session records them ("Nix.Eval.IO"'s @EvalState@).
data EvalObjects = EvalObjects
  { -- | Each source a path value was coerced from, with the store path
    -- evaluation named it by ('Nix.Store.materializeEvalSources').
    eoSources :: !(Map Text Text),
    -- | Each tree evaluation wrote, by its store path, with its
    -- references and the way its path was named
    -- ('Nix.Store.materializeEvalStoreWrites').
    eoStoreWrites :: !(Map Text ([StorePath], StoreWriteMode)),
    -- | Each derivation's ATerm, by its store path
    -- ('Nix.Store.writeDrvClosure').
    eoDerivations :: !(Map Text ByteString)
  }

-- ---------------------------------------------------------------------------
-- The local store
-- ---------------------------------------------------------------------------

-- | Run an action with the operations of the store in 'bcStoreDir'.  The
-- store is opened when an operation first needs it and closed once, when
-- the action ends, by returning or by an exception, so an action that
-- asks nothing of the store never opens it, and one handle serves every
-- operation ("Nix.Store.Handle" says why a process keeps one).  A build
-- and a check run under the configuration ('buildRequests',
-- 'checkStored'), whatever its 'bcMode' says.
--
-- An add, a build or a check in a store upstream Nix's database may also
-- manage is refused before the store is opened, since opening it would
-- make this store's database there ("Nix.Store.Upstream").  Under
-- 'RefuseSubstitution' a build there opens nothing and refuses only what
-- it would build ('realiseWithoutWriting'), so a substitution is refused
-- in upstream's read-only words first.  An add's other failures
-- are those of 'Nix.Store.materializeEvalSources',
-- 'Nix.Store.materializeEvalStoreWrites' and 'Nix.Store.writeDrvClosure',
-- which throw.
withLocalStore :: BuildConfig -> (StoreOps IO -> IO a) -> IO a
withLocalStore config action = withOpenedOnFirstUse (openStore dir) closeStore (action . localOps)
  where
    dir = bcStoreDir config
    refusing = upstreamManagedRefusal ReplacesUpstreamPaths dir
    localOps useStore =
      StoreOps
        { soQueries = queriesOf dir (\_ _ query -> useStore >>= query . stDB),
          soAddEvalObjects = \objects ->
            if nothingToAdd objects
              then pure (Right ())
              else refusing >>= maybe (Right <$> (useStore >>= (`addEvalObjects` objects))) (pure . Left),
          soBuild = \requests ->
            refusing >>= \case
              Nothing -> useStore >>= \store -> buildRequests config store requests
              Just reason -> case bcSubstitution config of
                SubstituteFromCaches -> pure (Left (BuildStop reason 1))
                RefuseSubstitution -> realiseWithoutWriting config (heldIn (localQueries dir)) reason requests,
          soCheck = \drvPath -> building (\store -> checkStored config store drvPath),
          soAddOutLinkRoot = \link path -> useStore >>= \store -> addOutLinkRoot store link path
        }
      where
        building run = refusing >>= maybe (useStore >>= run) (pure . Left . (`BuildStop` 1))
        heldIn queries path = Set.member path <$> sqValidPaths queries (Set.singleton path)

-- | The queries of the store in a directory for a session that only
-- reads: each opens the store's database read-only for itself and closes
-- it ('openStoreDBReadOnly', which says what SQLite still creates beside
-- it).  A store with no database, or one whose database does not hold the
-- store's tables yet ('storeTablesExist'), answers that it holds nothing,
-- as upstream's read-only evaluation answers once it has made the tables
-- of a store that has none (local-store.cc:340-344 at 2.24.9); neither
-- the directory, its metadata, its database nor its collector lock is
-- ever made.  No lease is taken, so a collection may remove a path a
-- query answered for, which a later read then finds missing.
localQueries :: StoreDir -> StoreQueries IO
localQueries rawDir = queriesOf rawDir inDatabase
  where
    inDatabase dir none query = do
      present <- doesFileExist (unStoreDir dir </> metaDirName </> dbFileName)
      if present then bracket (openStoreDBReadOnly dir) closeStoreDB (\db -> storeTablesExist db >>= \ready -> if ready then query db else pure none) else pure none

-- | The queries of the store in a directory, each run against its
-- database through the given action, given the directory's canonical
-- spelling, which answers the value it is given when there is no database
-- to ask.  In a store upstream Nix's database also manages the database
-- is never opened (the module header says why).
queriesOf :: StoreDir -> (forall a. StoreDir -> a -> (StoreDB -> IO a) -> IO a) -> StoreQueries IO
queriesOf rawDir withDatabase =
  StoreQueries
    { sqValidPaths = \paths ->
        asked $ \dir upstreamManaged ->
          if upstreamManaged
            then Set.fromList <$> filterM (onDisk dir) (Set.toList paths)
            else withDatabase dir Set.empty (\db -> Set.fromList <$> filterM (isValidPath db) (Set.toList paths)),
      sqReferences = \path ->
        asked $ \dir upstreamManaged ->
          if upstreamManaged
            then pure (Right Set.empty)
            else withDatabase dir (Right Set.empty) (\db -> fmap Set.fromList . traverse (named dir) <$> queryReferences db path)
    }
  where
    asked query = do
      dir <- canonicalStoreDir rawDir
      upstreamManaged <- isJust <$> upstreamManagedRefusal ReplacesUpstreamPaths dir
      query dir upstreamManaged
    onDisk dir path = (/= WalkAbsent) <$> classifyWalkNode (storePathToFilePath dir path)
    named dir text = maybe (Left ("unparseable reference in store DB: " <> text)) Right (parseStorePath dir text)

-- | Whether an evaluation made nothing to add: no source, tree or
-- derivation.
nothingToAdd :: EvalObjects -> Bool
nothingToAdd objects = Map.null (eoSources objects) && Map.null (eoStoreWrites objects) && Map.null (eoDerivations objects)

-- | Add an evaluation's objects to an open store, references first.
addEvalObjects :: Store -> EvalObjects -> IO ()
addEvalObjects store objects = do
  materializeEvalSources store (eoSources objects)
  materializeEvalStoreWrites store (eoStoreWrites objects)
  writeDrvClosure store (eoDerivations objects)

-- ---------------------------------------------------------------------------
-- Opening on first use
-- ---------------------------------------------------------------------------

-- | Where a resource opened on first use stands.
data Opening r = Unopened | Opened !r | Closed

-- | Run an action with a resource opened on first use.  The action is
-- given an action that opens the resource the first time it runs and
-- answers the same one after.  When the action ends, by returning or by
-- an exception, the resource is closed once if it was opened, and a use
-- after that fails with an illegal-operation 'IOError', as a closed
-- handle's does.  The opening runs with asynchronous exceptions masked,
-- as 'bracket' runs its acquisition, so a resource that opens is closed.
withOpenedOnFirstUse :: IO r -> (r -> IO ()) -> (IO r -> IO a) -> IO a
withOpenedOnFirstUse open close action = bracket (newMVar Unopened) release (action . use)
  where
    use opening = modifyMVarMasked opening $ \case
      Unopened -> (\resource -> (Opened resource, resource)) <$> open
      Opened resource -> pure (Opened resource, resource)
      Closed -> ioError (ioeSetErrorString (mkIOError illegalOperationErrorType "withOpenedOnFirstUse" Nothing Nothing) "used after it was closed")
    release opening =
      swapMVar opening Closed >>= \case
        Opened resource -> close resource
        _ -> pure ()
