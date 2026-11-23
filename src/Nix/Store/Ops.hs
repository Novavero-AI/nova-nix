{-# LANGUAGE LambdaCase #-}

-- | The store a command works against, as a record of its operations:
-- adding what an evaluation wrote, building, checking, and rooting an
-- out-link.  A command runs over the record rather than an open
-- t'Nix.Store.Handle.Store', so it runs as well over a store reached some
-- other way, and over a fake one in tests, in the pattern of
-- "Nix.Realise"'s 'Nix.Realise.RealiseEffects'.
--
-- 'withLocalStore' gives the operations of the store in a directory,
-- which it opens when an operation first needs it and closes once, when
-- the command is done with it.
module Nix.Store.Ops
  ( -- * The operations
    StoreOps (..),
    EvalObjects (..),
    BuildStop (..),

    -- * The local store
    withLocalStore,

    -- * Opening on first use
    withOpenedOnFirstUse,
  )
where

import Control.Concurrent.MVar (modifyMVarMasked, newMVar, swapMVar)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Nix.Builder (BuildConfig (..), BuildStop (..), buildRequests, checkStored)
import Nix.Store (Store, UpstreamHazard (..), addOutLinkRoot, closeStore, materializeEvalSources, materializeEvalStoreWrites, openStore, upstreamManagedRefusal, writeDrvClosure)
import Nix.Store.Path (StorePath, StoreWriteMode)
import System.IO.Error (illegalOperationErrorType, ioeSetErrorString, mkIOError)

-- ---------------------------------------------------------------------------
-- The operations
-- ---------------------------------------------------------------------------

-- | What a command writes to and builds in a store.
data StoreOps m = StoreOps
  { -- | Add what an evaluation wrote or named, references first: its
    -- sources, then the trees it wrote, each passed over when it is
    -- valid, then every derivation it recorded, written unless its file
    -- already holds those bytes and registered again whether or not it
    -- was valid.  A store that refuses the add says why before anything
    -- is written.
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
-- make this store's database there ("Nix.Store.Upstream").  An add's other failures
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
        { soAddEvalObjects = \objects -> refusing >>= maybe (Right <$> (useStore >>= (`addEvalObjects` objects))) (pure . Left),
          soBuild = \requests -> building (\store -> buildRequests config store requests),
          soCheck = \drvPath -> building (\store -> checkStored config store drvPath),
          soAddOutLinkRoot = \link path -> useStore >>= \store -> addOutLinkRoot store link path
        }
      where
        building run = refusing >>= maybe (useStore >>= run) (pure . Left . (`BuildStop` 1))

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
