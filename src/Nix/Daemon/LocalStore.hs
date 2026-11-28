{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The local store as a daemon serves it: each connection's
-- t'DaemonStore' over the daemon's one open t'Store'.
--
-- The daemon holds the store's lease for as long as it runs, so no
-- collection starts while it serves ("Nix.Store.Handle").  Each
-- connection reads the store's database through a connection of its
-- own, which it closes as it ends, and keeps its own temporary roots,
-- which a listing of roots shows with every other connection's and which
-- go with it.
--
-- What the store does not do through a daemon yet is refused, with the
-- client told so: building, substituting, adding paths, adding
-- signatures and collecting garbage, and optimising and verifying the
-- store, which nova-nix does not do at all.  A query a substitution
-- would answer is served as far as it needs none.
module Nix.Daemon.LocalStore
  ( ServedStore,
    newServedStore,
    serveLocal,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (Handler (..), IOException, bracket, catches, displayException)
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (SQLError)
import Nix.Daemon.Connection (ConnectionEnd, ServerConfig, Transport, serveConnection)
import Nix.Daemon.Ops (DaemonStore (..), Intake (..), RootCensoring (..), Substitute (..), daemonError)
import Nix.Daemon.Protocol
  ( DaemonError,
    OutputName (..),
    Trust,
    UnkeyedValidPathInfo (..),
    narHashFromDigest,
  )
import Nix.Derivation (DerivationOutput (..), drvOutputs, fromATerm)
import Nix.HostPath (hostPathBytes)
import Nix.Store (Store (..), isValid)
import Nix.Store.CaseSensitive (volumeCaseHack)
import Nix.Store.DB (PathInfo (..), Ultimate (..), closeStoreDB, openStoreDB, queryAllValidPaths, queryPathInfo, queryReferences, queryReferrers, queryValidDerivers)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.GC (GcRoot (..), addIndirectRoot, findRoots)
import Nix.Store.Path (StorePath, parseStorePath, storePathToFilePath, storePathToText)
import NovaCache.Hash (NixHash (..), parseNixHash)
import qualified System.OsPath as OP

-- ---------------------------------------------------------------------------
-- The store
-- ---------------------------------------------------------------------------

-- | The store a daemon serves, and every connection's temporary roots,
-- by the connection's number.
data ServedStore = ServedStore
  { ssStore :: !Store,
    ssTempRoots :: !(MVar (Map Int (Set StorePath))),
    ssNextConnection :: !(MVar Int)
  }

-- | Serve an open store, which the caller keeps open for as long as it
-- serves.
newServedStore :: Store -> IO ServedStore
newServedStore store = ServedStore store <$> newMVar Map.empty <*> newMVar 0

-- | Serve one connection over the store, its temporary roots dropped and
-- its database connection closed as it ends.
serveLocal :: ServedStore -> ServerConfig -> Trust -> Transport IO -> IO ConnectionEnd
serveLocal served config trust transport =
  bracket (openStoreDB (stDir (ssStore served))) closeStoreDB $ \db ->
    bracket register release $ \number ->
      serveConnection config trust (connectionStore served (ssStore served) {stDB = db} number) transport
  where
    register = do
      number <- modifyMVar (ssNextConnection served) (\next -> pure (next + 1, next))
      number <$ modifyMVar_ (ssTempRoots served) (pure . Map.insert number Set.empty)
    release number = modifyMVar_ (ssTempRoots served) (pure . Map.delete number)

-- ---------------------------------------------------------------------------
-- One connection's view
-- ---------------------------------------------------------------------------

-- | The store as one connection sees it, reading through its own handle
-- on the database.
connectionStore :: ServedStore -> Store -> Int -> DaemonStore IO
connectionStore served store number =
  DaemonStore
    { dsIsValidPath = \_ path -> reading (isValid store path),
      dsQueryValidPaths = \_ substitute paths -> reading (filterM (isValid store) (Set.toAscList paths)) >>= either (pure . Left) (validAmong substitute paths),
      dsQuerySubstitutablePaths = \_ _ -> pure (Left substitutingRefusal),
      dsQueryReferrers = \_ path -> (>>= storePaths) <$> reading (queryReferrers (stDB store) path),
      dsQueryValidDerivers = \_ path -> (>>= storePaths) <$> reading (queryValidDerivers (stDB store) path),
      dsQueryDerivationOutputMap = \_ path -> outputMap path,
      dsQueryAllValidPaths = \_ -> (>>= storePaths) <$> reading (queryAllValidPaths (stDB store)),
      dsQueryPathInfo = \_ path -> pathInfo path,
      dsQueryMissing = \_ _ -> pure (Left substitutingRefusal),
      dsAddToStore = \_ _ -> pure (Taken (Left addingRefusal)),
      dsAddToStoreNar = \_ _ _ _ -> pure (Taken (Left addingRefusal)),
      dsBuildPaths = \_ _ _ -> pure (Left buildingRefusal),
      dsBuildPathsWithResults = \_ _ _ -> pure (Left buildingRefusal),
      dsEnsurePath = \_ path -> (>>= \valid -> if valid then Right () else Left substitutingRefusal) <$> reading (isValid store path),
      dsAddTempRoot = \_ path -> reading (modifyMVar_ (ssTempRoots served) (pure . Map.adjust (Set.insert path) number)),
      dsAddIndirectRoot = \_ link -> reading (addIndirectRoot store link),
      dsFindRoots = \_ censoring -> roots censoring,
      dsCollectGarbage = \_ _ -> pure (Left (daemonError "the nova-nix daemon does not collect garbage yet")),
      dsOptimiseStore = \_ -> pure (Left (daemonError "optimising the store is not available in nova-nix")),
      dsVerifyStore = \_ _ _ -> pure (Left (daemonError "verifying the store is not available in nova-nix")),
      dsAddSignatures = \_ _ _ -> pure (Left (daemonError "the nova-nix daemon does not add signatures yet")),
      dsNarFromPath = narFromPath
    }
  where
    validAmong substitute asked valid
      | Substitute <- substitute, length valid /= Set.size asked = pure (Left substitutingRefusal)
      | otherwise = pure (Right (Set.fromList valid))
    pathInfo path =
      reading ((,) <$> queryPathInfo (stDB store) path <*> queryReferences (stDB store) path) >>= \case
        Left err -> pure (Left err)
        Right (Nothing, _) -> pure (Right Nothing)
        Right (Just info, references) -> pure (Just <$> protocolInfo store info references)
    outputMap drvPath =
      reading (isValid store drvPath) >>= \case
        Left err -> pure (Left err)
        Right False -> pure (Left (notValid drvPath))
        Right True ->
          reading (BS.readFile (storePathToFilePath (stDir store) drvPath)) >>= \case
            Left err -> pure (Left err)
            Right aterm -> pure $ case fromATerm aterm of
              Left refusal -> Left (daemonError refusal)
              Right drv -> Right (Map.fromList [(OutputName (doName output), Just (doPath output)) | output <- drvOutputs drv])
    roots censoring =
      reading (findRoots store >>= traverse rootLink) >>= \case
        Left err -> pure (Left err)
        Right links -> do
          held <- readMVar (ssTempRoots served)
          let temporary = [Right (path, tempRootName censoring holder) | (holder, paths) <- Map.toList held, path <- Set.toList paths]
          pure ((\pairs -> Map.fromListWith Set.union [(path, Set.singleton link) | (path, link) <- pairs]) <$> sequence (links ++ temporary))
    rootLink root = do
      spelled <- hostPathBytes <$> OP.encodeFS (grLink root)
      pure ((,) <$> storePath (grPath root) <*> maybe (Left (daemonError ("cannot spell the root '" <> T.pack (grLink root) <> "' as bytes"))) Right spelled)
    narFromPath path sink =
      reading (isValid store path) >>= \case
        Left err -> pure (Left err)
        Right False -> pure (Left (notValid path))
        Right True -> reading (ExecBit.withNarSource (volumeCaseHack (stCaseSensitivity store)) (storePathToFilePath (stDir store) path) (pump sink))
    pump sink pull =
      pull >>= \chunk ->
        if BS.null chunk then pure () else sink chunk >> pump sink pull
    storePaths = fmap Set.fromList . traverse storePath
    storePath spelled = maybe (Left (daemonError ("the store's database names '" <> spelled <> "', which is not a path in it"))) Right (parseStorePath (stDir store) spelled)
    -- Upstream's refusal of a path that is not valid.
    notValid path = daemonError ("path '" <> storePathToText (stDir store) path <> "' is not valid")

-- | How a connection's temporary root is listed: by the connection that
-- holds it, where upstream names the process that holds one, which for
-- its daemon is the connection's worker.
tempRootName :: RootCensoring -> Int -> ByteString
tempRootName censoring holder = case censoring of
  CensorRoots -> "{censored}"
  ShowRoots -> "{temp:" <> TE.encodeUtf8 (T.pack (show holder)) <> "}"

-- | A path's info from the store's database, in the protocol's terms.
protocolInfo :: Store -> PathInfo -> [Text] -> Either DaemonError UnkeyedValidPathInfo
protocolInfo store info references = do
  deriver <- traverse spelledPath (piDeriver info)
  refs <- traverse spelledPath references
  narHash <- either (const (Left (corrupt "its NAR hash"))) Right (parseNixHash (piNarHash info)) >>= \(NixHash digest) -> maybe (Left (corrupt "its NAR hash")) Right (narHashFromDigest digest)
  pure
    UnkeyedValidPathInfo
      { upiDeriver = deriver,
        upiNarHash = narHash,
        upiReferences = Set.fromList refs,
        upiRegistrationTime = fromIntegral (piRegTime info) :: Int64,
        upiNarSize = fromIntegral (piNarSize info),
        upiUltimate = piUltimate info == Ultimate,
        upiSignatures = piSignatures info,
        upiContentAddress = piContentAddress info
      }
  where
    spelledPath spelled = maybe (Left (corrupt ("'" <> spelled <> "'"))) Right (parseStorePath (stDir store) spelled)
    corrupt what = daemonError ("the store's database holds an entry for '" <> piPath info <> "' whose " <> what <> " cannot be read")

-- ---------------------------------------------------------------------------
-- Failures
-- ---------------------------------------------------------------------------

-- | Run a read of the store, its failure to read the disk or the
-- database an error the client is sent.
reading :: IO a -> IO (Either DaemonError a)
reading action =
  (Right <$> action)
    `catches` [ Handler (\(err :: IOException) -> pure (Left (daemonError (T.pack (displayException err))))),
                Handler (\(err :: SQLError) -> pure (Left (daemonError (T.pack (displayException err)))))
              ]

-- | What the daemon does not do yet.
buildingRefusal, substitutingRefusal, addingRefusal :: DaemonError
buildingRefusal = daemonError "the nova-nix daemon does not build yet"
substitutingRefusal = daemonError "the nova-nix daemon does not substitute yet"
addingRefusal = daemonError "the nova-nix daemon does not add paths yet"
