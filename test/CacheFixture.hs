-- | A model of nova-cache's write endpoints, its multipart upload among
-- them, behind a proxy that refuses a request body over a limit with 413
-- as Cloudflare does, served on loopback for the push tests.  Answers
-- follow nova-cache's server: a narinfo is accepted only when the NAR it
-- names is stored with its FileSize and FileHash, a part only when the
-- parts before it are held, and a completion only when the declared part
-- count and size match what is held.  Faults injected per request stand
-- in for a lost response or a server that lost an upload's parts.
module CacheFixture
  ( CacheSetup (..),
    Multipart (..),
    Fault (..),
    RequestKind (..),
    Logged (..),
    CacheState,
    cacheSetup,
    withModelCache,
    storedObject,
    requestLog,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import HttpFixture (FixtureRequest (..), Reply (..), httpResponse, withRequestServer)
import qualified NovaCache.Hash as Hash
import NovaCache.NarInfo (NarInfo (..), parseNarInfo, renderNarInfo)
import Numeric (showHex)

-- | Whether the cache offers the multipart upload: a nova-cache that does
-- not answers each of its requests with 404, before any key is checked.
data Multipart = WithMultipart | WithoutMultipart

-- | What a request is, as the faults and the log name it.
data RequestKind
  = HashList
  | NarInfoGet
  | NarInfoPut
  | NarPut
  | Begin
  | Part !Int
  | Complete
  | Unrouted
  deriving (Eq, Ord, Show)

-- | A fault in place of the server's usual handling of one request.
data Fault
  = -- | Handle the request, then close the connection with no response.
    DropResponse
  | -- | Answer with this status line, and handle nothing.
    Refuse !BS.ByteString
  | -- | Lose every part the request's upload holds, then handle it.
    LoseParts
  | -- | Forget the request's upload, as a reclaim does, then handle it.
    ForgetUpload

-- | The cache a test pushes to.
data CacheSetup = CacheSetup
  { -- | The proxy's limit: a request body over it is refused with 413.
    csBodyLimit :: !Int,
    csMultipart :: !Multipart,
    -- | The write key every write and the hash list must carry.
    csApiKey :: !BS.ByteString,
    -- | The fault, if any, for a request of this kind that the cache has
    -- seen this many times before.
    csFault :: !(RequestKind -> Int -> Maybe Fault)
  }

-- | A cache with the multipart upload, the given proxy limit and write
-- key, and no faults.
cacheSetup :: Int -> BS.ByteString -> CacheSetup
cacheSetup limit key =
  CacheSetup
    { csBodyLimit = limit,
      csMultipart = WithMultipart,
      csApiKey = key,
      csFault = \_ _ -> Nothing
    }

-- | One request as the cache received it.
data Logged = Logged
  { lgKind :: !RequestKind,
    lgBodyBytes :: !Int
  }
  deriving (Eq, Show)

-- | An upload in progress: its NAR's file name and the parts it holds,
-- which are always parts 1 to their count.
data Upload = Upload
  { upFile :: !BS.ByteString,
    upParts :: !(Map Int BS.ByteString)
  }

-- | What the cache holds and what it was asked.
data CacheState = CacheState
  { -- | Stored files by request path: @nar\/\<file\>@ and
    -- @\<hash\>.narinfo@.
    stObjects :: !(Map BS.ByteString BS.ByteString),
    stUploads :: !(Map BS.ByteString Upload),
    stIssued :: !Int,
    stSeen :: !(Map RequestKind Int),
    -- | Newest first.
    stLog :: ![Logged]
  }

-- | Serve the model on loopback for the action, which gets its base URL;
-- returns the action's result and what the cache ended up holding.
withModelCache :: CacheSetup -> (Text -> IO a) -> IO (a, CacheState)
withModelCache setup action = do
  ref <- newIORef (CacheState Map.empty Map.empty 0 Map.empty [])
  result <- withRequestServer (atomicModifyIORef' ref . step setup) action
  final <- readIORef ref
  pure (result, final)

-- | The file stored at a request path.
storedObject :: BS.ByteString -> CacheState -> Maybe BS.ByteString
storedObject path = Map.lookup path . stObjects

-- | Every request the cache received, in order.
requestLog :: CacheState -> [Logged]
requestLog = reverse . stLog

-- | The request paths the model routes.
data Route
  = HashListRoute
  | NarInfoRoute !BS.ByteString
  | NarRoute !BS.ByteString
  | UploadsRoute !BS.ByteString
  | UploadRoute !BS.ByteString !BS.ByteString
  | PartRoute !BS.ByteString !BS.ByteString !BS.ByteString
  | NoRoute

routeOf :: BS.ByteString -> Route
routeOf target = case BSC.split '/' (BS.drop 1 target) of
  ["narinfo-hashes"] -> HashListRoute
  [name] | Just hash <- BS.stripSuffix narInfoSuffix name -> NarInfoRoute hash
  ["nar", file] -> NarRoute file
  ["nar", file, "uploads"] -> UploadsRoute file
  ["nar", file, "uploads", uploadId] -> UploadRoute file uploadId
  ["nar", file, "uploads", uploadId, number] -> PartRoute file uploadId number
  _ -> NoRoute

kindOf :: BS.ByteString -> Route -> RequestKind
kindOf method route = case (method, route) of
  ("GET", HashListRoute) -> HashList
  ("GET", NarInfoRoute _) -> NarInfoGet
  ("PUT", NarInfoRoute _) -> NarInfoPut
  ("PUT", NarRoute _) -> NarPut
  ("POST", UploadsRoute _) -> Begin
  ("PUT", PartRoute _ _ number) -> maybe Unrouted Part (canonicalInt number)
  ("POST", UploadRoute _ _) -> Complete
  _ -> Unrouted

-- | One request: logged, refused by the proxy when its body is over the
-- limit, otherwise handled as the setup's fault for it says.
step :: CacheSetup -> FixtureRequest -> CacheState -> (CacheState, Reply)
step setup request before
  | BS.length (frBody request) > csBodyLimit setup = (logged, respond "413 Payload Too Large" "request body too large")
  | otherwise = case csFault setup kind seen of
      Nothing -> serve setup request route logged
      Just DropResponse -> (fst (serve setup request route logged), Drop)
      Just (Refuse status) -> (logged, respond status "refused by the fixture")
      Just LoseParts -> serve setup request route (withUpload (\upload -> Just upload {upParts = Map.empty}) logged)
      Just ForgetUpload -> serve setup request route (withUpload (const Nothing) logged)
  where
    route = routeOf (frTarget request)
    kind = kindOf (frMethod request) route
    seen = Map.findWithDefault 0 kind (stSeen before)
    logged =
      before
        { stSeen = Map.insert kind (seen + 1) (stSeen before),
          stLog = Logged kind (BS.length (frBody request)) : stLog before
        }
    withUpload change state = case route of
      PartRoute _ uploadId _ -> state {stUploads = Map.update change uploadId (stUploads state)}
      UploadRoute _ uploadId -> state {stUploads = Map.update change uploadId (stUploads state)}
      _ -> state

-- | Handle a request the proxy let through, as nova-cache does.
serve :: CacheSetup -> FixtureRequest -> Route -> CacheState -> (CacheState, Reply)
serve setup request route state
  | WithoutMultipart <- csMultipart setup, isUploadRoute route = (state, notFound)
  | writes && lookup "authorization" (frHeaders request) /= Just ("Bearer " <> csApiKey setup) =
      (state, respond "401 Unauthorized" "missing or wrong write key")
  | otherwise = case (frMethod request, route) of
      ("GET", HashListRoute) ->
        (state, respond "200 OK" (BSC.unlines (Map.keys (Map.mapMaybeWithKey (\path _ -> BS.stripSuffix narInfoSuffix path) (stObjects state)))))
      ("GET", NarInfoRoute hash) ->
        (state, maybe notFound (respond "200 OK") (Map.lookup (hash <> narInfoSuffix) (stObjects state)))
      ("PUT", NarInfoRoute hash) -> either (\err -> (state, respond "400 Bad Request" err)) (\signed -> (store (hash <> narInfoSuffix) signed, ok)) (checkNarInfo body)
      ("PUT", NarRoute file) -> (store (narPath file) body, ok)
      ("POST", UploadsRoute file) ->
        let uploadId = uploadIdText (stIssued state + 1)
         in ( state {stIssued = stIssued state + 1, stUploads = Map.insert uploadId (Upload file Map.empty) (stUploads state)},
              respond "201 Created" (uploadId <> "\n")
            )
      ("PUT", PartRoute file uploadId number) -> putPart file uploadId number
      ("POST", UploadRoute file uploadId) -> complete file uploadId
      _ -> (state, notFound)
  where
    body = frBody request
    writes = frMethod request /= "GET" || isHashList route
    isHashList HashListRoute = True
    isHashList _ = False
    isUploadRoute (UploadsRoute _) = True
    isUploadRoute (UploadRoute _ _) = True
    isUploadRoute (PartRoute {}) = True
    isUploadRoute _ = False
    notFound = respond "404 Not Found" "not found"
    ok = respond "200 OK" ""
    store path bytes = state {stObjects = Map.insert path bytes (stObjects state)}
    upload file uploadId = case Map.lookup uploadId (stUploads state) of
      Just found | upFile found == file -> Just found
      _ -> Nothing
    unknown file uploadId = (state, respond "404 Not Found" ("no upload " <> uploadId <> " of " <> narPath file))
    putPart file uploadId number = case (upload file uploadId, canonicalInt number) of
      (_, Nothing) -> (state, respond "400 Bad Request" "part number must be a whole number from 1 to 10000")
      (Nothing, _) -> unknown file uploadId
      (Just found, Just n)
        | n < 1 || n > maxParts -> (state, respond "400 Bad Request" "part number must be a whole number from 1 to 10000")
        | n > held + 1 ->
            ( state,
              respond
                "409 Conflict"
                ("upload of " <> narPath file <> " holds " <> countedParts held <> ", so the next is part " <> showBytes (held + 1) <> ", not part " <> number)
            )
        | otherwise -> (state {stUploads = Map.insert uploadId found {upParts = Map.insert n body (upParts found)} (stUploads state)}, ok)
        where
          held = Map.size (upParts found)
    complete file uploadId = case (upload file uploadId, parseTotals body) of
      (Nothing, _) -> unknown file uploadId
      (Just _, Nothing) -> (state, respond "400 Bad Request" "completion body is not Parts and FileSize lines")
      (Just found, Just (count, size))
        | count /= Map.size (upParts found) -> (state, respond "409 Conflict" "Parts is not the parts held")
        | size /= BS.length joined -> (state, respond "409 Conflict" "FileSize is not the size the parts join to")
        | otherwise -> ((store (narPath file) joined) {stUploads = Map.delete uploadId (stUploads state)}, ok)
        where
          joined = BS.concat (Map.elems (upParts found))
    -- The narinfo, signed, when the NAR it names is stored as it declares.
    checkNarInfo bytes = do
      narInfo <- either (const (Left "narinfo is not UTF-8")) Right (TE.decodeUtf8' bytes) >>= either (Left . BSC.pack) Right . parseNarInfo
      file <- maybe (Left "narinfo URL is not under nar/") Right (T.stripPrefix "nar/" (niUrl narInfo))
      let named = narPath (TE.encodeUtf8 file)
      nar <- maybe (Left ("NAR " <> named <> " is not stored; upload it before its narinfo")) Right (Map.lookup named (stObjects state))
      if niFileSize narInfo == Just (fromIntegral (BS.length nar)) && niFileHash narInfo == Just (Hash.formatNixHash (Hash.hashBytes nar))
        then Right (TE.encodeUtf8 (renderNarInfo narInfo {niSigs = [fixtureSignature]}))
        else Left ("NAR " <> named <> " is not the file the narinfo declares")

-- | The body that completes an upload: a @Parts@ and a @FileSize@ line,
-- in either order, each value in canonical decimal.
parseTotals :: BS.ByteString -> Maybe (Int, Int)
parseTotals body = case sort (BSC.lines body) of
  [sizeLine, partsLine] -> (,) <$> field "Parts: " partsLine <*> field "FileSize: " sizeLine
  _ -> Nothing
  where
    field key line = BS.stripPrefix key line >>= canonicalInt

-- | A number in canonical decimal: digits only, no sign, no leading zero.
canonicalInt :: BS.ByteString -> Maybe Int
canonicalInt text = case BSC.readInt text of
  Just (n, rest) | BS.null rest && showBytes n == text -> Just n
  _ -> Nothing

-- | The id of the nth upload the model issues: 32 lowercase hex digits,
-- as nova-cache issues them.
uploadIdText :: Int -> BS.ByteString
uploadIdText n = BSC.pack (replicate (uploadIdDigits - length digits) '0' <> digits)
  where
    digits = showHex n ""

-- | A count of parts with its noun, as nova-cache words it.
countedParts :: Int -> BS.ByteString
countedParts 1 = "1 part"
countedParts n = showBytes n <> " parts"

respond :: BS.ByteString -> BS.ByteString -> Reply
respond status = Send . httpResponse status

narPath :: BS.ByteString -> BS.ByteString
narPath file = "nar/" <> file

showBytes :: Int -> BS.ByteString
showBytes = BSC.pack . show

narInfoSuffix :: BS.ByteString
narInfoSuffix = ".narinfo"

-- | Stands in for the server's signature; push only checks one is there.
fixtureSignature :: Text
fixtureSignature = "fixture-1:c2lnbmVk"

uploadIdDigits, maxParts :: Int
uploadIdDigits = 32
maxParts = 10000
