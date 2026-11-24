{-# LANGUAGE OverloadedStrings #-}

-- | Scoped loopback HTTP servers for the download tests: the mirrors
-- @builtin:fetchurl@ tries, and the binary cache the substituter reads;
-- and for the push tests, a server that reads each request's body, and
-- one that serves a directory's files, closing each connection or keeping
-- it open for the next request.
module HttpFixture
  ( Reply (..),
    FixtureRequest (..),
    httpResponse,
    withHttpServer,
    withFetchurlServer,
    withRequestServer,
    withDirectoryServer,
    withKeepAliveDirectoryServer,
  )
where

import Control.Concurrent (newEmptyMVar, readMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (async, cancel, race, wait, withAsync)
import Control.Exception (IOException, bracket, bracketOnError, finally, try)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import System.FilePath (joinPath, (</>))

-- | How the server answers one request.
data Reply
  = -- | These bytes, a whole response, then a close.
    Send !BS.ByteString
  | -- | The headers of a body that outlives the cancellation watchdog,
    -- then that body a byte at a time; fixture cleanup stops it.
    Trickle
  | -- | No response: the connection closes once the request is read, as
    -- when a response is lost on its way back.
    Drop

-- | A request as 'withRequestServer' read it.
data FixtureRequest = FixtureRequest
  { frMethod :: !BS.ByteString,
    frTarget :: !BS.ByteString,
    -- | Each header's name in lowercase, with its value.
    frHeaders :: ![(BS.ByteString, BS.ByteString)],
    -- | The body, as long as its @Content-Length@ said.
    frBody :: !BS.ByteString
  }

-- | Loopback HTTP only: no external services, fixed ports, or subprocesses.
-- The route answers each request from its target and the number of times
-- that target was requested before.  The callback gets the base URL,
-- request history, and a first-request signal.  Socket and server-thread
-- lifetimes are scoped to the callback.
withHttpServer :: (BS.ByteString -> Int -> Reply) -> (Text -> IO [BS.ByteString] -> IO () -> IO a) -> IO a
withHttpServer route action = do
  requests <- newIORef []
  requested <- newEmptyMVar
  let serve client = do
        header <- readHeaders client BS.empty
        path <- case BSC.words (BSC.takeWhile (/= '\r') header) of
          "GET" : target : _ -> pure target
          _ -> fail "unexpected fixture request"
        earlier <- atomicModifyIORef' requests (\seen -> (path : seen, length (filter (== path) seen)))
        answer (void (tryPutMVar requested ())) client (route path earlier)
  withLoopbackServer OneAtATime serve (\base -> action base (reverse <$> readIORef requests) (readMVar requested))

-- | A loopback server for requests that carry a body: each request's
-- method, target, headers and body (read to its @Content-Length@, none
-- without one) go to the handler, which answers it.  Requests are served
-- one at a time, in the order they connect.
withRequestServer :: (FixtureRequest -> IO Reply) -> (Text -> IO a) -> IO a
withRequestServer handler = withLoopbackServer OneAtATime $ \client -> do
  (header, afterHeader) <- BS.breakSubstring headerEnd <$> readHeaders client BS.empty
  request <- case BSC.lines (BSC.filter (/= '\r') header) of
    requestLine : headerLines
      | method : target : _ <- BSC.words requestLine ->
          pure (FixtureRequest method target (map headerField headerLines) BS.empty)
    _ -> fail "unexpected fixture request"
  bodyLength <- case lookup "content-length" (frHeaders request) of
    Nothing -> pure 0
    Just value
      | Just (len, rest) <- BSC.readInt value, BS.null rest, len >= 0 -> pure len
      | otherwise -> fail "fixture request has a bad Content-Length"
  body <- readBody client bodyLength (BS.drop (BS.length headerEnd) afterHeader)
  answer (pure ()) client =<< handler request {frBody = body}
  where
    headerField line =
      let (name, value) = BSC.break (== ':') line
       in (BSC.map toLower name, BSC.dropWhile (== ' ') (BS.drop 1 value))
    readBody client len bytes
      | BS.length bytes >= len = pure (BS.take len bytes)
      | otherwise = do
          chunk <- Socket.recv client headerChunkBytes
          if BS.null chunk then fail "incomplete fixture request body" else readBody client len (bytes <> chunk)

-- | A static file server over a directory, as a web server serving a
-- binary cache's directory is: a GET answers the file under the directory
-- that its target names, and a request for anything else is a 404.  The
-- callback gets the base URL.
withDirectoryServer :: FilePath -> (Text -> IO a) -> IO a
withDirectoryServer root = withRequestServer (fmap (Send . uncurry httpResponse) . directoryAnswer root (,))

-- | 'withDirectoryServer' over connections it keeps open, as a web server
-- speaking HTTP/1.1 keeps them: each response leaves the connection open
-- for the client's next request, so a client may keep it in a pool.  Each
-- client is served on a thread of its own until it closes the
-- connection.
withKeepAliveDirectoryServer :: FilePath -> (Text -> IO a) -> IO a
withKeepAliveDirectoryServer root = withLoopbackServer EachOnItsOwnThread (serveKept BS.empty)
  where
    serveKept pending client = do
      next <- nextRequest client pending
      case next of
        Nothing -> pure ()
        Just (header, rest) -> do
          answered <- case BSC.words (BSC.takeWhile (/= '\r') header) of
            method : target : _ -> directoryAnswer root (,) (FixtureRequest method target [] BS.empty)
            _ -> fail "unexpected fixture request"
          Socket.sendAll client (uncurry keptResponse answered)
          serveKept rest client

-- | The status and body a directory server answers a request with: a GET
-- the file under the directory its target names, anything else a 404.
directoryAnswer :: FilePath -> (BS.ByteString -> BS.ByteString -> a) -> FixtureRequest -> IO a
directoryAnswer root reply request = case (frMethod request, servedFile (frTarget request)) of
  ("GET", Just relative) -> do
    found <- try (BS.readFile (root </> relative)) :: IO (Either IOException BS.ByteString)
    pure (either (const notFound) (reply "200 OK") found)
  _ -> pure notFound
  where
    notFound = reply "404 Not Found" "not found"
    -- A target's path below the directory, never one that leaves it.
    servedFile target = case BSC.split '/' target of
      "" : segments@(_ : _)
        | all (`notElem` ["", ".", ".."]) segments -> Just (joinPath (map BSC.unpack segments))
      _ -> Nothing

-- | The next request's headers on a kept connection, and the bytes that
-- came after them, or 'Nothing' once the client has closed the connection
-- between requests.
nextRequest :: Socket.Socket -> BS.ByteString -> IO (Maybe (BS.ByteString, BS.ByteString))
nextRequest client pending = case BS.breakSubstring headerEnd pending of
  (header, rest)
    | not (BS.null rest) -> pure (Just (header, BS.drop (BS.length headerEnd) rest))
    | BS.length pending > maxHeaderBytes -> fail "fixture request headers too large"
    | otherwise -> do
        chunk <- Socket.recv client headerChunkBytes
        case (BS.null chunk, BS.null pending) of
          (True, True) -> pure Nothing
          (True, False) -> fail "incomplete fixture request"
          _ -> nextRequest client (pending <> chunk)

-- | A whole response that leaves the connection open.
keptResponse :: BS.ByteString -> BS.ByteString -> BS.ByteString
keptResponse status body =
  "HTTP/1.1 " <> status <> "\r\nContent-Length: " <> BSC.pack (show (BS.length body)) <> "\r\n\r\n" <> body

-- | How a loopback server takes the clients it accepts.
data Clients
  = -- | Each served to the end before the next is accepted.
    OneAtATime
  | -- | Each served on a thread of its own, all of them stopped when the
    -- server's scope ends.
    EachOnItsOwnThread

-- | Serve each client the listener accepts with the action, for as long
-- as the callback runs, on a loopback port the callback gets as a base
-- URL.
withLoopbackServer :: Clients -> (Socket.Socket -> IO ()) -> (Text -> IO a) -> IO a
withLoopbackServer clients serve action = Socket.withSocketsDo $
  bracket listen Socket.close $ \listener -> do
    address <- Socket.getSocketName listener
    base <- case address of
      Socket.SockAddrInet port _ -> pure ("http://127.0.0.1:" <> T.pack (show port))
      _ -> fail "fixture did not bind IPv4"
    threads <- newIORef []
    let acceptOne = case clients of
          OneAtATime -> bracket (fst <$> Socket.accept listener) Socket.close serve
          EachOnItsOwnThread -> bracketOnError (fst <$> Socket.accept listener) Socket.close $ \client -> do
            thread <- async (serve client `finally` Socket.close client)
            atomicModifyIORef' threads (\started -> (thread : started, ()))
    withAsync (forever acceptOne) $ \worker -> do
      -- WinSock accept is a blocking safe FFI call. Close the listener before
      -- withAsync waits for cancellation, or that wait can never finish.
      outcome <- race (wait worker) (action base) `finally` (Socket.close listener >> readIORef threads >>= mapM_ cancel)
      case outcome of
        Left () -> fail "fixture server stopped unexpectedly"
        Right result -> pure result
  where
    listen = bracketOnError (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \sock -> do
      Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
      Socket.listen sock fixtureBacklog
      pure sock

-- | The bytes of a request up to and including the blank line that ends
-- its headers, and any of its body that arrived with them.
readHeaders :: Socket.Socket -> BS.ByteString -> IO BS.ByteString
readHeaders client bytes
  | headerEnd `BS.isInfixOf` bytes = pure bytes
  | BS.length bytes > maxHeaderBytes = fail "fixture request headers too large"
  | otherwise = do
      chunk <- Socket.recv client headerChunkBytes
      if BS.null chunk then fail "incomplete fixture request" else readHeaders client (bytes <> chunk)

-- | Answer a client: send a whole response, trickle one, or send nothing.
-- The signal runs once the client can see the answer begin: before a
-- whole response, after a trickled one's headers.
answer :: IO () -> Socket.Socket -> Reply -> IO ()
answer signal client reply = case reply of
  Send bytes -> signal >> Socket.sendAll client bytes
  Trickle -> do
    Socket.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\nConnection: close\r\n\r\npartial"
    signal
    sendSlowBody slowBodyChunks
  Drop -> signal
  where
    -- Keep WinSock's safe recv calls returning while the response remains in
    -- flight: cancellation is delivered between chunks. A peer disconnect is
    -- expected when the cancellation test closes its HTTP response.
    sendSlowBody :: Int -> IO ()
    sendSlowBody 0 = pure ()
    sendSlowBody remaining = do
      sent <- try (Socket.sendAll client "x") :: IO (Either IOException ())
      case sent of
        Left _ -> pure ()
        Right () -> threadDelay slowBodyIntervalMicros >> sendSlowBody (remaining - 1)

-- | The blank line that ends a request's headers.
headerEnd :: BS.ByteString
headerEnd = "\r\n\r\n"

-- | The mirrors the fetchurl tests name, answering the same way however
-- often they are asked.
withFetchurlServer :: (Text -> IO [BS.ByteString] -> IO () -> IO a) -> IO a
withFetchurlServer = withHttpServer (\path _ -> fetchurlReply path)
  where
    fetchurlReply path = case path of
      "/slow" -> Trickle
      "/missing" -> Send (httpResponse "404 Not Found" "missing")
      "/error" -> Send (httpResponse "500 Internal Server Error" "error")
      "/partial" -> Send "HTTP/1.1 200 OK\r\nContent-Length: 100\r\nConnection: close\r\n\r\nshort"
      "/bad" -> Send (httpResponse "200 OK" "wrong and longer than the expected output")
      "/good" -> Send (httpResponse "200 OK" "hello")
      _ -> Send (httpResponse "500 Internal Server Error" "unexpected request")

-- | A whole response: the status line's code and reason, then the body.
httpResponse :: BS.ByteString -> BS.ByteString -> BS.ByteString
httpResponse status body =
  "HTTP/1.1 "
    <> status
    <> "\r\nContent-Length: "
    <> BSC.pack (show (BS.length body))
    <> "\r\nConnection: close\r\n\r\n"
    <> body

-- The slow body outlives the cancellation watchdog; fixture cleanup stops it.
slowBodyChunks, slowBodyIntervalMicros, fixtureBacklog, maxHeaderBytes, headerChunkBytes :: Int
slowBodyChunks = 600
slowBodyIntervalMicros = 100000
fixtureBacklog = 8
maxHeaderBytes = 8192
headerChunkBytes = 4096
