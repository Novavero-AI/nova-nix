{-# LANGUAGE OverloadedStrings #-}

-- | Scoped loopback HTTP responses for fetch integrity and cancellation tests.
module FetchurlFixture (withFetchurlServer) where

import Control.Concurrent (newEmptyMVar, readMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (race, wait, withAsync)
import Control.Exception (IOException, bracket, bracketOnError, finally, try)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket

-- | Loopback HTTP only: no external services, fixed ports, or subprocesses.
-- The callback gets the base URL, request history, and a first-request signal.
-- Socket and server-thread lifetimes are scoped to the callback.
withFetchurlServer :: (Text -> IO [BS.ByteString] -> IO () -> IO a) -> IO a
withFetchurlServer action = Socket.withSocketsDo $ do
  requests <- newIORef []
  requested <- newEmptyMVar
  let serve listener = forever $ bracket (fst <$> Socket.accept listener) Socket.close $ \client -> do
        header <- readHeaders client BS.empty
        path <- case BSC.words (BSC.takeWhile (/= '\r') header) of
          "GET" : target : _ -> pure target
          _ -> fail "unexpected fixture request"
        atomicModifyIORef' requests (\seen -> (path : seen, ()))
        case path of
          "/slow" -> do
            Socket.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\nConnection: close\r\n\r\npartial"
            void (tryPutMVar requested ())
            sendSlowBody client slowBodyChunks
          _ -> do
            void (tryPutMVar requested ())
            Socket.sendAll client (response path)
  bracket listen Socket.close $ \listener -> do
    address <- Socket.getSocketName listener
    base <- case address of
      Socket.SockAddrInet port _ -> pure ("http://127.0.0.1:" <> T.pack (show port))
      _ -> fail "fixture did not bind IPv4"
    withAsync (serve listener) $ \worker -> do
      -- WinSock accept is a blocking safe FFI call. Close the listener before
      -- withAsync waits for cancellation, or that wait can never finish.
      outcome <- race (wait worker) (action base (reverse <$> readIORef requests) (readMVar requested)) `finally` Socket.close listener
      case outcome of
        Left () -> fail "fixture server stopped unexpectedly"
        Right result -> pure result
  where
    listen = bracketOnError (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \sock -> do
      Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
      Socket.listen sock fixtureBacklog
      pure sock
    readHeaders client bytes
      | "\r\n\r\n" `BS.isInfixOf` bytes = pure bytes
      | BS.length bytes > maxHeaderBytes = fail "fixture request headers too large"
      | otherwise = do
          chunk <- Socket.recv client headerChunkBytes
          if BS.null chunk then fail "incomplete fixture request" else readHeaders client (bytes <> chunk)
    -- Keep WinSock's safe recv calls returning while the response remains in
    -- flight: cancellation is delivered between chunks. A peer disconnect is
    -- expected when the cancellation test closes its HTTP response.
    sendSlowBody _ 0 = pure ()
    sendSlowBody client remaining = do
      sent <- try (Socket.sendAll client "x") :: IO (Either IOException ())
      case sent of
        Left _ -> pure ()
        Right () -> threadDelay slowBodyIntervalMicros >> sendSlowBody client (remaining - 1)
    response "/missing" = reply "404 Not Found" "missing"
    response "/error" = reply "500 Internal Server Error" "error"
    response "/partial" = "HTTP/1.1 200 OK\r\nContent-Length: 100\r\nConnection: close\r\n\r\nshort"
    response "/bad" = reply "200 OK" "wrong and longer than the expected output"
    response "/good" = reply "200 OK" "hello"
    response _ = reply "500 Internal Server Error" "unexpected request"
    reply status body =
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
