{-# LANGUAGE CPP #-}

-- | A builder's output, on its way to the build log.
--
-- Upstream gives the builder one channel for its standard output and
-- standard error, so what it wrote arrives in the order it wrote it,
-- and hands its logger each line as the line completes.  The line
-- discipline is pure ('feedBuildLog', 'endBuildLog'); the pipe and the
-- copy loop are the IO around it, and the logger ("Nix.Logger") decides
-- where a line goes.
module Nix.Builder.Log
  ( -- * Line discipline
    PendingLine,
    emptyPendingLine,
    feedBuildLog,
    endBuildLog,

    -- * Streaming
    openLogPipe,
    streamBuildLog,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (for_)
import Nix.Logger (Logger (..))
import qualified System.Process as Proc

#if defined(mingw32_HOST_OS)

import System.IO (Handle)

#else

import Control.Exception (onException)
import GHC.IO.FD (fdFD)
import GHC.IO.Handle.FD (handleToFd)
import System.IO (Handle, hClose)
import System.Posix.IO (FdOption (CloseOnExec), setFdOption)
import System.Posix.Types (Fd (..))

#endif

-- ---------------------------------------------------------------------------
-- Line discipline
-- ---------------------------------------------------------------------------

-- | The line a builder is writing, held as a terminal holds it: the
-- bytes left of the cursor, and the bytes from the cursor on, which the
-- next bytes written overwrite.  Upstream's @currentLogLine@ and
-- @currentLogLinePos@ (derivation-goal.hh at 2.24.9).
data PendingLine
  = PendingLine
      -- | The bytes left of the cursor in chunks, latest first, each
      -- shorter than the one after it ('pushChunk').
      ![BS.ByteString]
      -- | The bytes from the cursor to the end of the line.
      !BS.ByteString

-- | No line started yet.
emptyPendingLine :: PendingLine
emptyPendingLine = PendingLine [] BS.empty

-- | Split builder output into the lines it completes, in order, and the
-- line it leaves pending, by upstream's rule
-- (@DerivationGoal::handleChildOutput@, derivation-goal.cc at 2.24.9):
-- a line feed ends the line, a carriage return moves the cursor back to
-- the start of the line so what follows overwrites it, and any other
-- byte is written at the cursor.  A CRLF therefore ends a line without
-- leaving the CR in it, and a progress counter redrawn after CRs leaves
-- its last state.  The lines do not depend on where the output was
-- split into reads.
feedBuildLog :: PendingLine -> BS.ByteString -> ([BS.ByteString], PendingLine)
feedBuildLog = go []
  where
    go done !pending bytes =
      let (run, rest) = BS8.break isControl bytes
          !written = writeAtCursor run pending
       in case BS8.uncons rest of
            Nothing -> (reverse done, written)
            Just ('\r', after) -> go done (toLineStart written) after
            -- The line feed, the only other byte the break stops at.
            Just (_, after) ->
              let !line = lineBytes written
               in go (line : done) emptyPendingLine after
    isControl c = c == '\n' || c == '\r'

-- | The line pending when the builder's output ends, which upstream's
-- @handleEOF@ flushes when it holds any bytes.
endBuildLog :: PendingLine -> Maybe BS.ByteString
endBuildLog pending
  | BS.null line = Nothing
  | otherwise = Just line
  where
    line = lineBytes pending

-- | Write bytes at the cursor, overwriting as many as they replace.
writeAtCursor :: BS.ByteString -> PendingLine -> PendingLine
writeAtCursor !run pending@(PendingLine before fromCursor)
  | BS.null run = pending
  | otherwise = PendingLine (pushChunk run before) (BS.drop (BS.length run) fromCursor)

-- | Add the latest chunk left of the cursor, merging it into the chunks
-- before it while it is at least as long as they are.  A line that
-- arrives a few bytes per read is then held in a number of chunks
-- logarithmic in its length, rather than one per read.
pushChunk :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
pushChunk !newer chunks = case chunks of
  older : rest
    | BS.length newer >= BS.length older -> pushChunk (older <> newer) rest
  _ -> newer : chunks

-- | Move the cursor to the start of the line, keeping the line.
toLineStart :: PendingLine -> PendingLine
toLineStart pending = PendingLine [] (lineBytes pending)

-- | The whole line, cursor or not.
lineBytes :: PendingLine -> BS.ByteString
lineBytes (PendingLine before fromCursor) = BS.concat (reverse (fromCursor : before))

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

-- | A pipe to hand a builder as both its standard output and its
-- standard error, or the sandbox helper as its standard error, which the
-- helper makes the builder's output on Linux and copies a pipe of the
-- builder's to on macOS ('Nix.Builder.Sandbox.Spawn.runHelper'), as
-- @(read end, write end)@.
--
-- One channel keeps the order the builder wrote in.  Upstream's channel
-- is a pseudoterminal: the builder's standard error is its slave side,
-- and standard output is a copy of it (@openSlave@ in
-- local-derivation-goal.cc and @commonChildInit@ in child.cc at
-- 2.24.9).  A pipe is the channel "System.Process" offers on both
-- platforms, and what a builder can tell apart is @isatty@, which
-- upstream's terminal answers yes: a program that line-buffers or
-- colours its output only on a terminal block-buffers and stays plain
-- here.
--
-- On POSIX both ends are close-on-exec, so the builder holds the pipe
-- only as its standard output and standard error, as upstream's child
-- does after @closeMostFDs@: a daemon that closes those lets the build
-- finish.  A process another thread spawned between the pipe's creation
-- and the flags being set would still inherit it; nova-nix runs one
-- build at a time.  On Windows the ends stay inheritable, so a process
-- spawned while the pipe is open inherits them.  With one build at a
-- time that is the builder's own tree, which the job object the build
-- runs in already holds it open for until the tree exits.
openLogPipe :: IO (Handle, Handle)

#if defined(mingw32_HOST_OS)

openLogPipe = Proc.createPipe

#else

openLogPipe = do
  ends@(readEnd, writeEnd) <- Proc.createPipe
  (closeOnExec readEnd *> closeOnExec writeEnd)
    `onException` (hClose readEnd *> hClose writeEnd)
  pure ends
  where
    closeOnExec end = do
      fd <- handleToFd end
      setFdOption (Fd (fdFD fd)) CloseOnExec True

#endif

-- | Hand a builder's output to the logger ('logBuildLine') a line at a
-- time as each line completes, until every holder of the pipe's write
-- end has closed it, then the line left pending, if it holds any bytes.
--
-- Upstream first reads a line beginning @\@nix @ as a JSON log message
-- from the builder and does not log one that parses; here every line is
-- logged.  Upstream points @NIX_LOG_FD@ at standard error so stdenv
-- writes such lines, and nova-nix sets no @NIX_LOG_FD@.
--
-- The output is read to its end, so the builder never blocks on a full
-- pipe.
streamBuildLog :: Handle -> Logger -> IO ()
streamBuildLog fromBuilder logger = go emptyPendingLine
  where
    go pending = do
      chunk <- BS.hGetSome fromBuilder readSize
      if BS.null chunk
        then for_ (endBuildLog pending) (logBuildLine logger)
        else do
          let (complete, rest) = feedBuildLog pending chunk
          for_ complete (logBuildLine logger)
          go rest

-- | How much builder output one read takes at most.  The lines written
-- do not depend on it.
readSize :: Int
readSize = 65536
