{-# LANGUAGE CPP #-}

-- | A builder's output, on its way to the build log.
--
-- Upstream gives the builder one channel for its standard output and
-- standard error, so what it wrote arrives in the order it wrote it,
-- and its default logger prints each line to standard error as the line
-- completes, filtered for the terminal.  The line discipline and the
-- filter are pure ('feedBuildLog', 'endBuildLog', 'filterLogLine'); the
-- pipe and the copy loop are the IO around them.
module Nix.Builder.Log
  ( -- * Line discipline
    PendingLine,
    emptyPendingLine,
    feedBuildLog,
    endBuildLog,

    -- * Terminal filter
    EscapeFilter (..),
    filterLogLine,

    -- * Streaming
    openLogPipe,
    streamBuildLog,
  )
where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Word (Word8)
import Nix.Environment (EnvLookup (..), lookupEnvBytes)
import qualified System.Process as Proc

#if defined(mingw32_HOST_OS)

import Control.Exception (IOException, catch)
import System.IO (Handle, hIsTerminalDevice)

#else

import Control.Exception (IOException, catch, onException)
import GHC.IO.FD (fdFD)
import GHC.IO.Handle.FD (handleToFd)
import System.IO (Handle, hClose, hIsTerminalDevice)
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
-- Terminal filter
-- ---------------------------------------------------------------------------

-- | Which escape sequences a build-log line keeps on its way to the user.
data EscapeFilter
  = -- | Keep colour (an SGR sequence, CSI ending in @m@) and drop the rest,
    -- for a terminal that renders it.
    KeepColour
  | -- | Drop every escape sequence.
    DropEscapes
  deriving (Eq, Show)

-- | A build-log line as upstream's logger writes it to standard error
-- (@filterANSIEscapes@, terminal.cc at 2.24.9, at the unlimited width
-- @SimpleLogger::log@ passes): an escape sequence is dropped unless the
-- filter keeps it, a tab becomes spaces up to the next multiple of
-- eight columns, a bell or carriage return is dropped, and every other
-- byte is kept.  A column is one character as upstream groups one: a
-- UTF-8 lead byte with as many continuation bytes as it announces and
-- as follow it, or any other byte alone.
filterLogLine :: EscapeFilter -> BS.ByteString -> BS.ByteString
filterLogLine escapes = BL.toStrict . BB.toLazyByteString . go 0
  where
    go :: Int -> BS.ByteString -> BB.Builder
    go !column bytes =
      let (plain, rest) = BS.span isPlainAscii bytes
       in BB.byteString plain <> special (column + BS.length plain) rest
    special column bytes = case BS.uncons bytes of
      Nothing -> mempty
      Just (byte, rest)
        | byte == escapeByte ->
            let (escape, after) = escapeSequence rest
                escapeBytes = BS.cons byte (BS.take (BS.length rest - BS.length after) rest)
             in kept escape escapeBytes <> go column after
        | byte == tabByte ->
            let width = tabStop - column `mod` tabStop
             in BB.byteString (BS.replicate width spaceByte) <> go (column + width) rest
        | byte == bellByte || byte == carriageReturnByte -> go column rest
        | otherwise ->
            let continued = BS.takeWhile isContinuation (BS.take (continuationCount byte) rest)
             in BB.word8 byte <> BB.byteString continued <> go (column + 1) (BS.drop (BS.length continued) rest)
    kept escape escapeBytes
      | escapes == KeepColour && escape == ColourEscape = BB.byteString escapeBytes
      | otherwise = mempty
    isPlainAscii byte =
      byte < asciiLimit
        && byte /= escapeByte
        && byte /= tabByte
        && byte /= bellByte
        && byte /= carriageReturnByte

-- | What an escape sequence does, as far as the filter cares.
data EscapeKind = ColourEscape | OtherEscape
  deriving (Eq)

-- | The kind of escape the bytes after an ESC begin, and the bytes after
-- its sequence, which upstream's filter reads as a CSI's parameter,
-- intermediate and final bytes, or the one byte a two-byte escape takes,
-- or nothing.
escapeSequence :: BS.ByteString -> (EscapeKind, BS.ByteString)
escapeSequence bytes = case BS.uncons bytes of
  Just (byte, afterBracket)
    | byte == csiByte ->
        let afterParameters = BS.dropWhile (within csiParameterBytes) afterBracket
            afterIntermediates = BS.dropWhile (within csiIntermediateBytes) afterParameters
         in case BS.uncons afterIntermediates of
              Just (final, after)
                | within csiFinalBytes final -> (if final == sgrFinalByte then ColourEscape else OtherEscape, after)
              _ -> (OtherEscape, afterIntermediates)
    | within escapeFinalBytes byte -> (OtherEscape, afterBracket)
  _ -> (OtherEscape, bytes)

-- | How many continuation bytes a UTF-8 lead byte announces.
continuationCount :: Word8 -> Int
continuationCount lead
  | lead .&. 0xE0 == 0xC0 = 1
  | lead .&. 0xF0 == 0xE0 = 2
  | lead .&. 0xF8 == 0xF0 = 3
  | otherwise = 0

-- | Whether a byte continues a UTF-8 character.
isContinuation :: Word8 -> Bool
isContinuation byte = byte .&. 0xC0 == 0x80

-- | Whether a byte lies in an inclusive range.
within :: (Word8, Word8) -> Word8 -> Bool
within (low, high) byte = low <= byte && byte <= high

-- | The control bytes the filter treats specially, and the space a tab
-- expands to.
escapeByte, tabByte, bellByte, carriageReturnByte, spaceByte :: Word8
escapeByte = 0x1B
tabByte = 0x09
bellByte = 0x07
carriageReturnByte = 0x0D
spaceByte = 0x20

-- | The byte opening a control sequence after an ESC (@[@), and the
-- final byte of a colour one (@m@).
csiByte, sgrFinalByte :: Word8
csiByte = 0x5B
sgrFinalByte = 0x6D

-- | The ECMA-48 byte ranges upstream's filter reads a sequence by.
csiParameterBytes, csiIntermediateBytes, csiFinalBytes, escapeFinalBytes :: (Word8, Word8)
csiParameterBytes = (0x30, 0x3F)
csiIntermediateBytes = (0x20, 0x2F)
csiFinalBytes = (0x40, 0x7E)
escapeFinalBytes = (0x40, 0x5F)

-- | The first byte that is not ASCII.
asciiLimit :: Word8
asciiLimit = 0x80

-- | The columns between tab stops.
tabStop :: Int
tabStop = 8

-- | The filter upstream's logger applies (@isTTY@, terminal.cc at
-- 2.24.9): colour survives only on a terminal, with @TERM@ set to
-- something other than @dumb@, and neither @NO_COLOR@ nor @NOCOLOR@
-- set.  Upstream asks about its standard error; this asks about the
-- handle the log goes to, which is standard error by default.
escapeFilterFor :: Handle -> IO EscapeFilter
escapeFilterFor buildLog = do
  terminal <- hIsTerminalDevice buildLog
  term <- lookupEnvBytes termVar
  noColour <- traverse lookupEnvBytes noColourVars
  pure $
    if terminal && rendersColour term && all (== EnvUnset) noColour
      then KeepColour
      else DropEscapes
  where
    rendersColour term = case term of
      EnvUnset -> False
      EnvValue value -> value /= dumbTerm
      EnvUnpairedSurrogate -> True

-- | The variable naming the terminal type, and the type upstream reads
-- as rendering no colour.
termVar, dumbTerm :: BS.ByteString
termVar = "TERM"
dumbTerm = "dumb"

-- | The variables whose presence, whatever their value, turns colour off.
noColourVars :: [BS.ByteString]
noColourVars = ["NO_COLOR", "NOCOLOR"]

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

-- | A pipe to hand a builder as both its standard output and its
-- standard error, as @(read end, write end)@.
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

-- | Copy a builder's output to the build log a line at a time as each
-- line completes, until every holder of the pipe's write end has
-- closed it.
--
-- A line goes out filtered ('filterLogLine', under 'escapeFilterFor'
-- the log) in one write ending in a line feed, as upstream's default
-- logger writes a build-log line to standard error (@SimpleLogger@,
-- logging.cc at 2.24.9).  Upstream first reads a line beginning
-- @\@nix @ as a JSON log message from the builder and does not print
-- one that parses (@handleJSONLogMessage@); here every line is printed.
-- Upstream points @NIX_LOG_FD@ at standard error so stdenv writes such
-- lines, and nova-nix sets no @NIX_LOG_FD@.
--
-- A line the log fails to take is dropped and the build goes on, as
-- upstream's @writeToStderr@ ignores a failing write (logging.cc), so a
-- reader of standard error that went away does not fail the build.  The
-- output is read to its end either way, so the builder never blocks on
-- a full pipe.
streamBuildLog :: Handle -> Handle -> IO ()
streamBuildLog fromBuilder buildLog = do
  escapes <- escapeFilterFor buildLog
  let writeLine line = BS.hPut buildLog (BS8.snoc (filterLogLine escapes line) '\n') `catch` dropLine
      go pending = do
        chunk <- BS.hGetSome fromBuilder readSize
        if BS.null chunk
          then for_ (endBuildLog pending) writeLine
          else do
            let (complete, rest) = feedBuildLog pending chunk
            for_ complete writeLine
            go rest
  go emptyPendingLine
  where
    dropLine :: IOException -> IO ()
    dropLine _ = pure ()

-- | How much builder output one read takes at most.  The lines written
-- do not depend on it.
readSize :: Int
readSize = 65536
