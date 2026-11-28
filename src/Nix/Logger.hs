-- | Where the library reports what it is doing while it does it.
--
-- An operation with something to say as it runs (a wait on a lock, a
-- step of a collection, a warning, a line its builder wrote) says it to
-- the t'Logger' it was given and never writes to standard error itself,
-- so the program running it decides where each report goes.
-- 'handleLogger' writes them to a handle, a build line filtered for the
-- terminal as upstream's default logger filters one ('filterLogLine').
module Nix.Logger
  ( -- * Loggers
    Logger (..),
    logWarning,
    handleLogger,

    -- * Terminal filter
    EscapeFilter (..),
    filterLogLine,
    escapeFilterFor,
  )
where

import Control.Exception (IOException, catch)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Word (Word8)
import Nix.Environment (EnvLookup (..), lookupEnvBytes)
import System.IO (Handle, hIsTerminalDevice)

-- ---------------------------------------------------------------------------
-- Loggers
-- ---------------------------------------------------------------------------

-- | What an operation reports through.
data Logger = Logger
  { -- | A message for the user, one line without its line feed: a wait
    -- on a lock, a step of a collection, a warning ('logWarning').
    logMessage :: !(Text -> IO ()),
    -- | A line a builder wrote, as the line completed: its bytes,
    -- unfiltered, without the line feed ("Nix.Builder.Log").  A logger
    -- drops a line it cannot deliver rather than throw, since a build is
    -- not to fail over its log.
    logBuildLine :: !(BS.ByteString -> IO ())
  }

-- | Report a warning: the message after upstream's @warning: @ prefix.
logWarning :: Logger -> Text -> IO ()
logWarning logger = logMessage logger . ("warning: " <>)

-- | A logger writing each message and each build line to a handle, on a
-- line of its own.  A build line is filtered ('filterLogLine') and goes
-- out in one write, as upstream's default logger writes one to standard
-- error, and one the handle fails to take is dropped, as upstream's
-- logger ignores a failing write, so a reader that went away does not
-- fail the build.  A message is written as it is.  The command line
-- passes standard error, with the filter 'escapeFilterFor' finds for it.
handleLogger :: EscapeFilter -> Handle -> Logger
handleLogger escapes handle =
  Logger
    { logMessage = TIO.hPutStrLn handle,
      logBuildLine = \line -> BS.hPut handle (BS8.snoc (filterLogLine escapes line) '\n') `catch` dropLine
    }
  where
    dropLine :: IOException -> IO ()
    dropLine _ = pure ()

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

-- | A build-log line as upstream's default logger writes it to standard
-- error, at no width limit: an escape sequence is dropped unless the
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

-- | The filter upstream's default logger applies: colour survives only
-- on a terminal, with @TERM@ set to something other than @dumb@, and
-- neither @NO_COLOR@ nor @NOCOLOR@ set.  Upstream asks about its
-- standard error; this asks about the handle given.
escapeFilterFor :: Handle -> IO EscapeFilter
escapeFilterFor handle = do
  terminal <- hIsTerminalDevice handle
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
