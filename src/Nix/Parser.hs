-- | Hand-rolled Nix language parser.
--
-- == How Nix parsing works
--
-- A @.nix@ file is a single expression. There is no top-level module
-- declaration, no import list, no @main@.  The file IS the expression.
--
-- @
-- -- default.nix
-- { pkgs ? import \<nixpkgs\> {} }:
-- pkgs.mkShell { buildInputs = [ pkgs.ghc ]; }
-- @
--
-- This is a lambda that takes an attribute set with a default, and
-- returns a call to @mkShell@. The whole file is one 'Expr'.
--
-- == Why hand-rolled
--
-- hnix used Megaparsec and it was 10x slower than C++ Nix (38% of runtime
-- in GC from parser allocations). We use a hand-rolled recursive descent
-- parser over a lexer that slices the source's bytes to minimize
-- allocations.
--
-- == Operator precedence (lowest to highest)
--
-- @
-- ->            (right)    implication
-- ||            (left)     logical or
-- &&            (left)     logical and
-- == !=         (none)     equality
-- \< \> \<= \>=      (none)     comparison
-- //            (right)    attribute set merge
-- !             (prefix)   logical not
-- ++ (list)     (right)    list concatenation
-- + -           (left)     addition, subtraction
-- * /           (left)     multiplication, division
-- -             (prefix)   arithmetic negation
-- f x           (left)     function application
-- e.a           (left)     attribute selection
-- @
--
-- == Lifecycle
--
-- Parsing is pure and touches none of the C data layer, so it runs with
-- or without 'Nix.Eval.Arena.arenaInit'.  Evaluating the resulting
-- 'Expr' does need the arena; see "Nix.Eval.Arena".
module Nix.Parser
  ( -- * Parsing
    parseNix,
    parseNixWithScope,
    parseNixFile,

    -- * Origins
    SourceOrigin (..),

    -- * Errors
    SourceError (..),
    ParseError (..),
  )
where

import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Nix.Expr.ClosureTrim (trimClosures)
import Nix.Expr.Resolve (resolveRelativePaths, resolveVars, staticGlobalNames)
import Nix.Expr.Types (Expr, SourceOrigin (..), sourceName)
import Nix.Parser.Expr (parseTopLevel)
import Nix.Parser.Internal (ParseState (..), runParser)
import Nix.Parser.Lexer (tokenize)
import Nix.Parser.ParseError (ParseError (..))
import System.FilePath (takeDirectory)

-- | Why source text did not become an expression to evaluate.
data SourceError
  = -- | The text is not a Nix expression.
    SyntaxError !ParseError
  | -- | A variable that no enclosing scope binds and no enclosing @with@
    -- could supply.  Upstream raises this while binding variables, before
    -- evaluating anything, so it fails the expression wherever the
    -- variable sits, and @builtins.tryEval@ cannot catch it.
    UndefinedVariable !Text
  deriving (Eq, Show)

-- | Parse a Nix expression from source bytes and bind its variables
-- against the root environment's names under the default policy
-- ('staticGlobalNames').
--
-- The input is the full file contents, as read from disk: a leading byte
-- order mark is read as 'sourceUtf8' describes, and every other byte is
-- lexed as it is, so a string literal's bytes are its value's.  The origin
-- names the source in error messages and decides what the positions
-- recorded in it report: a 'SourceFile' position names the file, as
-- upstream's do for a file it parses, and a 'SourceText' one names none,
-- as upstream's do for an @--expr@ string.
--
-- The base directory is what relative path literals resolve against, and it
-- is an argument here rather than something the evaluator supplies later
-- because a path literal names a location relative to the file it was
-- written in.  By the time a value is forced the evaluator can be evaluating
-- a different file, and a literal that resolved against that one would name
-- a file its author never wrote.  Upstream resolves in its parser for the
-- same reason.  Pass the directory holding the file; for source with no file
-- behind it (@--expr@) pass the working directory, which is what upstream
-- parses against.
parseNix :: FilePath -> SourceOrigin -> ByteString -> Either SourceError Expr
parseNix = parseNixWithScope staticGlobalNames

-- | 'parseNix' against the names the environment the source is evaluated in
-- binds, which is upstream's @parse@ given a static environment: the root
-- environment's under the policy in force ('Nix.Builtins.rootScopeNames'),
-- with a @scopedImport@ scope's attributes over them, which upstream binds
-- in a static environment of their own over the base one.  Every one binds
-- like a global, so a @with@ in the source cannot shadow it.
parseNixWithScope :: Set Text -> FilePath -> SourceOrigin -> ByteString -> Either SourceError Expr
parseNixWithScope scope baseDir origin source = do
  utf8 <- first SyntaxError (sourceUtf8 (sourceName origin) source)
  tokens <- first SyntaxError (tokenize (sourceName origin) utf8)
  let st = ParseState {psTokens = tokens, psOrigin = origin}
  (expr, _remaining) <- first SyntaxError (runParser parseTopLevel st)
  resolved <- first UndefinedVariable (resolveVars scope expr)
  pure (resolveRelativePaths baseDir (trimClosures resolved))

-- | Parse a @.nix@ file from disk.
parseNixFile :: FilePath -> IO (Either SourceError Expr)
parseNixFile path = parseNix (takeDirectory path) (SourceFile (T.pack path)) <$> BS.readFile path

-- ---------------------------------------------------------------------------
-- Encoding detection
-- ---------------------------------------------------------------------------

-- | The source as the lexer reads it: UTF-8 bytes.  A leading byte order
-- mark decides the encoding, and nothing else does:
--
-- * @FF FE@ is UTF-16 LE (what PowerShell's @>@ writes) and @FE FF@ is
--   UTF-16 BE, either transcoded to UTF-8
-- * @EF BB BF@ is UTF-8, the mark dropped
-- * no mark is UTF-8, every byte kept, a malformed sequence included
--
-- Upstream recognizes no mark: it lexes a file's bytes as they are
-- (@EvalState::parseExprFromFile@, eval.cc at 2.24.9), so a file opening
-- with one fails to parse there.  Reading them is for the Windows tools
-- that write them.
sourceUtf8 :: Text -> ByteString -> Either ParseError ByteString
sourceUtf8 name bytes
  | Just body <- BS.stripPrefix utf16LeMark bytes = utf16ToUtf8 LittleEndian name body
  | Just body <- BS.stripPrefix utf16BeMark bytes = utf16ToUtf8 BigEndian name body
  | Just body <- BS.stripPrefix utf8Mark bytes = Right body
  | otherwise = Right bytes

-- | The byte order marks 'sourceUtf8' reads.
utf16LeMark, utf16BeMark, utf8Mark :: ByteString
utf16LeMark = BS.pack [0xFF, 0xFE]
utf16BeMark = BS.pack [0xFE, 0xFF]
utf8Mark = BS.pack [0xEF, 0xBB, 0xBF]

-- | Which byte of a UTF-16 code unit comes first.
data ByteOrder = LittleEndian | BigEndian

-- | A UTF-16 body as UTF-8.  An unpaired surrogate and an odd final byte
-- have no UTF-8 spelling, and replacing either would change the source, so
-- each is a parse error at the character it stands at, the line and column
-- counted as the lexer counts them.
utf16ToUtf8 :: ByteOrder -> Text -> ByteString -> Either ParseError ByteString
utf16ToUtf8 order name = go mempty 1 1
  where
    go !built !line !col units = case codeUnit units of
      Nothing
        | BS.null units -> Right (BL.toStrict (BB.toLazyByteString built))
        | otherwise -> malformed line col "UTF-16 source with an odd number of bytes"
      Just (unit, rest)
        | isHighSurrogate unit,
          Just (low, afterPair) <- codeUnit rest,
          isLowSurrogate low ->
            go (built <> utf8CodePoint (surrogatePair unit low)) line (col + 1) afterPair
        | isSurrogate unit -> malformed line col "unpaired UTF-16 surrogate"
        | unit == lineFeed -> go (built <> utf8CodePoint unit) (line + 1) 1 rest
        | otherwise -> go (built <> utf8CodePoint unit) line (col + 1) rest
    codeUnit bytes = do
      (leading, afterLeading) <- BS.uncons bytes
      (trailing, afterTrailing) <- BS.uncons afterLeading
      pure $ case order of
        LittleEndian -> (widen trailing `shiftL` 8 .|. widen leading, afterTrailing)
        BigEndian -> (widen leading `shiftL` 8 .|. widen trailing, afterTrailing)
    widen :: Word8 -> Word32
    widen = fromIntegral
    malformed line col message =
      Left ParseError {peFile = name, peLine = line, peCol = col, peMessage = message}
    lineFeed = 0x0A

-- | UTF-16 surrogates: a high one (D800-DBFF) followed by a low one
-- (DC00-DFFF) spells one code point past U+FFFF.
isHighSurrogate, isLowSurrogate, isSurrogate :: Word32 -> Bool
isHighSurrogate unit = unit >= 0xD800 && unit < 0xDC00
isLowSurrogate unit = unit >= 0xDC00 && unit < 0xE000
isSurrogate unit = unit >= 0xD800 && unit < 0xE000

-- | The code point a high and a low surrogate spell.
surrogatePair :: Word32 -> Word32 -> Word32
surrogatePair high low = 0x10000 + (high - 0xD800) `shiftL` 10 + (low - 0xDC00)

-- | The UTF-8 bytes of a code point below U+110000 that is not a
-- surrogate, which is every one 'utf16ToUtf8' spells.
utf8CodePoint :: Word32 -> BB.Builder
utf8CodePoint point
  | point < 0x80 = BB.word8 (fromIntegral point)
  | point < 0x800 = lead 0xC0 6 <> continuation 0
  | point < 0x10000 = lead 0xE0 12 <> continuation 6 <> continuation 0
  | otherwise = lead 0xF0 18 <> continuation 12 <> continuation 6 <> continuation 0
  where
    lead marker bits = BB.word8 (marker .|. fromIntegral (point `shiftR` bits))
    continuation bits = BB.word8 (0x80 .|. (fromIntegral (point `shiftR` bits) .&. 0x3F))
