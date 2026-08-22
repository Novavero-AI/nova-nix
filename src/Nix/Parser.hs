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
-- parser with 'Data.Text' slicing to minimize allocations.
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
module Nix.Parser
  ( -- * Parsing
    parseNix,
    parseNixFile,

    -- * Encoding
    readFileAutoEncoding,

    -- * Errors
    ParseError (..),
  )
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Nix.Expr.ClosureTrim (trimClosures)
import Nix.Expr.Resolve (resolveRelativePaths, resolveVars)
import Nix.Expr.Types (Expr)
import Nix.Parser.Expr (parseTopLevel)
import Nix.Parser.Internal (ParseState (..), runParser)
import Nix.Parser.Lexer (tokenize)
import Nix.Parser.ParseError (ParseError (..))
import System.FilePath (takeDirectory)

-- | Parse a Nix expression from source text.
--
-- The input is the full file contents. The file name is used only for
-- error messages.  Strips a leading UTF-8 BOM if present - Windows
-- editors (Notepad, PowerShell) commonly add one.
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
parseNix :: FilePath -> Text -> Text -> Either ParseError Expr
parseNix baseDir fileName source = do
  tokens <- tokenize fileName (stripBOM source)
  let st = ParseState {psTokens = tokens, psFile = fileName}
  (expr, _remaining) <- runParser parseTopLevel st
  pure (resolveRelativePaths baseDir (trimClosures (resolveVars expr)))

-- | Strip a leading UTF-8 byte order mark (U+FEFF) if present.
stripBOM :: Text -> Text
stripBOM t = case T.uncons t of
  Just ('\xFEFF', rest) -> rest
  _ -> t

-- | Parse a @.nix@ file from disk.
-- Uses 'readFileAutoEncoding' to handle UTF-8, UTF-16 LE, and UTF-16 BE.
parseNixFile :: FilePath -> IO (Either ParseError Expr)
parseNixFile path = do
  source <- readFileAutoEncoding path
  pure $ parseNix (takeDirectory path) (T.pack path) source

-- ---------------------------------------------------------------------------
-- Encoding detection
-- ---------------------------------------------------------------------------

-- | Read a file, detecting encoding from its byte order mark.
--
-- Windows tools (PowerShell's @>@ operator, Notepad) commonly write
-- UTF-16 LE.  Nix files are normally UTF-8, but on a Windows-first
-- implementation we handle all three BOM variants:
--
-- * @FF FE@ is UTF-16 LE (PowerShell default)
-- * @FE FF@ is UTF-16 BE
-- * @EF BB BF@ is UTF-8 (BOM stripped)
-- * No BOM is UTF-8
readFileAutoEncoding :: FilePath -> IO Text
readFileAutoEncoding path = decodeAutoEncoding <$> BS.readFile path

-- | Decode bytes to 'Text' based on byte order mark.
decodeAutoEncoding :: BS.ByteString -> Text
decodeAutoEncoding bytes
  | BS.length bytes >= 2,
    BS.index bytes 0 == 0xFF,
    BS.index bytes 1 == 0xFE =
      TE.decodeUtf16LEWith lenientDecode (BS.drop 2 bytes)
  | BS.length bytes >= 2,
    BS.index bytes 0 == 0xFE,
    BS.index bytes 1 == 0xFF =
      TE.decodeUtf16BEWith lenientDecode (BS.drop 2 bytes)
  | BS.length bytes >= 3,
    BS.index bytes 0 == 0xEF,
    BS.index bytes 1 == 0xBB,
    BS.index bytes 2 == 0xBF =
      TE.decodeUtf8With lenientDecode (BS.drop 3 bytes)
  | otherwise =
      TE.decodeUtf8With lenientDecode bytes
