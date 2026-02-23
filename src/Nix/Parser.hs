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

    -- * Errors
    ParseError (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Expr.Types (Expr)
import Nix.Parser.Expr (parseTopLevel)
import Nix.Parser.Internal (ParseState (..), runParser)
import Nix.Parser.Lexer (tokenize)
import Nix.Parser.ParseError (ParseError (..))

-- | Parse a Nix expression from source text.
--
-- The input is the full file contents. The file name is used only for
-- error messages.
parseNix :: Text -> Text -> Either ParseError Expr
parseNix fileName source = do
  tokens <- tokenize fileName source
  let st = ParseState {psTokens = tokens, psFile = fileName}
  (expr, _remaining) <- runParser parseTopLevel st
  pure expr

-- | Parse a @.nix@ file from disk.
parseNixFile :: FilePath -> IO (Either ParseError Expr)
parseNixFile path = do
  source <- TIO.readFile path
  let fileName = packFilePath path
  pure $ parseNix fileName source

-- | Convert a FilePath to Text for error reporting.
packFilePath :: FilePath -> Text
packFilePath = T.pack
