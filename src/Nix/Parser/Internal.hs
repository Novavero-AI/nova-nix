-- | Parser infrastructure: newtype, state, combinators.
--
-- Pure recursive descent on a token list. No Megaparsec, no Parsec.
-- Same cursor-passing pattern as nova-cache's NAR parser.
module Nix.Parser.Internal
  ( -- * Parser type
    Parser (..),
    ParseState (..),

    -- * Running
    runParser,

    -- * Combinators
    peek,
    peekMaybe,
    advance,
    expect,
    match,
    tryParser,
    pMany,
    atEnd,

    -- * Specific expects
    expectIdent,

    -- * Errors
    parseError,

    -- * Token display
    showToken,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Parser.Lexer (Located (..), Token (..))
import Nix.Parser.ParseError (ParseError (..))

-- | Remaining tokens plus file name for error messages.
data ParseState = ParseState
  { psTokens :: ![Located],
    psFile :: !Text
  }

-- | A parser that consumes tokens and produces a value or an error.
newtype Parser a = Parser
  { unParser :: ParseState -> Either ParseError (a, ParseState)
  }

instance Functor Parser where
  fmap f (Parser g) = Parser $ \st -> case g st of
    Left err -> Left err
    Right (val, rest) -> Right (f val, rest)

instance Applicative Parser where
  pure val = Parser $ \st -> Right (val, st)
  Parser pf <*> Parser pa = Parser $ \st -> case pf st of
    Left err -> Left err
    Right (f, st2) -> case pa st2 of
      Left err -> Left err
      Right (a, st3) -> Right (f a, st3)

instance Monad Parser where
  Parser pa >>= f = Parser $ \st -> case pa st of
    Left err -> Left err
    Right (a, st2) -> unParser (f a) st2

-- | Run a parser on a token list.
runParser :: Parser a -> ParseState -> Either ParseError (a, ParseState)
runParser = unParser

-- | Peek at the next token without consuming it.
peek :: Parser Token
peek = Parser $ \st -> case psTokens st of
  (Located _ _ tok : _) -> Right (tok, st)
  [] -> Left (makeEOF st)

-- | Peek at the next token, returning 'Nothing' at end of input.
peekMaybe :: Parser (Maybe Token)
peekMaybe = Parser $ \st -> case psTokens st of
  (Located _ _ tok : _) -> Right (Just tok, st)
  [] -> Right (Nothing, st)

-- | Consume the current token and return it.
advance :: Parser Token
advance = Parser $ \st -> case psTokens st of
  (Located _ _ tok : rest) -> Right (tok, st {psTokens = rest})
  [] -> Left (makeEOF st)

-- | Consume and assert the next token matches.
expect :: Token -> Parser ()
expect expected = Parser $ \st -> case psTokens st of
  (Located ln col tok : rest)
    | tok == expected -> Right ((), st {psTokens = rest})
    | otherwise ->
        Left
          ParseError
            { peFile = psFile st,
              peLine = ln,
              peCol = col,
              peMessage =
                "expected " <> showToken expected <> " but got " <> showToken tok
            }
  [] ->
    Left
      ParseError
        { peFile = psFile st,
          peLine = 0,
          peCol = 0,
          peMessage = "expected " <> showToken expected <> " but got end of input"
        }

-- | If the next token matches, consume it and return 'True'.
match :: Token -> Parser Bool
match tok = Parser $ \st -> case psTokens st of
  (Located _ _ t : rest)
    | t == tok -> Right (True, st {psTokens = rest})
  _ -> Right (False, st)

-- | Backtracking: try a parser, restore state on failure.
tryParser :: Parser a -> Parser (Maybe a)
tryParser (Parser p) = Parser $ \st -> case p st of
  Left _ -> Right (Nothing, st)
  Right (val, st2) -> Right (Just val, st2)

-- | Parse zero or more occurrences.
pMany :: Parser a -> Parser [a]
pMany p = go []
  where
    go acc = do
      result <- tryParser p
      case result of
        Nothing -> pure (reverse acc)
        Just val -> go (val : acc)

-- | Check if we've consumed all tokens.
atEnd :: Parser Bool
atEnd = Parser $ \st -> case psTokens st of
  (Located _ _ TokEOF : _) -> Right (True, st)
  [] -> Right (True, st)
  _ -> Right (False, st)

-- | Raise a parse error at the current position.
parseError :: Text -> Parser a
parseError msg = Parser $ \st -> case psTokens st of
  (Located ln col _ : _) ->
    Left ParseError {peFile = psFile st, peLine = ln, peCol = col, peMessage = msg}
  [] -> Left (makeEOF st)

-- | Consume and return an identifier, or fail.
expectIdent :: Parser Text
expectIdent = Parser $ \st -> case psTokens st of
  (Located _ _ (TokIdent name) : rest) -> Right (name, st {psTokens = rest})
  (Located ln col tok : _) ->
    Left
      ParseError
        { peFile = psFile st,
          peLine = ln,
          peCol = col,
          peMessage = "expected identifier but got " <> showToken tok
        }
  [] ->
    Left
      ParseError
        { peFile = psFile st,
          peLine = 0,
          peCol = 0,
          peMessage = "expected identifier but got end of input"
        }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

makeEOF :: ParseState -> ParseError
makeEOF st =
  ParseError
    { peFile = psFile st,
      peLine = 0,
      peCol = 0,
      peMessage = "unexpected end of input"
    }

-- | Render a token as the human-readable phrase used in parse-error messages.
showToken :: Token -> Text
showToken TokEOF = "end of input"
showToken TokIf = "'if'"
showToken TokThen = "'then'"
showToken TokElse = "'else'"
showToken TokLet = "'let'"
showToken TokIn = "'in'"
showToken TokWith = "'with'"
showToken TokAssert = "'assert'"
showToken TokRec = "'rec'"
showToken TokInherit = "'inherit'"
showToken TokTrue = "'true'"
showToken TokFalse = "'false'"
showToken TokNull = "'null'"
showToken (TokIdent name) = "identifier '" <> name <> "'"
showToken (TokInt n) = "integer " <> T.pack (show n)
showToken (TokFloat d) = "float " <> T.pack (show d)
showToken (TokUri u) = "URI '" <> u <> "'"
showToken (TokPath p) = "path '" <> p <> "'"
showToken (TokSearchPath p) = "search path '<" <> p <> ">'"
showToken TokStringOpen = "'\"'"
showToken TokStringClose = "'\"'"
showToken TokIndStringOpen = "'''"
showToken TokIndStringClose = "'''"
showToken (TokStringLit _) = "string literal"
showToken (TokStringEsc _) = "string escape"
showToken TokInterpOpen = "'${'"
showToken TokInterpClose = "'}'"
showToken TokPlus = "'+'"
showToken TokMinus = "'-'"
showToken TokStar = "'*'"
showToken TokSlash = "'/'"
showToken TokConcat = "'++'"
showToken TokUpdate = "'//'"
showToken TokNot = "'!'"
showToken TokAnd = "'&&'"
showToken TokOr = "'||'"
showToken TokImpl = "'->'"
showToken TokEq = "'=='"
showToken TokNeq = "'!='"
showToken TokLt = "'<'"
showToken TokLte = "'<='"
showToken TokGt = "'>'"
showToken TokGte = "'>='"
showToken TokQuestion = "'?'"
showToken TokDot = "'.'"
showToken TokEllipsis = "'...'"
showToken TokAt = "'@'"
showToken TokColon = "':'"
showToken TokSemicolon = "';'"
showToken TokAssign = "'='"
showToken TokComma = "','"
showToken TokLParen = "'('"
showToken TokRParen = "')'"
showToken TokLBrace = "'{'"
showToken TokRBrace = "'}'"
showToken TokLBracket = "'['"
showToken TokRBracket = "']'"
