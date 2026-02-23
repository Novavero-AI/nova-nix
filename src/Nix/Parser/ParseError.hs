-- | Shared parse error type used by both lexer and parser.
module Nix.Parser.ParseError
  ( ParseError (..),
  )
where

import Data.Text (Text)

-- | A parse error with position and message.
data ParseError = ParseError
  { peFile :: !Text,
    peLine :: !Int,
    peCol :: !Int,
    peMessage :: !Text
  }
  deriving (Eq, Show)
