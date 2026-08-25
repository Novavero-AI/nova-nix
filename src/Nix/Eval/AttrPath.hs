-- | Attribute-path selection, the machinery behind @build -A@.
--
-- An attribute path is a dotted sequence of names selected left to right
-- from an evaluated value: @stdenv.mkDerivation@ takes @mkDerivation@ out
-- of @stdenv@.  A component may be double-quoted to carry a literal dot,
-- as in @foo.\"bar.baz\"@.
--
-- Upstream tokenizes the same way (@parseAttrPath@,
-- src\/libexpr\/attr-path.cc), including three behaviours that read as
-- accidents but are load-bearing for compatibility: quotes concatenate
-- rather than delimit, so @foo\"bar\"@ is the single name @foobar@; the
-- final component is pushed only when non-empty, so a trailing dot and a
-- lone @\"\"@ both vanish instead of becoming a component; and an interior
-- empty component (a leading or doubled dot) survives tokenizing to be
-- rejected during selection, after the type of the value it would index.
--
-- Two upstream behaviours are deliberately absent.  A numeric component
-- indexes a list upstream (@-A foo.3.bar@); here it is an ordinary name,
-- because nothing this selects over is a list and upstream's
-- integer-or-name test defers to a C++ numeric parse whose accepted
-- spellings (a leading @+@, leading zeros) have not been checked against a
-- running Nix.  And a near-miss attribute name gets no @Did you mean@
-- line, which upstream renders from a Levenshtein search this evaluator
-- has no suggestion channel to carry.
--
-- The four messages otherwise match upstream byte for byte, diffed against
-- @nix-instantiate@ 2.33.2.
module Nix.Eval.AttrPath
  ( parseAttrPath,
    selectAttrPath,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval (MonadEval, NixValue (..), attrSetLookup, force)

-- ---------------------------------------------------------------------------
-- Tokenizing
-- ---------------------------------------------------------------------------

-- | Component separator.
attrSeparator :: Char
attrSeparator = '.'

-- | Opens and closes a component that may contain 'attrSeparator'.
quoteChar :: Char
quoteChar = '"'

quoteMark :: Text
quoteMark = T.singleton quoteChar

isDelimiter :: Char -> Bool
isDelimiter c = c == attrSeparator || c == quoteChar

-- | Split an attribute path into its components.
--
-- @Left@ carries a message ready for the user; the only way to get one is
-- a quote that is never closed.  An empty component is returned rather
-- than rejected, because upstream reports it against the value being
-- indexed and so cannot decide it here.
parseAttrPath :: Text -> Either Text [Text]
parseAttrPath path = go path T.empty []
  where
    go rest !current acc =
      let (plain, delimited) = T.break isDelimiter rest
          taken = current <> plain
       in case T.uncons delimited of
            Nothing
              | T.null taken -> Right (reverse acc)
              | otherwise -> Right (reverse (taken : acc))
            Just (c, more)
              | c == attrSeparator -> go more T.empty (taken : acc)
              | otherwise ->
                  let (quotedRun, closing) = T.breakOn quoteMark more
                   in case T.stripPrefix quoteMark closing of
                        Nothing -> Left (missingQuoteMessage path)
                        Just after -> go after (taken <> quotedRun) acc

-- ---------------------------------------------------------------------------
-- Selecting
-- ---------------------------------------------------------------------------

-- | Follow an attribute path into an evaluated value, forcing each step.
--
-- Only the components named are forced: selecting one attribute leaves its
-- siblings as thunks, so naming one package does not evaluate the rest of
-- a package set.
selectAttrPath :: (MonadEval m) => Text -> NixValue -> m (Either Text NixValue)
selectAttrPath path root = case parseAttrPath path of
  Left err -> pure (Left err)
  Right names -> walk names root
  where
    walk [] val = pure (Right val)
    walk (name : rest) val = case val of
      VAttrs attrs
        | T.null name -> pure (Left (emptyNameMessage path))
        | otherwise -> case attrSetLookup name attrs of
            Nothing -> pure (Left (notFoundMessage name path))
            Just thunk -> force thunk >>= walk rest
      -- Ordered as upstream orders it: the type of what is being indexed
      -- is reported before an empty component is complained about.
      _ -> pure (Left (notASetMessage path (describe val)))

-- | How a value is named in a selection error.
--
-- Deliberately not 'typeOfValue', which answers @builtins.typeOf@ and so
-- says @int@ and @lambda@.  These are upstream's @showType@ words, article
-- included, so the message reads as upstream's does; @null@ is the one
-- that takes no article.  Verified against @nix-instantiate@ 2.33.2.
describe :: NixValue -> Text
describe val = case val of
  VInt _ -> "an integer"
  VFloat _ -> "a float"
  VBool _ -> "a Boolean"
  VNull -> "null"
  VStr _ _ -> "a string"
  VPath _ -> "a path"
  VList _ -> "a list"
  VAttrs _ -> "a set"
  VDerivation _ -> "a set"
  VLambda {} -> "a function"
  VBuiltin _ _ -> "a function"
  VCompiledRegex _ -> "a function"

-- ---------------------------------------------------------------------------
-- Messages
-- ---------------------------------------------------------------------------

missingQuoteMessage :: Text -> Text
missingQuoteMessage path =
  "missing closing quote in selection path '" <> path <> "'"

emptyNameMessage :: Text -> Text
emptyNameMessage path =
  "empty attribute name in selection path '" <> path <> "'"

notFoundMessage :: Text -> Text -> Text
notFoundMessage name path =
  "attribute '" <> name <> "' in selection path '" <> path <> "' not found"

notASetMessage :: Text -> Text -> Text
notASetMessage path actual =
  "the expression selected by the selection path '"
    <> path
    <> "' should be a set but is "
    <> actual
