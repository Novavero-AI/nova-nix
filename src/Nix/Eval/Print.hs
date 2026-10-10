-- | The value printers upstream writes values with.
--
-- @printValue@ (print.cc at Nix 2.24.9) quotes a value in a message.
-- @builtins.trace@ prints a value that is not a string through it, and a
-- type error that shows the offending value prints it after the type, as
-- in @expected a set but found an integer: 1@.
--
-- Upstream prints with one of two option sets: its defaults, which print
-- the whole value, and @errorPrintOptions@ (print-options.hh), which cut
-- an oversized value short so it cannot flood an error message.
--
-- In 'printValue', scalars render as upstream renders them.  Lists, sets
-- and functions render as a placeholder naming their type: upstream shows
-- a lambda's source position, which nova-nix's function values do not
-- carry, and marks each list element and attribute it has not evaluated
-- yet, which follows its own thunk allocation.
--
-- Upstream's @printAmbiguous@ (print-ambiguous.cc) writes what
-- @nix-instantiate --eval@ prints, and 'printAmbiguous' writes what
-- @nova-nix eval@ prints.  It renders to bytes: a string's payload is
-- bytes that need not be UTF-8, and upstream writes them as they are.
module Nix.Eval.Print
  ( PrintOptions (..),
    printValue,
    printAmbiguous,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Eval.StringInterp (formatXmlFloat)
import Nix.Eval.Types (NixValue (..), Thunk (..), attrSetToAscList, bytesToTextLossy, clistThunks, readThunkValue, typeName)

-- | Which of upstream's option sets a print follows.
data PrintOptions
  = -- | Upstream's defaults: the whole value, as @builtins.trace@ prints it.
    PrintInFull
  | -- | @errorPrintOptions@: a string is cut after its first 1024 bytes
    -- and the rest is counted instead of shown.
    PrintForError

-- | @errorPrintOptions.maxStringLength@ (print-options.hh at 2.24.9).
errorMaxStringLength :: Int
errorMaxStringLength = 1024

-- | Render a value as upstream's @printValue@ renders it under the given
-- options.
printValue :: PrintOptions -> NixValue -> Text
printValue options val = case val of
  VInt n -> T.pack (show n)
  VFloat f -> formatXmlFloat f
  VBool True -> "true"
  VBool False -> "false"
  VNull -> "null"
  VStr bytes _ -> printString options bytes
  VPath p -> p
  VList _ -> placeholder
  VAttrs _ -> placeholder
  VLambda {} -> placeholder
  VBuiltin _ _ -> placeholder
  VCompiledRegex _ -> placeholder
  where
    placeholder = "<<" <> typeName val <> ">>"

-- | Upstream's @printLiteralString@: quoted and escaped so the lexer reads
-- the same string back.  The cut and the count of what it left out are in
-- bytes, as upstream's are.  A message is 'Text', so a multi-byte
-- character the cut splits decodes to U+FFFD where upstream writes the
-- partial bytes raw.
printString :: PrintOptions -> ByteString -> Text
printString options bytes =
  bytesToTextLossy (BL.toStrict (BB.toLazyByteString (quotedBytes shown bytes))) <> elision
  where
    total = BS.length bytes
    shown = case options of
      PrintInFull -> total
      PrintForError -> min errorMaxStringLength total
    elision = case total - shown of
      0 -> ""
      1 -> " " <> marker "1 byte elided"
      left -> " " <> marker (T.pack (show left) <> " bytes elided")

-- | The first @count@ bytes as a string literal.
quotedBytes :: Int -> ByteString -> BB.Builder
quotedBytes count bytes = "\"" <> escapeBytes count bytes <> "\""

-- | The first @count@ bytes, escaped.  The test for @${@ looks one byte
-- past the cut, as upstream's does, so a @$@ shown last is escaped when
-- the @{@ after it was cut.
escapeBytes :: Int -> ByteString -> BB.Builder
escapeBytes count bytes = case BC.uncons bytes of
  Just (c, rest) | count > 0 -> escapeChar c rest <> escapeBytes (count - 1) rest
  _ -> mempty

escapeChar :: Char -> ByteString -> BB.Builder
escapeChar c rest = case c of
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  '$' | "{" `BS.isPrefixOf` rest -> "\\$"
  _ -> BB.char8 c

-- | Upstream brackets its markers in guillemets (U+00AB, U+00BB), written
-- as escapes to keep the source ASCII.
marker :: Text -> Text
marker inner = "\x00AB" <> inner <> "\x00BB"

-- ---------------------------------------------------------------------------
-- The eval result
-- ---------------------------------------------------------------------------

-- | Render a value as @nix-instantiate --eval@ prints its result, through
-- upstream's @printAmbiguous@ (print-ambiguous.cc at 2.24.9), without the
-- newline the command writes after it.  A string prints as its own bytes,
-- escaped as 'printValue' escapes it and never cut, and a float in a
-- default ostream's six-digit form, as 'formatXmlFloat' writes it.
printAmbiguous :: NixValue -> BB.Builder
printAmbiguous val = case val of
  VInt n -> BB.int64Dec n
  VFloat f -> TE.encodeUtf8Builder (formatXmlFloat f)
  VBool True -> "true"
  VBool False -> "false"
  VNull -> "null"
  VStr bytes _ -> quotedBytes (BS.length bytes) bytes
  VPath p -> TE.encodeUtf8Builder p
  VList cl -> bracketed "[" "]" (map (printElement . Thunk) (clistThunks cl))
  VAttrs attrs -> bracketed "{" "}" (map printAttribute (attrSetToAscList attrs))
  VLambda {} -> "<LAMBDA>"
  VBuiltin _ [] -> "<PRIMOP>"
  VBuiltin _ _ -> "<PRIMOP-APP>"
  -- A compiled pattern exists only among the arguments of a partially
  -- applied match or split, so it prints as that application does.
  VCompiledRegex _ -> "<PRIMOP-APP>"
  where
    printAttribute (name, thunk) =
      printIdentifier (TE.encodeUtf8 name) <> " = " <> printElement thunk <> ";"

-- | Upstream follows the opening bracket and every item with a space, so
-- an empty list is @[ ]@.
bracketed :: BB.Builder -> BB.Builder -> [BB.Builder] -> BB.Builder
bracketed open close items = open <> " " <> foldMap (<> " ") items <> close

-- | A list element or attribute value: its value when it has one, and
-- upstream's marker for a thunk when it was never forced.
printElement :: Thunk -> BB.Builder
printElement thunk = maybe "<CODE>" printAmbiguous (readThunkValue thunk)

-- | Upstream's @printIdentifier@ (print.cc), which @printAmbiguous@ writes
-- an attribute name through (nixexpr.cc's @operator<<@ on a symbol): a
-- name the lexer reads back as an identifier prints bare, and any other
-- prints as a string literal.  The test is on bytes, so a byte past ASCII
-- always quotes.
printIdentifier :: ByteString -> BB.Builder
printIdentifier name
  | isBareIdentifier name = BB.byteString name
  | otherwise = quotedBytes (BS.length name) name

isBareIdentifier :: ByteString -> Bool
isBareIdentifier name = case BC.uncons name of
  Just (first, rest) ->
    identifierStart first
      && BC.all identifierChar rest
      && name `notElem` reservedKeywords
  Nothing -> False
  where
    identifierStart c = isAsciiLower c || isAsciiUpper c || c == '_'
    identifierChar c = identifierStart c || isDigit c || c == '\'' || c == '-'

-- | Upstream's @isReservedKeyword@ set (print.cc).  @or@ is not in it: the
-- lexer accepts @or@ as an attribute name.
reservedKeywords :: [ByteString]
reservedKeywords = ["if", "then", "else", "assert", "with", "let", "in", "rec", "inherit"]
