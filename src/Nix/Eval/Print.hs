-- | The value printer upstream quotes values with, @printValue@ (print.cc
-- at Nix 2.24.9).  @builtins.trace@ prints a value that is not a string
-- through it, and a type error that shows the offending value prints it
-- after the type, as in @expected a set but found an integer: 1@.
--
-- Upstream prints with one of two option sets: its defaults, which print
-- the whole value, and @errorPrintOptions@ (print-options.hh), which cut
-- an oversized value short so it cannot flood an error message.
--
-- Scalars render as upstream renders them.  Lists, sets and functions
-- render as a placeholder naming their type: upstream shows a lambda's
-- source position, which nova-nix's function values do not carry, and
-- marks each list element and attribute it has not evaluated yet, which
-- follows its own thunk allocation.
module Nix.Eval.Print
  ( PrintOptions (..),
    printValue,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.StringInterp (formatXmlFloat)
import Nix.Eval.Types (NixValue (..), bytesToTextLossy, typeName)

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
  bytesToTextLossy (BL.toStrict (BB.toLazyByteString quoted)) <> elision
  where
    total = BS.length bytes
    shown = case options of
      PrintInFull -> total
      PrintForError -> min errorMaxStringLength total
    quoted = "\"" <> escapeBytes shown bytes <> "\""
    elision = case total - shown of
      0 -> ""
      1 -> " " <> marker "1 byte elided"
      left -> " " <> marker (T.pack (show left) <> " bytes elided")

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
