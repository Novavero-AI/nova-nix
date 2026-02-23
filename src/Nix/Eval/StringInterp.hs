-- | String interpolation evaluation for Nix.
--
-- Handles both regular strings (@"..."@) and indented strings (@''...''@).
-- Takes the evaluator as a parameter to break the import cycle with
-- 'Nix.Eval'.
module Nix.Eval.StringInterp
  ( evalStringParts,
    evalIndStringParts,
    coerceToString,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.Types (Env, NixValue (..), typeName)
import Nix.Expr.Types (Expr, StringPart (..))

-- | The evaluator function, passed as a parameter to avoid cyclic imports.
type Eval = Env -> Expr -> Either Text NixValue

-- | Evaluate the parts of a regular string (@"..."@).
evalStringParts :: Eval -> Env -> [StringPart] -> Either Text Text
evalStringParts evalFn env parts = do
  chunks <- mapM (evalOnePart evalFn env) parts
  pure (T.concat chunks)

-- | Evaluate the parts of an indented string (@''...''@).
--
-- After interpolation, strips the common leading whitespace from all
-- non-empty lines (the standard Nix indented-string semantics).
evalIndStringParts :: Eval -> Env -> [StringPart] -> Either Text Text
evalIndStringParts evalFn env parts = do
  raw <- evalStringParts evalFn env parts
  pure (stripIndentation raw)

-- | Evaluate a single string part.
evalOnePart :: Eval -> Env -> StringPart -> Either Text Text
evalOnePart _ _ (StrLit txt) = Right txt
evalOnePart evalFn env (StrInterp expr) = do
  val <- evalFn env expr
  coerceToString val

-- | Coerce a Nix value to a string for interpolation.
--
-- Nix coercion rules: strings pass through, integers and floats are
-- shown, paths pass through, null becomes the empty string.  Other
-- types (lists, sets, functions) cannot be coerced.
coerceToString :: NixValue -> Either Text Text
coerceToString val = case val of
  VStr s -> Right s
  VInt n -> Right (T.pack (show n))
  VFloat n -> Right (T.pack (show n))
  VPath p -> Right p
  VNull -> Right ""
  VBool True -> Right "1"
  VBool False -> Right ""
  other ->
    Left ("cannot coerce " <> typeName other <> " to a string")

-- ---------------------------------------------------------------------------
-- Indented string whitespace stripping
-- ---------------------------------------------------------------------------

-- | Strip the common leading whitespace from an indented string.
--
-- Algorithm (matching C++ Nix):
-- 1. Split into lines.
-- 2. Find the minimum indentation of all non-empty lines (excluding
--    the first line, which has no leading whitespace in @''...''@).
-- 3. Strip that many spaces/tabs from the front of each line.
-- 4. Drop a single leading newline if present.
-- 5. Drop a single trailing newline if present.
stripIndentation :: Text -> Text
stripIndentation raw
  | T.null raw = raw
  | otherwise =
      let withLeadingStripped = stripLeadingNewline raw
          lns = T.splitOn "\n" withLeadingStripped
          minIndent = minimumIndent lns
          stripped = map (stripPrefix minIndent) lns
          joined = T.intercalate "\n" stripped
       in stripTrailingNewline joined

-- | Count leading spaces on a line (tabs count as one space).
countIndent :: Text -> Int
countIndent = T.length . T.takeWhile (\c -> c == ' ' || c == '\t')

-- | Find the minimum indentation across all non-empty lines.
minimumIndent :: [Text] -> Int
minimumIndent lns =
  let nonEmpty = filter (not . T.null) lns
      indents = map countIndent nonEmpty
   in case indents of
        [] -> 0
        xs -> minimum xs

-- | Strip up to @n@ leading whitespace characters from a line.
stripPrefix :: Int -> Text -> Text
stripPrefix 0 t = t
stripPrefix n t = case T.uncons t of
  Just (c, rest)
    | c == ' ' || c == '\t' -> stripPrefix (n - 1) rest
  _ -> t

-- | Drop a single leading newline.
stripLeadingNewline :: Text -> Text
stripLeadingNewline t = case T.uncons t of
  Just ('\n', rest) -> rest
  _ -> t

-- | Drop a single trailing newline.
stripTrailingNewline :: Text -> Text
stripTrailingNewline t
  | T.null t = t
  | T.last t == '\n' = T.init t
  | otherwise = t
