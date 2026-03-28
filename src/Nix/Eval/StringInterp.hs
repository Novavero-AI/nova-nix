-- | String interpolation evaluation for Nix.
--
-- Handles both regular strings (double-quoted) and indented strings
-- (double single-quoted).
-- Takes the evaluator as a parameter to break the import cycle with
-- @Nix.Eval@.
module Nix.Eval.StringInterp
  ( evalStringParts,
    evalIndStringParts,
    coerceToString,
    stripIndentation,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.Context (concatStrings)
import Nix.Eval.Types (Env, MonadEval (..), NixValue (..), StringContext, Thunk, attrSetLookup, emptyContext, typeName)
import Nix.Expr.Types (Expr, StringPart (..))

-- | The evaluator function, passed as a parameter to avoid cyclic imports.
type Eval m = Env -> Expr -> m NixValue

-- | Force a thunk to a value.
type Force m = Thunk -> m NixValue

-- | Apply a function value to an argument value.
type Apply m = NixValue -> NixValue -> m NixValue

-- | Evaluate the parts of a regular string (double-quoted).
-- Returns the concatenated text and the merged context from all parts.
evalStringParts :: (MonadEval m) => Eval m -> Force m -> Apply m -> Env -> [StringPart] -> m (Text, StringContext)
evalStringParts evalFn forceFn applyFn env parts = do
  chunks <- mapM (evalOnePart evalFn forceFn applyFn env) parts
  pure (concatStrings chunks)

-- | Evaluate the parts of an indented string (double single-quoted).
--
-- After interpolation, strips the common leading whitespace from all
-- non-empty lines (the standard Nix indented-string semantics).
-- Context is preserved through indentation stripping.
evalIndStringParts :: (MonadEval m) => Eval m -> Force m -> Apply m -> Env -> [StringPart] -> m (Text, StringContext)
evalIndStringParts evalFn forceFn applyFn env parts = do
  (raw, ctx) <- evalStringParts evalFn forceFn applyFn env parts
  pure (stripIndentation raw, ctx)

-- | Evaluate a single string part, returning its text and context.
evalOnePart :: (MonadEval m) => Eval m -> Force m -> Apply m -> Env -> StringPart -> m (Text, StringContext)
evalOnePart _ _ _ _ (StrLit txt) = pure (txt, emptyContext)
evalOnePart evalFn forceFn applyFn env (StrInterp expr) = do
  val <- evalFn env expr
  coerceToString forceFn applyFn val

-- | Coerce a Nix value to a string for interpolation.
--
-- Strict coercion: strings, ints, floats, paths, null, bools, and
-- attribute sets with @__toString@ or @outPath@.
-- Lists and functions without coercion metadata are errors.
-- Used by string interpolation (@"${...}"@) and @builtins.toString@.
coerceToString :: (MonadEval m) => Force m -> Apply m -> NixValue -> m (Text, StringContext)
coerceToString _ _ (VStr s ctx) = pure (s, ctx)
coerceToString _ _ (VInt n) = pure (T.pack (show n), emptyContext)
coerceToString _ _ (VFloat n) = pure (T.pack (show n), emptyContext)
coerceToString _ _ (VPath p) = pure (p, emptyContext)
coerceToString _ _ VNull = pure ("", emptyContext)
coerceToString _ _ (VBool True) = pure ("1", emptyContext)
coerceToString _ _ (VBool False) = pure ("", emptyContext)
-- Attribute sets: try __toString first, then outPath
coerceToString forceFn applyFn (VAttrs attrs) =
  case attrSetLookup "__toString" attrs of
    Just toStrThunk -> do
      toStrFn <- forceFn toStrThunk
      result <- applyFn toStrFn (VAttrs attrs)
      coerceToString forceFn applyFn result
    Nothing -> case attrSetLookup "outPath" attrs of
      Just outPathThunk -> do
        outPathVal <- forceFn outPathThunk
        coerceToString forceFn applyFn outPathVal
      Nothing ->
        throwEvalError "cannot coerce a set to a string (missing __toString or outPath)"
coerceToString _ _ other =
  throwEvalError ("cannot coerce " <> typeName other <> " to a string")

-- ---------------------------------------------------------------------------
-- Indented string whitespace stripping
-- ---------------------------------------------------------------------------

-- | Strip the common leading whitespace from an indented string.
--
-- Algorithm (matching C++ Nix):
-- 1. Split into lines.
-- 2. Find the minimum indentation of all non-empty lines (excluding
--    the first line, which has no leading whitespace in double single-quoted).
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
stripTrailingNewline t = case T.unsnoc t of
  Just (prefix, '\n') -> prefix
  _ -> t
