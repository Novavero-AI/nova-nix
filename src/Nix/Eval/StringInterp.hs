-- | String interpolation evaluation for Nix.
--
-- Handles both regular strings (double-quoted) and indented strings
-- (double single-quoted).
-- Takes the evaluator as a parameter to break the import cycle with
-- @Nix.Eval@.
module Nix.Eval.StringInterp
  ( evalStringParts,
    evalIndStringParts,
    stripIndentedChunks,
    coerceToString,
    formatNixFloat,
    stripIndentation,
  )
where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.Context (concatStrings)
import Nix.Eval.Types (Env, MonadEval (..), NixValue (..), StringContext, Thunk, attrSetLookup, emptyContext, typeName)
import Nix.Expr.Types (Expr, StringPart (..))
import Numeric (showFFloat)

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
-- The common indentation is stripped from the LITERAL parts, computed BEFORE
-- interpolation, matching C++ Nix.  Interpolated values are opaque content, so
-- a @${...}@ whose value starts (or wraps onto a line) at column 0 does not
-- drag the common indent to 0.  The single leading newline is dropped; the
-- trailing newline is kept.
evalIndStringParts :: (MonadEval m) => Eval m -> Force m -> Apply m -> Env -> [StringPart] -> m (Text, StringContext)
evalIndStringParts evalFn forceFn applyFn env parts = do
  chunks <- mapM (evalOnePartTagged evalFn forceFn applyFn env) parts
  pure (stripIndentedChunks chunks)

-- | Evaluate one indented-string part, tagging it literal (@True@) or
-- interpolated (@False@) so 'stripIndentedChunks' strips only the literals.
evalOnePartTagged :: (MonadEval m) => Eval m -> Force m -> Apply m -> Env -> StringPart -> m (Bool, Text, StringContext)
evalOnePartTagged _ _ _ _ (StrLit t) = pure (True, t, emptyContext)
evalOnePartTagged evalFn forceFn applyFn env (StrInterp expr) = do
  val <- evalFn env expr
  (txt, ctx) <- coerceToString forceFn applyFn val
  pure (False, txt, ctx)

-- | Strip the common indentation from already-evaluated indented-string chunks.
-- Each chunk is @(isLiteral, text, context)@.  Indentation is computed and
-- stripped from the LITERAL chunks only — interpolated chunks are opaque content
-- — the single leading newline is dropped, and the trailing newline is kept.
-- This matches C++ Nix, which strips at the string-part level (so a multi-line
-- interpolated value cannot drag the common indent down).
stripIndentedChunks :: [(Bool, Text, StringContext)] -> (Text, StringContext)
stripIndentedChunks chunks =
  let stripped = dropLeadingNL (chunksStrip (chunksMinIndent chunks) chunks)
   in (T.concat (map snd stripped), mconcat [c | (_, _, c) <- chunks])
  where
    dropLeadingNL ((True, t) : rest) =
      (True, case T.uncons t of { Just ('\n', r) -> r; _ -> t }) : rest
    dropLeadingNL other = other

-- | Common indentation across the LITERAL chunks.  An interpolation at line
-- start fixes that line's indent at the preceding literal whitespace and counts
-- as content; whitespace-only lines do not contribute.
chunksMinIndent :: [(Bool, Text, StringContext)] -> Int
chunksMinIndent = result . foldl' stepChunk (True, 0, Nothing)
  where
    result (_, _, Nothing) = 0
    result (_, _, Just m) = m
    stepChunk (atStart, cur, mi) (isLit, t, _)
      | not isLit = if atStart then (False, cur, bump mi cur) else (False, cur, mi)
      | otherwise = T.foldl' stepChar (atStart, cur, mi) t
    stepChar (atStart, cur, mi) c
      | atStart && (c == ' ' || c == '\t') = (True, cur + 1, mi)
      | atStart && c == '\n' = (True, 0, mi)
      | atStart = (False, cur, bump mi cur)
      | c == '\n' = (True, 0, mi)
      | otherwise = (False, cur, mi)
    bump Nothing x = Just x
    bump (Just m) x = Just (min m x)

-- | Strip @n@ columns of leading indentation from each line of the literal
-- chunks; interpolated chunks are emitted verbatim and reset the line position.
chunksStrip :: Int -> [(Bool, Text, StringContext)] -> [(Bool, Text)]
chunksStrip n = go True 0
  where
    go _ _ [] = []
    go _ _ ((False, t, _) : rest) = (False, t) : go False 0 rest
    go atStart dropped ((True, t, _) : rest) =
      let (acc, atStart', dropped') = T.foldl' stepC ([], atStart, dropped) t
       in (True, T.pack (reverse acc)) : go atStart' dropped' rest
    stepC (acc, atStart, dropped) c
      | atStart && (c == ' ' || c == '\t') =
          if dropped < n then (acc, True, dropped + 1) else (c : acc, True, dropped + 1)
      | atStart && c == '\n' = ('\n' : acc, True, 0)
      | atStart = (c : acc, False, dropped)
      | c == '\n' = ('\n' : acc, True, 0)
      | otherwise = (c : acc, False, dropped)

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
coerceToString _ _ (VFloat n) = pure (formatNixFloat n, emptyContext)
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

-- | Format a float the way C++ Nix does: 6 fixed decimal places
-- (@std::to_string@), then strip trailing zeros and unnecessary
-- decimal point.  E.g. @1.0@ → @"1"@, @3.14@ → @"3.14"@.
formatNixFloat :: Double -> Text
formatNixFloat n
  | isNaN n = "nan"
  | isInfinite n = if n > 0 then "inf" else "-inf"
  | otherwise =
      let fixed = showFFloat (Just 6) n ""
       in T.pack (stripZeros fixed)
  where
    stripZeros s
      | '.' `elem` s = reverse (dropDot (dropWhile (== '0') (reverse s)))
      | otherwise = s
    dropDot ('.' : rest) = rest
    dropDot xs = xs

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
--
-- The trailing newline is KEPT — matching C++ Nix, which drops only the
-- leading newline of an indented string, not the trailing one.
stripIndentation :: Text -> Text
stripIndentation raw
  | T.null raw = raw
  | otherwise =
      let withLeadingStripped = stripLeadingNewline raw
          lns = T.splitOn "\n" withLeadingStripped
          minIndent = minimumIndent lns
          stripped = map (stripPrefix minIndent) lns
       in T.intercalate "\n" stripped

-- | Count leading spaces on a line (tabs count as one space).
countIndent :: Text -> Int
countIndent = T.length . T.takeWhile (\c -> c == ' ' || c == '\t')

-- | Find the minimum indentation across all non-blank lines.
-- Whitespace-only lines are treated as blank (infinite indent) per Nix semantics.
minimumIndent :: [Text] -> Int
minimumIndent lns =
  let nonBlank = filter (\t -> not (T.null t) && not (T.all isSpace t)) lns
      indents = map countIndent nonBlank
   in case indents of
        [] -> 0
        xs -> minimum xs
  where
    isSpace c = c == ' ' || c == '\t'

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
