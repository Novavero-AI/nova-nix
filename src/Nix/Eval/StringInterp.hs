-- | String coercion and indented-string whitespace stripping.
--
-- Provides 'coerceToString' (string interpolation and @builtins.toString@) and
-- 'stripIndentedChunks' (the indented-string indentation algorithm, applied by
-- the bytecode evaluator).  Force/apply are passed in as parameters to break the
-- import cycle with @Nix.Eval@.
module Nix.Eval.StringInterp
  ( stripIndentedChunks,
    coerceToString,
    formatNixFloat,
  )
where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.Types (MonadEval (..), NixValue (..), StringContext, Thunk, attrSetLookup, emptyContext, typeName)
import Numeric (showFFloat)

-- | Force a thunk to a value.
type Force m = Thunk -> m NixValue

-- | Apply a function value to an argument value.
type Apply m = NixValue -> NixValue -> m NixValue

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
