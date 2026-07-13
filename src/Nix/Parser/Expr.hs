-- | Recursive descent expression parser for the Nix language.
--
-- 14 precedence levels, lowest (top) to highest (deepest):
--
-- @
-- lambda / -> / || / && / == != / < > <= >= / //
-- ! / + - / * \\/ / ++ / ? / negate / application / selection
-- @
--
-- Close to the C++ Nix parser (parser.y), with two inert deviations: here
-- @++@ binds looser than multiplication\/addition (parser.y binds it tighter)
-- and unary negate binds looser than @?@ (parser.y makes negate tightest).
-- Both only affect mixed expressions that are type errors regardless, so no
-- program that type-checks parses differently.
-- Entirely pure. No IO, no Megaparsec, no Parsec.
module Nix.Parser.Expr
  ( -- * Entry point
    parseTopLevel,

    -- * Expression parsers (exported for testing)
    parseExpr,
  )
where

import Control.Monad (foldM)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Expr.Types
import Nix.Parser.Internal
import Nix.Parser.Lexer (Token (..))

-- ---------------------------------------------------------------------------
-- Top level
-- ---------------------------------------------------------------------------

-- | Parse a complete Nix file (one expression, then EOF).
parseTopLevel :: Parser Expr
parseTopLevel = do
  expr <- parseExpr
  done <- atEnd
  if done
    then pure expr
    else do
      tok <- peek
      parseError ("unexpected " <> showToken tok <> " after expression")

-- ---------------------------------------------------------------------------
-- Expression (entry point for precedence climbing)
-- ---------------------------------------------------------------------------

-- | Parse a full expression. Tries lambda first, falls back to implication.
parseExpr :: Parser Expr
parseExpr = do
  tok <- peek
  case tok of
    TokLBrace -> parseLambdaOrAttrSet
    TokIdent _ -> parseLambdaOrIdent
    _ -> parseImplication

-- ---------------------------------------------------------------------------
-- Lambda detection
-- ---------------------------------------------------------------------------

-- | Identifier at start: could be @x: body@ or @name\@{...}: body@ or just expr.
parseLambdaOrIdent :: Parser Expr
parseLambdaOrIdent = do
  result <- tryParser trySimpleLambda
  case result of
    Just lam -> pure lam
    Nothing -> do
      result2 <- tryParser tryNamedSetLambda
      maybe parseImplication pure result2

-- | Try @x: body@
trySimpleLambda :: Parser Expr
trySimpleLambda = do
  name <- expectIdent
  expect TokColon
  (\body -> ELambda (FormalName name) body NoCaptureInfo) <$> parseExpr

-- | Try @name\@{ formals }: body@
tryNamedSetLambda :: Parser Expr
tryNamedSetLambda = do
  name <- expectIdent
  expect TokAt
  expect TokLBrace
  (formals, hasEllipsis) <- parseFormalsBody
  expect TokColon
  (\body -> ELambda (FormalNamedSet name formals hasEllipsis) body NoCaptureInfo) <$> parseExpr

-- | Brace at start: could be @{ formals }: body@, @{ formals }\@name: body@, or attr set.
-- Falls back to parsing as attr set (via implication) if lambda parse fails.
parseLambdaOrAttrSet :: Parser Expr
parseLambdaOrAttrSet = do
  result <- tryParser trySetLambda
  maybe parseImplication pure result

-- | Try @{ a, b ? default, ... }: body@ or @{ a, b }\@name: body@
trySetLambda :: Parser Expr
trySetLambda = do
  expect TokLBrace
  (formals, hasEllipsis) <- parseFormalsBody
  -- Check for @name after the closing brace
  tok <- peek
  case tok of
    TokColon -> do
      _ <- advance
      (\body -> ELambda (FormalSet formals hasEllipsis) body NoCaptureInfo) <$> parseExpr
    TokAt -> do
      _ <- advance
      name <- expectIdent
      expect TokColon
      (\body -> ELambda (FormalNamedSet name formals hasEllipsis) body NoCaptureInfo) <$> parseExpr
    _ -> parseError ("expected ':' or '@' after formals, got " <> showToken tok)

-- | Parse the inside of @{ ... }@ formals (comma-separated, optional defaults,
-- optional ellipsis). Expects the closing @}@.
parseFormalsBody :: Parser ([Formal], Bool)
parseFormalsBody = do
  tok <- peek
  case tok of
    TokRBrace -> do
      _ <- advance
      pure ([], False)
    TokEllipsis -> do
      _ <- advance
      expect TokRBrace
      pure ([], True)
    _ -> parseFormalsList []

parseFormalsList :: [Formal] -> Parser ([Formal], Bool)
parseFormalsList acc = do
  name <- expectIdent
  -- Check for default value
  hasDefault <- match TokQuestion
  defVal <-
    if hasDefault
      then Just <$> parseExpr
      else pure Nothing
  let formal = Formal {fName = name, fDefault = defVal}
      newAcc = formal : acc
  tok <- peek
  case tok of
    TokComma -> do
      _ <- advance
      -- Check for ellipsis after comma
      next <- peek
      case next of
        TokEllipsis -> do
          _ <- advance
          expect TokRBrace
          pure (reverse newAcc, True)
        TokRBrace -> do
          _ <- advance
          pure (reverse newAcc, False)
        _ -> parseFormalsList newAcc
    TokRBrace -> do
      _ <- advance
      pure (reverse newAcc, False)
    _ -> parseError ("expected ',' or '}' in formals, got " <> showToken tok)

-- ---------------------------------------------------------------------------
-- Precedence levels (lowest to highest)
-- ---------------------------------------------------------------------------

-- | Level 1: Implication (@->@, right-associative).
parseImplication :: Parser Expr
parseImplication = do
  lhs <- parseLogicalOr
  tok <- peekMaybe
  case tok of
    Just TokImpl -> do
      _ <- advance
      EBinary OpImpl lhs <$> parseImplication
    _ -> pure lhs

-- | Level 2: Logical OR (@||@, left-associative).
parseLogicalOr :: Parser Expr
parseLogicalOr = do
  lhs <- parseLogicalAnd
  loopOr lhs
  where
    loopOr lhs = do
      found <- match TokOr
      if found
        then do
          rhs <- parseLogicalAnd
          loopOr (EBinary OpOr lhs rhs)
        else pure lhs

-- | Level 3: Logical AND (@&&@, left-associative).
parseLogicalAnd :: Parser Expr
parseLogicalAnd = do
  lhs <- parseEquality
  loopAnd lhs
  where
    loopAnd lhs = do
      found <- match TokAnd
      if found
        then do
          rhs <- parseEquality
          loopAnd (EBinary OpAnd lhs rhs)
        else pure lhs

-- | Level 4: Equality (@==@, @!=@, non-associative).
parseEquality :: Parser Expr
parseEquality = do
  lhs <- parseComparison
  tok <- peekMaybe
  case tok of
    Just TokEq -> do
      _ <- advance
      EBinary OpEq lhs <$> parseComparison
    Just TokNeq -> do
      _ <- advance
      EBinary OpNeq lhs <$> parseComparison
    _ -> pure lhs

-- | Level 5: Comparison (@<@, @>@, @<=@, @>=@, non-associative).
parseComparison :: Parser Expr
parseComparison = do
  lhs <- parseUpdate
  tok <- peekMaybe
  case tok of
    Just TokLt -> do
      _ <- advance
      EBinary OpLt lhs <$> parseUpdate
    Just TokGt -> do
      _ <- advance
      EBinary OpGt lhs <$> parseUpdate
    Just TokLte -> do
      _ <- advance
      EBinary OpLte lhs <$> parseUpdate
    Just TokGte -> do
      _ <- advance
      EBinary OpGte lhs <$> parseUpdate
    _ -> pure lhs

-- | Level 6: Update (@//@, right-associative).
parseUpdate :: Parser Expr
parseUpdate = do
  lhs <- parseNot
  tok <- peekMaybe
  case tok of
    Just TokUpdate -> do
      _ <- advance
      EBinary OpUpdate lhs <$> parseUpdate
    _ -> pure lhs

-- | Level 7: Logical NOT (@!@, prefix).
parseNot :: Parser Expr
parseNot = do
  tok <- peek
  case tok of
    TokNot -> do
      _ <- advance
      EUnary OpNot <$> parseNot
    _ -> parseConcat

-- | Level 8: List concatenation (@++@, right-associative).
parseConcat :: Parser Expr
parseConcat = do
  lhs <- parseAddSub
  tok <- peekMaybe
  case tok of
    Just TokConcat -> do
      _ <- advance
      EBinary OpConcat lhs <$> parseConcat
    _ -> pure lhs

-- | Level 9: Addition and subtraction (@+@, @-@, left-associative).
parseAddSub :: Parser Expr
parseAddSub = do
  lhs <- parseMulDiv
  loopAddSub lhs
  where
    loopAddSub lhs = do
      tok <- peekMaybe
      case tok of
        Just TokPlus -> do
          _ <- advance
          rhs <- parseMulDiv
          loopAddSub (EBinary OpAdd lhs rhs)
        Just TokMinus -> do
          _ <- advance
          rhs <- parseMulDiv
          loopAddSub (EBinary OpSub lhs rhs)
        _ -> pure lhs

-- | Level 10: Multiplication and division (@*@, @/@, left-associative).
parseMulDiv :: Parser Expr
parseMulDiv = do
  lhs <- parseNegate
  loopMulDiv lhs
  where
    loopMulDiv lhs = do
      tok <- peekMaybe
      case tok of
        Just TokStar -> do
          _ <- advance
          rhs <- parseNegate
          loopMulDiv (EBinary OpMul lhs rhs)
        Just TokSlash -> do
          _ <- advance
          rhs <- parseNegate
          loopMulDiv (EBinary OpDiv lhs rhs)
        _ -> pure lhs

-- | Level 11: Arithmetic negation (@-@, prefix).
parseNegate :: Parser Expr
parseNegate = do
  tok <- peek
  case tok of
    TokMinus -> do
      _ <- advance
      EUnary OpNegate <$> parseNegate
    _ -> parseHasAttr

-- | Level 12: Has-attribute (@?@, non-associative).
-- Right operand is an attr path, not a full expression.
parseHasAttr :: Parser Expr
parseHasAttr = do
  lhs <- parseApp
  tok <- peekMaybe
  case tok of
    Just TokQuestion -> do
      _ <- advance
      EHasAttr lhs <$> parseAttrPath
    _ -> pure lhs

-- | Level 13: Function application (juxtaposition, left-associative).
parseApp :: Parser Expr
parseApp = do
  func <- parseSelect
  loopApp func
  where
    loopApp func = do
      tok <- peekMaybe
      case tok of
        Just t | canStartAtom t -> do
          arg <- parseSelect
          loopApp (EApp func arg)
        _ -> pure func

-- | Level 14: Attribute selection (@e.a@, @e.a or default@).
parseSelect :: Parser Expr
parseSelect = do
  expr <- parseAtom
  selectLoop expr

selectLoop :: Expr -> Parser Expr
selectLoop expr = do
  tok <- peekMaybe
  case tok of
    Just TokDot -> do
      _ <- advance
      path <- parseAttrPath
      -- Check for 'or' default
      orDefault <- tryParser parseOrDefault
      selectLoop (ESelect expr path orDefault)
    _ -> pure expr

-- | Parse the @or default@ part after @e.path@.
parseOrDefault :: Parser Expr
parseOrDefault = do
  tok <- peek
  case tok of
    TokIdent "or" -> do
      _ <- advance
      parseSelect
    _ -> parseError "expected 'or'"

-- ---------------------------------------------------------------------------
-- Atoms
-- ---------------------------------------------------------------------------

parseAtom :: Parser Expr
parseAtom = do
  tok <- peek
  case tok of
    TokInt n -> advance >> pure (ELit (NixInt n))
    TokFloat d -> advance >> pure (ELit (NixFloat d))
    TokTrue -> advance >> pure (ELit (NixBool True))
    TokFalse -> advance >> pure (ELit (NixBool False))
    TokNull -> advance >> pure (ELit NixNull)
    TokUri u -> advance >> pure (ELit (NixUri u))
    TokPath p -> advance >> pure (ELit (NixPath p))
    TokSearchPath p -> advance >> pure (ESearchPath p)
    TokIdent name -> advance >> pure (EVar name)
    TokStringOpen -> parseString
    TokIndStringOpen -> parseIndString
    TokLParen -> parseParen
    TokLBracket -> parseList
    TokLBrace -> do
      _ <- advance
      parseAttrSet False
    TokRec -> do
      _ <- advance
      expect TokLBrace
      parseAttrSet True
    TokLet -> parseLet
    TokIf -> parseIf
    TokWith -> parseWith
    TokAssert -> parseAssert
    _ -> parseError ("unexpected " <> showToken tok)

-- ---------------------------------------------------------------------------
-- Strings
-- ---------------------------------------------------------------------------

parseString :: Parser Expr
parseString = do
  expect TokStringOpen
  parts <- parseStringParts TokStringClose
  pure (EStr parts)

parseIndString :: Parser Expr
parseIndString = do
  expect TokIndStringOpen
  parts <- parseStringParts TokIndStringClose
  pure (EIndStr parts)

parseStringParts :: Token -> Parser [StringPart]
parseStringParts closer = go []
  where
    go acc = do
      tok <- peek
      case tok of
        _ | tok == closer -> do
          _ <- advance
          pure (reverse acc)
        TokStringLit txt -> do
          _ <- advance
          go (StrLit txt : acc)
        -- Indented-string escapes are opaque to indentation stripping
        -- (upstream marks them hasIndentation = false and strips only
        -- marked chunks): a constant-string interpolation part gets
        -- exactly that treatment from the evaluator.
        TokStringEsc txt -> do
          _ <- advance
          go (StrInterp (EStr [StrLit txt]) : acc)
        TokInterpOpen -> do
          _ <- advance
          expr <- parseExpr
          expect TokInterpClose
          go (StrInterp expr : acc)
        _ -> parseError ("unexpected " <> showToken tok <> " in string")

-- ---------------------------------------------------------------------------
-- Compound expressions
-- ---------------------------------------------------------------------------

parseParen :: Parser Expr
parseParen = do
  expect TokLParen
  expr <- parseExpr
  expect TokRParen
  pure expr

parseList :: Parser Expr
parseList = do
  expect TokLBracket
  EList <$> parseListElems

parseListElems :: Parser [Expr]
parseListElems = go []
  where
    go acc = do
      tok <- peek
      case tok of
        TokRBracket -> do
          _ <- advance
          pure (reverse acc)
        _ | canStartAtom tok -> do
          elem_ <- parseSelect
          go (elem_ : acc)
        _ -> parseError ("unexpected " <> showToken tok <> " in list")

parseAttrSet :: Bool -> Parser Expr
parseAttrSet isRec =
  (\bs -> EAttrs isRec bs NoCaptureInfo) <$> parseBindings

parseBindings :: Parser [Binding]
parseBindings = go [] >>= normalizeBindings
  where
    go acc = do
      tok <- peek
      case tok of
        TokRBrace -> do
          _ <- advance
          pure (reverse acc)
        TokInherit -> do
          binding <- parseInherit
          go (binding : acc)
        _ -> do
          binding <- parseNamedBinding
          go (binding : acc)

-- ---------------------------------------------------------------------------
-- Binding normalization (upstream parser.y addAttr semantics)
-- ---------------------------------------------------------------------------

-- | Accumulated definition while normalizing one binding list.
data NormEntry
  = -- | @name = value@ - mergeable when the value is a plain attrset literal.
    StaticEntry !Text !Expr
  | -- | An @inherit@ - its names are never mergeable.
    InheritEntry !Binding
  | -- | A dynamic-key binding - collisions surface at eval time.
    DynamicEntry !Binding

-- | How a name was defined, for duplicate checking.
data DefKind = DefStatic | DefInherit

-- | Normalize a binding list the way upstream's parser (@addAttr@) does:
--
-- * a nested attrpath (@a.b.c = v@) hoists into nested attrset literals
--   under its first key (@a = { b = { c = v; }; }@);
-- * two definitions of one key merge recursively when BOTH values are
--   attrset literals - so @a.b = 1; a.c = 2;@ and @a = { b = 1; }; a.c = 2;@
--   both work, with inner duplicates checked recursively.  A @rec@ marker
--   on the existing set governs the merged result; one on the new set is
--   discarded, exactly as upstream;
-- * any other duplicate definition is a parse error, as upstream
--   (@a = 1; a = 2;@, an @inherit@ name reused, merging into a
--   non-literal value).
--
-- After normalization every static top-level key appears exactly once -
-- which also makes the positional (slot-based) eval path applicable to
-- sets and lets that use nested attrpaths.
normalizeBindings :: [Binding] -> Parser [Binding]
normalizeBindings bindings =
  case normalizeBindingList bindings of
    Left err -> parseError err
    Right normalized -> pure normalized

-- | Pure core of 'normalizeBindings'; recurses into merged literals.
normalizeBindingList :: [Binding] -> Either Text [Binding]
normalizeBindingList bindings = do
  (entriesRev, _defined) <- foldM step ([], Map.empty) (map canonicalBinding bindings)
  pure (map renderEntry (reverse entriesRev))
  where
    step :: ([NormEntry], Map Text DefKind) -> Binding -> Either Text ([NormEntry], Map Text DefKind)
    step (entries, defined) binding = case binding of
      NamedBinding [StaticKey key] value ->
        case Map.lookup key defined of
          Nothing ->
            Right (StaticEntry key value : entries, Map.insert key DefStatic defined)
          Just DefStatic -> do
            updated <- mergeIntoEntries key value entries
            Right (updated, defined)
          Just DefInherit -> Left (duplicateAttr key)
      b@(NamedBinding _ _) -> Right (DynamicEntry b : entries, defined)
      b@(Inherit _ names) -> do
        mapM_ (\name -> if Map.member name defined then Left (duplicateAttr name) else Right ()) names
        Right (InheritEntry b : entries, foldl' (\m name -> Map.insert name DefInherit m) defined names)

    -- Replace the existing entry for @key@ with the merged value.
    mergeIntoEntries key newValue entries = case entries of
      [] -> Left (duplicateAttr key)
      (StaticEntry existingKey existingValue : rest)
        | existingKey == key -> do
            merged <- mergeAttrValues key existingValue newValue
            Right (StaticEntry existingKey merged : rest)
      (entry : rest) -> (entry :) <$> mergeIntoEntries key newValue rest

    -- Upstream's addAttr (parser-state.hh) merges ANY two attrset
    -- literals without consulting ->recursive on either side: the
    -- EXISTING set's rec marker governs the merged result and the new
    -- set's marker is DISCARDED (upstream's own comment documents the
    -- discard).  The merged binding list is re-normalized so inner
    -- duplicates are caught.
    mergeAttrValues _ (EAttrs existingRec existingBindings _) (EAttrs _ newBindings _) =
      (\merged -> EAttrs existingRec merged NoCaptureInfo)
        <$> normalizeBindingList (existingBindings ++ newBindings)
    mergeAttrValues key _ _ = Left (duplicateAttr key)

    renderEntry (StaticEntry key value) = NamedBinding [StaticKey key] value
    renderEntry (InheritEntry b) = b
    renderEntry (DynamicEntry b) = b

    duplicateAttr key = "attribute '" <> key <> "' already defined"

-- | Hoist a nested attrpath under its first key: @a.b.c = v@ becomes
-- @a = { b = { c = v; }; }@, one literal per level.
canonicalBinding :: Binding -> Binding
canonicalBinding (NamedBinding (firstKey : rest@(_ : _)) value) =
  NamedBinding [firstKey] (EAttrs False [canonicalBinding (NamedBinding rest value)] NoCaptureInfo)
canonicalBinding b = b

parseNamedBinding :: Parser Binding
parseNamedBinding = do
  path <- parseAttrPath
  expect TokAssign
  val <- parseExpr
  expect TokSemicolon
  pure (NamedBinding path val)

parseInherit :: Parser Binding
parseInherit = do
  expect TokInherit
  tok <- peek
  case tok of
    TokLParen -> do
      _ <- advance
      from <- parseExpr
      expect TokRParen
      names <- parseInheritNames
      expect TokSemicolon
      pure (Inherit (Just from) names)
    _ -> do
      names <- parseInheritNames
      expect TokSemicolon
      pure (Inherit Nothing names)

parseInheritNames :: Parser [Text]
parseInheritNames = go []
  where
    go acc = do
      tok <- peek
      case tok of
        TokIdent name -> do
          _ <- advance
          go (name : acc)
        -- Keywords are valid as inherit names: inherit if then else;
        _ | Just name <- keywordToText tok -> do
          _ <- advance
          go (name : acc)
        -- Quoted strings for keywords used as attribute names: inherit "or";
        TokStringOpen -> do
          _ <- advance
          name <- parseQuotedInheritName
          go (name : acc)
        _ -> pure (reverse acc)

-- | Parse a quoted inherit name: a simple string literal (no interpolation).
-- Used for keywords like @"or"@ in @inherit (self.trivial) "or";@.
parseQuotedInheritName :: Parser Text
parseQuotedInheritName = do
  tok <- peek
  case tok of
    TokStringLit s -> do
      _ <- advance
      expect TokStringClose
      pure s
    TokStringClose -> do
      _ <- advance
      pure ""
    _ -> parseError ("expected string literal in inherit, got " <> showToken tok)

parseAttrPath :: Parser AttrPath
parseAttrPath = do
  first <- parseAttrKey
  rest <- pMany (expect TokDot >> parseAttrKey)
  pure (first : rest)

parseAttrKey :: Parser AttrKey
parseAttrKey = do
  tok <- peek
  case tok of
    TokIdent name -> advance >> pure (StaticKey name)
    -- Keywords are valid as attribute names in Nix:
    -- { if = 1; }, a.then, { else = 2; }, etc.
    _ | Just name <- keywordToText tok -> advance >> pure (StaticKey name)
    TokStringOpen -> do
      strExpr <- parseString
      -- A string key with no interpolation is static, as in upstream Nix's
      -- parser (the string becomes a symbol at parse time) - which is also
      -- what makes @let "x" = 1; in x@ legal.
      pure (maybe (DynamicKey strExpr) StaticKey (literalStringKey strExpr))
    TokInterpOpen -> do
      _ <- advance
      expr <- parseExpr
      -- In expression context, the lexer doesn't track interpolation
      -- mode - } is TokRBrace rather than TokInterpClose.
      expect TokRBrace
      pure (DynamicKey expr)
    _ -> parseError ("expected attribute key, got " <> showToken tok)

-- | The literal text of a string expression that contains no interpolation.
-- Escape handling can split a literal into several 'StrLit' chunks, so the
-- parts are concatenated; any 'StrInterp' makes the key dynamic.
literalStringKey :: Expr -> Maybe Text
literalStringKey (EStr parts) = mconcat <$> traverse literalPart parts
  where
    literalPart (StrLit t) = Just t
    literalPart (StrInterp _) = Nothing
literalStringKey _ = Nothing

-- | Convert keyword tokens to their text for use as attribute names.
-- All Nix keywords are valid as attr keys in binding and select position.
keywordToText :: Token -> Maybe Text
keywordToText TokIf = Just "if"
keywordToText TokThen = Just "then"
keywordToText TokElse = Just "else"
keywordToText TokLet = Just "let"
keywordToText TokIn = Just "in"
keywordToText TokWith = Just "with"
keywordToText TokAssert = Just "assert"
keywordToText TokRec = Just "rec"
keywordToText TokInherit = Just "inherit"
keywordToText TokTrue = Just "true"
keywordToText TokFalse = Just "false"
keywordToText TokNull = Just "null"
keywordToText _ = Nothing

parseLet :: Parser Expr
parseLet = do
  expect TokLet
  bindings <- parseLetBindings
  expect TokIn
  (\body -> ELet bindings body NoCaptureInfo) <$> parseExpr

parseLetBindings :: Parser [Binding]
parseLetBindings = go [] >>= normalizeBindings
  where
    go acc = do
      tok <- peek
      case tok of
        TokIn -> pure (reverse acc)
        TokInherit -> do
          binding <- parseInherit
          go (binding : acc)
        _ -> do
          binding <- parseNamedBinding
          -- Upstream rejects a dynamic TOP-LEVEL key in a let at parse time;
          -- nested dynamic keys (let a.${k} = 1) live in a nested attrset
          -- and are fine.
          case binding of
            NamedBinding (DynamicKey _ : _) _ ->
              parseError "dynamic attributes not allowed in let"
            _ -> go (binding : acc)

parseIf :: Parser Expr
parseIf = do
  expect TokIf
  cond <- parseExpr
  expect TokThen
  thenExpr <- parseExpr
  expect TokElse
  EIf cond thenExpr <$> parseExpr

parseWith :: Parser Expr
parseWith = do
  expect TokWith
  env <- parseExpr
  expect TokSemicolon
  EWith env <$> parseExpr

parseAssert :: Parser Expr
parseAssert = do
  expect TokAssert
  cond <- parseExpr
  expect TokSemicolon
  EAssert cond <$> parseExpr

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Can this token start an atomic expression (for application parsing)?
canStartAtom :: Token -> Bool
canStartAtom (TokIdent _) = True
canStartAtom (TokInt _) = True
canStartAtom (TokFloat _) = True
canStartAtom TokTrue = True
canStartAtom TokFalse = True
canStartAtom TokNull = True
canStartAtom (TokUri _) = True
canStartAtom (TokPath _) = True
canStartAtom (TokSearchPath _) = True
canStartAtom TokStringOpen = True
canStartAtom TokIndStringOpen = True
canStartAtom TokLParen = True
canStartAtom TokLBrace = True
canStartAtom TokLBracket = True
canStartAtom TokRec = True
canStartAtom TokLet = True
canStartAtom TokIf = True
canStartAtom TokWith = True
canStartAtom TokAssert = True
canStartAtom _ = False
