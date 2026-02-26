-- | Recursive descent expression parser for the Nix language.
--
-- 13 precedence levels, lowest to highest:
--
-- @
-- lambda / implication / || / && / == != / < > <= >= / //
-- ! / ++ / + - / * \\/ / negate / application / selection
-- @
--
-- Entirely pure. No IO, no Megaparsec, no Parsec.
module Nix.Parser.Expr
  ( -- * Entry point
    parseTopLevel,

    -- * Expression parsers (exported for testing)
    parseExpr,
  )
where

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
  ELambda (FormalName name) <$> parseExpr

-- | Try @name\@{ formals }: body@
tryNamedSetLambda :: Parser Expr
tryNamedSetLambda = do
  name <- expectIdent
  expect TokAt
  expect TokLBrace
  (formals, hasEllipsis) <- parseFormalsBody
  expect TokColon
  ELambda (FormalNamedSet name formals hasEllipsis) <$> parseExpr

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
      ELambda (FormalSet formals hasEllipsis) <$> parseExpr
    TokAt -> do
      _ <- advance
      name <- expectIdent
      expect TokColon
      ELambda (FormalNamedSet name formals hasEllipsis) <$> parseExpr
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
    _ -> parseApp

-- | Level 12: Function application (juxtaposition, left-associative).
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

-- | Level 13: Attribute selection (@e.a@, @e.a or default@) and
-- has-attribute (@e ? a@).
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
    Just TokQuestion -> do
      _ <- advance
      path <- parseAttrPath
      selectLoop (EHasAttr expr path)
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
  EAttrs isRec <$> parseBindings

parseBindings :: Parser [Binding]
parseBindings = go []
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
    -- 'or' can be used as an attr key (handled by TokIdent since 'or' is not a keyword)
    TokIdent name -> advance >> pure (StaticKey name)
    TokStringOpen ->
      DynamicKey <$> parseString
    TokInterpOpen -> do
      _ <- advance
      expr <- parseExpr
      -- In expression context, the lexer doesn't track interpolation
      -- mode — } is TokRBrace rather than TokInterpClose.
      expect TokRBrace
      pure (DynamicKey expr)
    _ -> parseError ("expected attribute key, got " <> showToken tok)

parseLet :: Parser Expr
parseLet = do
  expect TokLet
  bindings <- parseLetBindings
  expect TokIn
  ELet bindings <$> parseExpr

parseLetBindings :: Parser [Binding]
parseLetBindings = go []
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
          go (binding : acc)

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
