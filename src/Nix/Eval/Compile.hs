-- | Compile Nix 'Expr' AST trees to flat C bytecode.
--
-- Post-order traversal: children are compiled before parents, so child
-- bytecode indices are always less than parent indices.  Variable-length
-- data (string parts, bindings, formals, capture info, attr paths) goes
-- into a separate @uint32_t[]@ data buffer, referenced by offset from
-- instructions.
--
-- Must be called after 'cbcInit' and 'symbolInit'.
module Nix.Eval.Compile
  ( compileExpr,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64)
import Nix.Eval.CBytecode
  ( attrkeyDynamic,
    attrkeyStatic,
    binaryAdd,
    binaryAnd,
    binaryConcat,
    binaryDiv,
    binaryEq,
    binaryGt,
    binaryGte,
    binaryImpl,
    binaryLt,
    binaryLte,
    binaryMul,
    binaryNeq,
    binaryOr,
    binarySub,
    binaryUpdate,
    bindInherit,
    bindNamed,
    captureNone,
    captureSlots,
    captureWithScopes,
    cbcEmit,
    cbcEmitData,
    formalName,
    formalNamedSet,
    formalSet,
    opApp,
    opAssert,
    opAttrs,
    opBinary,
    opHasAttr,
    opIf,
    opIndStr,
    opLambda,
    opLet,
    opList,
    opLitBool,
    opLitFloat,
    opLitInt,
    opLitNull,
    opLitPath,
    opLitUri,
    opResolvedVar,
    opSearchPath,
    opSelect,
    opStr,
    opUnary,
    opVar,
    opWith,
    opWithVar,
    strpartInterp,
    strpartLit,
    unaryNegate,
    unaryNot,
  )
import Nix.Eval.Symbol (Symbol (..), symbolIntern)
import Nix.Expr.Types
  ( AttrKey (..),
    BinaryOp (..),
    Binding (..),
    CaptureInfo (..),
    Expr (..),
    Formal (..),
    Formals (..),
    NixAtom (..),
    StringPart (..),
    UnaryOp (..),
  )

-- | Compile an 'Expr' tree to bytecode, returning the root instruction index.
-- The bytecode is stored in the global @nn_bytecode@ arena.
compileExpr :: Expr -> IO Word32
compileExpr = go
  where
    go :: Expr -> IO Word32
    go (ELit atom) = compileLit atom
    go (EStr parts) = compileStringParts opStr parts
    go (EIndStr parts) = compileStringParts opIndStr parts
    go (EVar name) = compileSymbolOp opVar name
    go (EWithVar name) = compileSymbolOp opWithVar name
    go (EResolvedVar level idx) =
      cbcEmit opResolvedVar 0 0 (fi level) (fi idx) 0
    go (EAttrs isRec bindings captureInfo) =
      compileAttrs isRec bindings captureInfo
    go (EList exprs) = compileList exprs
    go (ESelect target path defExpr) =
      compileSelect target path defExpr
    go (EHasAttr target path) = compileHasAttr target path
    go (EApp func arg) = do
      funcIdx <- go func
      argIdx <- go arg
      cbcEmit opApp 0 0 funcIdx argIdx 0
    go (ELambda formals body captureInfo) =
      compileLambda formals body captureInfo
    go (ELet bindings body captureInfo) =
      compileLet bindings body captureInfo
    go (EIf cond thenExpr elseExpr) = do
      condIdx <- go cond
      thenIdx <- go thenExpr
      elseIdx <- go elseExpr
      cbcEmit opIf 0 0 condIdx thenIdx elseIdx
    go (EWith scope body) = do
      scopeIdx <- go scope
      bodyIdx <- go body
      cbcEmit opWith 0 0 scopeIdx bodyIdx 0
    go (EAssert cond body) = do
      condIdx <- go cond
      bodyIdx <- go body
      cbcEmit opAssert 0 0 condIdx bodyIdx 0
    go (EUnary op operand) = do
      operandIdx <- go operand
      cbcEmit opUnary (encodeUnaryOp op) 0 operandIdx 0 0
    go (EBinary op left right) = do
      leftIdx <- go left
      rightIdx <- go right
      cbcEmit opBinary (encodeBinaryOp op) 0 leftIdx rightIdx 0
    go (ESearchPath name) = compileSymbolOp opSearchPath name

    -- -----------------------------------------------------------------
    -- Literals
    -- -----------------------------------------------------------------

    compileLit :: NixAtom -> IO Word32
    compileLit (NixInt n) =
      let (lo, hi) = splitInt64 n
       in cbcEmit opLitInt 0 0 lo hi 0
    compileLit (NixFloat d) =
      let (lo, hi) = splitDouble d
       in cbcEmit opLitFloat 0 0 lo hi 0
    compileLit (NixBool b) =
      cbcEmit opLitBool 0 (if b then 1 else 0) 0 0 0
    compileLit NixNull =
      cbcEmit opLitNull 0 0 0 0 0
    compileLit (NixUri u) = compileSymbolOp opLitUri u
    compileLit (NixPath p) = compileSymbolOp opLitPath p

    -- \| Emit an instruction whose only operand is an interned symbol.
    compileSymbolOp :: Word8 -> Text -> IO Word32
    compileSymbolOp op name = do
      Symbol sym <- symbolIntern name
      cbcEmit op 0 0 sym 0 0

    -- -----------------------------------------------------------------
    -- Strings (EStr / EIndStr)
    -- -----------------------------------------------------------------

    compileStringParts :: Word8 -> [StringPart] -> IO Word32
    compileStringParts op parts = do
      compiled <- mapM compileOnePart parts
      dataOff <- emitPairs compiled
      cbcEmit op 0 (fi (length parts)) dataOff 0 0

    compileOnePart :: StringPart -> IO (Word32, Word32)
    compileOnePart (StrLit t) = do
      Symbol sym <- symbolIntern t
      pure (strpartLit, sym)
    compileOnePart (StrInterp expr) = do
      idx <- go expr
      pure (strpartInterp, idx)

    -- -----------------------------------------------------------------
    -- Attribute sets (EAttrs)
    -- -----------------------------------------------------------------

    compileAttrs :: Bool -> [Binding] -> CaptureInfo -> IO Word32
    compileAttrs isRec bindings captureInfo = do
      dataOff <- compileBindings bindings
      capOff <- compileCaptureInfo captureInfo
      cbcEmit opAttrs (if isRec then 1 else 0) (fi (length bindings)) dataOff capOff 0

    -- -----------------------------------------------------------------
    -- Lists (EList)
    -- -----------------------------------------------------------------

    compileList :: [Expr] -> IO Word32
    compileList exprs = do
      childIndices <- mapM go exprs
      dataOff <- emitWordList childIndices
      cbcEmit opList 0 (fi (length exprs)) dataOff 0 0

    -- -----------------------------------------------------------------
    -- Select / HasAttr
    -- -----------------------------------------------------------------

    compileSelect :: Expr -> [AttrKey] -> Maybe Expr -> IO Word32
    compileSelect target path defExpr = do
      targetIdx <- go target
      defIdx <- case defExpr of
        Nothing -> pure 0
        Just d -> go d
      (pathOff, pathLen) <- compileAttrPath path
      let hasDef = case defExpr of Nothing -> 0; Just _ -> 1
      cbcEmit opSelect hasDef pathLen targetIdx pathOff defIdx

    compileHasAttr :: Expr -> [AttrKey] -> IO Word32
    compileHasAttr target path = do
      targetIdx <- go target
      (pathOff, pathLen) <- compileAttrPath path
      cbcEmit opHasAttr 0 pathLen targetIdx pathOff 0

    compileAttrPath :: [AttrKey] -> IO (Word32, Word16)
    compileAttrPath path = do
      compiled <- mapM compileAttrKey path
      off <- emitPairs compiled
      pure (off, fi (length path))

    compileAttrKey :: AttrKey -> IO (Word32, Word32)
    compileAttrKey (StaticKey name) = do
      Symbol sym <- symbolIntern name
      pure (attrkeyStatic, sym)
    compileAttrKey (DynamicKey expr) = do
      idx <- go expr
      pure (attrkeyDynamic, idx)

    -- -----------------------------------------------------------------
    -- Lambda (ELambda)
    -- -----------------------------------------------------------------

    compileLambda :: Formals -> Expr -> CaptureInfo -> IO Word32
    compileLambda formals body captureInfo = do
      bodyIdx <- go body
      formalsOff <- compileFormals formals
      capOff <- compileCaptureInfo captureInfo
      cbcEmit opLambda (encodeFormalType formals) 0 formalsOff bodyIdx capOff

    compileFormals :: Formals -> IO Word32
    compileFormals (FormalName name) = do
      Symbol sym <- symbolIntern name
      emitWordList [sym]
    compileFormals (FormalSet formals ellipsis) = do
      formalWords <- compileFormalEntries formals
      emitWordList $
        [fi (length formals), if ellipsis then 1 else 0]
          ++ formalWords
    compileFormals (FormalNamedSet name formals ellipsis) = do
      Symbol nameSym <- symbolIntern name
      formalWords <- compileFormalEntries formals
      emitWordList $
        [nameSym, fi (length formals), if ellipsis then 1 else 0]
          ++ formalWords

    compileFormalEntries :: [Formal] -> IO [Word32]
    compileFormalEntries = fmap concat . mapM compileOneFormal

    compileOneFormal :: Formal -> IO [Word32]
    compileOneFormal (Formal name defExpr) = do
      Symbol nameSym <- symbolIntern name
      case defExpr of
        Nothing -> pure [nameSym, 0, 0]
        Just d -> do
          defIdx <- go d
          pure [nameSym, 1, defIdx]

    -- -----------------------------------------------------------------
    -- Let (ELet)
    -- -----------------------------------------------------------------

    compileLet :: [Binding] -> Expr -> CaptureInfo -> IO Word32
    compileLet bindings body captureInfo = do
      bodyIdx <- go body
      dataOff <- compileBindings bindings
      capOff <- compileCaptureInfo captureInfo
      cbcEmit opLet 0 (fi (length bindings)) dataOff bodyIdx capOff

    -- -----------------------------------------------------------------
    -- Bindings (shared by EAttrs and ELet)
    -- -----------------------------------------------------------------

    compileBindings :: [Binding] -> IO Word32
    compileBindings bindings = do
      allWords <- mapM compileOneBinding bindings
      emitWordList (concat allWords)

    compileOneBinding :: Binding -> IO [Word32]
    compileOneBinding (NamedBinding path expr) = do
      compiledKeys <- mapM compileAttrKey path
      valIdx <- go expr
      pure $
        [bindNamed, fi (length path)]
          ++ concatMap (\(t, v) -> [t, v]) compiledKeys
          ++ [valIdx]
    compileOneBinding (Inherit maybeFrom names) = do
      (hasFrom, fromIdx) <- case maybeFrom of
        Nothing -> pure (0 :: Word32, 0 :: Word32)
        Just fromExpr -> do
          idx <- go fromExpr
          pure (1, idx)
      syms <- mapM internName names
      pure $
        [bindInherit, hasFrom, fromIdx, fi (length names)]
          ++ syms

    internName :: Text -> IO Word32
    internName name = do
      Symbol sym <- symbolIntern name
      pure sym

    -- -----------------------------------------------------------------
    -- CaptureInfo (shared by EAttrs, ELet, ELambda)
    -- -----------------------------------------------------------------

    compileCaptureInfo :: CaptureInfo -> IO Word32
    compileCaptureInfo NoCaptureInfo =
      emitWordList [captureNone]
    compileCaptureInfo (Captures pairs) =
      emitWordList $
        [captureSlots, fi (length pairs)]
          ++ concatMap (\(level, idx) -> [fi level, fi idx]) pairs
    compileCaptureInfo (CapturesWithScopes pairs) =
      emitWordList $
        [captureWithScopes, fi (length pairs)]
          ++ concatMap (\(level, idx) -> [fi level, fi idx]) pairs

    -- -----------------------------------------------------------------
    -- Operator encoding
    -- -----------------------------------------------------------------

    encodeUnaryOp :: UnaryOp -> Word8
    encodeUnaryOp OpNot = unaryNot
    encodeUnaryOp OpNegate = unaryNegate

    encodeBinaryOp :: BinaryOp -> Word8
    encodeBinaryOp OpAdd = binaryAdd
    encodeBinaryOp OpSub = binarySub
    encodeBinaryOp OpMul = binaryMul
    encodeBinaryOp OpDiv = binaryDiv
    encodeBinaryOp OpAnd = binaryAnd
    encodeBinaryOp OpOr = binaryOr
    encodeBinaryOp OpImpl = binaryImpl
    encodeBinaryOp OpEq = binaryEq
    encodeBinaryOp OpNeq = binaryNeq
    encodeBinaryOp OpLt = binaryLt
    encodeBinaryOp OpLte = binaryLte
    encodeBinaryOp OpGt = binaryGt
    encodeBinaryOp OpGte = binaryGte
    encodeBinaryOp OpConcat = binaryConcat
    encodeBinaryOp OpUpdate = binaryUpdate

    encodeFormalType :: Formals -> Word8
    encodeFormalType (FormalName {}) = formalName
    encodeFormalType (FormalSet {}) = formalSet
    encodeFormalType (FormalNamedSet {}) = formalNamedSet

    -- -----------------------------------------------------------------
    -- Numeric encoding
    -- -----------------------------------------------------------------

    splitInt64 :: Integer -> (Word32, Word32)
    splitInt64 n =
      let w64 = fromIntegral n :: Word64
       in (fromIntegral (w64 .&. 0xFFFFFFFF), fromIntegral (shiftR w64 32))

    splitDouble :: Double -> (Word32, Word32)
    splitDouble d =
      let w64 = castDoubleToWord64 d
       in (fromIntegral (w64 .&. 0xFFFFFFFF), fromIntegral (shiftR w64 32))

    -- -----------------------------------------------------------------
    -- Data buffer helpers
    -- -----------------------------------------------------------------

    -- \| Emit pairs of (tag, value) to the data buffer.
    -- Returns the offset of the first emitted word, or 0 if empty.
    emitPairs :: [(Word32, Word32)] -> IO Word32
    emitPairs [] = pure 0
    emitPairs ((tag, val) : rest) = do
      off <- cbcEmitData tag
      _ <- cbcEmitData val
      mapM_ (\(t, v) -> cbcEmitData t >> cbcEmitData v) rest
      pure off

    -- \| Emit a list of uint32 values to the data buffer.
    -- Returns the offset of the first emitted word, or 0 if empty.
    emitWordList :: [Word32] -> IO Word32
    emitWordList [] = pure 0
    emitWordList (x : xs) = do
      off <- cbcEmitData x
      mapM_ cbcEmitData xs
      pure off

    -- \| @fromIntegral@ shorthand.
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral
