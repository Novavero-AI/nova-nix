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
    compileFormalsToEval,
    decodeBcFormals,
    decodeBcCaptureInfo,
    BcBinding (..),
    BcAttrKey (..),
    decodeBcBindings,
    reassembleInt64,
    reassembleDouble,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
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
    cbcData,
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
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..))
import Nix.Eval.Symbol (Symbol (..), symbolIntern, symbolText)
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
    compileFormalEntries = fmap concat . mapM compileFormalEntry

    compileFormalEntry :: Formal -> IO [Word32]
    compileFormalEntry (Formal name defExpr) = do
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

-- ---------------------------------------------------------------------------
-- Compile parser Formals to eval EvalFormals
-- ---------------------------------------------------------------------------

-- | Compile parser 'Formals' (with 'Maybe Expr' defaults) to eval-time
-- 'EvalFormals' (with 'Maybe Word32' bc_idx defaults).
compileFormalsToEval :: Formals -> IO EvalFormals
compileFormalsToEval (FormalName name) = pure (EFName name)
compileFormalsToEval (FormalSet formals ellipsis) =
  EFSet <$> mapM compileOneFormal formals <*> pure ellipsis
compileFormalsToEval (FormalNamedSet name formals ellipsis) =
  EFNamedSet name <$> mapM compileOneFormal formals <*> pure ellipsis

compileOneFormal :: Formal -> IO EvalFormal
compileOneFormal (Formal name defExpr) = do
  defBcIdx <- case defExpr of
    Nothing -> pure Nothing
    Just expr -> Just <$> compileExpr expr
  pure (EvalFormal name defBcIdx)

-- ---------------------------------------------------------------------------
-- Bytecode decoding (eval-time: read from C data buffer)
-- ---------------------------------------------------------------------------

-- | Decode formals from the bytecode data buffer.
-- @flags@ is the formal type (0=Name, 1=Set, 2=NamedSet).
-- @dataOff@ is the offset into the data buffer.
decodeBcFormals :: Word8 -> Word32 -> IO EvalFormals
decodeBcFormals flags dataOff = case flags of
  0 {- FormalName -} -> do
    sym <- cbcData dataOff
    pure (EFName (symbolText (Symbol sym)))
  1 {- FormalSet -} -> do
    count <- cbcData dataOff
    ellipsis <- cbcData (dataOff + 1)
    formals <- decodeFormalEntries (fromIntegral count) (dataOff + 2)
    pure (EFSet formals (ellipsis /= 0))
  2 {- FormalNamedSet -} -> do
    nameSym <- cbcData dataOff
    count <- cbcData (dataOff + 1)
    ellipsis <- cbcData (dataOff + 2)
    formals <- decodeFormalEntries (fromIntegral count) (dataOff + 3)
    pure (EFNamedSet (symbolText (Symbol nameSym)) formals (ellipsis /= 0))
  _ -> error "decodeBcFormals: invalid formal type flag"

-- | Decode a list of formal entries from the data buffer.
-- Each entry is 3 words: [name_sym, has_default, default_bc_idx].
decodeFormalEntries :: Int -> Word32 -> IO [EvalFormal]
decodeFormalEntries 0 _ = pure []
decodeFormalEntries n off = do
  nameSym <- cbcData off
  hasDef <- cbcData (off + 1)
  defBcIdx <- cbcData (off + 2)
  let defMaybe = if hasDef /= 0 then Just defBcIdx else Nothing
  rest <- decodeFormalEntries (n - 1) (off + 3)
  pure (EvalFormal (symbolText (Symbol nameSym)) defMaybe : rest)

-- | Decode capture info from the bytecode data buffer.
decodeBcCaptureInfo :: Word32 -> IO CaptureInfo
decodeBcCaptureInfo dataOff = do
  tag <- cbcData dataOff
  case tag of
    0 {- NoCaptureInfo -} -> pure NoCaptureInfo
    1 {- Captures -} -> do
      count <- cbcData (dataOff + 1)
      pairs <- decodePairs (fromIntegral count) (dataOff + 2)
      pure (Captures pairs)
    2 {- CapturesWithScopes -} -> do
      count <- cbcData (dataOff + 1)
      pairs <- decodePairs (fromIntegral count) (dataOff + 2)
      pure (CapturesWithScopes pairs)
    _ -> error "decodeBcCaptureInfo: invalid capture type tag"

-- | Decode (level, idx) pairs from the data buffer.
decodePairs :: Int -> Word32 -> IO [(Int, Int)]
decodePairs 0 _ = pure []
decodePairs n off = do
  level <- cbcData off
  idx <- cbcData (off + 1)
  rest <- decodePairs (n - 1) (off + 2)
  pure ((fromIntegral level, fromIntegral idx) : rest)

-- ---------------------------------------------------------------------------
-- Binding decoding (for evalBcAttrs / evalBcLet)
-- ---------------------------------------------------------------------------

-- | A decoded binding from the bytecode data buffer.
data BcBinding
  = -- | @path = expr@ with attr path keys and value bc_idx
    BcNamed ![BcAttrKey] !Word32
  | -- | @inherit names@ from surrounding scope (symbol ids)
    BcInherit ![Word32]
  | -- | @inherit (from) names@ with from-expr bc_idx and name symbols
    BcInheritFrom !Word32 ![Word32]

-- | An attribute key in a binding path.
data BcAttrKey
  = -- | Static key (interned symbol)
    BcStaticKey !Word32
  | -- | Dynamic key (bc_idx of expression to evaluate)
    BcDynamicKey !Word32

-- | Decode all bindings from the bytecode data buffer.
-- @bindCount@ is the number of bindings, @dataOff@ is the start offset.
decodeBcBindings :: Word16 -> Word32 -> IO [BcBinding]
decodeBcBindings 0 _ = pure []
decodeBcBindings count dataOff = do
  (binding, nextOff) <- decodeOneBinding dataOff
  rest <- decodeBcBindings (count - 1) nextOff
  pure (binding : rest)

-- | Decode a single binding, returning it and the offset past it.
decodeOneBinding :: Word32 -> IO (BcBinding, Word32)
decodeOneBinding off = do
  tag <- cbcData off
  case tag of
    0 {- NamedBinding -} -> do
      pathLen <- cbcData (off + 1)
      let keyStart = off + 2
          keyWords = fromIntegral pathLen * 2
          valOff = keyStart + keyWords
      keys <- decodeAttrKeys (fromIntegral pathLen) keyStart
      valBcIdx <- cbcData valOff
      pure (BcNamed keys valBcIdx, valOff + 1)
    1 {- Inherit -} -> do
      hasFrom <- cbcData (off + 1)
      fromBcIdx <- cbcData (off + 2)
      nameCount <- cbcData (off + 3)
      syms <- decodeSymList (fromIntegral nameCount) (off + 4)
      let nextOff = off + 4 + nameCount
      if hasFrom /= 0
        then pure (BcInheritFrom fromBcIdx syms, nextOff)
        else pure (BcInherit syms, nextOff)
    _ -> error "decodeOneBinding: invalid binding type tag"

-- | Decode attr path keys: pairs of (is_expr, key_or_bc_idx).
decodeAttrKeys :: Int -> Word32 -> IO [BcAttrKey]
decodeAttrKeys 0 _ = pure []
decodeAttrKeys n off = do
  isExpr <- cbcData off
  val <- cbcData (off + 1)
  rest <- decodeAttrKeys (n - 1) (off + 2)
  let key = if isExpr /= 0 then BcDynamicKey val else BcStaticKey val
  pure (key : rest)

-- | Decode a list of symbol IDs from the data buffer.
decodeSymList :: Int -> Word32 -> IO [Word32]
decodeSymList 0 _ = pure []
decodeSymList n off = do
  sym <- cbcData off
  rest <- decodeSymList (n - 1) (off + 1)
  pure (sym : rest)

-- ---------------------------------------------------------------------------
-- Numeric reassembly (from two uint32 halves)
-- ---------------------------------------------------------------------------

-- | Reassemble an Int64 from two uint32 halves (lo, hi).
reassembleInt64 :: Word32 -> Word32 -> Integer
reassembleInt64 lo hi =
  let w64 = fromIntegral lo .|. (fromIntegral hi `shiftL` 32) :: Word64
      signed = fromIntegral w64 :: Int64
   in fromIntegral signed

-- | Reassemble a Double from two uint32 halves (lo, hi).
reassembleDouble :: Word32 -> Word32 -> Double
reassembleDouble lo hi =
  let w64 = fromIntegral lo .|. (fromIntegral hi `shiftL` 32) :: Word64
   in castWord64ToDouble w64
