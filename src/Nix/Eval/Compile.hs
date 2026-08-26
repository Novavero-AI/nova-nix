{-# LANGUAGE PatternSynonyms #-}

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
    spilledCountSentinel,
    strpartInterp,
    strpartLit,
    unaryNegate,
    unaryNot,
    pattern OpApp,
    pattern OpAssert,
    pattern OpAttrs,
    pattern OpBinary,
    pattern OpHasAttr,
    pattern OpIf,
    pattern OpIndStr,
    pattern OpLambda,
    pattern OpLet,
    pattern OpList,
    pattern OpLitBool,
    pattern OpLitFloat,
    pattern OpLitInt,
    pattern OpLitNull,
    pattern OpLitPath,
    pattern OpLitUri,
    pattern OpPathStr,
    pattern OpResolvedVar,
    pattern OpSearchPath,
    pattern OpSelect,
    pattern OpStr,
    pattern OpUnary,
    pattern OpVar,
    pattern OpWith,
    pattern OpWithVar,
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
    go (EStr parts) = compileStringParts OpStr parts
    go (EIndStr parts) = compileStringParts OpIndStr parts
    go (EPathStr parts) = compileStringParts OpPathStr parts
    go (EVar name) = compileSymbolOp OpVar name
    go (EWithVar name) = compileSymbolOp OpWithVar name
    go (EResolvedVar level idx) =
      cbcEmit OpResolvedVar 0 0 (fi level) (fi idx) 0
    go (EAttrs isRec bindings captureInfo) =
      compileAttrs isRec bindings captureInfo
    go (EList exprs) = compileList exprs
    go (ESelect target path defExpr) =
      compileSelect target path defExpr
    go (EHasAttr target path) = compileHasAttr target path
    go (EApp func arg) = do
      funcIdx <- go func
      argIdx <- go arg
      cbcEmit OpApp 0 0 funcIdx argIdx 0
    go (ELambda formals body captureInfo) =
      compileLambda formals body captureInfo
    go (ELet bindings body captureInfo) =
      compileLet bindings body captureInfo
    go (EIf cond thenExpr elseExpr) = do
      condIdx <- go cond
      thenIdx <- go thenExpr
      elseIdx <- go elseExpr
      cbcEmit OpIf 0 0 condIdx thenIdx elseIdx
    go (EWith scope body) = do
      scopeIdx <- go scope
      bodyIdx <- go body
      cbcEmit OpWith 0 0 scopeIdx bodyIdx 0
    go (EAssert cond body) = do
      condIdx <- go cond
      bodyIdx <- go body
      cbcEmit OpAssert 0 0 condIdx bodyIdx 0
    go (EUnary op operand) = do
      operandIdx <- go operand
      cbcEmit OpUnary (encodeUnaryOp op) 0 operandIdx 0 0
    go (EBinary op left right) = do
      leftIdx <- go left
      rightIdx <- go right
      cbcEmit OpBinary (encodeBinaryOp op) 0 leftIdx rightIdx 0
    go (ESearchPath name) = compileSymbolOp OpSearchPath name

    -- -----------------------------------------------------------------
    -- Literals
    -- -----------------------------------------------------------------

    compileLit :: NixAtom -> IO Word32
    compileLit (NixInt n) =
      let (lo, hi) = splitInt64 n
       in cbcEmit OpLitInt 0 0 lo hi 0
    compileLit (NixFloat d) =
      let (lo, hi) = splitDouble d
       in cbcEmit OpLitFloat 0 0 lo hi 0
    compileLit (NixBool b) =
      cbcEmit OpLitBool 0 (if b then 1 else 0) 0 0 0
    compileLit NixNull =
      cbcEmit OpLitNull 0 0 0 0 0
    compileLit (NixUri u) = compileSymbolOp OpLitUri u
    compileLit (NixPath p) = compileSymbolOp OpLitPath p

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
      (partCount, dataOff) <- emitCounted "string with parts" (length parts) (pairWords compiled)
      cbcEmit op 0 partCount dataOff 0 0

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
      bindingWords <- compileBindingWords bindings
      capOff <- compileCaptureInfo captureInfo
      (bindingCount, dataOff) <- emitCounted "attribute set with bindings" (length bindings) bindingWords
      cbcEmit OpAttrs (if isRec then 1 else 0) bindingCount dataOff capOff 0

    -- -----------------------------------------------------------------
    -- Lists (EList)
    -- -----------------------------------------------------------------

    compileList :: [Expr] -> IO Word32
    compileList exprs = do
      childIndices <- mapM go exprs
      (elemCount, dataOff) <- emitCounted "list with elements" (length exprs) childIndices
      cbcEmit OpList 0 elemCount dataOff 0 0

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
      cbcEmit OpSelect hasDef pathLen targetIdx pathOff defIdx

    compileHasAttr :: Expr -> [AttrKey] -> IO Word32
    compileHasAttr target path = do
      targetIdx <- go target
      (pathOff, pathLen) <- compileAttrPath path
      cbcEmit OpHasAttr 0 pathLen targetIdx pathOff 0

    compileAttrPath :: [AttrKey] -> IO (Word32, Word16)
    compileAttrPath path = do
      compiled <- mapM compileAttrKey path
      (pathLen, off) <- emitCounted "attribute path with segments" (length path) (pairWords compiled)
      pure (off, pathLen)

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
      cbcEmit OpLambda (encodeFormalType formals) 0 formalsOff bodyIdx capOff

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
      bindingWords <- compileBindingWords bindings
      capOff <- compileCaptureInfo captureInfo
      (bindingCount, dataOff) <- emitCounted "let with bindings" (length bindings) bindingWords
      cbcEmit OpLet 0 bindingCount dataOff bodyIdx capOff

    -- -----------------------------------------------------------------
    -- Bindings (shared by EAttrs and ELet)
    -- -----------------------------------------------------------------

    -- \| The flat data words for a binding list; the caller emits them
    -- (with the count, via emitCounted) so a spilled count can precede
    -- them contiguously.
    compileBindingWords :: [Binding] -> IO [Word32]
    compileBindingWords bindings = concat <$> mapM compileOneBinding bindings

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

    splitInt64 :: Int64 -> (Word32, Word32)
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

    -- \| Flatten (tag, value) pairs into data-buffer words.
    pairWords :: [(Word32, Word32)] -> [Word32]
    pairWords = concatMap (\(tag, val) -> [tag, val])

    -- \| Emit a list of uint32 values to the data buffer.
    -- Returns the offset of the first emitted word, or 0 if empty.
    emitWordList :: [Word32] -> IO Word32
    emitWordList [] = pure 0
    emitWordList (x : xs) = do
      off <- cbcEmitData x
      mapM_ cbcEmitData xs
      pure off

    -- \| Emit an op's counted payload, returning the short_arg and data
    -- offset to store in the op.  A count below 'spilledCountSentinel'
    -- travels inline in short_arg; a larger one is written as the first
    -- data word with the sentinel inline ('cbcCountedPayload' is the
    -- decode side).  nn_op_t stays 16 bytes either way; only an
    -- oversized literal pays the extra word.
    emitCounted :: String -> Int -> [Word32] -> IO (Word16, Word32)
    emitCounted what n payload
      | n < fromIntegral spilledCountSentinel = do
          off <- emitWordList payload
          pure (fromIntegral n, off)
      | otherwise = do
          spilled <- countArg what n
          off <- emitWordList (spilled : payload)
          pure (spilledCountSentinel, off)

    -- \| Convert a spilled element count into its 32-bit data word,
    -- failing loudly at the (unreachable-in-practice) ceiling: a silent
    -- truncation would make an oversized literal report a wrong length.
    countArg :: String -> Int -> IO Word32
    countArg what n
      | toInteger n > toInteger (maxBound :: Word32) =
          ioError (userError ("compile: " <> what <> " exceeding the bytecode limit of 4294967295 (got " <> show n <> ")"))
      | otherwise = pure (fromIntegral n)

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
  -- Unreachable: the flag is written only by this module's formals compiler,
  -- which emits 0, 1, or 2 - all matched above.
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
    -- Unreachable: the tag is written only by this module's capture compiler,
    -- which emits 0, 1, or 2 - all matched above.
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
-- @bindCount@ is the number of bindings (already resolved through the
-- short_arg spill by the caller), @dataOff@ is the start offset.
decodeBcBindings :: Int -> Word32 -> IO [BcBinding]
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
    -- Unreachable: the tag is written only by this module's binding compiler,
    -- which emits 0 or 1 - both matched above.
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
reassembleInt64 :: Word32 -> Word32 -> Int64
reassembleInt64 lo hi =
  let w64 = fromIntegral lo .|. (fromIntegral hi `shiftL` 32) :: Word64
   in fromIntegral w64

-- | Reassemble a Double from two uint32 halves (lo, hi).
reassembleDouble :: Word32 -> Word32 -> Double
reassembleDouble lo hi =
  let w64 = fromIntegral lo .|. (fromIntegral hi `shiftL` 32) :: Word64
   in castWord64ToDouble w64
