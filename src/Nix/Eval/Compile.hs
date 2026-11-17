{-# LANGUAGE PatternSynonyms #-}

-- | Compile Nix 'Expr' AST trees to flat C bytecode.
--
-- Post-order traversal: children are compiled before parents, so child
-- bytecode indices are always less than parent indices.  Variable-length
-- data (string parts, bindings, formals, capture info, attr paths) goes
-- into a separate @uint32_t[]@ data buffer, referenced by offset from
-- instructions; a binding refers to its position there by the index
-- "Nix.Eval.SourcePos" records it at.
--
-- The operator, formals, capture and binding decoders answer a tag this
-- module never writes, which only a corrupt store holds, with a failure
-- for the evaluator to report.  The store hands each tag back as a whole
-- byte or word, so no type on the decoding side can rule such a tag out.
--
-- Compiling writes the bytecode store and interns names, so it runs in an
-- evaluation session ('Nix.Eval.IO.withEvalSession').
module Nix.Eval.Compile
  ( compileExpr,
    compileParsed,
    compileFormalsToEval,
    encodeUnaryOp,
    decodeUnaryOp,
    encodeBinaryOp,
    decodeBinaryOp,
    decodeBcFormals,
    decodeBcCaptureInfo,
    BcBinding (..),
    BcInheritedName (..),
    BcAttrKey (..),
    decodeBcBindings,
    reassembleInt64,
    reassembleDouble,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
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
    bindInheritFrom,
    bindNamed,
    captureNone,
    captureSlots,
    captureWithScopes,
    cbcData,
    cbcEmit,
    cbcEmitData,
    cbcEmitString,
    cbcEmitText,
    formalName,
    formalNamedSet,
    formalSet,
    spilledCountSentinel,
    strpartEsc,
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
    pattern OpPos,
    pattern OpResolvedVar,
    pattern OpSelect,
    pattern OpStr,
    pattern OpUnary,
    pattern OpVar,
    pattern OpWith,
    pattern OpWithVar,
  )
import Nix.Eval.CData (MonadCData)
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..), LambdaName (..), lambdaNameFromSymbol, lambdaNameSymbol)
import Nix.Eval.SourcePos (PosIdx (..), PosRecorder, newPosRecorder, recordPos)
import Nix.Eval.Symbol (Symbol (..), symbolIntern, symbolText)
import Nix.Expr.Types
  ( AttrKey (..),
    BinaryOp (..),
    Binding (..),
    CaptureInfo (..),
    Expr (..),
    Formal (..),
    Formals (..),
    InheritedName (..),
    NixAtom (..),
    SrcPos,
    StringPart (..),
    UnaryOp (..),
  )
import Nix.Parser (ParsedSource, parsedExpr, parsedNames)

-- | Compile an 'Expr' tree to bytecode, returning the root instruction index.
-- The bytecode is stored in the global @nn_bytecode@ arena.  The names in
-- it are interned in the order this module meets them.
compileExpr :: Expr -> IO Word32
compileExpr expr = do
  recorder <- newPosRecorder
  compileWith recorder expr

-- | 'compileExpr' for parsed source, its names interned first in the order
-- upstream's parser interns them ('parsedNames'), so that compiling, which
-- meets them in its own order, finds each already in its place.
compileParsed :: ParsedSource -> IO Word32
compileParsed parsed = do
  mapM_ symbolIntern (parsedNames parsed)
  compileExpr (parsedExpr parsed)

-- | 'compileExpr' recording positions with one recorder for the whole tree.
compileWith :: PosRecorder -> Expr -> IO Word32
compileWith recorder = go
  where
    go :: Expr -> IO Word32
    go (ELit atom) = compileLit atom
    go (EStr parts) = compileStringParts OpStr parts
    go (EIndStr parts) = compileStringParts OpIndStr parts
    go (EPathStr headPiece parts) = compileStringParts OpPathStr (StrLit (TE.encodeUtf8 headPiece) : parts)
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
    go (EApp func arg) = compileApp func arg
    go (ELambda formals body captureInfo) =
      compileLambda AnonymousLambda formals body captureInfo
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
    go (EPos pos) = do
      posWord <- positionWord pos
      cbcEmit OpPos 0 0 posWord 0 0

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
    compileLit (NixUri u) = compileTextOp OpLitUri u
    compileLit (NixPath p) = compileTextOp OpLitPath p

    -- \| Emit an instruction whose only operand is an interned symbol.
    compileSymbolOp :: Word8 -> Text -> IO Word32
    compileSymbolOp op name = do
      Symbol sym <- symbolIntern name
      cbcEmit op 0 0 sym 0 0

    -- \| Emit an instruction whose only operand is a string of the code's:
    -- a URI, a path or a search path, each a value upstream's parser
    -- makes (an @ExprString@ or an @ExprPath@, parser.y:294-308 and
    -- :340-357 at 2.24.9), not a name it interns.
    compileTextOp :: Word8 -> Text -> IO Word32
    compileTextOp op text = do
      index <- cbcEmitText text
      cbcEmit op 0 0 index 0 0

    -- -----------------------------------------------------------------
    -- Applications (EApp)
    -- -----------------------------------------------------------------

    compileApp :: Expr -> Expr -> IO Word32
    compileApp func arg = do
      funcIdx <- go func
      argIdx <- go arg
      cbcEmit OpApp 0 0 funcIdx argIdx 0

    -- -----------------------------------------------------------------
    -- Strings (EStr / EIndStr)
    -- -----------------------------------------------------------------

    compileStringParts :: Word8 -> [StringPart] -> IO Word32
    compileStringParts op parts = do
      compiled <- mapM compileOnePart parts
      (partCount, dataOff) <- emitCounted "string with parts" (length parts) (pairWords compiled)
      cbcEmit op 0 partCount dataOff 0 0

    -- \| A literal part's bytes are a string of the code's, which a value
    -- the part makes alone points at, as upstream's @ExprString@ holds
    -- the value its bytes make (nixexpr.hh:122-129 at 2.24.9).
    compileOnePart :: StringPart -> IO (Word32, Word32)
    compileOnePart (StrLit bytes) = do
      index <- cbcEmitString bytes
      pure (strpartLit, index)
    compileOnePart (StrEsc bytes) = do
      index <- cbcEmitString bytes
      pure (strpartEsc, index)
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

    -- \| A function and, when its body is a function too, the body under
    -- the same name, as upstream's @ExprLambda::setName@ names the body
    -- (nixexpr.cc:573-577 at 2.24.9).
    compileLambda :: LambdaName -> Formals -> Expr -> CaptureInfo -> IO Word32
    compileLambda name formals body captureInfo = do
      bodyIdx <- compileNamed name body
      formalsOff <- compileFormals name formals
      capOff <- compileCaptureInfo captureInfo
      cbcEmit OpLambda (encodeFormalType formals) 0 formalsOff bodyIdx capOff

    -- \| An expression whose value a binding names: a function there
    -- takes the name, anything else compiles as it would anywhere.
    compileNamed :: LambdaName -> Expr -> IO Word32
    compileNamed name (ELambda formals body captureInfo) = compileLambda name formals body captureInfo
    compileNamed _ expr = go expr

    -- \| A set pattern's words start with the function's name
    -- ('lambdaNameSymbol').
    compileFormals :: LambdaName -> Formals -> IO Word32
    compileFormals _ (FormalName name) = do
      Symbol sym <- symbolIntern name
      emitWordList [sym]
    compileFormals name (FormalSet formals ellipsis) = do
      nameWord <- lambdaNameSymbol name
      formalWords <- compileFormalEntries formals
      emitWordList $
        [nameWord, fi (length formals), if ellipsis then 1 else 0]
          ++ formalWords
    compileFormals name (FormalNamedSet argName formals ellipsis) = do
      nameWord <- lambdaNameSymbol name
      Symbol argSym <- symbolIntern argName
      formalWords <- compileFormalEntries formals
      emitWordList $
        [nameWord, argSym, fi (length formals), if ellipsis then 1 else 0]
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

    -- \| A function bound to a static name is named by it, as upstream's
    -- @addAttr@ names the value of an attribute path's last name
    -- (parser-state.hh:176-177 at 2.24.9).  Upstream names a function
    -- under a dynamic key too, with the name the key evaluates to, each
    -- time it evaluates the set (@ExprAttrs::eval@, eval.cc:1270); a
    -- name the compiler cannot know, so such a function is anonymous
    -- here.
    compileOneBinding :: Binding -> IO [Word32]
    compileOneBinding (NamedBinding pos key expr) = do
      (keyKind, keyWord) <- compileAttrKey key
      valIdx <- case key of
        StaticKey name -> compileNamed (NamedLambda name) expr
        DynamicKey _ -> go expr
      posWord <- positionWord pos
      pure [bindNamed, keyKind, keyWord, valIdx, posWord]
    compileOneBinding (Inherit pos name var) = do
      sym <- internName name
      valIdx <- go var
      posWord <- positionWord pos
      pure [bindInherit, sym, valIdx, posWord]
    compileOneBinding (InheritFrom fromExpr names) = do
      fromIdx <- go fromExpr
      inherited <- mapM compileInheritedName names
      pure $
        [bindInheritFrom, fromIdx, fi (length names)]
          ++ concat inherited

    -- \| One name of @inherit (from) names@: its symbol, then the select
    -- that reads it from slot 0 of the env the evaluator puts the
    -- from-value in, then its position.  Compiled here with the binding,
    -- as upstream's parser makes an @ExprSelect@ per name, rather than
    -- each time the binding is evaluated, which would add a copy to a
    -- store that only grows.
    compileInheritedName :: InheritedName -> IO [Word32]
    compileInheritedName (InheritedName pos name) = do
      sym <- internName name
      selectIdx <- go (ESelect (EResolvedVar 0 0) [StaticKey name] Nothing)
      posWord <- positionWord pos
      pure [sym, selectIdx, posWord]

    internName :: Text -> IO Word32
    internName name = do
      Symbol sym <- symbolIntern name
      pure sym

    positionWord :: SrcPos -> IO Word32
    positionWord pos = unPosIdx <$> recordPos recorder pos

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
    -- Formal type encoding
    -- -----------------------------------------------------------------

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
-- 'EvalFormals' (with 'Maybe Word32' bc_idx defaults), for a function no
-- binding names.
compileFormalsToEval :: Formals -> IO EvalFormals
compileFormalsToEval (FormalName name) = pure (EFName name)
compileFormalsToEval (FormalSet formals ellipsis) =
  EFSet AnonymousLambda <$> mapM compileOneFormal formals <*> pure ellipsis
compileFormalsToEval (FormalNamedSet name formals ellipsis) =
  EFNamedSet AnonymousLambda name <$> mapM compileOneFormal formals <*> pure ellipsis

compileOneFormal :: Formal -> IO EvalFormal
compileOneFormal (Formal name defExpr) = do
  defBcIdx <- case defExpr of
    Nothing -> pure Nothing
    Just expr -> Just <$> compileExpr expr
  pure (EvalFormal name defBcIdx)

-- ---------------------------------------------------------------------------
-- Operator tags (the flags byte of OpUnary and OpBinary)
-- ---------------------------------------------------------------------------

-- | The flags byte an @OpUnary@ instruction carries for its operator.
encodeUnaryOp :: UnaryOp -> Word8
encodeUnaryOp OpNot = unaryNot
encodeUnaryOp OpNegate = unaryNegate

-- | The operator an @OpUnary@ flags byte names: the inverse of
-- 'encodeUnaryOp', and 'Nothing' for a byte it never writes.
decodeUnaryOp :: Word8 -> Maybe UnaryOp
decodeUnaryOp tag
  | tag == unaryNot = Just OpNot
  | tag == unaryNegate = Just OpNegate
  | otherwise = Nothing

-- | The flags byte an @OpBinary@ instruction carries for its operator.
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

-- | The operator an @OpBinary@ flags byte names: the inverse of
-- 'encodeBinaryOp', and 'Nothing' for a byte it never writes.
decodeBinaryOp :: Word8 -> Maybe BinaryOp
decodeBinaryOp tag
  | tag == binaryAdd = Just OpAdd
  | tag == binarySub = Just OpSub
  | tag == binaryMul = Just OpMul
  | tag == binaryDiv = Just OpDiv
  | tag == binaryAnd = Just OpAnd
  | tag == binaryOr = Just OpOr
  | tag == binaryImpl = Just OpImpl
  | tag == binaryEq = Just OpEq
  | tag == binaryNeq = Just OpNeq
  | tag == binaryLt = Just OpLt
  | tag == binaryLte = Just OpLte
  | tag == binaryGt = Just OpGt
  | tag == binaryGte = Just OpGte
  | tag == binaryConcat = Just OpConcat
  | tag == binaryUpdate = Just OpUpdate
  | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Bytecode decoding (eval-time: read from C data buffer)
-- ---------------------------------------------------------------------------

-- | Decode formals from the bytecode data buffer: @flags@ is the lambda's
-- formal type ('formalName', 'formalSet' or 'formalNamedSet'), @dataOff@
-- the offset of its words.  'Nothing' for any other type.  The offset is
-- strict, though an unknown type never reads it, so a caller passes it
-- evaluated rather than as a thunk.
decodeBcFormals :: (MonadCData m) => Word8 -> Word32 -> m (Maybe EvalFormals)
decodeBcFormals flags !dataOff
  | flags == formalName = do
      sym <- cbcData dataOff
      pure (Just (EFName (symbolText (Symbol sym))))
  | flags == formalSet = do
      lambdaSym <- cbcData dataOff
      count <- cbcData (dataOff + 1)
      ellipsis <- cbcData (dataOff + 2)
      formals <- decodeFormalEntries (fromIntegral count) (dataOff + 3)
      pure (Just (EFSet (lambdaNameFromSymbol lambdaSym) formals (ellipsis /= 0)))
  | flags == formalNamedSet = do
      lambdaSym <- cbcData dataOff
      nameSym <- cbcData (dataOff + 1)
      count <- cbcData (dataOff + 2)
      ellipsis <- cbcData (dataOff + 3)
      formals <- decodeFormalEntries (fromIntegral count) (dataOff + 4)
      pure (Just (EFNamedSet (lambdaNameFromSymbol lambdaSym) (symbolText (Symbol nameSym)) formals (ellipsis /= 0)))
  | otherwise = pure Nothing

-- | Decode a list of formal entries from the data buffer.
-- Each entry is 3 words: [name_sym, has_default, default_bc_idx].
decodeFormalEntries :: (MonadCData m) => Int -> Word32 -> m [EvalFormal]
decodeFormalEntries 0 _ = pure []
decodeFormalEntries n off = do
  nameSym <- cbcData off
  hasDef <- cbcData (off + 1)
  defBcIdx <- cbcData (off + 2)
  let defMaybe = if hasDef /= 0 then Just defBcIdx else Nothing
  rest <- decodeFormalEntries (n - 1) (off + 3)
  pure (EvalFormal (symbolText (Symbol nameSym)) defMaybe : rest)

-- | Decode capture info from the bytecode data buffer, or answer the tag
-- at @dataOff@ when it is none of 'captureNone', 'captureSlots' and
-- 'captureWithScopes'.
decodeBcCaptureInfo :: (MonadCData m) => Word32 -> m (Either Word32 CaptureInfo)
decodeBcCaptureInfo dataOff = cbcData dataOff >>= decodeTagged
  where
    decodeTagged tag
      | tag == captureNone = pure (Right NoCaptureInfo)
      | tag == captureSlots = Right . Captures <$> capturedPairs
      | tag == captureWithScopes = Right . CapturesWithScopes <$> capturedPairs
      | otherwise = pure (Left tag)
    capturedPairs = do
      count <- cbcData (dataOff + 1)
      decodePairs (fromIntegral count) (dataOff + 2)

-- | Decode (level, idx) pairs from the data buffer.
decodePairs :: (MonadCData m) => Int -> Word32 -> m [(Int, Int)]
decodePairs 0 _ = pure []
decodePairs n off = do
  level <- cbcData off
  idx <- cbcData (off + 1)
  rest <- decodePairs (n - 1) (off + 2)
  pure ((fromIntegral level, fromIntegral idx) : rest)

-- ---------------------------------------------------------------------------
-- Binding decoding (for evalBcAttrs / evalBcLet)
-- ---------------------------------------------------------------------------

-- | A decoded binding from the bytecode data buffer, each name at the
-- position it was defined at.
data BcBinding
  = -- | @key = expr@: its position, the key and the value's bc_idx
    BcNamed !PosIdx !BcAttrKey !Word32
  | -- | One name of @inherit x@: its position, its symbol and the bc_idx
    -- of the variable it copies, which is evaluated in the env around the
    -- binding set
    BcInherit !PosIdx !Word32 !Word32
  | -- | @inherit (from) names@ with from-expr bc_idx and the names
    BcInheritFrom !Word32 ![BcInheritedName]

-- | One name of an @inherit (from) names@ binding: its position, its
-- symbol and the bc_idx of the select that reads it from slot 0 of an env
-- holding the from-value.
data BcInheritedName = BcInheritedName !PosIdx !Word32 !Word32

-- | A binding's attribute key.
data BcAttrKey
  = -- | Static key (interned symbol)
    BcStaticKey !Word32
  | -- | Dynamic key (bc_idx of expression to evaluate)
    BcDynamicKey !Word32

-- | Decode all bindings from the bytecode data buffer, or answer the
-- first binding tag that is none of 'bindNamed', 'bindInherit' and
-- 'bindInheritFrom'.  @bindCount@ is the number of bindings (already
-- resolved through the short_arg spill by the caller), @off@ is the start
-- offset.
decodeBcBindings :: (MonadCData m) => Int -> Word32 -> m (Either Word32 [BcBinding])
decodeBcBindings 0 _ = pure (Right [])
decodeBcBindings bindCount off = cbcData off >>= decodeTagged
  where
    decodeTagged tag
      | tag == bindNamed = do
          key <- decodeAttrKey (off + 1)
          valBcIdx <- cbcData (off + 3)
          pos <- cbcData (off + 4)
          withRest (BcNamed (PosIdx pos) key valBcIdx) (off + 5)
      | tag == bindInherit = do
          sym <- cbcData (off + 1)
          valBcIdx <- cbcData (off + 2)
          pos <- cbcData (off + 3)
          withRest (BcInherit (PosIdx pos) sym valBcIdx) (off + 4)
      | tag == bindInheritFrom = do
          fromBcIdx <- cbcData (off + 1)
          nameCount <- cbcData (off + 2)
          names <- decodeInheritedNames (fromIntegral nameCount) (off + 3)
          withRest (BcInheritFrom fromBcIdx names) (off + 3 + 3 * nameCount)
      | otherwise = pure (Left tag)
    -- Strict in the binding and the result, so each level conses onto
    -- the decoded rest directly rather than leaving thunks for both.
    withRest !binding nextOff = do
      rest <- decodeBcBindings (bindCount - 1) nextOff
      pure $! (binding :) <$> rest

-- | Decode a binding's key: the pair (is_expr, key_or_bc_idx).
decodeAttrKey :: (MonadCData m) => Word32 -> m BcAttrKey
decodeAttrKey off = do
  isExpr <- cbcData off
  val <- cbcData (off + 1)
  pure (if isExpr /= 0 then BcDynamicKey val else BcStaticKey val)

-- | Decode inherited names: triples of (name_sym, select_bc_idx, pos).
decodeInheritedNames :: (MonadCData m) => Int -> Word32 -> m [BcInheritedName]
decodeInheritedNames 0 _ = pure []
decodeInheritedNames n off = do
  sym <- cbcData off
  selectBcIdx <- cbcData (off + 1)
  pos <- cbcData (off + 2)
  rest <- decodeInheritedNames (n - 1) (off + 3)
  pure (BcInheritedName (PosIdx pos) sym selectBcIdx : rest)

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
