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
-- The operator, formals and capture decoders and the binding fold answer
-- a tag this module never writes, which only a corrupt store holds, with
-- a failure for the evaluator to report.  The store hands each tag back
-- as a whole byte or word, so no type on the decoding side can rule such
-- a tag out.
--
-- A set's or a let's bindings and a closure's captured slots are read by
-- folds over the data buffer each time their code runs, so no list of
-- them is built.  A binding list's words start with the names it binds
-- counted, so the evaluator knows a frame's layout and a set's size
-- before it reads a binding, and then the frame's shares, which the
-- compiler decides once ('siblingShares') for the evaluator to copy each
-- time it fills the frame.
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
    BcCaptureInfo (..),
    BcCapturedSlots (..),
    decodeBcCaptureInfo,
    foldBcCapturedSlots,
    BcBindings (..),
    BcBindingCounts (..),
    BcShares (..),
    decodeBcBindings,
    foldBcShares,
    SiblingVar (..),
    siblingShares,
    formalDefaultWords,
    formalDefaultFromWords,
    BcBinding (..),
    BcInheritedNames (..),
    BcInheritedName (..),
    foldBcBindings,
    foldBcInheritedNames,
    reassembleInt64,
    reassembleDouble,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Nix.Eval.CBytecode
  ( attrkeyDynamic,
    attrkeyStatic,
    attrsPlain,
    attrsRec,
    attrsRecOverrides,
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
    formalDefault,
    formalName,
    formalNamedSet,
    formalNoDefault,
    formalSet,
    formalSharedDefault,
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
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..), FormalDefault (..), LambdaName (..), lambdaNameFromSymbol, lambdaNameSymbol)
import Nix.Eval.SourcePos (PosIdx (..), PosRecorder, newPosRecorder, recordPos)
import Nix.Eval.Symbol (Symbol (..), symbolIntern)
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
    go (EAttrs False bindings captureInfo) =
      compileAttrs attrsPlain NothingShares bindings captureInfo
    go (EAttrs True bindings captureInfo)
      | bindsOverrides bindings = compileAttrs attrsRecOverrides NothingShares bindings captureInfo
      | otherwise = compileAttrs attrsRec SiblingsShare bindings captureInfo
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
    -- a URI or a path, each a value upstream's parser makes (an
    -- @ExprString@ or an @ExprPath@, in the @expr_simple@ and @path_start@
    -- rules of parser.y), not a name it interns.
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
    -- the value its bytes make (nixexpr.hh).
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

    -- \| A set whose instruction holds the flag given ('attrsPlain',
    -- 'attrsRec' or 'attrsRecOverrides'), its values sharing as
    -- @sharing@ says.
    compileAttrs :: Word8 -> FrameSharing -> [Binding] -> CaptureInfo -> IO Word32
    compileAttrs kind sharing bindings captureInfo = do
      bindingWords <- compileBindingWords sharing bindings
      capOff <- compileCaptureInfo captureInfo
      (bindingCount, dataOff) <- emitCounted "attribute set with bindings" (length bindings) bindingWords
      cbcEmit OpAttrs kind bindingCount dataOff capOff 0

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
    -- (nixexpr.cc).
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
      formalWords <- compileFormalEntries setFirstFormalSlot formals
      emitWordList $
        [lambdaNameSymbol name, fi (length formals), if ellipsis then 1 else 0]
          ++ formalWords
    compileFormals name (FormalNamedSet argName formals ellipsis) = do
      Symbol argSym <- symbolIntern argName
      formalWords <- compileFormalEntries namedSetFirstFormalSlot formals
      emitWordList $
        [lambdaNameSymbol name, argSym, fi (length formals), if ellipsis then 1 else 0]
          ++ formalWords

    -- \| A set pattern's formal entries, the first formal in the slot
    -- given, each three words: the name, the kind of its default and the
    -- default's code ('formalDefaultWords').
    compileFormalEntries :: Int -> [Formal] -> IO [Word32]
    compileFormalEntries firstFormal formals = do
      pending <- mapM compilePendingFormal formals
      entries <- shareFormalDefaults firstFormal pending
      pure (concat [[unSymbol name, kind, code] | EvalFormal name def <- entries, let (kind, code) = formalDefaultWords def])

    compilePendingFormal :: Formal -> IO PendingFormal
    compilePendingFormal (Formal name defExpr) = do
      sym <- symbolIntern name
      PendingFormal sym <$> pendingDefault go defExpr

    -- -----------------------------------------------------------------
    -- Let (ELet)
    -- -----------------------------------------------------------------

    compileLet :: [Binding] -> Expr -> CaptureInfo -> IO Word32
    compileLet bindings body captureInfo = do
      bodyIdx <- go body
      bindingWords <- compileBindingWords SiblingsShare bindings
      capOff <- compileCaptureInfo captureInfo
      (bindingCount, dataOff) <- emitCounted "let with bindings" (length bindings) bindingWords
      cbcEmit OpLet 0 bindingCount dataOff bodyIdx capOff

    -- -----------------------------------------------------------------
    -- Bindings (shared by EAttrs and ELet)
    -- -----------------------------------------------------------------

    -- \| The flat data words for a binding list: the names it binds
    -- statically and the bindings with a dynamic name, counted
    -- ('bindingNameCounts'), then its frame's shares, counted, then each
    -- binding's words.  The caller emits them (with the count, via
    -- emitCounted) so a spilled count can precede them contiguously.  The
    -- shares are decided once the bindings are compiled, so every name
    -- they read is interned where compiling interns it.
    compileBindingWords :: FrameSharing -> [Binding] -> IO [Word32]
    compileBindingWords sharing bindings = do
      let BcBindingCounts names dynamics = bindingNameCounts bindings
      namesWord <- countArg "binding set with static names" names
      dynamicsWord <- countArg "binding set with dynamic names" dynamics
      bindingWords <- mapM compileOneBinding bindings
      shares <- case sharing of
        NothingShares -> pure []
        SiblingsShare
          | dynamics == 0 -> slotShares bindings
          | otherwise -> nameShares bindings
      sharesWord <- countArg "binding set with shares" (length shares)
      pure (namesWord : dynamicsWord : sharesWord : concat [[slot, target] | (slot, target) <- shares] ++ concat bindingWords)

    -- \| A function bound to a static name is named by it, as upstream's
    -- @addAttr@ names the value of an attribute path's last name
    -- (parser-state.hh).  Upstream names a function under a dynamic key too,
    -- with the name the key evaluates to, each time it evaluates the set
    -- (@ExprAttrs::eval@ in eval.cc); a name the compiler cannot know, so
    -- such a function is anonymous here.
    compileOneBinding :: Binding -> IO [Word32]
    compileOneBinding (NamedBinding pos (StaticKey name) expr) = do
      sym <- symbolIntern name
      valIdx <- compileNamed (NamedLambda sym) expr
      posWord <- positionWord pos
      pure [bindNamed, attrkeyStatic, unSymbol sym, valIdx, posWord]
    compileOneBinding (NamedBinding pos (DynamicKey keyExpr) expr) = do
      keyIdx <- go keyExpr
      valIdx <- go expr
      posWord <- positionWord pos
      pure [bindNamed, attrkeyDynamic, keyIdx, valIdx, posWord]
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
-- 'EvalFormals', each default compiled and decided as a set pattern's
-- entries hold it ('shareFormalDefaults'), for a function no binding
-- names, interning each name.
compileFormalsToEval :: Formals -> IO EvalFormals
compileFormalsToEval (FormalName name) = EFName <$> symbolIntern name
compileFormalsToEval (FormalSet formals ellipsis) =
  EFSet AnonymousLambda <$> (mapM compileOneFormal formals >>= shareFormalDefaults setFirstFormalSlot) <*> pure ellipsis
compileFormalsToEval (FormalNamedSet name formals ellipsis) =
  EFNamedSet AnonymousLambda <$> symbolIntern name <*> (mapM compileOneFormal formals >>= shareFormalDefaults namedSetFirstFormalSlot) <*> pure ellipsis

-- | The slot of a set pattern's frame its first formal fills, the rest
-- following it: the first, or for a pattern with an \@-pattern the one
-- after the \@-pattern's name, which upstream's @callFunction@ fills
-- first (eval.cc) and the resolver numbers first
-- ('Nix.Expr.Resolve.lexicalScopeFromFormals').
setFirstFormalSlot, namedSetFirstFormalSlot :: Int
setFirstFormalSlot = 0
namedSetFirstFormalSlot = 1

-- | A formal with its default compiled, its name interned after the
-- default's names, as upstream's parser interns them (the @formal@ rule
-- of parser.y).
compileOneFormal :: Formal -> IO PendingFormal
compileOneFormal (Formal name defExpr) = do
  pending <- pendingDefault compileExpr defExpr
  (`PendingFormal` pending) <$> symbolIntern name

-- | A formal whose default is compiled but for a variable of the call's
-- own frame, which waits for the formals' shares to be decided.
data PendingFormal = PendingFormal !Symbol !PendingDefault

-- | A formal's default before the formals' shares are decided.
data PendingDefault
  = -- | None.
    NoPendingDefault
  | -- | Compiled, at a bytecode index.
    CompiledDefault !Word32
  | -- | A variable naming a slot of the call's own frame, not compiled
    -- yet.  Compiling it interns no name.
    FrameSlotDefault !Int

-- | A default compiled as a pending one: a variable of level 0, which
-- names a slot of the call's own frame, is left for
-- 'shareFormalDefaults', and anything else is compiled by @compileDefault@.
pendingDefault :: (Expr -> IO Word32) -> Maybe Expr -> IO PendingDefault
pendingDefault _ Nothing = pure NoPendingDefault
pendingDefault _ (Just (EResolvedVar 0 slot)) = pure (FrameSlotDefault slot)
pendingDefault compileDefault (Just defExpr) = CompiledDefault <$> compileDefault defExpr

-- | Formals with their defaults decided, the first formal in the slot
-- given, as upstream's @callFunction@ fills a set pattern's frame through
-- @maybeThunk@ (eval.cc): the @-pattern's slot first, then the formals'
-- in symbol order (@validateFormals@ in parser-state.hh).  A default that
-- is a variable naming the @-pattern, or a formal 'siblingShares' says is
-- filled before its own, shares that slot's cell ('SharedDefault'); one
-- naming a formal filled after its own is a variable of its own
-- ('Default').  Either stays a variable naming the slot it names: which
-- cell a formal's slot holds depends on whether a call's argument holds the
-- formal, so a chain of shared defaults ends where the call finds it.
shareFormalDefaults :: Int -> [PendingFormal] -> IO [EvalFormal]
shareFormalDefaults firstFormal pending = mapM decide (zip [firstFormal ..] pending)
  where
    names = Map.fromList (zip [firstFormal ..] [name | PendingFormal name _ <- pending])
    shared = Set.fromList (map fst (siblingShares (`Map.lookup` names) [SiblingVar name slot target | (slot, PendingFormal name (FrameSlotDefault target)) <- zip [firstFormal ..] pending, target >= firstFormal]))
    decide (slot, PendingFormal name pendingDef) =
      EvalFormal name <$> case pendingDef of
        NoPendingDefault -> pure NoDefault
        CompiledDefault code -> pure (Default code)
        FrameSlotDefault target
          | target < firstFormal || Set.member slot shared -> SharedDefault <$> frameSlotVar target
          | otherwise -> Default <$> frameSlotVar target
    frameSlotVar slot = cbcEmit OpResolvedVar 0 0 0 (fromIntegral slot) 0

-- | The kind and code words of a formal's default, as a set pattern's
-- entries and a C lambda's hold them.
formalDefaultWords :: FormalDefault -> (Word32, Word32)
formalDefaultWords NoDefault = (formalNoDefault, 0)
formalDefaultWords (Default code) = (formalDefault, code)
formalDefaultWords (SharedDefault code) = (formalSharedDefault, code)

-- | The default 'formalDefaultWords' writes as a kind and a code word,
-- or 'Nothing' for a kind it never writes.
formalDefaultFromWords :: Word32 -> Word32 -> Maybe FormalDefault
formalDefaultFromWords kind code
  | kind == formalNoDefault = Just NoDefault
  | kind == formalDefault = Just (Default code)
  | kind == formalSharedDefault = Just (SharedDefault code)
  | otherwise = Nothing

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
-- the offset of its words.  'Nothing' for any other type, and a formal's
-- default kind the compiler never writes ('formalDefaultFromWords') is
-- handed to @unknownKind@.  The offset is strict, though an unknown type
-- never reads it, so a caller passes it evaluated rather than as a thunk.
decodeBcFormals :: (MonadCData m) => (Word32 -> m FormalDefault) -> Word8 -> Word32 -> m (Maybe EvalFormals)
decodeBcFormals unknownKind flags !dataOff
  | flags == formalName = Just . EFName . Symbol <$> cbcData dataOff
  | flags == formalSet = do
      lambdaSym <- cbcData dataOff
      count <- cbcData (dataOff + 1)
      ellipsis <- cbcData (dataOff + 2)
      formals <- decodeFormalEntries unknownKind (fromIntegral count) (dataOff + 3)
      pure (Just (EFSet (lambdaNameFromSymbol lambdaSym) formals (ellipsis /= 0)))
  | flags == formalNamedSet = do
      lambdaSym <- cbcData dataOff
      nameSym <- cbcData (dataOff + 1)
      count <- cbcData (dataOff + 2)
      ellipsis <- cbcData (dataOff + 3)
      formals <- decodeFormalEntries unknownKind (fromIntegral count) (dataOff + 4)
      pure (Just (EFNamedSet (lambdaNameFromSymbol lambdaSym) (Symbol nameSym) formals (ellipsis /= 0)))
  | otherwise = pure Nothing

-- | Decode a list of formal entries from the data buffer.
-- Each entry is 3 words: the name's symbol, then the default's kind and
-- code ('formalDefaultFromWords'), a kind the compiler never writes
-- handed to @unknownKind@.
decodeFormalEntries :: (MonadCData m) => (Word32 -> m FormalDefault) -> Int -> Word32 -> m [EvalFormal]
decodeFormalEntries _ 0 _ = pure []
decodeFormalEntries unknownKind n off = do
  nameSym <- cbcData off
  kind <- cbcData (off + 1)
  code <- cbcData (off + 2)
  def <- maybe (unknownKind kind) pure (formalDefaultFromWords kind code)
  rest <- decodeFormalEntries unknownKind (n - 1) (off + 3)
  pure (EvalFormal (Symbol nameSym) def : rest)

-- ---------------------------------------------------------------------------
-- Capture info reading (for evalBcLambda, evalBcAttrs and evalBcLet)
-- ---------------------------------------------------------------------------

-- | A closure's or frame's capture info, its captured slots left in the
-- data buffer for 'foldBcCapturedSlots' to read.
data BcCaptureInfo
  = -- | The closure runs in the env it is made in ('NoCaptureInfo').
    BcNoCaptureInfo
  | -- | The closure runs in an env of the captured slots alone
    -- ('Captures').
    BcCaptures !BcCapturedSlots
  | -- | The closure runs in an env of the captured slots that sees the
    -- with-scopes of the env it is made in ('CapturesWithScopes').
    BcCapturesWithScopes !BcCapturedSlots

-- | The slots a closure captures: their count and the data offset of the
-- first one's (level, index) pair.
data BcCapturedSlots = BcCapturedSlots !Int !Word32

-- | The capture info at a data offset, or the tag there when it is none
-- of 'captureNone', 'captureSlots' and 'captureWithScopes'.
decodeBcCaptureInfo :: (MonadCData m) => Word32 -> m (Either Word32 BcCaptureInfo)
decodeBcCaptureInfo dataOff = cbcData dataOff >>= decodeTagged
  where
    decodeTagged tag
      | tag == captureNone = pure (Right BcNoCaptureInfo)
      | tag == captureSlots = Right . BcCaptures <$> capturedSlots
      | tag == captureWithScopes = Right . BcCapturesWithScopes <$> capturedSlots
      | otherwise = pure (Left tag)
    capturedSlots = do
      count <- cbcData (dataOff + 1)
      pure (BcCapturedSlots (fromIntegral count) (dataOff + 2))

-- | Fold over captured slots, first to last, each given as the resolved
-- level and index the slot is read from in the env the closure is made
-- in.  Each pair's words are read as the fold reaches them.
{-# INLINE foldBcCapturedSlots #-}
foldBcCapturedSlots :: (MonadCData m) => (acc -> Word32 -> Word32 -> m acc) -> acc -> BcCapturedSlots -> m acc
foldBcCapturedSlots step start (BcCapturedSlots count dataOff) = go start count dataOff
  where
    go !acc 0 !_ = pure acc
    go !acc n !off = do
      level <- cbcData off
      idx <- cbcData (off + 1)
      next <- step acc level idx
      go next (n - 1) (off + 2)

-- ---------------------------------------------------------------------------
-- Binding reading (for evalBcAttrs and evalBcLet)
-- ---------------------------------------------------------------------------

-- | The bindings of a set or a let: their names counted, the frame's
-- shares, then their count, resolved through the short_arg spill, and the
-- data offset of the first.
data BcBindings = BcBindings !BcBindingCounts !BcShares !Int !Word32

-- | How many names bindings bind statically, one for each static key and
-- inherited variable and one for each name of an @inherit (from)@, and
-- how many bindings have a dynamic name.
data BcBindingCounts = BcBindingCounts !Int !Int

-- | The names of a binding list counted, as the compiler writes them at
-- the head of its words.  The bindings' variables are resolved by slot
-- exactly when none has a dynamic name, one slot per static name in
-- declaration order: this must stay in sync with @allStaticSingleKey@
-- and @lexicalScopeFromBindings@ in "Nix.Expr.Resolve".
bindingNameCounts :: [Binding] -> BcBindingCounts
bindingNameCounts = foldl' countOne (BcBindingCounts 0 0)
  where
    countOne (BcBindingCounts names dynamics) binding = case binding of
      NamedBinding _ (StaticKey _) _ -> BcBindingCounts (names + 1) dynamics
      NamedBinding _ (DynamicKey _) _ -> BcBindingCounts names (dynamics + 1)
      Inherit {} -> BcBindingCounts (names + 1) dynamics
      InheritFrom _ inherited -> BcBindingCounts (names + length inherited) dynamics

-- | Whether a recursive set's bindings bind 'overridesName' statically,
-- as upstream looks for it among a recursive set's static attributes, an
-- inherited one included (@ExprAttrs::eval@ in eval.cc).  Its instruction
-- then holds 'attrsRecOverrides' rather than 'attrsRec'.
bindsOverrides :: [Binding] -> Bool
bindsOverrides = any bindsName
  where
    bindsName binding = case binding of
      NamedBinding _ (StaticKey name) _ -> name == overridesName
      NamedBinding _ (DynamicKey _) _ -> False
      Inherit _ name _ -> name == overridesName
      InheritFrom _ inherited -> overridesName `elem` [name | InheritedName _ name <- inherited]

-- | The attribute a recursive set overrides its own attributes with
-- upstream (@sOverrides@ in eval.cc at 2.24.9, @StaticEvalSymbols@'s
-- @overrides@ in eval.hh at 2.36.0).
overridesName :: Text
overridesName = "__overrides"

-- | Whether a binding list's values may share the cells of its own names:
-- a recursive set's and a let's, but not a recursive set's that binds
-- @__overrides@, whose values upstream makes thunks of their own
-- (@ExprAttrs::eval@ in eval.cc), or a plain set's, which binds no name
-- its values read.
data FrameSharing = SiblingsShare | NothingShares

-- | A binding of a frame being tied whose whole value is a variable naming
-- another binding of the same frame: the name the binding binds, the key
-- its cell is kept under, and the key of the binding the variable names.
data SiblingVar k = SiblingVar !Symbol !k !k

-- | Of a frame's sibling variables, each a t'SiblingVar', the ones that
-- share a sibling's cell where upstream's do, each as its binding's key
-- and the key of the binding whose cell it takes, in the order the frame
-- fills them.  Upstream fills a frame's slots in symbol order, from an
-- env whose slots start empty (@allocEnv@ in eval-inline.hh), and
-- @ExprVar::maybeThunk@ binds a variable to its slot's cell when the slot
-- is filled and to a thunk of its own when it is not
-- (eval.cc).  So a variable shares the cell of a sibling whose name has the
-- lesser symbol, and keeps its own thunk otherwise, as a variable naming its
-- own binding does.  A sibling that shares holds the cell it takes, so each
-- pair names the binding at the end of the chain, whose cell is its own, and
-- the pairs can be copied in any order.  @nameOf@ gives the name the binding
-- under a key binds.
siblingShares :: (Ord k) => (k -> Maybe Symbol) -> [SiblingVar k] -> [(k, k)]
siblingShares nameOf siblings = reverse shares
  where
    SharesMade _ shares = foldl' share (SharesMade Map.empty []) (sortOn bindingName siblings)
    bindingName (SiblingVar name _ _) = name
    share made@(SharesMade roots found) (SiblingVar name key target) = case nameOf target of
      Just targetName
        | targetName < name ->
            let root = Map.findWithDefault target target roots
             in SharesMade (Map.insert key root roots) ((key, root) : found)
      _ -> made

-- | The shares 'siblingShares' has made so far: the binding at the end of
-- each sharing binding's chain, and the pairs, last first.
data SharesMade k = SharesMade !(Map.Map k k) ![(k, k)]

-- | Each slot a binding list's static names fill, in declaration order:
-- the name, and the slot of the frame its value names when that value is
-- a variable of the frame's own slots.
staticSlots :: [Binding] -> [(Text, Maybe Int)]
staticSlots = concatMap slotsOf
  where
    slotsOf (NamedBinding _ (StaticKey name) value) = [(name, frameSlot value)]
    slotsOf (NamedBinding _ (DynamicKey _) _) = []
    slotsOf (Inherit _ name _) = [(name, Nothing)]
    slotsOf (InheritFrom _ inherited) = [(name, Nothing) | InheritedName _ name <- inherited]
    frameSlot (EResolvedVar 0 slot) = Just slot
    frameSlot _ = Nothing

-- | The shares of a binding list whose names are in slots, as slot pairs
-- ('siblingShares').  Its names are interned by then.
slotShares :: [Binding] -> IO [(Word32, Word32)]
slotShares bindings = do
  let slots = staticSlots bindings
  names <- traverse (symbolIntern . fst) slots
  let siblings = [SiblingVar sym slot target | (slot, sym, Just target) <- zip3 [0 ..] names (map snd slots)]
  pure [(fromIntegral slot, fromIntegral target) | (slot, target) <- siblingShares (`Map.lookup` Map.fromList (zip [0 ..] names)) siblings]

-- | The shares of a binding list whose static names are in a name table,
-- as symbol pairs ('siblingShares'): a static name's value that is a
-- variable the resolver left to the table names another static name.
-- Its names are interned by then.
nameShares :: [Binding] -> IO [(Word32, Word32)]
nameShares bindings = do
  let staticNames = Set.fromList (map fst (staticSlots bindings))
  siblings <-
    sequence
      [ (\sym target -> SiblingVar sym sym target) <$> symbolIntern own <*> symbolIntern name
      | NamedBinding _ (StaticKey own) (EVar name) <- bindings,
        Set.member name staticNames
      ]
  pure [(unSymbol key, unSymbol target) | (key, target) <- siblingShares Just siblings]

-- | The shares of a frame's bindings, in the order the frame fills them:
-- their count and the data offset of the first one's pair of words, each
-- pair the slot or the symbol of a binding whose whole value is a
-- variable naming another of the frame's names and the one whose cell it
-- takes ('siblingShares').
data BcShares = BcShares !Int !Word32

-- | The bindings in an instruction's counted payload: @count@ bindings,
-- resolved through the short_arg spill, whose words start at @dataOff@
-- with their names counted and their shares after the counts.
decodeBcBindings :: (MonadCData m) => Int -> Word32 -> m BcBindings
decodeBcBindings count dataOff = do
  names <- cbcData dataOff
  dynamics <- cbcData (dataOff + 1)
  shares <- cbcData (dataOff + 2)
  let firstShare = dataOff + 3
  pure
    ( BcBindings
        (BcBindingCounts (fromIntegral names) (fromIntegral dynamics))
        (BcShares (fromIntegral shares) firstShare)
        count
        (firstShare + 2 * shares)
    )

-- | Fold over a frame's shares, first to last, each given as the slot or
-- symbol that takes a cell and the one whose cell it takes.  Each pair's
-- words are read as the fold reaches them.
{-# INLINE foldBcShares #-}
foldBcShares :: (MonadCData m) => (acc -> Word32 -> Word32 -> m acc) -> acc -> BcShares -> m acc
foldBcShares step start (BcShares count dataOff) = go start count dataOff
  where
    go !acc 0 !_ = pure acc
    go !acc n !off = do
      key <- cbcData off
      target <- cbcData (off + 1)
      next <- step acc key target
      go next (n - 1) (off + 2)

-- | A binding of a set or a let, each name at the position it was
-- defined at.
data BcBinding
  = -- | @name = expr@: its position, the name's symbol and the value's
    -- bc_idx
    BcNamed !PosIdx !Word32 !Word32
  | -- | @${key} = expr@: its position, the bc_idx of the expression the
    -- name is computed by and the value's bc_idx
    BcDynamic !PosIdx !Word32 !Word32
  | -- | One name of @inherit x@: its position, its symbol and the bc_idx
    -- of the variable it copies, which is evaluated in the env around the
    -- binding set
    BcInherit !PosIdx !Word32 !Word32
  | -- | @inherit (from) names@: the from-expr's bc_idx and the names,
    -- which 'foldBcInheritedNames' reads
    BcInheritFrom !Word32 !BcInheritedNames

-- | The names of an @inherit (from) names@ binding: their count and the
-- data offset of the first.
data BcInheritedNames = BcInheritedNames !Int !Word32

-- | One name of an @inherit (from) names@ binding: its position, its
-- symbol and the bc_idx of the select that reads it from slot 0 of an env
-- holding the from-value.
data BcInheritedName = BcInheritedName !PosIdx !Word32 !Word32

-- | Fold over bindings, first to last, each read from the data buffer as
-- the fold reaches it.  The data buffer only grows, so a step may compile
-- code.  A binding tag that is none of 'bindNamed', 'bindInherit' and
-- 'bindInheritFrom' ends the fold with @unknownTag@ applied to it.
--
-- It is inlined wherever it is given its two functions, as a partial
-- application of it is.  Each binding is built where the step is applied
-- to it, so a step that is inlined too (an INLINE pragma on it) takes
-- the binding's fields apart without allocating it.
{-# INLINE foldBcBindings #-}
foldBcBindings :: (MonadCData m) => (Word32 -> m acc) -> (acc -> BcBinding -> m acc) -> acc -> BcBindings -> m acc
foldBcBindings unknownTag step = foldFrom
  where
    foldFrom start (BcBindings _ _ count dataOff) = go start count dataOff
    go !acc 0 !_ = pure acc
    go !acc n !off = cbcData off >>= stepTagged
      where
        stepTagged tag
          | tag == bindNamed = do
              isExpr <- cbcData (off + 1)
              keyWord <- cbcData (off + 2)
              valBcIdx <- cbcData (off + 3)
              pos <- PosIdx <$> cbcData (off + 4)
              if isExpr /= 0
                then step acc (BcDynamic pos keyWord valBcIdx) >>= continueAt (off + 5)
                else step acc (BcNamed pos keyWord valBcIdx) >>= continueAt (off + 5)
          | tag == bindInherit = do
              sym <- cbcData (off + 1)
              valBcIdx <- cbcData (off + 2)
              pos <- PosIdx <$> cbcData (off + 3)
              step acc (BcInherit pos sym valBcIdx) >>= continueAt (off + 4)
          | tag == bindInheritFrom = do
              fromBcIdx <- cbcData (off + 1)
              nameCount <- cbcData (off + 2)
              step acc (BcInheritFrom fromBcIdx (BcInheritedNames (fromIntegral nameCount) (off + 3)))
                >>= continueAt (off + 3 + 3 * nameCount)
          | otherwise = unknownTag tag
        continueAt nextOff next = go next (n - 1) nextOff

-- | Fold over the names of an @inherit (from)@ binding, first to last.
{-# INLINE foldBcInheritedNames #-}
foldBcInheritedNames :: (MonadCData m) => (acc -> BcInheritedName -> m acc) -> acc -> BcInheritedNames -> m acc
foldBcInheritedNames step start (BcInheritedNames count dataOff) = go start count dataOff
  where
    go !acc 0 !_ = pure acc
    go !acc n !off = do
      sym <- cbcData off
      selectBcIdx <- cbcData (off + 1)
      pos <- cbcData (off + 2)
      next <- step acc (BcInheritedName (PosIdx pos) sym selectBcIdx)
      go next (n - 1) (off + 3)

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
