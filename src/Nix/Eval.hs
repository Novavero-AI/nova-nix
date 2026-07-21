{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

-- | Nix expression evaluator.
--
-- Nix evaluation is LAZY.  Attribute set members and list elements
-- are stored as thunks and only forced when their value is demanded.
-- Function arguments are likewise thunked - @(x: 1) (throw "boom")@
-- returns @1@ because @x@ is never referenced.
--
-- The evaluator maintains an environment ('Env') that maps variable
-- names to thunks.  @let@, @with@, function application, and
-- recursive attribute sets all extend the environment.
module Nix.Eval
  ( -- * Values (re-exported from Types)
    NixValue (..),
    CompiledRegex (..),
    Thunk (..),

    -- * Attribute sets (re-exported from Types)
    AttrSet (..),
    CAttrSet,
    attrSetFromMap,
    attrSetLookup,
    attrSetKeys,
    attrSetToMap,
    attrSetToAscList,
    attrSetMember,
    attrSetElems,
    attrSetNull,
    attrSetRemoveKeys,
    attrSetSize,

    -- * String context (re-exported from Types)
    StringContextElement (..),
    StringContext (..),
    emptyContext,
    mkStr,

    -- * Environment (re-exported from Types)
    Env (..),
    emptyEnv,

    -- * Evaluation monad (re-exported from Types)
    MonadEval (..),
    PureEval,
    runPureEval,

    -- * Evaluation
    eval,
    evalBytecode,
    force,

    -- * Helpers (for Builtins)
    typeName,
    evaluated,
    readThunkValue,

    -- * Fetcher transport validation (pure, exported for tests)
    checkGitUrl,

    -- * Builtin registry
    BuiltinDef (..),
    builtinRegistry,
    builtinNames,

    -- * Platform
    currentSystemStr,
  )
where

import Control.Monad (foldM, forM_, when, (>=>))
import qualified Crypto.Hash as CH
import qualified Data.Array as Array
import Data.Bits (complement, xor, (.&.), (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.List (find, foldl', partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, castPtr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Nix.Derivation (Derivation (..), DerivationOutput (..), textToPlatform, toATerm, toATermForHash)
import Nix.Eval.CBytecode (cbcArg1, cbcArg2, cbcArg3, cbcCountedPayload, cbcData, cbcFlags, cbcOpcode, cbcShortArg, pattern OpApp, pattern OpAssert, pattern OpAttrs, pattern OpBinary, pattern OpHasAttr, pattern OpIf, pattern OpIndStr, pattern OpLambda, pattern OpLet, pattern OpList, pattern OpLitBool, pattern OpLitFloat, pattern OpLitInt, pattern OpLitNull, pattern OpLitPath, pattern OpLitUri, pattern OpResolvedVar, pattern OpSearchPath, pattern OpSelect, pattern OpStr, pattern OpUnary, pattern OpVar, pattern OpWith, pattern OpWithVar)
import Nix.Eval.CEnv (cenvPushWith)
import Nix.Eval.CList (CList (..), clistGet)
import Nix.Eval.CThunk (CThunkPtr)
import Nix.Eval.CanonPath (canonPath)
import Nix.Eval.Compile (BcAttrKey (..), BcBinding (..), compileExpr, decodeBcBindings, decodeBcCaptureInfo, decodeBcFormals, reassembleDouble, reassembleInt64)
import Nix.Eval.Context (extractAllOutputRefs, extractInputDrvs, extractInputSrcs, plainContext)
import Nix.Eval.Operator (checkedAdd, checkedMul, checkedSub, evalBinary, evalUnary, nixCompare, nixEqual)
import Nix.Eval.StringInterp (coerceToString, formatJsonFloat, formatNixFloat, formatXmlFloat, stripIndentedChunks)
import Nix.Eval.Symbol (Symbol (..), symbolBytes, symbolText)
import Nix.Eval.Types
  ( AttrSet (..),
    CAttrSet,
    CompiledRegex (..),
    Env (..),
    EvalFormal (..),
    EvalFormals (..),
    MonadEval (..),
    NixValue (..),
    PureEval,
    StringContext (..),
    StringContextElement (..),
    Thunk (..),
    allocCSlots,
    attrSetElems,
    attrSetFromMap,
    attrSetKeys,
    attrSetLookup,
    attrSetMapWithKey,
    attrSetMember,
    attrSetNull,
    attrSetRemoveKeys,
    attrSetSize,
    attrSetToAscList,
    attrSetToMap,
    buildCAttrSetKeys,
    buildCSlots,
    bytesToTextLossy,
    cheapThunkBc,
    checkedCPtr,
    clistFromThunks,
    clistLen,
    clistThunks,
    emptyContext,
    emptyEnv,
    envFromSlots,
    envLookup,
    envLookupResolved,
    envWithScopesRaw,
    evaluated,
    fillCAttrSetValues,
    fillCSlots,
    mkStr,
    mkStrBytes,
    mkSyntheticThunk,
    mkThunk,
    mkThunkBc,
    newCEnv,
    newMinimalEnv,
    readThunkValue,
    runPureEval,
    storePathOrThrow,
    thunkToCPtr,
    typeName,
    withScopesForCapture,
  )
import Nix.Expr.Types
  ( AttrKey (..),
    BinaryOp (..),
    CaptureInfo (..),
    Expr (..),
    NixAtom (..),
    UnaryOp (..),
  )
import Nix.Hash (base64HashLen, bytesToHexText, hashAlgoBytes, hashPlaceholder, hexHashLen, hexToBytes, makeFixedOutputPath, makeOutputPath, makeTextPath, nix32HashLen, sha256Digest, sha256Hex)
import Nix.Store.Path (StorePath (..), StorePathNameError (..), checkStorePathName, defaultStoreDir, defaultStoreDirText, parseStorePath, storePathNameErrorText, storePathNameReasonText, storePathToText)
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Base64 as B64
import qualified NovaCache.NAR as NAR
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info
import Text.Regex.TDFA (matchAllText)
import qualified Text.Regex.TDFA as RE
import Text.Regex.TDFA.ByteString ()

-- | Evaluate a Nix expression in an environment.
-- Compiles the expression to bytecode and dispatches to 'evalBytecode'.
eval :: (MonadEval m) => Env -> Expr -> m NixValue
eval env expr =
  let bcIdx = unsafePerformIO (compileExpr expr)
   in evalBytecode env bcIdx

-- | Force a thunk to a value.
--
-- Delegates to 'forceThunk' which is a 'MonadEval' method - this
-- allows IO evaluators to implement memoization (caching the result
-- after the first force) while pure evaluators simply re-evaluate.
force :: (MonadEval m) => Thunk -> m NixValue
force = forceThunk evalBytecode

-- | Strict UTF-8 decode at a Text-typed boundary (attr names, filesystem
-- paths, algorithm names, ...).  A Nix string is a byte string; where the
-- implementation needs 'Text', invalid UTF-8 is a clean eval error - never
-- a lossy replacement, which would smuggle a DIFFERENT string through an
-- identity-bearing boundary.
decodedText :: (MonadEval m) => Text -> BS.ByteString -> m Text
decodedText what bytes = case TE.decodeUtf8' bytes of
  Right t -> pure t
  Left _ -> throwEvalError (what <> ": invalid UTF-8 in string")

-- | Evaluate a bytecode instruction by index.
-- This is the primary evaluator - reads opcodes from the C bytecode
-- store and dispatches.  All recursive evaluation goes through this
-- function, never through 'eval' directly.
evalBytecode :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBytecode env bcIdx =
  let opcode = unsafePerformIO (cbcOpcode bcIdx)
   in case opcode of
        OpLitInt ->
          let lo = unsafePerformIO (cbcArg1 bcIdx)
              hi = unsafePerformIO (cbcArg2 bcIdx)
           in pure (VInt (reassembleInt64 lo hi))
        OpLitFloat ->
          let lo = unsafePerformIO (cbcArg1 bcIdx)
              hi = unsafePerformIO (cbcArg2 bcIdx)
           in pure (VFloat (reassembleDouble lo hi))
        OpLitBool ->
          let flag = unsafePerformIO (cbcShortArg bcIdx)
           in pure (VBool (flag /= 0))
        OpLitNull -> pure VNull
        OpLitUri ->
          let sym = unsafePerformIO (cbcArg1 bcIdx)
           in pure (mkStrBytes (symbolBytes (Symbol sym)))
        OpLitPath ->
          let sym = unsafePerformIO (cbcArg1 bcIdx)
           in VPath <$> resolvePathLiteral (symbolText (Symbol sym))
        OpStr -> evalBcStr env bcIdx
        OpIndStr -> evalBcIndStr env bcIdx
        OpVar ->
          let sym = unsafePerformIO (cbcArg1 bcIdx)
           in evalVar env (symbolText (Symbol sym))
        OpWithVar ->
          let sym = unsafePerformIO (cbcArg1 bcIdx)
              name = symbolText (Symbol sym)
           in evalWithVar env name
        OpResolvedVar ->
          let level = fromIntegral (unsafePerformIO (cbcArg1 bcIdx))
              idx = fromIntegral (unsafePerformIO (cbcArg2 bcIdx))
           in force (envLookupResolved level idx env)
        OpAttrs -> evalBcAttrs env bcIdx
        OpList -> evalBcList env bcIdx
        OpSelect -> evalBcSelect env bcIdx
        OpHasAttr -> evalBcHasAttr env bcIdx
        OpApp -> evalBcApp env bcIdx
        OpLambda -> evalBcLambda env bcIdx
        OpLet -> evalBcLet env bcIdx
        OpIf ->
          let condIdx = unsafePerformIO (cbcArg1 bcIdx)
              thenIdx = unsafePerformIO (cbcArg2 bcIdx)
              elseIdx = unsafePerformIO (cbcArg3 bcIdx)
           in do
                condVal <- evalBytecode env condIdx
                case condVal of
                  VBool True -> evalBytecode env thenIdx
                  VBool False -> evalBytecode env elseIdx
                  _ -> throwEvalError ("'if' condition must be a Boolean, got " <> typeName condVal)
        OpWith ->
          let scopeIdx = unsafePerformIO (cbcArg1 bcIdx)
              bodyIdx = unsafePerformIO (cbcArg2 bcIdx)
              -- Lazy with: defer forcing the scope until a WITH_VAR lookup
              -- actually needs it.  This is critical for nixpkgs where
              -- all-packages.nix uses `with pkgs;` inside a fixpoint -
              -- eagerly forcing the scope would blackhole.
              scopeThunk = cheapThunkBc env scopeIdx
           in evalBytecode (pushLazyWithScope scopeThunk env) bodyIdx
        OpAssert ->
          let condIdx = unsafePerformIO (cbcArg1 bcIdx)
              bodyIdx = unsafePerformIO (cbcArg2 bcIdx)
           in do
                condVal <- evalBytecode env condIdx
                case condVal of
                  VBool True -> evalBytecode env bodyIdx
                  -- A failed assert is catchable by tryEval (upstream
                  -- AssertionError); a non-bool condition is a type error.
                  VBool False -> throwCatchableError "assertion failed"
                  _ -> throwEvalError ("assertion condition must be a Boolean, got " <> typeName condVal)
        OpUnary ->
          let flags_ = unsafePerformIO (cbcFlags bcIdx)
              operandIdx = unsafePerformIO (cbcArg1 bcIdx)
           in do
                val <- evalBytecode env operandIdx
                evalUnary (decodeUnaryOp flags_) val
        OpBinary -> evalBcBinary env bcIdx
        OpSearchPath ->
          let sym = unsafePerformIO (cbcArg1 bcIdx)
           in evalSearchPath env (symbolText (Symbol sym))
        _ -> throwEvalError "evalBytecode: unknown opcode"

-- ---------------------------------------------------------------------------
-- Bytecode helpers
-- ---------------------------------------------------------------------------

-- | Decode a UnaryOp from bytecode flags.
decodeUnaryOp :: Word8 -> UnaryOp
decodeUnaryOp 0 = OpNot
decodeUnaryOp 1 = OpNegate
-- Unreachable: the tag is written by Nix.Eval.Compile's encodeUnaryOp, which
-- only emits 0 or 1.
decodeUnaryOp n = error ("decodeUnaryOp: unknown tag " <> show n)

-- | Decode a BinaryOp from bytecode flags.
decodeBinaryOp :: Word8 -> BinaryOp
decodeBinaryOp 0 = OpAdd
decodeBinaryOp 1 = OpSub
decodeBinaryOp 2 = OpMul
decodeBinaryOp 3 = OpDiv
decodeBinaryOp 4 = OpAnd
decodeBinaryOp 5 = OpOr
decodeBinaryOp 6 = OpImpl
decodeBinaryOp 7 = OpEq
decodeBinaryOp 8 = OpNeq
decodeBinaryOp 9 = OpLt
decodeBinaryOp 10 = OpLte
decodeBinaryOp 11 = OpGt
decodeBinaryOp 12 = OpGte
decodeBinaryOp 13 = OpConcat
decodeBinaryOp 14 = OpUpdate
-- Unreachable: the tag is written by Nix.Eval.Compile's encodeBinaryOp, which
-- only emits 0..14.
decodeBinaryOp n = error ("decodeBinaryOp: unknown tag " <> show n)

-- | Evaluate a binary operation from bytecode, with short-circuit
-- support for &&, ||, ->.
evalBcBinary :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcBinary env bcIdx0 =
  let flags_ = unsafePerformIO (cbcFlags bcIdx0)
      leftIdx = unsafePerformIO (cbcArg1 bcIdx0)
      rightIdx = unsafePerformIO (cbcArg2 bcIdx0)
      op = decodeBinaryOp flags_
   in case op of
        OpAnd -> evalShortCircuitAnd env leftIdx rightIdx
        OpOr -> evalShortCircuitOr env leftIdx rightIdx
        OpImpl -> evalShortCircuitImpl env leftIdx rightIdx
        OpAdd -> do
          leftVal <- evalBytecode env leftIdx
          rightVal <- evalBytecode env rightIdx
          evalAddWithCoercion leftVal rightVal
        _ -> do
          leftVal <- evalBytecode env leftIdx
          rightVal <- evalBytecode env rightIdx
          evalBinary force op leftVal rightVal

-- | Addition with string coercion fallback, matching C++ Nix behavior.
-- C++ Nix's ExprOpAdd falls through to concatStrings when operands
-- are not both numeric and neither is a path.  concatStrings calls
-- coerceToString on each part, which handles attrsets via outPath.
evalAddWithCoercion :: (MonadEval m) => NixValue -> NixValue -> m NixValue
evalAddWithCoercion left right = case (left, right) of
  -- Numeric: direct arithmetic (matches C++ Nix priority)
  (VFloat _, _) -> evalBinary force OpAdd left right
  (_, VFloat _) -> evalBinary force OpAdd left right
  (VInt _, VInt _) -> evalBinary force OpAdd left right
  -- Path + path: text concatenation, canonicalized - the joined spelling
  -- (dot segments, doubled separators) never survives into the value, as
  -- upstream (CanonPath on the concatenated text).
  (VPath a, VPath b) -> pure (VPath (canonPath (a <> b)))
  -- Path + coercible: the result stays a path, the right side coerces
  -- WITHOUT a store copy, and a right side carrying string context is an
  -- error, as upstream (a store-path reference cannot survive inside a
  -- path value).  Paths are Text in nova, so the appended bytes must
  -- decode; invalid UTF-8 cannot form a filesystem path here.
  (VPath a, _) -> do
    (rightStr, rightCtx) <- coerceAddOperand right
    if rightCtx == emptyContext
      then do
        appended <- decodedText "path concatenation" rightStr
        pure (VPath (canonPath (a <> appended)))
      else throwEvalError "cannot append a string with context (a store-path reference) to a path"
  -- Strings: direct concat.
  (VStr {}, VStr {}) -> evalBinary force OpAdd left right
  -- Otherwise: string concatenation with STRICT coercion (Nix coerceMore=false)
  -- - only strings, sets with __toString/outPath, and paths coerce; numbers,
  -- bools, null, lists, and functions are type errors, matching C++ Nix's `+`.
  -- A path on the right of a string is COPIED to the store (upstream
  -- copyToStore=true), so "x" + ./src concatenates the source's store path
  -- and carries it in the result's context.
  _ -> do
    (leftStr, leftCtx) <- coerceAddOperand left
    (rightStr, rightCtx) <- coerceAddOperand right
    pure (VStr (leftStr <> rightStr) (leftCtx <> rightCtx))

-- | Strict string coercion for the @+@ operator (Nix @coerceMore = false@):
-- strings, sets with @__toString@\/@outPath@, and paths (copied to the
-- store, carrying context) coerce; numbers, booleans, null, lists, and
-- functions are type errors.
coerceAddOperand :: (MonadEval m) => NixValue -> m (BS.ByteString, StringContext)
coerceAddOperand v@(VStr {}) = coerceToString False force applyValue v
coerceAddOperand v@(VAttrs {}) = coerceToString False force applyValue v
coerceAddOperand v@(VPath _) = coerceToStoreString v
coerceAddOperand v =
  throwEvalError ("cannot coerce " <> typeName v <> " to a string with the + operator")

-- | Bytecode short-circuit &&
evalShortCircuitAnd :: (MonadEval m) => Env -> Word32 -> Word32 -> m NixValue
evalShortCircuitAnd env leftIdx rightIdx = do
  leftVal <- evalBytecode env leftIdx
  case leftVal of
    VBool False -> pure (VBool False)
    VBool True -> do
      rightVal <- evalBytecode env rightIdx
      case rightVal of
        VBool _ -> pure rightVal
        _ -> throwEvalError ("second operand of && must be a Boolean, got " <> typeName rightVal)
    _ -> throwEvalError ("first operand of && must be a Boolean, got " <> typeName leftVal)

-- | Bytecode short-circuit ||
evalShortCircuitOr :: (MonadEval m) => Env -> Word32 -> Word32 -> m NixValue
evalShortCircuitOr env leftIdx rightIdx = do
  leftVal <- evalBytecode env leftIdx
  case leftVal of
    VBool True -> pure (VBool True)
    VBool False -> do
      rightVal <- evalBytecode env rightIdx
      case rightVal of
        VBool _ -> pure rightVal
        _ -> throwEvalError ("second operand of || must be a Boolean, got " <> typeName rightVal)
    _ -> throwEvalError ("first operand of || must be a Boolean, got " <> typeName leftVal)

-- | Bytecode short-circuit ->
evalShortCircuitImpl :: (MonadEval m) => Env -> Word32 -> Word32 -> m NixValue
evalShortCircuitImpl env leftIdx rightIdx = do
  leftVal <- evalBytecode env leftIdx
  case leftVal of
    VBool False -> pure (VBool True)
    VBool True -> do
      rightVal <- evalBytecode env rightIdx
      case rightVal of
        VBool _ -> pure rightVal
        _ -> throwEvalError ("second operand of -> must be a Boolean, got " <> typeName rightVal)
    _ -> throwEvalError ("first operand of -> must be a Boolean, got " <> typeName leftVal)

-- | Evaluate a string literal from bytecode data buffer.
evalBcStr :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcStr env bcIdx0 = do
  let (count, dataOff) =
        unsafePerformIO (cbcCountedPayload bcIdx0 =<< cbcArg1 bcIdx0)
  chunks <- evalBcStringParts env count dataOff
  pure (VStr (BS.concat [t | (_, t, _) <- chunks]) (mconcat [c | (_, _, c) <- chunks]))

-- | Evaluate an indented string literal from bytecode data buffer.  The common
-- indentation is stripped from the LITERAL chunks before concatenation, so an
-- interpolated multi-line value cannot drag the computed indent down - matching
-- C++ Nix.
evalBcIndStr :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcIndStr env bcIdx0 = do
  let (count, dataOff) =
        unsafePerformIO (cbcCountedPayload bcIdx0 =<< cbcArg1 bcIdx0)
  chunks <- evalBcStringParts env count dataOff
  let (text, ctx) = stripIndentedChunks chunks
  pure (VStr text ctx)

-- | Evaluate string parts from the bytecode data buffer.  Each part is two
-- words: (tag, value).  tag=0 means literal (value = symbol), tag=1 means
-- interpolation (value = bc_idx).  The 'Bool' marks literal (@True@) vs
-- interpolated (@False@) so indented strings strip only the literals.
evalBcStringParts :: (MonadEval m) => Env -> Int -> Word32 -> m [(Bool, BS.ByteString, StringContext)]
evalBcStringParts _ 0 _ = pure []
evalBcStringParts env n off = do
  let tag = unsafePerformIO (cbcData off)
      val = unsafePerformIO (cbcData (off + 1))
  chunk <- case tag of
    0 -> pure (True, symbolBytes (Symbol val), emptyContext)
    _ -> do
      v <- evalBytecode env val
      (txt, ctx) <- coerceToStringInterp v
      pure (False, txt, ctx)
  rest <- evalBcStringParts env (n - 1) (off + 2)
  pure (chunk : rest)

-- | Evaluate a list from bytecode data buffer.
evalBcList :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcList env bcIdx0 =
  let (count, dataOff) =
        unsafePerformIO (cbcCountedPayload bcIdx0 =<< cbcArg1 bcIdx0)
      readChildren 0 _ = []
      readChildren n off =
        let childIdx = unsafePerformIO (cbcData off)
         in cheapThunkBc env childIdx : readChildren (n - 1) (off + 1)
   in pure (VList (clistFromThunks (map thunkToCPtr (readChildren count dataOff))))

-- | Evaluate a function application from bytecode.
evalBcApp :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcApp env bcIdx0 = do
  let funcIdx = unsafePerformIO (cbcArg1 bcIdx0)
      argIdx = unsafePerformIO (cbcArg2 bcIdx0)
  funcVal <- evalBytecode env funcIdx
  case funcVal of
    VLambda closureEnv formals bodyBcIdx -> do
      let argThunk = cheapThunkBc env argIdx
      extEnv <- matchFormals closureEnv formals argThunk
      evalBytecode extEnv bodyBcIdx
    VBuiltin "tryEval" [] -> tryEvalAction (evalBytecode env argIdx)
    VBuiltin name accArgs -> do
      argVal <- evalBytecode env argIdx
      applyBuiltin name accArgs argVal
    VAttrs attrs
      | Just functorThunk <- attrSetLookup "__functor" attrs -> do
          functor <- force functorThunk
          partiallyApplied <- applyValue functor funcVal
          -- Maintain laziness: thunk the argument for lambdas, like
          -- the normal VLambda path above.  Builtins force anyway.
          case partiallyApplied of
            VLambda closureEnv formals bodyBcIdx -> do
              let argThunk = cheapThunkBc env argIdx
              extEnv <- matchFormals closureEnv formals argThunk
              evalBytecode extEnv bodyBcIdx
            -- A functor returning tryEval keeps the catch around the
            -- argument's evaluation, like the direct arm above.
            VBuiltin "tryEval" [] -> tryEvalAction (evalBytecode env argIdx)
            _ -> do
              argVal <- evalBytecode env argIdx
              applyValue partiallyApplied argVal
    _ -> throwEvalError ("attempt to call " <> typeName funcVal <> ", which is not a function")

-- | @builtins.tryEval@ over an argument's evaluation: @{ success, value }@
-- with catchable errors (throw, failed assert - the only kinds
-- 'catchEvalError' recovers) mapped to @success = false@.  The catch must
-- wrap the ARGUMENT'S EVALUATION: an application path that forces the
-- argument before dispatching here has already let the throw escape.
-- Callers holding an already-forced value pass @pure val@ - nothing left
-- to catch, so it success-wraps, exactly as upstream treats a value in
-- WHNF.
tryEvalAction :: (MonadEval m) => m NixValue -> m NixValue
tryEvalAction act = do
  result <- catchEvalError act
  pure $ case result of
    Right val ->
      VAttrs
        ( attrSetFromMap $
            Map.fromList
              [ ("success", evaluated (VBool True)),
                ("value", evaluated val)
              ]
        )
    Left _ ->
      VAttrs
        ( attrSetFromMap $
            Map.fromList
              [ ("success", evaluated (VBool False)),
                ("value", evaluated (VBool False))
              ]
        )

-- | Evaluate a lambda literal from bytecode - returns VLambda.
evalBcLambda :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcLambda env bcIdx0 =
  let flags_ = unsafePerformIO (cbcFlags bcIdx0)
      formalsOff = unsafePerformIO (cbcArg1 bcIdx0)
      bodyBcIdx = unsafePerformIO (cbcArg2 bcIdx0)
      captureOff = unsafePerformIO (cbcArg3 bcIdx0)
      formals = unsafePerformIO (decodeBcFormals flags_ formalsOff)
      captureInfo = unsafePerformIO (decodeBcCaptureInfo captureOff)
   in pure (VLambda (buildCaptureEnv env captureInfo) formals bodyBcIdx)

-- | Evaluate a select expression from bytecode.
evalBcSelect :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcSelect env bcIdx0 = do
  let hasDef = unsafePerformIO (cbcFlags bcIdx0) /= 0
      targetIdx = unsafePerformIO (cbcArg1 bcIdx0)
      (pathLen, pathOff) =
        unsafePerformIO (cbcCountedPayload bcIdx0 =<< cbcArg2 bcIdx0)
      defIdx = unsafePerformIO (cbcArg3 bcIdx0)
  targetVal <- evalBytecode env targetIdx
  result <- walkBcAttrPath env pathLen pathOff targetVal
  case result of
    Just val -> pure val
    Nothing
      | hasDef -> evalBytecode env defIdx
      | otherwise -> do
          let pathName = collectBcAttrPathNames pathLen pathOff
              targetKeys = case targetVal of
                VAttrs attrs -> T.intercalate ", " (take 20 (attrSetKeys attrs))
                _ -> ""
          throwEvalError ("attribute '" <> pathName <> "' not found in " <> typeName targetVal <> " {" <> targetKeys <> "}")

-- | Evaluate a hasAttr expression from bytecode.
evalBcHasAttr :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcHasAttr env bcIdx0 = do
  let targetIdx = unsafePerformIO (cbcArg1 bcIdx0)
      (pathLen, pathOff) =
        unsafePerformIO (cbcCountedPayload bcIdx0 =<< cbcArg2 bcIdx0)
  targetVal <- evalBytecode env targetIdx
  VBool <$> hasBcAttrPath env pathLen pathOff targetVal

-- | Collect static attribute path names for error reporting.
collectBcAttrPathNames :: Int -> Word32 -> Text
collectBcAttrPathNames pathLen pathOff = T.intercalate "." (go pathLen pathOff)
  where
    go 0 _ = []
    go n off =
      let isExpr = unsafePerformIO (cbcData off)
          keyVal = unsafePerformIO (cbcData (off + 1))
          name = if isExpr /= 0 then "<expr>" else symbolText (Symbol keyVal)
       in name : go (n - 1) (off + 2)

-- | Resolve one attr-path element (two words at @off@) to its key text.
-- A dynamic key must produce a string - null included, as upstream's
-- select and has-attr key coercion rejects null with a type error.
resolveBcAttrKey :: (MonadEval m) => Env -> Word32 -> m Text
resolveBcAttrKey env off = do
  let isExpr = unsafePerformIO (cbcData off)
      keyVal = unsafePerformIO (cbcData (off + 1))
  if isExpr /= 0
    then do
      keyResult <- evalBytecode env keyVal
      case keyResult of
        VStr s _ -> decodedText "dynamic attribute key" s
        _ -> throwEvalError ("dynamic attribute key must be a string, got " <> typeName keyResult)
    else pure (symbolText (Symbol keyVal))

-- | Walk an attribute path stored in the bytecode data buffer.
-- Each element is two words: (is_expr, key_or_bc_idx).  Every matched
-- element is forced - select needs the terminal value, and the walk
-- needs each intermediate to be a set.
walkBcAttrPath :: (MonadEval m) => Env -> Int -> Word32 -> NixValue -> m (Maybe NixValue)
walkBcAttrPath _ 0 _ val = pure (Just val)
walkBcAttrPath env n off val = case val of
  VAttrs attrs -> do
    key <- resolveBcAttrKey env off
    case attrSetLookup key attrs of
      Just thunk -> do
        inner <- force thunk
        walkBcAttrPath env (n - 1) (off + 2) inner
      Nothing -> pure Nothing
  -- Non-attrset: attribute path cannot continue.  Return Nothing so
  -- that callers with a default (``a.b or def'') can handle it
  -- gracefully, matching C++ Nix behaviour.
  _ -> pure Nothing

-- | Presence walk for has-attr: intermediates force (the walk must reach
-- a set), but the terminal element is a membership check only - upstream
-- ``a ? b.c`` never evaluates the final attribute's value.
hasBcAttrPath :: (MonadEval m) => Env -> Int -> Word32 -> NixValue -> m Bool
hasBcAttrPath _ 0 _ _ = pure True
hasBcAttrPath env n off val = case val of
  VAttrs attrs -> do
    key <- resolveBcAttrKey env off
    case attrSetLookup key attrs of
      Just thunk
        | n == 1 -> pure True
        | otherwise -> do
            inner <- force thunk
            hasBcAttrPath env (n - 1) (off + 2) inner
      Nothing -> pure False
  _ -> pure False

-- | Evaluate attribute set from bytecode.
evalBcAttrs :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcAttrs env bcIdx0 = do
  let isRec = unsafePerformIO (cbcFlags bcIdx0) /= 0
      (bindCount, dataOff) =
        unsafePerformIO (cbcCountedPayload bcIdx0 =<< cbcArg1 bcIdx0)
      captureOff = unsafePerformIO (cbcArg2 bcIdx0)
      bindings = unsafePerformIO (decodeBcBindings bindCount dataOff)
  if isRec
    then do
      let captureInfo = unsafePerformIO (decodeBcCaptureInfo captureOff)
      evalBcRecAttrs env bindings captureInfo
    else evalBcNonRecAttrs env bindings

-- | Evaluate a non-recursive attr set from bytecode bindings.
evalBcNonRecAttrs :: (MonadEval m) => Env -> [BcBinding] -> m NixValue
evalBcNonRecAttrs env bindings = do
  thunkMap <- buildBcThunkMap env bindings
  pure (VAttrs (attrSetFromMap thunkMap))

-- | Build a let\/rec frame env, sharing the parent-chain and with-scope
-- plumbing otherwise repeated across the positional and dynamic paths.  The
-- frame's own bindings are positional slots (@slots@\/@slotCount@) or a lazy
-- name-table @scope@; the parent and with-scopes come from the enclosing @env@
-- and @captureInfo@.
newFrameEnv :: Env -> CaptureInfo -> Ptr CThunkPtr -> Int -> Maybe AttrSet -> Env
newFrameEnv env captureInfo slots slotCount scope =
  newCEnv slots slotCount scope (Just (buildCaptureEnv env captureInfo)) withArr withCount
  where
    (withArr, withCount) = case captureInfo of
      NoCaptureInfo -> envWithScopesRaw env
      Captures _ -> (nullPtr, 0)
      CapturesWithScopes _ -> withScopesForCapture env

-- | Evaluate a recursive attr set from bytecode bindings.
evalBcRecAttrs :: (MonadEval m) => Env -> [BcBinding] -> CaptureInfo -> m NixValue
evalBcRecAttrs env bindings captureInfo
  | allBcPositional bindings =
      -- Positional path: slots for variable lookup, CAttrSet for return.
      let slotCount = bcBindingSlotCount bindings
          slotsPtr = allocCSlots slotCount
          recEnv = newFrameEnv env captureInfo slotsPtr slotCount Nothing
          thunkList = buildBcSlotThunks recEnv env bindings
          filled = fillCSlots slotsPtr thunkList
          attrMap = buildBcAttrMapFromSlots bindings thunkList
       in filled `seq` pure (VAttrs (attrSetFromMap attrMap))
  | otherwise = do
      -- Dynamic-key path: the set has a ${dynamic} key or a nested a.b path, so
      -- names are not all known at compile time.  Not a corner-cutting fallback:
      -- it follows C++ Nix's env2 semantics.  The rec env holds the
      -- statically-named bindings as thunks, and the dynamic keys AND values are
      -- evaluated against it - so they can see static siblings and enclosing
      -- vars, but not another dynamic key.  The result is the static bindings
      -- plus the dynamic (name -> value) pairs.
      let scopeCset = buildCAttrSetKeys (bcBindingStaticKeys bindings)
          recEnv = newFrameEnv env captureInfo nullPtr 0 (Just (AttrSet scopeCset))
          (staticBs, dynBs) = partition bcBindingIsStatic bindings
      staticThunks <- buildBcThunkMap recEnv staticBs
      let scopeFilled = fillCAttrSetValues scopeCset staticThunks
      dynThunks <- scopeFilled `seq` buildBcThunkMap recEnv dynBs
      -- A dynamic key colliding with a static sibling is an eval error
      -- upstream ("dynamic attribute already defined"), never a merge.
      case Map.keys (Map.intersection dynThunks staticThunks) of
        (dupKey : _) -> throwEvalError ("dynamic attribute '" <> dupKey <> "' already defined")
        [] ->
          let allThunks = Map.union dynThunks staticThunks
              resultCset = buildCAttrSetKeys (Map.keys allThunks)
              filled = fillCAttrSetValues resultCset allThunks
           in filled `seq` pure (VAttrs (AttrSet resultCset))

-- | Evaluate a let expression from bytecode.
evalBcLet :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcLet env bcIdx0 = do
  let (bindCount, dataOff) =
        unsafePerformIO (cbcCountedPayload bcIdx0 =<< cbcArg1 bcIdx0)
      bodyIdx = unsafePerformIO (cbcArg2 bcIdx0)
      captureOff = unsafePerformIO (cbcArg3 bcIdx0)
      bindings = unsafePerformIO (decodeBcBindings bindCount dataOff)
      captureInfo = unsafePerformIO (decodeBcCaptureInfo captureOff)
  if allBcPositional bindings
    then do
      -- Positional path: slots for O(1) lookup.
      let slotCount = bcBindingSlotCount bindings
          slotsPtr = allocCSlots slotCount
          letEnv = newFrameEnv env captureInfo slotsPtr slotCount Nothing
          filled = fillCSlots slotsPtr (buildBcSlotThunks letEnv env bindings)
       in filled `seq` evalBytecode letEnv bodyIdx
    else do
      -- Dynamic path: a nested a.b binding (a dynamic top-level key is not valid
      -- in a let).  Every top-level key is therefore static; sub-keys and values
      -- resolve in the let env, which the body also sees.
      let cset = buildCAttrSetKeys (bcBindingStaticKeys bindings)
          letEnv = newFrameEnv env captureInfo nullPtr 0 (Just (AttrSet cset))
      thunkMap <- buildBcThunkMap letEnv bindings
      let filled = fillCAttrSetValues cset thunkMap
       in filled `seq` evalBytecode letEnv bodyIdx

-- | Check if all bytecode bindings are single static keys (eligible for positional).
-- Must stay in sync with 'allStaticSingleKey' in 'Nix.Expr.Resolve'.
allBcPositional :: [BcBinding] -> Bool
allBcPositional = all isEligible
  where
    isEligible (BcNamed [BcStaticKey _] _) = True
    isEligible (BcInherit _) = True
    isEligible (BcInheritFrom _ _) = True
    isEligible _ = False

-- | Count positional slots for bytecode bindings.
-- Must stay in sync with 'lexicalScopeFromBindings' in 'Nix.Expr.Resolve'.
bcBindingSlotCount :: [BcBinding] -> Int
bcBindingSlotCount = foldl' countOne 0
  where
    countOne !acc (BcNamed [BcStaticKey _] _) = acc + 1
    countOne !acc (BcInherit syms) = acc + length syms
    countOne !acc (BcInheritFrom _ syms) = acc + length syms
    countOne !acc _ = acc

-- | Build thunks for positional bytecode bindings in declaration order.
buildBcSlotThunks :: Env -> Env -> [BcBinding] -> [Thunk]
buildBcSlotThunks recEnv outerEnv = concatMap slotThunk
  where
    slotThunk (BcNamed [BcStaticKey _] valBcIdx) =
      [mkThunkBc recEnv valBcIdx]
    slotThunk (BcInherit syms) =
      map (inheritLookup outerEnv . symbolText . Symbol) syms
    slotThunk (BcInheritFrom fromBcIdx syms) =
      -- inherit (from) x y z; becomes one thunk per name that selects from the from-expr.
      -- Each thunk gets a minimal env with the from-value at slot 0.
      let fromThunk = mkThunkBc recEnv fromBcIdx
          mkInheritThunk sym =
            let name = symbolText (Symbol sym)
                selectExpr = ESelect (EResolvedVar 0 0) [StaticKey name] Nothing
                (sp, sc) = buildCSlots [fromThunk]
                fromEnv = newMinimalEnv sp sc
             in mkSyntheticThunk fromEnv selectExpr
       in map mkInheritThunk syms
    -- Unreachable: allBcPositional guards this path.
    slotThunk _ = []

-- | Build attr map from slots for positional bytecode bindings.
buildBcAttrMapFromSlots :: [BcBinding] -> [Thunk] -> Map Text Thunk
buildBcAttrMapFromSlots bindings thunks = go bindings thunks Map.empty
  where
    go [] _ !acc = acc
    go (BcNamed [BcStaticKey sym] _ : bs) (t : ts) !acc =
      go bs ts (Map.insert (symbolText (Symbol sym)) t acc)
    go (BcInherit syms : bs) ts !acc =
      let (used, rest) = splitAt (length syms) ts
          accMerged = foldl' (\a (sym, t0) -> Map.insert (symbolText (Symbol sym)) t0 a) acc (zip syms used)
       in go bs rest accMerged
    go (BcInheritFrom _ syms : bs) ts !acc =
      let (used, rest) = splitAt (length syms) ts
          accMerged = foldl' (\a (sym, t0) -> Map.insert (symbolText (Symbol sym)) t0 a) acc (zip syms used)
       in go bs rest accMerged
    -- Unreachable: allBcPositional guards this path.
    go (_ : bs) ts !acc = go bs ts acc

-- | Build thunk map for bytecode attrs (non-rec or fallback rec path).
buildBcThunkMap :: (MonadEval m) => Env -> [BcBinding] -> m (Map Text Thunk)
buildBcThunkMap thunkEnv = foldM addBinding Map.empty
  where
    addBinding acc (BcNamed keys valBcIdx) = do
      resolvedKeys <- mapM (resolveBcKey thunkEnv) keys
      case sequence resolvedKeys of
        Nothing -> pure acc -- null key -> skip
        Just [key] ->
          insertChecked acc key (mkThunkBc thunkEnv valBcIdx)
        Just path ->
          let nested = buildBcNestedAttr thunkEnv path valBcIdx
           in foldM (\a (k, t0) -> insertChecked a k t0) acc (Map.toList nested)
    addBinding acc (BcInherit syms) =
      foldM (\a sym -> let name = symbolText (Symbol sym) in insertChecked a name (inheritLookup thunkEnv name)) acc syms
    addBinding acc (BcInheritFrom fromBcIdx syms) =
      -- inherit (from) name selects name from the from-expr.
      -- Create a small env with the from value at slot 0, then a
      -- synthetic expression that selects name from slot 0.
      let addInheritFrom a sym =
            let name = symbolText (Symbol sym)
                selectExpr = ESelect (EResolvedVar 0 0) [StaticKey name] Nothing
                (sp, sc) = buildCSlots [mkThunkBc thunkEnv fromBcIdx]
                fromEnv = newMinimalEnv sp sc
             in insertChecked a name (mkSyntheticThunk fromEnv selectExpr)
       in foldM addInheritFrom acc syms

    -- The parser normalizes static keys to appear exactly once, so a
    -- collision here means an evaluated DYNAMIC key hit an existing
    -- attr - an eval error upstream, never a silent merge.
    insertChecked acc key thunk =
      case Map.lookup key acc of
        Nothing -> pure (Map.insert key thunk acc)
        Just _ -> throwEvalError ("dynamic attribute '" <> key <> "' already defined")

-- | Resolve a bytecode attr key to text.
resolveBcKey :: (MonadEval m) => Env -> BcAttrKey -> m (Maybe Text)
resolveBcKey _env (BcStaticKey sym) = pure (Just (symbolText (Symbol sym)))
resolveBcKey env (BcDynamicKey bcIdx0) = do
  val <- evalBytecode env bcIdx0
  case val of
    VStr s _ -> Just <$> decodedText "dynamic attribute key" s
    VNull -> pure Nothing
    _ -> throwEvalError ("dynamic attribute key must be a string, got " <> typeName val)

-- | Build a nested attribute structure from a resolved dotted path (bytecode).
buildBcNestedAttr :: Env -> [Text] -> Word32 -> Map Text Thunk
buildBcNestedAttr _thunkEnv [] _valBcIdx = Map.empty
buildBcNestedAttr thunkEnv [key] valBcIdx =
  Map.singleton key (mkThunkBc thunkEnv valBcIdx)
buildBcNestedAttr thunkEnv (key : rest) valBcIdx =
  Map.singleton key (evaluated (VAttrs (attrSetFromMap (buildBcNestedAttr thunkEnv rest valBcIdx))))

-- | Top-level keys knowable without evaluating any dynamic key: static keys
-- and inherited names.  These populate the rec env's name table while the
-- dynamic keys are evaluated, so a dynamic key can reference a static sibling
-- or an enclosing variable (matching C++ Nix's env2), but not another dynamic
-- key.
bcBindingStaticKeys :: [BcBinding] -> [Text]
bcBindingStaticKeys = concatMap oneBinding
  where
    oneBinding (BcNamed (BcStaticKey sym : _) _) = [symbolText (Symbol sym)]
    oneBinding (BcNamed _ _) = []
    oneBinding (BcInherit syms) = map (symbolText . Symbol) syms
    oneBinding (BcInheritFrom _ syms) = map (symbolText . Symbol) syms

-- | True when a binding's top-level key is known statically (a static key or an
-- inherit).  These bindings populate the rec env's name table (C++ Nix's env2);
-- a binding with a dynamic top-level key does not - it is added to the value.
bcBindingIsStatic :: BcBinding -> Bool
bcBindingIsStatic (BcNamed (BcStaticKey _ : _) _) = True
bcBindingIsStatic (BcNamed _ _) = False
bcBindingIsStatic (BcInherit _) = True
bcBindingIsStatic (BcInheritFrom _ _) = True

-- ---------------------------------------------------------------------------
-- Search paths (<nixpkgs>, <nixpkgs/lib>)
-- ---------------------------------------------------------------------------

-- | Evaluate a search path expression.
-- Desugars to @builtins.findFile builtins.nixPath "name"@ - exactly how
-- real Nix handles @\<name\>@ expressions.
evalSearchPath :: (MonadEval m) => Env -> Text -> m NixValue
evalSearchPath env name = do
  builtinsVal <- evalVar env "builtins"
  case builtinsVal of
    VAttrs builtinsAttrs ->
      case attrSetLookup "nixPath" builtinsAttrs of
        Just nixPathThunk -> do
          nixPathVal <- force nixPathThunk
          builtinFindFile nixPathVal (mkStr name)
        Nothing ->
          throwCatchableError ("file '" <> name <> "' was not found in the Nix search path")
    _ ->
      throwCatchableError ("file '" <> name <> "' was not found in the Nix search path")

-- ---------------------------------------------------------------------------
-- Variables
-- ---------------------------------------------------------------------------

evalVar :: (MonadEval m) => Env -> Text -> m NixValue
evalVar env name =
  case envLookup name env of
    Just thunk -> force thunk
    Nothing -> throwEvalError ("undefined variable '" <> name <> "'")

-- | Evaluate a with-scoped variable: check with-scopes first (innermost
-- to outermost), then fall back to the standard name-based lookup
-- (parent chain to builtins).  For trimmed envs ('CapturesWithScopes'),
-- the root scope is already appended to 'envWithScopes' so the
-- with-scope lookup finds builtins without needing a parent chain.
-- Supports both resolved (CAttrSet*) and lazy (CThunk*, tagged with bit 0)
-- with-scope entries.  Lazy entries are forced on first lookup and the
-- pointer is updated in place so subsequent lookups hit the resolved
-- attrset directly.
evalWithVar :: (MonadEval m) => Env -> Text -> m NixValue
evalWithVar env name =
  let (withArr, withCount) = envWithScopesRaw env
   in evalWithVarScopes env name withArr (fromIntegral withCount) 0

evalWithVarScopes :: (MonadEval m) => Env -> Text -> Ptr (Ptr ()) -> Int -> Int -> m NixValue
evalWithVarScopes env name withArr count idx
  | idx >= count = evalVar env name
  | otherwise = do
      let scopePtr = unsafePerformIO (peekElemOff withArr idx)
      if isLazyWithScope scopePtr
        then do
          -- Lazy with-scope: force the thunk to get the attrset
          let thunkPtr = untagWithScope scopePtr
          scopeVal <- force (Thunk thunkPtr)
          case scopeVal of
            VAttrs (AttrSet cset) -> do
              -- Cache: replace the tagged thunk pointer with the resolved
              -- attrset pointer so future lookups are fast.
              let resolvedPtr = castPtr cset
              seq (unsafePerformIO (pokeElemOff withArr idx resolvedPtr)) (pure ())
              case attrSetLookup name (AttrSet cset) of
                Just thunk -> force thunk
                Nothing -> evalWithVarScopes env name withArr count (idx + 1)
            _ -> throwEvalError ("'with' requires a set, got " <> typeName scopeVal)
        else
          -- Resolved attrset scope (normal path)
          case attrSetLookup name (AttrSet (castPtr scopePtr)) of
            Just thunk -> force thunk
            Nothing -> evalWithVarScopes env name withArr count (idx + 1)

-- | Check if a with-scope pointer is tagged as lazy (bit 0 set).
isLazyWithScope :: Ptr () -> Bool
isLazyWithScope ptr = ptrToWordPtr ptr .&. 1 /= 0

-- | Remove the lazy tag from a with-scope pointer, returning a CThunkPtr.
untagWithScope :: Ptr () -> CThunkPtr
untagWithScope ptr =
  let tagged = ptrToWordPtr ptr
   in wordPtrToPtr (tagged .&. complement 1)

-- | Tag a CThunkPtr as a lazy with-scope (set bit 0).
tagLazyWithScope :: CThunkPtr -> Ptr ()
tagLazyWithScope ptr =
  let raw = ptrToWordPtr (castPtr ptr)
   in wordPtrToPtr (raw .|. 1)

-- | Push a lazy (thunk-based) with-scope onto the environment.
-- The scope is NOT forced until a WITH_VAR lookup actually needs it.
{-# NOINLINE pushLazyWithScope #-}
pushLazyWithScope :: Thunk -> Env -> Env
pushLazyWithScope (Thunk thunkPtr) =
  pushWithScopeRaw (tagLazyWithScope thunkPtr)

-- | Push a raw pointer as a with-scope.
{-# NOINLINE pushWithScopeRaw #-}
pushWithScopeRaw :: Ptr () -> Env -> Env
pushWithScopeRaw ptr (Env envPtr) =
  Env (checkedCPtr "pushWithScopeRaw" (unsafePerformIO (cenvPushWith envPtr ptr)))

-- ---------------------------------------------------------------------------
-- Formals matching + env helpers (used by evalBcApp, applyValue, etc.)
-- ---------------------------------------------------------------------------

-- | Match a lambda's formals against an argument thunk.
matchFormals :: (MonadEval m) => Env -> EvalFormals -> Thunk -> m Env
matchFormals closureEnv (EFName _) argThunk =
  let (sp, sc) = buildCSlots [argThunk]
   in pure (envFromSlots sp sc closureEnv)
matchFormals closureEnv (EFSet formals allowExtra) argThunk = do
  argVal <- force argThunk
  matchFormalSet closureEnv formals allowExtra argVal Nothing
matchFormals closureEnv (EFNamedSet _ formals allowExtra) argThunk = do
  argVal <- force argThunk
  matchFormalSet closureEnv formals allowExtra argVal (Just argThunk)

-- | Match destructuring set pattern formals against an attrset value.
matchFormalSet :: (MonadEval m) => Env -> [EvalFormal] -> Bool -> NixValue -> Maybe Thunk -> m Env
matchFormalSet closureEnv formals allowExtra argVal atThunk =
  case argVal of
    VAttrs attrs -> do
      checkExtraKeys formals allowExtra attrs
      checkMissingFormals attrs formals
      let formalEnv = envFromSlots formalSlotsPtr formalSlotCount closureEnv
          (formalSlotsPtr, formalSlotCount) = buildCSlots formalThunks
          formalThunks = case atThunk of
            Nothing -> map resolveOneFormal formals
            Just at -> at : map resolveOneFormal formals
          resolveOneFormal (EvalFormal name defBcIdx) =
            case attrSetLookup name attrs of
              Just thunk -> thunk
              Nothing -> case defBcIdx of
                Just bcIdx -> mkThunkBc formalEnv bcIdx
                Nothing -> error "matchFormalSet: missing required formal (unreachable)"
      pure formalEnv
    _ -> throwEvalError ("function expects a set argument, got " <> typeName argVal)

checkExtraKeys :: (MonadEval m) => [EvalFormal] -> Bool -> AttrSet -> m ()
checkExtraKeys _ True _ = pure ()
checkExtraKeys formals False attrs =
  let expected = map efName formals
      actual = attrSetKeys attrs
      extra = filter (`notElem` expected) actual
   in case extra of
        [] -> pure ()
        (k : _) -> throwEvalError ("unexpected attribute '" <> k <> "' in function argument")

checkMissingFormals :: (MonadEval m) => AttrSet -> [EvalFormal] -> m ()
checkMissingFormals attrs formals =
  let allMissing = [efName f | f <- formals, isNothing (efDefault f), not (attrSetMember (efName f) attrs)]
   in case allMissing of
        [] -> pure ()
        (name : _) ->
          let provided = attrSetKeys attrs
              provSnippet = T.intercalate ", " (take 20 provided)
              missSnippet = T.intercalate ", " allMissing
           in throwEvalError ("missing required attribute '" <> name <> "'; all missing: [" <> missSnippet <> "]; provided keys (" <> T.pack (show (length provided)) <> "): [" <> provSnippet <> "]")

-- | Build capture environment from capture info.
-- NoCaptureInfo: no trimming, use env as-is.
-- Captures: build minimal env from captured slots.
-- CapturesWithScopes: build minimal env + copy with-scopes.
buildCaptureEnv :: Env -> CaptureInfo -> Env
buildCaptureEnv env NoCaptureInfo = env
buildCaptureEnv env (Captures captureList) =
  let (slotsPtr, slotCount) = buildCSlots [envLookupResolved lvl idx env | (lvl, idx) <- captureList]
   in newMinimalEnv slotsPtr slotCount
buildCaptureEnv env (CapturesWithScopes captureList) =
  let (slotsPtr, slotCount) = buildCSlots [envLookupResolved lvl idx env | (lvl, idx) <- captureList]
      (withArr, withCount) = withScopesForCapture env
   in newCEnv slotsPtr slotCount Nothing Nothing withArr withCount

-- | Look up a name in the environment and return its thunk.
-- Used by @inherit@ bindings (both bytecode and Expr paths).
inheritLookup :: Env -> Text -> Thunk
inheritLookup env name =
  case envLookup name env of
    Just thunk -> thunk
    Nothing -> error ("inheritLookup: undefined variable '" <> T.unpack name <> "' (unreachable)")

-- ---------------------------------------------------------------------------
-- Builtin registry (single-definition-site for all builtins)
-- ---------------------------------------------------------------------------

-- | A builtin function definition: its arity and implementation.
data BuiltinDef m = BuiltinDef
  { bdArity :: !Int,
    bdApply :: [NixValue] -> m NixValue
  }

-- | Define an arity-1 builtin.
builtin1 :: (MonadEval m) => Text -> (NixValue -> m NixValue) -> (Text, BuiltinDef m)
builtin1 name f =
  ( name,
    BuiltinDef 1 $ \case
      [a] -> f a
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
  )

-- | Define an arity-2 builtin.
builtin2 ::
  (MonadEval m) =>
  Text ->
  (NixValue -> NixValue -> m NixValue) ->
  (Text, BuiltinDef m)
builtin2 name f =
  ( name,
    BuiltinDef 2 $ \case
      [a, b] -> f a b
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
  )

-- | Define an arity-3 builtin.
builtin3 ::
  (MonadEval m) =>
  Text ->
  (NixValue -> NixValue -> NixValue -> m NixValue) ->
  (Text, BuiltinDef m)
builtin3 name f =
  ( name,
    BuiltinDef 3 $ \case
      [a, b, c] -> f a b c
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
  )

-- | Central registry of all builtins.  Adding a new builtin is a single
-- entry here plus its implementation function - no other files need changes.
builtinRegistry :: (MonadEval m) => Map Text (BuiltinDef m)
builtinRegistry =
  Map.fromList
    [ -- Type checking (arity 1)
      builtin1 "typeOf" (pure . mkStr . typeOfValue),
      builtin1 "isNull" (pure . VBool . isNullVal),
      builtin1 "isInt" (pure . VBool . isIntVal),
      builtin1 "isFloat" (pure . VBool . isFloatVal),
      builtin1 "isBool" (pure . VBool . isBoolVal),
      builtin1 "isString" (pure . VBool . isStringVal),
      builtin1 "isList" (pure . VBool . isListVal),
      builtin1 "isAttrs" (pure . VBool . isAttrsVal),
      builtin1 "isFunction" (pure . VBool . isFunctionVal),
      -- List operations (arity 1)
      builtin1 "length" builtinLength,
      builtin1 "head" builtinHead,
      builtin1 "tail" builtinTail,
      -- String operations (arity 1)
      builtin1 "toString" (fmap (uncurry VStr) . coerceToStringPermissive),
      builtin1 "stringLength" builtinStringLength,
      -- Control (arity 1)
      builtin1 "throw" builtinThrow,
      builtin1 "abort" builtinAbort,
      -- Attr set operations (arity 1)
      builtin1 "attrNames" builtinAttrNames,
      builtin1 "attrValues" builtinAttrValues,
      builtin1 "listToAttrs" builtinListToAttrs,
      -- Attr set operations (arity 2)
      builtin2 "hasAttr" builtinHasAttr,
      builtin2 "getAttr" builtinGetAttr,
      builtin2 "removeAttrs" builtinRemoveAttrs,
      builtin2 "intersectAttrs" builtinIntersectAttrs,
      builtin2 "catAttrs" builtinCatAttrs,
      -- List higher-order (arity 2)
      builtin2 "map" builtinMap,
      builtin2 "filter" builtinFilter,
      builtin2 "genList" builtinGenList,
      builtin2 "sort" builtinSort,
      builtin2 "concatMap" builtinConcatMap,
      builtin2 "any" builtinAny,
      builtin2 "all" builtinAll,
      builtin2 "elem" builtinElem,
      builtin2 "elemAt" builtinElemAt,
      builtin2 "partition" builtinPartition,
      builtin2 "groupBy" builtinGroupBy,
      -- String operations (arity 2)
      builtin2 "concatStringsSep" builtinConcatStringsSep,
      -- Arity 3
      builtin3 "foldl'" builtinFoldl,
      builtin3 "substring" builtinSubstring,
      -- Numeric
      builtin1 "isPath" (pure . VBool . isPathVal),
      builtin1 "ceil" builtinCeil,
      builtin1 "floor" builtinFloor,
      builtin2 "seq" builtinSeq,
      builtin2 "trace" builtinTrace,
      builtin2 "warn" builtinWarn,
      builtin1 "unsafeDiscardStringContext" builtinDiscardContext,
      builtin1 "unsafeDiscardOutputDependency" builtinDiscardOutputDep,
      builtin1 "addDrvOutputDependencies" builtinAddDrvOutputDeps,
      -- String context introspection
      builtin1 "hasContext" builtinHasContext,
      builtin1 "getContext" builtinGetContext,
      builtin2 "appendContext" builtinAppendContext,
      builtin1 "baseNameOf" builtinBaseNameOf,
      builtin1 "dirOf" builtinDirOf,
      builtin1 "concatLists" builtinConcatLists,
      builtin2 "lessThan" builtinLessThan,
      -- Arithmetic + bitwise
      builtin2 "add" builtinAdd,
      builtin2 "sub" builtinSub,
      builtin2 "mul" builtinMul,
      builtin2 "div" builtinDiv,
      builtin2 "bitAnd" builtinBitAnd,
      builtin2 "bitOr" builtinBitOr,
      builtin2 "bitXor" builtinBitXor,
      -- Attr set higher-order
      builtin2 "mapAttrs" builtinMapAttrs,
      builtin1 "functionArgs" builtinFunctionArgs,
      builtin2 "zipAttrsWith" builtinZipAttrsWith,
      -- String manipulation
      builtin2 "match" builtinMatch,
      builtin2 "split" builtinSplit,
      builtin3 "replaceStrings" builtinReplaceStrings,
      builtin2 "compareVersions" builtinCompareVersions,
      builtin1 "splitVersion" builtinSplitVersion,
      builtin1 "parseDrvName" builtinParseDrvName,
      -- Serialization + hashing
      builtin1 "toJSON" builtinToJSON,
      builtin1 "fromJSON" builtinFromJSON,
      builtin2 "hashString" builtinHashString,
      -- Error handling + sequencing
      -- An argument reaching here is already forced (any throw happened
      -- before tryEval saw it), so it success-wraps; the catching paths
      -- are evalBcApp and applyValueLazy.
      builtin1 "tryEval" (tryEvalAction . pure),
      builtin2 "deepSeq" builtinDeepSeq,
      -- Graph traversal
      builtin1 "genericClosure" builtinGenericClosure,
      -- IO builtins (delegate to MonadEval methods)
      builtin1 "import" builtinImport,
      builtin1 "readFile" builtinReadFile,
      builtin1 "pathExists" builtinPathExists,
      builtin1 "readDir" builtinReadDir,
      builtin1 "getEnv" builtinGetEnv,
      builtin1 "toPath" builtinToPath,
      -- Store path operations
      builtin1 "placeholder" builtinPlaceholder,
      builtin1 "storePath" builtinStorePath,
      builtin2 "findFile" builtinFindFile,
      builtin2 "toFile" builtinToFile,
      builtin2 "scopedImport" builtinScopedImport,
      -- Network fetchers
      builtin1 "fetchurl" builtinFetchurl,
      builtin1 "fetchTarball" builtinFetchTarball,
      builtin1 "fetchGit" builtinFetchGit,
      -- Derivation construction: lazy 'derivation' wrapper over the eager
      -- 'derivationStrict' primop (matches C++ Nix corepkgs/derivation.nix).
      builtin1 "derivation" builtinDerivationLazy,
      builtin1 "derivationStrict" builtinDerivationStrict,
      -- Error context (pass-through - context only matters on error)
      builtin2 "addErrorContext" (\_ val -> pure val),
      -- Attr position (return null - nixpkgs handles this gracefully)
      builtin2 "unsafeGetAttrPos" (\_ _ -> pure VNull),
      -- Debugging (traceVerbose: same as trace for now, --trace-verbose not yet gated)
      builtin2 "traceVerbose" builtinTrace,
      builtin1 "break" pure,
      -- IO: file hashing + type detection
      builtin2 "hashFile" builtinHashFile,
      builtin1 "readFileType" builtinReadFileType,
      -- Serialization
      builtin1 "fromTOML" builtinFromTOML,
      -- Hash conversion
      builtin1 "convertHash" builtinConvertHash,
      -- XML serialization
      builtin1 "toXML" builtinToXML,
      -- Source filtering + path import
      builtin1 "path" builtinPath,
      builtin2 "filterSource" builtinFilterSource,
      -- Experimental feature stubs
      builtin2 "outputOf" builtinOutputOf,
      builtin1 "fetchTree" builtinFetchTree,
      builtin1 "fetchClosure" builtinFetchClosure
    ]

-- | Names of all registered builtins.
builtinNames :: [Text]
builtinNames = Map.keys (builtinRegistry :: Map Text (BuiltinDef PureEval))

-- ---------------------------------------------------------------------------
-- Builtin dispatch (partial application via accumulated args)
-- ---------------------------------------------------------------------------

-- | Arity of a builtin (how many arguments before execution).
builtinArity :: Text -> Int
builtinArity name = maybe 1 bdArity (Map.lookup name (builtinRegistry :: Map Text (BuiltinDef PureEval)))

-- | Apply a builtin with accumulated args.  If we have enough args,
-- execute; otherwise return a partially applied builtin.
applyBuiltin :: (MonadEval m) => Text -> [NixValue] -> NixValue -> m NixValue
applyBuiltin name accArgs arg =
  let allArgs = accArgs ++ [arg]
      arity = builtinArity name
   in if length allArgs < arity
        then pure (VBuiltin name (precompileArgs name allArgs))
        else executeBuiltin name allArgs

-- | Pre-compile regex patterns at partial application time.
-- When builtins.match or builtins.split receives its first argument
-- (the pattern string), compile it immediately and store the compiled
-- RE.Regex in a VCompiledRegex, replacing the raw VStr.  The compiled
-- form is carried in VBuiltin's accumulated args and reused on every
-- subsequent application - zero recompilation.  Both builtins compile
-- the RAW pattern: match's whole-string requirement is a span check at
-- match time, not textual @^...$@ anchoring, which would misparse a
-- top-level alternation (@^a|b$@ is @(^a)|(b$)@).
precompileArgs :: Text -> [NixValue] -> [NixValue]
precompileArgs "match" [VStr pat _] = compiledRegexArg pat
precompileArgs "split" [VStr pat _] = compiledRegexArg pat
precompileArgs _ args = args

-- | The single-element arg list for a regex builtin: the compiled pattern
-- if valid, the raw string otherwise (the error surfaces at execute time).
compiledRegexArg :: BS.ByteString -> [NixValue]
compiledRegexArg pat = case cachedCompileRegex pat of
  Just compiled -> [VCompiledRegex (CompiledRegex pat compiled)]
  Nothing -> [VStr pat emptyContext]

-- | Apply a function value (lambda or builtin) to one argument.
-- Used by higher-order builtins to invoke user-supplied functions.
applyValue :: (MonadEval m) => NixValue -> NixValue -> m NixValue
applyValue (VLambda closureEnv formals bodyBcIdx) arg = do
  extEnv <- matchFormals closureEnv formals (evaluated arg)
  evalBytecode extEnv bodyBcIdx
applyValue (VBuiltin name accArgs) arg =
  applyBuiltin name accArgs arg
applyValue other _ =
  throwEvalError ("attempt to call " <> typeName other <> ", which is not a function")

-- | Apply a function value to an UNFORCED thunk argument.  Upstream's
-- higher-order list builtins (filter, any, all, partition, groupBy,
-- foldl') pass elements as unforced values, so an element the function
-- never inspects may contain a throw without failing the call.  A
-- builtin callee still receives a forced value (builtins force their
-- arguments regardless), and set-pattern formals force on destructuring
-- exactly as upstream does.
applyValueLazy :: (MonadEval m) => NixValue -> Thunk -> m NixValue
applyValueLazy (VLambda closureEnv formals bodyBcIdx) argThunk = do
  extEnv <- matchFormals closureEnv formals argThunk
  evalBytecode extEnv bodyBcIdx
-- tryEval's catch must wrap the argument's forcing (see 'tryEvalAction'):
-- map/filter passing a throwing element to tryEval yields success = false,
-- not an escaped error.
applyValueLazy (VBuiltin "tryEval" []) argThunk = tryEvalAction (force argThunk)
applyValueLazy other argThunk = do
  val <- force argThunk
  applyValue other val

-- | Execute a builtin once all arguments are collected.
--
-- Direct case dispatch avoids rebuilding the polymorphic 'builtinRegistry'
-- Map on every call.  'builtinRegistry' is polymorphic in @m@ so GHC
-- cannot cache it as a CAF - it gets reconstructed on every use.
-- Pattern matching on the name is zero-allocation.
executeBuiltin :: (MonadEval m) => Text -> [NixValue] -> m NixValue
executeBuiltin name args = case name of
  -- Type checking (arity 1)
  "typeOf" -> apply1 (pure . mkStr . typeOfValue)
  "isNull" -> apply1 (pure . VBool . isNullVal)
  "isInt" -> apply1 (pure . VBool . isIntVal)
  "isFloat" -> apply1 (pure . VBool . isFloatVal)
  "isBool" -> apply1 (pure . VBool . isBoolVal)
  "isString" -> apply1 (pure . VBool . isStringVal)
  "isList" -> apply1 (pure . VBool . isListVal)
  "isAttrs" -> apply1 (pure . VBool . isAttrsVal)
  "isFunction" -> apply1 (pure . VBool . isFunctionVal)
  -- List operations (arity 1)
  "length" -> apply1 builtinLength
  "head" -> apply1 builtinHead
  "tail" -> apply1 builtinTail
  -- String operations (arity 1)
  "toString" -> apply1 (fmap (uncurry VStr) . coerceToStringPermissive)
  "stringLength" -> apply1 builtinStringLength
  -- Control (arity 1)
  "throw" -> apply1 builtinThrow
  "abort" -> apply1 builtinAbort
  -- Attr set operations (arity 1)
  "attrNames" -> apply1 builtinAttrNames
  "attrValues" -> apply1 builtinAttrValues
  "listToAttrs" -> apply1 builtinListToAttrs
  -- Attr set operations (arity 2)
  "hasAttr" -> apply2 builtinHasAttr
  "getAttr" -> apply2 builtinGetAttr
  "removeAttrs" -> apply2 builtinRemoveAttrs
  "intersectAttrs" -> apply2 builtinIntersectAttrs
  "catAttrs" -> apply2 builtinCatAttrs
  -- List higher-order (arity 2)
  "map" -> apply2 builtinMap
  "filter" -> apply2 builtinFilter
  "genList" -> apply2 builtinGenList
  "sort" -> apply2 builtinSort
  "concatMap" -> apply2 builtinConcatMap
  "any" -> apply2 builtinAny
  "all" -> apply2 builtinAll
  "elem" -> apply2 builtinElem
  "elemAt" -> apply2 builtinElemAt
  "partition" -> apply2 builtinPartition
  "groupBy" -> apply2 builtinGroupBy
  -- String operations (arity 2)
  "concatStringsSep" -> apply2 builtinConcatStringsSep
  -- Arity 3
  "foldl'" -> apply3 builtinFoldl
  "substring" -> apply3 builtinSubstring
  -- Numeric
  "isPath" -> apply1 (pure . VBool . isPathVal)
  "ceil" -> apply1 builtinCeil
  "floor" -> apply1 builtinFloor
  "seq" -> apply2 builtinSeq
  "trace" -> apply2 builtinTrace
  "warn" -> apply2 builtinWarn
  "unsafeDiscardStringContext" -> apply1 builtinDiscardContext
  "unsafeDiscardOutputDependency" -> apply1 builtinDiscardOutputDep
  "addDrvOutputDependencies" -> apply1 builtinAddDrvOutputDeps
  -- String context introspection
  "hasContext" -> apply1 builtinHasContext
  "getContext" -> apply1 builtinGetContext
  "appendContext" -> apply2 builtinAppendContext
  "baseNameOf" -> apply1 builtinBaseNameOf
  "dirOf" -> apply1 builtinDirOf
  "concatLists" -> apply1 builtinConcatLists
  "lessThan" -> apply2 builtinLessThan
  -- Arithmetic + bitwise
  "add" -> apply2 builtinAdd
  "sub" -> apply2 builtinSub
  "mul" -> apply2 builtinMul
  "div" -> apply2 builtinDiv
  "bitAnd" -> apply2 builtinBitAnd
  "bitOr" -> apply2 builtinBitOr
  "bitXor" -> apply2 builtinBitXor
  -- Attr set higher-order
  "mapAttrs" -> apply2 builtinMapAttrs
  "functionArgs" -> apply1 builtinFunctionArgs
  "zipAttrsWith" -> apply2 builtinZipAttrsWith
  -- String manipulation
  "match" -> apply2 builtinMatch
  "split" -> apply2 builtinSplit
  "replaceStrings" -> apply3 builtinReplaceStrings
  "compareVersions" -> apply2 builtinCompareVersions
  "splitVersion" -> apply1 builtinSplitVersion
  "parseDrvName" -> apply1 builtinParseDrvName
  -- Serialization + hashing
  "toJSON" -> apply1 builtinToJSON
  "fromJSON" -> apply1 builtinFromJSON
  "hashString" -> apply2 builtinHashString
  -- Error handling + sequencing
  -- Already-forced argument: success-wrap (see the registry entry).
  "tryEval" -> apply1 (tryEvalAction . pure)
  "deepSeq" -> apply2 builtinDeepSeq
  -- Graph traversal
  "genericClosure" -> apply1 builtinGenericClosure
  -- IO builtins (delegate to MonadEval methods)
  "import" -> apply1 builtinImport
  "readFile" -> apply1 builtinReadFile
  "pathExists" -> apply1 builtinPathExists
  "readDir" -> apply1 builtinReadDir
  "getEnv" -> apply1 builtinGetEnv
  "toPath" -> apply1 builtinToPath
  -- Store path operations
  "placeholder" -> apply1 builtinPlaceholder
  "storePath" -> apply1 builtinStorePath
  "findFile" -> apply2 builtinFindFile
  "toFile" -> apply2 builtinToFile
  "scopedImport" -> apply2 builtinScopedImport
  -- Network fetchers
  "fetchurl" -> apply1 builtinFetchurl
  "fetchTarball" -> apply1 builtinFetchTarball
  "fetchGit" -> apply1 builtinFetchGit
  -- Derivation construction: lazy 'derivation' over eager 'derivationStrict'
  "derivation" -> apply1 builtinDerivationLazy
  "derivationStrict" -> apply1 builtinDerivationStrict
  -- Error context (pass-through - context only matters on error)
  "addErrorContext" -> apply2 (\_ val -> pure val)
  -- Attr position (return null - nixpkgs handles this gracefully)
  "unsafeGetAttrPos" -> apply2 (\_ _ -> pure VNull)
  -- Debugging (traceVerbose: same as trace for now)
  "traceVerbose" -> apply2 builtinTrace
  "break" -> apply1 pure
  -- IO: file hashing + type detection
  "hashFile" -> apply2 builtinHashFile
  "readFileType" -> apply1 builtinReadFileType
  -- Serialization
  "fromTOML" -> apply1 builtinFromTOML
  -- Hash conversion
  "convertHash" -> apply1 builtinConvertHash
  -- XML serialization
  "toXML" -> apply1 builtinToXML
  -- Source filtering + path import
  "path" -> apply1 builtinPath
  "filterSource" -> apply2 builtinFilterSource
  -- Experimental feature stubs
  "outputOf" -> apply2 builtinOutputOf
  "fetchTree" -> apply1 builtinFetchTree
  "fetchClosure" -> apply1 builtinFetchClosure
  _ -> throwEvalError ("unknown builtin '" <> name <> "'")
  where
    apply1 f = case args of
      [a] -> f a
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
    apply2 f = case args of
      [a, b] -> f a b
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")
    apply3 f = case args of
      [a, b, c] -> f a b c
      _ -> throwEvalError ("builtins." <> name <> ": internal arity error")

-- ---------------------------------------------------------------------------
-- Builtin implementations - type checking
-- ---------------------------------------------------------------------------

typeOfValue :: NixValue -> Text
typeOfValue val = case val of
  VInt _ -> "int"
  VFloat _ -> "float"
  VBool _ -> "bool"
  VNull -> "null"
  VStr _ _ -> "string"
  VPath _ -> "path"
  VList _ -> "list"
  VAttrs _ -> "set"
  VLambda {} -> "lambda"
  VBuiltin _ _ -> "lambda"
  VDerivation _ -> "set"
  VCompiledRegex _ -> "lambda"

isNullVal :: NixValue -> Bool
isNullVal VNull = True
isNullVal _ = False

isIntVal :: NixValue -> Bool
isIntVal (VInt _) = True
isIntVal _ = False

isFloatVal :: NixValue -> Bool
isFloatVal (VFloat _) = True
isFloatVal _ = False

isBoolVal :: NixValue -> Bool
isBoolVal (VBool _) = True
isBoolVal _ = False

isStringVal :: NixValue -> Bool
isStringVal (VStr _ _) = True
isStringVal _ = False

isListVal :: NixValue -> Bool
isListVal (VList _) = True
isListVal _ = False

isAttrsVal :: NixValue -> Bool
isAttrsVal (VAttrs _) = True
isAttrsVal _ = False

isFunctionVal :: NixValue -> Bool
isFunctionVal (VLambda {}) = True
isFunctionVal (VBuiltin _ _) = True
isFunctionVal _ = False

-- ---------------------------------------------------------------------------
-- Builtin implementations - list (arity 1)
-- ---------------------------------------------------------------------------

builtinLength :: (MonadEval m) => NixValue -> m NixValue
builtinLength (VList cl) = pure (VInt (fromIntegral (clistLen cl)))
builtinLength other = throwEvalError ("builtins.length: expected a list, got " <> typeName other)

builtinHead :: (MonadEval m) => NixValue -> m NixValue
builtinHead (VList cl)
  | clistLen cl == 0 = throwEvalError "builtins.head: empty list"
  | otherwise = case clistThunks cl of
      (p : _) -> force (Thunk p)
      [] -> throwEvalError "builtins.head: empty list" -- unreachable: clistLen > 0
builtinHead other = throwEvalError ("builtins.head: expected a list, got " <> typeName other)

builtinTail :: (MonadEval m) => NixValue -> m NixValue
builtinTail (VList cl)
  | clistLen cl == 0 = throwEvalError "builtins.tail: empty list"
  | otherwise = pure (VList (clistFromThunks (drop 1 (clistThunks cl))))
builtinTail other = throwEvalError ("builtins.tail: expected a list, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - string (arity 1)
-- ---------------------------------------------------------------------------

-- | Byte length, as upstream: stringLength of a 2-byte character is 2, not 1.
builtinStringLength :: (MonadEval m) => NixValue -> m NixValue
builtinStringLength (VStr s _) = pure (VInt (fromIntegral (BS.length s)))
builtinStringLength other =
  throwEvalError ("builtins.stringLength: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - control
-- ---------------------------------------------------------------------------

-- | The error channel is Text and display-only (tryEval discards the
-- message), so throw/abort messages decode lossily.
builtinThrow :: (MonadEval m) => NixValue -> m NixValue
builtinThrow (VStr msg _) = throwCatchableError (bytesToTextLossy msg)
builtinThrow other = throwEvalError ("builtins.throw: expected a string, got " <> typeName other)

builtinAbort :: (MonadEval m) => NixValue -> m NixValue
builtinAbort (VStr msg _) = abortEvaluation (bytesToTextLossy msg)
builtinAbort other = abortEvaluation ("builtins.abort: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - attr set (arity 1)
-- ---------------------------------------------------------------------------

builtinAttrNames :: (MonadEval m) => NixValue -> m NixValue
builtinAttrNames (VAttrs attrs) =
  -- Nix returns attribute names lexicographically sorted; the C array is in
  -- interned-symbol order, so sort here (consistent with builtins.attrValues,
  -- which sorts via attrSetElems).
  let thunks = map (evaluated . mkStr) (sort (attrSetKeys attrs))
   in pure (VList (clistFromThunks (map thunkToCPtr thunks)))
builtinAttrNames other =
  throwEvalError ("builtins.attrNames: expected a set, got " <> typeName other)

builtinAttrValues :: (MonadEval m) => NixValue -> m NixValue
builtinAttrValues (VAttrs attrs) =
  pure (VList (clistFromThunks (map thunkToCPtr (attrSetElems attrs))))
builtinAttrValues other =
  throwEvalError ("builtins.attrValues: expected a set, got " <> typeName other)

builtinListToAttrs :: (MonadEval m) => NixValue -> m NixValue
builtinListToAttrs (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  pairs <- mapM listToAttrsPair thunks
  -- Nix listToAttrs uses first-wins: if duplicate name, first element wins.
  let firstWins = Map.fromListWith (\_ kept -> kept) pairs
  pure (VAttrs (attrSetFromMap firstWins))
builtinListToAttrs other =
  throwEvalError ("builtins.listToAttrs: expected a list, got " <> typeName other)

-- | Extract { name, value } from a thunk for listToAttrs.
listToAttrsPair :: (MonadEval m) => Thunk -> m (Text, Thunk)
listToAttrsPair thunk = do
  val <- force thunk
  case val of
    VAttrs attrs -> do
      nameThunk <-
        maybe (throwEvalError "builtins.listToAttrs: element missing 'name'") pure $
          attrSetLookup "name" attrs
      nameVal <- force nameThunk
      case nameVal of
        VStr keyName _ ->
          case attrSetLookup "value" attrs of
            Just valueThunk -> do
              key <- decodedText "builtins.listToAttrs: attribute name" keyName
              pure (key, valueThunk)
            Nothing -> throwEvalError "builtins.listToAttrs: element missing 'value'"
        _ -> throwEvalError "builtins.listToAttrs: 'name' must be a string"
    _ -> throwEvalError "builtins.listToAttrs: element must be a set"

-- ---------------------------------------------------------------------------
-- Builtin implementations - attr set (arity 2)
-- ---------------------------------------------------------------------------

builtinHasAttr :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinHasAttr (VStr key _) (VAttrs attrs) = do
  name <- decodedText "builtins.hasAttr" key
  pure (VBool (attrSetMember name attrs))
builtinHasAttr (VStr _ _) other =
  throwEvalError ("builtins.hasAttr: expected a set, got " <> typeName other)
builtinHasAttr other _ =
  throwEvalError ("builtins.hasAttr: expected a string, got " <> typeName other)

builtinGetAttr :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinGetAttr (VStr key _) (VAttrs attrs) = do
  name <- decodedText "builtins.getAttr" key
  case attrSetLookup name attrs of
    Just thunk -> force thunk
    Nothing -> throwEvalError ("builtins.getAttr: attribute '" <> name <> "' not found")
builtinGetAttr (VStr _ _) other =
  throwEvalError ("builtins.getAttr: expected a set, got " <> typeName other)
builtinGetAttr other _ =
  throwEvalError ("builtins.getAttr: expected a string, got " <> typeName other)

builtinRemoveAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinRemoveAttrs (VAttrs attrs) (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  keys <- mapM forceToString thunks
  pure (VAttrs (attrSetRemoveKeys keys attrs))
  where
    forceToString thunk = do
      val <- force thunk
      case val of
        VStr s _ -> decodedText "builtins.removeAttrs" s
        _ -> throwEvalError "builtins.removeAttrs: key list must contain strings"
builtinRemoveAttrs (VAttrs _) other =
  throwEvalError ("builtins.removeAttrs: expected a list, got " <> typeName other)
builtinRemoveAttrs other _ =
  throwEvalError ("builtins.removeAttrs: expected a set, got " <> typeName other)

builtinIntersectAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinIntersectAttrs (VAttrs a) (VAttrs b) =
  -- Iterate keys of 'a' (typically the smaller set, e.g. functionArgs)
  -- and point-lookup each in 'b' (typically the large set, e.g. nixpkgs).
  -- This avoids materializing all thunks in 'b'.
  let keysA = attrSetKeys a
      result = Map.fromList [(k, thunk) | k <- keysA, Just thunk <- [attrSetLookup k b]]
   in pure (VAttrs (attrSetFromMap result))
builtinIntersectAttrs (VAttrs _) other =
  throwEvalError ("builtins.intersectAttrs: expected a set, got " <> typeName other)
builtinIntersectAttrs other _ =
  throwEvalError ("builtins.intersectAttrs: expected a set, got " <> typeName other)

builtinCatAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinCatAttrs (VStr key _) (VList cl) = do
  name <- decodedText "builtins.catAttrs" key
  let thunks = map Thunk (clistThunks cl)
  vals <- catAttrsCollect name thunks
  pure (VList (clistFromThunks (map thunkToCPtr vals)))
builtinCatAttrs (VStr _ _) other =
  throwEvalError ("builtins.catAttrs: expected a list, got " <> typeName other)
builtinCatAttrs other _ =
  throwEvalError ("builtins.catAttrs: expected a string, got " <> typeName other)

-- | Collect values for a given key from a list of attrsets.
-- Tail-recursive with accumulator to avoid stack overflow on large lists.
catAttrsCollect :: (MonadEval m) => Text -> [Thunk] -> m [Thunk]
catAttrsCollect key = go []
  where
    go !acc [] = pure (reverse acc)
    go !acc (thunk : rest) = do
      val <- force thunk
      case val of
        VAttrs attrs ->
          case attrSetLookup key attrs of
            Just found -> go (found : acc) rest
            Nothing -> go acc rest
        _ -> throwEvalError "builtins.catAttrs: list element must be a set"

-- ---------------------------------------------------------------------------
-- Builtin implementations - list higher-order (arity 2)
-- ---------------------------------------------------------------------------

builtinMap :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinMap func (VList cl) =
  -- Lazy: each element is a deferred application, forced only on demand.
  let thunks = map Thunk (clistThunks cl)
   in pure (VList (clistFromThunks (map (thunkToCPtr . deferApply func) thunks)))
builtinMap _ other =
  throwEvalError ("builtins.map: expected a list, got " <> typeName other)

builtinFilter :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinFilter predFn (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  filtered <- filterThunks predFn thunks
  pure (VList (clistFromThunks (map thunkToCPtr filtered)))
builtinFilter _ other =
  throwEvalError ("builtins.filter: expected a list, got " <> typeName other)

filterThunks :: (MonadEval m) => NixValue -> [Thunk] -> m [Thunk]
filterThunks _ [] = pure []
filterThunks predFn (thunk : rest) = do
  result <- applyValueLazy predFn thunk
  case result of
    VBool True -> (thunk :) <$> filterThunks predFn rest
    VBool False -> filterThunks predFn rest
    _ -> throwEvalError "builtins.filter: predicate must return a bool"

builtinGenList :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinGenList func (VInt n)
  | n < 0 = throwEvalError "builtins.genList: length must be non-negative"
  | otherwise =
      -- Lazy: each element is a deferred @f i@, forced only on demand.
      -- Slot 0 = function.
      let fnThunk = evaluated func
          (sp, sc) = buildCSlots [fnThunk]
          env = newMinimalEnv sp sc
          mkIndexThunk i = mkThunk env (EApp (EResolvedVar 0 0) (ELit (NixInt i)))
       in pure (VList (clistFromThunks (map (thunkToCPtr . mkIndexThunk) [0 .. n - 1])))
builtinGenList _ other =
  throwEvalError ("builtins.genList: expected an integer, got " <> typeName other)

builtinSort :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinSort comparator (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  vals <- mapM force thunks
  sorted <- mergeSort comparator vals
  pure (VList (clistFromThunks (map (thunkToCPtr . evaluated) sorted)))
builtinSort _ other =
  throwEvalError ("builtins.sort: expected a list, got " <> typeName other)

-- | Stable O(n log n) merge sort using a user-supplied comparator.
-- The comparator takes two args (curried) and returns bool.
mergeSort :: (MonadEval m) => NixValue -> [NixValue] -> m [NixValue]
mergeSort _ [] = pure []
mergeSort _ [x] = pure [x]
mergeSort cmp xs = do
  let half = length xs `div` 2
      (left, right) = splitAt half xs
  sortedLeft <- mergeSort cmp left
  sortedRight <- mergeSort cmp right
  mergeSorted cmp sortedLeft sortedRight

mergeSorted :: (MonadEval m) => NixValue -> [NixValue] -> [NixValue] -> m [NixValue]
mergeSorted _ [] ys = pure ys
mergeSorted _ xs [] = pure xs
mergeSorted cmp (x : xs) (y : ys) = do
  -- Stable merge: take the right element only when it is STRICTLY less than the
  -- left (@cmp y x@).  On a tie - neither strictly less - take the left element,
  -- so comparator-equal elements keep their input order, matching C++ Nix's
  -- std::stable_sort.  (Comparing @cmp x y@ instead would emit @y@ on a tie and
  -- reverse equal runs.)
  partial <- applyValue cmp y
  result <- applyValue partial x
  case result of
    VBool True -> (y :) <$> mergeSorted cmp (x : xs) ys
    VBool False -> (x :) <$> mergeSorted cmp xs (y : ys)
    _ -> throwEvalError "builtins.sort: comparator must return a bool"

builtinConcatMap :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinConcatMap func (VList cl) = do
  -- Semi-eager: must force each application to discover list structure for
  -- concatenation, but element thunks within those sub-lists stay lazy.
  let thunks = map Thunk (clistThunks cl)
      deferredApps = map (deferApply func) thunks
  results <- mapM force deferredApps
  concatted <- mapM extractList results
  pure (VList (clistFromThunks (map thunkToCPtr (concat concatted))))
builtinConcatMap _ other =
  throwEvalError ("builtins.concatMap: expected a list, got " <> typeName other)

extractList :: (MonadEval m) => NixValue -> m [Thunk]
extractList (VList cl) = pure (map Thunk (clistThunks cl))
extractList other =
  throwEvalError ("builtins.concatMap: function must return a list, got " <> typeName other)

builtinAny :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAny predFn (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  result <- anyThunk predFn thunks
  pure (VBool result)
builtinAny _ other =
  throwEvalError ("builtins.any: expected a list, got " <> typeName other)

anyThunk :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
anyThunk _ [] = pure False
anyThunk predFn (thunk : rest) = do
  result <- applyValueLazy predFn thunk
  case result of
    VBool True -> pure True
    VBool False -> anyThunk predFn rest
    _ -> throwEvalError "builtins.any: predicate must return a bool"

builtinAll :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAll predFn (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  result <- allThunk predFn thunks
  pure (VBool result)
builtinAll _ other =
  throwEvalError ("builtins.all: expected a list, got " <> typeName other)

allThunk :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
allThunk _ [] = pure True
allThunk predFn (thunk : rest) = do
  result <- applyValueLazy predFn thunk
  case result of
    VBool True -> allThunk predFn rest
    VBool False -> pure False
    _ -> throwEvalError "builtins.all: predicate must return a bool"

builtinElem :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinElem needle (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  found <- elemCheck needle thunks
  pure (VBool found)
builtinElem _ other =
  throwEvalError ("builtins.elem: expected a list, got " <> typeName other)

elemCheck :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
elemCheck _ [] = pure False
elemCheck needle (thunk : rest) = do
  val <- force thunk
  eq <- nixEqual force needle val
  if eq then pure True else elemCheck needle rest

builtinElemAt :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinElemAt (VList cl) (VInt idx)
  | idx < 0 || fromIntegral idx >= clistLen cl = elemAtOOB idx cl
  | otherwise =
      -- O(1) direct C array access instead of materializing the whole list
      let ptr = unsafePerformIO (clistGet (unCList cl) (fromIntegral idx))
       in force (Thunk ptr)
  where
    elemAtOOB i c =
      throwEvalError
        ( "builtins.elemAt: index "
            <> T.pack (show i)
            <> " out of bounds for list of length "
            <> T.pack (show (clistLen c))
        )
builtinElemAt (VList _) other =
  throwEvalError ("builtins.elemAt: expected an integer, got " <> typeName other)
builtinElemAt other _ =
  throwEvalError ("builtins.elemAt: expected a list, got " <> typeName other)

builtinPartition :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinPartition predFn (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  (rightThunks, wrongThunks) <- partitionThunks predFn thunks
  pure
    ( VAttrs
        ( attrSetFromMap $
            Map.fromList
              [ ("right", evaluated (VList (clistFromThunks (map thunkToCPtr rightThunks)))),
                ("wrong", evaluated (VList (clistFromThunks (map thunkToCPtr wrongThunks))))
              ]
        )
    )
builtinPartition _ other =
  throwEvalError ("builtins.partition: expected a list, got " <> typeName other)

-- | Tail-recursive partition with accumulator to avoid stack overflow.
partitionThunks :: (MonadEval m) => NixValue -> [Thunk] -> m ([Thunk], [Thunk])
partitionThunks predFn = go [] []
  where
    go !rs !ws [] = pure (reverse rs, reverse ws)
    go !rs !ws (thunk : rest) = do
      result <- applyValueLazy predFn thunk
      case result of
        VBool True -> go (thunk : rs) ws rest
        VBool False -> go rs (thunk : ws) rest
        _ -> throwEvalError "builtins.partition: predicate must return a bool"

builtinGroupBy :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinGroupBy func (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  groups <- groupByCollect func thunks Map.empty
  pure (VAttrs (attrSetFromMap (Map.map (evaluated . VList . clistFromThunks . map thunkToCPtr . reverse) groups)))
builtinGroupBy _ other =
  throwEvalError ("builtins.groupBy: expected a list, got " <> typeName other)

groupByCollect ::
  (MonadEval m) =>
  NixValue ->
  [Thunk] ->
  Map Text [Thunk] ->
  m (Map Text [Thunk])
groupByCollect _ [] acc = pure acc
groupByCollect func (thunk : rest) acc = do
  result <- applyValueLazy func thunk
  case result of
    VStr key _ -> do
      name <- decodedText "builtins.groupBy" key
      groupByCollect func rest (Map.insertWith (++) name [thunk] acc)
    _ -> throwEvalError "builtins.groupBy: function must return a string"

-- ---------------------------------------------------------------------------
-- Builtin implementations - string (arity 2)
-- ---------------------------------------------------------------------------

builtinConcatStringsSep :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinConcatStringsSep (VStr sep sepCtx) (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  pairs <- mapM forceToStrCtx thunks
  let texts = map fst pairs
      mergedCtx = sepCtx <> mconcat (map snd pairs)
  pure (VStr (BS.intercalate sep texts) mergedCtx)
  where
    forceToStrCtx thunk = do
      val <- force thunk
      -- Nix 2.24's coerceToString defaults copyToStore=true and
      -- concatStringsSep passes no override, so a path element is copied into
      -- the store and the result carries its SCPlain context.  Non-string
      -- scalars still error (coerceMore=False), matching interpolation.
      coerceToStringInterp val
builtinConcatStringsSep (VStr _ _) other =
  throwEvalError ("builtins.concatStringsSep: expected a list, got " <> typeName other)
builtinConcatStringsSep other _ =
  throwEvalError ("builtins.concatStringsSep: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - arity 3
-- ---------------------------------------------------------------------------

builtinFoldl :: (MonadEval m) => NixValue -> NixValue -> NixValue -> m NixValue
builtinFoldl op initial (VList cl) =
  foldlStrict op initial (map Thunk (clistThunks cl))
builtinFoldl _ _ other =
  throwEvalError ("builtins.foldl': expected a list, got " <> typeName other)

-- | Strict left fold: apply @op acc elem@ for each element.  The
-- accumulator is forced each step (upstream foldl' strictness); the
-- element is passed as an unforced thunk, as upstream does.
foldlStrict :: (MonadEval m) => NixValue -> NixValue -> [Thunk] -> m NixValue
foldlStrict _ acc [] = pure acc
foldlStrict op acc (thunk : rest) = do
  partial <- applyValue op acc
  stepped <- applyValueLazy partial thunk
  foldlStrict op stepped rest

-- | Byte-indexed slicing, as upstream: offsets and lengths count bytes,
-- and a slice may fall mid-codepoint (the resulting bytes are the value).
builtinSubstring :: (MonadEval m) => NixValue -> NixValue -> NixValue -> m NixValue
builtinSubstring (VInt start) (VInt len) (VStr s ctx)
  | start < 0 = throwEvalError "builtins.substring: negative start position"
  | otherwise =
      let startPos = fromIntegral start
          -- Nix clamps len to available length (negative len means rest of string)
          available = BS.length s - startPos
          clampedLen =
            if len < 0
              then available
              else min (fromIntegral len) available
       in -- Context is preserved through substring (matching real Nix).
          pure (VStr (BS.take clampedLen (BS.drop startPos s)) ctx)
builtinSubstring _ _ (VStr _ _) =
  throwEvalError "builtins.substring: start and length must be integers"
builtinSubstring _ _ other =
  throwEvalError ("builtins.substring: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin helpers
-- ---------------------------------------------------------------------------

-- | Build a thunk that defers @f arg@ - the application only happens when
-- the thunk is forced.  Reuses the existing eval machinery via a synthetic
-- @EApp (EResolvedVar 0 0) (EResolvedVar 0 1)@ in a self-contained env.
-- Slot 0 = function, slot 1 = argument.
deferApply :: NixValue -> Thunk -> Thunk
deferApply func argThunk =
  let (sp, sc) = buildCSlots [evaluated func, argThunk]
      env = newMinimalEnv sp sc
   in mkSyntheticThunk env deferApplyExpr

-- | Shared expression for 'deferApply'.  Allocated once as a CAF.
deferApplyExpr :: Expr
deferApplyExpr = EApp (EResolvedVar 0 0) (EResolvedVar 0 1)
{-# NOINLINE deferApplyExpr #-}

-- | Permissive coercion used by @builtins.toString@.
--
-- Like 'coerceToString' but additionally handles lists: elements are
-- recursively coerced and joined with spaces, matching real Nix semantics.
-- @toString [1 2 3]@ gives @"1 2 3"@.
coerceToStringPermissive :: (MonadEval m) => NixValue -> m (BS.ByteString, StringContext)
coerceToStringPermissive (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  parts <- mapM coerceThunk thunks
  let texts = map fst parts
      ctx = mconcat (map snd parts)
  pure (BS.intercalate " " texts, ctx)
  where
    coerceThunk thunk = do
      val <- force thunk
      coerceToStringPermissive val
coerceToStringPermissive other = coerceToString True force applyValue other

-- | Coerce a value to a string for a DERIVATION field (an env value or an
-- arg).  Like 'coerceToStringPermissive', but a path literal is copied into
-- the store: it becomes its source store path, with that path added to the
-- string context so it lands in the derivation's @inputSrcs@ - matching C++
-- Nix's copy-to-store coercion of paths in derivation arguments/environment.
coerceToStoreString :: (MonadEval m) => NixValue -> m (BS.ByteString, StringContext)
coerceToStoreString (VPath p) = do
  (spText, ctx) <- sourcePathWithContext p
  pure (TE.encodeUtf8 spText, ctx)
coerceToStoreString (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  parts <- mapM (force >=> coerceToStoreString) thunks
  pure (BS.intercalate " " (map fst parts), mconcat (map snd parts))
coerceToStoreString other = coerceToStringPermissive other

-- | Coerce a value for string interpolation (@"${...}"@).  Like
-- 'coerceToString', but a path literal is copied into the store and replaced by
-- its source store path (with context) - matching C++ Nix, where interpolation
-- uses @copyToStore = true@, unlike 'builtins.toString', which does not copy.
coerceToStringInterp :: (MonadEval m) => NixValue -> m (BS.ByteString, StringContext)
coerceToStringInterp (VPath p) = do
  (spText, ctx) <- sourcePathWithContext p
  pure (TE.encodeUtf8 spText, ctx)
coerceToStringInterp other = coerceToString False force applyValue other

-- | The store path a source path literal coerces to, carrying its
-- SCPlain context - the copy-to-store coercion shared by interpolation,
-- derivation arguments/env values, and @builtins.toJSON@.
sourcePathWithContext :: (MonadEval m) => Text -> m (Text, StringContext)
sourcePathWithContext p
  -- An already-in-store path coerces to itself with an SCPlain (Opaque)
  -- reference - never re-copied.  This is the shared choke point for
  -- interpolation, derivation args, concatStringsSep, and toJSON, so all of
  -- them inherit the in-store short-circuit (and the Windows re-NAR fix).
  | Just sp <- enclosingStorePath p = pure (p, plainContext sp)
  | otherwise = do
      spText <- storeSourcePath p
      case parseStorePath defaultStoreDir spText of
        Just sp -> pure (spText, plainContext sp)
        Nothing -> pure (spText, mempty)

-- | The current system platform string.
currentSystemStr :: Text
currentSystemStr = case (System.Info.arch, System.Info.os) of
  ("x86_64", "mingw32") -> "x86_64-windows"
  ("x86_64", "darwin") -> "x86_64-darwin"
  ("aarch64", "darwin") -> "aarch64-darwin"
  ("aarch64", "linux") -> "aarch64-linux"
  ("x86_64", "linux") -> "x86_64-linux"
  (arch, os) -> T.pack arch <> "-" <> T.pack os

-- | Store dir with trailing slash, for building store paths.
storeDirPrefix :: Text
storeDirPrefix = defaultStoreDirText <> "/"

-- ---------------------------------------------------------------------------
-- Builtin implementations - numeric + context
-- ---------------------------------------------------------------------------

isPathVal :: NixValue -> Bool
isPathVal (VPath _) = True
isPathVal _ = False

builtinCeil :: (MonadEval m) => NixValue -> m NixValue
builtinCeil (VFloat f) = intFromDouble "builtins.ceil" ceiling f
builtinCeil (VInt n) = pure (VInt n)
builtinCeil other = throwEvalError ("builtins.ceil: expected a number, got " <> typeName other)

builtinFloor :: (MonadEval m) => NixValue -> m NixValue
builtinFloor (VFloat f) = intFromDouble "builtins.floor" floor f
builtinFloor (VInt n) = pure (VInt n)
builtinFloor other = throwEvalError ("builtins.floor: expected a number, got " <> typeName other)

-- | Round a float to Int64 with the checks Nix 2.24 performs: NaN,
-- infinity, and out-of-range values are eval errors, never a garbage
-- Int64 out of Haskell's unchecked conversion.
intFromDouble :: (MonadEval m) => Text -> (Double -> Integer) -> Double -> m NixValue
intFromDouble ctx rounder f
  | isNaN f = throwEvalError (ctx <> ": NaN cannot be converted to an integer")
  | isInfinite f = throwEvalError (ctx <> ": infinity cannot be converted to an integer")
  | rounded < toInteger (minBound :: Int64) || rounded > toInteger (maxBound :: Int64) =
      throwEvalError (ctx <> ": " <> formatNixFloat f <> " is out of integer range")
  | otherwise = pure (VInt (fromInteger rounded))
  where
    rounded = rounder f

builtinDiscardContext :: (MonadEval m) => NixValue -> m NixValue
builtinDiscardContext (VStr s _) = pure (mkStrBytes s)
builtinDiscardContext other =
  throwEvalError ("builtins.unsafeDiscardStringContext: expected a string, got " <> typeName other)

-- | @builtins.unsafeDiscardOutputDependency@ - downgrade all-outputs (DrvDeep)
-- references to a plain (Opaque) reference on the same @.drv@ path, and KEEP
-- derivation-output (Built) references unchanged, matching upstream.  It
-- formerly dropped both kinds and kept only plain references, losing the
-- @.drv@ reference the downgrade must preserve.  'Set.map' also folds a
-- downgraded element into an existing plain reference on the same path.
builtinDiscardOutputDep :: (MonadEval m) => NixValue -> m NixValue
builtinDiscardOutputDep (VStr s (StringContext ctx)) =
  pure (VStr s (StringContext (Set.map downgrade ctx)))
  where
    downgrade (SCAllOutputs sp) = SCPlain sp
    downgrade other = other
builtinDiscardOutputDep other =
  throwEvalError ("builtins.unsafeDiscardOutputDependency: expected a string, got " <> typeName other)

-- | @builtins.addDrvOutputDependencies@ - the inverse of the
-- 'builtinDiscardOutputDep' downgrade for a single element: upgrade a plain
-- (Opaque) reference to a @.drv@ path into an all-outputs (DrvDeep) reference.
-- Upstream requires the string's context to have exactly one element and
-- errors on a derivation-output (Built) element or a non-@.drv@ plain path.
builtinAddDrvOutputDeps :: (MonadEval m) => NixValue -> m NixValue
builtinAddDrvOutputDeps (VStr s (StringContext ctx)) =
  case Set.toList ctx of
    [only] -> VStr s . StringContext . Set.singleton <$> upgrade only
    _ ->
      throwEvalError
        ( "builtins.addDrvOutputDependencies: the string must have exactly one "
            <> "context element, but has "
            <> T.pack (show (Set.size ctx))
        )
  where
    upgrade (SCPlain sp)
      | ".drv" `T.isSuffixOf` spName sp = pure (SCAllOutputs sp)
      | otherwise =
          throwEvalError
            ( "builtins.addDrvOutputDependencies: path "
                <> storePathToText defaultStoreDir sp
                <> " is not a derivation"
            )
    upgrade (SCAllOutputs sp) = pure (SCAllOutputs sp)
    upgrade (SCDrvOutput _ outName) =
      throwEvalError
        ( "builtins.addDrvOutputDependencies: can only act on derivations, not "
            <> "on a derivation output such as "
            <> outName
        )
builtinAddDrvOutputDeps other =
  throwEvalError ("builtins.addDrvOutputDependencies: expected a string, got " <> typeName other)

-- | Check whether a string has any context elements.
builtinHasContext :: (MonadEval m) => NixValue -> m NixValue
builtinHasContext (VStr _ ctx) = pure (VBool (ctx /= emptyContext))
builtinHasContext other =
  throwEvalError ("builtins.hasContext: expected a string, got " <> typeName other)

-- | Return the context of a string as an attrset.
--
-- Each key is a store path string.  Each value is an attrset with:
--   - @path@: true if there's a SCPlain reference
--   - @allOutputs@: true if there's a SCAllOutputs reference
--   - @outputs@: list of output names from SCDrvOutput references
builtinGetContext :: (MonadEval m) => NixValue -> m NixValue
builtinGetContext (VStr _ (StringContext ctx)) = do
  let grouped = groupContextByPath (Set.toList ctx)
      attrMap = Map.map contextEntryToAttrs grouped
  pure (VAttrs (attrSetFromMap attrMap))
builtinGetContext other =
  throwEvalError ("builtins.getContext: expected a string, got " <> typeName other)

-- | Intermediate representation for grouping context elements by store path.
data ContextEntry = ContextEntry
  { cePath :: !Bool,
    ceAllOutputs :: !Bool,
    ceOutputs :: ![Text]
  }

-- | Group context elements by their store path.
groupContextByPath :: [StringContextElement] -> Map Text ContextEntry
groupContextByPath = foldl' addElement Map.empty
  where
    addElement acc (SCPlain sp) =
      Map.insertWith mergeEntry (spToText sp) (ContextEntry True False []) acc
    addElement acc (SCDrvOutput sp outName) =
      Map.insertWith mergeEntry (spToText sp) (ContextEntry False False [outName]) acc
    addElement acc (SCAllOutputs sp) =
      Map.insertWith mergeEntry (spToText sp) (ContextEntry False True []) acc
    -- 'Set.toList' feeds elements in ascending order and 'old' holds the
    -- earlier (smaller) output names, so 'old ++ new' keeps the rendered
    -- outputs list ascending, as upstream getContext produces it.
    mergeEntry new old =
      ContextEntry
        (cePath new || cePath old)
        (ceAllOutputs new || ceAllOutputs old)
        (ceOutputs old ++ ceOutputs new)
    -- Context keys are identity, not IO: always the canonical /nix/store
    -- spelling, never the platform file-path mapping.
    spToText = storePathToText defaultStoreDir

-- | Convert a ContextEntry to an attrset thunk.
contextEntryToAttrs :: ContextEntry -> Thunk
contextEntryToAttrs entry =
  let fields =
        [("path", evaluated (VBool True)) | cePath entry]
          ++ [("allOutputs", evaluated (VBool True)) | ceAllOutputs entry]
          ++ [("outputs", evaluated (VList (clistFromThunks [thunkToCPtr (evaluated (mkStr o)) | o <- ceOutputs entry]))) | not (null (ceOutputs entry))]
   in evaluated (VAttrs (attrSetFromMap (Map.fromList fields)))

-- | Append context entries to a string from an attrset.
--
-- @builtins.appendContext string contextAttrset@ adds the specified
-- context elements to the string.
builtinAppendContext :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAppendContext (VStr s ctx) (VAttrs contextAttrs) = do
  newCtx <- parseContextAttrs (attrSetToMap contextAttrs)
  pure (VStr s (ctx <> newCtx))
builtinAppendContext (VStr _ _) other =
  throwEvalError ("builtins.appendContext: second argument must be a set, got " <> typeName other)
builtinAppendContext other _ =
  throwEvalError ("builtins.appendContext: first argument must be a string, got " <> typeName other)

-- | Parse a context attrset into a StringContext.
-- Each key is a store path; each value is an attrset with optional
-- @path@, @allOutputs@, and @outputs@ fields.
parseContextAttrs :: (MonadEval m) => Map Text Thunk -> m StringContext
parseContextAttrs attrs = do
  elements <- mapM parseOneCtx (Map.toList attrs)
  pure (StringContext (Set.fromList (concat elements)))
  where
    parseOneCtx (pathText, thunk) = do
      val <- force thunk
      case val of
        VAttrs inner -> do
          let sp = case parseStorePath defaultStoreDir pathText of
                Just parsed -> parsed
                Nothing -> StorePath (T.take 32 (T.drop (T.length storeDirPrefix) pathText)) (T.drop 33 (T.drop (T.length storeDirPrefix) pathText))
          hasPath <- getBoolAttr "path" inner
          hasAllOuts <- getBoolAttr "allOutputs" inner
          outNames <- getOutputsList inner
          let pathElems = [SCPlain sp | hasPath]
              allOutElems = [SCAllOutputs sp | hasAllOuts]
              outElems = [SCDrvOutput sp o | o <- outNames]
          pure (pathElems ++ allOutElems ++ outElems)
        _ -> throwEvalError "builtins.appendContext: context entry must be a set"

    getBoolAttr key attrs' = case attrSetLookup key attrs' of
      Nothing -> pure False
      Just thunk -> do
        val <- force thunk
        case val of
          VBool b -> pure b
          _ -> pure False

    getOutputsList attrs' = case attrSetLookup "outputs" attrs' of
      Nothing -> pure []
      Just thunk -> do
        val <- force thunk
        case val of
          VList cl -> mapM (forceToOutputName . Thunk) (clistThunks cl)
          _ -> pure []

    forceToOutputName thunk = do
      val <- force thunk
      case val of
        VStr s _ -> decodedText "builtins.appendContext" s
        _ -> throwEvalError "builtins.appendContext: output name must be a string"

builtinBaseNameOf :: (MonadEval m) => NixValue -> m NixValue
builtinBaseNameOf (VStr s ctx) = pure (VStr (lastComponentBytes s) ctx)
builtinBaseNameOf (VPath p) = pure (mkStr (lastComponent p))
builtinBaseNameOf other =
  throwEvalError ("builtins.baseNameOf: expected a string or path, got " <> typeName other)

lastComponent :: Text -> Text
lastComponent t = case reverse (filter (not . T.null) (T.splitOn "/" t)) of
  [] -> ""
  (final : _) -> final

-- | Byte-level 'lastComponent' for string operands ('/' is a single byte
-- in UTF-8 and never occurs inside a multi-byte sequence).
lastComponentBytes :: BS.ByteString -> BS.ByteString
lastComponentBytes t = case reverse (filter (not . BS.null) (BC.split '/' t)) of
  [] -> ""
  (final : _) -> final

builtinDirOf :: (MonadEval m) => NixValue -> m NixValue
builtinDirOf (VStr s ctx) = pure (VStr (dirComponentBytes s) ctx)
builtinDirOf (VPath p) = pure (VPath (dirComponent p))
builtinDirOf other =
  throwEvalError ("builtins.dirOf: expected a string or path, got " <> typeName other)

dirComponent :: Text -> Text
dirComponent t =
  case T.findIndex (== '/') (T.reverse t) of
    Nothing -> "."
    Just n ->
      let dir = T.take (T.length t - n - 1) t
       in -- A leading or sole '/' leaves an empty prefix; Nix's dirOf yields "/".
          if T.null dir then "/" else dir

-- | Byte-level 'dirComponent' for string operands: everything before the
-- last '/' byte, with the same "." / "/" edge results.
dirComponentBytes :: BS.ByteString -> BS.ByteString
dirComponentBytes t =
  case BC.elemIndexEnd '/' t of
    Nothing -> "."
    Just idx ->
      let dir = BS.take idx t
       in if BS.null dir then "/" else dir

builtinConcatLists :: (MonadEval m) => NixValue -> m NixValue
builtinConcatLists (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  sublists <- mapM forceThenExtractList thunks
  pure (VList (clistFromThunks (concat sublists)))
  where
    forceThenExtractList thunk = do
      val <- force thunk
      case val of
        VList innerCl -> pure (clistThunks innerCl)
        _ -> throwEvalError "builtins.concatLists: element must be a list"
builtinConcatLists other =
  throwEvalError ("builtins.concatLists: expected a list, got " <> typeName other)

builtinLessThan :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinLessThan a b = VBool <$> nixCompare force a b

-- ---------------------------------------------------------------------------
-- Builtin implementations - arithmetic + bitwise
-- ---------------------------------------------------------------------------

builtinAdd :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinAdd (VInt a) (VInt b) = either throwEvalError (pure . VInt) (checkedAdd a b)
builtinAdd (VInt a) (VFloat b) = pure (VFloat (fromIntegral a + b))
builtinAdd (VFloat a) (VInt b) = pure (VFloat (a + fromIntegral b))
builtinAdd (VFloat a) (VFloat b) = pure (VFloat (a + b))
builtinAdd l r = throwEvalError ("builtins.add: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinSub :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinSub (VInt a) (VInt b) = either throwEvalError (pure . VInt) (checkedSub a b)
builtinSub (VInt a) (VFloat b) = pure (VFloat (fromIntegral a - b))
builtinSub (VFloat a) (VInt b) = pure (VFloat (a - fromIntegral b))
builtinSub (VFloat a) (VFloat b) = pure (VFloat (a - b))
builtinSub l r = throwEvalError ("builtins.sub: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinMul :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinMul (VInt a) (VInt b) = either throwEvalError (pure . VInt) (checkedMul a b)
builtinMul (VInt a) (VFloat b) = pure (VFloat (fromIntegral a * b))
builtinMul (VFloat a) (VInt b) = pure (VFloat (a * fromIntegral b))
builtinMul (VFloat a) (VFloat b) = pure (VFloat (a * b))
builtinMul l r = throwEvalError ("builtins.mul: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinDiv :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinDiv _ (VInt 0) = throwEvalError "builtins.div: division by zero"
builtinDiv (VInt a) (VInt b)
  -- The one overflowing division: |minBound| has no representation.
  | a == minBound && b == -1 =
      throwEvalError
        ("integer overflow in dividing " <> T.pack (show a) <> " and " <> T.pack (show b))
  | otherwise = pure (VInt (quot a b))
builtinDiv _ (VFloat 0) = throwEvalError "builtins.div: division by zero"
builtinDiv (VInt a) (VFloat b) = pure (VFloat (fromIntegral a / b))
builtinDiv (VFloat a) (VInt b) = pure (VFloat (a / fromIntegral b))
builtinDiv (VFloat a) (VFloat b) = pure (VFloat (a / b))
builtinDiv l r = throwEvalError ("builtins.div: expected numbers, got " <> typeName l <> " and " <> typeName r)

builtinBitAnd :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinBitAnd (VInt a) (VInt b) = pure (VInt (a .&. b))
builtinBitAnd _ _ = throwEvalError "builtins.bitAnd: expected two integers"

builtinBitOr :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinBitOr (VInt a) (VInt b) = pure (VInt (a .|. b))
builtinBitOr _ _ = throwEvalError "builtins.bitOr: expected two integers"

builtinBitXor :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinBitXor (VInt a) (VInt b) = pure (VInt (xor a b))
builtinBitXor _ _ = throwEvalError "builtins.bitXor: expected two integers"

-- ---------------------------------------------------------------------------
-- Builtin implementations - attr set higher-order
-- ---------------------------------------------------------------------------

builtinMapAttrs :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinMapAttrs func (VAttrs attrs) =
  -- Each attr value is a deferred @f key val@, forced only on demand.
  -- Eagerly builds all thunks via attrSetMapWithKey - with C arena thunks
  -- (~16 bytes each), this is cheaper than the former MappedAttrs overhead
  -- and keeps all data off the GHC heap.
  -- Slot 0 = function, slot 1 = key, slot 2 = value.
  pure (VAttrs (attrSetMapWithKey deferAttr attrs))
  where
    deferAttr key valThunk =
      let (sp, sc) = buildCSlots [evaluated func, evaluated (mkStr key), valThunk]
          env = newMinimalEnv sp sc
       in mkSyntheticThunk env mapAttrsExpr
builtinMapAttrs _ other =
  throwEvalError ("builtins.mapAttrs: expected a set, got " <> typeName other)

-- | Shared expression for 'builtinMapAttrs'.  Allocated once as a CAF.
mapAttrsExpr :: Expr
mapAttrsExpr = EApp (EApp (EResolvedVar 0 0) (EResolvedVar 0 1)) (EResolvedVar 0 2)
{-# NOINLINE mapAttrsExpr #-}

-- | Formals for lambdas, an empty set for builtins, an error otherwise -
-- including functor sets: upstream's primop never consults
-- @__functionArgs@ (nixpkgs lib.functionArgs handles that in Nix).
builtinFunctionArgs :: (MonadEval m) => NixValue -> m NixValue
builtinFunctionArgs (VLambda _ formals _) = pure (formalsToAttrs formals)
builtinFunctionArgs (VBuiltin _ _) = pure (VAttrs (attrSetFromMap Map.empty))
builtinFunctionArgs other =
  throwEvalError ("builtins.functionArgs: expected a function, got " <> typeName other)

formalsToAttrs :: EvalFormals -> NixValue
formalsToAttrs (EFName _) = VAttrs (attrSetFromMap Map.empty)
formalsToAttrs (EFSet formals _) = formalsListToAttrs formals
formalsToAttrs (EFNamedSet _ formals _) = formalsListToAttrs formals

formalsListToAttrs :: [EvalFormal] -> NixValue
formalsListToAttrs formals =
  VAttrs $
    attrSetFromMap $
      Map.fromList
        [(efName f, evaluated (VBool (isJust (efDefault f)))) | f <- formals]

builtinZipAttrsWith :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinZipAttrsWith func (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  attrSets <- mapM forceToAttrSet thunks
  let merged = mergeAllAttrs attrSets
      -- Lazy: each result is a deferred f(name)(values) thunk, not eagerly
      -- evaluated.  Critical for nixpkgs evalModules fixpoint - config is a
      -- self-referencing lazy attrset that must be COMPUTED (holding the lazy
      -- result) before any individual attribute thunks are forced.
      resultPairs = map (deferZip func) (Map.toList merged)
  pure (VAttrs (attrSetFromMap (Map.fromList resultPairs)))
  where
    forceToAttrSet thunk = do
      val <- force thunk
      case val of
        VAttrs attrs -> pure (attrSetToMap attrs)
        _ -> throwEvalError "builtins.zipAttrsWith: list element must be a set"
    mergeAllAttrs = foldl' (\acc m -> Map.unionWith (++) acc (Map.map (: []) m)) Map.empty
    -- Slot 0 = function, slot 1 = name, slot 2 = values list.
    deferZip fn (key, thunkList) =
      let valueList = VList (clistFromThunks (map thunkToCPtr thunkList))
          (slots, slotCount) = buildCSlots [evaluated fn, evaluated (mkStr key), evaluated valueList]
          env = newMinimalEnv slots slotCount
       in (key, mkSyntheticThunk env mapAttrsExpr)
builtinZipAttrsWith _ other =
  throwEvalError ("builtins.zipAttrsWith: expected a list, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - string manipulation
-- ---------------------------------------------------------------------------

builtinReplaceStrings ::
  (MonadEval m) => NixValue -> NixValue -> NixValue -> m NixValue
builtinReplaceStrings (VList fromCl) (VList toCl) (VStr input inputCtx) = do
  let fromThunks = map Thunk (clistThunks fromCl)
      toThunks = map Thunk (clistThunks toCl)
  when (length fromThunks /= length toThunks) $
    throwEvalError "builtins.replaceStrings: 'from' and 'to' must have the same length"
  froms <- mapM forceStr fromThunks
  -- Match-gated replacement forcing, as upstream: a 'to' element is
  -- forced - and its context joins the result - only the first time its
  -- pattern matches, memoized per index.  An unmatched replacement is
  -- never evaluated, so it may throw or diverge harmlessly.
  --
  -- Matching and stepping are byte-level, as upstream: an empty @from@
  -- element inserts between BYTES, so a 2-byte character gets a
  -- replacement inside it.  Chunks accumulate reversed, one BS.concat
  -- at the end.
  let rules = zip3 [0 :: Int ..] (map fst froms) toThunks
      findRule txt =
        listToMaybe [r | r@(_, from, _) <- rules, BS.null from || from `BS.isPrefixOf` txt]
      forcedTo memo (i, _, toThunk) = case Map.lookup i memo of
        Just hit -> pure (hit, memo)
        Nothing -> do
          forced <- forceStr toThunk
          pure (forced, Map.insert i forced memo)
      step remaining acc memo
        | BS.null remaining = case findRule remaining of
            -- At end of string, still check for an empty-from match.
            Just rule -> do
              ((to, _), advanced) <- forcedTo memo rule
              pure (to : acc, advanced)
            Nothing -> pure (acc, memo)
        | otherwise = case findRule remaining of
            Just rule@(_, from, _) -> do
              ((to, _), advanced) <- forcedTo memo rule
              if BS.null from
                then case BS.uncons remaining of
                  -- empty-from: insert replacement then advance ONE BYTE
                  Just (byte, after) -> step after (BS.singleton byte : to : acc) advanced
                  -- Unreachable: the null string is handled by the guard above.
                  Nothing -> pure (to : acc, advanced)
                else step (BS.drop (BS.length from) remaining) (to : acc) advanced
            Nothing -> case BS.uncons remaining of
              Just (byte, after) -> step after (BS.singleton byte : acc) memo
              Nothing -> pure (acc, memo)
  (revChunks, forcedTos) <- step input [] Map.empty
  let mergedCtx = inputCtx <> mconcat (map snd (Map.elems forcedTos))
  pure (VStr (BS.concat (reverse revChunks)) mergedCtx)
  where
    forceStr thunk = do
      val <- force thunk
      case val of
        VStr s ctx -> pure (s, ctx)
        _ -> throwEvalError "builtins.replaceStrings: elements must be strings"
builtinReplaceStrings _ _ (VStr _ _) =
  throwEvalError "builtins.replaceStrings: first two arguments must be lists"
builtinReplaceStrings _ _ other =
  throwEvalError ("builtins.replaceStrings: expected a string, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - regex (POSIX ERE via regex-tdfa)
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Regex compilation cache
-- ---------------------------------------------------------------------------

-- | Global regex compilation cache.  Keyed by the raw pattern bytes
-- (match and split share entries).  Idempotent memoization via
-- unsafePerformIO - same rationale as thunk memoization.
{-# NOINLINE regexCacheRef #-}
regexCacheRef :: IORef (Map BS.ByteString RE.Regex)
regexCacheRef = unsafePerformIO (newIORef Map.empty)

-- | Compile options matching C++ Nix's POSIX ERE semantics: no multiline
-- mode, so @^@\/@$@ anchor only at the string boundaries and @.@ (and a
-- negated bracket class) match a newline.  regex-tdfa's default has
-- multiline=True, which silently diverges on any subject containing @\n@.
wholeStringCompOpt :: RE.CompOption
wholeStringCompOpt = RE.defaultCompOpt {RE.multiline = False}

-- | Compile a regex from its raw pattern bytes, using the global cache to
-- avoid recompilation.  Compiling from bytes makes the PATTERN byte-level
-- too, matching upstream: a multi-byte character in the pattern is a
-- sequence of single-byte atoms, so it lines up with byte-level subjects.
-- Returns Nothing for invalid patterns.  NOINLINE prevents GHC from
-- inlining and floating the unsafePerformIO reads.
{-# NOINLINE cachedCompileRegex #-}
cachedCompileRegex :: BS.ByteString -> Maybe RE.Regex
cachedCompileRegex pat =
  unsafePerformIO $ do
    cache <- atomicModifyIORef' regexCacheRef (\c -> (c, c))
    case Map.lookup pat cache of
      Just compiled -> pure (Just compiled)
      Nothing -> case RE.makeRegexOptsM wholeStringCompOpt RE.defaultExecOpt pat :: Maybe RE.Regex of
        Nothing -> pure Nothing
        Just compiled -> do
          atomicModifyIORef' regexCacheRef (\c -> (Map.insert pat compiled c, ()))
          pure (Just compiled)

-- ---------------------------------------------------------------------------
-- Regex builtins
-- ---------------------------------------------------------------------------

-- | @builtins.match regex str@: match a POSIX ERE against a string.
-- The regex must match the ENTIRE string (like C++ Nix's regex_match).
-- Returns @null@ if no match, or a list of capture group strings
-- (@null@ for non-participating groups).
builtinMatch :: (MonadEval m) => NixValue -> NixValue -> m NixValue
-- Pre-compiled path: regex was compiled at partial-application time.
builtinMatch (VCompiledRegex (CompiledRegex _ compiled)) (VStr str _) =
  matchWithCompiled compiled str
-- Direct 2-arg call: use global compilation cache.
builtinMatch (VStr regex _) (VStr str _) =
  case cachedCompileRegex regex of
    Nothing -> throwEvalError ("builtins.match: invalid regex: " <> bytesToTextLossy regex)
    Just compiled -> matchWithCompiled compiled str
builtinMatch (VStr _ _) other =
  throwEvalError ("builtins.match: expected a string, got " <> typeName other)
builtinMatch (VCompiledRegex _) other =
  throwEvalError ("builtins.match: expected a string, got " <> typeName other)
builtinMatch other _ =
  throwEvalError ("builtins.match: expected a string (regex), got " <> typeName other)

-- | Shared match logic for pre-compiled and freshly-compiled regex paths.
--
-- Whole-string semantics via a span check on the leftmost-longest match:
-- if any whole-string match exists it starts at 0, and POSIX picks the
-- longest match at the leftmost start, so the reported match spans the
-- whole subject exactly when a whole-string match exists.  This is what
-- regex_match gives C++ Nix; textual @^...$@ anchoring is NOT equivalent
-- (it misparses top-level alternation).
matchWithCompiled :: (MonadEval m) => RE.Regex -> BS.ByteString -> m NixValue
matchWithCompiled compiled str =
  case RE.matchOnceText compiled str of
    Just (beforeMatch, match, afterMatch)
      | BS.null beforeMatch,
        BS.null afterMatch ->
          -- match is an Array of (bytes, (offset, len)) pairs.
          -- Index 0 is the full match; indices 1.. are capture groups.
          let captureGroups = drop 1 (Array.elems match)
              -- A non-participating capture group has offset (-1); C++ Nix
              -- yields null for it, not the empty string.
              toThunk (s, (off, _)) =
                if off < 0 then evaluated VNull else evaluated (mkStrBytes s)
           in pure (VList (clistFromThunks (map (thunkToCPtr . toThunk) captureGroups)))
    _ -> pure VNull

-- | @builtins.split regex str@: split a string by a POSIX ERE.
-- Returns an alternating list of non-matched strings and match-group lists.
-- Example: @split "(x)" "axbxc"@ yields @["a" ["x"] "b" ["x"] "c"]@
builtinSplit :: (MonadEval m) => NixValue -> NixValue -> m NixValue
-- Pre-compiled path: regex was compiled at partial-application time.
builtinSplit (VCompiledRegex (CompiledRegex _ compiled)) (VStr str _) =
  splitWithCompiled compiled str
-- Direct 2-arg call: use global compilation cache.
builtinSplit (VStr regex _) (VStr str _) =
  case cachedCompileRegex regex of
    Nothing -> throwEvalError ("builtins.split: invalid regex: " <> bytesToTextLossy regex)
    Just compiled -> splitWithCompiled compiled str
builtinSplit (VStr _ _) other =
  throwEvalError ("builtins.split: expected a string, got " <> typeName other)
builtinSplit (VCompiledRegex _) other =
  throwEvalError ("builtins.split: expected a string, got " <> typeName other)
builtinSplit other _ =
  throwEvalError ("builtins.split: expected a string (regex), got " <> typeName other)

-- | Shared split logic for pre-compiled and freshly-compiled regex paths.
splitWithCompiled :: (MonadEval m) => RE.Regex -> BS.ByteString -> m NixValue
splitWithCompiled compiled str =
  let allMatches = matchAllText compiled str
      result = buildSplitResult str 0 allMatches
   in pure (VList (clistFromThunks (map thunkToCPtr result)))

-- | Build the alternating list for builtins.split.  Offsets from the
-- ByteString regex instance are byte offsets, so slicing is O(1)
-- 'BS.take'/'BS.drop'.
buildSplitResult :: BS.ByteString -> Int -> [Array.Array Int (BS.ByteString, (Int, Int))] -> [Thunk]
buildSplitResult remaining pos [] =
  -- No more matches - emit the rest of the string.
  [evaluated (mkStrBytes (BS.drop pos remaining))]
buildSplitResult remaining pos (match : rest) =
  let elems = Array.elems match
      (_, (matchStart, matchLen)) = case elems of
        (full : _) -> full
        [] -> ("", (pos, 0)) -- defensive: should not happen from matchAllText
        -- Bytes before this match
      before = BS.take (matchStart - pos) (BS.drop pos remaining)
      -- Capture groups (indices 1..)
      groups = drop 1 elems
      -- A non-participating capture group has offset (-1) becomes null (as 'match').
      groupThunks =
        map (\(s, (off, _)) -> if off < 0 then evaluated VNull else evaluated (mkStrBytes s)) groups
      -- Continue after this match
      afterPos = matchStart + matchLen
   in evaluated (mkStrBytes before)
        : evaluated (VList (clistFromThunks (map thunkToCPtr groupThunks)))
        : buildSplitResult remaining afterPos rest

builtinCompareVersions :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinCompareVersions (VStr a _) (VStr b _) =
  pure (VInt (compareVersionParts (splitVersionStr a) (splitVersionStr b)))
builtinCompareVersions _ _ = throwEvalError "builtins.compareVersions: expected two strings"

-- | Convert 'Ordering' to the Nix compareVersions convention: -1, 0, 1.
ordToNix :: Ordering -> Int64
ordToNix LT = -1
ordToNix EQ = 0
ordToNix GT = 1

compareVersionParts :: [BS.ByteString] -> [BS.ByteString] -> Int64
compareVersionParts [] [] = 0
-- When one version runs out, Nix pads the shorter side with an empty
-- component and keeps comparing - so "1.0" > "1.0pre" (empty sorts AFTER
-- "pre"), not "<" as a naive length comparison would give.
compareVersionParts [] (b : bs) =
  case compareComponent "" b of
    EQ -> compareVersionParts [] bs
    cmp -> ordToNix cmp
compareVersionParts (a : as) [] =
  case compareComponent a "" of
    EQ -> compareVersionParts as []
    cmp -> ordToNix cmp
compareVersionParts (a : as) (b : bs) =
  case compareComponent a b of
    EQ -> compareVersionParts as bs
    cmp -> ordToNix cmp

-- | Byte-level alpha classification, as upstream (C @isalpha@ over
-- bytes): a non-ASCII letter is a separator, never an alphabetic
-- component.  Digits need no counterpart - 'isDigit' is @0-9@ only.
isVersionAlpha :: Char -> Bool
isVersionAlpha c = isAsciiLower c || isAsciiUpper c

compareComponent :: BS.ByteString -> BS.ByteString -> Ordering
compareComponent a b
  | a == b = EQ
  | allDigits a && allDigits b = compare (readInt a) (readInt b)
  -- "pre" sorts before everything else (Nix pre-release convention)
  | a == "pre" = LT
  | b == "pre" = GT
  | a == "" = LT
  | b == "" = GT
  -- Alphabetic components sort before numeric in Nix
  | isAlphaComp a && allDigits b = LT
  | allDigits a && isAlphaComp b = GT
  | otherwise = compare a b
  where
    allDigits t = not (BS.null t) && BC.all isDigit t
    isAlphaComp t = case BC.uncons t of
      Just (c, _) -> isVersionAlpha c
      Nothing -> False
    readInt :: BS.ByteString -> Int64
    readInt = BC.foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt c)) (0 :: Int64)

splitVersionStr :: BS.ByteString -> [BS.ByteString]
splitVersionStr t
  | BS.null t = []
  | otherwise =
      let (component, rest) = spanComponent t
       in component : splitVersionAfterComponent rest

splitVersionAfterComponent :: BS.ByteString -> [BS.ByteString]
splitVersionAfterComponent t = case BC.uncons t of
  Nothing -> []
  -- '.' and '-' are both component separators in Nix's version grammar, so a
  -- '-' is skipped, not emitted as a one-character component.
  Just (sep, rest) | sep == '.' || sep == '-' -> splitVersionStr rest
  Just _ -> splitVersionStr t

spanComponent :: BS.ByteString -> (BS.ByteString, BS.ByteString)
spanComponent t = case BC.uncons t of
  Nothing -> ("", "")
  Just (c, rest)
    | isDigit c -> BC.span isDigit t
    | isVersionAlpha c -> BC.span isVersionAlpha t
    | otherwise -> (BS.take 1 t, rest)

builtinSplitVersion :: (MonadEval m) => NixValue -> m NixValue
builtinSplitVersion (VStr s _) =
  pure (VList (clistFromThunks (map (thunkToCPtr . evaluated . mkStrBytes) (splitVersionComponents s))))
builtinSplitVersion other =
  throwEvalError ("builtins.splitVersion: expected a string, got " <> typeName other)

splitVersionComponents :: BS.ByteString -> [BS.ByteString]
splitVersionComponents t = case BC.uncons t of
  Nothing -> []
  Just ('.', rest) -> splitVersionComponents rest
  Just (c, _)
    | isDigit c ->
        let (digits, rest) = BC.span isDigit t
         in digits : splitVersionComponents rest
    | isVersionAlpha c ->
        let (alpha, rest) = BC.span isVersionAlpha t
         in alpha : splitVersionComponents rest
    | otherwise ->
        -- Non-alphanumeric separator byte (e.g. '-', '_'): consume it
        splitVersionComponents (BS.drop 1 t)

builtinParseDrvName :: (MonadEval m) => NixValue -> m NixValue
builtinParseDrvName (VStr s _) =
  let (name, version) = parseName s
   in pure
        ( VAttrs
            ( attrSetFromMap $
                Map.fromList
                  [ ("name", evaluated (mkStrBytes name)),
                    ("version", evaluated (mkStrBytes version))
                  ]
            )
        )
builtinParseDrvName other =
  throwEvalError ("builtins.parseDrvName: expected a string, got " <> typeName other)

parseName :: BS.ByteString -> (BS.ByteString, BS.ByteString)
parseName t =
  case findVersionDash t 0 of
    Nothing -> (t, "")
    Just idx -> (BS.take idx t, BS.drop (idx + 1) t)

findVersionDash :: BS.ByteString -> Int -> Maybe Int
findVersionDash t idx = case BC.uncons (BS.drop idx t) of
  Nothing -> Nothing
  Just ('-', after)
    | Just (d, _) <- BC.uncons after,
      isDigit d ->
        Just idx
  Just _ -> findVersionDash t (idx + 1)

-- ---------------------------------------------------------------------------
-- Builtin implementations - serialization + hashing
-- ---------------------------------------------------------------------------

builtinToJSON :: (MonadEval m) => NixValue -> m NixValue
builtinToJSON val = do
  (json, ctx) <- valueToJSON val
  pure (VStr (TE.encodeUtf8 json) ctx)

-- | JSON is built as 'Text' and encoded once at the top: upstream's
-- serializer (nlohmann) REJECTS invalid UTF-8, so string payloads decode
-- strictly here and invalid bytes are an eval error, matching upstream.
valueToJSON :: (MonadEval m) => NixValue -> m (Text, StringContext)
valueToJSON VNull = pure ("null", emptyContext)
valueToJSON (VBool True) = pure ("true", emptyContext)
valueToJSON (VBool False) = pure ("false", emptyContext)
valueToJSON (VInt n) = pure (T.pack (show n), emptyContext)
valueToJSON (VFloat f)
  -- upstream's serializer (nlohmann dump) writes a non-finite float as null
  | isNaN f || isInfinite f = pure ("null", emptyContext)
  | otherwise = pure (formatJsonFloat f, emptyContext)
valueToJSON (VStr s ctx) = do
  decoded <- decodedText "builtins.toJSON" s
  pure (jsonEscapeString decoded, ctx)
valueToJSON (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  vals <- mapM force thunks
  results <- mapM valueToJSON vals
  let jsonVals = map fst results
      ctx = mconcat (map snd results)
  pure ("[" <> T.intercalate "," jsonVals <> "]", ctx)
valueToJSON (VAttrs attrs) =
  -- C++ Nix serializes an attrset via __toString first, then outPath, and only
  -- falls back to an object when neither is present.
  case (attrSetLookup "__toString" attrs, attrSetLookup "outPath" attrs) of
    (Nothing, Nothing) -> do
      let m = attrSetToMap attrs
          sortedKeys = Map.keys m
      results <- mapM (jsonPair m) sortedKeys
      let pairs = map fst results
          ctx = mconcat (map snd results)
      pure ("{" <> T.intercalate "," pairs <> "}", ctx)
    _ -> do
      (s, ctx) <- coerceToString True force applyValue (VAttrs attrs)
      decoded <- decodedText "builtins.toJSON" s
      pure (jsonEscapeString decoded, ctx)
  where
    jsonPair attrMap key = case Map.lookup key attrMap of
      Nothing -> pure ("", emptyContext)
      Just thunk -> do
        val <- force thunk
        (jsonVal, ctx) <- valueToJSON val
        pure (jsonEscapeString key <> ":" <> jsonVal, ctx)
-- A path serializes as its source store path, with context - the same
-- copy-to-store coercion as interpolation (upstream value-to-json.cc
-- serializes paths with copyToStore = true).
valueToJSON (VPath p) = do
  (spText, ctx) <- sourcePathWithContext p
  pure (jsonEscapeString spText, ctx)
valueToJSON (VLambda {}) = throwEvalError "builtins.toJSON: cannot convert a function to JSON"
valueToJSON (VBuiltin _ _) = throwEvalError "builtins.toJSON: cannot convert a function to JSON"
valueToJSON (VDerivation _) = throwEvalError "builtins.toJSON: cannot convert a derivation to JSON"
valueToJSON (VCompiledRegex _) = throwEvalError "builtins.toJSON: cannot convert a function to JSON"

jsonEscapeString :: Text -> Text
jsonEscapeString s = "\"" <> T.concatMap escapeChar s <> "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | ord c < 0x20 = "\\u" <> T.pack (padHex 4 (showHex' (ord c)))
      | otherwise = T.singleton c
    padHex n str = replicate (n - length str) '0' ++ str
    showHex' 0 = "0"
    showHex' num = go num ""
      where
        go 0 acc = acc
        go v acc =
          let (q, r) = quotRem v 16
           in go q (hexDigit r : acc)

-- | Safe hex digit lookup (total for 0-15).
hexDigit :: Int -> Char
hexDigit n
  | n >= 0 && n <= 9 = chr (ord '0' + n)
  | n >= 10 && n <= 15 = chr (ord 'a' + n - 10)
  | otherwise = '?' -- unreachable for valid hex

builtinFromJSON :: (MonadEval m) => NixValue -> m NixValue
builtinFromJSON (VStr s _) = do
  -- JSON input must be valid UTF-8 (upstream's parser rejects it too).
  decoded <- decodedText "builtins.fromJSON" s
  case parseJSON (T.strip decoded) of
    Just (val, rest)
      | T.null (T.strip rest) -> pure val
      | otherwise -> throwEvalError "builtins.fromJSON: trailing content after JSON value"
    Nothing -> throwEvalError "builtins.fromJSON: invalid JSON"
builtinFromJSON other =
  throwEvalError ("builtins.fromJSON: expected a string, got " <> typeName other)

parseJSON :: Text -> Maybe (NixValue, Text)
parseJSON t = case T.uncons (T.stripStart t) of
  Nothing -> Nothing
  Just ('n', rest)
    | Just suffix <- T.stripPrefix "ull" rest -> Just (VNull, suffix)
  Just ('t', rest)
    | Just suffix <- T.stripPrefix "rue" rest -> Just (VBool True, suffix)
  Just ('f', rest)
    | Just suffix <- T.stripPrefix "alse" rest -> Just (VBool False, suffix)
  Just ('"', _) -> parseJSONString (T.stripStart t)
  Just ('[', rest) -> parseJSONArray rest
  Just ('{', rest) -> parseJSONObject rest
  Just (c, _)
    | c == '-' || isDigit c -> parseJSONNumber (T.stripStart t)
  _ -> Nothing

parseJSONString :: Text -> Maybe (NixValue, Text)
parseJSONString t = case T.uncons t of
  Just ('"', rest) ->
    let (strVal, remaining) = parseJSONStringContent rest
     in Just (mkStr strVal, remaining)
  _ -> Nothing

-- | Parse JSON string content, O(n) via chunk list + T.concat.
parseJSONStringContent :: Text -> (Text, Text)
parseJSONStringContent = go []
  where
    go !chunks t = case T.uncons t of
      Nothing -> (T.concat (reverse chunks), "")
      Just ('"', rest) -> (T.concat (reverse chunks), rest)
      Just ('\\', rest) -> case T.uncons rest of
        Just ('"', r) -> go ("\"" : chunks) r
        Just ('\\', r) -> go ("\\" : chunks) r
        Just ('/', r) -> go ("/" : chunks) r
        Just ('n', r) -> go ("\n" : chunks) r
        Just ('r', r) -> go ("\r" : chunks) r
        Just ('t', r) -> go ("\t" : chunks) r
        Just ('u', r) -> case parseHex4 r of
          Just (hi, r2)
            -- UTF-16 surrogate pair: high surrogate followed by \uXXXX low
            | hi >= 0xD800 && hi <= 0xDBFF ->
                case T.stripPrefix "\\u" r2 of
                  Just r3 -> case parseHex4 r3 of
                    Just (lo, r4)
                      | lo >= 0xDC00 && lo <= 0xDFFF ->
                          let combined = 0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00)
                           in go (T.singleton (chr combined) : chunks) r4
                    _ -> go (T.singleton (chr hi) : chunks) r2
                  Nothing -> go (T.singleton (chr hi) : chunks) r2
          Just (codepoint, r2) ->
            go (T.singleton (chr codepoint) : chunks) r2
          Nothing -> go ("u" : chunks) r
        _ -> (T.concat (reverse chunks), rest)
      Just (c, rest) -> go (T.singleton c : chunks) rest

parseHex4 :: Text -> Maybe (Int, Text)
parseHex4 t
  | T.length t >= 4 =
      let hex = T.take 4 t
       in if T.all isHexDigit hex
            then Just (readHex4 hex, T.drop 4 t)
            else Nothing
  | otherwise = Nothing

readHex4 :: Text -> Int
readHex4 = T.foldl' (\acc c -> acc * 16 + digitToInt c) 0

parseJSONNumber :: Text -> Maybe (NixValue, Text)
parseJSONNumber t =
  let (numStr, rest) = T.span (\c -> isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E' || c == '+') t
   in if T.null numStr
        then Nothing
        else
          if T.any (\c -> c == '.' || c == 'e' || c == 'E') numStr
            then case reads (T.unpack numStr) :: [(Double, String)] of
              [(d, "")] -> Just (VFloat d, rest)
              _ -> Nothing
            else case reads (T.unpack numStr) :: [(Integer, String)] of
              [(n, "")] -> Just (jsonInteger n, rest)
              _ -> Nothing

-- | Nix value for a JSON integer literal, as upstream's nlohmann-based
-- parser produces it: a value in int64 range is an int; a positive value
-- that fits only uint64 still arrives as an int (nlohmann hands it over
-- unsigned, upstream stores it into the signed NixInt, two's-complement);
-- anything wider than 64 bits falls back to a float.
jsonInteger :: Integer -> NixValue
jsonInteger n
  | n >= toInteger (minBound :: Int64) && n <= toInteger (maxBound :: Word64) = VInt (fromInteger n)
  | otherwise = VFloat (fromInteger n)

parseJSONArray :: Text -> Maybe (NixValue, Text)
parseJSONArray t = parseJSONArrayElements (T.stripStart t) []

parseJSONArrayElements :: Text -> [Thunk] -> Maybe (NixValue, Text)
parseJSONArrayElements t acc = case T.uncons (T.stripStart t) of
  Just (']', rest) -> Just (VList (clistFromThunks (map thunkToCPtr (reverse acc))), rest)
  _ -> case parseJSON t of
    Just (val, rest) ->
      let stripped = T.stripStart rest
       in case T.uncons stripped of
            Just (',', rest2) -> parseJSONArrayElements rest2 (evaluated val : acc)
            Just (']', rest2) -> Just (VList (clistFromThunks (map thunkToCPtr (reverse (evaluated val : acc)))), rest2)
            _ -> Nothing
    Nothing -> Nothing

parseJSONObject :: Text -> Maybe (NixValue, Text)
parseJSONObject t = parseJSONObjectEntries (T.stripStart t) Map.empty

parseJSONObjectEntries :: Text -> Map Text Thunk -> Maybe (NixValue, Text)
parseJSONObjectEntries t acc = case T.uncons (T.stripStart t) of
  Just ('}', rest) -> Just (VAttrs (attrSetFromMap acc), rest)
  -- Keys read via 'parseJSONStringContent' directly: they become attr
  -- names (Text), not string values.
  Just ('"', afterQuote) ->
    let (key, rest) = parseJSONStringContent afterQuote
     in case T.uncons (T.stripStart rest) of
          Just (':', rest2) -> case parseJSON rest2 of
            Just (val, rest3) ->
              let stripped = T.stripStart rest3
                  updated = Map.insert key (evaluated val) acc
               in case T.uncons stripped of
                    Just (',', rest4) -> parseJSONObjectEntries rest4 updated
                    Just ('}', rest4) -> Just (VAttrs (attrSetFromMap updated), rest4)
                    _ -> Nothing
            Nothing -> Nothing
          _ -> Nothing
  _ -> Nothing

builtinHashString :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinHashString (VStr algo _) (VStr input _) = do
  -- The payload IS the hashed bytes - no encode step, so a mid-codepoint
  -- substring hashes to the sha256 of exactly those bytes, as upstream.
  algoName <- decodedText "builtins.hashString" algo
  hashBytesWithAlgo "hashString" algoName input
builtinHashString (VStr _ _) other =
  throwEvalError ("builtins.hashString: expected a string, got " <> typeName other)
builtinHashString other _ =
  throwEvalError ("builtins.hashString: expected a string, got " <> typeName other)

digestToHex :: (BA.ByteArrayAccess a) => a -> Text
digestToHex = bytesToHexText . BA.convert

-- ---------------------------------------------------------------------------
-- Builtin implementations - deep evaluation
-- ---------------------------------------------------------------------------

builtinDeepSeq :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinDeepSeq first second = do
  deepForce first
  pure second

deepForce :: (MonadEval m) => NixValue -> m ()
deepForce (VList cl) = mapM_ ((force >=> deepForce) . Thunk) (clistThunks cl)
deepForce (VAttrs attrs) = mapM_ (force >=> deepForce) (attrSetElems attrs)
deepForce _ = pure ()

-- | @builtins.seq a b@ - evaluate @a@ to WHNF, then return @b@.
builtinSeq :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinSeq !_first = pure

-- | @builtins.trace msg val@ - print @msg@ to stderr, return @val@.
builtinTrace :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinTrace msgVal result = do
  msg <- case msgVal of
    VStr s _ -> pure (bytesToTextLossy s)
    other -> pure (showValueForTrace other)
  traceMessage ("trace: " <> msg)
  pure result

-- | @builtins.warn msg val@ - print warning to stderr, return @val@.
builtinWarn :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinWarn msgVal result = do
  msg <- case msgVal of
    VStr s _ -> pure (bytesToTextLossy s)
    other -> pure (showValueForTrace other)
  traceMessage ("warning: " <> msg)
  pure result

-- | Pretty-print a value for trace/warn, matching C++ Nix's printValue.
showValueForTrace :: NixValue -> Text
showValueForTrace (VInt n) = T.pack (show n)
showValueForTrace (VFloat f) = formatNixFloat f
showValueForTrace (VBool True) = "true"
showValueForTrace (VBool False) = "false"
showValueForTrace VNull = "null"
showValueForTrace (VPath p) = p
showValueForTrace other = "<<" <> typeName other <> ">>"

-- ---------------------------------------------------------------------------
-- Builtin implementations - graph traversal
-- ---------------------------------------------------------------------------

builtinGenericClosure :: (MonadEval m) => NixValue -> m NixValue
builtinGenericClosure (VAttrs attrs) = do
  startSetThunk <-
    maybe (throwEvalError "builtins.genericClosure: missing 'startSet'") pure $
      attrSetLookup "startSet" attrs
  operatorThunk <-
    maybe (throwEvalError "builtins.genericClosure: missing 'operator'") pure $
      attrSetLookup "operator" attrs
  startSetVal <- force startSetThunk
  operatorVal <- force operatorThunk
  case startSetVal of
    VList cl -> do
      let items = map Thunk (clistThunks cl)
      result <- closureLoop operatorVal (Seq.fromList items) [] []
      pure (VList (clistFromThunks (map (thunkToCPtr . evaluated) result)))
    _ -> throwEvalError "builtins.genericClosure: 'startSet' must be a list"
builtinGenericClosure other =
  throwEvalError ("builtins.genericClosure: expected a set, got " <> typeName other)

-- | BFS loop for genericClosure.  Uses Data.Sequence for O(1) queue
-- append (the old list-based version was O(n) per operator call).
-- seenKeys is still a linear scan - Nix value equality is monadic so
-- Set/HashMap is not directly applicable without specialising on key type.
closureLoop ::
  (MonadEval m) =>
  NixValue ->
  Seq Thunk ->
  [NixValue] ->
  [NixValue] ->
  m [NixValue]
closureLoop _ Empty _ acc = pure (reverse acc)
closureLoop operator (thunk :<| rest) seenKeys acc = do
  item <- force thunk
  key <- extractKey item
  alreadySeen <- keyInList key seenKeys
  if alreadySeen
    then closureLoop operator rest seenKeys acc
    else do
      newItems <- applyValue operator item
      case newItems of
        VList newCl ->
          closureLoop operator (rest <> Seq.fromList (map Thunk (clistThunks newCl))) (key : seenKeys) (item : acc)
        _ -> throwEvalError "builtins.genericClosure: operator must return a list"

extractKey :: (MonadEval m) => NixValue -> m NixValue
extractKey (VAttrs attrs) =
  case attrSetLookup "key" attrs of
    Just thunk -> force thunk
    Nothing -> throwEvalError "builtins.genericClosure: item missing 'key' attribute"
extractKey _ = throwEvalError "builtins.genericClosure: item must be a set with 'key'"

keyInList :: (MonadEval m) => NixValue -> [NixValue] -> m Bool
keyInList _ [] = pure False
keyInList key (seen : rest) = do
  eq <- nixEqual force key seen
  if eq then pure True else keyInList key rest

-- ---------------------------------------------------------------------------
-- IO builtins (delegate to MonadEval methods)
-- ---------------------------------------------------------------------------

-- | Coerce a value to a path 'Text'.  Accepts 'VPath' and 'VStr';
-- throws a type error for anything else.  A string operand is a byte
-- string naming a filesystem path, so it decodes strictly.
coerceToPath :: (MonadEval m) => Text -> NixValue -> m Text
coerceToPath _ (VPath p) = pure p
coerceToPath name (VStr s _) = decodedText ("builtins." <> name) s
coerceToPath name other =
  throwEvalError ("builtins." <> name <> ": expected a path or string, got " <> typeName other)

builtinImport :: (MonadEval m) => NixValue -> m NixValue
builtinImport (VPath p) = importFile p
builtinImport (VStr s _) = importFile =<< decodedText "import" s
builtinImport other =
  throwEvalError ("import: expected a path or string, got " <> typeName other)

builtinReadFile :: (MonadEval m) => NixValue -> m NixValue
builtinReadFile val = do
  p <- coerceToPath "readFile" val
  bytes <- readFileBytes p
  -- The value is the file's RAW BYTES, as upstream: no BOM stripping, no
  -- UTF-16 transcoding, no replacement characters (the auto-decode stays
  -- on the parser/import path only).  The only rejection is NUL, which a
  -- Nix string cannot represent upstream.
  when (BS.elem 0 bytes) $
    throwEvalError
      ("builtins.readFile: the contents of the file '" <> p <> "' cannot be represented as a Nix string")
  pure (mkStrBytes bytes)

builtinPathExists :: (MonadEval m) => NixValue -> m NixValue
builtinPathExists val = do
  p <- coerceToPath "pathExists" val
  VBool <$> doesPathExist p

builtinReadDir :: (MonadEval m) => NixValue -> m NixValue
builtinReadDir val = do
  p <- coerceToPath "readDir" val
  entries <- listDirectory p
  pure (VAttrs (attrSetFromMap (Map.fromList [(name, evaluated (mkStr fileType)) | (name, fileType) <- entries])))

-- ---------------------------------------------------------------------------
-- Builtin implementations - environment + paths
-- ---------------------------------------------------------------------------

builtinGetEnv :: (MonadEval m) => NixValue -> m NixValue
builtinGetEnv (VStr name _) = do
  varName <- decodedText "builtins.getEnv" name
  mkStr <$> getEnvVar varName
builtinGetEnv other =
  throwEvalError ("builtins.getEnv: expected a string, got " <> typeName other)

builtinToPath :: (MonadEval m) => NixValue -> m NixValue
builtinToPath (VPath p) = pure (VPath p)
builtinToPath (VStr rawBytes _) = do
  s <- decodedText "builtins.toPath" rawBytes
  case T.uncons s of
    Nothing -> throwEvalError "builtins.toPath: empty path"
    -- Canonicalized like every other path production site, as upstream.
    Just ('/', _) -> pure (VPath (canonPath s))
    Just _ -> throwEvalError ("builtins.toPath: path must be absolute, got " <> s)
builtinToPath other =
  throwEvalError ("builtins.toPath: expected a string or path, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - store path operations
-- ---------------------------------------------------------------------------

builtinPlaceholder :: (MonadEval m) => NixValue -> m NixValue
builtinPlaceholder (VStr outputName _) = do
  name <- decodedText "builtins.placeholder" outputName
  pure (mkStr (hashPlaceholder name))
builtinPlaceholder other =
  throwEvalError ("builtins.placeholder: expected a string, got " <> typeName other)

builtinStorePath :: (MonadEval m) => NixValue -> m NixValue
builtinStorePath (VPath p) = validateStorePath p
builtinStorePath (VStr s _) = validateStorePath =<< decodedText "builtins.storePath" s
builtinStorePath other =
  throwEvalError ("builtins.storePath: expected a path or string, got " <> typeName other)

-- | @builtins.storePath@ - mark an already-in-store path as such.  Upstream
-- returns a STRING carrying an Opaque (SCPlain) context entry for the enclosing
-- store path, NOT a bare path: the Opaque marker is what stops a later coercion
-- from re-serialising (re-NARing) the already-present path into a new store
-- path.  Returning a context-free path (the old behavior) re-copied the path on
-- coercion - a wrong store path on Unix, and a file-not-found on Windows, where
-- the canonical @/nix/store@ text is not the on-disk location.
validateStorePath :: (MonadEval m) => Text -> m NixValue
validateStorePath p = case enclosingStorePath p of
  Just sp -> pure (VStr (TE.encodeUtf8 p) (plainContext sp))
  Nothing -> throwEvalError ("builtins.storePath: not a valid store path: " <> p)

-- | The store path enclosing an absolute path under the store dir, or
-- 'Nothing' if the path is not in the store.  Accepts a bare store path and a
-- subpath (@\/nix\/store\/\<hash\>-\<name\>\/sub@ resolves to its
-- @\<hash\>-\<name\>@ component), the way upstream marks the enclosing store
-- path Opaque for either.
enclosingStorePath :: Text -> Maybe StorePath
enclosingStorePath p = do
  rest <- T.stripPrefix storeDirPrefix p
  let component = T.takeWhile (/= '/') rest
      (hash, hyphenName) = T.splitAt 32 component
  (hyphen, name) <- T.uncons hyphenName
  if hyphen == '-' && not (T.null name) && T.length hash == 32
    then Just (StorePath hash name)
    else Nothing

-- ---------------------------------------------------------------------------
-- Builtin implementations - Nix search path
-- ---------------------------------------------------------------------------

builtinFindFile :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinFindFile (VList cl) (VStr name _) = do
  fileName <- decodedText "builtins.findFile" name
  let searchPath = map Thunk (clistThunks cl)
  entries <- mapM forceSearchEntry searchPath
  findFirst entries fileName
builtinFindFile (VList _) other =
  throwEvalError ("builtins.findFile: expected a string, got " <> typeName other)
builtinFindFile other _ =
  throwEvalError ("builtins.findFile: expected a list, got " <> typeName other)

-- | Extract {prefix, path} from a search path entry thunk.
forceSearchEntry :: (MonadEval m) => Thunk -> m (Text, Text)
forceSearchEntry thunk = do
  val <- force thunk
  case val of
    VAttrs attrs -> do
      prefixThunk <-
        maybe (throwEvalError "builtins.findFile: entry missing 'prefix'") pure $
          attrSetLookup "prefix" attrs
      pathThunk <-
        maybe (throwEvalError "builtins.findFile: entry missing 'path'") pure $
          attrSetLookup "path" attrs
      prefixVal <- force prefixThunk
      pathVal <- force pathThunk
      prefix <- case prefixVal of
        VStr s _ -> decodedText "builtins.findFile" s
        _ -> throwEvalError "builtins.findFile: 'prefix' must be a string"
      path <- case pathVal of
        VStr s _ -> decodedText "builtins.findFile" s
        VPath s -> pure s
        _ -> throwEvalError "builtins.findFile: 'path' must be a string or path"
      pure (prefix, path)
    _ -> throwEvalError "builtins.findFile: search path entry must be a set"

-- | Iterate search path entries, checking for a match.
-- A miss is a CATCHABLE error (upstream raises ThrownError here):
-- nixpkgs' impure.nix wraps @<nixpkgs-overlays>@ in @tryEval@ and
-- relies on catching the miss.
findFirst :: (MonadEval m) => [(Text, Text)] -> Text -> m NixValue
findFirst [] name =
  throwCatchableError ("file '" <> name <> "' was not found in the Nix search path")
findFirst ((prefix, path) : rest) name
  | prefix == name || (not (T.null prefix) && (prefix <> "/") `T.isPrefixOf` name) =
      let suffix = if prefix == name then "" else T.drop (T.length prefix + 1) name
          candidate = canonPath (if T.null suffix then path else path <> "/" <> suffix)
       in do
            exists <- doesPathExist candidate
            if exists
              then pure (VPath candidate)
              else findFirst rest name
  | T.null prefix =
      let candidate = canonPath (path <> "/" <> name)
       in do
            exists <- doesPathExist candidate
            if exists
              then pure (VPath candidate)
              else findFirst rest name
  | otherwise = findFirst rest name

-- ---------------------------------------------------------------------------
-- Builtin implementations - store file creation
-- ---------------------------------------------------------------------------

builtinToFile :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinToFile (VStr name _) (VStr contents ctx) = do
  fileName <- decodedText "builtins.toFile" name
  refs <- toFileRefs ctx
  -- The contents are stored (and hashed) as their raw bytes.
  storePath <- writeToStore fileName contents refs
  -- Upstream returns a STRING whose context is the new path itself; the
  -- refs travel through the store path computation, not the eval context.
  let selfContext = maybe emptyContext plainContext (parseStorePath defaultStoreDir storePath)
  pure (VStr (TE.encodeUtf8 storePath) selfContext)
builtinToFile (VStr _ _) other =
  throwEvalError ("builtins.toFile: expected a string, got " <> typeName other)
builtinToFile other _ =
  throwEvalError ("builtins.toFile: expected a string, got " <> typeName other)

-- | The store-path references a toFile output may carry: the contents'
-- plain-path context, in sorted order.  A derivation-output reference is
-- an error, exactly as upstream ("files created by builtins.toFile may
-- not reference derivations").
toFileRefs :: (MonadEval m) => StringContext -> m [StorePath]
toFileRefs (StringContext elems) = mapM refOf (Set.toAscList elems)
  where
    refOf (SCPlain sp) = pure sp
    refOf (SCDrvOutput _ _) =
      throwEvalError "builtins.toFile: files created by toFile may not reference derivations"
    refOf (SCAllOutputs _) =
      throwEvalError "builtins.toFile: files created by toFile may not reference derivations"

-- ---------------------------------------------------------------------------
-- Builtin implementations - scoped import
-- ---------------------------------------------------------------------------

builtinScopedImport :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinScopedImport (VAttrs attrs) pathVal = do
  p <- coerceToPath "scopedImport" pathVal
  let scope = Map.toList (attrSetToMap attrs)
  scopedImportFile scope p
builtinScopedImport other _ =
  throwEvalError ("builtins.scopedImport: expected a set, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - network fetchers
-- ---------------------------------------------------------------------------

builtinFetchurl :: (MonadEval m) => NixValue -> m NixValue
builtinFetchurl (VStr url _) = do
  urlText <- decodedText "builtins.fetchurl" url
  fetchUrlSimple urlText Nothing
builtinFetchurl (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchurl" "url" attrs
  sha256 <- forceOptionalAttrStr attrs "sha256"
  fetchUrlSimple url sha256
builtinFetchurl other =
  throwEvalError ("builtins.fetchurl: expected a string or set, got " <> typeName other)

builtinFetchTarball :: (MonadEval m) => NixValue -> m NixValue
builtinFetchTarball (VStr url _) = do
  urlText <- decodedText "builtins.fetchTarball" url
  fetchAndExtractTarball urlText Nothing tarballSourceName
builtinFetchTarball (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchTarball" "url" attrs
  sha256 <- forceOptionalAttrStr attrs "sha256"
  nameOverride <- forceOptionalAttrStr attrs "name"
  fetchAndExtractTarball url sha256 (fromMaybe tarballSourceName nameOverride)
builtinFetchTarball other =
  throwEvalError ("builtins.fetchTarball: expected a string or set, got " <> typeName other)

-- | Upstream's default store name for fetched source trees.
tarballSourceName :: Text
tarballSourceName = "source"

-- | Download a tarball, extract it, verify the optional sha256 pin, and
-- copy the tree to its content-addressed store path.  The pin is the
-- recursive NAR hash of the EXTRACTED tree, as upstream.  Download and
-- extraction share one shell pipeline to avoid binary-as-text encoding
-- issues.  The scratch dir is exclusively created with an unpredictable
-- name ('createScratchDir') and removed once the tree reaches the store.
fetchAndExtractTarball :: (MonadEval m) => Text -> Maybe Text -> Text -> m NixValue
fetchAndExtractTarball url mSha256 name = do
  extractDir <- createScratchDir "nova-nix-tarball-"
  -- The -- separator prevents argument injection from the URL.
  (code, _, errOut) <-
    runProcess
      "sh"
      [ "-c",
        "curl -sSfL -- \"$1\" | tar -xz -C \"$2\" --strip-components=1",
        "--",
        url,
        extractDir
      ]
      ""
  case code of
    0 -> do
      pin <- traverse (decodeSha256Pin "builtins.fetchTarball") mSha256
      storePath <-
        copyPathToStore
          extractDir
          name
          (fmap ("builtins.fetchTarball: " <> url,) pin)
      removeScratchDir extractDir
      pure (VPath storePath)
    _ -> do
      removeScratchDir extractDir
      throwEvalError ("builtins.fetchTarball: " <> errOut)

-- | Transports 'builtinFetchGit' accepts.  Also spelled into the
-- clone's @protocol.*.allow@ config, so git enforces the same set
-- internally.
allowedGitSchemes :: [Text]
allowedGitSchemes = ["http", "https", "ssh", "git", "file"]

-- | Validate a fetchGit URL's transport before anything spawns.  git's
-- transport-helper syntax (@\<helper\>::\<address\>@) makes the helper
-- name a command to run as the transport (@ext::@ runs a shell command,
-- @fd::@ reads a descriptor), so evaluating an expression carrying such
-- a URL would execute it.  Accepted: an allowlisted explicit scheme, an
-- scp-like remote, or a local path.  Left carries the eval-error text.
checkGitUrl :: Text -> Either Text Text
checkGitUrl url
  | T.null url = Left "builtins.fetchGit: empty url"
  | not (T.null schemeRest),
    schemeShaped scheme,
    T.toLower scheme `elem` allowedGitSchemes =
      Right url
  | not (T.null schemeRest) = Left (disallowed scheme)
  | Just helper <- helperPrefix = Left (disallowed helper)
  | otherwise = Right url
  where
    (scheme, schemeRest) = T.breakOn "://" url
    -- RFC 3986 scheme shape; anything else before :// is not a scheme.
    schemeShaped s = case T.uncons s of
      Just (leading, rest) -> asciiAlpha leading && T.all schemeChar rest
      Nothing -> False
    schemeChar c = asciiAlpha c || isDigit c || c == '+' || c == '-' || c == '.'
    asciiAlpha c = isAsciiLower c || isAsciiUpper c
    -- <helper>:: counts only when the prefix is shaped like a helper
    -- name; "[::1]"-style scp hosts contain :: but are not helpers.
    helperPrefix = case T.breakOn "::" url of
      (prefix, rest)
        | not (T.null rest),
          T.null prefix || T.all helperChar prefix ->
            Just prefix
      _ -> Nothing
    helperChar c = asciiAlpha c || isDigit c || c == '-' || c == '_'
    disallowed t =
      "builtins.fetchGit: transport '" <> t <> "' is not allowed in url: " <> url

-- | @-c@ config pinning the clone to 'allowedGitSchemes': every
-- transport defaults to never, each allowed scheme is re-enabled.  This
-- enforces what URL validation cannot see up front - a helper reached
-- indirectly, or a @remote-\<helper\>@ binary on PATH.
gitTransportConfig :: [Text]
gitTransportConfig =
  ["-c", "protocol.allow=never"]
    ++ concat [["-c", "protocol." <> scheme <> ".allow=always"] | scheme <- allowedGitSchemes]

builtinFetchGit :: (MonadEval m) => NixValue -> m NixValue
builtinFetchGit (VStr rawUrl _) = do
  url <- decodedText "builtins.fetchGit" rawUrl
  case checkGitUrl url of
    Left err -> throwEvalError err
    Right allowedUrl -> do
      cloneDir <- createScratchDir "nova-nix-fetchgit-"
      (code, _, errOut) <-
        runProcess "git" (gitTransportConfig ++ ["clone", "--depth", "1", "--", allowedUrl, cloneDir]) ""
      case code of
        0 -> pure (VPath cloneDir)
        _ -> do
          removeScratchDir cloneDir
          throwEvalError ("builtins.fetchGit: git clone failed: " <> errOut)
builtinFetchGit (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchGit" "url" attrs
  builtinFetchGit (mkStr url)
builtinFetchGit other =
  throwEvalError ("builtins.fetchGit: expected a string or set, got " <> typeName other)

-- | Resolve the system temp directory.  Checks @TMPDIR@ (Unix), then
-- @TEMP@ (Windows), falls back to @\/tmp@.
getTempDir :: (MonadEval m) => m Text
getTempDir = do
  candidates <- mapM getEnvVar ["TMPDIR", "TEMP"]
  pure (fromMaybe "/tmp" (find (not . T.null) candidates))

-- | Fetch a URL and optionally verify its hash.
fetchUrlSimple :: (MonadEval m) => Text -> Maybe Text -> m NixValue
fetchUrlSimple url mSha256 = do
  -- The store name derives from the URL alone, so an unusable name fails
  -- here, before the download side effect.  Upstream rejects the same
  -- names when it adds the fetched file to the store.
  let name = urlBaseName url
  case checkStorePathName name of
    Left err -> throwEvalError ("builtins.fetchurl: " <> storePathNameErrorText err)
    Right () -> pure ()
  -- Download to a file, not through the text-mode stdout pipe (which mangles
  -- binary), read the raw bytes, verify the pin if one was given, then store at
  -- the canonical fixed-output path.
  tmpDir <- getTempDir
  let tmpFile = tmpDir <> "/nova-nix-fetchurl-" <> sha256Hex (TE.encodeUtf8 url)
  (code, _, stderr) <- runProcess "curl" ["-sSfL", "-o", tmpFile, "--", url] ""
  if code /= 0
    then throwEvalError ("builtins.fetchurl: fetch failed: " <> stderr)
    else do
      bytes <- readFileBytes tmpFile
      mapM_ (verifyFetchPin "builtins.fetchurl" url bytes) mSha256
      storePath <- addFixedOutputFile name bytes
      pure (VPath storePath)

-- | Store name for a fetched URL: its basename (minus query/fragment), matching
-- C++ Nix's @baseNameOf url@ default so the fixed-output path agrees with it.
urlBaseName :: Text -> Text
urlBaseName url =
  let stripped = T.takeWhile (\c -> c /= '?' && c /= '#') url
      base = T.takeWhileEnd (/= '/') stripped
   in if T.null base then "source" else base

-- | Verify fetched bytes against a user-supplied sha256 pin, erroring on
-- mismatch.  Accepts SRI, @sha256:@-prefixed, and bare digest forms.
verifyFetchPin :: (MonadEval m) => Text -> Text -> BS.ByteString -> Text -> m ()
verifyFetchPin ctx url bytes expectedStr = do
  expected <- decodeSha256Pin ctx expectedStr
  let got = sha256Digest bytes
  when (expected /= got) $
    throwEvalError
      ( ctx
          <> ": hash mismatch for "
          <> url
          <> "\n  expected: "
          <> expectedStr
          <> "\n  got:      sha256:"
          <> bytesToHexText got
      )

-- | Decode a sha256 pin to raw bytes, reusing the convertHash decoders (SRI,
-- @sha256:@-prefixed, or a bare hex/nix32/base64 digest).
decodeSha256Pin :: (MonadEval m) => Text -> Text -> m BS.ByteString
decodeSha256Pin ctx s
  | Just (algo, b64) <- parseSRI s = do
      requireSha256 algo
      decodeSRI ctx algo b64
  | Just (algo, rest) <- parseAlgoPrefix s = do
      requireSha256 algo
      snd <$> decodeWithAlgo algo rest
  | otherwise = snd <$> decodeWithAlgo "sha256" s
  where
    -- The pin names a sha256 digest specifically, so a spelling carrying
    -- its own algorithm tag must carry sha256 - a sha512 pin here is an
    -- error at decode time, as upstream's typed Hash parse.
    requireSha256 "sha256" = pure ()
    requireSha256 algo =
      throwEvalError (ctx <> ": hash '" <> s <> "' should have type 'sha256', not '" <> algo <> "'")

-- | Force a required string attribute from an attrset, using full Nix string
-- coercion (VStr, VPath, VInt, VBool, VAttrs via __toString/outPath).
-- A coercion failure (or a throwing attr value) propagates with its own
-- message, as upstream - swallowing it here once masked user throws.
-- The Text result decodes strictly: callers use it as a name, URL, or
-- platform string, none of which may carry invalid UTF-8 here.
forceAttrStr :: (MonadEval m) => Text -> Text -> AttrSet -> m Text
forceAttrStr builtin key attrs = do
  bytes <- forceAttrBytes builtin key attrs
  decodedText builtin bytes

-- | Like 'forceAttrStr' but returns the coerced RAW BYTES - for
-- derivation fields (builder) that flow byte-exact into the ATerm.
forceAttrBytes :: (MonadEval m) => Text -> Text -> AttrSet -> m BS.ByteString
forceAttrBytes builtin key attrs =
  case attrSetLookup key attrs of
    Nothing -> throwEvalError (builtin <> ": missing required attribute '" <> key <> "'")
    Just thunk -> do
      val <- force thunk
      (s, _ctx) <- coerceToString True force applyValue val
      pure s

-- | Force an optional string attribute via full Nix coercion.  A present
-- but uncoercible (or throwing) value is an error, not 'Nothing'.
forceOptionalAttrStr :: (MonadEval m) => AttrSet -> Text -> m (Maybe Text)
forceOptionalAttrStr attrs key =
  case attrSetLookup key attrs of
    Nothing -> pure Nothing
    Just thunk -> do
      val <- force thunk
      (s, _ctx) <- coerceToString True force applyValue val
      Just <$> decodedText "string attribute" s

-- ---------------------------------------------------------------------------
-- Builtin implementations - derivation construction
-- ---------------------------------------------------------------------------

-- | Resolve an input derivation's modulo hash (hex) for the ATerm substitution
-- 'toATermForHash' performs.  The drv-hash cache is populated bottom-up as each
-- derivation is evaluated, so an in-session input hits directly.  A miss - a
-- cross-session reference, or a path fabricated by @builtins.appendContext@ -
-- reads the input @.drv@ from the store and recurses, exactly as upstream
-- @hashDerivationModulo@ does.  A miss whose @.drv@ cannot be read (pure
-- evaluation, or a @.drv@ absent from the store) fails loudly: a divergent
-- derivation hash is never emitted from a guessed input hash.  Reading the
-- store is an effect, so hashing a dependent derivation is an IO-evaluator
-- capability - pure evaluation, which cannot read the store, cannot do it.
resolveInputModulo :: (MonadEval m) => (StorePath, [Text]) -> m (Text, [Text])
resolveInputModulo (sp, outs) = do
  let inputPathText = storePathToText defaultStoreDir sp
  cached <- lookupDrvHash inputPathText
  case cached of
    Just hex -> pure (hex, outs)
    Nothing -> do
      mDrv <- readStoreDerivation sp
      case mDrv of
        Just inputDrv -> do
          hex <- derivationModuloHex inputDrv
          cacheDrvHash inputPathText hex
          pure (hex, outs)
        Nothing ->
          throwEvalError
            ( "derivation: cannot compute the input-derivation modulo hash for "
                <> inputPathText
                <> " - it was not evaluated in this session and its .drv is not "
                <> "readable from the store (dependent-derivation hashing needs the IO evaluator)"
            )

-- | The derivation-modulo hash (hex) of a derivation, matching upstream
-- @hashDerivationModulo@: a fixed-output derivation hashes the
-- @fixed:out:\<algo\>:\<hash\>:\<path\>@ form; an input-addressed derivation
-- substitutes each input's modulo hash into its ATerm and hashes that.  These
-- are the same two rules 'builtinDerivationStrict' applies when it first
-- computes a derivation's hash, so a store-read input yields the value it had
-- in-session.
derivationModuloHex :: (MonadEval m) => Derivation -> m Text
derivationModuloHex drv = case drvOutputs drv of
  [DerivationOutput "out" outPath algoField hashHex]
    | not (T.null algoField) && not (T.null hashHex) ->
        let outPathText = storePathToText defaultStoreDir outPath
            fixedForm = "fixed:out:" <> algoField <> ":" <> hashHex <> ":" <> outPathText
         in pure (bytesToHexText (sha256Digest (TE.encodeUtf8 fixedForm)))
  _ -> do
    inputSubst <- mapM resolveInputModulo (Map.toList (drvInputDrvs drv))
    pure (bytesToHexText (sha256Digest (toATermForHash False (Just inputSubst) drv)))

-- | The full output-name list of an all-outputs (upstream DrvDeep) reference:
-- every output name of the referenced @.drv@, which upstream's derivationStrict
-- inserts into the consuming derivation's inputDrvs.  Mirrors the
-- 'resolveInputModulo' ladder - an in-session derivation from the recorded
-- ATerm ('lookupSessionDrv'), else a cross-session @.drv@ read from the store
-- ('readStoreDerivation'), else a loud error rather than a dropped reference
-- (which would under-hash the dependent).  Pure evaluation can read neither
-- source, so a dependent carrying an all-outputs reference errors there,
-- consistent with dependent-derivation hashing being an IO-evaluator capability.
resolveAllOutputNames :: (MonadEval m) => StorePath -> m [Text]
resolveAllOutputNames sp = do
  let drvPathText = storePathToText defaultStoreDir sp
  session <- lookupSessionDrv drvPathText
  case session of
    Just drv -> pure (outputNamesOf drv)
    Nothing -> do
      onDisk <- readStoreDerivation sp
      case onDisk of
        Just drv -> pure (outputNamesOf drv)
        Nothing ->
          throwEvalError
            ( "derivation: cannot resolve the output names of the all-outputs reference "
                <> drvPathText
                <> " - it was not evaluated in this session and its .drv is not "
                <> "readable from the store (all-outputs references need the IO evaluator)"
            )
  where
    outputNamesOf = map doName . drvOutputs

-- | Eager derivation computation - @builtins.derivationStrict@.  Forces all
-- input attrs into env vars, content-hashes, and returns the full derivation
-- attrset (drvPath, outPath, per-output, _derivation).  Called LAZILY by the
-- @derivation@ wrapper ('builtinDerivationLazy'), so forcing a derivation to
-- WHNF never forces this - matching C++ Nix's derivationStrict/derivation split.
builtinDerivationStrict :: (MonadEval m) => NixValue -> m NixValue
builtinDerivationStrict (VAttrs attrs) = do
  -- Extract required attributes.  The name and system are Text (they feed
  -- store-path and platform machinery); the builder stays raw bytes - it
  -- lands byte-exact in the ATerm's builder field.
  drvName <- forceAttrStr "derivation" "name" attrs
  -- The name becomes the store-path name of the .drv and of every output,
  -- so it must satisfy the store-path name rules.  The path constructors
  -- re-check what they build; this early check reports the offending
  -- FIELD rather than a composed path name.
  case checkStorePathName drvName of
    Left err ->
      throwEvalError
        ("derivation: invalid derivation name '" <> drvName <> "': " <> storePathNameReasonText (spneReason err))
    Right () -> pure ()
  system <- forceAttrStr ("derivation \"" <> drvName <> "\"") "system" attrs
  builder <- forceAttrBytes ("derivation \"" <> drvName <> "\"") "builder" attrs

  -- __ignoreNulls: when true, null-valued attrs are dropped from the
  -- derivation env (stdenv.mkDerivation sets it); when absent or false,
  -- null coerces to "" like any other coerceMore value - C++ Nix semantics.
  ignoreNulls <- case attrSetLookup "__ignoreNulls" attrs of
    Nothing -> pure False
    Just thunk -> do
      val <- force thunk
      case val of
        VBool b -> pure b
        other -> throwEvalError ("derivation: '__ignoreNulls' must be a boolean, got " <> typeName other)

  -- Extract optional outputs (default ["out"])
  outputNames <- case attrSetLookup "outputs" attrs of
    Nothing -> pure ["out"]
    Just thunk -> do
      val <- force thunk
      case val of
        VList cl -> mapM (forceToText . Thunk) (clistThunks cl)
        -- A null outputs attr is dropped by __ignoreNulls, falling back to
        -- the default output set; without it, null is an error as upstream.
        VNull | ignoreNulls -> pure ["out"]
        _ -> throwEvalError "derivation: 'outputs' must be a list of strings"

  -- Each output name composes into a store-path name (drvName-<output>)
  -- and names the on-disk location the builder later clears and moves
  -- onto, so it must satisfy the same store-path name rules (the
  -- composed length is re-checked at construction).
  forM_ outputNames $ \outName ->
    case checkStorePathName outName of
      Left err ->
        throwEvalError
          ( "derivation \""
              <> drvName
              <> "\": invalid derivation output name '"
              <> outName
              <> "': "
              <> storePathNameReasonText (spneReason err)
          )
      Right () -> pure ()

  -- Extract optional args (default []).  Path literals in args (e.g. stdenv's
  -- ./default-builder.sh) are copied into the store; their source paths flow
  -- into inputSrcs via the returned context.
  (builderArgs, argsContext) <- case attrSetLookup "args" attrs of
    Nothing -> pure ([], mempty)
    Just thunk -> do
      val <- force thunk
      case val of
        VList cl -> do
          parts <- mapM (\t -> force (Thunk t) >>= coerceToStoreString) (clistThunks cl)
          pure (map fst parts, mconcat (map snd parts))
        VNull | ignoreNulls -> pure ([], mempty)
        _ -> throwEvalError "derivation: 'args' must be a list of strings"

  -- Materialize once, reuse for both env collection and result merge
  let materialized = attrSetToMap attrs

  -- Collect string-coercible attrs into the build env, EXCLUDING "args"
  -- (C++ Nix puts args in the Derive() args field, never the env).  The
  -- per-output env vars ($out, ...) are added below.  Carries merged context.
  (drvEnvPairs, envContext) <- collectDrvEnvWithContext ignoreNulls (Map.delete "__ignoreNulls" (Map.delete "args" materialized))

  let fullContext = envContext <> argsContext
      builtInputDrvs = extractInputDrvs fullContext
      inputSrcs = extractInputSrcs fullContext
      allOutputRefs = extractAllOutputRefs fullContext
  -- All-outputs (DrvDeep) references - e.g. an embedded @dep.drvPath@ - add the
  -- referenced .drv to inputDrvs with ALL its output names, as upstream's
  -- derivationStrict does.  Merged in BEFORE drvRefs and the modulo
  -- substitution so both the dependent's own .drv hash and its output paths
  -- account for the reference.
  deepInputDrvs <-
    Map.fromList <$> mapM (\drvSp -> (,) drvSp <$> resolveAllOutputNames drvSp) allOutputRefs
  let inputDrvs = Map.unionWith (++) builtInputDrvs deepInputDrvs
      platform = textToPlatform system
      baseEnv = Map.fromList drvEnvPairs
      drvRefs = inputSrcs ++ Map.keys inputDrvs
      drvFileName = drvName <> ".drv"
      -- Build a Derivation sharing this call's inputs/platform/builder/args.
      mkDrv outs env =
        Derivation
          { drvOutputs = outs,
            drvInputDrvs = inputDrvs,
            drvInputSrcs = inputSrcs,
            drvPlatform = platform,
            drvBuilder = builder,
            drvArgs = builderArgs,
            drvEnv = env
          }
      -- Output carrying only its name; the path is masked at render time.
      maskedOutput name = DerivationOutput name (StorePath "" "") "" ""

  -- Fixed-output derivations (fetchurl etc.) are content-addressed and hash
  -- via the @fixed:out:@ scheme; input-addressed derivations recurse through
  -- the modulo hashes of their inputs.
  mFixed <- detectFixedOutput attrs

  -- Path construction returns Left on a name the store rules reject; the
  -- early field checks above make that unreachable here except through
  -- composition (drvName <> ".drv", drvName-output past the length cap),
  -- which only the constructors see.
  let drvContext = "derivation \"" <> drvName <> "\""
  (drvPathText, drvSP, outPaths, completeDrv) <- case mFixed of
    Just (foAlgo, foMode, foDigest) -> do
      foPath <- storePathOrThrow drvContext (makeFixedOutputPath drvName foAlgo foMode foDigest)
      let foPathText = storePathToText defaultStoreDir foPath
          algoField = (if foMode == "recursive" then "r:" else "") <> foAlgo
          foHashHex = bytesToHexText foDigest
          foModulo =
            sha256Digest
              (TE.encodeUtf8 ("fixed:out:" <> algoField <> ":" <> foHashHex <> ":" <> foPathText))
          contents =
            mkDrv
              [DerivationOutput "out" foPath algoField foHashHex]
              (Map.insert "out" (TE.encodeUtf8 foPathText) baseEnv)
      drvSp <- storePathOrThrow drvContext (makeTextPath drvFileName (sha256Digest (toATerm contents)) drvRefs)
      let drvText = storePathToText defaultStoreDir drvSp
      cacheDrvHash drvText (bytesToHexText foModulo)
      pure (drvText, drvSp, [("out", foPathText)], contents)
    Nothing -> do
      inputSubst <- mapM resolveInputModulo (Map.toList inputDrvs)
      let maskedEnv = foldr (`Map.insert` "") baseEnv outputNames
          maskedDrv = mkDrv (map maskedOutput outputNames) maskedEnv
          -- Masked modulo hash yields this derivation's own output paths.
          moduloMasked = sha256Digest (toATermForHash True (Just inputSubst) maskedDrv)
      outStorePaths <-
        mapM
          (\n -> (,) n <$> storePathOrThrow drvContext (makeOutputPath n moduloMasked drvName))
          outputNames
      let outPathTexts = [(n, storePathToText defaultStoreDir sp) | (n, sp) <- outStorePaths]
          realEnv = foldr (\(n, t) e -> Map.insert n (TE.encodeUtf8 t) e) baseEnv outPathTexts
          contents = mkDrv [DerivationOutput n sp "" "" | (n, sp) <- outStorePaths] realEnv
          -- Unmasked modulo hash (real outputs, inputs substituted) cached for
          -- when this derivation is itself an input to another.
          moduloUnmasked = sha256Digest (toATermForHash False (Just inputSubst) contents)
      drvSp <- storePathOrThrow drvContext (makeTextPath drvFileName (sha256Digest (toATerm contents)) drvRefs)
      let drvText = storePathToText defaultStoreDir drvSp
      cacheDrvHash drvText (bytesToHexText moduloUnmasked)
      pure (drvText, drvSp, outPathTexts, contents)

  -- Record this derivation's full .drv ATerm (the exact bytes whose hash is its
  -- store path) so the build driver can materialize the entire input-.drv
  -- closure before building.  Bottom-up eval guarantees every transitive input
  -- is recorded by the time a dependent is.
  recordDrvAterm drvPathText (toATerm completeDrv)

  let mainOutPath = case outPaths of
        ((_, p) : _) -> p
        [] -> ""
      -- The default output is the FIRST in @outputs@ (matching C++ Nix, which
      -- returns @(head outputsList).value@) - not necessarily @out@.
      mainOutName = case outPaths of
        ((n, _) : _) -> n
        [] -> "out"

  -- Context for output paths: each output carries SCDrvOutput context
  -- Context for drvPath: carries SCAllOutputs context
  let drvPathCtx = StringContext (Set.singleton (SCAllOutputs drvSP))
      outPathCtx outName = StringContext (Set.singleton (SCDrvOutput drvSP outName))

  -- Build per-output attrsets matching Nix: drv.out = { outPath, drvPath, type }
  let mkOutputAttrs outName outP =
        let outCtx = outPathCtx outName
            outputAttrMap =
              Map.fromList
                [ ("outPath", evaluated (VStr (TE.encodeUtf8 outP) outCtx)),
                  ("drvPath", evaluated (VStr (TE.encodeUtf8 drvPathText) drvPathCtx)),
                  ("type", evaluated (mkStr "derivation"))
                ]
         in evaluated (VAttrs (attrSetFromMap outputAttrMap))

  -- Build result attrset: original attrs + drvPath, outPath, type, per-output attrs
  let baseAttrs =
        Map.fromList $
          [ ("type", evaluated (mkStr "derivation")),
            ("drvPath", evaluated (VStr (TE.encodeUtf8 drvPathText) drvPathCtx)),
            ("outPath", evaluated (VStr (TE.encodeUtf8 mainOutPath) (outPathCtx mainOutName))),
            ("name", evaluated (mkStr drvName)),
            ("system", evaluated (mkStr system)),
            ("builder", evaluated (mkStrBytes builder)),
            ("_derivation", evaluated (VDerivation completeDrv))
          ]
            ++ [(outName, mkOutputAttrs outName outP) | (outName, outP) <- outPaths]
      -- Merge original attrs underneath so computed attrs take priority
      resultAttrs = Map.union baseAttrs materialized

  pure (VAttrs (attrSetFromMap resultAttrs))
builtinDerivationStrict other =
  throwEvalError ("derivation: expected a set, got " <> typeName other)

-- | Detect a fixed-output derivation.  Returns @Just (algo, mode, rawDigest)@
-- when @outputHash@ is present and non-empty (fetchurl, fetchgit, ...), else
-- 'Nothing' for an ordinary input-addressed derivation.  @mode@ is
-- @\"flat\"@ or @\"recursive\"@; @algo@ is e.g. @\"sha256\"@.
detectFixedOutput :: (MonadEval m) => AttrSet -> m (Maybe (Text, Text, BS.ByteString))
detectFixedOutput attrs =
  case attrSetLookup "outputHash" attrs of
    Nothing -> pure Nothing
    Just thunk -> do
      val <- force thunk
      case val of
        VStr rawHash _
          | not (BS.null rawHash) -> do
              ohash <- decodedText "derivation: outputHash" rawHash
              ohAlgo <- optDrvStrAttr "outputHashAlgo" attrs
              ohMode <- optDrvStrAttr "outputHashMode" attrs
              (algo, digest) <- normalizeFixedHash ohash ohAlgo
              let mode = if ohMode == "recursive" then "recursive" else "flat"
              pure (Just (algo, mode, digest))
        _ -> pure Nothing

-- | Read an optional string attribute, defaulting to @\"\"@ when absent or
-- not a string.
optDrvStrAttr :: (MonadEval m) => Text -> AttrSet -> m Text
optDrvStrAttr key attrs =
  case attrSetLookup key attrs of
    Nothing -> pure ""
    Just thunk -> do
      val <- force thunk
      case val of
        VStr s _ -> decodedText ("derivation: " <> key) s
        _ -> pure ""

-- | Decode a fixed-output hash (SRI @algo-base64@, @algo:hash@, or a bare
-- hash plus a separate algorithm) to its algorithm name and raw bytes.
normalizeFixedHash :: (MonadEval m) => Text -> Text -> m (Text, BS.ByteString)
normalizeFixedHash ohash ohAlgo
  | Just (algo, b64) <- parseSRI ohash = do
      bytes <- decodeSRI "derivation" algo b64
      pure (algo, bytes)
  | Just (algo, rest) <- parseAlgoPrefix ohash = decodeWithAlgo algo rest
  | not (T.null ohAlgo) = decodeWithAlgo ohAlgo ohash
  | otherwise =
      throwEvalError ("derivation: cannot determine outputHash algorithm for " <> ohash)

-- | Lazy @derivation@ wrapper - mirrors C++ Nix's @corepkgs/derivation.nix@.
-- Returns a WHNF attrset whose @drvPath@/@outPath@/output-path/@_derivation@
-- attrs are LAZY thunks that defer to 'builtinDerivationStrict'.  Forcing a
-- derivation to WHNF therefore does NOT force its input/env closure - which is
-- essential for nixpkgs, where merely referencing a derivation (e.g.
-- @drv != null@, @assert (libxcrypt != null)@) must not build its whole closure.
--
-- The lazy thunks are built with the same synthetic-select pattern used by
-- @inherit (from)@: a single shared @strict@ thunk (so the eager computation
-- runs at most once) selected from via fresh minimal envs.
builtinDerivationLazy :: (MonadEval m) => NixValue -> m NixValue
builtinDerivationLazy (VAttrs attrs) = do
  -- Output names are cheap (matches @drvAttrs @ { outputs ? [ "out" ], ... }@).
  outputNames <- case attrSetLookup "outputs" attrs of
    Nothing -> pure ["out"]
    Just thunk -> do
      val <- force thunk
      case val of
        VList cl -> mapM (forceToText . Thunk) (clistThunks cl)
        _ -> throwEvalError "derivation: 'outputs' must be a list of strings"
  -- One shared thunk computing @derivationStrict attrs@, forced only when an
  -- output path / drvPath is actually read.
  let drvAttrsThunk = evaluated (VAttrs attrs)
      strictBuiltinThunk = evaluated (VBuiltin "derivationStrict" [])
      strictThunk =
        let (sp, sc) = buildCSlots [drvAttrsThunk, strictBuiltinThunk]
            envDS = newMinimalEnv sp sc
         in mkSyntheticThunk envDS (EApp (EResolvedVar 0 1) (EResolvedVar 0 0))
      selectStrict field =
        let (sp, sc) = buildCSlots [strictThunk]
            envF = newMinimalEnv sp sc
         in mkSyntheticThunk envF (ESelect (EResolvedVar 0 0) [StaticKey field] Nothing)
  -- WHNF spine: input attrs (unforced) overlaid with the lazy computed attrs.
  let computedAttrs =
        Map.fromList $
          [ ("type", evaluated (mkStr "derivation")),
            ("drvPath", selectStrict "drvPath"),
            ("outPath", selectStrict "outPath"),
            ("_derivation", selectStrict "_derivation")
          ]
            ++ [(outName, selectStrict outName) | outName <- outputNames]
      resultAttrs = Map.union computedAttrs (attrSetToMap attrs)
  pure (VAttrs (attrSetFromMap resultAttrs))
builtinDerivationLazy other =
  throwEvalError ("derivation: expected a set, got " <> typeName other)

-- | Force a thunk to a Text string via full Nix coercion (strict decode:
-- used for output names, which are ASCII-shaped identity components).
forceToText :: (MonadEval m) => Thunk -> m Text
forceToText thunk = do
  val <- force thunk
  (s, _ctx) <- coerceToString True force applyValue val
  decodedText "derivation" s

-- | Collect all derivation attributes into env pairs via full Nix coercion
-- (__toString, outPath, list-to-space-separated-string), along with the
-- merged string context from all collected values.  Values are RAW BYTES:
-- they flow byte-exact into the ATerm env section (and its hash).
--
-- A coercion failure (a thrown attr value, an uncoercible function, a failed
-- import inside the value) fails the WHOLE derivation, as in C++ Nix.
-- Swallowing it silently produces a wrong .drv - this exact bug once turned
-- a seed derivation with 17 failed src attrs into an empty no-input drv that
-- "built" successfully.  Null attrs are dropped only under __ignoreNulls;
-- otherwise null coerces to @""@ like any other coerceMore value.
collectDrvEnvWithContext :: (MonadEval m) => Bool -> Map Text Thunk -> m ([(Text, BS.ByteString)], StringContext)
collectDrvEnvWithContext ignoreNulls attrs = do
  let pairs = Map.toList attrs
  results <- mapM coerceEnvAttr pairs
  let envPairs = catMaybes [fmap (\(k, v, _) -> (k, v)) r | r <- results]
      mergedCtx = mconcat [ctx | Just (_, _, ctx) <- results]
  pure (envPairs, mergedCtx)
  where
    coerceEnvAttr (key, thunk) = do
      val <- force thunk
      case val of
        VNull | ignoreNulls -> pure Nothing
        _ -> do
          (s, ctx) <- coerceToStoreString val
          pure (Just (key, s, ctx))

-- ---------------------------------------------------------------------------
-- Builtin implementations - hashFile, readFileType
-- ---------------------------------------------------------------------------

-- | @builtins.hashFile algo path@ - hash raw bytes of a file on disk.
-- Returns base-16 hex string, matching @builtins.hashString@ output format.
builtinHashFile :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinHashFile (VStr algo _) (VPath path) = do
  algoName <- decodedText "builtins.hashFile" algo
  bytes <- readFileBytes path
  hashBytesWithAlgo "hashFile" algoName bytes
builtinHashFile (VStr algo _) (VStr path _) = do
  algoName <- decodedText "builtins.hashFile" algo
  filePath <- decodedText "builtins.hashFile" path
  bytes <- readFileBytes filePath
  hashBytesWithAlgo "hashFile" algoName bytes
builtinHashFile (VStr _ _) other =
  throwEvalError ("builtins.hashFile: expected a path, got " <> typeName other)
builtinHashFile other _ =
  throwEvalError ("builtins.hashFile: expected a string, got " <> typeName other)

-- | Shared hash dispatch for raw 'ByteString' input.
hashBytesWithAlgo :: (MonadEval m) => Text -> Text -> BS.ByteString -> m NixValue
hashBytesWithAlgo ctx algo bytes = case algo of
  "sha256" -> pure (mkStr (digestToHex (CH.hash bytes :: CH.Digest CH.SHA256)))
  "sha512" -> pure (mkStr (digestToHex (CH.hash bytes :: CH.Digest CH.SHA512)))
  "sha1" -> pure (mkStr (digestToHex (CH.hash bytes :: CH.Digest CH.SHA1)))
  "md5" -> pure (mkStr (digestToHex (CH.hash bytes :: CH.Digest CH.MD5)))
  _ -> throwEvalError ("builtins." <> ctx <> ": unknown hash algorithm '" <> algo <> "'")

-- | @builtins.readFileType path@ - classify a filesystem entry.
-- Returns @"regular"@, @"directory"@, @"symlink"@, or @"unknown"@.
builtinReadFileType :: (MonadEval m) => NixValue -> m NixValue
builtinReadFileType (VPath path) = mkStr <$> getFileType path
builtinReadFileType (VStr path _) = do
  filePath <- decodedText "builtins.readFileType" path
  mkStr <$> getFileType filePath
builtinReadFileType other =
  throwEvalError ("builtins.readFileType: expected a path, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - convertHash
-- ---------------------------------------------------------------------------

-- | @builtins.convertHash { hash, hashAlgo?, toHashFormat }@ - convert
-- between hash representations.  Supports base16, nix32, base64, and sri.
builtinConvertHash :: (MonadEval m) => NixValue -> m NixValue
builtinConvertHash (VAttrs attrs) = do
  hashVal <- requireStrAttr "convertHash" "hash" attrs
  toFmt <- requireStrAttr "convertHash" "toHashFormat" attrs
  -- Detect input format and decode to raw bytes + algo
  (algo, rawBytes) <- decodeHashInput attrs hashVal
  -- Encode to target format
  case toFmt of
    "base16" -> pure (mkStr (bytesToHexText rawBytes))
    "nix32" -> pure (mkStr (Nix32.encode rawBytes))
    "base32" -> pure (mkStr (Nix32.encode rawBytes)) -- deprecated alias
    "base64" -> pure (mkStr (bytesToBase64 rawBytes))
    "sri" -> pure (mkStr (algo <> "-" <> bytesToBase64 rawBytes))
    _ -> throwEvalError ("builtins.convertHash: unknown toHashFormat '" <> toFmt <> "'")
builtinConvertHash other =
  throwEvalError ("builtins.convertHash: expected a set, got " <> typeName other)

-- | Extract algo + raw bytes from the hash input, handling SRI, prefixed, and plain formats.
decodeHashInput :: (MonadEval m) => AttrSet -> Text -> m (Text, BS.ByteString)
decodeHashInput attrs hashStr
  -- SRI format: algo-base64
  | Just (algo, b64) <- parseSRI hashStr = do
      bytes <- decodeSRI "convertHash" algo b64
      pure (algo, bytes)
  -- Prefixed format: algo:hex or algo:nix32
  | Just (algo, rest) <- parseAlgoPrefix hashStr =
      decodeWithAlgo algo rest
  -- Plain hash - need hashAlgo attribute
  | otherwise = do
      algo <- requireStrAttr "convertHash" "hashAlgo" attrs
      decodeWithAlgo algo hashStr

-- | Decode a bare hash string for a known algorithm, keyed by length as
-- upstream (@hash.cc@ @Hash@ parsing): an algorithm's base16, nix32, and
-- base64 spellings have pairwise distinct lengths, so the input length
-- selects the decoder and every other length is an error.  Trying decoders
-- in sequence instead mis-reads edge inputs - an all-hex-digit string of
-- nix32 length is a nix32 hash, and a truncated hash must be rejected, not
-- decoded by whichever shorter format happens to accept it.
decodeWithAlgo :: (MonadEval m) => Text -> Text -> m (Text, BS.ByteString)
decodeWithAlgo algo s = case hashAlgoBytes algo of
  Nothing -> throwEvalError ("unknown hash algorithm '" <> algo <> "'")
  Just size
    | T.length s == hexHashLen size -> accept size "base16" (hexToBytes s)
    | T.length s == nix32HashLen size -> accept size "nix32" (rightToMaybe (Nix32.decode s))
    | T.length s == base64HashLen size -> accept size "base64" (rightToMaybe (decodeBase64Pure s))
    | otherwise ->
        throwEvalError ("hash '" <> s <> "' has wrong length for hash algorithm '" <> algo <> "'")
  where
    accept size spelling decoded = case decoded of
      Just bytes | BS.length bytes == size -> pure (algo, bytes)
      _ -> throwEvalError ("hash '" <> s <> "' is not a valid " <> spelling <> " " <> algo <> " hash")
    rightToMaybe = either (const Nothing) Just

-- | Decode an SRI digest (@algo-base64@) with the decoded-length check
-- every spelling gets upstream (hash.cc checks SRI too): well-formed
-- base64 of the wrong byte count is an invalid hash at eval time, not a
-- short digest that defers failure to fetch or build time.  Shared by
-- convertHash, the fetch pins, and fixed-output outputHash.
decodeSRI :: (MonadEval m) => Text -> Text -> Text -> m BS.ByteString
decodeSRI ctx algo b64 = case hashAlgoBytes algo of
  -- Unreachable via parseSRI (it admits only known algorithm tags), but
  -- this helper must not silently skip the check for an unknown one.
  Nothing -> throwEvalError ("unknown hash algorithm '" <> algo <> "'")
  Just size -> do
    bytes <- decodeBase64E ctx b64
    when (BS.length bytes /= size) $
      throwEvalError
        ("hash '" <> algo <> "-" <> b64 <> "' has wrong length for hash algorithm '" <> algo <> "'")
    pure bytes

-- | Parse @sha256-base64...@ SRI format.
parseSRI :: Text -> Maybe (Text, Text)
parseSRI t = case T.breakOn "-" t of
  (algo, rest)
    | not (T.null rest) && algo `elem` ["sha256", "sha512", "sha1", "md5"] ->
        Just (algo, T.drop 1 rest)
  _ -> Nothing

-- | Parse @sha256:value@ prefixed format.
parseAlgoPrefix :: Text -> Maybe (Text, Text)
parseAlgoPrefix t = case T.breakOn ":" t of
  (algo, rest)
    | not (T.null rest) && algo `elem` ["sha256", "sha512", "sha1", "md5"] ->
        Just (algo, T.drop 1 rest)
  _ -> Nothing

-- | Require a string attribute from an attrset.
requireStrAttr :: (MonadEval m) => Text -> Text -> AttrSet -> m Text
requireStrAttr ctx key attrs = case attrSetLookup key attrs of
  Just thunk -> do
    val <- force thunk
    case val of
      VStr s _ -> decodedText ("builtins." <> ctx) s
      _ -> throwEvalError ("builtins." <> ctx <> ": " <> key <> " must be a string")
  Nothing -> throwEvalError ("builtins." <> ctx <> ": missing required attribute '" <> key <> "'")

-- ---------------------------------------------------------------------------
-- Base64 encode/decode - delegates to nova-cache (base64-bytestring under the hood)
-- ---------------------------------------------------------------------------

-- | Encode bytes to base64.
bytesToBase64 :: BS.ByteString -> Text
bytesToBase64 = B64.encode

-- | Decode base64 text to bytes (pure).
decodeBase64Pure :: Text -> Either Text BS.ByteString
decodeBase64Pure t =
  -- Strip whitespace and any existing padding, then re-pad to a multiple of
  -- 4.  base64-bytestring's 'decode' requires correct padding, so SRI hashes
  -- (correctly-padded standard base64, e.g. @sha256-...NQ=@) would otherwise be
  -- rejected once their trailing @=@ was removed.
  let stripped = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '=') t
      padLen = (4 - (T.length stripped `mod` 4)) `mod` 4
      padded = stripped <> T.replicate padLen "="
   in case B64.decode padded of
        Right bytes -> Right bytes
        Left _ -> Left "invalid base64"

-- | Decode base64 with error context for builtins.
decodeBase64E :: (MonadEval m) => Text -> Text -> m BS.ByteString
decodeBase64E ctx t = case decodeBase64Pure t of
  Right bytes -> pure bytes
  Left _ -> throwEvalError ("builtins." <> ctx <> ": invalid base64 encoding")

-- ---------------------------------------------------------------------------
-- Builtin implementations - fromTOML
-- ---------------------------------------------------------------------------

-- | @builtins.fromTOML str@ - parse a TOML document to a Nix value.
-- Hand-rolled parser covering the TOML v1.0 subset used by nixpkgs:
-- bare/quoted keys, dotted keys, basic/literal strings (multiline),
-- integers (dec/hex/oct/bin), floats (inc. inf/nan), booleans,
-- inline tables, arrays, array-of-tables, and standard tables.
-- Datetimes are represented as strings (matching real Nix).
builtinFromTOML :: (MonadEval m) => NixValue -> m NixValue
builtinFromTOML (VStr s _) = do
  -- TOML is UTF-8 by definition; upstream's parser rejects invalid bytes.
  decoded <- decodedText "builtins.fromTOML" s
  case parseTOML decoded of
    Right val -> pure val
    Left err -> throwEvalError ("builtins.fromTOML: " <> err)
builtinFromTOML other =
  throwEvalError ("builtins.fromTOML: expected a string, got " <> typeName other)

-- | Intermediate TOML value before conversion to NixValue.
data TOMLValue
  = TOMLStr !Text
  | TOMLInt !Int64
  | TOMLFloat !Double
  | TOMLBool !Bool
  | TOMLArray ![TOMLValue]
  | TOMLTable !(Map Text TOMLValue)
  deriving (Show)

-- | Parse a TOML document into a NixValue.
parseTOML :: Text -> Either Text NixValue
parseTOML input = do
  table <- parseTOMLDoc (T.lines input)
  pure (tomlToNix (TOMLTable table))

-- | Convert a TOMLValue to NixValue.
tomlToNix :: TOMLValue -> NixValue
tomlToNix val = case val of
  TOMLStr s -> mkStr s
  TOMLInt n -> VInt n
  TOMLFloat d -> VFloat d
  TOMLBool b -> VBool b
  TOMLArray xs -> VList (clistFromThunks (map (thunkToCPtr . evaluated . tomlToNix) xs))
  TOMLTable m -> VAttrs (attrSetFromMap (Map.map (evaluated . tomlToNix) m))

-- | Parse all lines of a TOML document into a table.
parseTOMLDoc :: [Text] -> Either Text (Map Text TOMLValue)
parseTOMLDoc lns = go lns [] Map.empty
  where
    go [] _ root = Right root
    go (line : rest) currentPath root
      | T.null stripped || T.isPrefixOf "#" stripped =
          -- Empty line or comment
          go rest currentPath root
      | T.isPrefixOf "[[" stripped && T.isSuffixOf "]]" stripped =
          -- Array of tables: [[key]]
          let keyStr = T.strip (T.drop 2 (T.dropEnd 2 stripped))
              keys = parseDottedKey keyStr
           in go rest keys (insertArrayTable keys root)
      | T.isPrefixOf "[" stripped && T.isSuffixOf "]" stripped =
          -- Standard table: [key]
          let keyStr = T.strip (T.drop 1 (T.dropEnd 1 stripped))
              keys = parseDottedKey keyStr
           in go rest keys (ensureTable keys root)
      | otherwise =
          -- Key = value pair.  The value may span physical lines (arrays
          -- and multi-line strings - the Cargo.lock shapes), so join
          -- until brackets and multi-line string delimiters balance.
          let (logical, remaining) = joinLogicalLine stripped rest
           in case parseKVLine logical of
                Right (keys, val) ->
                  go remaining currentPath (insertNested (currentPath ++ keys) val root)
                Left err -> Left err
      where
        stripped = T.strip line

-- | Which TOML string form a scan position is inside.
data TomlStringState = TSNone | TSBasic | TSLiteral | TSMultiBasic | TSMultiLiteral
  deriving (Eq)

-- | Scanner state for joining physical lines into one logical
-- key = value line: array-bracket depth outside strings, plus the
-- string form currently open.
data TomlScan = TomlScan
  { tsBracketDepth :: !Int,
    tsString :: !TomlStringState
  }

emptyTomlScan :: TomlScan
emptyTomlScan = TomlScan 0 TSNone

-- | Whether a logical value is still open at end of line: inside a
-- multi-line string, or under an unclosed array bracket.  A single-line
-- string left open is NOT continuable (TOML basic\/literal strings
-- cannot span lines) - the value parser reports that case.
tomlNeedsMoreLines :: TomlScan -> Bool
tomlNeedsMoreLines st =
  tsBracketDepth st > 0
    || tsString st == TSMultiBasic
    || tsString st == TSMultiLiteral

-- | Scan one physical line, returning the state at end of line and the
-- line's visible text: a comment outside strings is cut here, where the
-- string context is still known ('#' inside any string form is content).
scanTomlLine :: TomlScan -> Text -> (TomlScan, Text)
scanTomlLine st0 line = go st0 line 0
  where
    go st t !seen = case T.uncons t of
      Nothing -> (st, line)
      Just (c, rest) ->
        let one newSt = go newSt rest (seen + 1)
            jump n newSt = go newSt (T.drop (n - 1) rest) (seen + n)
         in case tsString st of
              TSBasic
                | c == '\\' -> jump 2 st
                | c == '"' -> one st {tsString = TSNone}
                | otherwise -> one st
              TSLiteral
                | c == '\'' -> one st {tsString = TSNone}
                | otherwise -> one st
              TSMultiBasic
                | c == '\\' -> jump 2 st
                | c == '"', Just _ <- T.stripPrefix "\"\"" rest -> jump 3 st {tsString = TSNone}
                | otherwise -> one st
              TSMultiLiteral
                | c == '\'', Just _ <- T.stripPrefix "''" rest -> jump 3 st {tsString = TSNone}
                | otherwise -> one st
              TSNone
                | c == '#' -> (st, T.take seen line)
                | c == '"', Just _ <- T.stripPrefix "\"\"" rest -> jump 3 st {tsString = TSMultiBasic}
                | c == '"' -> one st {tsString = TSBasic}
                | c == '\'', Just _ <- T.stripPrefix "''" rest -> jump 3 st {tsString = TSMultiLiteral}
                | c == '\'' -> one st {tsString = TSLiteral}
                | c == '[' -> one st {tsBracketDepth = tsBracketDepth st + 1}
                | c == ']' -> one st {tsBracketDepth = max 0 (tsBracketDepth st - 1)}
                | otherwise -> one st

-- | Join physical lines into one logical key = value line, returning it
-- with the unconsumed lines.  An unterminated construct at end of input
-- hands what accumulated to the value parser, which reports the specific
-- failure.
joinLogicalLine :: Text -> [Text] -> (Text, [Text])
joinLogicalLine firstLine rest0 =
  let (st0, visible0) = scanTomlLine emptyTomlScan firstLine
   in go st0 [visible0] rest0
  where
    -- Reversed line chunks, joined once - O(n) in the logical line's
    -- length instead of the O(n^2) of appending per physical line.
    go st !chunks remaining
      | not (tomlNeedsMoreLines st) = (T.intercalate "\n" (reverse chunks), remaining)
      | otherwise = case remaining of
          [] -> (T.intercalate "\n" (reverse chunks), [])
          (next : more) ->
            let (advanced, visible) = scanTomlLine st next
             in go advanced (visible : chunks) more

-- | Parse a key = value line.
parseKVLine :: Text -> Either Text ([Text], TOMLValue)
parseKVLine line =
  let (keyPart, afterEq) = splitAtEquals line
   in case T.uncons afterEq of
        Nothing -> Left ("expected '=' in: " <> line)
        Just _ -> do
          let val = T.strip afterEq
          parsed <- parseTOMLValue val
          Right (parseDottedKey (T.strip keyPart), parsed)

-- | Split a line at the first unquoted '=' sign.
-- O(n) via bulk spans into a chunk list instead of O(n^2) T.snoc.
splitAtEquals :: Text -> (Text, Text)
splitAtEquals = go []
  where
    keyPart chunks = T.concat (reverse chunks)
    go !chunks t = case T.uncons t of
      Nothing -> (keyPart chunks, T.empty)
      Just ('=', rest) -> (keyPart chunks, rest)
      Just ('"', rest) ->
        let (quoted, after) = T.break (== '"') rest
         in case T.uncons after of
              Just ('"', r) -> go ("\"" : quoted : "\"" : chunks) r
              _ -> go (quoted : "\"" : chunks) after
      Just ('\'', rest) ->
        let (quoted, after) = T.break (== '\'') rest
         in case T.uncons after of
              Just ('\'', r) -> go ("'" : quoted : "'" : chunks) r
              _ -> go (quoted : "'" : chunks) after
      Just (_, _) ->
        let (plain, after) = T.break (\c -> c == '=' || c == '"' || c == '\'') t
         in go (plain : chunks) after

-- | Parse dotted key like @foo.bar."baz qux"@ into @["foo", "bar", "baz qux"]@.
parseDottedKey :: Text -> [Text]
parseDottedKey t
  | T.null t = []
  | otherwise = case T.uncons t of
      Just ('"', rest) ->
        let (key, after) = T.break (== '"') rest
         in key : parseDottedKey (T.drop 1 (T.stripStart (dropDot after)))
      Just ('\'', rest) ->
        let (key, after) = T.break (== '\'') rest
         in key : parseDottedKey (T.drop 1 (T.stripStart (dropDot after)))
      _ ->
        let (key, after) = T.break (\c -> c == '.' || c == '"') t
         in T.strip key : case T.uncons after of
              Nothing -> []
              Just _ -> parseDottedKey (T.drop 1 (T.stripStart after))
  where
    dropDot txt = case T.uncons txt of
      Just ('.', rest) -> rest
      _ -> txt

-- | Parse a TOML value (right side of '=').
parseTOMLValue :: Text -> Either Text TOMLValue
parseTOMLValue t =
  let stripped = T.strip t
      -- Strip inline comments (not inside strings)
      cleaned = stripInlineComment stripped
   in case T.uncons cleaned of
        Nothing -> Left "empty value"
        Just ('"', _)
          | T.isPrefixOf "\"\"\"" cleaned -> parseMultilineBasicStr (T.drop 3 cleaned)
          | otherwise -> parseBasicStr (T.drop 1 cleaned)
        Just ('\'', _)
          | T.isPrefixOf "'''" cleaned -> parseMultilineLiteralStr (T.drop 3 cleaned)
          | otherwise -> parseLiteralStr (T.drop 1 cleaned)
        Just ('{', _) -> parseInlineTable cleaned
        Just ('[', _) -> parseInlineArray cleaned
        Just ('t', _)
          | T.isPrefixOf "true" cleaned -> Right (TOMLBool True)
        Just ('f', _)
          | T.isPrefixOf "false" cleaned -> Right (TOMLBool False)
        Just ('i', _)
          | T.isPrefixOf "inf" cleaned -> Right (TOMLFloat (1 / 0))
        Just ('+', rest)
          | T.isPrefixOf "inf" rest -> Right (TOMLFloat (1 / 0))
          | T.isPrefixOf "nan" rest -> Right (TOMLFloat (0 / 0))
        Just ('-', rest)
          | T.isPrefixOf "inf" rest -> Right (TOMLFloat (negate (1 / 0)))
          | T.isPrefixOf "nan" rest -> Right (TOMLFloat (0 / 0))
        Just ('n', _)
          | T.isPrefixOf "nan" cleaned -> Right (TOMLFloat (0 / 0))
        _ -> parseTOMLNumberOrDatetime cleaned

-- | Strip inline comment from a value (not inside quotes).
-- O(n) via bulk spans into a chunk list instead of O(n^2)
-- per-character T.cons (each cons copies the whole tail).
stripInlineComment :: Text -> Text
stripInlineComment = go (0 :: Int) []
  where
    finish chunks = T.concat (reverse chunks)
    go depth !chunks t = case T.uncons t of
      Nothing -> finish chunks
      Just ('#', _) | depth == 0 -> finish chunks
      Just ('"', rest)
        | depth == 0 ->
            let (str, after) = T.break (== '"') rest
             in go depth (T.take 1 after : str : "\"" : chunks) (T.drop 1 after)
      Just ('[', rest) -> go (depth + 1) ("[" : chunks) rest
      Just ('{', rest) -> go (depth + 1) ("{" : chunks) rest
      Just (']', rest) -> go (max 0 (depth - 1)) ("]" : chunks) rest
      Just ('}', rest) -> go (max 0 (depth - 1)) ("}" : chunks) rest
      Just (c, rest) ->
        -- c is plain (or a quote/hash inside brackets, kept as content);
        -- take it plus the whole plain run after it in one span.
        let (plain, after) = T.break scanBreak rest
         in go depth (plain : T.singleton c : chunks) after
    scanBreak c = c == '#' || c == '"' || c == '[' || c == '{' || c == ']' || c == '}'

-- | Parse a basic (double-quoted) TOML string.
-- O(n) via chunk list + T.concat instead of O(n^2) T.snoc.
parseBasicStr :: Text -> Either Text TOMLValue
parseBasicStr = go []
  where
    go !chunks t = case T.uncons t of
      Nothing -> Left "unterminated basic string"
      Just ('"', _) -> Right (TOMLStr (T.concat (reverse chunks)))
      Just ('\\', rest) -> case T.uncons rest of
        Just ('n', r) -> go ("\n" : chunks) r
        Just ('t', r) -> go ("\t" : chunks) r
        Just ('r', r) -> go ("\r" : chunks) r
        Just ('\\', r) -> go ("\\" : chunks) r
        Just ('"', r) -> go ("\"" : chunks) r
        Just ('b', r) -> go ("\b" : chunks) r
        Just ('f', r) -> go ("\f" : chunks) r
        Just ('u', r) -> case parseHex4 r of
          Just (cp, r2) -> go (T.singleton (chr cp) : chunks) r2
          Nothing -> Left "invalid \\u escape"
        _ -> Left "invalid escape sequence"
      Just (c, rest) -> go (T.singleton c : chunks) rest

-- | Parse a literal (single-quoted) TOML string.
parseLiteralStr :: Text -> Either Text TOMLValue
parseLiteralStr t =
  let (content, rest) = T.break (== '\'') t
   in case T.uncons rest of
        Just ('\'', _) -> Right (TOMLStr content)
        _ -> Left "unterminated literal string"

-- | Parse a multiline basic string.
parseMultilineBasicStr :: Text -> Either Text TOMLValue
parseMultilineBasicStr t =
  case T.breakOn "\"\"\"" t of
    (content, rest)
      | T.isPrefixOf "\"\"\"" rest ->
          Right (TOMLStr (T.replace "\\\n" "" (stripLeadingNewline content)))
      | otherwise -> Left ("unterminated multiline basic string, remaining: " <> T.take 20 rest)

-- | Parse a multiline literal string.
parseMultilineLiteralStr :: Text -> Either Text TOMLValue
parseMultilineLiteralStr t =
  case T.breakOn "'''" t of
    (content, rest)
      | T.isPrefixOf "'''" rest -> Right (TOMLStr (stripLeadingNewline content))
      | otherwise -> Left "unterminated multiline literal string"

-- | Strip a leading newline (TOML spec: first newline after opening quotes is trimmed).
stripLeadingNewline :: Text -> Text
stripLeadingNewline t = case T.uncons t of
  Just ('\n', rest) -> rest
  Just ('\r', rest) -> case T.uncons rest of
    Just ('\n', r) -> r
    _ -> rest
  _ -> t

-- | Parse a TOML number or datetime.
parseTOMLNumberOrDatetime :: Text -> Either Text TOMLValue
parseTOMLNumberOrDatetime s
  -- Hex, octal, binary integers
  | T.isPrefixOf "0x" s || T.isPrefixOf "0X" s = parseHexInt (T.drop 2 s)
  | T.isPrefixOf "0o" s || T.isPrefixOf "0O" s = parseOctInt (T.drop 2 s)
  | T.isPrefixOf "0b" s || T.isPrefixOf "0B" s = parseBinInt (T.drop 2 s)
  -- Contains date separators, so treat as a datetime string
  | T.any (== 'T') s && T.any (== '-') s = Right (TOMLStr s)
  | T.count "-" s >= 2 && T.any isDigit s = Right (TOMLStr s)
  | T.any (== ':') s && T.any isDigit s = Right (TOMLStr s)
  -- Float (has dot or exponent)
  | T.any (== '.') s || T.any (\c -> c == 'e' || c == 'E') s = parseFloat s
  -- Plain integer
  | otherwise = parseInt s

-- | Parse a plain decimal integer, ignoring underscores.
parseInt :: Text -> Either Text TOMLValue
parseInt t =
  let cleaned = T.filter (/= '_') t
      (sign, digits) = case T.uncons cleaned of
        Just ('+', rest) -> (1 :: Integer, rest)
        Just ('-', rest) -> (-1, rest)
        _ -> (1, cleaned)
   in case readDecimal digits of
        Just n -> tomlInt t (sign * n)
        Nothing -> Left ("invalid integer: " <> t)

parseHexInt :: Text -> Either Text TOMLValue
parseHexInt t =
  let cleaned = T.filter (/= '_') t
   in case readHexT cleaned of
        Just n -> tomlInt t n
        Nothing -> Left ("invalid hex integer: " <> t)

parseOctInt :: Text -> Either Text TOMLValue
parseOctInt t =
  let cleaned = T.filter (/= '_') t
   in case readOctT cleaned of
        Just n -> tomlInt t n
        Nothing -> Left ("invalid octal integer: " <> t)

parseBinInt :: Text -> Either Text TOMLValue
parseBinInt t =
  let cleaned = T.filter (/= '_') t
   in case readBinT cleaned of
        Just n -> tomlInt t n
        Nothing -> Left ("invalid binary integer: " <> t)

-- | Finish a parsed TOML integer: a value outside the 64-bit signed range
-- is a parse error, as upstream's TOML parser reports - silently wrapping
-- would hand the evaluator a different number than the document wrote.
tomlInt :: Text -> Integer -> Either Text TOMLValue
tomlInt original n
  | n < toInteger (minBound :: Int64) || n > toInteger (maxBound :: Int64) =
      Left ("integer out of 64-bit range: " <> original)
  | otherwise = Right (TOMLInt (fromInteger n))

parseFloat :: Text -> Either Text TOMLValue
parseFloat t =
  let cleaned = T.filter (/= '_') t
   in case readDouble cleaned of
        Just d -> Right (TOMLFloat d)
        Nothing -> Left ("invalid float: " <> t)

-- Digit readers accumulate an unbounded 'Integer'; 'tomlInt' applies the
-- 64-bit range gate after any sign, so the minimum int64 (whose magnitude
-- alone exceeds the maximum) still parses.

-- | Read an unbounded decimal integer from Text.
readDecimal :: Text -> Maybe Integer
readDecimal t
  | T.null t = Nothing
  | T.all isDigit t = Just (T.foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt c)) 0 t)
  | otherwise = Nothing

readHexT :: Text -> Maybe Integer
readHexT t
  | T.null t = Nothing
  | T.all isHexDigit t = Just (T.foldl' (\acc c -> acc * 16 + fromIntegral (digitToInt c)) 0 t)
  | otherwise = Nothing

readOctT :: Text -> Maybe Integer
readOctT t
  | T.null t = Nothing
  | T.all isOctDigit t =
      Just (T.foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 t)
  | otherwise = Nothing

readBinT :: Text -> Maybe Integer
readBinT t
  | T.null t = Nothing
  | T.all (\c -> c == '0' || c == '1') t =
      Just (T.foldl' (\acc c -> acc * 2 + fromIntegral (digitToInt c)) 0 t)
  | otherwise = Nothing

readDouble :: Text -> Maybe Double
readDouble t = case reads (T.unpack t) of
  [(d, "")] -> Just d
  _ -> Nothing

-- | Parse an inline table: @{ key = val, ... }@.
parseInlineTable :: Text -> Either Text TOMLValue
parseInlineTable t = case T.uncons t of
  Just ('{', rest) ->
    let inner = T.strip (T.dropWhileEnd (== '}') (T.strip rest))
     in if T.null inner
          then Right (TOMLTable Map.empty)
          else do
            pairs <- mapM parseInlineKV (splitCommas inner)
            Right (TOMLTable (Map.fromList (concatMap flattenPair pairs)))
  _ -> Left "expected '{'"
  where
    flattenPair (keys, val) = case keys of
      [] -> []
      [k] -> [(k, val)]
      (k : ks) -> [(k, nestKeys ks val)]
    nestKeys [] v = v
    nestKeys (k : ks) v = TOMLTable (Map.singleton k (nestKeys ks v))

-- | Parse an inline array: @[ val, ... ]@.
parseInlineArray :: Text -> Either Text TOMLValue
parseInlineArray t = case T.uncons t of
  Just ('[', rest) ->
    let inner = T.strip (T.dropWhileEnd (== ']') (T.strip rest))
     in if T.null inner
          then Right (TOMLArray [])
          else do
            vals <- mapM (parseTOMLValue . T.strip) (splitCommas inner)
            Right (TOMLArray vals)
  _ -> Left "expected '['"

-- | Parse a single key=value pair in an inline table.
parseInlineKV :: Text -> Either Text ([Text], TOMLValue)
parseInlineKV t =
  let (keyPart, afterEq) = splitAtEquals (T.strip t)
   in do
        val <- parseTOMLValue (T.strip afterEq)
        Right (parseDottedKey (T.strip keyPart), val)

-- | Split on commas not inside brackets or braces.
-- O(n) via chunk list + T.concat instead of O(n^2) T.snoc.
splitCommas :: Text -> [Text]
splitCommas = go (0 :: Int) []
  where
    finalize chunks =
      let t = T.concat (reverse chunks)
       in [t | not (T.null (T.strip t))]
    go _ !chunks t | T.null t = finalize chunks
    go depth !chunks t = case T.uncons t of
      Nothing -> finalize chunks
      Just (',', rest) | depth == 0 -> T.concat (reverse chunks) : go 0 [] rest
      Just ('[', rest) -> go (depth + 1) ("[" : chunks) rest
      Just ('{', rest) -> go (depth + 1) ("{" : chunks) rest
      Just (']', rest) -> go (max 0 (depth - 1)) ("]" : chunks) rest
      Just ('}', rest) -> go (max 0 (depth - 1)) ("}" : chunks) rest
      Just ('"', rest) ->
        let (str, after) = T.break (== '"') rest
            consumed = "\"" <> str <> T.take 1 after
         in go depth (consumed : chunks) (T.drop 1 after)
      Just (c, rest) -> go depth (T.singleton c : chunks) rest

-- | Insert a value at a nested key path into a table.
insertNested :: [Text] -> TOMLValue -> Map Text TOMLValue -> Map Text TOMLValue
insertNested [] _ m = m
insertNested [k] v m = Map.insert k v m
insertNested (k : ks) v m = case Map.lookup k m of
  -- Under a [[table]] header the path crosses an array of tables: keys
  -- belong to its LAST element, not to a table replacing the array.
  Just (TOMLArray xs)
    | (TOMLTable lastInner : prev) <- reverse xs ->
        Map.insert k (TOMLArray (reverse prev ++ [TOMLTable (insertNested ks v lastInner)])) m
  Just (TOMLTable inner) -> Map.insert k (TOMLTable (insertNested ks v inner)) m
  _ -> Map.insert k (TOMLTable (insertNested ks v Map.empty)) m

-- | Ensure a table path exists (for @[table]@ headers).
ensureTable :: [Text] -> Map Text TOMLValue -> Map Text TOMLValue
ensureTable [] m = m
ensureTable [k] m = case Map.lookup k m of
  Just (TOMLTable _) -> m
  Nothing -> Map.insert k (TOMLTable Map.empty) m
  _ -> m
ensureTable (k : ks) m =
  let sub = case Map.lookup k m of
        Just (TOMLTable inner) -> inner
        _ -> Map.empty
   in Map.insert k (TOMLTable (ensureTable ks sub)) m

-- | Insert an entry into an array-of-tables (@[[table]]@).
insertArrayTable :: [Text] -> Map Text TOMLValue -> Map Text TOMLValue
insertArrayTable [] m = m
insertArrayTable [k] m = case Map.lookup k m of
  Just (TOMLArray xs) -> Map.insert k (TOMLArray (xs ++ [TOMLTable Map.empty])) m
  Nothing -> Map.insert k (TOMLArray [TOMLTable Map.empty]) m
  _ -> Map.insert k (TOMLArray [TOMLTable Map.empty]) m
insertArrayTable (k : ks) m =
  let sub = case Map.lookup k m of
        Just (TOMLTable inner) -> inner
        Just (TOMLArray xs) ->
          -- Descend into the last element of the array
          case reverse xs of
            (TOMLTable inner : _) -> inner
            _ -> Map.empty
        _ -> Map.empty
      updated = insertArrayTable ks sub
   in case Map.lookup k m of
        Just (TOMLArray xs) ->
          case reverse xs of
            (TOMLTable _ : prev) ->
              Map.insert k (TOMLArray (reverse prev ++ [TOMLTable updated])) m
            _ -> Map.insert k (TOMLTable updated) m
        _ -> Map.insert k (TOMLTable updated) m

-- ---------------------------------------------------------------------------
-- Builtin implementations - toXML
-- ---------------------------------------------------------------------------

-- | @builtins.toXML val@ - convert a Nix value to its XML representation.
-- Matches the format defined by the Nix manual: strings, ints, floats,
-- bools, nulls, lists, and attrsets map to their XML counterparts.
-- Built over BYTES: upstream's serializer copies string payloads into the
-- output with only the four ASCII escapes, so invalid UTF-8 passes
-- through raw rather than erroring (unlike toJSON, whose upstream
-- serializer validates).
builtinToXML :: (MonadEval m) => NixValue -> m NixValue
builtinToXML val = do
  xml <- valueToXML 0 val
  pure (mkStrBytes ("<?xml version='1.0' encoding='utf-8'?>\n<expr>\n" <> xml <> "</expr>\n"))

valueToXML :: (MonadEval m) => Int -> NixValue -> m BS.ByteString
valueToXML depth val = case val of
  VStr s _ ->
    pure (indent depth <> "<string value=" <> xmlQuote s <> " />\n")
  VInt n ->
    pure (indent depth <> "<int value=\"" <> BC.pack (show n) <> "\" />\n")
  VFloat d ->
    pure (indent depth <> "<float value=\"" <> TE.encodeUtf8 (formatXmlFloat d) <> "\" />\n")
  VBool True ->
    pure (indent depth <> "<bool value=\"true\" />\n")
  VBool False ->
    pure (indent depth <> "<bool value=\"false\" />\n")
  VNull ->
    pure (indent depth <> "<null />\n")
  VPath p ->
    pure (indent depth <> "<path value=" <> xmlQuote (TE.encodeUtf8 p) <> " />\n")
  VList cl -> do
    let thunks = map Thunk (clistThunks cl)
    items <- mapM (force >=> valueToXML (depth + 1)) thunks
    pure (indent depth <> "<list>\n" <> BS.concat items <> indent depth <> "</list>\n")
  VAttrs attrs -> do
    let pairs = attrSetToAscList attrs
    items <- mapM (attrToXML (depth + 1)) pairs
    pure (indent depth <> "<attrs>\n" <> BS.concat items <> indent depth <> "</attrs>\n")
  VLambda {} ->
    pure (indent depth <> "<function />\n")
  VBuiltin _ _ ->
    pure (indent depth <> "<function />\n")
  VDerivation _ ->
    pure (indent depth <> "<derivation />\n")
  VCompiledRegex _ ->
    pure (indent depth <> "<function />\n")
  where
    attrToXML d (name, thunk) = do
      v <- force thunk
      inner <- valueToXML d v
      pure (indent d <> "<attr name=" <> xmlQuote (TE.encodeUtf8 name) <> ">\n" <> inner <> indent d <> "</attr>\n")

indent :: Int -> BS.ByteString
indent n = BC.replicate (n * 2) ' '

xmlQuote :: BS.ByteString -> BS.ByteString
xmlQuote s = "\"" <> BC.concatMap escapeChar s <> "\""
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar c = BC.singleton c

-- ---------------------------------------------------------------------------
-- Builtin implementations - builtins.path
-- ---------------------------------------------------------------------------

-- | @builtins.path { path; name?; filter?; sha256?; recursive?; }@
--
-- Copy a path to the store and return the store path as a string with
-- context.  @name@ defaults to the basename of @path@.  @filter@ is
-- accepted but not yet applied (copies everything).
builtinPath :: (MonadEval m) => NixValue -> m NixValue
builtinPath (VAttrs attrs) = do
  pathStr <- forceAttrStr "builtins.path" "path" attrs
  nameOverride <- forceOptionalAttrStr attrs "name"
  expectedDigest <- case attrSetLookup "sha256" attrs of
    Nothing -> pure Nothing
    Just thunk -> do
      pinVal <- force thunk
      case pinVal of
        VStr pin _ -> do
          pinText <- decodedText "builtins.path" pin
          Just <$> decodeSha256Pin "builtins.path" pinText
        other -> throwEvalError ("builtins.path: 'sha256' must be a string, got " <> typeName other)
  let name = fromMaybe (extractBaseName pathStr) nameOverride
      pinSubject = "builtins.path: " <> pathStr
  storePathText <- case attrSetLookup "filter" attrs of
    Nothing -> copyPathToStore pathStr name (fmap (pinSubject,) expectedDigest)
    Just filterThunk -> do
      filterFn <- force filterThunk
      narBytes <- filteredSourceNar filterFn pathStr
      -- The sha256 pin applies to the FILTERED tree, as upstream.
      let filteredDigest = sha256Digest narBytes
      case expectedDigest of
        Just expected
          | expected /= filteredDigest ->
              throwEvalError
                ( pinSubject
                    <> ": hash mismatch: expected sha256:"
                    <> bytesToHexText expected
                    <> ", got sha256:"
                    <> bytesToHexText filteredDigest
                )
        _ -> pure ()
      addSourceNar name narBytes
  pure (sourceResultString storePathText)
builtinPath other =
  throwEvalError ("builtins.path: expected an attribute set, got " <> typeName other)

-- | The result value both source importers return: the store path as a
-- string carrying itself as context.
sourceResultString :: Text -> NixValue
sourceResultString storePathText =
  case parseStorePath defaultStoreDir storePathText of
    Just sp -> VStr (TE.encodeUtf8 storePathText) (plainContext sp)
    Nothing -> VStr (TE.encodeUtf8 storePathText) emptyContext

-- | Serialise a source tree to a NAR, keeping only entries the Nix filter
-- function accepts - upstream's addToStore filtering: the filter receives
-- @(path, type)@ for every entry BELOW the root (the root itself is never
-- filtered), and rejecting a directory prunes its whole subtree.  Child
-- paths hand the filter @parent/name@ with a forward slash; both path
-- styles reach Nix code only through separator-agnostic helpers like
-- @baseNameOf@.
filteredSourceNar :: (MonadEval m) => NixValue -> Text -> m BS.ByteString
filteredSourceNar filterFn rootPath = do
  rootType <- getFileType rootPath
  entry <- buildEntry rootPath rootType
  pure (NAR.serialise entry)
  where
    buildEntry path fileType = case fileType of
      "regular" -> do
        executable <- isExecutableFile path
        bytes <- readFileBytes path
        pure (NAR.NarRegular executable bytes)
      "symlink" -> NAR.NarSymlink <$> readSymlinkTarget path
      "directory" -> do
        children <- listDirectory path
        kept <- mapM (keepChild path) children
        pure (NAR.NarDirectory (catMaybes kept))
      other -> throwEvalError ("builtins.path: unsupported file type '" <> other <> "' at " <> path)
    keepChild parent (name, childType) = do
      let childPath = parent <> "/" <> name
      keep <- filterAccepts childPath childType
      if keep
        then Just . (,) name <$> buildEntry childPath childType
        else pure Nothing
    filterAccepts path fileType = do
      partial <- applyValue filterFn (mkStr path)
      result <- applyValue partial (mkStr fileType)
      case result of
        VBool b -> pure b
        other -> throwEvalError ("builtins.path: the filter function must return a Boolean, got " <> typeName other)

-- | Extract the last path component from a path string.
extractBaseName :: Text -> Text
extractBaseName path =
  let stripped = T.dropWhileEnd (\c -> c == '/' || c == '\\') path
   in case T.breakOnEnd "/" stripped of
        ("", _) -> case T.breakOnEnd "\\" stripped of
          ("", _) -> stripped
          (_, name) -> name
        (_, name) -> name

-- ---------------------------------------------------------------------------
-- Builtin implementations - filterSource
-- ---------------------------------------------------------------------------

-- | @builtins.filterSource filter path@ - copy a path to the store,
-- filtering entries via a predicate.  The filter function receives
-- @(path, type)@ where type is @"regular"@, @"directory"@, @"symlink"@,
-- or @"unknown"@.  Equivalent to @builtins.path@ with a filter and the
-- source's basename as the store name.
builtinFilterSource :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinFilterSource filterFn (VPath path) = filterSourceInto filterFn path
builtinFilterSource filterFn (VStr path _) =
  filterSourceInto filterFn =<< decodedText "builtins.filterSource" path
builtinFilterSource _ other =
  throwEvalError ("builtins.filterSource: expected a path, got " <> typeName other)

filterSourceInto :: (MonadEval m) => NixValue -> Text -> m NixValue
filterSourceInto filterFn path = do
  narBytes <- filteredSourceNar filterFn path
  storePathText <- addSourceNar (extractBaseName path) narBytes
  pure (sourceResultString storePathText)

-- ---------------------------------------------------------------------------
-- Builtin stubs - experimental features
-- ---------------------------------------------------------------------------

builtinOutputOf :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinOutputOf _ _ =
  throwEvalError "builtins.outputOf: requires experimental feature 'dynamic-derivations'"

builtinFetchTree :: (MonadEval m) => NixValue -> m NixValue
builtinFetchTree _ =
  throwEvalError "builtins.fetchTree: requires experimental feature 'fetch-tree'"

builtinFetchClosure :: (MonadEval m) => NixValue -> m NixValue
builtinFetchClosure _ =
  throwEvalError "builtins.fetchClosure: requires experimental feature 'fetch-closure'"
