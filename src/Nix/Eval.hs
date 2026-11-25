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
-- The evaluator maintains an environment, a t'Env', that maps variable
-- names to thunks.  @let@, @with@, function application, and
-- recursive attribute sets all extend the environment.
--
-- Thunks, environments and values live in the C data layer, so
-- everything here that builds or forces one runs in an evaluation
-- session ('Nix.Eval.IO.withEvalSession').  The exceptions are the shared
-- @null@, @true@ and @false@ cells and each builtin's own cell, which are
-- C statics outside every arena.  Outside a session the first C call that
-- needs an arena raises 'Nix.Eval.CStatus.ArenaNotInitialized' instead of
-- faulting.
module Nix.Eval
  ( -- * Values (re-exported from Types)
    NixValue (..),
    BuiltinArg (..),
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
    EvalPolicy (..),
    PathExistence (..),
    unrestrictedPolicy,
    PureEval,
    runPureEval,

    -- * Evaluation
    eval,
    evalParsed,
    compileSource,
    evalBytecode,
    force,
    deepForce,

    -- * Helpers (for Builtins)
    typeName,
    evaluated,
    readThunkValue,
    deferApply,

    -- * Fetcher transport validation (pure, exported for tests)
    FetchGitArgs (..),
    checkGitUrl,
    checkGitRef,
    checkGitRev,
    gitInputUrl,

    -- * What a fetch hands its tool (exported for tests)
    localFileUrl,
    gitRemote,

    -- * Remembered fetches (pure, exported for tests)
    FetchCache (..),
    fetchCacheKey,
    encodeFetchCache,
    decodeFetchCache,

    -- * Builtin definitions
    listedBuiltins,

    -- * Search path
    findInSearchPath,

    -- * Derivations
    inputModuloHashes,

    -- * Platform
    currentSystemStr,
  )
where

import Control.Monad (foldM, foldM_, forM_, mfilter, unless, void, when, (>=>))
import qualified Crypto.Hash as CH
import qualified Data.Array as Array
import Data.Bits (xor, (.&.), (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, digitToInt, intToDigit, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, ord, toUpper)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Read as TR
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Nix.Derivation (Derivation (..), DerivationOutput (..), drvFile, drvFileATerm, drvFileDerivation, drvFilePath, inputAddressed, textToPlatform, toATermForHash)
import Nix.Derivation.StructuredAttrs (StructuredAttrs (..), encodeStructuredAttrs, structuredAttrsKey)
import Nix.Eval.AllocSite (EnvSite (..), SetSite (..))
import Nix.Eval.Builtin (Builtin, builtinName)
import qualified Nix.Eval.Builtin as Builtin
import Nix.Eval.CBytecode (attrsPlain, attrsRec, attrsRecOverrides, cbcArg1, cbcArg2, cbcArg3, cbcCountedPayload, cbcData, cbcFlags, cbcOpcode, cbcShortArg, cbcString, cbcStringBytes, strpartEsc, strpartLit, pattern OpApp, pattern OpAssert, pattern OpAttrs, pattern OpBinary, pattern OpHasAttr, pattern OpIf, pattern OpIndStr, pattern OpLambda, pattern OpLet, pattern OpList, pattern OpLitBool, pattern OpLitFloat, pattern OpLitInt, pattern OpLitNull, pattern OpLitPath, pattern OpLitUri, pattern OpPathStr, pattern OpPos, pattern OpResolvedVar, pattern OpSelect, pattern OpStr, pattern OpUnary, pattern OpVar, pattern OpWith, pattern OpWithVar)
import Nix.Eval.CData (MonadCData)
import Nix.Eval.CList (CList, clistDrop, clistIndex)
import Nix.Eval.CStr (cstrText)
import Nix.Eval.CanonPath (canonBaseName, canonDirName, canonPathValue)
import Nix.Eval.Compile (BcBinding (..), BcBindingCounts (..), BcBindings (..), BcCaptureInfo (..), BcInheritedName (..), BcShares (..), decodeBcBindings, decodeBcCaptureInfo, decodeBcFormals, decodeBinaryOp, decodeUnaryOp, foldBcBindings, foldBcInheritedNames, foldBcShares, reassembleDouble, reassembleInt64)
import Nix.Eval.Context (contextIsEmpty, encodedElement, extractAllOutputRefs, extractInputDrvs, extractInputSrcs, firstContextEncoded, inUpstreamOrder, plainContext)
import Nix.Eval.EvalFormals (FormalDefault (..), LambdaName, hasDefault, lambdaNameText)
import Nix.Eval.Operator (addToFloat, addToInteger, evalBinary, evalUnary, evalUpdateChain, expectInt, expectedFailure, nixCompare, nixEqual, primAdd, primDiv, primMul, primSub)
import Nix.Eval.Policy (fileUrlPrefix, isAbsolutePath)
import Nix.Eval.Print (PrintOptions (..), formatXmlFloat, printSymbol, printValue)
import Nix.Eval.RealiseContext (PendingOrigin (..), contextRefusalMessage, findFileRefusalMessage)
import Nix.Eval.SourcePos (PosIdx (..), RecordedPos (..), lookupPos, printPos)
import Nix.Eval.StringInterp (StringChunk (..), coerceToString, concatChunks, formatNixFloat, stripIndentedChunks)
import Nix.Eval.Symbol (Symbol (..), symbolIntern, symbolText)
import Nix.Eval.Types
  ( AttrEntry (..),
    AttrSet (..),
    BuiltinArg (..),
    CAttrSet,
    Env (..),
    EvalFormal (..),
    EvalFormals (..),
    EvalPolicy (..),
    HeldBytes (..),
    MonadEval (..),
    NixValue (..),
    PathExistence (..),
    Pending (..),
    PureEval,
    StringContext (..),
    StringContextElement (..),
    Thunk (..),
    addErrorTrace,
    attrSetElems,
    attrSetFindName,
    attrSetFoldM,
    attrSetFromMap,
    attrSetFromSymbolEntries,
    attrSetFromSymbolEntriesSizedFor,
    attrSetFromSymbols,
    attrSetKeys,
    attrSetLookup,
    attrSetLookupEntry,
    attrSetLookupSymbol,
    attrSetLookupSymbolEntry,
    attrSetMapWithKey,
    attrSetMember,
    attrSetMemberSymbol,
    attrSetNull,
    attrSetRemoveKeys,
    attrSetSize,
    attrSetToAscList,
    attrSetToMap,
    buildBcCaptureEnv,
    buildCAttrSetKeys,
    bytesToTextLossy,
    cheapThunkBc,
    clistFromThunks,
    clistLen,
    clistThunks,
    compile,
    compileSource,
    emptyContext,
    emptyEnv,
    envLookup,
    envLookupResolved,
    envSlotCells,
    envWithScopesRaw,
    evaluated,
    fillCAttrSetValues,
    flatCAttrSet,
    frameThunkBc,
    heldString,
    mkApply2Thunk,
    mkApplyThunk,
    mkStr,
    mkStrBytes,
    mkThunkBc,
    newEnv,
    newScopeEnv,
    pushLazyWithScope,
    readThunkValue,
    resolveWithScopeAt,
    runPureEval,
    shareSlot,
    storePathOrThrow,
    tieFrame,
    typeName,
    typeOfValue,
    unrestrictedPolicy,
    warnMessage,
    withScopeAt,
  )
import Nix.Expr.Resolve (undefinedVariableMessage)
import Nix.Expr.Types
  ( BinaryOp (..),
    Expr (..),
    UnaryOp (..),
  )
import Nix.Hash (FileIngestionMethod (..), base64ToBytes, bytesToHexText, fileIngestionPrefix, hashAlgoBytes, hashPlaceholder, makeFixedOutputPath, parseAnyHash, parseHashAlgo, sha256Digest, sriDigest, unknownHashAlgo)
import Nix.Json (IntegerRange (..), Json (..), JsonBuilder (..), parseJsonInto, renderJson)
import Nix.Parser (ParsedSource)
import Nix.Store.Path (StorePath (spName), StorePathNameError (..), checkStorePathName, defaultStoreDir, defaultStoreDirText, isInStoreText, parseStorePath, parseStorePathBaseName, storePathBaseNameError, storePathNameErrorText, storePathNameReasonText, storePathNameUpstreamText, storePathToText)
import Nix.Store.Path.Internal (maskedOutputPath)
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
-- Its names are interned in the order the compiler meets them; source
-- goes through 'evalParsed', which interns them in upstream's order.
eval :: (MonadEval m) => Env -> Expr -> m NixValue
eval env expr = compile expr >>= evalBytecode env

-- | Evaluate parsed source in the environment the action builds: its
-- names are interned in the order upstream's parser interns them, it
-- compiles, and only then is the environment built and the code run as
-- under 'eval'.  Upstream interns no string, so a program's names keep
-- their parse order whatever strings its environment binds; building the
-- environment after the compile keeps the program's names ahead of the
-- strings and names that building it interns here.
evalParsed :: (MonadEval m) => m Env -> ParsedSource -> m NixValue
evalParsed makeEnv parsed = do
  code <- compileSource parsed
  env <- makeEnv
  evalBytecode env code

-- | Force a thunk to a value.
--
-- Delegates to 'forceThunk' which is a 'MonadEval' method - this
-- allows IO evaluators to implement memoization (caching the result
-- after the first force) while pure evaluators simply re-evaluate.
force :: (MonadEval m) => Thunk -> m NixValue
force = forceThunk runPending

-- | Do what a pending thunk holds: run its code, or make its deferred
-- application.  An application opens its call frame and forces its
-- function inside it, as upstream's @forceValue@ on an app value is a
-- @callFunction@ that forces the function after the depth check.  One
-- of two arguments applies the function to the first in a second frame,
-- as the inner of upstream's two nested app values does.
runPending :: (MonadEval m) => Pending -> m NixValue
runPending (PendingCode env bcIdx) = evalBytecode env bcIdx
runPending (PendingApply fn arg) = withCallFrame (force fn >>= callCell arg)
runPending (PendingApply2 fn first second) =
  withCallFrame (withCallFrame (force fn >>= callCell first) >>= callCell second)

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
  cbcOpcode bcIdx >>= \case
    OpLitInt -> VInt . uncurry reassembleInt64 <$> literalWords bcIdx
    OpLitFloat -> VFloat . uncurry reassembleDouble <$> literalWords bcIdx
    OpLitBool -> VBool . (/= 0) <$> cbcShortArg bcIdx
    OpLitNull -> pure VNull
    OpLitUri -> cbcArg1 bcIdx >>= cbcString >>= heldString
    OpLitPath -> do
      code <- cbcArg1 bcIdx >>= cbcString
      text <- cstrText code
      resolved <- resolvePathLiteral text
      pure (VPathHeld resolved (if resolved == text then HeldStr code else NotHeld))
    OpStr -> evalBcStr env bcIdx
    OpIndStr -> evalBcIndStr env bcIdx
    OpPathStr -> evalBcPathStr env bcIdx
    OpVar -> cbcArg1 bcIdx >>= evalVar env . Symbol
    OpWithVar -> cbcArg1 bcIdx >>= evalWithVar env . Symbol
    OpResolvedVar -> do
      level <- cbcArg1 bcIdx
      idx <- cbcArg2 bcIdx
      envLookupResolved level idx env >>= force
    OpAttrs -> evalBcAttrs env bcIdx
    OpList -> evalBcList env bcIdx
    OpSelect -> evalBcSelect env bcIdx
    OpHasAttr -> evalBcHasAttr env bcIdx
    OpApp -> evalBcApp env bcIdx
    OpLambda -> evalBcLambda env bcIdx
    OpLet -> evalBcLet env bcIdx
    OpIf -> do
      condIdx <- cbcArg1 bcIdx
      condVal <- evalBytecode env condIdx
      case condVal of
        VBool True -> cbcArg2 bcIdx >>= evalBytecode env
        VBool False -> cbcArg3 bcIdx >>= evalBytecode env
        _ -> throwEvalError ("'if' condition must be a Boolean, got " <> typeName condVal)
    OpWith -> do
      scopeIdx <- cbcArg1 bcIdx
      bodyIdx <- cbcArg2 bcIdx
      -- Lazy with: defer forcing the scope until a WITH_VAR lookup
      -- actually needs it.  This is critical for nixpkgs where
      -- all-packages.nix uses `with pkgs;` inside a fixpoint -
      -- eagerly forcing the scope would blackhole.
      scopeThunk <- cheapThunkBc env scopeIdx
      withEnv <- pushLazyWithScope scopeThunk env
      evalBytecode withEnv bodyIdx
    OpAssert -> do
      condIdx <- cbcArg1 bcIdx
      condVal <- evalBytecode env condIdx
      case condVal of
        VBool True -> cbcArg2 bcIdx >>= evalBytecode env
        -- A failed assert is catchable by tryEval (upstream
        -- AssertionError); a non-bool condition is a type error.
        VBool False -> throwCatchableError "assertion failed"
        _ -> throwEvalError ("assertion condition must be a Boolean, got " <> typeName condVal)
    OpUnary -> do
      flags_ <- cbcFlags bcIdx
      operandIdx <- cbcArg1 bcIdx
      case decodeUnaryOp flags_ of
        Just unop ->
          let operate = evalBytecode env operandIdx >>= evalUnary unop
           in case unop of
                -- Upstream's parser turns @-e@ into @__sub 0 e@, a
                -- call frame around the operand's forcing.
                OpNegate -> withCallFrame operate
                OpNot -> operate
        Nothing -> corruptBytecode "unary operator tag" flags_
    OpBinary -> evalBcBinary env bcIdx
    OpPos -> cbcArg1 bcIdx >>= positionValue . PosIdx
    opcode -> corruptBytecode "opcode" opcode

-- | The two 32-bit words of an integer or float literal, low word first.
literalWords :: (MonadCData m) => Word32 -> m (Word32, Word32)
literalWords bcIdx = (,) <$> cbcArg1 bcIdx <*> cbcArg2 bcIdx

-- ---------------------------------------------------------------------------
-- Bytecode helpers
-- ---------------------------------------------------------------------------

-- | The error for a value the compiler never writes where the evaluator
-- reads one (@what@ names the field), which only a corrupt bytecode store
-- holds.  It is an eval error, so @builtins.tryEval@ does not catch it.
corruptBytecode :: (MonadEval m, Show tag) => Text -> tag -> m a
corruptBytecode what tag = throwEvalError ("corrupt bytecode: unknown " <> what <> " " <> T.pack (show tag))

-- | Fold over bindings ('foldBcBindings'), or fail on a binding tag the
-- compiler never writes.  Inlined, so that each caller's step is a known
-- function rather than a closure allocated every time the instruction
-- runs, and each caller's step has an INLINE pragma, so that a binding's
-- fields reach it without the binding being allocated.
foldBindings :: (MonadEval m) => (acc -> BcBinding -> m acc) -> acc -> BcBindings -> m acc
foldBindings = foldBcBindings (corruptBytecode "binding tag")
{-# INLINE foldBindings #-}

-- | Continue with the capture info at a data offset, or fail on a capture
-- tag the compiler never writes.  Inlined, as 'foldBindings' is.
withBcCaptureInfo :: (MonadEval m) => Word32 -> (BcCaptureInfo -> m a) -> m a
withBcCaptureInfo captureOff next =
  decodeBcCaptureInfo captureOff >>= either (corruptBytecode "capture info tag") next
{-# INLINE withBcCaptureInfo #-}

-- | Whether upstream's parser desugars the operator into a builtin call
-- (@__sub@, @__mul@, @__div@ or @__lessThan@; parser.y at 2.24.9), which
-- then costs a call frame while its operands are forced.  The others are
-- @Expr@ nodes of their own there and cost nothing.
desugaredToCall :: BinaryOp -> Bool
desugaredToCall op = case op of
  OpSub -> True
  OpMul -> True
  OpDiv -> True
  OpLt -> True
  OpLte -> True
  OpGt -> True
  OpGte -> True
  OpAdd -> False
  OpAnd -> False
  OpOr -> False
  OpImpl -> False
  OpEq -> False
  OpNeq -> False
  OpConcat -> False
  OpUpdate -> False

-- | Evaluate a binary operation from bytecode, with short-circuit
-- support for &&, ||, ->.
evalBcBinary :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcBinary env bcIdx0 = do
  flags_ <- cbcFlags bcIdx0
  leftIdx <- cbcArg1 bcIdx0
  rightIdx <- cbcArg2 bcIdx0
  case decodeBinaryOp flags_ of
    Just op ->
      let operate = do
            leftVal <- evalBytecode env leftIdx
            rightVal <- evalBytecode env rightIdx
            evalBinary force op leftVal rightVal
       in case op of
            OpAnd -> evalShortCircuitAnd env leftIdx rightIdx
            OpOr -> evalShortCircuitOr env leftIdx rightIdx
            OpImpl -> evalShortCircuitImpl env leftIdx rightIdx
            OpAdd -> evalAddition (evalBytecode env leftIdx) (evalBytecode env rightIdx)
            -- Not through 'operate', which forces both operands before either
            -- is looked at: each operand of a chain is checked before the
            -- next is evaluated.
            OpUpdate -> do
              operands <- (<>) <$> updateOperands leftIdx <*> updateOperands rightIdx
              evalUpdateChain (evalBytecode env <$> operands)
            _
              | desugaredToCall op -> withCallFrame operate
              | otherwise -> operate
    Nothing -> corruptBytecode "binary operator tag" flags_

-- | The operands of the chain of @//@ an operand starts, first to last:
-- the operand itself, or, for an update, its two operands' own, as
-- upstream's @evalForUpdate@ queues a chain, descending into an operand
-- only when it is an @ExprOpUpdate@ (eval.cc at 2.34.9).  Parentheses make no
-- node, so a chain however they group it is one.
updateOperands :: (MonadCData m) => Word32 -> m (NonEmpty Word32)
updateOperands bcIdx = do
  opcode <- cbcOpcode bcIdx
  flags_ <- cbcFlags bcIdx
  case (opcode, decodeBinaryOp flags_) of
    (OpBinary, Just OpUpdate) -> (<>) <$> (cbcArg1 bcIdx >>= updateOperands) <*> (cbcArg2 bcIdx >>= updateOperands)
    _ -> pure (bcIdx :| [])

-- | @+@, as upstream's @ExprConcatStrings::eval@ runs it over two operands
-- (eval.cc at 2.24.9, the same in 2.33.2).  The left operand is evaluated
-- and dealt with before the right is evaluated, and its type alone decides
-- what @+@ means: an integer or float makes it arithmetic, a path makes the
-- result a path, and anything else makes it string concatenation, each
-- operand coerced strictly (no numbers, Booleans, null or lists), with
-- upstream's message for a value that does not coerce.  So @{} + throw
-- "b"@ fails on the set without evaluating the throw, and @true + 1@ is a
-- coercion failure, not a refused addition.
--
-- Only a string on the left copies a path into the store, as upstream
-- copies only when the first operand is a string: @"x" + ./src@ carries
-- the source's store path in its context, while @{ outPath = ./src; } +
-- "x"@ is the path's own text with no context.  A path on the left
-- appends the right operand's text without a copy, and a right operand
-- carrying string context is refused, since a path value cannot keep the
-- reference.  Paths are Text here, so the appended bytes must decode.
evalAddition :: (MonadEval m) => m NixValue -> m NixValue -> m NixValue
evalAddition evalLeft evalRight =
  evalLeft >>= \case
    VInt a -> evalRight >>= addToInteger a
    VFloat a -> evalRight >>= addToFloat a
    VPath base -> do
      (appended, context) <- evalRight >>= coerceToString False force applyValue coercePathVerbatim
      unless (contextIsEmpty context) $
        throwEvalError "a string that refers to a store path cannot be appended to a path"
      text <- decodedText "path concatenation" appended
      pure (VPath (canonPathValue (base <> text)))
    left -> do
      let coercePath = case left of
            VStr {} -> coercePathToStore
            _ -> coercePathVerbatim
          coerce = coerceToString False force applyValue coercePath
      (leftStr, leftCtx) <- coerce left
      (rightStr, rightCtx) <- evalRight >>= coerce
      pure (VStr (leftStr <> rightStr) (leftCtx <> rightCtx))

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

-- | Evaluate a string literal from bytecode data buffer.  A string with no
-- interpolation is one literal part, since the lexer joins a run of
-- characters and escapes into one chunk, and its value is that part's
-- string in the code, which a cell storing the value points at: upstream's
-- parser makes it an @ExprString@ holding the value (parser.y), where an
-- interpolated string is an @ExprConcatStrings@.
evalBcStr :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcStr env bcIdx0 = do
  (count, dataOff) <- countedPayload1 bcIdx0
  -- The first part's tag is read only when there is one: an empty string
  -- owns no data word at its offset.
  literal <- if count == 1 then (== strpartLit) <$> cbcData dataOff else pure False
  if literal
    then cbcData (dataOff + 1) >>= cbcString >>= heldString
    else do
      chunks <- evalBcStringParts env count dataOff
      let (text, ctx) = concatChunks chunks
      pure (VStr text ctx)

-- | An instruction's payload count and data offset, from the offset in
-- its first argument ('cbcCountedPayload').
countedPayload1 :: (MonadCData m) => Word32 -> m (Int, Word32)
countedPayload1 bcIdx = cbcCountedPayload bcIdx =<< cbcArg1 bcIdx

-- | Evaluate an indented string literal from bytecode data buffer.  The
-- common indentation is stripped before concatenation, part by part as
-- upstream strips it, so an interpolated multi-line value cannot drag the
-- computed indent down.
evalBcIndStr :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcIndStr env bcIdx0 = do
  (count, dataOff) <- countedPayload1 bcIdx0
  chunks <- evalBcStringParts env count dataOff
  let (text, ctx) = stripIndentedChunks chunks
  pure (VStr text ctx)

-- | Evaluate an interpolated path literal from the bytecode data
-- buffer.  The pieces concatenate with no separator, each interpolated
-- value coerced as a path segment, and the whole resolves like a plain
-- path literal - absolutized if still relative, @~\/@ expanded, and
-- canonicalized, so a @..@ or a slash arriving at runtime collapses
-- textually, matching upstream (verified against nix-instantiate
-- 2.33.2: @let v = "a\/.."; in .\/${v}@ is the base directory itself).
evalBcPathStr :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcPathStr env bcIdx0 = do
  (count, dataOff) <- countedPayload1 bcIdx0
  parts <- evalBcPathParts env count dataOff
  unless (all (contextIsEmpty . snd) parts) $
    throwEvalError "a string that refers to a store path cannot be appended to a path"
  text <- decodedText "path literal" (BS.concat (map fst parts))
  VPath <$> resolvePathLiteral text

-- | The path-segment walk under 'evalBcPathStr': literal pieces pass
-- through; an interpolated value coerces with upstream's path-segment
-- rules - a context-free string passes, and a path contributes its text
-- WITHOUT a store copy (unlike string interpolation, which copies).  The
-- context comes back with each piece, because a string carrying it is
-- refused only once every piece has coerced: a path value has nowhere to
-- keep the context that holds a store reference alive, and upstream's
-- @ExprConcatStrings::eval@ checks for one after its loop, so a later
-- piece that fails to coerce is the error reported.  Observed from
-- nix-instantiate 2.33.2.
evalBcPathParts :: (MonadEval m) => Env -> Int -> Word32 -> m [(BS.ByteString, StringContext)]
evalBcPathParts _ 0 _ = pure []
evalBcPathParts env n off = do
  tag <- cbcData off
  val <- cbcData (off + 1)
  part <- case tag of
    0 -> (,emptyContext) <$> cbcStringBytes val
    _ -> evalBytecode env val >>= coerceToString False force applyValue coercePathVerbatim
  rest <- evalBcPathParts env (n - 1) (off + 2)
  pure (part : rest)

-- | Evaluate string parts from the bytecode data buffer.  Each part is two
-- words: (tag, value), the index of a string of the code's under
-- 'strpartLit' or 'strpartEsc' and a bytecode index under
-- 'Nix.Eval.CBytecode.strpartInterp'.  The kind survives into the chunk
-- because an indented string strips an escape and an interpolation
-- differently.
evalBcStringParts :: (MonadEval m) => Env -> Int -> Word32 -> m [StringChunk]
evalBcStringParts _ 0 _ = pure []
evalBcStringParts env n off = do
  tag <- cbcData off
  val <- cbcData (off + 1)
  chunk <- decodeChunk tag val
  rest <- evalBcStringParts env (n - 1) (off + 2)
  pure (chunk : rest)
  where
    decodeChunk tag val
      | tag == strpartLit = ChunkLit <$> cbcStringBytes val
      | tag == strpartEsc = ChunkEsc <$> cbcStringBytes val
      | otherwise = do
          v <- evalBytecode env val
          (txt, ctx) <- coerceToStringInterp v
          pure (ChunkInterp txt ctx)

-- | Evaluate a list from bytecode data buffer.
evalBcList :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcList env bcIdx0 = do
  (count, dataOff) <- countedPayload1 bcIdx0
  children <- mapM (\i -> cbcData (dataOff + fromIntegral i) >>= cheapThunkBc env) [0 .. count - 1]
  VList <$> clistFromThunks (map unThunk children)

-- | Evaluate a function application from bytecode.  A source application
-- ('EApp') evaluates the function position outside the call frame and
-- everything after it inside, as upstream's @ExprCall::eval@ evaluates
-- @fun@ before @callFunction@ opens the frame.
evalBcApp :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcApp env bcIdx0 = do
  funcIdx <- cbcArg1 bcIdx0
  argIdx <- cbcArg2 bcIdx0
  evalBytecode env funcIdx >>= withCallFrame . callBc env argIdx

-- | Apply an evaluated function to the argument at a bytecode index,
-- inside the caller's frame ('callWith').
callBc :: (MonadEval m) => Env -> Word32 -> NixValue -> m NixValue
callBc env argIdx = callWith (cheapThunkBc env argIdx) (evalBytecode env argIdx) (callBc env argIdx)

-- | Apply an evaluated function to the argument in a cell, inside the
-- caller's frame ('callWith'): a deferred application's.
callCell :: (MonadEval m) => Thunk -> NixValue -> m NixValue
callCell cell = callWith (pure cell) (force cell) (callCell cell)

-- | Apply an evaluated function to an argument inside the caller's frame,
-- the argument given as its cell, as its value, and as the application of
-- another function to it, a functor's second call; each is taken only
-- where the function takes it so.  The argument stays a cell for a lambda
-- (the normal laziness).  A builtin takes it evaluated, without a cell,
-- or as a cell, by its mode for the position.  Not recursive, and given
-- its three arguments by 'callBc' and 'callCell', so that it inlines into
-- both and their actions are never built as closures.
{-# INLINE callWith #-}
callWith :: (MonadEval m) => m Thunk -> m NixValue -> (NixValue -> m NixValue) -> NixValue -> m NixValue
callWith takeCell takeValue callAgain = \case
  VLambda closureEnv formals bodyBcIdx -> do
    argThunk <- takeCell
    extEnv <- matchFormals closureEnv formals argThunk
    evalBytecode extEnv bodyBcIdx
  VBuiltin builtin taken -> applyBuiltin builtin taken $ \case
    ForceOnApply -> ArgForced <$> takeValue
    ForceOnUse -> ArgDeferred <$> takeCell
  funcVal@(VAttrs attrs) -> callFunctor funcVal attrs callAgain
  funcVal -> notAFunction funcVal

-- | The refusal of a call to a value that is not a function.
notAFunction :: (MonadEval m) => NixValue -> m a
notAFunction value = throwEvalError ("attempt to call " <> typeName value <> ", which is not a function")

-- | @builtins.tryEval e@: @{ success, value }@ with catchable errors
-- (a throw, a failed assert, and a search path lookup that matches no
-- entry - the kinds 'catchEvalError' recovers) mapped to @success = false@.
-- The catch must wrap the argument's forcing, so the argument is taken
-- unforced.  One that arrives already forced has nothing left to catch and is
-- success-wrapped, as upstream treats a value in WHNF.
builtinTryEval :: (MonadEval m) => BuiltinArg -> m NixValue
builtinTryEval arg = do
  result <- catchEvalError (argValue arg)
  valuesSet SetTryEval . Map.fromList $ case result of
    Right val -> [("success", VBool True), ("value", val)]
    Left _ -> [("success", VBool False), ("value", VBool False)]

-- | Evaluate a lambda literal from bytecode - returns VLambda.  Its
-- formals and capture info are decoded here, so that a corrupt tag fails
-- the instruction, and the value is built here too, rather than left as a
-- thunk holding them that would cost more than the value.  Building it
-- reads the captured slots without forcing them.
evalBcLambda :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcLambda env bcIdx0 = do
  flags_ <- cbcFlags bcIdx0
  formalsOff <- cbcArg1 bcIdx0
  decodeBcFormals (corruptBytecode "formal default kind") flags_ formalsOff >>= \case
    Just formals -> do
      bodyBcIdx <- cbcArg2 bcIdx0
      captureOff <- cbcArg3 bcIdx0
      withBcCaptureInfo captureOff $ \captureInfo -> do
        closureEnv <- buildBcCaptureEnv EnvClosure env captureInfo
        pure (VLambda closureEnv formals bodyBcIdx)
    Nothing -> corruptBytecode "formals type" flags_

-- | Evaluate a select expression from bytecode, as upstream's
-- @ExprSelect::eval@ in eval.cc does: with no default, a value the path goes
-- into fails as one that is not a set, and a set that lacks the next name
-- fails naming it as a symbol prints ('printSymbol').
evalBcSelect :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcSelect env bcIdx0 = do
  targetIdx <- cbcArg1 bcIdx0
  (pathLen, pathOff) <- cbcCountedPayload bcIdx0 =<< cbcArg2 bcIdx0
  targetVal <- evalBytecode env targetIdx
  result <- walkBcAttrPath env pathLen pathOff (pure targetVal)
  case result of
    PathFound val -> pure val
    PathMissing name -> orDefault (throwEvalError ("attribute '" <> printSymbol (symbolText name) <> "' missing"))
    PathNotASet val -> orDefault (addErrorTrace "while selecting an attribute" (expectedFailure "a set" val))
  where
    orDefault failure = do
      hasDef <- (/= 0) <$> cbcFlags bcIdx0
      if hasDef then cbcArg3 bcIdx0 >>= evalBytecode env else failure

-- | Evaluate a hasAttr expression from bytecode.
evalBcHasAttr :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcHasAttr env bcIdx0 = do
  targetIdx <- cbcArg1 bcIdx0
  (pathLen, pathOff) <- cbcCountedPayload bcIdx0 =<< cbcArg2 bcIdx0
  targetVal <- evalBytecode env targetIdx
  VBool <$> hasBcAttrPath env pathLen pathOff targetVal

-- | Resolve one attr-path element (two words at @off@) to the name it
-- selects.  A static name is the symbol the bytecode holds.  A computed
-- one is read and interned as upstream's select and has-attr read and
-- intern one (@getName@ in eval.cc), null refused with the rest
-- ('internComputedName').
resolveBcAttrKey :: (MonadEval m) => Env -> Word32 -> m Symbol
resolveBcAttrKey env off = do
  isExpr <- cbcData off
  keyVal <- cbcData (off + 1)
  if isExpr /= 0
    then evalBytecode env keyVal >>= internComputedName
    else pure (Symbol keyVal)

-- | Where a select's walk down its attribute path ended.
data PathWalk
  = -- | Every name found: the value at the end, forced.
    PathFound !NixValue
  | -- | A set the path goes into lacks this name.
    PathMissing !Symbol
  | -- | A value the path goes into is not a set.
    PathNotASet !NixValue

-- | Walk an attribute path stored in the bytecode data buffer.
-- Each element is two words: (is_expr, key_or_bc_idx).  Each step reads
-- its name, interning one it computes, then forces the value it selects
-- from, then looks the name up, and the value found is forced at the
-- next step or at the end, the order upstream's @ExprSelect::eval@ keeps
-- (eval.cc), so a name that fails to evaluate fails before the value it
-- selects from, and a computed name takes its symbol before any name that
-- forcing the value interns.
walkBcAttrPath :: (MonadEval m) => Env -> Int -> Word32 -> m NixValue -> m PathWalk
walkBcAttrPath _ 0 _ current = PathFound <$> current
walkBcAttrPath env n off current = do
  key <- resolveBcAttrKey env off
  val <- current
  case val of
    VAttrs attrs ->
      attrSetLookupSymbol key attrs >>= \case
        Just thunk -> walkBcAttrPath env (n - 1) (off + 2) (force thunk)
        Nothing -> pure (PathMissing key)
    _ -> pure (PathNotASet val)

-- | Presence walk for has-attr: intermediates force (the walk must reach
-- a set), but the terminal element is a membership check only - upstream
-- ``a ? b.c`` never evaluates the final attribute's value.
hasBcAttrPath :: (MonadEval m) => Env -> Int -> Word32 -> NixValue -> m Bool
hasBcAttrPath _ 0 _ _ = pure True
hasBcAttrPath env n off val = case val of
  VAttrs attrs -> do
    key <- resolveBcAttrKey env off
    attrSetLookupSymbol key attrs >>= \case
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
  kind <- cbcFlags bcIdx0
  bindings <- countedPayload1 bcIdx0 >>= uncurry decodeBcBindings
  case setKind kind of
    Just PlainSet -> evalBcNonRecAttrs env bindings
    Just (RecSet values) -> cbcArg2 bcIdx0 >>= \captureOff -> withBcCaptureInfo captureOff (evalBcRecAttrs values env bindings)
    Nothing -> corruptBytecode "attribute set flag" kind

-- | What an attribute set is.
data SetKind
  = -- | A set that is not recursive.
    PlainSet
  | -- | A recursive set, its static values made as the 'FrameValues' say.
    RecSet !FrameValues

-- | The kind of attribute set an instruction's flag says it is, or
-- 'Nothing' for a flag the compiler never writes.
setKind :: Word8 -> Maybe SetKind
setKind kind
  | kind == attrsPlain = Just PlainSet
  | kind == attrsRec = Just (RecSet MaybeThunkValues)
  | kind == attrsRecOverrides = Just (RecSet ThunkValues)
  | otherwise = Nothing

-- | Evaluate a non-recursive attr set from bytecode bindings: its static
-- attributes ('bindStaticAttrs'), then its dynamic ones
-- ('bindDynamicAttrs'), whose names are evaluated in the enclosing env,
-- as upstream's @ExprAttrs::eval@ in eval.cc binds them.
--
-- Values go through 'cheapThunkBc', so a constant or a variable becomes
-- the value rather than a thunk over it, which is what upstream's
-- non-recursive @ExprAttrs::eval@ does through @maybeThunk@.  The env here
-- is the enclosing one and is fully built, so every variable's cell can
-- be read now.
evalBcNonRecAttrs :: (MonadEval m) => Env -> BcBindings -> m NixValue
evalBcNonRecAttrs env bindings =
  bindStaticAttrs cheapThunkBc env env bindings
    >>= fmap VAttrs . literalSet SetLiteral cheapThunkBc env bindings

-- | The set of bindings whose static attributes are bound, with its
-- dynamic ones bound in @dynamicEnv@ ('bindDynamicAttrs') when it has
-- any.  It is sized as upstream sizes a literal's set, for one attribute
-- per static name and one per dynamic binding (@attrs.size() +
-- dynamicAttrs.size()@ in eval.cc's @ExprAttrs::eval@), so a literal
-- whose every name is a null @${...}@ makes an empty set of its own.
literalSet :: (MonadEval m) => SetSite -> (Env -> Word32 -> m Thunk) -> Env -> BcBindings -> Map Symbol AttrEntry -> m AttrSet
literalSet site mkValueThunk dynamicEnv bindings@(BcBindings (BcBindingCounts names dynamics) _ _ _) statics =
  withDynamics >>= attrSetFromSymbolEntriesSizedFor site (names + dynamics)
  where
    withDynamics
      | dynamics == 0 = pure statics
      | otherwise = bindDynamicAttrs mkValueThunk dynamicEnv bindings statics

-- | The construct a binding frame belongs to.
data FrameKind = LetFrame | RecFrame

-- | The site of a frame's env.
frameSite :: FrameKind -> EnvSite
frameSite LetFrame = EnvLet
frameSite RecFrame = EnvRec

-- | The site of the env holding what a frame captures from outside it.
frameCaptureSite :: FrameKind -> EnvSite
frameCaptureSite LetFrame = EnvLetCapture
frameCaptureSite RecFrame = EnvRecCapture

-- | How the frame of a recursive attr set or a @let@ holds its names.
data FrameLayout
  = -- | In this many slots, one per name in declaration order: every
    -- name is static.
    PositionalFrame !Int
  | -- | In a name table: a name is dynamic.
    ScopedFrame

-- | The layout of the frame bindings make, from the names the compiler
-- counted in their t'BcBindingCounts': by slot when no name is dynamic,
-- as the resolver resolved the bindings' variables.
frameLayout :: BcBindings -> FrameLayout
frameLayout (BcBindings (BcBindingCounts names 0) _ _ _) = PositionalFrame names
frameLayout _ = ScopedFrame

-- | How a frame makes the value a static name's binding gives it.
data FrameValues
  = -- | As upstream's @maybeThunk@ makes it ('frameThunkBc'), a variable
    -- naming one of the frame's own names sharing its cell where the
    -- frame's shares say ('Nix.Eval.Compile.siblingShares').
    MaybeThunkValues
  | -- | As a thunk of its own, but for an inherited name's, as upstream
    -- makes the values of a recursive set that binds @__overrides@
    -- (@ExprAttrs::eval@ in eval.cc).
    ThunkValues

-- | The frame of a recursive attr set or a @let@ in @env@ whose names are
-- not all static: an env whose name table binds the static names
-- ('bcBindingStaticKeys'), under @env@ as @captureInfo@ trims it
-- ('buildBcCaptureEnv'), whose with-scopes the frame sees.  Answers the env
-- and the static attributes, their values made as @values@ says and
-- filled into its table before any dynamic name is evaluated in it, as
-- upstream's @ExprAttrs::eval@ in eval.cc fills @env2@ first.  Inlined,
-- as 'tieBindingFrame' is, so that each caller's sites are constants.
{-# INLINE scopeBindingFrame #-}
scopeBindingFrame :: (MonadEval m) => FrameKind -> FrameValues -> Env -> BcBindings -> BcCaptureInfo -> m (Env, Map Symbol AttrEntry)
scopeBindingFrame kind values env bindings captureInfo = do
  names <- bcBindingStaticKeys bindings
  scope <- buildCAttrSetKeys names
  parent <- buildBcCaptureEnv (frameCaptureSite kind) env captureInfo
  frame <- newScopeEnv (frameSite kind) (Just parent) (AttrSet (flatCAttrSet scope))
  statics <- case values of
    MaybeThunkValues -> bindScopedStatics (AttrSet (flatCAttrSet scope)) frame env bindings
    ThunkValues -> bindStaticAttrs mkThunkBc frame env bindings
  fillCAttrSetValues scope (Map.map entryThunk statics)
  pure (frame, statics)

-- | The static attributes of a frame whose names are in @table@, bound in
-- it ('bindStaticAttrs'), each value as upstream's @maybeThunk@ makes it.
-- Upstream binds a recursive set's static names by slot whether it has a
-- dynamic name or not, so a variable naming one of them, whose entry is
-- not filled yet, gets a thunk of its own ('scopedValueThunk'), and then
-- takes its sibling's cell where the frame's shares say, each share a
-- pair of symbols ('Nix.Eval.Compile.siblingShares').  A share naming a
-- symbol the frame does not bind statically, which only corrupt bytecode
-- holds, fails.
bindScopedStatics :: (MonadEval m) => AttrSet -> Env -> Env -> BcBindings -> m (Map Symbol AttrEntry)
bindScopedStatics table frame env bindings@(BcBindings _ shares _ _) =
  bindStaticAttrs (scopedValueThunk table) frame env bindings >>= \bound -> foldBcShares shareEntry bound shares
  where
    shareEntry entries name target = case (Map.lookup (Symbol name) entries, Map.lookup (Symbol target) entries) of
      (Just (AttrEntry pos _), Just (AttrEntry _ cell)) -> pure (Map.insert (Symbol name) (AttrEntry pos cell) entries)
      (Nothing, _) -> corruptBytecode "share symbol" name
      (_, Nothing) -> corruptBytecode "share symbol" target

-- | 'cheapThunkBc' for a static name's value in a frame whose names are in
-- @table@, but for a variable naming one of them: that name's entry is
-- not filled yet, so the variable gets a thunk of its own, as a variable
-- whose slot is empty does upstream.  'cheapThunkBc' would look the name
-- up now, and a lookup passes over an entry not filled yet to an
-- enclosing name table, which may bind the same name.
scopedValueThunk :: (MonadEval m) => AttrSet -> Env -> Word32 -> m Thunk
scopedValueThunk table frame valBcIdx =
  cbcOpcode valBcIdx >>= \case
    OpVar ->
      cbcArg1 valBcIdx >>= \sym ->
        attrSetMemberSymbol (Symbol sym) table >>= \case
          True -> mkThunkBc frame valBcIdx
          False -> cheapThunkBc frame valBcIdx
    _ -> cheapThunkBc frame valBcIdx

-- | The positional frame of a recursive attr set or a @let@ in @env@: the
-- frame's env and the cells in its slots, in declaration order, tied by
-- 'tieFrame' as upstream's @ExprAttrs::eval@ and @ExprLet::eval@ tie
-- @env2@, their values made as @values@ says ('buildBcSlotThunks').  A
-- variable naming one of the frame's own slots then takes that slot's
-- cell where the frame's shares say, each a pair of slots
-- ('Nix.Eval.Compile.siblingShares'), and the cells answered are read
-- back from the slots; a frame with none reads no more than their count.
-- Its parent is @env@ as @captureInfo@ trims it
-- ('buildBcCaptureEnv'), whose with-scopes the frame sees, and it has the
-- slots its 'PositionalFrame' layout counts.
{-# INLINE tieBindingFrame #-}
tieBindingFrame :: (MonadEval m) => FrameKind -> FrameValues -> Env -> BcBindings -> BcCaptureInfo -> Int -> m (Env, [Thunk])
tieBindingFrame kind values env bindings@(BcBindings _ shares _ _) captureInfo slotCount = do
  parent <- buildBcCaptureEnv (frameCaptureSite kind) env captureInfo
  (frame, cells) <- tieFrame (frameSite kind) (Just parent) slotCount (\frame -> buildBcSlotThunks values frame env bindings)
  case shares of
    BcShares 0 _ -> pure (frame, cells)
    _ -> do
      foldBcShares (\() slot target -> shareSlot (corruptBytecode "share slot") frame slot target) () shares
      (frame,) <$> envSlotCells frame slotCount

-- | Evaluate a recursive attr set from bytecode bindings, its static
-- values made as @values@ says.
evalBcRecAttrs :: (MonadEval m) => FrameValues -> Env -> BcBindings -> BcCaptureInfo -> m NixValue
evalBcRecAttrs values env bindings captureInfo = case frameLayout bindings of
  PositionalFrame slotCount -> do
    -- Positional path: slots for variable lookup, CAttrSet for return.
    (_, thunkList) <- tieBindingFrame RecFrame values env bindings captureInfo slotCount
    staticAttrsFromSlots bindings thunkList >>= fmap VAttrs . attrSetFromSymbolEntries SetRec
  ScopedFrame -> do
    -- Dynamic-key path: each dynamic name and value is evaluated in a
    -- frame whose name table holds the static names, as upstream's are
    -- in env2, so a dynamic name sees its static siblings and the
    -- enclosing variables, and no other dynamic name.  The table is
    -- filled by then, so a dynamic value is made as upstream's
    -- @maybeThunk@ makes it (eval.cc's @ExprAttrs::eval@).
    (recEnv, statics) <- scopeBindingFrame RecFrame values env bindings captureInfo
    VAttrs <$> literalSet SetRec cheapThunkBc recEnv bindings statics

-- | Evaluate a let expression from bytecode.
evalBcLet :: (MonadEval m) => Env -> Word32 -> m NixValue
evalBcLet env bcIdx0 = do
  bindings <- countedPayload1 bcIdx0 >>= uncurry decodeBcBindings
  bodyIdx <- cbcArg2 bcIdx0
  captureOff <- cbcArg3 bcIdx0
  withBcCaptureInfo captureOff $ \captureInfo -> case frameLayout bindings of
    PositionalFrame slotCount -> do
      -- Positional path: slots for O(1) lookup.
      (letEnv, _) <- tieBindingFrame LetFrame MaybeThunkValues env bindings captureInfo slotCount
      evalBytecode letEnv bodyIdx
    ScopedFrame -> do
      -- Dynamic path: a dynamic key, which the parser refuses in a
      -- let as upstream's parser.y does, so only an AST built
      -- outside the parser reaches here.  Its dynamic names are
      -- evaluated and checked in the let env as a recursive set's
      -- are, and bind nothing: the body sees the static names alone.
      (letEnv, statics) <- scopeBindingFrame LetFrame MaybeThunkValues env bindings captureInfo
      void (bindDynamicAttrs mkThunkBc letEnv bindings statics)
      evalBytecode letEnv bodyIdx

-- | The cells of positional bytecode bindings' slots, in declaration
-- order, as upstream's @ExprAttrs::eval@ and @ExprLet::eval@ fill
-- @env2@'s (eval.cc).  A name's value is made in the frame as @values@ says:
-- under 'MaybeThunkValues' as 'frameThunkBc' makes it, a variable naming one
-- of the frame's own slots, none of which is filled yet, a thunk of its own.
-- An inherited variable is read from the enclosing env, as upstream evaluates
-- an Inherited attribute there through @maybeThunk@; that env is fully built,
-- so the variable's own cell can be shared now.
buildBcSlotThunks :: (MonadEval m) => FrameValues -> Env -> Env -> BcBindings -> m [Thunk]
buildBcSlotThunks values recEnv outerEnv bindings = reverse <$> foldBindings slotCell [] bindings
  where
    {-# INLINE slotCell #-}
    slotCell made (BcNamed _ _ valBcIdx) = (: made) <$> valueCell valBcIdx
    slotCell made (BcInherit _ _ valBcIdx) = (: made) <$> cheapThunkBc outerEnv valBcIdx
    slotCell made (BcInheritFrom fromBcIdx names) = do
      -- inherit (from) x y z; becomes one thunk per name that selects
      -- from the from-expr, each in the clause's env ('inheritFromEnv').
      fromEnv <- mkThunkBc recEnv fromBcIdx >>= inheritFromEnv
      foldBcInheritedNames (\selects inherited -> (: selects) <$> inheritedSelect fromEnv inherited) made names
    -- Unreachable for compiled code: the compiler counts every dynamic
    -- name, and frameLayout makes a positional frame only when it counts
    -- none.
    slotCell made (BcDynamic {}) = pure made
    valueCell valBcIdx = case values of
      MaybeThunkValues -> frameThunkBc recEnv valBcIdx
      ThunkValues -> mkThunkBc recEnv valBcIdx

-- | The env an @inherit (from)@ clause's selects run in: the from-value's
-- thunk at slot 0, one env for every name of the clause, as upstream's
-- @ExprAttrs::buildInheritFromEnv@ makes one env for a binding set's
-- from-values (eval.cc).
inheritFromEnv :: (MonadCData m) => Thunk -> m Env
inheritFromEnv fromThunk = newEnv EnvInheritFrom Nothing [fromThunk]

-- | The thunk that selects an @inherit (from)@ name: its select runs in
-- the clause's env ('inheritFromEnv').
inheritedSelect :: (MonadCData m) => Env -> BcInheritedName -> m Thunk
inheritedSelect fromEnv (BcInheritedName _ _ selectBcIdx) = mkThunkBc fromEnv selectBcIdx

-- | Positional attributes being matched with their slots' thunks.
data SlotWalk = SlotWalk !(Map Symbol AttrEntry) ![Thunk]

-- | The attributes of positional bytecode bindings, from the cells in
-- their slots: each under the symbol the bytecode holds for it and at its
-- binding's position.
staticAttrsFromSlots :: (MonadEval m) => BcBindings -> [Thunk] -> m (Map Symbol AttrEntry)
staticAttrsFromSlots bindings thunks = (\(SlotWalk attrs _) -> attrs) <$> foldBindings step (SlotWalk Map.empty thunks) bindings
  where
    {-# INLINE step #-}
    step (SlotWalk acc (t : ts)) (BcNamed pos sym _) =
      pure (SlotWalk (Map.insert (Symbol sym) (AttrEntry pos t) acc) ts)
    step (SlotWalk acc (t : ts)) (BcInherit pos sym _) =
      pure (SlotWalk (Map.insert (Symbol sym) (AttrEntry pos t) acc) ts)
    step walk (BcInheritFrom _ names) = foldBcInheritedNames insertName walk names
    -- Unreachable for compiled code: frameLayout makes a positional frame
    -- only when the compiler counts no dynamic name, and of as many slots
    -- as it counts static names, so there is a thunk for each.
    step walk _ = pure walk
    insertName (SlotWalk acc (t : ts)) (BcInheritedName pos sym _) =
      pure (SlotWalk (Map.insert (Symbol sym) (AttrEntry pos t) acc) ts)
    insertName walk _ = pure walk

-- | The attributes bindings name statically, which upstream's
-- @ExprAttrs::eval@ in eval.cc binds before any dynamic one: a static
-- key, an inherited variable and each name of an @inherit (from)@, each
-- under the symbol the bytecode holds for it and at its binding's
-- position.  A dynamic key is left to 'bindDynamicAttrs'.  A name bound
-- twice, which only an AST built outside the parser holds, keeps its last
-- definition, as the resolver keeps it.
--
-- How a named value becomes a thunk is the caller's decision: a
-- non-recursive set makes a constant's value and shares an in-scope
-- variable's cell at once ('cheapThunkBc'); a frame whose name table is
-- being filled does the same but for a variable naming one of the frame's
-- own names, whose entry is not filled yet ('scopedValueThunk'); and a
-- recursive set that binds @__overrides@ makes each a thunk of its own
-- ('mkThunkBc').  An inherited variable is read from @outerEnv@, the env
-- around the binding set, which is always built (for a non-recursive set
-- it is @thunkEnv@ itself).
bindStaticAttrs :: (MonadEval m) => (Env -> Word32 -> m Thunk) -> Env -> Env -> BcBindings -> m (Map Symbol AttrEntry)
bindStaticAttrs mkValueThunk thunkEnv outerEnv = foldBindings addBinding Map.empty
  where
    {-# INLINE addBinding #-}
    addBinding !bound (BcNamed pos sym valBcIdx) =
      bindAt bound pos sym <$> mkValueThunk thunkEnv valBcIdx
    addBinding !bound (BcDynamic {}) = pure bound
    addBinding !bound (BcInherit pos sym valBcIdx) =
      bindAt bound pos sym <$> cheapThunkBc outerEnv valBcIdx
    addBinding !bound (BcInheritFrom fromBcIdx names) = do
      -- inherit (from) name selects name from the from-expr.  One thunk
      -- of the from-expr serves every name, as upstream's
      -- ExprAttrs::buildInheritFromEnv in eval.cc makes one, so the
      -- from-expr is evaluated once however many names it gives.  Each
      -- name's select runs in the clause's env ('inheritFromEnv').
      fromEnv <- mkThunkBc thunkEnv fromBcIdx >>= inheritFromEnv
      let addInheritFrom !acc inherited@(BcInheritedName pos sym _) =
            bindAt acc pos sym <$> inheritedSelect fromEnv inherited
      foldBcInheritedNames addInheritFrom bound names
    bindAt bound pos sym thunk = Map.insert (Symbol sym) (AttrEntry pos thunk) bound

-- | Bind the dynamic attributes of bindings whose static ones are bound,
-- one at a time in source order, as upstream's @ExprAttrs::eval@ does
-- (eval.cc).  Each name is evaluated in @dynamicEnv@
-- ('resolveBcDynamicKey'), and binds nothing if it is null.  A name
-- already bound, static or dynamic, raises upstream's error, which says
-- where the name was defined, before the next name is evaluated.  Any
-- other name is bound at its binding's position to its value, a thunk in
-- @dynamicEnv@ made as the caller's @mkValueThunk@ makes one.
bindDynamicAttrs :: (MonadEval m) => (Env -> Word32 -> m Thunk) -> Env -> BcBindings -> Map Symbol AttrEntry -> m (Map Symbol AttrEntry)
bindDynamicAttrs mkValueThunk dynamicEnv bindings statics = foldBindings bindDynamic statics bindings
  where
    {-# INLINE bindDynamic #-}
    bindDynamic !bound (BcDynamic pos keyBcIdx valBcIdx) =
      resolveBcDynamicKey dynamicEnv keyBcIdx >>= \case
        Nothing -> pure bound
        Just sym -> case Map.lookup sym bound of
          Just earlier -> do
            definedAt <- printPos (entryPos earlier)
            throwEvalError ("dynamic attribute '" <> printSymbol (symbolText sym) <> "' already defined at " <> definedAt)
          Nothing -> (\thunk -> Map.insert sym (AttrEntry pos thunk) bound) <$> mkValueThunk dynamicEnv valBcIdx
    bindDynamic !bound _ = pure bound

-- | The name a dynamic attribute binds, its expression evaluated in
-- @env@: none for null, and any other value read as an attribute name and
-- interned ('internComputedName'), as upstream's @ExprAttrs::eval@ skips
-- a null name and reads and interns any other (eval.cc).
resolveBcDynamicKey :: (MonadEval m) => Env -> Word32 -> m (Maybe Symbol)
resolveBcDynamicKey env keyBcIdx =
  evalBytecode env keyBcIdx >>= \case
    VNull -> pure Nothing
    name -> Just <$> internComputedName name

-- | A computed attribute name, read as a string without context and
-- interned, as upstream's @getName@ and @ExprAttrs::eval@ read and intern
-- one (eval.cc).
internComputedName :: (MonadEval m) => NixValue -> m Symbol
internComputedName = attrNameOf "dynamic attribute key" >=> symbolIntern

-- | The names bindings bind statically: static keys and inherited names,
-- which 'bindStaticAttrs' binds.  They are the name table of a frame
-- whose names are not all static ('scopeBindingFrame'), in which a
-- dynamic name and value are evaluated, so they see a static sibling and
-- the enclosing variables, as upstream's see @env2@, and no dynamic name.
bcBindingStaticKeys :: (MonadEval m) => BcBindings -> m [Symbol]
bcBindingStaticKeys = foldBindings oneBinding []
  where
    {-# INLINE oneBinding #-}
    oneBinding keys (BcNamed _ sym _) = pure (Symbol sym : keys)
    oneBinding keys (BcDynamic {}) = pure keys
    oneBinding keys (BcInherit _ sym _) = pure (Symbol sym : keys)
    oneBinding keys (BcInheritFrom _ names) = foldBcInheritedNames (\acc (BcInheritedName _ sym _) -> pure (Symbol sym : acc)) keys names

-- ---------------------------------------------------------------------------
-- Variables
-- ---------------------------------------------------------------------------

-- | Evaluate a variable the resolver left to a name table ('EVar'):
-- the value 'envLookup' finds, or upstream's undefined-variable error for
-- a name no table on the parent chain binds.
evalVar :: (MonadEval m) => Env -> Symbol -> m NixValue
evalVar env name =
  envLookup name env >>= \case
    Just thunk -> force thunk
    Nothing -> throwEvalError (undefinedVariableMessage (symbolText name))

-- | Evaluate a variable only a with could supply ('EWithVar'): the name in
-- the with-scopes the env sees, innermost to outermost, or upstream's
-- undefined-variable error when none has it, as upstream's
-- @EvalState::lookupVar@ walks a @fromWith@ variable's withs (eval.cc at
-- 2.24.9).  The resolver makes an 'EWithVar' only of a name no lexical
-- binding and no global binds, so no other lookup could find it.  A
-- trimmed closure's env holds the with-scopes it was made under
-- ('BcCapturesWithScopes').
-- Supports both resolved and lazy with-scope entries ('withScopeAt').  A
-- lazy entry is forced on first lookup and replaced in place by its set
-- ('resolveWithScopeAt'), so subsequent lookups read the set directly.
evalWithVar :: (MonadEval m) => Env -> Symbol -> m NixValue
evalWithVar env name = do
  (withArr, withCount) <- envWithScopesRaw env
  evalWithVarScopes name withArr (fromIntegral withCount) 0

-- | 'evalWithVar' from the with-scope at an index of the array onward.
evalWithVarScopes :: (MonadEval m) => Symbol -> Ptr (Ptr ()) -> Int -> Int -> m NixValue
evalWithVarScopes name withArr count idx
  | idx >= count = throwEvalError (undefinedVariableMessage (symbolText name))
  | otherwise =
      withScopeAt withArr idx >>= \case
        Left scopeThunk -> do
          scopeVal <- force scopeThunk
          case scopeVal of
            VAttrs scope -> do
              resolveWithScopeAt withArr idx scope
              lookupIn scope
            _ -> throwEvalError ("'with' requires a set, got " <> typeName scopeVal)
        Right scope -> lookupIn scope
  where
    lookupIn scope =
      attrSetLookupSymbol name scope >>= \case
        Just thunk -> force thunk
        Nothing -> evalWithVarScopes name withArr count (idx + 1)

-- ---------------------------------------------------------------------------
-- Formals matching + env helpers (used by evalBcApp, applyValue, etc.)
-- ---------------------------------------------------------------------------

-- | Match a lambda's formals against an argument thunk.
matchFormals :: (MonadEval m) => Env -> EvalFormals -> Thunk -> m Env
matchFormals closureEnv (EFName _) argThunk =
  newEnv EnvCall (Just closureEnv) [argThunk]
matchFormals closureEnv (EFSet lambdaName formals allowExtra) argThunk = do
  argVal <- force argThunk
  matchFormalSet closureEnv lambdaName formals allowExtra argVal Nothing
matchFormals closureEnv (EFNamedSet lambdaName _ formals allowExtra) argThunk = do
  argVal <- force argThunk
  matchFormalSet closureEnv lambdaName formals allowExtra argVal (Just argThunk)

-- | Match destructuring set pattern formals against an attrset value, as
-- upstream's @callFunction@ in eval.cc does: the argument must be a set, then
-- each formal the set lacks must have a default, then, with no ellipsis, the
-- set may hold nothing else.
--
-- The env is a knot: a default is evaluated in it, where it may name
-- another formal or the @-pattern, and its slots hold the defaults'
-- cells ('formalCell', 'shareFormalSlots').  'tieFrame' builds it as
-- upstream's @callFunction@ builds @env2@.
matchFormalSet :: (MonadEval m) => Env -> LambdaName -> [EvalFormal] -> Bool -> NixValue -> Maybe Thunk -> m Env
matchFormalSet closureEnv lambdaName formals allowExtra argVal atThunk =
  case argVal of
    VAttrs attrs -> do
      slots <- formalSlots lambdaName attrs formals
      unless allowExtra (checkExtraKeys lambdaName formals (length [() | Given _ <- slots]) attrs)
      let atSlot = maybeToList atThunk
      (formalEnv, _) <-
        tieFrame
          EnvCallFormals
          (Just closureEnv)
          (length atSlot + length slots)
          (\frame -> (atSlot ++) <$> mapM (formalCell frame) slots)
      when (any sharesSlot slots) (shareFormalSlots formalEnv (length atSlot) slots)
      pure formalEnv
    _ ->
      callSiteError
        $ addErrorTrace "while evaluating the value passed for the lambda argument"
        $ expectedFailure "a set" argVal

-- | A failure of a call to a set pattern, with the line of context
-- upstream's @callFunction@ in eval.cc adds for the call.  Upstream adds
-- it to a non-set argument only when the call has a position, which every
-- call written in source has; this evaluator records no call positions
-- and adds it to every such failure.  Upstream adds it as well where
-- forcing the argument fails, which this does not do, so that a call that
-- succeeds installs no handler.  It adds it, and a @while calling@ line,
-- to a failure in the function's body only under @show-trace@, which this
-- evaluator does not have.
callSiteError :: (MonadEval m) => m a -> m a
callSiteError = addErrorTrace "from call site"

-- | With no ellipsis, a set holding an attribute that no formal names
-- fails the call, as upstream's @callFunction@ in eval.cc checks it:
-- no two formals share a name, so the set holds one exactly when fewer of
-- its attributes fill a formal than it holds, and only then is the set
-- walked for the first such in symbol order, as upstream walks its
-- @Bindings@.  The message names the function as its binding spells it
-- and the argument as a symbol prints ('printSymbol'), as upstream passes
-- the one as a string and the other as a symbol.
checkExtraKeys :: (MonadEval m) => LambdaName -> [EvalFormal] -> Int -> AttrSet -> m ()
checkExtraKeys lambdaName formals given attrs = do
  size <- attrSetSize attrs
  when (given /= size) $
    attrSetFindName (`notElem` map efName formals) attrs >>= mapM_ unexpected
  where
    unexpected name = callSiteError (throwEvalError ("function '" <> lambdaNameText lambdaName <> "' called with unexpected argument '" <> printSymbol (symbolText name) <> "'"))

-- | What fills a formal's slot at a call.
data FormalSlot
  = -- | The argument's attribute of the formal's name.
    Given !Thunk
  | -- | The formal's default, by its bytecode index.
    Defaulted !Word32
  | -- | The formal's default, by its bytecode index: a variable naming a
    -- slot of the call's frame whose cell the formal's slot shares
    -- ('SharedDefault').
    DefaultedShared !Word32

-- | Whether a formal's slot shares another slot's cell.
sharesSlot :: FormalSlot -> Bool
sharesSlot (DefaultedShared _) = True
sharesSlot _ = False

-- | The cell a formal's slot holds before a default naming a slot takes
-- that slot's cell ('shareFormalSlots'), as upstream's @callFunction@
-- makes it (eval.cc): the argument's attribute, or a default as
-- 'frameThunkBc' makes it in the call's frame, a variable naming a slot of
-- the frame, which is not filled yet, a thunk of its own.
formalCell :: (MonadEval m) => Env -> FormalSlot -> m Thunk
formalCell _ (Given thunk) = pure thunk
formalCell frame (Defaulted defBcIdx) = frameThunkBc frame defBcIdx
formalCell frame (DefaultedShared defBcIdx) = frameThunkBc frame defBcIdx

-- | Give each formal's slot whose default shares a slot's cell
-- ('DefaultedShared') the cell that slot holds once filled, the first
-- formal in the slot given.  The default is a variable naming the slot
-- (its code's second argument).  That slot shares a cell in turn when its
-- formal's default does and the argument leaves the formal out, so each
-- default follows the chain to the first slot that holds a cell of its
-- own, which upstream fills before every slot that shares it.  A chain
-- the compiler writes only names slots filled before its own, so it
-- follows each formal's default at most once and ends; one that goes on,
-- a cycle, or names a slot past the frame's, is corrupt bytecode.
shareFormalSlots :: (MonadEval m) => Env -> Int -> [FormalSlot] -> m ()
shareFormalSlots frame firstFormal slots =
  forM_ (zip [fromIntegral firstFormal ..] slots) $ \case
    (slot, DefaultedShared defBcIdx) ->
      cbcArg2 defBcIdx >>= ownCellSlot slots >>= \end -> case slotAt end of
        DefaultedShared _ : _ -> corruptBytecode "shared default slot" end
        _ -> shareSlot (corruptBytecode "share slot") frame slot end
    _ -> pure ()
  where
    -- The formals' slots from a slot of the frame on: none for the
    -- @-pattern's slot or for one past the frame's.
    slotAt target
      | fromIntegral target < firstFormal = []
      | otherwise = drop (fromIntegral target - firstFormal) slots
    -- The walk takes one of @unwalked@'s formals for each default it
    -- follows and stops when none is left, having followed as many
    -- defaults as there are formals, which only a cycle makes it do; it
    -- then ends on a slot whose default still shares.  It answers a slot
    -- on every path rather than failing on the cycle itself: a path that
    -- fails would keep GHC from passing and answering the slot unboxed,
    -- and box it on every call.
    ownCellSlot unwalked target = case (slotAt target, unwalked) of
      (DefaultedShared defBcIdx : _, _ : rest) -> cbcArg2 defBcIdx >>= ownCellSlot rest
      _ -> pure target

-- | What fills each formal's slot, found by the formal's symbol.  A
-- formal with neither an attribute nor a default fails the call, the
-- first such in symbol order named, as upstream sorts a set pattern's
-- formals by symbol (@validateFormals@ in parser-state.hh) and stops at
-- the first it cannot fill (@callFunction@ in eval.cc).
formalSlots :: (MonadEval m) => LambdaName -> AttrSet -> [EvalFormal] -> m [FormalSlot]
formalSlots lambdaName attrs formals = do
  slots <- mapM formalSlot formals
  case partitionEithers slots of
    ([], found) -> pure found
    (name : names, _) ->
      callSiteError (throwEvalError ("function '" <> lambdaNameText lambdaName <> "' called without required argument '" <> printSymbol (symbolText (minimum (name :| names))) <> "'"))
  where
    formalSlot (EvalFormal name def) =
      attrSetLookupSymbol name attrs >>= \case
        Just thunk -> pure (Right (Given thunk))
        Nothing -> pure $! defaultedSlot name def
    defaultedSlot name = \case
      NoDefault -> Left name
      Default defBcIdx -> Right (Defaulted defBcIdx)
      SharedDefault defBcIdx -> Right (DefaultedShared defBcIdx)

-- ---------------------------------------------------------------------------
-- Builtin definitions
-- ---------------------------------------------------------------------------

-- | How a builtin takes the argument at one position.
data ArgMode
  = -- | Forced as it is applied, for a primop that forces it before
    -- anything it does can be observed.  That matches upstream once the
    -- primop has every argument; a partial application forces it sooner
    -- than upstream, which forces nothing before the last argument.
    ForceOnApply
  | -- | Held as a thunk, for a primop that forces it later than that:
    -- after it checks an earlier argument, after a later argument, around
    -- a catch, or only on some paths.  The builtin forces it where
    -- upstream does.
    ForceOnUse
  deriving (Eq, Show)

-- | A builtin's shape: how it takes each argument, first to last (the
-- arity is the length), and its implementation over the arguments taken.
-- Each shape below defines both together, and its implementation type
-- marks a 'ForceOnUse' position with a 'BuiltinArg' parameter, so the
-- modes 'applyBuiltin' takes arguments by are the ones the implementation
-- matches.
data BuiltinDef m = BuiltinDef
  { bdArgs :: !(NonEmpty ArgMode),
    bdApply :: [BuiltinArg] -> m NixValue
  }

-- | The value of an argument a builtin has taken, forcing it if it was
-- held unforced.
argValue :: (MonadEval m) => BuiltinArg -> m NixValue
argValue (ArgForced val) = pure val
argValue (ArgDeferred thunk) = force thunk

-- | An argument a builtin has taken, as a thunk to share, such as the
-- function every element of @map@'s result applies.
argAsThunk :: (MonadCData m) => BuiltinArg -> m Thunk
argAsThunk (ArgForced val) = evaluated val
argAsThunk (ArgDeferred thunk) = pure thunk

-- | A set, at a site, of attributes defined nowhere in source, each holding a value
-- already computed.
valuesSet :: (MonadCData m) => SetSite -> Map Text NixValue -> m NixValue
valuesSet site values = VAttrs <$> (traverse evaluated values >>= attrSetFromMap site)

-- | A list of values already computed.
valuesList :: (MonadCData m) => [NixValue] -> m NixValue
valuesList values = mapM evaluated values >>= thunksList

-- | A list of thunks.
thunksList :: (MonadCData m) => [Thunk] -> m NixValue
thunksList thunks = VList <$> clistFromThunks (map unThunk thunks)

-- | A list's elements, as thunks.
listThunks :: (MonadCData m) => CList -> m [Thunk]
listThunks cl = map Thunk <$> clistThunks cl

-- | The failure of a builtin run with arguments that do not fit its
-- shape, which 'applyBuiltin' never builds: it runs a builtin at its last
-- position, and every caller supplies a 'ForceOnApply' position as
-- 'ArgForced'.
arityError :: (MonadEval m) => Builtin -> m a
arityError builtin = throwEvalError ("builtins." <> builtinName builtin <> ": internal arity error")

-- | One argument, forced as it is applied.
forced1 :: (MonadEval m) => (NixValue -> m NixValue) -> Builtin -> BuiltinDef m
forced1 f builtin = BuiltinDef (ForceOnApply :| []) $ \case
  [ArgForced a] -> f a
  _ -> arityError builtin

-- | Two arguments, each forced as it is applied.
forced2 :: (MonadEval m) => (NixValue -> NixValue -> m NixValue) -> Builtin -> BuiltinDef m
forced2 f builtin = BuiltinDef (ForceOnApply :| [ForceOnApply]) $ \case
  [ArgForced a, ArgForced b] -> f a b
  _ -> arityError builtin

-- | One argument, held unforced.
onUse1 :: (MonadEval m) => (BuiltinArg -> m NixValue) -> Builtin -> BuiltinDef m
onUse1 f builtin = BuiltinDef (ForceOnUse :| []) $ \case
  [a] -> f a
  _ -> arityError builtin

-- | Two arguments, both held unforced.
onUse2 :: (MonadEval m) => (BuiltinArg -> BuiltinArg -> m NixValue) -> Builtin -> BuiltinDef m
onUse2 f builtin = BuiltinDef (ForceOnUse :| [ForceOnUse]) $ \case
  [a, b] -> f a b
  _ -> arityError builtin

-- | Two arguments, the first held unforced and the second forced as it
-- is applied.
onUseFirst :: (MonadEval m) => (BuiltinArg -> NixValue -> m NixValue) -> Builtin -> BuiltinDef m
onUseFirst f builtin = BuiltinDef (ForceOnUse :| [ForceOnApply]) $ \case
  [a, ArgForced b] -> f a b
  _ -> arityError builtin

-- | Two arguments, the first forced as it is applied and the second held
-- unforced.
onUseSecond :: (MonadEval m) => (NixValue -> BuiltinArg -> m NixValue) -> Builtin -> BuiltinDef m
onUseSecond f builtin = BuiltinDef (ForceOnApply :| [ForceOnUse]) $ \case
  [ArgForced a, b] -> f a b
  _ -> arityError builtin

-- | Three arguments, the first forced as it is applied and the others held
-- unforced.
onUseLastTwo :: (MonadEval m) => (NixValue -> BuiltinArg -> BuiltinArg -> m NixValue) -> Builtin -> BuiltinDef m
onUseLastTwo f builtin = BuiltinDef (ForceOnApply :| [ForceOnUse, ForceOnUse]) $ \case
  [ArgForced a, b, c] -> f a b c
  _ -> arityError builtin

-- | Every builtin's shape and implementation.  Upstream binds
-- @fetchMercurial@ and nova-nix has no implementation for it: it takes
-- one argument, forced, and fails naming itself an unknown builtin.
builtinDef :: (MonadEval m) => Builtin -> BuiltinDef m
builtinDef builtin = shape builtin
  where
    shape = case builtin of
      -- Type checking (arity 1)
      Builtin.TypeOf -> forced1 (pure . mkStr . typeOfValue)
      Builtin.IsNull -> forced1 (pure . VBool . isNullVal)
      Builtin.IsInt -> forced1 (pure . VBool . isIntVal)
      Builtin.IsFloat -> forced1 (pure . VBool . isFloatVal)
      Builtin.IsBool -> forced1 (pure . VBool . isBoolVal)
      Builtin.IsString -> forced1 (pure . VBool . isStringVal)
      Builtin.IsList -> forced1 (pure . VBool . isListVal)
      Builtin.IsAttrs -> forced1 (pure . VBool . isAttrsVal)
      Builtin.IsFunction -> forced1 (pure . VBool . isFunctionVal)
      -- List operations (arity 1)
      Builtin.Length -> forced1 builtinLength
      Builtin.Head -> forced1 builtinHead
      Builtin.Tail -> forced1 builtinTail
      -- String operations (arity 1)
      Builtin.ToString -> forced1 (fmap (uncurry VStr) . coerceToStringPermissive)
      Builtin.StringLength -> forced1 builtinStringLength
      -- Control (arity 1)
      Builtin.Throw -> forced1 builtinThrow
      Builtin.Abort -> forced1 builtinAbort
      -- Attr set operations (arity 1)
      Builtin.AttrNames -> forced1 builtinAttrNames
      Builtin.AttrValues -> forced1 builtinAttrValues
      Builtin.ListToAttrs -> forced1 builtinListToAttrs
      -- Attr set operations (arity 2)
      Builtin.HasAttr -> onUseSecond builtinHasAttr
      Builtin.GetAttr -> onUseSecond builtinGetAttr
      Builtin.RemoveAttrs -> onUseSecond builtinRemoveAttrs
      Builtin.IntersectAttrs -> onUseSecond builtinIntersectAttrs
      Builtin.CatAttrs -> onUseSecond builtinCatAttrs
      -- List higher-order (arity 2)
      Builtin.Map -> onUseFirst builtinMap
      Builtin.Filter -> onUseFirst builtinFilter
      Builtin.GenList -> onUseFirst builtinGenList
      Builtin.Sort -> onUseFirst builtinSort
      Builtin.ConcatMap -> onUseSecond builtinConcatMap
      Builtin.Any -> onUseSecond builtinAny
      Builtin.All -> onUseSecond builtinAll
      Builtin.Elem -> onUseFirst builtinElem
      Builtin.ElemAt -> onUseFirst builtinElemAt
      Builtin.Partition -> onUseSecond builtinPartition
      Builtin.GroupBy -> onUseSecond builtinGroupBy
      -- String operations (arity 2)
      Builtin.ConcatStringsSep -> onUseSecond builtinConcatStringsSep
      -- Arity 3
      Builtin.FoldlStrict -> onUseLastTwo builtinFoldl
      Builtin.Substring -> onUseLastTwo builtinSubstring
      -- Numeric
      Builtin.IsPath -> forced1 (pure . VBool . isPathVal)
      Builtin.Ceil -> forced1 builtinCeil
      Builtin.Floor -> forced1 builtinFloor
      Builtin.Seq -> forced2 builtinSeq
      Builtin.Trace -> onUseSecond builtinTrace
      Builtin.Warn -> onUseSecond builtinWarn
      Builtin.UnsafeDiscardStringContext -> forced1 builtinDiscardContext
      Builtin.UnsafeDiscardOutputDependency -> forced1 builtinDiscardOutputDep
      Builtin.AddDrvOutputDependencies -> forced1 builtinAddDrvOutputDeps
      -- String context introspection
      Builtin.HasContext -> forced1 builtinHasContext
      Builtin.GetContext -> forced1 builtinGetContext
      Builtin.AppendContext -> onUseSecond builtinAppendContext
      Builtin.BaseNameOf -> forced1 builtinBaseNameOf
      Builtin.DirOf -> forced1 builtinDirOf
      Builtin.ConcatLists -> forced1 builtinConcatLists
      Builtin.LessThan -> forced2 builtinLessThan
      -- Arithmetic + bitwise
      Builtin.Add -> forced2 primAdd
      Builtin.Sub -> forced2 primSub
      Builtin.Mul -> forced2 primMul
      Builtin.Div -> forced2 primDiv
      Builtin.BitAnd -> onUseSecond (bitwise (.&.))
      Builtin.BitOr -> onUseSecond (bitwise (.|.))
      Builtin.BitXor -> onUseSecond (bitwise xor)
      -- Attr set higher-order
      Builtin.MapAttrs -> onUseFirst builtinMapAttrs
      Builtin.FunctionArgs -> forced1 builtinFunctionArgs
      Builtin.ZipAttrsWith -> onUseSecond builtinZipAttrsWith
      -- String manipulation
      Builtin.Match -> onUseSecond builtinMatch
      Builtin.Split -> onUseSecond builtinSplit
      Builtin.ReplaceStrings -> onUseLastTwo builtinReplaceStrings
      Builtin.CompareVersions -> onUseSecond builtinCompareVersions
      Builtin.SplitVersion -> forced1 builtinSplitVersion
      Builtin.ParseDrvName -> forced1 builtinParseDrvName
      -- Serialization + hashing
      Builtin.ToJSON -> forced1 builtinToJSON
      Builtin.FromJSON -> forced1 builtinFromJSON
      Builtin.HashString -> onUseSecond builtinHashString
      -- Error handling + sequencing
      Builtin.TryEval -> onUse1 builtinTryEval
      Builtin.DeepSeq -> onUseSecond builtinDeepSeq
      -- Graph traversal
      Builtin.GenericClosure -> forced1 builtinGenericClosure
      -- IO builtins (delegate to MonadEval methods)
      Builtin.Import -> forced1 builtinImport
      Builtin.ReadFile -> forced1 builtinReadFile
      Builtin.PathExists -> forced1 builtinPathExists
      Builtin.ReadDir -> forced1 builtinReadDir
      Builtin.GetEnv -> forced1 builtinGetEnv
      Builtin.ToPath -> forced1 builtinToPath
      -- Store path operations
      Builtin.Placeholder -> forced1 builtinPlaceholder
      Builtin.StorePath -> onUse1 builtinStorePath
      Builtin.FindFile -> onUseSecond builtinFindFile
      Builtin.ToFile -> onUseSecond builtinToFile
      Builtin.ScopedImport -> onUseFirst builtinScopedImport
      -- Network fetchers
      Builtin.Fetchurl -> forced1 builtinFetchurl
      Builtin.FetchTarball -> forced1 builtinFetchTarball
      Builtin.FetchGit -> forced1 builtinFetchGit
      -- Derivation construction.  The lazy 'derivation' wrapper is not a
      -- primop: it is a Nix lambda over this one, bound as a constant by
      -- "Nix.Builtins".
      Builtin.DerivationStrict -> forced1 builtinDerivationStrict
      -- Error context
      Builtin.AddErrorContext -> onUse2 builtinAddErrorContext
      -- Attr position
      Builtin.UnsafeGetAttrPos -> onUseSecond builtinUnsafeGetAttrPos
      -- Debugging
      Builtin.TraceVerbose -> onUse2 builtinTraceVerbose
      Builtin.Break -> forced1 pure
      -- IO: file hashing + type detection
      Builtin.HashFile -> onUseSecond builtinHashFile
      Builtin.ReadFileType -> forced1 builtinReadFileType
      -- Serialization
      Builtin.FromTOML -> forced1 builtinFromTOML
      -- Hash conversion
      Builtin.ConvertHash -> forced1 builtinConvertHash
      -- XML serialization
      Builtin.ToXML -> forced1 builtinToXML
      -- Source filtering + path import
      Builtin.Path -> forced1 builtinPath
      Builtin.FilterSource -> onUseFirst builtinFilterSource
      Builtin.FetchMercurial -> forced1 (const (throwEvalError ("unknown builtin '" <> builtinName builtin <> "'")))

-- | The builtins @builtins@ lists: every one but @fetchMercurial@, which
-- it leaves out so that a feature test still sees it is missing.
listedBuiltins :: [Builtin]
listedBuiltins = filter (/= Builtin.FetchMercurial) [minBound .. maxBound]

-- ---------------------------------------------------------------------------
-- Builtin dispatch (partial application via accumulated args)
-- ---------------------------------------------------------------------------

-- | Apply a builtin, holding the arguments it has taken, to one more.
-- The caller supplies the argument as the builtin's mode for its
-- position asks: forced, or as a thunk.  The last position runs the
-- builtin; an earlier one makes the partial application.  Inlined into
-- each application site, which measurably cuts allocation on this path.
--
-- The argument is held evaluated: a value, or the cell of a thunk the
-- caller's action made as the builtin takes it, as upstream makes a call's
-- argument thunk when it makes the call.  The cell exists before this
-- runs, and the bang holds the argument as a constructor rather than as a
-- thunk that would build one.
applyBuiltin :: (MonadEval m) => Builtin -> [BuiltinArg] -> (ArgMode -> m BuiltinArg) -> m NixValue
applyBuiltin builtin taken takeArg = case bdArgs definition of
  first :| later -> nextPosition taken first later
  where
    definition = builtinDef builtin
    nextPosition [] mode later = do
      !arg <- takeArg mode
      let held = taken ++ [arg]
      if null later
        then bdApply definition held
        else pure (VBuiltin builtin held)
    nextPosition (_ : before) _ (mode : later) = nextPosition before mode later
    nextPosition (_ : _) _ [] = arityError builtin
{-# INLINE applyBuiltin #-}

-- | Apply a function value (lambda or builtin) to one argument, in a call
-- frame of its own.  Used by higher-order builtins to invoke
-- user-supplied functions.
applyValue :: (MonadEval m) => NixValue -> NixValue -> m NixValue
applyValue fun arg = withCallFrame (applyForced fun arg)

-- | 'applyValue' inside the caller's frame.
applyForced :: (MonadEval m) => NixValue -> NixValue -> m NixValue
applyForced (VLambda closureEnv formals bodyBcIdx) arg = do
  extEnv <- evaluated arg >>= matchFormals closureEnv formals
  evalBytecode extEnv bodyBcIdx
applyForced (VBuiltin builtin taken) arg =
  applyBuiltin builtin taken (const (pure (ArgForced arg)))
applyForced funcVal@(VAttrs attrs) arg = callFunctor funcVal attrs (`applyForced` arg)
applyForced other _ = notAFunction other

-- | Apply a function value to an UNFORCED thunk argument.  Upstream's
-- higher-order list builtins (filter, any, all, groupBy, foldl') pass
-- elements as unforced values, so an element the function never inspects
-- may contain a throw without failing the call.  A
-- builtin callee takes it by its mode for the position, and set-pattern
-- formals force on destructuring exactly as upstream does.
applyValueLazy :: (MonadEval m) => NixValue -> Thunk -> m NixValue
applyValueLazy fun argThunk = withCallFrame (applyUnforced fun argThunk)

-- | 'applyValueLazy' inside the caller's frame.
applyUnforced :: (MonadEval m) => NixValue -> Thunk -> m NixValue
applyUnforced (VLambda closureEnv formals bodyBcIdx) argThunk = do
  extEnv <- matchFormals closureEnv formals argThunk
  evalBytecode extEnv bodyBcIdx
applyUnforced (VBuiltin builtin taken) argThunk = applyBuiltin builtin taken $ \case
  ForceOnApply -> ArgForced <$> force argThunk
  ForceOnUse -> pure (ArgDeferred argThunk)
applyUnforced funcVal@(VAttrs attrs) argThunk = callFunctor funcVal attrs (`applyUnforced` argThunk)
applyUnforced other argThunk = do
  val <- force argThunk
  applyForced other val

-- | Call a set with @__functor@, given how to apply what the functor gives
-- to the argument.  Upstream applies the functor to the set and to the
-- argument in one nested call, so both bodies run a frame below the
-- caller's: 'applyValue' opens that frame for the self application,
-- and the frame opened here holds the application to the argument.  A set
-- without @__functor@ is not a function.
callFunctor :: (MonadEval m) => NixValue -> AttrSet -> (NixValue -> m NixValue) -> m NixValue
callFunctor funcVal attrs applyPartial =
  attrSetLookup "__functor" attrs >>= \case
    Just functorThunk -> do
      functor <- force functorThunk
      partiallyApplied <- applyValue functor funcVal
      withCallFrame (applyPartial partiallyApplied)
    Nothing -> notAFunction funcVal

-- | Apply a function value to an argument a builtin has taken: forced,
-- or as the thunk it was held as.
applyArg :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
applyArg fun (ArgForced val) = applyValue fun val
applyArg fun (ArgDeferred thunk) = applyValueLazy fun thunk

-- ---------------------------------------------------------------------------
-- Builtin implementations - type checking
-- ---------------------------------------------------------------------------

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
builtinLength (VList cl) = VInt . fromIntegral <$> clistLen cl
builtinLength other = expectedFailure "a list" other

-- | 2.24.9's @prim_head@ is its @elemAt@ at index 0, so an empty list
-- fails as an index past the end does.  From 2.26.0 upstream says
-- @'builtins.head' called on an empty list@ instead.
builtinHead :: (MonadEval m) => NixValue -> m NixValue
builtinHead (VList cl) = listElemAt cl 0
builtinHead other = expectedFailure "a list" other

-- | The result shares the argument's C array where upstream's
-- @prim_tail@ copies the elements after the first: a copy costs time and
-- arena memory in the list's length on every call, and sharing cannot be
-- observed because a list is never written after construction.  The
-- empty-list message is 2.24.9's; from 2.26.0 upstream says
-- @'builtins.tail' called on an empty list@.
builtinTail :: (MonadEval m) => NixValue -> m NixValue
builtinTail (VList cl) = do
  len <- clistLen cl
  if len == 0
    then throwEvalError "'tail' called on an empty list"
    else VList <$> clistDrop 1 cl
builtinTail other = expectedFailure "a list" other

-- | The element at an index, failing as the @elemAt@ helper behind
-- @prim_elemAt@ and @prim_head@ does (primops.cc at 2.24.9).  That helper
-- takes the index as a C @int@, so an index past 32 bits wraps there
-- before its bounds check; the whole index is checked here, as 2.33.2
-- checks it.  2.26.0 replaced the message with
-- @'builtins.elemAt' called with index N on a list of size M@.
listElemAt :: (MonadEval m) => CList -> Int64 -> m NixValue
listElemAt cl idx =
  clistIndex cl (fromIntegral idx)
    >>= maybe (throwEvalError ("list index " <> T.pack (show idx) <> " is out of bounds")) (force . Thunk)

-- ---------------------------------------------------------------------------
-- Builtin implementations - string (arity 1)
-- ---------------------------------------------------------------------------

-- | Byte length, as upstream: stringLength of a 2-byte character is 2, not 1.
builtinStringLength :: (MonadEval m) => NixValue -> m NixValue
builtinStringLength value = VInt . fromIntegral . BS.length . fst <$> coerceToStringInterp value

-- ---------------------------------------------------------------------------
-- Builtin implementations - control
-- ---------------------------------------------------------------------------

-- | @throw@ and @abort@ coerce their message as string interpolation
-- does, as upstream's coerce theirs: a path, or a set with @__toString@
-- or @outPath@, is a message too, and anything else fails to coerce.
-- The error channel is Text and display-only (tryEval discards the
-- message), so the messages decode lossily.
builtinThrow :: (MonadEval m) => NixValue -> m NixValue
builtinThrow val = do
  (msg, _) <- coerceToStringInterp val
  throwCatchableError (bytesToTextLossy msg)

-- | Upstream's @prim_abort@ in primops.cc words the message it was
-- given.
builtinAbort :: (MonadEval m) => NixValue -> m NixValue
builtinAbort val = do
  (msg, _) <- coerceToStringInterp val
  abortEvaluation ("evaluation aborted with the following error message: '" <> bytesToTextLossy msg <> "'")

-- | @builtins.addErrorContext message value@: the value, forced, and when
-- forcing it fails, the failure with the message as a line of its trace.
-- Upstream's @prim_addErrorContext@ (primops.cc at 2.36.0) forces the value
-- inside a @catch@ and coerces the message only there, so both are taken
-- unforced: a message is never forced when the value evaluates.  A message
-- that fails to coerce leaves the value's failure to escape, a throw still
-- catchable by @builtins.tryEval@, with a line naming the message's own
-- failure; 2.35.4 and earlier let it escape in place of the value's.  The
-- message coerces as a string, a path or a set with @__toString@ or
-- @outPath@, with no store copy (@coerceMore@ and @copyToStore@ false).
builtinAddErrorContext :: (MonadEval m) => BuiltinArg -> BuiltinArg -> m NixValue
builtinAddErrorContext message value = addEvaluatedTrace failedLine contextLine (argValue value)
  where
    contextLine = do
      (bytes, _) <- argValue message >>= coerceToString False force applyValue coercePathVerbatim
      pure $! bytesToTextLossy bytes
    failedLine reason = "while evaluating the error message passed to builtins.addErrorContext, another error occurred: " <> reason

-- ---------------------------------------------------------------------------
-- Builtin implementations - attr set (arity 1)
-- ---------------------------------------------------------------------------

builtinAttrNames :: (MonadEval m) => NixValue -> m NixValue
builtinAttrNames (VAttrs attrs) =
  -- Nix returns attribute names lexicographically sorted; a set walks in
  -- interned-symbol order, so sort here (consistent with builtins.attrValues,
  -- which sorts via attrSetElems).
  attrSetKeys attrs >>= valuesList . map mkStr . sort
builtinAttrNames other =
  throwEvalError ("builtins.attrNames: expected a set, got " <> typeName other)

builtinAttrValues :: (MonadEval m) => NixValue -> m NixValue
builtinAttrValues (VAttrs attrs) = attrSetElems attrs >>= thunksList
builtinAttrValues other =
  throwEvalError ("builtins.attrValues: expected a set, got " <> typeName other)

-- | Upstream's @prim_listToAttrs@ (primops.cc at 2.24.9): the
-- elements are read first to last, and the first element with a name
-- gives its attribute, so a later element with the same name is read only
-- as far as its name.  Each failure carries the line of context upstream
-- gives it.
builtinListToAttrs :: (MonadEval m) => NixValue -> m NixValue
builtinListToAttrs (VList cl) =
  listThunks cl >>= foldM addListToAttrsElement Map.empty >>= fmap VAttrs . attrSetFromSymbolEntries SetListToAttrs
builtinListToAttrs other =
  addErrorTrace "while evaluating the argument passed to builtins.listToAttrs" (expectedFailure "a list" other)

-- | Add one element of @listToAttrs@'s list to the attributes the elements
-- before it made, whose names are the names seen.  The element is forced
-- to a set and its @name@ read and interned, before the next element is
-- read, as upstream interns it (@prim_listToAttrs@ in primops.cc), so
-- names new to the symbol table take their place in list order.  A name
-- already seen keeps its attribute, and a new one takes the element's
-- @value@, unforced, at that attribute's position, as upstream inserts it.
addListToAttrsElement :: (MonadEval m) => Map Symbol AttrEntry -> Thunk -> m (Map Symbol AttrEntry)
addListToAttrsElement !made thunk = do
  attrs <-
    force thunk >>= \case
      VAttrs attrs -> pure attrs
      other -> addErrorTrace "while evaluating an element of the list passed to builtins.listToAttrs" (expectedFailure "a set" other)
  nameThunk <- attrSetLookup "name" attrs >>= maybe (missingPairAttr "name") pure
  name <-
    addErrorTrace "while evaluating the `name` attribute of an element of the list passed to builtins.listToAttrs" $
      force nameThunk >>= attrNameOf "builtins.listToAttrs: attribute name"
  sym <- symbolIntern name
  let newEntry = attrSetLookupEntry "value" attrs >>= maybe (missingPairAttr "value") pure
  Map.alterF (maybe (Just <$> newEntry) (pure . Just)) sym made

-- | A @listToAttrs@ element without the attribute, refused as upstream's
-- @getAttr@ (primops.cc at 2.24.9, @EvalState::getAttr@ in eval.cc at
-- 2.36.0) refuses it: the message, and a line of context naming the pair.
missingPairAttr :: (MonadEval m) => Text -> m a
missingPairAttr key = addErrorTrace "in a {name=...; value=...;} pair" (throwEvalError ("attribute '" <> key <> "' missing"))

-- ---------------------------------------------------------------------------
-- Builtin implementations - attr set (arity 2)
-- ---------------------------------------------------------------------------

-- | Upstream's @prim_hasAttr@ (primops.cc): the name, read as an attribute
-- name, then the set.
builtinHasAttr :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinHasAttr nameVal setArg = do
  name <- attrNameOf "builtins.hasAttr" nameVal
  argValue setArg >>= \case
    VAttrs attrs -> VBool <$> attrSetMember name attrs
    other -> throwEvalError ("builtins.hasAttr: expected a set, got " <> typeName other)

-- | Upstream's @prim_getAttr@ (primops.cc): the name, read as an attribute
-- name, then the set.
builtinGetAttr :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinGetAttr nameVal setArg = do
  name <- attrNameOf "builtins.getAttr" nameVal
  argValue setArg >>= \case
    VAttrs attrs ->
      attrSetLookup name attrs >>= \case
        Just thunk -> force thunk
        Nothing -> throwEvalError ("builtins.getAttr: attribute '" <> name <> "' not found")
    other -> throwEvalError ("builtins.getAttr: expected a set, got " <> typeName other)

-- | Where an attribute was defined, as upstream's @prim_unsafeGetAttrPos@
-- gives it (primops.cc at 2.24.9): its 'positionValue', and null for a
-- missing attribute.  The name is read as upstream's @forceStringNoCtx@
-- reads it, and checked before the set is forced.
builtinUnsafeGetAttrPos :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinUnsafeGetAttrPos nameVal setArg = do
  nameBytes <- structuredStringValue RefuseContext nameVal
  setVal <- argValue setArg
  attrs <- case setVal of
    VAttrs attrs -> pure attrs
    other -> expectedFailure "a set" other
  name <- decodedText "builtins.unsafeGetAttrPos" nameBytes
  attrSetLookupEntry name attrs >>= maybe (pure VNull) (positionValue . entryPos)

-- | The value of a position, as upstream's @EvalState::mkPos@ makes it
-- (eval.cc at 2.24.9): @{ column; file; line; }@ for one in a file, the
-- file a string, and null for one in text with no file behind it and for
-- 'Nix.Eval.SourcePos.noPos', the position of what no source defines.  The
-- set's own attributes are at 'Nix.Eval.SourcePos.noPos', as @mkPos@
-- allocates them with none.
positionValue :: (MonadCData m) => PosIdx -> m NixValue
positionValue idx =
  lookupPos idx >>= \case
    Just (PosInFile file line column) ->
      valuesSet SetPosition $
        Map.fromList
          [ ("column", VInt (fromIntegral column)),
            ("file", mkStr file),
            ("line", VInt (fromIntegral line))
          ]
    Just (PosInText _ _) -> pure VNull
    Nothing -> pure VNull

-- | The set, checked before the list is forced, then each name read as an
-- attribute name, first to last, as upstream Nix reads them.
builtinRemoveAttrs :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinRemoveAttrs (VAttrs attrs) namesArg =
  argValue namesArg >>= \case
    VList cl -> do
      keys <- listThunks cl >>= mapM (force >=> attrNameOf "builtins.removeAttrs")
      VAttrs <$> attrSetRemoveKeys SetRemoveAttrs keys attrs
    other -> throwEvalError ("builtins.removeAttrs: expected a list, got " <> typeName other)
builtinRemoveAttrs other _ = expectedFailure "a set" other

-- | Upstream's @prim_intersectAttrs@ (primops.cc): it walks the smaller
-- set, the second when the two are one size, and finds each name it meets
-- in the other.  Each attribute it keeps is the second set's, position
-- included.  The result is sized for the smaller set, so two sets with
-- attributes and no name in common make an empty set of their own.  The
-- first is checked before the second is forced.
builtinIntersectAttrs :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinIntersectAttrs (VAttrs left) rightArg =
  argValue rightArg >>= \case
    VAttrs right -> intersectSets left right
    other -> throwEvalError ("builtins.intersectAttrs: expected a set, got " <> typeName other)
builtinIntersectAttrs other _ = expectedFailure "a set" other

-- | The attributes of the second set whose names the first has.
intersectSets :: (MonadEval m) => AttrSet -> AttrSet -> m NixValue
intersectSets left right = do
  sizeLeft <- attrSetSize left
  sizeRight <- attrSetSize right
  let shared
        | sizeLeft < sizeRight = attrSetFoldM keepFoundInRight Map.empty left
        | otherwise = attrSetFoldM keepIfInLeft Map.empty right
  shared >>= fmap VAttrs . attrSetFromSymbolEntriesSizedFor SetIntersectAttrs (min sizeLeft sizeRight)
  where
    keepFoundInRight kept name _ = maybe kept (\entry -> Map.insert name entry kept) <$> attrSetLookupSymbolEntry name right
    keepIfInLeft kept name entry = maybe kept (const (Map.insert name entry kept)) <$> attrSetLookupSymbol name left

-- | Upstream's @prim_catAttrs@ (primops.cc): the name, read as an attribute
-- name, then the list.
builtinCatAttrs :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinCatAttrs nameVal listArg = do
  name <- attrNameOf "builtins.catAttrs" nameVal
  argValue listArg >>= \case
    VList cl -> listThunks cl >>= catAttrsCollect name >>= thunksList
    other -> throwEvalError ("builtins.catAttrs: expected a list, got " <> typeName other)

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
          attrSetLookup key attrs >>= \case
            Just found -> go (found : acc) rest
            Nothing -> go acc rest
        _ -> throwEvalError "builtins.catAttrs: list element must be a set"

-- ---------------------------------------------------------------------------
-- Builtin implementations - list higher-order (arity 2)
-- ---------------------------------------------------------------------------

-- | Force a builtin's function argument and check it as 'expectFunction'
-- does.
forceFunctionArg :: (MonadEval m) => BuiltinArg -> m NixValue
forceFunctionArg = argValue >=> expectFunction

-- | Upstream's @forceFunction@ on a forced value (eval.cc): a lambda, a
-- builtin, or a set with @__functor@, and a failure for anything else.
expectFunction :: (MonadEval m) => NixValue -> m NixValue
expectFunction val = case val of
  VLambda {} -> pure val
  VBuiltin _ _ -> pure val
  VAttrs attrs -> do
    isFunctor <- attrSetMember "__functor" attrs
    if isFunctor then pure val else expectedFailure "a function" val
  other -> expectedFailure "a function" other

-- | @prim_map@ forces the list, returns an empty one as it is, and only
-- then forces the function (primops.cc at 2.24.9), so
-- @map (throw "a") [ ]@ is @[ ]@.  Each element is a deferred
-- application, forced only on demand.
builtinMap :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinMap funcArg (VList cl) = do
  len <- clistLen cl
  if len == 0
    then pure (VList cl)
    else do
      _ <- forceFunctionArg funcArg
      fnThunk <- argAsThunk funcArg
      listThunks cl >>= mapM (mkApplyThunk fnThunk) >>= thunksList
builtinMap _ other =
  throwEvalError ("builtins.map: expected a list, got " <> typeName other)

-- | @prim_filter@, like @prim_map@, forces its function only once the
-- list is known not to be empty (primops.cc at 2.24.9).
builtinFilter :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinFilter predArg (VList cl) = do
  len <- clistLen cl
  if len == 0
    then pure (VList cl)
    else do
      predFn <- forceFunctionArg predArg
      listThunks cl >>= filterThunks predFn >>= thunksList
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

-- | @prim_genList@ forces the length, then the function, even for a
-- length of zero (primops.cc at 2.24.9).
builtinGenList :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinGenList funcArg (VInt n)
  | n < 0 = throwEvalError "builtins.genList: length must be non-negative"
  | otherwise = do
      _ <- forceFunctionArg funcArg
      -- Lazy: each element is a deferred @f i@, forced only on demand.
      -- The index is an argument value, as upstream's @prim_genList@
      -- allocates an int and an app per element.
      fnThunk <- argAsThunk funcArg
      mapM (evaluated . VInt >=> mkApplyThunk fnThunk) [0 .. n - 1] >>= thunksList
builtinGenList _ other =
  throwEvalError ("builtins.genList: expected an integer, got " <> typeName other)

-- | @prim_sort@ forces the list, returns an empty one as it is, and forces
-- the comparator before the elements (primops.cc at 2.24.9).
builtinSort :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinSort cmpArg (VList cl) = do
  len <- clistLen cl
  if len == 0
    then pure (VList cl)
    else do
      comparator <- forceFunctionArg cmpArg
      vals <- listThunks cl >>= mapM force
      mergeSort (sortComparison comparator) vals >>= valuesList
builtinSort _ other =
  throwEvalError ("builtins.sort: expected a list, got " <> typeName other)

-- | The comparison a sort runs between two elements.  A bare
-- @builtins.lessThan@ compares directly, as upstream's @prim_sort@
-- bypasses @callFunction@ for it (primops.cc at 2.24.9) and so opens no
-- call frame; any other comparator is applied as a function, one frame
-- per application.
sortComparison :: (MonadEval m) => NixValue -> NixValue -> NixValue -> m NixValue
sortComparison (VBuiltin Builtin.LessThan []) a b = builtinLessThan a b
sortComparison cmp a b = applyValue cmp a >>= (`applyValue` b)

-- | Stable O(n log n) merge sort under a comparison answering whether its
-- first argument is strictly less than its second.
mergeSort :: (MonadEval m) => (NixValue -> NixValue -> m NixValue) -> [NixValue] -> m [NixValue]
mergeSort _ [] = pure []
mergeSort _ [x] = pure [x]
mergeSort isLess xs = do
  let half = length xs `div` 2
      (left, right) = splitAt half xs
  sortedLeft <- mergeSort isLess left
  sortedRight <- mergeSort isLess right
  mergeSorted isLess sortedLeft sortedRight

mergeSorted :: (MonadEval m) => (NixValue -> NixValue -> m NixValue) -> [NixValue] -> [NixValue] -> m [NixValue]
mergeSorted _ [] ys = pure ys
mergeSorted _ xs [] = pure xs
mergeSorted isLess (x : xs) (y : ys) = do
  -- Stable merge: take the right element only when it is STRICTLY less than the
  -- left (@isLess y x@).  On a tie - neither strictly less - take the left
  -- element, so comparator-equal elements keep their input order, matching C++
  -- Nix's std::stable_sort.  (Comparing @isLess x y@ instead would emit @y@ on
  -- a tie and reverse equal runs.)
  result <- isLess y x
  case result of
    VBool True -> (y :) <$> mergeSorted isLess (x : xs) ys
    VBool False -> (x :) <$> mergeSorted isLess xs (y : ys)
    _ -> throwEvalError "builtins.sort: comparator must return a bool"

-- | Call the function on each element in turn, each in a call frame of
-- its own, as @prim_concatMap@ does (primops.cc at 2.24.9), and join the
-- lists the calls return, whose elements stay as they are.  Upstream
-- forces the function before any call and checks each result is a list
-- as its call returns; this checks the results once every call has
-- returned.
-- | The function is checked before the list is forced, and applied to
-- each element unforced, as upstream Nix applies it.
builtinConcatMap :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinConcatMap funcVal listArg = do
  func <- expectFunction funcVal
  argValue listArg >>= \case
    VList cl -> do
      thunks <- listThunks cl
      results <- mapM (\element -> withCallFrame (callCell element func)) thunks
      concatted <- mapM extractList results
      thunksList (concat concatted)
    other -> throwEvalError ("builtins.concatMap: expected a list, got " <> typeName other)

extractList :: (MonadEval m) => NixValue -> m [Thunk]
extractList (VList cl) = listThunks cl
extractList other =
  throwEvalError ("builtins.concatMap: function must return a list, got " <> typeName other)

-- | @any@ and @all@ check the function, then force the list, and apply
-- the function to each element unforced.
builtinAny :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinAny predVal listArg = do
  predFn <- expectFunction predVal
  argValue listArg >>= \case
    VList cl -> VBool <$> (listThunks cl >>= anyThunk predFn)
    other -> throwEvalError ("builtins.any: expected a list, got " <> typeName other)

anyThunk :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
anyThunk _ [] = pure False
anyThunk predFn (thunk : rest) = do
  result <- applyValueLazy predFn thunk
  case result of
    VBool True -> pure True
    VBool False -> anyThunk predFn rest
    _ -> throwEvalError "builtins.any: predicate must return a bool"

builtinAll :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinAll predVal listArg = do
  predFn <- expectFunction predVal
  argValue listArg >>= \case
    VList cl -> VBool <$> (listThunks cl >>= allThunk predFn)
    other -> throwEvalError ("builtins.all: expected a list, got " <> typeName other)

allThunk :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
allThunk _ [] = pure True
allThunk predFn (thunk : rest) = do
  result <- applyValueLazy predFn thunk
  case result of
    VBool True -> allThunk predFn rest
    VBool False -> pure False
    _ -> throwEvalError "builtins.all: predicate must return a bool"

-- | @prim_elem@ forces the list, then compares the needle with each
-- element through @eqValues@, which forces the needle before the element
-- (primops.cc and eval.cc at 2.24.9), so an empty list never forces it.
builtinElem :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinElem needleArg (VList cl) =
  fmap VBool $
    listThunks cl >>= \case
      [] -> pure False
      thunks -> do
        needle <- argValue needleArg
        elemCheck needle thunks
builtinElem _ other =
  throwEvalError ("builtins.elem: expected a list, got " <> typeName other)

elemCheck :: (MonadEval m) => NixValue -> [Thunk] -> m Bool
elemCheck _ [] = pure False
elemCheck needle (thunk : rest) = do
  val <- force thunk
  eq <- nixEqual force needle val
  if eq then pure True else elemCheck needle rest

-- | The index is forced and checked before the list: @prim_elemAt@ forces
-- it with @forceInt@ before its @elemAt@ forces the list (primops.cc at
-- 2.24.9).
builtinElemAt :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinElemAt listArg (VInt idx) = do
  list <- argValue listArg
  case list of
    VList cl -> listElemAt cl idx
    other -> expectedFailure "a list" other
builtinElemAt _ other =
  expectedFailure "an integer" other

-- | The function is checked before the list is forced, as upstream Nix
-- checks it.
builtinPartition :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinPartition predVal listArg = do
  predFn <- expectFunction predVal
  argValue listArg >>= \case
    VList cl -> do
      (rightThunks, wrongThunks) <- listThunks cl >>= partitionThunks predFn
      right <- thunksList rightThunks
      wrong <- thunksList wrongThunks
      valuesSet SetPartition (Map.fromList [("right", right), ("wrong", wrong)])
    other -> throwEvalError ("builtins.partition: expected a list, got " <> typeName other)

-- | Each element, forced before the function is applied to it, as
-- upstream forces each one, in the side the function puts it on.
-- Tail-recursive with accumulators to avoid stack overflow.
partitionThunks :: (MonadEval m) => NixValue -> [Thunk] -> m ([Thunk], [Thunk])
partitionThunks predFn = go [] []
  where
    go !rs !ws [] = pure (reverse rs, reverse ws)
    go !rs !ws (thunk : rest) = do
      _ <- force thunk
      result <- applyValueLazy predFn thunk
      case result of
        VBool True -> go (thunk : rs) ws rest
        VBool False -> go rs (thunk : ws) rest
        _ -> throwEvalError "builtins.partition: predicate must return a bool"

-- | Upstream's @prim_groupBy@ (primops.cc): each element joins the group its
-- function names, and each group holds its elements in list order.
builtinGroupBy :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinGroupBy funcVal listArg = do
  func <- expectFunction funcVal
  argValue listArg >>= \case
    VList cl -> do
      thunks <- listThunks cl
      groups <- groupByCollect func thunks Map.empty
      traverse (thunksList . reverse >=> evaluated) groups >>= fmap VAttrs . attrSetFromSymbols SetGroupBy
    other -> throwEvalError ("builtins.groupBy: expected a list, got " <> typeName other)

-- | The groups, each holding its elements last first.  A group's name is
-- interned before the function is called on the next element, as upstream
-- interns it (@prim_groupBy@ in primops.cc), so names new to the symbol
-- table take their place in list order.
groupByCollect ::
  (MonadEval m) =>
  NixValue ->
  [Thunk] ->
  Map Symbol [Thunk] ->
  m (Map Symbol [Thunk])
groupByCollect _ [] acc = pure acc
groupByCollect func (thunk : rest) !acc = do
  name <- applyValueLazy func thunk >>= attrNameOf "builtins.groupBy"
  sym <- symbolIntern name
  groupByCollect func rest (Map.insertWith (++) sym [thunk] acc)

-- ---------------------------------------------------------------------------
-- Builtin implementations - string (arity 2)
-- ---------------------------------------------------------------------------

builtinConcatStringsSep :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinConcatStringsSep (VStr sep sepCtx) listArg =
  argValue listArg >>= \case
    VList cl -> do
      pairs <- listThunks cl >>= mapM forceToStrCtx
      let texts = map fst pairs
          mergedCtx = sepCtx <> mconcat (map snd pairs)
      pure (VStr (BS.intercalate sep texts) mergedCtx)
    other -> throwEvalError ("builtins.concatStringsSep: expected a list, got " <> typeName other)
  where
    forceToStrCtx thunk = do
      val <- force thunk
      -- Nix 2.24's coerceToString defaults copyToStore=true and
      -- concatStringsSep passes no override, so a path element is copied into
      -- the store and the result carries its SCPlain context.  Non-string
      -- scalars still error (coerceMore=False), matching interpolation.
      coerceToStringInterp val
builtinConcatStringsSep other _ = expectedFailure "a string" other

-- ---------------------------------------------------------------------------
-- Builtin implementations - arity 3
-- ---------------------------------------------------------------------------

-- | The function is checked before the list is forced, and the initial
-- accumulator reaches the first call of the operator unforced and is
-- forced here only when the list is empty, as upstream Nix forces it, so
-- it is taken unforced.
builtinFoldl :: (MonadEval m) => NixValue -> BuiltinArg -> BuiltinArg -> m NixValue
builtinFoldl opVal initial listArg = do
  op <- expectFunction opVal
  argValue listArg >>= \case
    VList cl -> listThunks cl >>= foldlStrict op initial
    other -> throwEvalError ("builtins.foldl': expected a list, got " <> typeName other)

-- | Strict left fold: apply @op acc elem@ for each element.  Each step's
-- result is forced (upstream foldl' strictness), the initial accumulator
-- reaches the first step as it was taken, and the element is passed as an
-- unforced thunk, as upstream does.
foldlStrict :: (MonadEval m) => NixValue -> BuiltinArg -> [Thunk] -> m NixValue
foldlStrict _ acc [] = argValue acc
foldlStrict op acc (thunk : rest) = do
  partial <- applyArg op acc
  stepped <- applyValueLazy partial thunk
  foldlStrict op (ArgForced stepped) rest

-- | Byte-indexed slicing, as upstream: offsets and lengths count bytes,
-- and a slice may fall mid-codepoint (the resulting bytes are the value).
--
-- The start is checked before the length is forced, and the length
-- before the string, as upstream checks each.
builtinSubstring :: (MonadEval m) => NixValue -> BuiltinArg -> BuiltinArg -> m NixValue
builtinSubstring startVal lenArg valueArg = do
  start <- expectInt startVal
  when (start < 0) (throwEvalError "negative start position in 'substring'")
  len <- argValue lenArg >>= expectInt
  (s, ctx) <- argValue valueArg >>= coerceToStringInterp
  let startPos = fromIntegral start
      -- Nix clamps len to available length (negative len means rest of string)
      available = BS.length s - startPos
      clampedLen =
        if len < 0
          then available
          else min (fromIntegral len) available
  -- Context is preserved through substring (matching real Nix).
  pure (VStr (BS.take clampedLen (BS.drop startPos s)) ctx)

-- ---------------------------------------------------------------------------
-- Builtin helpers
-- ---------------------------------------------------------------------------

-- | Build a thunk that defers @f arg@ - the application only happens when
-- the thunk is forced ('mkApplyThunk', upstream's @mkApp@).
deferApply :: (MonadCData m) => NixValue -> Thunk -> m Thunk
deferApply func argThunk = do
  fnThunk <- evaluated func
  mkApplyThunk fnThunk argThunk

-- | A thunk deferring @f name value@, the name passed as a string, as
-- upstream's @mapAttrs@ and @zipAttrsWith@ build one from two nested
-- @mkApp@s ('mkApply2Thunk').
-- A builtin that makes one per attribute shares one function cell across
-- them, as upstream's primops share @args[0]@.
deferApply2 :: (MonadCData m) => Thunk -> Symbol -> Thunk -> m Thunk
deferApply2 fnThunk name valueThunk = do
  nameThunk <- evaluated (mkStr (symbolText name))
  mkApply2Thunk fnThunk nameThunk valueThunk

-- | The non-copying path coercion: the path text verbatim, no context -
-- upstream's @coerceToString@ with @copyToStore = false@.
coercePathVerbatim :: (MonadEval m) => Text -> m (BS.ByteString, StringContext)
coercePathVerbatim p = pure (TE.encodeUtf8 p, emptyContext)

-- | The copy-to-store path coercion: the path becomes its source store
-- path, carried in the context - upstream's @copyToStore = true@.  Passed
-- into 'coerceToString' so a path reached through an attrset's @outPath@
-- (or a @__toString@ result) coerces exactly as the same path written
-- directly would.
coercePathToStore :: (MonadEval m) => Text -> m (BS.ByteString, StringContext)
coercePathToStore p = do
  (spText, ctx) <- sourcePathWithContext p
  pure (TE.encodeUtf8 spText, ctx)

-- | @builtins.toString@'s coercion: permissive, a path verbatim
-- (upstream's @coerceMore@ without @copyToStore@).
coerceToStringPermissive :: (MonadEval m) => NixValue -> m (BS.ByteString, StringContext)
coerceToStringPermissive = coerceToString True force applyValue coercePathVerbatim

-- | Coerce a value to a string for a DERIVATION field (an env value or an
-- arg).  Like 'coerceToStringPermissive', but a path is copied into
-- the store: it becomes its source store path, with that path added to the
-- string context so it lands in the derivation's @inputSrcs@ - matching C++
-- Nix's copy-to-store coercion of paths in derivation arguments/environment.
coerceToStoreString :: (MonadEval m) => NixValue -> m (BS.ByteString, StringContext)
coerceToStoreString = coerceToString True force applyValue coercePathToStore

-- | Upstream's @coerceToString@ at its default arguments (@coerceMore =
-- false@, @copyToStore = true@): what string interpolation (@"${...}"@) and
-- every builtin taking a string that calls it without overrides use.  Like
-- 'coerceToString', but a path literal is copied into the store and replaced
-- by its source store path (with context), unlike 'builtins.toString', which
-- does not copy.
coerceToStringInterp :: (MonadEval m) => NixValue -> m (BS.ByteString, StringContext)
coerceToStringInterp = coerceToString False force applyValue coercePathToStore

-- | The store path a path value coerces to, carrying its SCPlain
-- context - the copy-to-store coercion shared by interpolation,
-- derivation arguments/env values, @concatStringsSep@ and
-- @builtins.toJSON@.  Every path is copied under its base name, one
-- inside the store too: upstream's @copyPathToStore@ in eval.cc has no
-- store case, so a path that is exactly a store path is copied under its
-- @\<hash\>-\<name\>@ base name.  A string naming a store path, as
-- @builtins.storePath@ ('validateStorePath') and the fetchers
-- ('storePathString') give one, coerces to itself instead.
sourcePathWithContext :: (MonadEval m) => Text -> m (Text, StringContext)
sourcePathWithContext p = do
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
builtinDiscardContext value = mkStrBytes . fst <$> coerceToStringInterp value

-- | @builtins.unsafeDiscardOutputDependency@ - downgrade all-outputs (DrvDeep)
-- references to a plain (Opaque) reference on the same @.drv@ path, and KEEP
-- derivation-output (Built) references unchanged, matching upstream.
-- 'Set.map' also folds a downgraded element into an existing plain
-- reference on the same path.
builtinDiscardOutputDep :: (MonadEval m) => NixValue -> m NixValue
builtinDiscardOutputDep value = do
  (s, StringContext ctx) <- coerceToStringInterp value
  pure (VStr s (StringContext (Set.map downgrade ctx)))
  where
    downgrade (SCAllOutputs sp) = SCPlain sp
    downgrade other = other

-- | @builtins.addDrvOutputDependencies@ - the inverse of the
-- 'builtinDiscardOutputDep' downgrade for a single element: upgrade a plain
-- (Opaque) reference to a @.drv@ path into an all-outputs (DrvDeep) reference.
-- Upstream requires the string's context to have exactly one element and
-- errors on a derivation-output (Built) element or a non-@.drv@ plain path.
builtinAddDrvOutputDeps :: (MonadEval m) => NixValue -> m NixValue
builtinAddDrvOutputDeps value = do
  (s, StringContext ctx) <- coerceToStringInterp value
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
builtinGetContext (VStr _ (StringContext ctx)) =
  traverse contextEntryToAttrs (groupContextByPath (Set.toList ctx)) >>= valuesSet SetGetContext
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

-- | Convert a ContextEntry to its attrset.
contextEntryToAttrs :: (MonadCData m) => ContextEntry -> m NixValue
contextEntryToAttrs entry = do
  outputs <- case ceOutputs entry of
    [] -> pure []
    names -> (\list -> [("outputs", list)]) <$> valuesList (map mkStr names)
  valuesSet SetGetContext . Map.fromList $
    [("path", VBool True) | cePath entry]
      ++ [("allOutputs", VBool True) | ceAllOutputs entry]
      ++ outputs

-- | Append context entries to a string from an attrset.
--
-- @builtins.appendContext string contextAttrset@ adds the specified
-- context elements to the string.
builtinAppendContext :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinAppendContext (VStr s ctx) contextArg =
  argValue contextArg >>= \case
    VAttrs contextAttrs -> do
      newCtx <- attrSetToMap contextAttrs >>= parseContextAttrs
      pure (VStr s (ctx <> newCtx))
    other -> throwEvalError ("builtins.appendContext: second argument must be a set, got " <> typeName other)
builtinAppendContext other _ = expectedFailure "a string" other

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
          -- The key must parse as a store path, as upstream requires; a
          -- fabricated identity here would flow into derivation inputs.
          sp <- case parseStorePath defaultStoreDir pathText of
            Just parsed -> pure parsed
            Nothing ->
              throwEvalError
                ("builtins.appendContext: context key is not a store path: " <> pathText)
          hasPath <- getBoolAttr "path" inner
          hasAllOuts <- getBoolAttr "allOutputs" inner
          outNames <- getOutputsList inner
          let pathElems = [SCPlain sp | hasPath]
              allOutElems = [SCAllOutputs sp | hasAllOuts]
              outElems = [SCDrvOutput sp o | o <- outNames]
          pure (pathElems ++ allOutElems ++ outElems)
        _ -> throwEvalError "builtins.appendContext: context entry must be a set"

    getBoolAttr key attrs' =
      attrSetLookup key attrs' >>= \case
        Nothing -> pure False
        Just thunk -> do
          val <- force thunk
          case val of
            VBool b -> pure b
            _ -> pure False

    getOutputsList attrs' =
      attrSetLookup "outputs" attrs' >>= \case
        Nothing -> pure []
        Just thunk -> do
          val <- force thunk
          case val of
            VList cl -> listThunks cl >>= mapM forceToOutputName
            _ -> pure []

    forceToOutputName thunk = do
      val <- force thunk
      case val of
        VStr s _ -> decodedText "builtins.appendContext" s
        _ -> throwEvalError "builtins.appendContext: output name must be a string"

-- | For a STRING operand the rule is textual, as upstream Nix reads it
-- ('lastComponentBytes'): one trailing @/@ is dropped and everything after
-- the final @/@ is the name, so a string ending in two or more slashes has
-- an empty one.  For a PATH operand the value may be native-spelled
-- (eval's base dir can be a native Windows path), so the split is
-- separator-aware via 'canonBaseName'.
builtinBaseNameOf :: (MonadEval m) => NixValue -> m NixValue
builtinBaseNameOf (VPath p) = pure (mkStr (canonBaseName p))
-- Anything else is coerced to a string without a copy, as upstream's
-- prim_baseNameOf coerces it (primops.cc at 2.24.9): an outPath or a
-- __toString answers, an integer does not.
builtinBaseNameOf other = do
  (s, ctx) <- coerceToString False force applyValue coercePathVerbatim other
  pure (VStr (lastComponentBytes s) ctx)

-- | A string operand's base name, as upstream reads one: a single
-- trailing @/@ is dropped unless it is the whole string, and the name is
-- what follows the last @/@ before it, so @"a/"@ is @"a"@ and @"a//"@ is
-- empty.  Read as bytes, since @/@ is one byte in UTF-8 and never occurs
-- inside a multi-byte sequence.
lastComponentBytes :: BS.ByteString -> BS.ByteString
lastComponentBytes t = BC.takeWhileEnd (/= '/') trimmed
  where
    trimmed = case BC.unsnoc t of
      Just (rest, '/') | not (BS.null rest) -> rest
      _ -> t

-- | String operands keep upstream's textual '/'-only rule
-- ('dirComponentBytes'); path operands may be native-spelled and split
-- separator-aware via 'canonDirName'.
builtinDirOf :: (MonadEval m) => NixValue -> m NixValue
builtinDirOf (VPath p) = pure (VPath (canonDirName p))
-- Only a path value stays a path (prim_dirOf); anything else is coerced
-- to a string without a copy, so a set whose outPath is a path gives a
-- string.
builtinDirOf other = do
  (s, ctx) <- coerceToString False force applyValue coercePathVerbatim other
  pure (VStr (dirComponentBytes s) ctx)

-- | Byte-level dir component for string operands: everything before the
-- last '/' byte, with upstream's "." / "/" edge results.
dirComponentBytes :: BS.ByteString -> BS.ByteString
dirComponentBytes t =
  case BC.elemIndexEnd '/' t of
    Nothing -> "."
    Just idx ->
      let dir = BS.take idx t
       in if BS.null dir then "/" else dir

builtinConcatLists :: (MonadEval m) => NixValue -> m NixValue
builtinConcatLists (VList cl) = do
  sublists <- listThunks cl >>= mapM forceThenExtractList
  thunksList (concat sublists)
  where
    forceThenExtractList thunk = do
      val <- force thunk
      case val of
        VList innerCl -> listThunks innerCl
        _ -> throwEvalError "builtins.concatLists: element must be a list"
builtinConcatLists other =
  throwEvalError ("builtins.concatLists: expected a list, got " <> typeName other)

builtinLessThan :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinLessThan a b = VBool <$> nixCompare force a b

-- ---------------------------------------------------------------------------
-- Builtin implementations - arithmetic + bitwise
-- ---------------------------------------------------------------------------

-- | @prim_bitAnd@ and its kin (primops.cc at 2.24.9): both operands go
-- through @forceInt@, the left first.
bitwise :: (MonadEval m) => (Int64 -> Int64 -> Int64) -> NixValue -> BuiltinArg -> m NixValue
bitwise op a bArg = do
  x <- expectInt a
  y <- argValue bArg >>= expectInt
  pure (VInt (op x y))

-- ---------------------------------------------------------------------------
-- Builtin implementations - attr set higher-order
-- ---------------------------------------------------------------------------

-- | @prim_mapAttrs@ forces only the set: the function is applied in each
-- attribute's deferred application and never forced by the primop itself
-- (primops.cc at 2.24.9), so @mapAttrs (throw "a") { }@ is @{ }@.
builtinMapAttrs :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinMapAttrs funcArg (VAttrs attrs) =
  -- Each attr value is a deferred @f key val@, forced only on demand.
  do
    fnThunk <- argAsThunk funcArg
    VAttrs <$> attrSetMapWithKey SetMapAttrs (deferApply2 fnThunk) attrs
builtinMapAttrs _ other =
  throwEvalError ("builtins.mapAttrs: expected a set, got " <> typeName other)

-- | Formals for lambdas, an empty set for builtins, an error otherwise -
-- including functor sets: upstream's primop never consults
-- @__functionArgs@ (nixpkgs lib.functionArgs handles that in Nix).
builtinFunctionArgs :: (MonadEval m) => NixValue -> m NixValue
builtinFunctionArgs (VLambda _ formals _) = formalsToAttrs formals
builtinFunctionArgs (VBuiltin _ _) = valuesSet SetFunctionArgs Map.empty
builtinFunctionArgs _ = throwEvalError "'functionArgs' requires a function"

formalsToAttrs :: (MonadCData m) => EvalFormals -> m NixValue
formalsToAttrs (EFName _) = valuesSet SetFunctionArgs Map.empty
formalsToAttrs (EFSet _ formals _) = formalsListToAttrs formals
formalsToAttrs (EFNamedSet _ _ formals _) = formalsListToAttrs formals

formalsListToAttrs :: (MonadCData m) => [EvalFormal] -> m NixValue
formalsListToAttrs formals =
  traverse evaluated (Map.fromList [(efName f, VBool (hasDefault (efDefault f))) | f <- formals])
    >>= fmap VAttrs . attrSetFromSymbols SetFunctionArgs

-- | Upstream's @prim_zipAttrsWith@ (primops.cc): the function is forced and
-- checked as @forceFunction@ checks it before the list is forced, so it
-- fails on a first argument that is not a function whatever the list holds.
-- The list's sets are forced first to last, and each name any of them binds
-- gathers its values in list order, under the name's symbol.
builtinZipAttrsWith :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinZipAttrsWith func listArg = do
  _ <- expectFunction func
  argValue listArg >>= \case
    VList cl -> listThunks cl >>= mapM forceToAttrSet >>= zipSets
    other -> throwEvalError ("builtins.zipAttrsWith: expected a list, got " <> typeName other)
  where
    zipSets attrSets = do
      fnThunk <- evaluated func
      gathered <- foldM (attrSetFoldM gather) Map.empty attrSets
      -- Lazy: each result is a deferred f(name)(values) thunk, not eagerly
      -- evaluated.  Critical for nixpkgs evalModules fixpoint - config is a
      -- self-referencing lazy attrset that must be COMPUTED (holding the lazy
      -- result) before any individual attribute thunks are forced.
      results <- Map.traverseWithKey (deferZip fnThunk) gathered
      VAttrs <$> attrSetFromSymbols SetZipAttrsWith results
    forceToAttrSet thunk = do
      val <- force thunk
      case val of
        VAttrs attrs -> pure attrs
        _ -> throwEvalError "builtins.zipAttrsWith: list element must be a set"
    gather seen name entry = pure (Map.alter (Just . maybe (Seq.singleton (entryThunk entry)) (Seq.|> entryThunk entry)) name seen)
    deferZip fnThunk name values = do
      valueList <- thunksList (toList values) >>= evaluated
      deferApply2 fnThunk name valueList

-- ---------------------------------------------------------------------------
-- Builtin implementations - string manipulation
-- ---------------------------------------------------------------------------

-- | The lists are forced and their lengths compared, and every @from@
-- string forced, before the subject is, as upstream reads them.
builtinReplaceStrings ::
  (MonadEval m) => NixValue -> BuiltinArg -> BuiltinArg -> m NixValue
builtinReplaceStrings (VList fromCl) toArg inputArg = do
  toCl <-
    argValue toArg >>= \case
      VList cl -> pure cl
      other -> expectedFailure "a list" other
  fromThunks <- listThunks fromCl
  toThunks <- listThunks toCl
  when (length fromThunks /= length toThunks) $
    throwEvalError "'from' and 'to' arguments passed to builtins.replaceStrings have different lengths"
  froms <- mapM forceStr fromThunks
  (input, inputCtx) <-
    argValue inputArg >>= \case
      VStr bytes ctx -> pure (bytes, ctx)
      other -> expectedFailure "a string" other
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
        other -> expectedFailure "a string" other
builtinReplaceStrings other _ _ = expectedFailure "a list" other

-- ---------------------------------------------------------------------------
-- Builtin implementations - regex (POSIX ERE via regex-tdfa)
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Regex compilation cache
-- ---------------------------------------------------------------------------

-- | Global regex compilation cache, the one place a compiled pattern
-- lives: no value holds one, and @match@ and @split@ take their pattern
-- as a string and compile it here on each call, as upstream keeps its
-- compiled @std::regex@ in a cache keyed by the pattern rather than in
-- the value of a partial application (@RegexCache@ in primops.cc).  Keyed
-- by the raw pattern bytes (match and split share entries).  A memo of a
-- pure function, so the one process-wide table is safe to create outside
-- any monad: a pattern compiles to the same regex whether the cache answers
-- or the compiler does.  It holds no C memory, so no root of the C data
-- layer is in it.
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
-- Returns Nothing for invalid patterns.  Pure for the reason the cache
-- is ('regexCacheRef'); NOINLINE keeps GHC from inlining and floating
-- the cache's reads and writes.
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
builtinMatch :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinMatch patternVal subjectArg = do
  compiled <- compiledPattern "builtins.match" patternVal
  argValue subjectArg >>= \case
    VStr str _ -> matchWithCompiled compiled str
    other -> throwEvalError ("builtins.match: expected a string, got " <> typeName other)

-- | The pattern of @match@ or @split@, compiled.  Upstream reads the
-- pattern with @forceStringNoCtx@ and compiles it before it forces the
-- subject (@prim_match@ and @prim_split@ in primops.cc), so a pattern
-- with context is refused.
compiledPattern :: (MonadEval m) => Text -> NixValue -> m RE.Regex
compiledPattern reader patternVal = do
  pat <- structuredStringValue RefuseContext patternVal
  maybe (throwEvalError (reader <> ": invalid regex: " <> bytesToTextLossy pat)) pure (cachedCompileRegex pat)

-- | The result of @match@ for a compiled pattern and a subject's bytes.
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
          valuesList (map captureGroupValue (drop 1 (Array.elems match)))
    _ -> pure VNull

-- | A capture group of a match: its bytes, or null for a group that did
-- not participate (offset -1), as C++ Nix yields for one rather than the
-- empty string.
captureGroupValue :: (BS.ByteString, (Int, Int)) -> NixValue
captureGroupValue (s, (off, _))
  | off < 0 = VNull
  | otherwise = mkStrBytes s

-- | @builtins.split regex str@: split a string by a POSIX ERE.
-- Returns an alternating list of non-matched strings and match-group lists.
-- Example: @split "(x)" "axbxc"@ yields @["a" ["x"] "b" ["x"] "c"]@
builtinSplit :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinSplit patternVal subjectArg = do
  compiled <- compiledPattern "builtins.split" patternVal
  argValue subjectArg >>= \case
    VStr str ctx -> splitPieces compiled str ctx >>= valuesList
    other -> throwEvalError ("builtins.split: expected a string, got " <> typeName other)

-- | The elements of @split@'s list for a subject.  A subject the pattern
-- does not match is the one element, context and all, as upstream
-- returns its argument there (@prim_split@ in primops.cc); the pieces of one
-- it matches carry no context, as upstream's do.
splitPieces :: (MonadCData m) => RE.Regex -> BS.ByteString -> StringContext -> m [NixValue]
splitPieces compiled str ctx = case matchAllText compiled str of
  [] -> pure [VStr str ctx]
  matches -> buildSplitResult str 0 matches

-- | Build the alternating list for builtins.split.  Offsets from the
-- ByteString regex instance are byte offsets, so slicing is O(1)
-- 'BS.take'/'BS.drop'.
buildSplitResult :: (MonadCData m) => BS.ByteString -> Int -> [Array.Array Int (BS.ByteString, (Int, Int))] -> m [NixValue]
buildSplitResult remaining pos [] =
  -- No more matches - emit the rest of the string.
  pure [mkStrBytes (BS.drop pos remaining)]
buildSplitResult remaining pos (match : rest) = do
  let elems = Array.elems match
      (_, (matchStart, matchLen)) = case elems of
        (full : _) -> full
        [] -> ("", (pos, 0)) -- defensive: should not happen from matchAllText
        -- Bytes before this match
      before = BS.take (matchStart - pos) (BS.drop pos remaining)
      -- Continue after this match
      afterPos = matchStart + matchLen
  -- Capture groups (indices 1..), each as 'match' gives it.
  groups <- valuesList (map captureGroupValue (drop 1 elems))
  (\later -> mkStrBytes before : groups : later) <$> buildSplitResult remaining afterPos rest

-- | Upstream reads both versions with @forceStringNoCtx@, the first
-- before the second (@prim_compareVersions@ in primops.cc).
builtinCompareVersions :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinCompareVersions first secondArg = do
  a <- structuredStringValue RefuseContext first
  b <- argValue secondArg >>= structuredStringValue RefuseContext
  pure (VInt (compareVersionParts (splitVersionStr a) (splitVersionStr b)))

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
builtinSplitVersion val = do
  s <- structuredStringValue RefuseContext val
  valuesList (map mkStrBytes (splitVersionComponents s))

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
builtinParseDrvName val = do
  (name, version) <- parseName <$> structuredStringValue RefuseContext val
  valuesSet SetParseDrvName (Map.fromList [("name", mkStrBytes name), ("version", mkStrBytes version)])

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
  pure (VStr (renderJson json) ctx)

-- | The JSON of a value, with the context of every string it holds.
-- Strings decode strictly: upstream's serializer (nlohmann) REJECTS
-- invalid UTF-8, so invalid bytes are an eval error, matching upstream.
-- 'Nix.Json.renderJson' writes the result; @builtins.toJSON@ and a
-- structured-attributes derivation's @__json@ share both halves, as
-- upstream's share @printValueAsJSON@.
valueToJSON :: (MonadEval m) => NixValue -> m (Json, StringContext)
valueToJSON VNull = pure (JsonNull, emptyContext)
valueToJSON (VBool b) = pure (JsonBool b, emptyContext)
valueToJSON (VInt n) = pure (JsonInt (toInteger n), emptyContext)
valueToJSON (VFloat f) = pure (JsonFloat f, emptyContext)
valueToJSON (VStr s ctx) = do
  decoded <- decodedText "builtins.toJSON" s
  pure (JsonString decoded, ctx)
valueToJSON (VList cl) = do
  vals <- listThunks cl >>= mapM force
  results <- mapM valueToJSON vals
  pure (JsonArray (map fst results), mconcat (map snd results))
-- An attrset is upstream's three-way choice (value-to-json.cc at 2.24.9):
-- a __toString result coerced as tryAttrsToString is called there, with
-- coerceMore and copyToStore both off, so the result must already be a
-- string or a path and a path stays its own absolute text; otherwise the
-- outPath value, converted as any value is, so it need not be a string;
-- otherwise an object.
valueToJSON (VAttrs attrs) = do
  hasToString <- attrSetMember "__toString" attrs
  if hasToString
    then do
      (s, ctx) <- coerceToString False force applyValue coercePathVerbatim (VAttrs attrs)
      decoded <- decodedText "builtins.toJSON" s
      pure (JsonString decoded, ctx)
    else
      attrSetLookup "outPath" attrs >>= \case
        Just outPathThunk -> force outPathThunk >>= valueToJSON
        Nothing -> do
          results <- attrSetToAscList attrs >>= mapM jsonMember
          pure (JsonObject (Map.fromAscList [(key, json) | (key, json, _) <- results]), mconcat [ctx | (_, _, ctx) <- results])
  where
    jsonMember (key, thunk) = do
      val <- force thunk
      (json, ctx) <- valueToJSON val
      pure (key, json, ctx)
-- A path serializes as its source store path, with context - the same
-- copy-to-store coercion as interpolation (upstream value-to-json.cc
-- serializes paths with copyToStore = true).
valueToJSON (VPath p) = do
  (spText, ctx) <- sourcePathWithContext p
  pure (JsonString spText, ctx)
valueToJSON (VLambda {}) = throwEvalError "builtins.toJSON: cannot convert a function to JSON"
valueToJSON (VBuiltin _ _) = throwEvalError "builtins.toJSON: cannot convert a function to JSON"

-- | Upstream reads the string with nlohmann's SAX parser (json-to-value.cc),
-- whose grammar 'parseJsonInto' implements, as it does for the builder's
-- @__json@: strict RFC 8259, so a trailing comma, a raw control character
-- or a lone surrogate is refused.  'NixRange' keeps every integer a 'VInt'
-- can hold.
builtinFromJSON :: (MonadEval m) => NixValue -> m NixValue
builtinFromJSON val = do
  s <- structuredStringValue RefuseContext val
  -- JSON input must be valid UTF-8 (upstream's parser rejects it too).
  decoded <- decodedText "builtins.fromJSON" s
  either throwEvalError id (parseJsonInto NixRange fromJSONBuilder decoded)

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

-- | The value upstream's SAX consumer, @JSONSax@ (json-to-value.cc), makes
-- of each JSON value.  An object interns a member's name as the parse
-- reaches it, before the names in the member's value
-- (@JSONObjectState::key@), so names new to the symbol table take their
-- place in document order.  A name the object repeats keeps the symbol its
-- first occurrence took and takes its last value, as @insert_or_assign@
-- assigns it; the value it replaces is still made, and the names in it
-- interned.
fromJSONBuilder :: (MonadCData m) => JsonBuilder (m NixValue)
fromJSONBuilder =
  JsonBuilder
    { buildNull = pure VNull,
      buildBool = pure . VBool,
      buildInt = pure . VInt . fromInteger,
      buildFloat = pure . VFloat,
      buildString = pure . mkStr,
      buildArray = sequence >=> valuesList,
      buildObject = foldM addMember Map.empty >=> fmap VAttrs . attrSetFromSymbols SetFromJSON
    }
  where
    addMember !made (name, makeValue) = do
      sym <- symbolIntern name
      thunk <- makeValue >>= evaluated
      pure (Map.insert sym thunk made)

-- | Upstream reads the algorithm with @forceStringNoCtx@ before it forces
-- the string hashed, whose context it discards (@prim_hashString@ in
-- primops.cc).
builtinHashString :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinHashString algoVal inputArg = do
  algoName <- decodedText "builtins.hashString" =<< structuredStringValue RefuseContext algoVal
  -- The algorithm is checked before the string is forced, as upstream
  -- checks it.
  digest <- maybe (throwEvalError (unknownHashAlgo algoName)) pure (hexDigestOf algoName)
  argValue inputArg >>= \case
    -- The payload IS the hashed bytes - no encode step, so a mid-codepoint
    -- substring hashes to the sha256 of exactly those bytes, as upstream.
    VStr input _ -> pure (mkStr (digest input))
    other -> throwEvalError ("builtins.hashString: expected a string, got " <> typeName other)

digestToHex :: (BA.ByteArrayAccess a) => a -> Text
digestToHex = bytesToHexText . BA.convert

-- ---------------------------------------------------------------------------
-- Builtin implementations - deep evaluation
-- ---------------------------------------------------------------------------

-- | @builtins.deepSeq a b@.  @prim_deepSeq@ forces the first argument
-- deeply before it forces the second (primops.cc at 2.24.9), so the
-- second is taken unforced.  'builtinTrace' and 'builtinWarn' take theirs
-- unforced for the same reason: @prim_trace@ prints and @prim_warn@ warns
-- before forcing the second.
builtinDeepSeq :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinDeepSeq first second = deepForce first >> argValue second

-- | Force every value inside a value, through its lists and sets, as
-- upstream's @forceValueDeep@ does for @builtins.deepSeq@ and
-- @nix-instantiate --eval --strict@ (eval.cc): a list's elements first to
-- last, and a set's values in the order the set holds them, by the symbols
-- of their names, so of two values that fail, the one whose name was
-- interned first fails first.  Upstream enters each @Value@ once, so a value
-- reached twice is forced once and one that holds itself, as a derivation
-- does through its @all@ and output attributes, is not entered without end.
-- The cell holding a value is its identity here: every cell the walk enters
-- is reached from the value handed over, which is older than every force the
-- walk runs, so no close of one frees a cell the walk has entered
-- ("Nix.Eval.CRegion"), and a cell is one value for the whole force.  The
-- value handed over has no cell, so it is not entered by one; what it holds
-- is.
deepForce :: (MonadEval m) => NixValue -> m ()
deepForce = void . forceInside Set.empty
  where
    forceInside entered (VList cl) = listThunks cl >>= foldM forceCell entered
    forceInside entered (VAttrs attrs) = attrSetFoldM (\seen _ entry -> forceCell seen (entryThunk entry)) entered attrs
    forceInside entered _ = pure entered
    forceCell entered thunk
      | Set.member cell entered = pure entered
      | otherwise = force thunk >>= forceInside (Set.insert cell entered)
      where
        cell = unThunk thunk

-- | @builtins.seq a b@ - evaluate @a@ to WHNF, then return @b@.
builtinSeq :: (MonadEval m) => NixValue -> NixValue -> m NixValue
builtinSeq !_first = pure

-- | @builtins.trace msg val@ - print @msg@ to stderr, then force and
-- return @val@.
builtinTrace :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinTrace msgVal result = emitTrace msgVal >> argValue result

-- | @builtins.traceVerbose msg val@: @builtins.trace@ under
-- @trace-verbose@, and otherwise @val@ alone.  Upstream binds the name to
-- @prim_trace@ or to @prim_second@ by the setting (primops.cc at 2.24.9),
-- and @prim_second@ never forces its first argument, so both are taken
-- unforced and @traceVerbose (throw "a") 1@ is @1@ without the setting.
builtinTraceVerbose :: (MonadEval m) => BuiltinArg -> BuiltinArg -> m NixValue
builtinTraceVerbose msgArg result = do
  policy <- evalPolicy
  when (epTraceVerbose policy) (argValue msgArg >>= emitTrace)
  argValue result

emitTrace :: (MonadEval m) => NixValue -> m ()
emitTrace msgVal =
  traceMessage . ("trace: " <>) =<< case msgVal of
    VStr s _ -> pure (bytesToTextLossy s)
    other -> printValue PrintInFull other

-- | @builtins.warn msg val@ - print warning to stderr, then force and
-- return @val@.
--
-- The message must be a string: upstream's @prim_warn@ forces it with
-- @forceString@ (primops.cc at 2.24.9), leaving room to extend the
-- argument later, where @builtins.trace@ prints any value.
builtinWarn :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinWarn msgVal result = emitWarning msgVal >> argValue result

-- | Upstream logs the message as a warning from an expression, which its
-- logger prefixes @evaluation warning:@.
emitWarning :: (MonadEval m) => NixValue -> m ()
emitWarning msgVal = case msgVal of
  VStr s _ -> traceMessage ("evaluation warning: " <> bytesToTextLossy s)
  other ->
    expectedFailure "a string" other

-- ---------------------------------------------------------------------------
-- Builtin implementations - graph traversal
-- ---------------------------------------------------------------------------

builtinGenericClosure :: (MonadEval m) => NixValue -> m NixValue
builtinGenericClosure (VAttrs attrs) = do
  startSetThunk <-
    attrSetLookup "startSet" attrs
      >>= maybe (throwEvalError "builtins.genericClosure: missing 'startSet'") pure
  operatorThunk <-
    attrSetLookup "operator" attrs
      >>= maybe (throwEvalError "builtins.genericClosure: missing 'operator'") pure
  startSetVal <- force startSetThunk
  operatorVal <- force operatorThunk
  case startSetVal of
    VList cl -> do
      items <- listThunks cl
      closureLoop operatorVal (Seq.fromList items) [] [] >>= valuesList
    _ -> throwEvalError "builtins.genericClosure: 'startSet' must be a list"
builtinGenericClosure other =
  throwEvalError ("builtins.genericClosure: expected a set, got " <> typeName other)

-- | BFS loop for genericClosure.  Uses Data.Sequence for O(1) queue
-- append (a list would make each append O(n)).
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
        VList newCl -> do
          newThunks <- listThunks newCl
          closureLoop operator (rest <> Seq.fromList newThunks) (key : seenKeys) (item : acc)
        _ -> throwEvalError "builtins.genericClosure: operator must return a list"

extractKey :: (MonadEval m) => NixValue -> m NixValue
extractKey (VAttrs attrs) =
  attrSetLookup "key" attrs >>= \case
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

-- | Upstream's @coerceToPath@ (eval.cc at 2.24.9), which every builtin
-- taking a path runs its argument through: a path is itself, an attribute
-- set's @__toString@ result is coerced again (it may be a path), and any
-- other value is coerced to a string without a copy, which must spell an
-- absolute path.  The string's bytes name a filesystem path, so they
-- decode strictly, the label naming the builtin if they cannot.  The
-- context the coercion gathers comes back with the path.
coerceToPathWithContext :: (MonadEval m) => Text -> NixValue -> m (Text, StringContext)
coerceToPathWithContext _ (VPath p) = pure (p, emptyContext)
coerceToPathWithContext label val@(VAttrs attrs) =
  attrSetLookup "__toString" attrs >>= \case
    Just toStringThunk -> do
      toStringFn <- force toStringThunk
      coerceToPathWithContext label =<< applyValue toStringFn val
    Nothing -> coerceToStringAsPath label val
coerceToPathWithContext label val = coerceToStringAsPath label val

-- | The last case of 'coerceToPathWithContext': the value coerced to a
-- string without a copy, which must spell an absolute path.
coerceToStringAsPath :: (MonadEval m) => Text -> NixValue -> m (Text, StringContext)
coerceToStringAsPath label val = do
  (bytes, ctx) <- coerceToString False force applyValue coercePathVerbatim val
  path <- absolutePathString =<< decodedText label bytes
  pure (path, ctx)

-- | 'coerceToPathWithContext' for a builtin that keeps no string and
-- realises no context: one that reads nothing at the path, or reads it
-- only as upstream's does without realising (@builtins.storePath@
-- resolves symlinks through it).
coerceToPath :: (MonadEval m) => Text -> NixValue -> m Text
coerceToPath label val = fst <$> coerceToPathWithContext label val

-- | Upstream's @realisePath@ (primops.cc), which every builtin that reads a
-- path runs its argument through: the path the argument coerces to, once the
-- store paths its context names are realised ('realiseContext'), which adds
-- the line of context @while realising the context of path \'...\'@ to a
-- failure.  A refusal is an evaluation error, which @builtins.tryEval@ does
-- not catch, as upstream's catches only an assertion (@prim_tryEval@ in
-- primops.cc).  A path value has no context and is read as it is.
realisePath :: (MonadEval m) => Text -> NixValue -> m Text
realisePath label val = do
  (path, ctx) <- coerceToPathWithContext label val
  unless (contextIsEmpty ctx) $
    addErrorTrace ("while realising the context of path '" <> path <> "'") (realisedOrRefused ctx)
  pure path

-- | Upstream's @addPath@ around the copy @builtins.path@ and
-- @builtins.filterSource@ make of a path, once their arguments are read
-- (primops.cc): a path in the store has its context realised first, and any
-- failure from there on, the realisation's, the filter's and the copy's,
-- carries the line of context @while adding path \'...\'@.
addingPath :: (MonadEval m) => Text -> StringContext -> m a -> m a
addingPath path ctx copy =
  addErrorTrace ("while adding path '" <> path <> "'") $ do
    when (isInStoreText path && not (contextIsEmpty ctx)) (realisedOrRefused ctx)
    copy

-- | Realise a context, failing with the refusal's message when it cannot
-- be.
realisedOrRefused :: (MonadEval m) => StringContext -> m ()
realisedOrRefused ctx = realiseContext ctx >>= either (throwEvalError . contextRefusalMessage) pure

-- | Upstream @coerceToPath@'s rule for a string operand: it must spell an
-- absolute path, which is then canonicalized like any path value
-- (@rootPath(CanonPath(path))@).  A relative string is refused with
-- upstream's wording rather than read against the working directory,
-- which is also what keeps the access policy sound: the path checked is
-- the path opened.
absolutePathString :: (MonadEval m) => Text -> m Text
absolutePathString s
  | isAbsolutePath canonical = pure canonical
  | otherwise = throwEvalError ("string '" <> s <> "' doesn't represent an absolute path")
  where
    canonical = canonPathValue s

builtinImport :: (MonadEval m) => NixValue -> m NixValue
builtinImport val = importFile =<< realisePath "import" val

builtinReadFile :: (MonadEval m) => NixValue -> m NixValue
builtinReadFile val = do
  p <- realisePath "builtins.readFile" val
  bytes <- readFileBytes p
  -- The value is the file's RAW BYTES, as upstream: no BOM stripping, no
  -- UTF-16 transcoding, no replacement characters (the auto-decode stays
  -- on the parser/import path only).  The only rejection is NUL, which a
  -- Nix string cannot represent upstream.
  when (BS.elem 0 bytes) $
    throwEvalError
      ("builtins.readFile: the contents of the file '" <> p <> "' cannot be represented as a Nix string")
  pure (mkStrBytes bytes)

-- | @builtins.pathExists@.  Whether a directory is demanded is read off
-- the argument's spelling before coercion, as upstream's
-- @prim_pathExists@ reads it: a string ending in @/@ or @/.@, which
-- canonicalization would otherwise erase.
builtinPathExists :: (MonadEval m) => NixValue -> m NixValue
builtinPathExists val = do
  p <- realisePath "builtins.pathExists" val
  VBool <$> doesPathExist (existenceQuery val) p
  where
    existenceQuery (VStr s _)
      | BS.isSuffixOf "/" s || BS.isSuffixOf "/." s = ExistsAsDirectory
    existenceQuery _ = ExistsAsEntry

-- | @builtins.readDir@.  An entry the listing gives no type is bound to a
-- deferred @builtins.readFileType@ of its path, as @prim_readDir@ binds
-- it (primops.cc), so only forcing that entry fails, and it fails as
-- @readFileType@ does.  The path is the entry's under the directory with its
-- symlinks resolved, as @prim_readDir@ realises the directory before it reads
-- it, so the lstat @readFileType@ makes meets no symlink above the entry.
builtinReadDir :: (MonadEval m) => NixValue -> m NixValue
builtinReadDir val = do
  p <- realisePath "builtins.readDir" val
  entries <- listDirectory p
  -- Resolved only when an entry needs it, so a listing that types every
  -- entry costs nothing more.
  dir <- if any (isNothing . snd) entries then resolveSymlinks p else pure p
  let untypedEntry name = evaluated (VPath (canonPathValue (dir <> "/" <> name))) >>= deferApply (VBuiltin Builtin.ReadFileType [])
      entryValue name = maybe (untypedEntry name) (evaluated . mkStr)
  values <- Map.traverseWithKey entryValue (Map.fromList entries)
  VAttrs <$> attrSetFromMap SetReadDir values

-- | A listed entry's type, or for an entry the listing gives no type, the
-- type an lstat of the entry reads, which fails as @builtins.readFileType@
-- fails on it.  Upstream's tree walks lstat every entry they visit (the
-- path filter's @callPathFilter@ in primops.cc, and @dumpPath@ in
-- archive.cc), so an entry the listing cannot type fails there.
entryFileType :: (MonadEval m) => Text -> Maybe Text -> m Text
entryFileType path = maybe (getFileType path) pure

-- ---------------------------------------------------------------------------
-- Builtin implementations - environment + paths
-- ---------------------------------------------------------------------------

builtinGetEnv :: (MonadEval m) => NixValue -> m NixValue
builtinGetEnv val = mkStrBytes <$> (getEnvVar =<< structuredStringValue RefuseContext val)

-- | Upstream's @prim_toPath@ returns the coerced path as a string, with
-- its context, not as a path value (primops.cc at 2.24.9).
builtinToPath :: (MonadEval m) => NixValue -> m NixValue
builtinToPath val = do
  (path, ctx) <- coerceToPathWithContext "builtins.toPath" val
  pure (VStr (TE.encodeUtf8 path) ctx)

-- ---------------------------------------------------------------------------
-- Builtin implementations - store path operations
-- ---------------------------------------------------------------------------

builtinPlaceholder :: (MonadEval m) => NixValue -> m NixValue
builtinPlaceholder val = do
  name <- decodedText "builtins.placeholder" =<< structuredStringValue RefuseContext val
  pure (mkStr (hashPlaceholder name))

-- | Refused under @pure-eval@ before the argument is forced, as upstream
-- Nix refuses it: a dependency on a store path that merely exists is
-- ambient state.
builtinStorePath :: (MonadEval m) => BuiltinArg -> m NixValue
builtinStorePath arg = do
  policy <- evalPolicy
  if epPureEval policy
    then throwEvalError "'builtins.storePath' is not allowed in pure evaluation mode"
    else validateStorePath =<< resolvedUnlessStorePath =<< coerceToPath "builtins.storePath" =<< argValue arg
  where
    -- prim_storePath in primops.cc resolves the path's symlinks unless the
    -- path is itself a store path, a link directly in the store, so a link
    -- outside the store into it names its target.
    resolvedUnlessStorePath path
      | isBareStorePath path = pure path
      | otherwise = resolveSymlinks path
    isBareStorePath path = case T.stripPrefix storeDirPrefix path of
      Just base -> not (T.any (== '/') base) && isJust (parseStorePathBaseName base)
      Nothing -> False

-- | @builtins.storePath@ - mark an already-in-store path as such.  Upstream
-- returns a STRING carrying an Opaque (SCPlain) context entry for the enclosing
-- store path, NOT a bare path: a string coerces to itself, so a derivation
-- naming it depends on that store path, where a path value is copied to
-- the store again under its base name (the documentation of
-- @primop_storePath@ in primops.cc).
--
-- A path outside the store is upstream's @isInStore@ refusal; one inside
-- it whose component is no store path is its @StorePath@ constructor's
-- ('storePathBaseNameError').
validateStorePath :: (MonadEval m) => Text -> m NixValue
validateStorePath p = case T.stripPrefix storeDirPrefix p of
  Just rest
    | not (T.null rest) -> case parseStorePathBaseName base of
        Just sp -> pure (VStr (TE.encodeUtf8 p) (plainContext sp))
        Nothing -> throwEvalError (fromMaybe ("store path '" <> base <> "' has no '-' after its hash") (storePathBaseNameError base))
    where
      base = T.takeWhile (/= '/') rest
  _ -> throwEvalError ("path '" <> p <> "' is not in the Nix store")

-- ---------------------------------------------------------------------------
-- Builtin implementations - Nix search path
-- ---------------------------------------------------------------------------

-- | Every entry is read before the looked-up name is forced, as upstream
-- Nix reads them, so an entry's failure comes ahead of a name's.
builtinFindFile :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinFindFile (VList cl) nameArg = do
  entries <- listThunks cl >>= mapM forceSearchEntry
  name <- argValue nameArg >>= structuredStringValue RefuseContext
  VPath <$> findFirst entries name
builtinFindFile other _ = expectedFailure "a list" other

-- | The path a name is found at in a search path's entries, failing as a
-- @\<name\>@ lookup does: upstream's @EvalState::findFile@, which both the
-- builtin and a command line's @\<name\>@ argument call (eval.cc, and
-- @lookupFileArg@ in common-eval-args.cc).
findInSearchPath :: (MonadEval m) => [Thunk] -> BS.ByteString -> m Text
findInSearchPath entryThunks name = mapM forceSearchEntry entryThunks >>= (`findFirst` name)

-- | Extract {prefix, path} from a search path entry thunk, as the bytes
-- the entry holds, read as upstream reads one: a set, whose @prefix@ is a
-- string without context, empty when it is absent, and whose @path@ is
-- coerced to a string without a copy, its context realised.  Upstream
-- keeps both as byte strings, so an entry whose prefix or path has no
-- UTF-8 reading is still an entry, and one that names something else does
-- not stop the lookup.
forceSearchEntry :: (MonadEval m) => Thunk -> m (BS.ByteString, BS.ByteString)
forceSearchEntry thunk =
  force thunk >>= \case
    VAttrs attrs -> do
      prefix <- attrSetLookup "prefix" attrs >>= maybe (pure BS.empty) (force >=> structuredStringValue RefuseContext)
      pathThunk <- attrSetLookup "path" attrs >>= maybe (throwEvalError "attribute 'path' missing") pure
      (path, ctx) <- force pathThunk >>= coerceToString False force applyValue coercePathVerbatim
      realiseSearchEntry path ctx
      pure (prefix, path)
    other -> expectedFailure "a set" other

-- | Realise the context of a search path entry's path before its lookup,
-- as upstream's @builtins.findFile@ realises it, naming the entry when a
-- path its context names is not valid (@prim_findFile@ in primops.cc).
realiseSearchEntry :: (MonadEval m) => BS.ByteString -> StringContext -> m ()
realiseSearchEntry path ctx =
  unless (contextIsEmpty ctx) $
    realiseContext ctx >>= either (throwEvalError . findFileRefusalMessage (bytesToTextLossy path)) pure

-- | A lookup that matched no entry.  A CATCHABLE error (upstream raises
-- ThrownError here): nixpkgs' impure.nix wraps @<nixpkgs-overlays>@ in
-- @tryEval@ and relies on catching the miss.  The wording is upstream's
-- @findFile@: under @pure-eval@ the search path is empty by construction,
-- and the message says so instead of pointing at @NIX_PATH@.
searchPathMiss :: (MonadEval m) => Text -> m a
searchPathMiss name = do
  policy <- evalPolicy
  throwCatchableError $
    if epPureEval policy
      then "cannot look up '<" <> name <> ">' in pure evaluation mode (use '--impure' to override)"
      else "file '" <> name <> "' was not found in the Nix search path (add it using $NIX_PATH or -I)"

-- | Iterate search path entries, checking for a match.  A miss is
-- 'searchPathMiss'.  The existence probe is the policy-gated one: a
-- candidate the policy refuses counts as absent, which is stricter than
-- the oracle (2.24.9 probes outside its accessor and would hand the path
-- back) and quieter than 2.33.2 (which refuses from here), and which
-- gives no caller an existence oracle over paths it may not read.
--
-- A matching entry's candidate is a path value's text, so one with no
-- UTF-8 reading is an error where upstream would probe the bytes; a lossy
-- spelling would probe a different file.
findFirst :: (MonadEval m) => [(BS.ByteString, BS.ByteString)] -> BS.ByteString -> m Text
findFirst [] name = searchPathMiss (bytesToTextLossy name)
findFirst ((prefix, path) : rest) name = case searchPathSuffix prefix name of
  Nothing -> findFirst rest name
  Just suffix -> do
    candidate <- canonPathValue <$> decodedText "builtins.findFile" (if BS.null suffix then path else path <> "/" <> suffix)
    exists <- doesPathExist ExistsAsEntry candidate
    if exists
      then pure candidate
      else findFirst rest name

-- | The part of a looked-up name below a search path prefix that names
-- it, compared byte for byte: upstream's
-- @LookupPath::Prefix::suffixIfPotentialMatch@ (src/libexpr/search-path.cc
-- at Nix 2.24.9).  An empty prefix names every lookup.
searchPathSuffix :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
searchPathSuffix prefix name
  | BS.null prefix = Just name
  | prefix == name = Just BS.empty
  | otherwise = BS.stripPrefix (prefix <> "/") name

-- ---------------------------------------------------------------------------
-- Builtin implementations - store file creation
-- ---------------------------------------------------------------------------

-- | Upstream reads the name with @forceStringNoCtx@ before it forces the
-- contents (@prim_toFile@ in primops.cc).
builtinToFile :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinToFile nameVal contentsArg = do
  name <- structuredStringValue RefuseContext nameVal
  argValue contentsArg >>= \case
    VStr contents ctx -> do
      fileName <- decodedText "builtins.toFile" name
      refs <- toFileRefs fileName ctx
      -- The contents are stored (and hashed) as their raw bytes.
      storePath <- writeToStore fileName contents refs
      -- Upstream returns a STRING whose context is the new path itself; the
      -- refs travel through the store path computation, not the eval context.
      let selfContext = maybe emptyContext plainContext (parseStorePath defaultStoreDir storePath)
      pure (VStr (TE.encodeUtf8 storePath) selfContext)
    other -> throwEvalError ("builtins.toFile: expected a string, got " <> typeName other)

-- | The store-path references a toFile output named so may carry: the
-- contents' plain-path context, in sorted order.  A derivation or
-- derivation-output reference is an error, as upstream makes it one, in
-- upstream's words, which name the file and the first such reference in
-- upstream's order of a context.
toFileRefs :: (MonadEval m) => Text -> StringContext -> m [StorePath]
toFileRefs fileName ctx = case [element | element <- inUpstreamOrder ctx, not (isPlain element)] of
  [] -> pure [sp | SCPlain sp <- Set.toAscList (unStringContext ctx)]
  element : _ ->
    throwEvalError ("files created by builtins.toFile may not reference derivations, but " <> fileName <> " references " <> encodedElement element)
  where
    isPlain element = case element of
      SCPlain _ -> True
      SCDrvOutput _ _ -> False
      SCAllOutputs _ -> False

-- ---------------------------------------------------------------------------
-- Builtin implementations - scoped import
-- ---------------------------------------------------------------------------

-- | Upstream's @import@ helper behind @scopedImport@ realises the path
-- before it forces the scope (primops.cc at 2.24.9), so the scope is
-- taken unforced.
builtinScopedImport :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinScopedImport scopeArg pathVal = do
  p <- realisePath "builtins.scopedImport" pathVal
  scopeVal <- argValue scopeArg
  case scopeVal of
    VAttrs attrs -> scopedImportFile attrs p
    other -> throwEvalError ("builtins.scopedImport: expected a set, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - network fetchers
-- ---------------------------------------------------------------------------

builtinFetchurl :: (MonadEval m) => NixValue -> m NixValue
builtinFetchurl (VStr url _) = do
  urlText <- decodedText "builtins.fetchurl" url
  fetchUrlSimple urlText Nothing
builtinFetchurl (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchurl" "url" attrs
  -- The pin is read with the arguments, before anything is fetched, as
  -- upstream's fetch in fetchTree.cc reads it.
  sha256 <- traverse decodeSha256Pin =<< forceOptionalAttrStr attrs "sha256"
  fetchUrlSimple url sha256
builtinFetchurl other =
  throwEvalError ("builtins.fetchurl: expected a string or set, got " <> typeName other)

builtinFetchTarball :: (MonadEval m) => NixValue -> m NixValue
builtinFetchTarball (VStr url _) = do
  urlText <- decodedText "builtins.fetchTarball" url
  fetchAndExtractTarball urlText Nothing tarballSourceName
builtinFetchTarball (VAttrs attrs) = do
  url <- forceAttrStr "builtins.fetchTarball" "url" attrs
  sha256 <- traverse decodeSha256Pin =<< forceOptionalAttrStr attrs "sha256"
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
fetchAndExtractTarball :: (MonadEval m) => Text -> Maybe BS.ByteString -> Text -> m NixValue
fetchAndExtractTarball url pin name = do
  checkUri url
  requireFetchPin "fetchTarball" pin
  held <- heldPinnedPath name NarIngestion pin
  maybe (downloadAndExtractTarball url pin name) (pure . storePathString) held

-- | The download and copy of 'fetchAndExtractTarball', for a fetch the
-- store does not hold already.
downloadAndExtractTarball :: (MonadEval m) => Text -> Maybe BS.ByteString -> Text -> m NixValue
downloadAndExtractTarball url pin name = do
  toolUrl <- localFileUrl url
  extractDir <- createScratchDir "nova-nix-tarball-"
  -- The -- separator prevents argument injection from the URL.
  (code, _, errOut) <-
    runProcess
      "sh"
      [ "-c",
        "curl -sSfL -- \"$1\" | tar -xz -C \"$2\" --strip-components=1",
        "--",
        toolUrl,
        extractDir
      ]
      ""
  case code of
    0 -> do
      storePath <-
        copyPathToStore
          Fetched
          extractDir
          name
          (fmap (\expected -> (expected, fetchHashMismatch url expected)) pin)
          `onEvalError` removeScratchDir extractDir
      removeScratchDir extractDir
      pure (storePathString storePath)
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

-- | What @git fetch@ asks for when the caller pinned no @ref@: the
-- remote's default branch.
defaultFetchRef :: Text
defaultFetchRef = "HEAD"

-- | What @git checkout@ resolves when the caller pinned no @rev@: the
-- tip of whatever the fetch just brought down.
fetchHeadRef :: Text
fetchHeadRef = "FETCH_HEAD"

-- | Characters upstream's @refRegexS@ allows after the first one.
gitRefTrailingPunctuation :: [Char]
gitRefTrailingPunctuation = "_./@+-"

-- | Validate a @ref@ against upstream's @refRegexS@
-- (@[a-zA-Z0-9\@][a-zA-Z0-9_.\/\@+-]*@).  The leading class excludes
-- @-@, and that is the load-bearing part: git parses options after the
-- remote name, so a @ref@ of @--upload-pack=\<cmd\>@ names the command
-- git runs as the transport.  Left carries the eval-error text.
checkGitRef :: Text -> Either Text Text
checkGitRef ref = case T.uncons ref of
  Just (leading, rest)
    | refLeadChar leading,
      T.all refChar rest ->
        Right ref
  _ -> Left ("builtins.fetchGit: ref is not a valid git ref name: " <> ref)
  where
    refLeadChar c = asciiAlphaNum c || c == '@'
    refChar c = asciiAlphaNum c || c `elem` gitRefTrailingPunctuation
    asciiAlphaNum c = isAsciiLower c || isAsciiUpper c || isDigit c

-- | Hex characters in a full SHA-1.
gitRevHexLength :: Int
gitRevHexLength = 40

-- | Validate a @rev@ against upstream's @revRegexS@ (@[0-9a-fA-F]{40}@).
-- Upstream parses it as a SHA-1 @Hash@ before it reaches git, which
-- rejects a tag or an abbreviated prefix as well as a leading @-@: a
-- \"pinned\" fetch that still resolves through git's DWIM rules is not
-- pinned.  Left carries the eval-error text.
checkGitRev :: Text -> Either Text Text
checkGitRev rev
  | T.length rev == gitRevHexLength,
    T.all isHexDigit rev =
      Right rev
  | otherwise = Left ("builtins.fetchGit: rev is not a full SHA-1: " <> rev)

-- | @-c@ config pinning the clone to 'allowedGitSchemes': every
-- transport defaults to never, each allowed scheme is re-enabled.  This
-- enforces what URL validation cannot see up front - a helper reached
-- indirectly, or a @remote-\<helper\>@ binary on PATH.
gitTransportConfig :: [Text]
gitTransportConfig =
  [ "-c",
    "protocol.allow=never",
    -- Keep checkout bytes platform-independent: no CRLF rewriting.
    "-c",
    "core.autocrlf=false",
    "-c",
    "core.eol=lf",
    -- Fail loudly rather than hash a symlink as a plain file (Git for
    -- Windows' fallback when it can't create real symlinks).
    "-c",
    "core.symlinks=true"
  ]
    ++ concat [["-c", "protocol." <> scheme <> ".allow=always"] | scheme <- allowedGitSchemes]

-- | The parsed arguments of a @builtins.fetchGit@ call.  A record
-- rather than positional threading: eight independent knobs invite
-- transposition, and the names keep every site readable.
data FetchGitArgs = FetchGitArgs
  { fgaUrl :: !Text,
    fgaName :: !Text,
    fgaRef :: !(Maybe Text),
    fgaRev :: !(Maybe Text),
    fgaSubmodules :: !Bool,
    fgaShallow :: !Bool,
    fgaAllRefs :: !Bool,
    fgaNarHash :: !(Maybe Text)
  }

-- | The attributes @builtins.fetchGit@ accepts.  Anything outside the
-- set is an error rather than a silent drop - an ignored attribute is
-- exactly how an unhonored pin stays invisible.  Upstream refuses the
-- same way ("input attribute \'...\' not supported by scheme \'git\'",
-- observed from nix-instantiate 2.33.2).
fetchGitSupportedAttrs :: [Text]
fetchGitSupportedAttrs = ["url", "name", "ref", "rev", "submodules", "shallow", "allRefs", "narHash"]

builtinFetchGit :: (MonadEval m) => NixValue -> m NixValue
builtinFetchGit (VStr rawUrl _) = do
  url <- decodedText "builtins.fetchGit" rawUrl
  fetchGit
    FetchGitArgs
      { fgaUrl = url,
        fgaName = "source",
        fgaRef = Nothing,
        fgaRev = Nothing,
        fgaSubmodules = False,
        fgaShallow = False,
        fgaAllRefs = False,
        fgaNarHash = Nothing
      }
builtinFetchGit (VAttrs attrs) = do
  names <- Map.keys <$> attrSetToMap attrs
  case filter (`notElem` fetchGitSupportedAttrs) names of
    (unsupported : _) ->
      throwEvalError ("builtins.fetchGit: attribute '" <> unsupported <> "' not supported")
    [] -> pure ()
  url <- forceAttrStr "builtins.fetchGit" "url" attrs
  name <- fromMaybe "source" <$> forceOptionalAttrStr attrs "name"
  ref <- forceOptionalAttrStr attrs "ref"
  rev <- forceOptionalAttrStr attrs "rev"
  submodules <- forceOptionalAttrBool "builtins.fetchGit" attrs "submodules" False
  shallow <- forceOptionalAttrBool "builtins.fetchGit" attrs "shallow" False
  allRefs <- forceOptionalAttrBool "builtins.fetchGit" attrs "allRefs" False
  narHash <- forceOptionalAttrStr attrs "narHash"
  fetchGit
    FetchGitArgs
      { fgaUrl = url,
        fgaName = name,
        fgaRef = ref,
        fgaRev = rev,
        fgaSubmodules = submodules,
        fgaShallow = shallow,
        fgaAllRefs = allRefs,
        fgaNarHash = narHash
      }
builtinFetchGit other =
  throwEvalError ("builtins.fetchGit: expected a string or set, got " <> typeName other)

-- | Fetch one revision into the store; returns upstream's @fetchGit@
-- attrset (@outPath@, @rev@, @shortRev@, @revCount@, @submodules@,
-- @lastModified@, @lastModifiedDate@, @narHash@).  @ref@ and @rev@ are
-- validated to upstream's shapes before they reach git (see
-- 'checkGitRef' and 'checkGitRev'); neither may lead with @-@.
fetchGit :: (MonadEval m) => FetchGitArgs -> m NixValue
fetchGit args =
  case (,,) <$> checkGitUrl (fgaUrl args) <*> traverse checkGitRef (fgaRef args) <*> traverse checkGitRev (fgaRev args) of
    Left err -> throwEvalError err
    Right (url, checkedRef, checkedRev) -> do
      let name = fgaName args
          submodules = fgaSubmodules args
          shallow = fgaShallow args
          inputUrl = gitInputUrl args {fgaUrl = url, fgaRef = checkedRef, fgaRev = checkedRev}
      -- Upstream's order (fetchTree.cc): the pure-mode lock check, then
      -- the URI gate, before anything is fetched or remembered.  A rev
      -- is what locks a git input at 2.24.9 (GitInputScheme::isLocked).
      policy <- evalPolicy
      when (epPureEval policy && isNothing checkedRev) $
        throwEvalError ("in pure evaluation mode, 'fetchGit' will not fetch unlocked input '" <> inputUrl <> "'")
      checkUri inputUrl
      -- A declared narHash is parsed before anything is fetched, as
      -- Input::fetchToStore reads it first (fetchers.cc at 2.24.9).
      declaredNarHash <- traverse (either throwEvalError pure . parseNarHash) (fgaNarHash args)
      -- Only a pinned rev is cached: a bare ref means "whatever this branch
      -- points at now", which a note taken earlier cannot answer.
      cached <- case checkedRev of
        Nothing -> pure Nothing
        Just rev -> do
          entry <- lookupFetchCache (fetchCacheKey url name rev submodules shallow)
          case entry >>= decodeFetchCache of
            Nothing -> pure Nothing
            Just fields -> do
              -- The note is only good while the tree it names is still
              -- there, and a hit has to re-record the write the fetch it
              -- stands in for would have recorded: on disk and registered
              -- are different facts, and a derivation naming an
              -- unregistered outPath fails to build.
              adopted <- adoptStorePath (fcStorePath fields)
              pure (if adopted then Just fields else Nothing)
      fields <- case cached of
        Just hit -> pure hit
        Nothing -> fetchGitUncached args {fgaUrl = url} checkedRef checkedRev
      -- A declared narHash pins the tree; a mismatch is an error on the
      -- hit and fresh paths alike, never a silent recompute.  The hashes
      -- are compared, not their spellings, as checkLocks compares them
      -- (fetchers.cc), and the pin is reported in SRI.  Upstream's
      -- orientation: "expected" is what the fetch produced, "got" is the
      -- declared pin (observed from nix-instantiate 2.33.2).
      case declaredNarHash of
        Nothing -> pure ()
        Just declared ->
          unless (base64ToBytes (fcNarHash fields) == Just declared) $
            throwEvalError
              ("builtins.fetchGit: NAR hash mismatch in '" <> url <> "', expected 'sha256-" <> fcNarHash fields <> "' but got 'sha256-" <> bytesToBase64 declared <> "'")
      fetchGitResult submodules fields

-- | The URL upstream renders a @fetchGit@ input as
-- (@fetchers::Input::toURLString@ at 2.24.9): the url after @fixGitURL@
-- (an scp-like @user\@host:path@ becomes @ssh://user\@host/path@, a bare
-- path becomes @file://@ plus the path), its scheme prefixed @git+@ unless
-- it is @git@ already, and the query carrying the input's attributes in
-- key order as @ParsedURL::to_string@ sorts them: @exportIgnore=1@ (which
-- @fetchGit@ sets unless submodules are on), @ref@, @rev@, @shallow=1@,
-- @submodules=1@.  This is the string @checkURI@ matches @allowed-uris@
-- against and the one the pure-mode refusal names, so an operator's
-- prefix has to be written for this form (@git+https://...@), as it does
-- upstream.
gitInputUrl :: FetchGitArgs -> Text
gitInputUrl args =
  let fixed = fixGitUrl (fgaUrl args)
      (scheme, afterScheme) = T.breakOn ":" fixed
      (authority, afterAuthority) = case T.stripPrefix "://" afterScheme of
        Just rest -> let (host, more) = T.break (\c -> c == '/' || c == '?' || c == '#') rest in (Just host, more)
        Nothing -> (Nothing, T.drop 1 afterScheme)
      (pathPart, afterPath) = T.break (\c -> c == '?' || c == '#') afterAuthority
      (queryPart, fragmentPart) = case T.uncons afterPath of
        Just ('?', rest) -> T.break (== '#') rest
        _ -> ("", afterPath)
      urlQuery = Map.fromList (map queryPair (filter (not . T.null) (T.splitOn "&" queryPart)))
      queryPair pair =
        let (k, v) = T.breakOn "=" pair
         in (percentDecode k, percentDecode (T.drop 1 v))
      pins =
        catMaybes
          [ ("rev",) . T.toLower <$> fgaRev args,
            ("ref",) <$> fgaRef args,
            ("shallow", "1") <$ guardFlag (fgaShallow args),
            ("submodules", "1") <$ guardFlag (fgaSubmodules args),
            ("exportIgnore", "1") <$ guardFlag (not (fgaSubmodules args))
          ]
      query = Map.union (Map.fromList pins) urlQuery
      renderedScheme = if scheme == gitScheme then scheme else "git+" <> scheme
      renderedQuery
        | Map.null query = ""
        | otherwise = "?" <> T.intercalate "&" [percentEncode queryKeep k <> "=" <> percentEncode queryKeep v | (k, v) <- Map.toAscList query]
      renderedFragment = case T.stripPrefix "#" fragmentPart of
        Just fragment | not (T.null fragment) -> "#" <> percentEncode "" (percentDecode fragment)
        _ -> ""
   in renderedScheme
        <> ":"
        <> maybe "" ("//" <>) authority
        <> percentEncode pathKeep (percentDecode pathPart)
        <> renderedQuery
        <> renderedFragment
  where
    guardFlag flag = if flag then Just () else Nothing

-- | The one scheme upstream leaves without a @git+@ prefix.
gitScheme :: Text
gitScheme = "git"

-- | Characters @ParsedURL::to_string@ leaves unencoded in a path and in a
-- query (@allowedInPath@ and @allowedInQuery@ in url.cc).
pathKeep, queryKeep :: Text
pathKeep = ":@/"
queryKeep = ":@/?"

-- | Upstream @fixGitURL@ (url.cc at 2.24.9): an scp-like remote not
-- starting with @/@ (@([^\/]*)\@(.*):(.*)@, the first group as long as a
-- match allows, the second greedy) is rewritten to @ssh://@; a @file:@ URL
-- and anything with @://@ pass through; a bare path becomes a @file://@
-- URL.
fixGitUrl :: Text -> Text
fixGitUrl url
  | not (T.isPrefixOf "/" url),
    Just (user, hostAndPath) <- scpSplit url =
      let (host, path) = T.breakOnEnd ":" hostAndPath
       in "ssh://" <> user <> "@" <> T.dropEnd 1 host <> "/" <> path
  | T.isPrefixOf "file:" url = url
  | not (T.isInfixOf "://" url) = "file://" <> url
  | otherwise = url
  where
    -- The longest slash-free prefix ending at an '@' whose remainder still
    -- carries a ':' is what the regex's backtracking selects.
    scpSplit u =
      let slashFree = T.takeWhile (/= '/') u
          candidates = [i | (i, c) <- zip [0 ..] (T.unpack slashFree), c == '@']
          viable i = T.any (== ':') (T.drop (i + 1) u)
       in case reverse (filter viable candidates) of
            (i : _) -> Just (T.take i u, T.drop (i + 1) u)
            [] -> Nothing

-- | @percentEncode@ from url.cc: unreserved characters and the kept set
-- pass, every other byte of the UTF-8 form becomes @%XX@.
percentEncode :: Text -> Text -> Text
percentEncode keep = T.concat . map encodeByte . BS.unpack . TE.encodeUtf8
  where
    encodeByte byte
      | unreserved byte || T.any (== chr (fromIntegral byte)) keep = T.singleton (chr (fromIntegral byte))
      | otherwise = T.pack ('%' : hexPair byte)
    unreserved byte =
      let c = chr (fromIntegral byte)
       in isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` unreservedPunctuation
    hexPair byte = map (toUpper . intToDigit) [fromIntegral byte `div` hexBase, fromIntegral byte `mod` hexBase]

-- | The unreserved punctuation of RFC 3986, kept as is.
unreservedPunctuation :: [Char]
unreservedPunctuation = "-._~"

hexBase :: Int
hexBase = 16

-- | @percentDecode@ from url.cc, over the UTF-8 bytes.  A malformed escape
-- is kept literally; upstream throws BadURL there, which this renderer
-- has no reason to reproduce for an error message's sake.
percentDecode :: Text -> Text
percentDecode = TE.decodeUtf8With lenientDecode . BS.pack . go . BS.unpack . TE.encodeUtf8
  where
    go (percent : hi : lo : rest)
      | percent == percentByte,
        isHexDigit (chr (fromIntegral hi)),
        isHexDigit (chr (fromIntegral lo)) =
          fromIntegral (digitToInt (chr (fromIntegral hi)) * hexBase + digitToInt (chr (fromIntegral lo))) : go rest
    go (byte : rest) = byte : go rest
    go [] = []
    percentByte = fromIntegral (ord '%')

-- | The fetch-everything refspec @allRefs@ selects, upstream's exact
-- spelling: every remote ref lands under the same local name, so a
-- pinned rev reachable from ANY ref becomes fetchable even when the
-- remote refuses a direct SHA want.
allRefsFetchSpec :: Text
allRefsFetchSpec = "refs/*:refs/*"

-- | The fetch itself, when nothing is remembered about it.  @checkedRef@
-- and @checkedRev@ have already passed 'checkGitRef'\/'checkGitRev'.
-- The scratch clone is removed on success once the tree is copied out,
-- and by 'onEvalError' when any step throws: a failing fetch must not
-- leave the clone behind.
fetchGitUncached :: (MonadEval m) => FetchGitArgs -> Maybe Text -> Maybe Text -> m FetchCache
fetchGitUncached args checkedRef checkedRev = do
  cloneDir <- createScratchDir "nova-nix-fetchgit-"
  fetchGitInClone args checkedRef checkedRev cloneDir
    `onEvalError` removeScratchDir cloneDir

-- | Every step of an uncached fetch that can throw, run under
-- 'fetchGitUncached'\'s scratch-dir cleanup.
fetchGitInClone :: (MonadEval m) => FetchGitArgs -> Maybe Text -> Maybe Text -> Text -> m FetchCache
fetchGitInClone args checkedRef checkedRev cloneDir = do
  let ctx = "builtins.fetchGit"
      git = gitRun ctx cloneDir
      url = fgaUrl args
      name = fgaName args
      submodules = fgaSubmodules args
      shallow = fgaShallow args
      depthArgs = if shallow then ["--depth", "1"] else []
      -- An allRefs fetch writes every remote ref, including the one the
      -- scratch clone's unborn HEAD names, which git refuses in a repo
      -- with a worktree (upstream fetches into a BARE cache repo and
      -- never hits this).  The refusal protects a checked-out branch
      -- from moving under the worktree; the explicit checkout below
      -- replaces the worktree state anyway, so allowing it is safe.
      updateHeadArgs = ["--update-head-ok" | fgaAllRefs args]
      -- Upstream's refspec ternary, in its order.  @allRefs@ fetches
      -- everything.  Otherwise a pinned rev is itself the refspec: a
      -- depth-1 fetch of a branch tip cannot contain any other revision,
      -- so asking the remote for the SHA is what lets @shallow@ and
      -- @rev@ compose (and what finds a rev not on the fetched ref).  A
      -- server may refuse a SHA it does not advertise; that surfaces as
      -- git's own fetch error, the same surface upstream has, and
      -- @allRefs = true@ is the caller's way around it.
      fetchTarget
        | fgaAllRefs args = allRefsFetchSpec
        | Just pinned <- checkedRev = pinned
        | otherwise = fromMaybe defaultFetchRef checkedRef
      -- The pinned rev when there is one; under an allRefs fetch with
      -- only a ref, that ref (FETCH_HEAD after a refs/* fetch is
      -- whichever ref arrived last, not the named one); otherwise
      -- whatever the fetch brought down.
      checkoutTarget = case (checkedRev, fgaAllRefs args, checkedRef) of
        (Just pinned, _, _) -> pinned
        (Nothing, True, Just ref) -> ref
        _ -> fetchHeadRef
  remote <- gitRemote url
  _ <- git ["init", "--quiet", "."]
  _ <- git ["remote", "add", "origin", "--", remote]
  -- @--@ before the remote: git keeps parsing options after it, so
  -- without a separator a refspec is indistinguishable from a flag.
  _ <- git (["fetch", "--quiet"] ++ depthArgs ++ updateHeadArgs ++ ["--", "origin", fetchTarget])
  -- Trailing @--@, not leading: @git checkout -- X@ reads X as a
  -- pathspec, the opposite of what a leading separator means elsewhere.
  _ <- git ["checkout", "--quiet", checkoutTarget, "--"]
  when submodules $
    void (git ["submodule", "update", "--init", "--recursive"])
  markExecutablesFromIndex ctx cloneDir
  rev <- git ["rev-parse", "HEAD"]
  -- Truncated history cannot answer rev-list: the count would be the
  -- fetch depth, not the revision's depth, and @revCount@ can reach a
  -- derivation's environment.  Upstream reports 0 under @shallow@ (its
  -- fetcher skips computing the attribute and eval fills the default;
  -- observed from nix-instantiate 2.33.2), so 0 is recorded here and
  -- cache hits replay it.
  revCount <-
    if shallow
      then pure 0
      else decodeDecimal ctx =<< git ["rev-list", "--count", "HEAD"]
  lastModifiedText <- git ["show", "-s", "--format=%ct", "HEAD"]
  removeGitMetadata ctx cloneDir
  storePath <- copyPathToStore Fetched cloneDir name Nothing
  narHash <- narHashOfPath cloneDir
  removeScratchDir cloneDir
  lastModified <- decodeDecimal ctx lastModifiedText
  let fields =
        FetchCache
          { fcStorePath = storePath,
            fcRev = rev,
            fcRevCount = revCount,
            fcLastModified = lastModified,
            fcNarHash = bytesToBase64 narHash
          }
  -- Remembered only for a pinned revision; see fetchGit.  @allRefs@ is
  -- deliberately not in the key: it changes only whether a fetch can
  -- succeed, never what a given revision's entry holds.
  case checkedRev of
    Nothing -> pure ()
    Just pinned -> writeFetchCache (fetchCacheKey url name pinned submodules shallow) (encodeFetchCache fields)
  pure fields

-- | Mark every file git records as executable (mode 100755) in the
-- checked-out tree.  A no-op on Unix; on Windows the mode has nowhere to
-- live on disk, so it's read back from the index before @.git@ is stripped.
markExecutablesFromIndex :: (MonadEval m) => Text -> Text -> m ()
markExecutablesFromIndex ctx cloneDir = do
  listing <- gitRun ctx cloneDir ["ls-files", "--recurse-submodules", "--stage", "-z"]
  mapM_ markOne (T.split (== '\0') listing)
  where
    markOne row
      | Just rest <- T.stripPrefix "100755 " row,
        (_, tabbed) <- T.breakOn "\t" rest,
        Just relPath <- T.stripPrefix "\t" tabbed,
        not (T.null relPath) =
          setExecutableFile (cloneDir <> "/" <> relPath)
      | otherwise = pure ()

-- | What @builtins.fetchGit@ hands back, from the few facts about a fetch
-- that are worth keeping.  Shared by the fetch and by the cache, so the two
-- cannot describe the same tree differently.
data FetchCache = FetchCache
  { fcStorePath :: !Text,
    fcRev :: !Text,
    fcRevCount :: !Integer,
    fcLastModified :: !Integer,
    fcNarHash :: !Text
  }
  deriving (Eq, Show)

fetchGitResult :: (MonadCData m) => Bool -> FetchCache -> m NixValue
fetchGitResult submodules fields =
  let lastModifiedDate =
        T.pack
          ( formatTime
              defaultTimeLocale
              "%Y%m%d%H%M%S"
              (posixSecondsToUTCTime (fromIntegral (fcLastModified fields)))
          )
   in valuesSet SetFetchGit $
        Map.fromList
          [ ("outPath", storePathString (canonPathValue (fcStorePath fields))),
            ("rev", mkStr (fcRev fields)),
            ("shortRev", mkStr (T.take 7 (fcRev fields))),
            ("revCount", VInt (fromIntegral (fcRevCount fields))),
            ("submodules", VBool submodules),
            ("lastModified", VInt (fromIntegral (fcLastModified fields))),
            ("lastModifiedDate", mkStr lastModifiedDate),
            ("narHash", mkStr ("sha256-" <> fcNarHash fields))
          ]

-- | What a remembered fetch is filed under.  Everything that decides what
-- comes back is in the key, so a hit cannot describe a different fetch: the
-- URL, the store name, the revision, whether submodules were taken, and
-- whether the clone was shallow.
--
-- @shallow@ is in the key even though it cannot change the tree, because it
-- decides @revCount@: a shallow fetch reports 0 where a full clone of the
-- same rev counts the real depth.  Sharing an entry between the two would
-- let whichever ran first decide what the other sees, and @revCount@ can
-- reach a derivation's environment, so that is a reproducibility problem
-- rather than a stale-data one.
--
-- The leading version tag invalidates every entry written before a change
-- in what the fields mean.
fetchCacheKey :: Text -> Text -> Text -> Bool -> Bool -> Text
fetchCacheKey url name rev submodules shallow =
  T.intercalate
    "\n"
    [ "fetchGit-2",
      url,
      name,
      rev,
      if submodules then "submodules" else "no-submodules",
      if shallow then "shallow" else "full"
    ]

-- | One field per line, in a fixed order.  Anything that does not read back
-- as exactly five lines is treated as no entry at all.
encodeFetchCache :: FetchCache -> Text
encodeFetchCache fields =
  T.intercalate
    "\n"
    [ fcStorePath fields,
      fcRev fields,
      T.pack (show (fcRevCount fields)),
      T.pack (show (fcLastModified fields)),
      fcNarHash fields
    ]

-- | Read back what 'encodeFetchCache' wrote: 'Nothing' unless there are
-- exactly five lines, both counts are decimal and the NAR hash is a whole
-- base64 SHA-256.
decodeFetchCache :: Text -> Maybe FetchCache
decodeFetchCache raw = case T.splitOn "\n" raw of
  [storePath, rev, revCount, lastModified, narHash] -> do
    count <- readDecimalMaybe revCount
    modified <- readDecimalMaybe lastModified
    -- Every field is checked, the last one included.  Truncation before
    -- the last field yields fewer than five and is rejected above, but a
    -- write cut inside narHash still yields five and would otherwise
    -- decode as valid with a short hash - and an entry is trusted for as
    -- long as its store path survives.
    validBase64Sha256 narHash
    pure
      FetchCache
        { fcStorePath = storePath,
          fcRev = rev,
          fcRevCount = count,
          fcLastModified = modified,
          fcNarHash = narHash
        }
  _ -> Nothing
  where
    readDecimalMaybe t = case TR.decimal t of
      Right (value, rest) | T.null rest -> Just value
      _ -> Nothing
    -- Base64 of 32 bytes: 44 characters, the last one the single pad.
    validBase64Sha256 t =
      if T.length t == 44 && T.all isBase64Char (T.init t) && T.last t == '='
        then Just ()
        else Nothing
    isBase64Char c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '+' || c == '/'

-- | Run one git subcommand under the same transport allowlist as the URL
-- check.  Returns trimmed stdout; a nonzero exit throws with git's stderr.
gitRun :: (MonadEval m) => Text -> Text -> [Text] -> m Text
gitRun ctx cloneDir args = do
  (code, out, err) <- runProcess "git" (gitTransportConfig ++ ["-C", cloneDir] ++ args) ""
  if code == 0
    then pure (T.strip out)
    else throwEvalError (ctx <> ": git " <> T.unwords args <> " failed: " <> err)

-- | The remote git is handed for a fetch's url: a local path, bare or as
-- a file URL, named on the drive the evaluator reads it on, where git,
-- run in the scratch clone, would read it on that directory's drive.  A
-- url with any other scheme, or an scp-like remote, is handed over as
-- given.
gitRemote :: (MonadEval m) => Text -> m Text
gitRemote url
  | T.isInfixOf "://" url = localFileUrl url
  | otherwise = qualifyPath url

-- | Strip every @.git@ entry (clone's own, and each submodule's) before the
-- tree is copied to the store.  Walked directly, not shelled out to
-- @find@\/@rm@: no @sh@ on a bare Windows install.
removeGitMetadata :: (MonadEval m) => Text -> Text -> m ()
removeGitMetadata _ctx = go
  where
    go dir = do
      entries <- listDirectory dir
      forM_ entries $ \(name, listedType) ->
        let path = dir <> "/" <> name
         in if name == ".git"
              then removeScratchDir path
              else do
                fileType <- entryFileType path listedType
                when (fileType == "directory") $ go path

-- | Parse a decimal from trusted git stdout (@rev-list --count@, @show
-- --format=%ct@); a failure means git itself misbehaved.
decodeDecimal :: (MonadEval m) => Text -> Text -> m Integer
decodeDecimal ctx t = case TR.decimal t of
  Right (n, rest) | T.null rest -> pure n
  _ -> throwEvalError (ctx <> ": expected a decimal integer from git, got " <> t)

-- | Fetch a URL and optionally verify its hash.
fetchUrlSimple :: (MonadEval m) => Text -> Maybe BS.ByteString -> m NixValue
fetchUrlSimple url pin = do
  -- Upstream's order (fetchTree.cc, fetch()): the URI gate, then the
  -- name, then pure mode's hash requirement, all before the download.
  checkUri url
  -- The store name derives from the URL alone, so an unusable name fails
  -- here, before the download side effect.  Upstream rejects the same
  -- names when it adds the fetched file to the store.
  let name = urlBaseName url
  case checkStorePathName name of
    Left err -> throwEvalError ("builtins.fetchurl: " <> storePathNameErrorText err)
    Right () -> pure ()
  requireFetchPin "fetchurl" pin
  held <- heldPinnedPath name FlatIngestion pin
  case held of
    Just storePath -> pure (storePathString storePath)
    Nothing -> do
      -- Download to a file, not through the text-mode stdout pipe (which
      -- mangles binary), read the raw bytes, verify the pin if one was
      -- given, then store at the canonical fixed-output path.  The file
      -- lives in an exclusively created scratch directory
      -- ('createScratchDir'), removed once read.
      scratch <- createScratchDir "nova-nix-fetchurl-"
      bytes <- downloadInto scratch url `onEvalError` removeScratchDir scratch
      removeScratchDir scratch
      forM_ pin $ \expected ->
        let got = sha256Digest bytes
         in when (expected /= got) $ throwEvalError (fetchHashMismatch url expected got)
      storePath <- addFixedOutputFile Fetched name bytes
      pure (storePathString storePath)

-- | Upstream's early exit for a fetch a sha256 pins (@fetch@ in
-- fetchTree.cc at 2.24.9): the path the pin names, when the store holds
-- it already, which the fetch then returns without downloading or writing
-- anything.  2.34.9 ensures the path instead (@fetch@ in fetchTree.cc): a
-- valid one returns at once (@Store::ensurePath@ in
-- build\/entry-points.cc), and one the store lacks is substituted where a
-- substituter has it, by a daemon, which is not read-only (the
-- @EnsurePath@ case of @performOp@ in daemon.cc), or refused by a local
-- store in read-only mode (@PathSubstitutionGoal::init@ in
-- substitution-goal.cc) and then downloaded.  nova-nix downloads it.
heldPinnedPath :: (MonadEval m) => Text -> FileIngestionMethod -> Maybe BS.ByteString -> m (Maybe Text)
heldPinnedPath _ _ Nothing = pure Nothing
heldPinnedPath name method (Just pin) = do
  sp <- storePathOrThrow (makeFixedOutputPath name "sha256" method pin)
  held <- pinnedFetchHeld sp
  pure (if held then Just (storePathToText defaultStoreDir sp) else Nothing)

-- | Fetch a URL into the scratch directory and hand back its bytes.
downloadInto :: (MonadEval m) => Text -> Text -> m BS.ByteString
downloadInto scratch url = do
  let tmpFile = scratch <> "/" <> downloadFileName
  toolUrl <- localFileUrl url
  (code, _, stderr) <- runProcess "curl" ["-sSfL", "-o", tmpFile, "--", toolUrl] ""
  if code /= 0
    then throwEvalError ("builtins.fetchurl: fetch failed: " <> stderr)
    else readFileBytes tmpFile

-- | The downloaded file's name inside its scratch directory.
downloadFileName :: Text
downloadFileName = "download"

-- | The URL a fetch hands its tool: a @file://@ URL's path named on the
-- drive the evaluator reads it on ('qualifyPath'), so the tool reads the
-- file 'checkUri' checked rather than one on its own current drive.  A
-- drive it gains is spelled @file://C:/...@, the form 'checkUri' reads as
-- that drive's path.  Any other URL is handed over as given.
localFileUrl :: (MonadEval m) => Text -> m Text
localFileUrl url = case T.stripPrefix fileUrlPrefix url of
  Just path -> (fileUrlPrefix <>) <$> qualifyPath path
  Nothing -> pure url

-- | Upstream's pure-mode rule for @fetchurl@ and @fetchTarball@
-- (fetchTree.cc): without a @sha256@ the fetch is refused, since its
-- result would be whatever the network answers today.
requireFetchPin :: (MonadEval m) => Text -> Maybe BS.ByteString -> m ()
requireFetchPin who pin = do
  policy <- evalPolicy
  when (epPureEval policy && isNothing pin) $
    throwEvalError ("in pure evaluation mode, '" <> who <> "' requires a 'sha256' argument")

-- | Store name for a fetched URL: its basename (minus query/fragment), matching
-- C++ Nix's @baseNameOf url@ default so the fixed-output path agrees with it.
urlBaseName :: Text -> Text
urlBaseName url =
  let stripped = T.takeWhile (\c -> c /= '?' && c /= '#') url
      base = T.takeWhileEnd (/= '/') stripped
   in if T.null base then "source" else base

-- | Upstream's refusal of a fetch whose content has another SHA-256 hash
-- than its pin (@fetch@ in fetchTree.cc), each hash in nix32 after
-- its algorithm.
fetchHashMismatch :: Text -> BS.ByteString -> BS.ByteString -> Text
fetchHashMismatch url expected got =
  "hash mismatch in file downloaded from '"
    <> url
    <> "':\n  specified: sha256:"
    <> Nix32.encode expected
    <> "\n  got:       sha256:"
    <> Nix32.encode got

-- | Decode a sha256 pin to raw bytes as upstream's @newHashAllowEmpty@
-- with SHA-256 reads it (hash.cc): SRI, prefixed or bare, a spelling that
-- carries its own tag naming sha256, or empty, the digest whose every byte is
-- zero, with upstream's warning.
decodeSha256Pin :: (MonadEval m) => Text -> m BS.ByteString
decodeSha256Pin s = snd <$> newHashAllowEmpty (Just "sha256") s

-- | Force a required string attribute from an attrset, using full Nix string
-- coercion (VStr, VPath, VInt, VBool, VAttrs via __toString/outPath).
-- A coercion failure (or a throwing attr value) propagates with its own
-- message, as upstream's does.
-- The Text result decodes strictly: callers use it as a name, URL, or
-- platform string, none of which may carry invalid UTF-8 here.
forceAttrStr :: (MonadEval m) => Text -> Text -> AttrSet -> m Text
forceAttrStr builtin key attrs = do
  bytes <- forceAttrBytes builtin key attrs
  decodedText builtin bytes

-- | Like 'forceAttrStr' but returns the coerced RAW BYTES.
forceAttrBytes :: (MonadEval m) => Text -> Text -> AttrSet -> m BS.ByteString
forceAttrBytes builtin key attrs =
  attrSetLookup key attrs >>= \case
    Nothing -> throwEvalError (builtin <> ": missing required attribute '" <> key <> "'")
    Just thunk -> do
      val <- force thunk
      (s, _ctx) <- coerceToString True force applyValue coercePathVerbatim val
      pure s

-- | Force an optional string attribute via full Nix coercion.  A present
-- but uncoercible (or throwing) value is an error, not 'Nothing'.
forceOptionalAttrStr :: (MonadEval m) => AttrSet -> Text -> m (Maybe Text)
forceOptionalAttrStr attrs key = attrSetLookup key attrs >>= traverse forceAttrValueStr

-- | An attribute's value forced and coerced to a string, as
-- 'forceOptionalAttrStr' reads one it finds.
forceAttrValueStr :: (MonadEval m) => Thunk -> m Text
forceAttrValueStr thunk = do
  val <- force thunk
  (s, _ctx) <- coerceToString True force applyValue coercePathVerbatim val
  decodedText "string attribute" s

-- | Force an optional boolean attribute; present but non-boolean is an error,
-- not a silent coercion.
forceOptionalAttrBool :: (MonadEval m) => Text -> AttrSet -> Text -> Bool -> m Bool
forceOptionalAttrBool builtin attrs key def =
  attrSetLookup key attrs >>= \case
    Nothing -> pure def
    Just thunk -> do
      val <- force thunk
      case val of
        VBool b -> pure b
        other -> throwEvalError (builtin <> ": attribute '" <> key <> "' should be a bool, but is " <> typeName other)

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
    inputSubst <- inputModuloHashes (drvInputDrvs drv)
    pure (bytesToHexText (sha256Digest (toATermForHash False (Just inputSubst) drv)))

-- | The modulo hash (hex) of each input derivation, with the outputs taken
-- from it, in path order, as 'Nix.Derivation.inputAddressed' and
-- 'toATermForHash' take them ('resolveInputModulo').
inputModuloHashes :: (MonadEval m) => Map StorePath [Text] -> m [(Text, [Text])]
inputModuloHashes = mapM resolveInputModulo . Map.toList

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
resolveAllOutputNames sp = map doName . drvOutputs <$> derivationAt sp

-- | The derivation a @.drv@ path names, from this session or the store.
derivationAt :: (MonadEval m) => StorePath -> m Derivation
derivationAt sp = do
  let drvPathText = storePathToText defaultStoreDir sp
  session <- lookupSessionDrv drvPathText
  case session of
    Just drv -> pure drv
    Nothing -> do
      onDisk <- readStoreDerivation sp
      case onDisk of
        Just drv -> pure drv
        Nothing ->
          throwEvalError
            ( "derivation: cannot resolve the output names of the all-outputs reference "
                <> drvPathText
                <> " - it was not evaluated in this session and its .drv is not "
                <> "readable from the store (all-outputs references need the IO evaluator)"
            )

-- | The filesystem closure of the @.drv@ files an all-outputs context
-- names, as upstream's derivationStrict takes it for each
-- (@computeFSClosure@ in @derivationStrictInternal@, primops.cc): every
-- @.drv@ reachable through @inputDrvs@, every source in their @inputSrcs@,
-- and the references this session recorded for a source it wrote.
allOutputsClosure :: (MonadEval m) => [StorePath] -> m (Set.Set StorePath)
allOutputsClosure = go Set.empty
  where
    go !seen [] = pure seen
    go !seen (sp : rest)
      | Set.member sp seen = go seen rest
      | ".drv" `T.isSuffixOf` spName sp = do
          drv <- derivationAt sp
          go (Set.insert sp seen) (Map.keys (drvInputDrvs drv) ++ drvInputSrcs drv ++ rest)
      | otherwise = do
          refs <- fromMaybe [] <$> lookupSessionReferences sp
          go (Set.insert sp seen) (refs ++ rest)

-- | Eager derivation computation - @builtins.derivationStrict@.  Forces all
-- input attrs into env vars (under @__structuredAttrs@, into the one
-- @__json@ entry), content-hashes, and returns upstream's result
-- set: @drvPath@ and one attribute per output holding that output's path,
-- each carrying its store context.
-- The @derivation@ wrapper ("Nix.Builtins") reaches this only through
-- @outPath@ and @drvPath@, so forcing a derivation to WHNF never runs it.
builtinDerivationStrict :: (MonadEval m) => NixValue -> m NixValue
builtinDerivationStrict (VAttrs attrs) = do
  -- Extract required attributes.  The name and system are Text (they feed
  -- store-path and platform machinery); the builder stays raw bytes - it
  -- lands byte-exact in the ATerm's builder field.
  -- prim_derivationStrict names the attribute outside the line its
  -- forceStringNoCtx adds.
  let nameTrace =
        traceIfPresent attrs "name" "while evaluating the derivation attribute 'name'"
          . traceIfPresent attrs "name" "while evaluating the `name` attribute passed to builtins.derivationStrict"
  -- Read with forceStringNoCtx (prim_derivationStrict in primops.cc): a
  -- string with no context, never a value coerced to one.  An absent name
  -- is getAttr's error.
  drvName <-
    nameTrace $
      attrSetLookup "name" attrs >>= \case
        Nothing -> throwEvalError "attribute 'name' missing"
        Just thunk -> decodedText "derivation" =<< structuredStringValue RefuseContext =<< force thunk
  -- The name becomes the store-path name of the .drv and of every output,
  -- so it must satisfy the store-path name rules.  The path constructors
  -- re-check what they build; this early check reports the offending
  -- FIELD rather than a composed path name.
  case checkStorePathName drvName of
    Left err ->
      throwEvalError ("invalid derivation name: " <> storePathNameUpstreamText err <> ". Please pass a different 'name'.")
    Right () -> pure ()
  passing <-
    addErrorTrace "while evaluating the `__structuredAttrs` attribute passed to builtins.derivationStrict" $
      attrPassingOf attrs
  -- __ignoreNulls: when true, null-valued attrs are dropped from the
  -- derivation env (stdenv.mkDerivation sets it); when absent or false,
  -- null coerces to "" like any other coerceMore value - C++ Nix semantics.
  nulls <-
    attrSetLookup ignoreNullsFlag attrs >>= \case
      Nothing -> pure KeepNullAttrs
      Just thunk ->
        addErrorTrace "while evaluating the `__ignoreNulls` attribute passed to builtins.derivationStrict" $ do
          ignore <- force thunk >>= expectBool
          pure (if ignore then IgnoreNullAttrs else KeepNullAttrs)
  let fieldContext = "derivation \"" <> drvName <> "\""
  attrsInOrder <- Map.toAscList . Map.delete ignoreNullsFlag <$> attrSetToMap attrs
  -- Each attribute is one environment entry, or one member of a single
  -- JSON object under __json for a structured derivation.  The per-output
  -- env vars ($out, ...) are added below in both modes.
  (drvEnvPairs, fields) <- case passing of
    PassAsEnvironment -> do
      (entries, passed) <- derivationPass drvName nulls readEnvEntry attrsInOrder
      pure (Map.toList entries, passed)
    PassAsStructuredAttrs -> do
      (members, passed) <- derivationPass drvName nulls (readStructuredEntry drvName fieldContext) attrsInOrder
      pure ([(structuredAttrsKey, encodeStructuredAttrs (StructuredAttrs members))], passed)

  let fullContext = dfContext fields
      builtInputDrvs = extractInputDrvs fullContext
      allOutputRefs = extractAllOutputRefs fullContext
  -- All-outputs (DrvDeep) references - e.g. an embedded @dep.drvPath@ - add
  -- the referenced .drv's whole closure, as upstream's derivationStrict
  -- does: every path in it to inputSrcs, every .drv in it to inputDrvs with
  -- ALL its output names.  Merged in BEFORE drvRefs and the modulo
  -- substitution so both the dependent's own .drv hash and its output paths
  -- account for the reference.
  deepClosure <- allOutputsClosure allOutputRefs
  deepInputDrvs <-
    Map.fromList
      <$> mapM (\drvSp -> (,) drvSp <$> resolveAllOutputNames drvSp) (filter ((".drv" `T.isSuffixOf`) . spName) (Set.toList deepClosure))
  -- Upstream checks both once its pass and the closure are done, the
  -- builder first, and an empty one counts as absent
  -- (derivationStrictInternal in primops.cc).
  let requiredField key value = case value of
        Just bytes | not (BS.null bytes) -> pure bytes
        _ -> throwEvalError ("required attribute '" <> key <> "' missing")
  builder <- requiredField builderAttr (dfBuilder fields)
  system <- decodedText fieldContext =<< requiredField systemAttr (dfSystem fields)
  let outputNames = NE.toList (dfOutputs fields)
      inputDrvs = Map.unionWith (++) builtInputDrvs deepInputDrvs
      inputSrcs = Set.toList (Set.fromList (extractInputSrcs fullContext) <> deepClosure)
      platform = textToPlatform system
      baseEnv = Map.fromList drvEnvPairs
      -- Build a Derivation sharing this call's inputs/platform/builder/args.
      mkDrv outs env =
        Derivation
          { drvOutputs = outs,
            drvInputDrvs = inputDrvs,
            drvInputSrcs = inputSrcs,
            drvPlatform = platform,
            drvBuilder = builder,
            drvArgs = dfArgs fields,
            drvEnv = env
          }
      -- An output carrying only its name, for 'inputAddressed' to fill in.
      pendingOutput name = DerivationOutput name maskedOutputPath "" ""

  -- Fixed-output derivations (fetchurl etc.) are content-addressed and hash
  -- via the @fixed:out:@ scheme; input-addressed derivations recurse through
  -- the modulo hashes of their inputs.
  mFixed <- traverse (fixedOutputOf fields) (dfOutputHash fields)

  -- Each output name composes into a store-path name (drvName-<output>)
  -- and names the on-disk location the builder later clears and moves
  -- onto, so it must satisfy the same store-path name rules.  Upstream
  -- refuses such a name when it builds the output's path, past its pass
  -- and its presence checks.
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

  -- Path construction returns Left on a name the store rules reject; the
  -- field checks above make that unreachable here except through
  -- composition (drvName <> ".drv", drvName-output past the length cap),
  -- which only the constructors see, and which they refuse in upstream's
  -- words, naming the path.
  (file, moduloHash) <- case mFixed of
    Just (foAlgo, foMethod, foDigest) -> do
      foPath <- storePathOrThrow (makeFixedOutputPath drvName foAlgo foMethod foDigest)
      let foPathText = storePathToText defaultStoreDir foPath
          algoField = fileIngestionPrefix foMethod <> foAlgo
          foHashHex = bytesToHexText foDigest
          foModulo =
            sha256Digest
              (TE.encodeUtf8 ("fixed:out:" <> algoField <> ":" <> foHashHex <> ":" <> foPathText))
      file <-
        storePathOrThrow . drvFile drvName $
          mkDrv
            [DerivationOutput "out" foPath algoField foHashHex]
            (Map.insert "out" (TE.encodeUtf8 foPathText) baseEnv)
      pure (file, foModulo)
    Nothing -> do
      inputSubst <- inputModuloHashes inputDrvs
      file <- storePathOrThrow (inputAddressed drvName inputSubst (mkDrv (map pendingOutput outputNames) baseEnv))
      -- The unmasked modulo hash (real outputs, inputs substituted), cached
      -- for when this derivation is itself an input to another.
      pure (file, sha256Digest (toATermForHash False (Just inputSubst) (drvFileDerivation file)))

  -- The .drv's bytes are rendered once, as upstream's writeDerivation hashes
  -- and writes the one unparse.  They are recorded whole so the build driver
  -- can materialize the entire input-.drv closure before building; bottom-up
  -- eval guarantees every transitive input is recorded by the time a
  -- dependent is.
  let drvSP = drvFilePath file
      drvPathText = storePathToText defaultStoreDir drvSP
  cacheDrvHash drvPathText (bytesToHexText moduloHash)
  recordDrvAterm drvPathText (drvFileATerm file)

  -- drvPath carries the whole derivation (upstream's DrvDeep); an output
  -- path carries that one output of it.
  let drvPathCtx = StringContext (Set.singleton (SCAllOutputs drvSP))
      outPathCtx outName = StringContext (Set.singleton (SCDrvOutput drvSP outName))
      outputEntry output =
        (doName output, VStr (TE.encodeUtf8 (storePathToText defaultStoreDir (doPath output))) (outPathCtx (doName output)))
  valuesSet SetDerivationStrict . Map.fromList $
    ("drvPath", VStr (TE.encodeUtf8 drvPathText) drvPathCtx) : map outputEntry (drvOutputs (drvFileDerivation file))
builtinDerivationStrict other =
  throwEvalError ("derivation: expected a set, got " <> typeName other)

-- | The output of a derivation whose pass read an @outputHash@, as
-- upstream's derivationStrict makes a fixed-output one
-- (@derivationStrictInternal@ in primops.cc): its outputs must be @out@
-- alone, and the hash is read with 'newHashAllowEmpty' under the algorithm
-- @outputHashAlgo@ names, so the hash's own tag decides when that names
-- none upstream knows.  Returns the algorithm, the method the pass read
-- from @outputHashMode@, flat when it read none, and the digest.
fixedOutputOf :: (MonadEval m) => DrvFields -> BS.ByteString -> m (Text, FileIngestionMethod, BS.ByteString)
fixedOutputOf fields rawHash = do
  unless (dfOutputs fields == "out" :| []) $
    throwEvalError "multiple outputs are not supported in fixed-output derivations"
  ohash <- decodedText "derivation: outputHash" rawHash
  (algo, digest) <- newHashAllowEmpty (dfOutputHashAlgo fields) ohash
  pure (algo, fromMaybe FlatIngestion (dfOutputHashMode fields), digest)

-- | Whether a derivation leaves its null attributes out (@__ignoreNulls@).
data NullAttrs = KeepNullAttrs | IgnoreNullAttrs

-- | The attribute choosing whether null attributes are left out.  Upstream
-- reads it before its pass and leaves it out of the pass.
ignoreNullsFlag :: Text
ignoreNullsFlag = "__ignoreNulls"

-- | The attributes derivationStrict reads at their keys.
argsAttr, builderAttr, outputHashAttr, outputHashAlgoAttr, outputHashModeAttr, outputsAttr, systemAttr :: Text
argsAttr = "args"
builderAttr = "builder"
outputHashAttr = "outputHash"
outputHashAlgoAttr = "outputHashAlgo"
outputHashModeAttr = "outputHashMode"
outputsAttr = "outputs"
systemAttr = "system"

-- | What derivationStrict's pass does with an attribute once it is forced.
data DrvAttrUse
  = -- | @args@: the builder's command line, in neither the environment nor
    -- the JSON object.
    CommandLine
  | -- | An environment entry or JSON member, which may also be read as one
    -- of the derivation's fields.
    Entry !DrvEntryRole

-- | The field of a derivation an entry is read as, at its key.
data DrvEntryRole = BuilderEntry | SystemEntry | OutputsEntry | FixedOutputEntry !FixedOutputField | PlainEntry

-- | An attribute that says what a fixed-output derivation's output is.
data FixedOutputField = OutputHashField | OutputHashAlgoField | OutputHashModeField

-- | The use of the attribute at a key.
drvAttrUse :: Text -> DrvAttrUse
drvAttrUse key
  | key == argsAttr = CommandLine
  | key == builderAttr = Entry BuilderEntry
  | key == systemAttr = Entry SystemEntry
  | key == outputsAttr = Entry OutputsEntry
  | key == outputHashAttr = Entry (FixedOutputEntry OutputHashField)
  | key == outputHashAlgoAttr = Entry (FixedOutputEntry OutputHashAlgoField)
  | key == outputHashModeAttr = Entry (FixedOutputEntry OutputHashModeField)
  | otherwise = Entry PlainEntry

-- | What derivationStrict's pass has read of a derivation's fields: the
-- context every entry and argument carries, the builder, the system, the
-- outputs, the builder's arguments, and the fixed-output hash, its
-- algorithm and its mode.  The outputs are @out@ until an @outputs@
-- attribute names others, as upstream's set starts.  The hash is the
-- string its attribute was read as, an empty one included: a derivation
-- whose pass reads one is fixed-output, as upstream's is
-- (@derivationStrictInternal@ in primops.cc).  The algorithm is one
-- upstream knows, or none.  The mode is the method an @outputHashMode@
-- names, read in the pass whether or not the derivation is fixed-output, as
-- upstream's @handleHashMode@ reads it.
data DrvFields = DrvFields
  { dfContext :: !StringContext,
    dfBuilder :: !(Maybe BS.ByteString),
    dfSystem :: !(Maybe BS.ByteString),
    dfOutputs :: !(NonEmpty Text),
    dfArgs :: ![BS.ByteString],
    dfOutputHash :: !(Maybe BS.ByteString),
    dfOutputHashAlgo :: !(Maybe Text),
    dfOutputHashMode :: !(Maybe FileIngestionMethod)
  }

-- | Upstream's one pass over a derivation's attributes
-- (@derivationStrictInternal@ in primops.cc), given in name order: each is
-- forced, a null one is left out under @__ignoreNulls@, @args@ is read as
-- the builder's command line, and every other one is read by the mode's
-- reader into its entry and any field it is at the key of.  Each attribute
-- is processed under upstream's line naming it, and a failure fails the
-- whole derivation, so the first attribute by name that fails is the
-- failure, whatever a later one holds.
derivationPass ::
  (MonadEval m) =>
  Text ->
  NullAttrs ->
  (DrvEntryRole -> Text -> NixValue -> DrvFields -> m (Maybe entry, DrvFields)) ->
  [(Text, Thunk)] ->
  m (Map Text entry, DrvFields)
derivationPass drvName nulls readEntry = foldM step (Map.empty, initial)
  where
    initial = DrvFields mempty Nothing Nothing ("out" :| []) [] Nothing Nothing Nothing
    step (!entries, !fields) (key, thunk) = addErrorTrace (derivationAttrTrace drvName key) $ do
      val <- force thunk
      case (val, nulls, drvAttrUse key) of
        (VNull, IgnoreNullAttrs, _) -> pure (entries, fields)
        (_, _, CommandLine) -> (,) entries <$> readCommandLine val fields
        (_, _, Entry role) -> do
          (entry, updated) <- readEntry role key val fields
          pure (maybe entries (\e -> Map.insert key e entries) entry, updated)

-- | @args@, each element coerced as an environment entry is
-- (@derivationStrictInternal@ in primops.cc).  A path in it (stdenv's
-- ./default-builder.sh, say) is copied into the store, and its store path
-- reaches inputSrcs through the context.
readCommandLine :: (MonadEval m) => NixValue -> DrvFields -> m DrvFields
readCommandLine val fields = case val of
  VList cl -> do
    parts <- listThunks cl >>= mapM (force >=> coerceToStoreString)
    pure fields {dfArgs = map fst parts, dfContext = dfContext fields <> foldMap snd parts}
  _ -> throwEvalError "derivation: 'args' must be a list of strings"

-- | An entry of a derivation that passes its attributes through its
-- environment: the string upstream's @coerceToString@ makes of the value
-- with @coerceMore@, a path copied to the store, whose bytes flow
-- byte-exact into the ATerm env section (and its hash).  That string is
-- the builder, the system, the outputs or a fixed-output field at those
-- keys, the outputs split at whitespace (@derivationStrictInternal@ in
-- primops.cc), so an @outputHash@ that is not a string is the string it
-- coerces to.
readEnvEntry :: (MonadEval m) => DrvEntryRole -> Text -> NixValue -> DrvFields -> m (Maybe BS.ByteString, DrvFields)
readEnvEntry role _ val fields = do
  (bytes, ctx) <- coerceToStoreString val
  let entered = fields {dfContext = dfContext fields <> ctx}
  updated <- case role of
    BuilderEntry -> pure entered {dfBuilder = Just bytes}
    SystemEntry -> pure entered {dfSystem = Just bytes}
    OutputsEntry -> do
      outputs <- checkedOutputs =<< traverse (decodedText "derivation") (outputTokens bytes)
      pure entered {dfOutputs = outputs}
    FixedOutputEntry field -> withFixedOutputField field bytes entered
    PlainEntry -> pure entered
  pure (Just bytes, updated)

-- | An entry of a structured derivation: a member of its JSON object,
-- converted as @builtins.toJSON@ converts it (upstream calls the same
-- @printValueAsJSON@), with the builder (and its context), the system,
-- the outputs and the fixed-output fields read as strings at their keys,
-- never as values coerced to them (@derivationStrictInternal@ in
-- primops.cc).  The attribute that chooses structured attributes has no
-- member.
readStructuredEntry :: (MonadEval m) => Text -> Text -> DrvEntryRole -> Text -> NixValue -> DrvFields -> m (Maybe Json, DrvFields)
readStructuredEntry drvName fieldContext role key val fields
  | key == structuredAttrsFlag = pure (Nothing, fields)
  | otherwise = do
      (json, ctx) <- valueToJSON val
      let entered = fields {dfContext = dfContext fields <> ctx}
      updated <- case role of
        BuilderEntry -> (\bytes -> entered {dfBuilder = Just bytes}) <$> structuredStringValue AllowContext val
        SystemEntry -> (\bytes -> entered {dfSystem = Just bytes}) <$> structuredStringValue RefuseContext val
        OutputsEntry -> case val of
          VList cl -> do
            outputs <- checkedOutputs =<< (listThunks cl >>= mapM (structuredOutputName fieldContext))
            pure entered {dfOutputs = outputs}
          _ -> throwEvalError "derivation: 'outputs' must be a list of strings"
        FixedOutputEntry field -> structuredStringValue RefuseContext val >>= \bytes -> withFixedOutputField field bytes entered
        PlainEntry -> pure entered
      when (Set.member key structuredDisabledChecks) $
        warnMessage (disabledByStructuredAttrs drvName key)
      pure (Just json, updated)

-- | A fixed-output field read from its attribute's string.  The algorithm
-- is read with @parseHashAlgoOpt@ (@derivationStrictInternal@ in primops.cc),
-- which gives none for a name it does not know, and the mode with
-- 'outputHashModeMethod', which refuses one it does not take.
withFixedOutputField :: (MonadEval m) => FixedOutputField -> BS.ByteString -> DrvFields -> m DrvFields
withFixedOutputField field bytes fields = case field of
  OutputHashField -> pure fields {dfOutputHash = Just bytes}
  OutputHashAlgoField -> pure fields {dfOutputHashAlgo = knownHashAlgo bytes}
  OutputHashModeField -> either throwEvalError (\method -> pure fields {dfOutputHashMode = Just method}) (outputHashModeMethod bytes)

-- | The method an @outputHashMode@ names, as upstream's @handleHashMode@
-- reads it (primops.cc) through @ContentAddressMethod::parse@
-- (content-address.cc) and @parseFileIngestionMethod@
-- (file-content-address.cc): @nar@, and @recursive@ as its older name, hash
-- a NAR, and @flat@ the file's bytes.  @text@ and @git@ parse there too but
-- need the @dynamic-derivations@ and @git-hashing@ experimental features,
-- which nova-nix does not implement, so each is refused as upstream refuses
-- it with its feature off.
outputHashModeMethod :: BS.ByteString -> Either Text FileIngestionMethod
outputHashModeMethod mode = case mode of
  "nar" -> Right NarIngestion
  "recursive" -> Right NarIngestion
  "flat" -> Right FlatIngestion
  "text" -> Left (experimentalFeatureDisabled "dynamic-derivations")
  "git" -> Left (experimentalFeatureDisabled "git-hashing")
  _ -> Left ("invalid value '" <> bytesToTextLossy mode <> "' for 'outputHashMode' attribute")

-- | Upstream's refusal of what an experimental feature that is off would
-- allow (@MissingExperimentalFeature@ in experimental-features.cc at
-- 2.24.9).
experimentalFeatureDisabled :: Text -> Text
experimentalFeatureDisabled feature =
  "experimental Nix feature '" <> feature <> "' is disabled; add '--extra-experimental-features " <> feature <> "' to enable it"

-- | The algorithm a name names, as upstream's @parseHashAlgoOpt@ reads it
-- (hash.cc at 2.24.9): one of the four it knows, or none.
knownHashAlgo :: BS.ByteString -> Maybe Text
knownHashAlgo bytes = either (const Nothing) (\algo -> algo <$ hashAlgoBytes algo) (TE.decodeUtf8' bytes)

-- | Upstream's @handleOutputs@ (primops.cc): the names in order, a repeated
-- name and the name drvPath refused, as the result set already carries a
-- drvPath, then an empty list.
checkedOutputs :: (MonadEval m) => [Text] -> m (NonEmpty Text)
checkedOutputs names = do
  foldM_ checkOutputName Set.empty names
  maybe (throwEvalError "derivation cannot have an empty set of outputs") pure (NE.nonEmpty names)
  where
    checkOutputName seen outName
      | Set.member outName seen = throwEvalError ("duplicate derivation output '" <> outName <> "'")
      | outName == "drvPath" = throwEvalError "invalid derivation output name 'drvPath'"
      | otherwise = pure (Set.insert outName seen)

-- | The output names an environment's @outputs@ entry holds: its bytes
-- split at the separators of upstream's @tokenizeString@ (util.hh at
-- 2.24.9, strings.hh at 2.36.0), empty pieces dropped.
outputTokens :: BS.ByteString -> [BS.ByteString]
outputTokens = filter (not . BS.null) . BS.splitWith (`BS.elem` tokenSeparators)
  where
    tokenSeparators = BC.pack " \t\n\r"

-- | How a derivation hands its attributes to the builder.
data AttrPassing
  = -- | One environment variable per attribute, each coerced to a string.
    PassAsEnvironment
  | -- | @__structuredAttrs = true@: one JSON object under @__json@, so an
    -- attribute may be a set or a list (upstream's @jsonObject@ branch of
    -- @derivationStrictInternal@, primops.cc at 2.24.9).
    PassAsStructuredAttrs

-- | The attribute choosing structured attributes.  It is not itself one of
-- them: upstream leaves it out of the JSON object.
structuredAttrsFlag :: Text
structuredAttrsFlag = "__structuredAttrs"

-- | Upstream reads @__structuredAttrs@ before any other attribute and as a
-- boolean, whatever @__ignoreNulls@ says, refusing anything else with
-- @forceBool@'s message.
attrPassingOf :: (MonadEval m) => AttrSet -> m AttrPassing
attrPassingOf attrs =
  attrSetLookup structuredAttrsFlag attrs >>= \case
    Nothing -> pure PassAsEnvironment
    Just thunk -> do
      structured <- force thunk >>= expectBool
      pure (if structured then PassAsStructuredAttrs else PassAsEnvironment)

-- | Whether a string read without coercion may carry context: one
-- upstream reads with @forceString@, such as a structured derivation's
-- builder, may; one it reads with @forceStringNoCtx@ (eval.cc), such as a
-- derivation's name or a regex, may not.
data StringContextRule = AllowContext | RefuseContext

-- | A string as upstream's @forceString@ reads it: a string, never a
-- value coerced to one.  Under 'RefuseContext' it is read as
-- @forceStringNoCtx@ reads it, and a string with context is refused in
-- that function's words, which name the first element of the context.
structuredStringValue :: (MonadEval m) => StringContextRule -> NixValue -> m BS.ByteString
structuredStringValue rule val = case (val, rule) of
  (VStr s ctx, RefuseContext)
    | Just shown <- firstContextEncoded ctx ->
        throwEvalError ("the string '" <> bytesToTextLossy s <> "' is not allowed to refer to a store path (such as '" <> shown <> "')")
  (VStr s _, _) -> pure s
  (other, _) ->
    expectedFailure "a string" other

-- | An attribute name a program computes, read as upstream reads every
-- one, with @forceStringNoCtx@ (eval.cc): a string, never a value coerced to
-- one, and one with no context.  The text names the reader in an invalid
-- UTF-8 refusal, which upstream, holding bytes, never makes.
attrNameOf :: (MonadEval m) => Text -> NixValue -> m Text
attrNameOf reader = structuredStringValue RefuseContext >=> decodedText reader

-- | An element of a structured derivation's @outputs@ list: a string
-- without context, where the environment would coerce it.
structuredOutputName :: (MonadEval m) => Text -> Thunk -> m Text
structuredOutputName fieldContext thunk =
  force thunk >>= structuredStringValue RefuseContext >>= decodedText fieldContext

-- | Reference and size checks that a structured derivation takes from
-- @outputChecks@ instead; upstream warns when one is set at the top level.
structuredDisabledChecks :: Set.Set Text
structuredDisabledChecks =
  Set.fromList
    ["allowedReferences", "allowedRequisites", "disallowedReferences", "disallowedRequisites", "maxSize", "maxClosureSize"]

-- | Upstream's warning for a 'structuredDisabledChecks' attribute.
disabledByStructuredAttrs :: Text -> Text -> Text
disabledByStructuredAttrs drvName key =
  "In a derivation named '"
    <> drvName
    <> "', 'structuredAttrs' disables the effect of the derivation attribute '"
    <> key
    <> "'; use 'outputChecks.<output>."
    <> key
    <> "' instead"

-- | Upstream's line of context for a failure processing one attribute of
-- a derivation's argument set (derivationStrictInternal, primops.cc at
-- 2.24.9).
derivationAttrTrace :: Text -> Text -> Text
derivationAttrTrace drvName key = "while evaluating attribute '" <> key <> "' of derivation '" <> drvName <> "'"

-- | Run the action with a line of context when the attribute is present,
-- and as it is when it is absent, which leaves a missing attribute's
-- refusal unadorned.
traceIfPresent :: (MonadEval m) => AttrSet -> Text -> Text -> m a -> m a
traceIfPresent attrs key line action = do
  present <- attrSetMember key attrs
  if present then addErrorTrace line action else action

-- | A Boolean as upstream's @forceBool@ reads it, refused with its message
-- (eval.cc): a derivation's switches and the
-- @recursive@ of @builtins.path@.
expectBool :: (MonadEval m) => NixValue -> m Bool
expectBool (VBool b) = pure b
expectBool other = expectedFailure "a Boolean" other

-- ---------------------------------------------------------------------------
-- Builtin implementations - hashFile, readFileType
-- ---------------------------------------------------------------------------

-- | @builtins.hashFile algo path@ - hash raw bytes of a file on disk.
-- Returns base-16 hex string, matching @builtins.hashString@ output format.
-- The algorithm is read as upstream's @forceStringNoCtx@ reads it and
-- refused before the path is realised or read (@prim_hashFile@ in
-- primops.cc).
builtinHashFile :: (MonadEval m) => NixValue -> BuiltinArg -> m NixValue
builtinHashFile algoVal pathArg = do
  algoName <- decodedText "builtins.hashFile" =<< structuredStringValue RefuseContext algoVal
  digest <- maybe (throwEvalError (unknownHashAlgo algoName)) pure (hexDigestOf algoName)
  bytes <- readFileBytes =<< realisePath "builtins.hashFile" =<< argValue pathArg
  pure (mkStr (digest bytes))

-- | The base-16 digest an algorithm's name selects, if it names one.
hexDigestOf :: Text -> Maybe (BS.ByteString -> Text)
hexDigestOf = \case
  "sha256" -> Just (\bytes -> digestToHex (CH.hash bytes :: CH.Digest CH.SHA256))
  "sha512" -> Just (\bytes -> digestToHex (CH.hash bytes :: CH.Digest CH.SHA512))
  "sha1" -> Just (\bytes -> digestToHex (CH.hash bytes :: CH.Digest CH.SHA1))
  "md5" -> Just (\bytes -> digestToHex (CH.hash bytes :: CH.Digest CH.MD5))
  _ -> Nothing

-- | @builtins.readFileType path@ - classify a filesystem entry.
-- Returns @"regular"@, @"directory"@, @"symlink"@, or @"unknown"@.
builtinReadFileType :: (MonadEval m) => NixValue -> m NixValue
builtinReadFileType val = mkStr <$> (getFileType =<< realisePath "builtins.readFileType" val)

-- ---------------------------------------------------------------------------
-- Builtin implementations - convertHash
-- ---------------------------------------------------------------------------

-- | @builtins.convertHash { hash, hashAlgo?, toHashFormat }@ - convert
-- between hash representations.  Supports base16, nix32, base64, and sri.
builtinConvertHash :: (MonadEval m) => NixValue -> m NixValue
builtinConvertHash (VAttrs attrs) = do
  hashVal <- requireStrAttr "convertHash" "hash" attrs
  -- hashAlgo, when given, is parsed before the hash and handed to
  -- Hash::parseAny (prim_convertHash in primops.cc), so an unknown one is
  -- refused, and a tag naming another algorithm too.
  hasAlgo <- attrSetMember "hashAlgo" attrs
  expected <-
    if hasAlgo
      then do
        algo <- requireStrAttr "convertHash" "hashAlgo" attrs
        Just algo <$ either throwEvalError pure (parseHashAlgo algo)
      else pure Nothing
  toFmt <- requireStrAttr "convertHash" "toHashFormat" attrs
  (algo, rawBytes) <- either throwEvalError pure (parseAnyHash expected hashVal)
  case toFmt of
    "base16" -> pure (mkStr (bytesToHexText rawBytes))
    "nix32" -> pure (mkStr (Nix32.encode rawBytes))
    "base32" -> pure (mkStr (Nix32.encode rawBytes)) -- deprecated alias
    "base64" -> pure (mkStr (bytesToBase64 rawBytes))
    "sri" -> pure (mkStr (algo <> "-" <> bytesToBase64 rawBytes))
    _ -> throwEvalError ("builtins.convertHash: unknown toHashFormat '" <> toFmt <> "'")
builtinConvertHash other =
  throwEvalError ("builtins.convertHash: expected a set, got " <> typeName other)

-- | Read a hash as upstream's @newHashAllowEmpty@ does (hash.cc): an empty
-- one is the digest of @expected@ whose every byte is zero, which upstream
-- warns of, naming it in SRI, and is refused with no @expected@; any other
-- is read as 'parseAnyHash' reads it.
newHashAllowEmpty :: (MonadEval m) => Maybe Text -> Text -> m (Text, BS.ByteString)
newHashAllowEmpty expected original
  | T.null original = case expected of
      Nothing -> throwEvalError "empty hash requires explicit hash algorithm"
      Just algo -> do
        zero <- either throwEvalError (\size -> pure (BS.replicate size 0)) (parseHashAlgo algo)
        warnMessage ("found empty hash, assuming '" <> algo <> "-" <> bytesToBase64 zero <> "'")
        pure (algo, zero)
  | otherwise = either throwEvalError pure (parseAnyHash expected original)

-- | Parse an SRI hash as upstream's @Hash::parseSRI@ does (hash.cc): the
-- algorithm before the first @-@ and a base64 digest after it.  A hash in
-- any other spelling is refused.
parseSriHash :: Text -> Either Text (Text, BS.ByteString)
parseSriHash original = case T.breakOn "-" original of
  (algo, rest) | Just digest <- T.stripPrefix "-" rest -> (algo,) <$> sriDigest algo digest
  _ -> Left ("hash '" <> original <> "' is not SRI")

-- | A declared @narHash@'s SHA-256 digest, read as upstream's
-- @Input::getNarHash@ reads it (fetchers.cc): an SRI hash, which must be
-- SHA-256, or the empty string, which is a digest whose every byte is zero.
parseNarHash :: Text -> Either Text BS.ByteString
parseNarHash declared
  | T.null declared = Right (BS.replicate sha256DigestSize 0)
  | otherwise =
      parseSriHash declared >>= \case
        ("sha256", digest) -> Right digest
        _ -> Left "narHash must use SHA-256"
  where
    sha256DigestSize = 32

-- | Require a string attribute from an attrset.
requireStrAttr :: (MonadEval m) => Text -> Text -> AttrSet -> m Text
requireStrAttr ctx key attrs =
  attrSetLookup key attrs >>= \case
    Just thunk -> do
      val <- force thunk
      case val of
        VStr s _ -> decodedText ("builtins." <> ctx) s
        _ -> throwEvalError ("builtins." <> ctx <> ": " <> key <> " must be a string")
    Nothing -> throwEvalError ("builtins." <> ctx <> ": missing required attribute '" <> key <> "'")

-- ---------------------------------------------------------------------------
-- Base64 encoding - delegates to nova-cache (base64-bytestring under the hood)
-- ---------------------------------------------------------------------------

-- | Encode bytes to base64.
bytesToBase64 :: BS.ByteString -> Text
bytesToBase64 = B64.encode

-- ---------------------------------------------------------------------------
-- Builtin implementations - fromTOML
-- ---------------------------------------------------------------------------

-- | @builtins.fromTOML str@ - parse a TOML document to a Nix value.
-- Hand-rolled parser covering the TOML v1.0 subset used by nixpkgs:
-- bare/quoted keys, dotted keys, basic/literal strings (multiline),
-- integers (dec/hex/oct/bin), floats (inc. inf/nan), booleans,
-- inline tables, arrays, array-of-tables, and standard tables.
-- A date or a time is refused once the whole document has parsed, as
-- upstream Nix refuses one unless its @parse-toml-timestamps@ experimental
-- feature, which nova-nix does not have, is on.
builtinFromTOML :: (MonadEval m) => NixValue -> m NixValue
builtinFromTOML (VStr s _) = do
  -- TOML is UTF-8 by definition; upstream's parser rejects invalid bytes.
  decoded <- decodedText "builtins.fromTOML" s
  case parseTOML decoded of
    Right toml -> tomlToNix toml
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
  | -- | A date, a time, or both, which no value holds.
    TOMLDatetime
  deriving (Show)

-- | Parse a TOML document: its top-level table.
parseTOML :: Text -> Either Text TOMLValue
parseTOML input = TOMLTable <$> parseTOMLDoc (T.lines input)

-- | Convert a TOMLValue to NixValue.
tomlToNix :: (MonadEval m) => TOMLValue -> m NixValue
tomlToNix val = case val of
  TOMLDatetime -> throwEvalError "while parsing TOML: Dates and times are not supported"
  TOMLStr s -> pure (mkStr s)
  TOMLInt n -> pure (VInt n)
  TOMLFloat d -> pure (VFloat d)
  TOMLBool b -> pure (VBool b)
  TOMLArray xs -> mapM tomlToNix xs >>= valuesList
  TOMLTable m -> traverse tomlToNix m >>= valuesSet SetFromTOML

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
-- string context is still known (@#@ inside any string form is content).
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
  -- Contains date or time separators, so a date or a time
  | T.any (== 'T') s && T.any (== '-') s = Right TOMLDatetime
  | T.count "-" s >= 2 && T.any isDigit s = Right TOMLDatetime
  | T.any (== ':') s && T.any isDigit s = Right TOMLDatetime
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

-- | @builtins.toXML val@ - the value as upstream's @prim_toXML@ writes it:
-- @printValueAsXML@ forcing every value and recording no positions
-- (value-to-xml.cc and xml-writer.cc at 2.24.9).  The result keeps the
-- contexts of the strings written into it, so a derivation built from the
-- document depends on the store paths it names, as upstream's does.
--
-- Built over BYTES: upstream's writer copies a string into an attribute
-- value with only its five escapes, so invalid UTF-8 passes through raw
-- rather than erroring (unlike toJSON, whose upstream serializer
-- validates).
builtinToXML :: (MonadEval m) => NixValue -> m NixValue
builtinToXML val = do
  (body, XmlWritten ctx _) <- valueToXML 1 (XmlWritten emptyContext Set.empty) val
  let document =
        "<?xml version='1.0' encoding='utf-8'?>\n"
          <> xmlOpen 0 "expr" []
          <> body
          <> xmlClose 0 "expr"
  pure (VStr (BL.toStrict (BB.toLazyByteString document)) ctx)

-- | What writing a document has gathered so far: the contexts of the
-- strings written, and the drvPaths of the derivations written in full,
-- each of which a later occurrence writes as @<repeated />@ (upstream's
-- @drvsSeen@).  That is also what ends the cycle every derivation's
-- @all@ and output attributes close.
data XmlWritten = XmlWritten !StringContext !(Set.Set BS.ByteString)

-- | One XML attribute: its name and its unescaped value.
type XmlAttr = (BS.ByteString, BS.ByteString)

-- | Write one value as the element upstream writes for it, at the given
-- depth (an element's depth is the number of elements open around it, and
-- each level indents two spaces).
valueToXML :: (MonadEval m) => Int -> XmlWritten -> NixValue -> m (BB.Builder, XmlWritten)
valueToXML depth written val = case val of
  VInt n -> leaf "int" [("value", BC.pack (show n))]
  VBool b -> leaf "bool" [("value", if b then "true" else "false")]
  VStr s ctx ->
    let XmlWritten seenCtx seenDrvs = written
     in pure (xmlEmpty depth "string" [("value", s)], XmlWritten (seenCtx <> ctx) seenDrvs)
  VPath p -> leaf "path" [("value", TE.encodeUtf8 p)]
  VNull -> leaf "null" []
  VFloat d -> leaf "float" [("value", TE.encodeUtf8 (formatXmlFloat d))]
  VList cl ->
    xmlElement depth "list" [] $
      listThunks cl >>= foldM (writeChild (\thunk inner -> force thunk >>= valueToXML (depth + 1) inner)) (mempty, written)
  VAttrs attrs -> do
    identity <- xmlDerivationIdentity attrs
    case identity of
      Nothing -> xmlElement depth "attrs" [] (attrsToXML (depth + 1) written attrs)
      Just (drvAttrs, drvPath) ->
        let XmlWritten seenCtx seenDrvs = written
         in xmlElement depth "derivation" drvAttrs $
              if BS.null drvPath || Set.member drvPath seenDrvs
                then pure (xmlEmpty (depth + 1) "repeated" [], written)
                else attrsToXML (depth + 1) (XmlWritten seenCtx (Set.insert drvPath seenDrvs)) attrs
  VLambda _ formals _ -> xmlElement depth "function" [] (pure (formalsToXML (depth + 1) formals, written))
  -- A primop, applied in part or not at all, is not a lambda, which
  -- upstream writes as unevaluated (its FIXME: primops are not serialized).
  VBuiltin _ _ -> leaf "unevaluated" []
  where
    leaf name attrs = pure (xmlEmpty depth name attrs, written)

-- | Each attribute of a set, in name order, as an @attr@ element holding
-- its value.
attrsToXML :: (MonadEval m) => Int -> XmlWritten -> AttrSet -> m (BB.Builder, XmlWritten)
attrsToXML depth written attrs =
  attrSetToAscList attrs >>= foldM (writeChild attrToXML) (mempty, written)
  where
    attrToXML (name, thunk) inner =
      xmlElement depth "attr" [("name", TE.encodeUtf8 name)] (force thunk >>= valueToXML (depth + 1) inner)

-- | Append one child's output to what its siblings wrote, threading what
-- the document has gathered from one to the next.
writeChild :: (Monad m) => (a -> XmlWritten -> m (BB.Builder, XmlWritten)) -> (BB.Builder, XmlWritten) -> a -> m (BB.Builder, XmlWritten)
writeChild write (before, written) child = do
  (out, next) <- write child written
  pure (before <> out, next)

-- | The XML attributes of a derivation's element and the drvPath that
-- identifies it, or 'Nothing' for a set that is not a derivation: upstream's
-- @isDerivation@, a @type@ attribute that is the string @"derivation"@.
-- @drvPath@ and @outPath@ are written only when they are strings, and their
-- contexts are not gathered here: a derivation written in full gathers them
-- as attributes, and a repeated one adds none.
xmlDerivationIdentity :: (MonadEval m) => AttrSet -> m (Maybe ([XmlAttr], BS.ByteString))
xmlDerivationIdentity attrs = do
  typeVal <- attrSetLookup "type" attrs >>= traverse force
  case typeVal of
    Just (VStr "derivation" _) -> do
      drvPath <- stringAttr "drvPath"
      outPath <- stringAttr "outPath"
      let present = [(key, s) | (key, Just s) <- [("drvPath", drvPath), ("outPath", outPath)]]
      pure (Just (present, fromMaybe "" drvPath))
    _ -> pure Nothing
  where
    stringAttr key = do
      found <- attrSetLookup key attrs >>= traverse force
      pure $ case found of
        Just (VStr s _) -> Just s
        _ -> Nothing

-- | A lambda's argument pattern: @varpat@ for a plain argument, otherwise
-- @attrspat@ naming the formals in name order, with the @\@@ binding and
-- the ellipsis as its attributes.
formalsToXML :: Int -> EvalFormals -> BB.Builder
formalsToXML depth formals = case formals of
  EFName name -> xmlEmpty depth "varpat" [("name", symbolBytes name)]
  EFSet _ entries ellipsis -> attrsPattern (ellipsisAttr ellipsis) entries
  EFNamedSet _ name entries ellipsis -> attrsPattern (ellipsisAttr ellipsis <> [("name", symbolBytes name)]) entries
  where
    symbolBytes = TE.encodeUtf8 . symbolText
    ellipsisAttr ellipsis = [("ellipsis", "1") | ellipsis]
    attrsPattern attrs entries =
      xmlOpen depth "attrspat" attrs
        <> foldMap (\name -> xmlEmpty (depth + 1) "attr" [("name", name)]) (sort (map (symbolBytes . efName) entries))
        <> xmlClose depth "attrspat"

-- | An element with children: the opening tag, what the children write one
-- level deeper, and the closing tag.
xmlElement :: (Monad m) => Int -> BS.ByteString -> [XmlAttr] -> m (BB.Builder, XmlWritten) -> m (BB.Builder, XmlWritten)
xmlElement depth name attrs children = do
  (inner, written) <- children
  pure (xmlOpen depth name attrs <> inner <> xmlClose depth name, written)

-- | Upstream's @XMLWriter@ lines.  The attributes are written in the order
-- given, which callers keep sorted by name as upstream's @std::map@ is.
xmlOpen, xmlEmpty :: Int -> BS.ByteString -> [XmlAttr] -> BB.Builder
xmlOpen depth name attrs = xmlIndent depth <> "<" <> BB.byteString name <> foldMap xmlAttr attrs <> ">\n"
xmlEmpty depth name attrs = xmlIndent depth <> "<" <> BB.byteString name <> foldMap xmlAttr attrs <> " />\n"

xmlClose :: Int -> BS.ByteString -> BB.Builder
xmlClose depth name = xmlIndent depth <> "</" <> BB.byteString name <> ">\n"

xmlIndent :: Int -> BB.Builder
xmlIndent depth = BB.byteString (BC.replicate (2 * depth) ' ')

-- | One attribute with upstream's escapes.  A newline is written as a
-- character reference because an XML parser normalizes a literal one in an
-- attribute value to a space (XML 1.0, section 3.3.3).
xmlAttr :: XmlAttr -> BB.Builder
xmlAttr (name, value) = " " <> BB.byteString name <> "=\"" <> BC.foldr (\c rest -> escapeChar c <> rest) mempty value <> "\""
  where
    escapeChar '"' = "&quot;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '\n' = "&#xA;"
    escapeChar c = BB.char8 c

-- ---------------------------------------------------------------------------
-- Builtin implementations - builtins.path
-- ---------------------------------------------------------------------------

-- | @builtins.path { path; name?; filter?; sha256?; recursive?; }@
--
-- Copy a path to the store and return the store path as a string with
-- context.  @name@ defaults to the basename of @path@.  @recursive@,
-- true unless given, copies the tree at the path as a NAR, and false the
-- one file at it by its bytes ('flatFileCopy').  @filter@, when given,
-- decides which entries of a tree are copied ('filteredSourceNar').
builtinPath :: (MonadEval m) => NixValue -> m NixValue
builtinPath (VAttrs attrs) = do
  -- prim_path reads the arguments one at a time in the set's own order,
  -- refusing one it does not take at its turn, and checks for the path
  -- once that loop is done (primops.cc).  The order is by interned symbol,
  -- as upstream's attribute sets are sorted.  The path's context is
  -- realised only after that, in addPath.
  args <- attrSetFoldM readPathArgument noPathArguments attrs
  (pathStr, pathContext) <- maybe (throwEvalError "missing required 'path' attribute in the first argument to builtins.path") pure (paPath args)
  -- An empty name is the path's base name, as prim_path's is.
  let name = fromMaybe (canonBaseName pathStr) (mfilter (not . T.null) (paName args))
      expectedDigest = paPin args
      -- Upstream compares the copy's store path with the one the pin
      -- gives (addPath in primops.cc); under one name and one method the two
      -- differ exactly when the digests do.
      mismatch = "store path mismatch in (possibly filtered) path added from '" <> pathStr <> "'"
  addingPath pathStr pathContext $ do
    -- A pinned copy's path follows from the pin.  Upstream's addPath
    -- builds it once the context is realised and before it reads the tree
    -- (primops.cc), so a name it refuses is refused under the pinned hash,
    -- before the filter is applied.
    forM_ expectedDigest $ \expected ->
      storePathOrThrow (makeFixedOutputPath name "sha256" (paMethod args) expected)
    storePathText <- case (paMethod args, paFilter args) of
      (FlatIngestion, _) -> flatFileCopy pathStr name expectedDigest mismatch
      (NarIngestion, Nothing) -> copyPathToStore Computed pathStr name (fmap (,const mismatch) expectedDigest)
      (NarIngestion, Just keep) -> do
        narBytes <- filteredSourceNar keep pathStr
        -- The sha256 pin applies to the FILTERED tree, as upstream.
        case expectedDigest of
          Just expected | expected /= sha256Digest narBytes -> throwEvalError mismatch
          _ -> pure ()
        addSourceNar name narBytes
    pure (storePathString storePathText)
builtinPath other =
  throwEvalError ("builtins.path: expected an attribute set, got " <> typeName other)

-- | What @builtins.path@ has read of its arguments, as @prim_path@'s
-- locals hold them (primops.cc): a NAR copy until
-- @recursive@ says otherwise.
data PathArguments = PathArguments
  { paPath :: !(Maybe (Text, StringContext)),
    paName :: !(Maybe Text),
    paFilter :: !(Maybe NixValue),
    paMethod :: !FileIngestionMethod,
    paPin :: !(Maybe BS.ByteString)
  }

-- | The arguments before any is read.
noPathArguments :: PathArguments
noPathArguments = PathArguments Nothing Nothing Nothing NarIngestion Nothing

-- | One argument of @builtins.path@, read as @prim_path@ reads the one at
-- its key (primops.cc): @name@ as a string with no context, @filter@ as a
-- function, each refused otherwise under upstream's line of context for it,
-- @recursive@ as a Boolean choosing a NAR or a flat copy, and any key it does
-- not take refused with its message.
readPathArgument :: (MonadEval m) => PathArguments -> Symbol -> AttrEntry -> m PathArguments
readPathArgument args name entry = case key of
  "path" -> (\path -> args {paPath = Just path}) <$> (coerceToPathWithContext "builtins.path" =<< force thunk)
  "name" -> (\named -> args {paName = Just named}) <$> (checked "`name` attribute" nameOf =<< force thunk)
  "filter" -> (\keep -> args {paFilter = Just keep}) <$> (checked "`filter` parameter" expectFunction =<< force thunk)
  "recursive" -> (\recursive -> args {paMethod = if recursive then NarIngestion else FlatIngestion}) <$> (expectBool =<< force thunk)
  "sha256" -> (\pin -> args {paPin = Just pin}) <$> (pathPin =<< force thunk)
  _ -> throwEvalError ("unsupported argument '" <> key <> "' to 'addPath'")
  where
    key = symbolText name
    thunk = entryThunk entry
    checked what check value = addErrorTrace ("while evaluating the " <> what <> " passed to builtins.path") (check value)
    nameOf value = decodedText "builtins.path" =<< structuredStringValue RefuseContext value
    pathPin pinVal = case pinVal of
      VStr pin _ -> decodeSha256Pin =<< decodedText "builtins.path" pin
      other -> throwEvalError ("builtins.path: 'sha256' must be a string, got " <> typeName other)

-- | The copy @recursive = false@ makes: the bytes of the file at the
-- path, its symlinks resolved, at their flat fixed-output path.
-- Upstream's addPath hands fetchToStore the resolved path
-- (primops.cc), and a flat dump reads the file and calls no filter
-- (@dumpPath@ in file-content-address.cc).  That read fails on a directory
-- with EISDIR (@PosixSourceAccessor::readFile@ in posix-source-accessor.cc
-- at 2.24.9), and this copy refuses one with the message that read gives.
flatFileCopy :: (MonadEval m) => Text -> Text -> Maybe BS.ByteString -> Text -> m Text
flatFileCopy rawPath name pin mismatch = do
  resolved <- resolveSymlinks rawPath
  fileType <- getFileType resolved
  when (fileType == "directory") $
    throwEvalError ("reading from file '" <> resolved <> "': Is a directory")
  bytes <- readFileBytes resolved
  case pin of
    Just expected | expected /= sha256Digest bytes -> throwEvalError mismatch
    _ -> pure ()
  addFixedOutputFile Computed name bytes

-- | A store path as upstream's @mkStorePathString@ gives it (eval.cc): its
-- text, carrying itself as context.  What the source importers and the
-- fetchers return, so a coercion names the store path rather than copying
-- it again as it would a path value.
storePathString :: Text -> NixValue
storePathString storePathText =
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
filteredSourceNar filterFn rawRoot = do
  -- The root is resolved through its symlinks before the walk, as
  -- upstream's addPath and filterSource resolve theirs: a root that is a
  -- symlink stores what it points at, and no entry below has a symlink
  -- above it for the lstats to refuse.
  rootPath <- resolveSymlinks rawRoot
  rootType <- getFileType rootPath
  entry <- buildEntry rootPath rootType
  pure (NAR.serialise entry)
  where
    buildEntry path fileType = case fileType of
      "regular" -> do
        executable <- isExecutableFile path
        bytes <- readFileBytes path
        pure (NAR.NarRegular executable bytes)
      "symlink" -> NAR.NarSymlink . TE.encodeUtf8 <$> readSymlinkTarget path
      "directory" -> do
        children <- listDirectory path
        kept <- mapM (keepChild path) children
        pure (NAR.NarDirectory (catMaybes kept))
      other -> throwEvalError ("builtins.path: unsupported file type '" <> other <> "' at " <> path)
    keepChild parent (name, listedType) = do
      let childPath = parent <> "/" <> name
      childType <- entryFileType childPath listedType
      keep <- filterAccepts childPath childType
      if keep
        then Just . (,) (TE.encodeUtf8 name) <$> buildEntry childPath childType
        else pure Nothing
    filterAccepts path fileType = do
      partial <- applyValue filterFn (mkStr path)
      result <- applyValue partial (mkStr fileType)
      case result of
        VBool b -> pure b
        other -> throwEvalError ("builtins.path: the filter function must return a Boolean, got " <> typeName other)

-- ---------------------------------------------------------------------------
-- Builtin implementations - filterSource
-- ---------------------------------------------------------------------------

-- | @builtins.filterSource filter path@ - copy a path to the store,
-- filtering entries via a predicate.  The filter function receives
-- @(path, type)@ where type is @"regular"@, @"directory"@, @"symlink"@,
-- or @"unknown"@.  Equivalent to @builtins.path@ with a filter and the
-- source's basename as the store name.  @prim_filterSource@ coerces the
-- path before it forces the filter, and realises the path's context only
-- after both, in addPath (primops.cc), so the filter is taken unforced.
builtinFilterSource :: (MonadEval m) => BuiltinArg -> NixValue -> m NixValue
builtinFilterSource filterArg pathVal = do
  (path, ctx) <- coerceToPathWithContext "builtins.filterSource" pathVal
  filterFn <- forceFunctionArg filterArg
  addingPath path ctx (filterSourceInto filterFn path)

filterSourceInto :: (MonadEval m) => NixValue -> Text -> m NixValue
filterSourceInto filterFn path = do
  narBytes <- filteredSourceNar filterFn path
  storePathText <- addSourceNar (canonBaseName path) narBytes
  pure (storePathString storePathText)
