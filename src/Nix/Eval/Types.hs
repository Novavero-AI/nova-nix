{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Shared types for the Nix evaluator.
--
-- Extracted into its own module so that "Nix.Eval.Operator",
-- "Nix.Eval.StringInterp", and "Nix.Builtins" can all reference
-- 'NixValue', t'Thunk', and t'Env' without creating import cycles.
module Nix.Eval.Types
  ( -- * Values
    NixValue (.., VStr, VPath),
    HeldBytes (..),
    BuiltinArg (..),
    Thunk (..),
    readThunkValue,
    readComputed,
    thunkUnderEvaluation,

    -- * Attribute sets (C-backed sorted arrays)
    AttrSet (..),
    CAttrSet,
    AttrEntry (..),
    attrSetLookup,
    attrSetLookupEntry,
    attrSetKeys,
    firstInSymbolOrder,
    attrSetAt,
    attrSetToMap,
    emptyAttrSet,
    attrSetFromMap,
    attrSetFromEntries,
    attrSetFromSymbols,
    attrSetFromSymbolEntries,
    attrSetFromEntriesSizedFor,
    attrSetMember,
    attrSetNull,
    attrSetSize,
    attrSetElems,
    attrSetToAscList,
    attrSetMapWithKey,
    attrSetRemoveKeys,
    buildCAttrSetKeys,
    fillCAttrSetValues,

    -- * Lists (C-backed)
    CList (..),
    emptyCList,
    clistFromThunks,
    clistThunks,
    clistLen,

    -- * String context
    StringContextElement (..),
    StringContext (..),
    emptyContext,
    mkStr,
    mkStrBytes,
    bytesToTextLossy,
    marshalStringContext,
    unmarshalStringContext,

    -- * Environment
    Env (..),
    NnEnv,
    emptyEnv,
    envLookup,
    envLookupResolved,
    newEnv,
    newScopeEnv,
    pushLazyWithScope,
    buildCaptureEnv,
    withScopeAt,
    resolveWithScopeAt,
    withScopesForCapture,
    envWithScopesRaw,

    -- * Eval-time formals (re-exported from EvalFormals)
    EvalFormals (..),
    EvalFormal (..),

    -- * Thunk operations
    compile,
    compileSource,
    mkThunk,
    cheapThunk,
    cheapThunkBc,
    mkThunkBc,
    pendingCode,
    pendingThunkEnv,
    evaluated,
    thunkSameRef,
    thunkToCPtr,
    tieFrame,

    -- * Lambda marshalling
    marshalLambda,
    unmarshalLambdaValue,

    -- * Display
    typeName,
    typeOfValue,

    -- * C thunk-state and value tags (mirror cbits/nn_thunk.h)
    pattern ThunkPending,
    pattern ThunkComputed,
    pattern ThunkBlackhole,
    pattern ValueInt,
    pattern ValueFloat,
    pattern ValueBool,
    pattern ValueNull,
    pattern ValueStr,
    pattern ValuePath,
    pattern ValueList,
    pattern ValueAttrs,
    pattern ValueCtxStr,
    pattern ValueLambda,

    -- * Cells holding strings
    heldString,
    StringCell (..),
    stringCell,
    pathCell,
    PrimopCell (..),
    primopCell,

    -- * Evaluation monad
    MonadEval (..),
    PathExistence (..),
    EvalPolicy (..),
    unrestrictedPolicy,
    PureEval,
    runPureEval,
    storePathOrThrow,
  )
where

import Control.Monad (forM_, zipWithM_)
import Data.Bits (bit, complement, shiftL, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, WordPtr, castPtr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Float (castWord64ToDouble)
import Nix.Derivation (Derivation)
import Nix.Eval.AllocSite (EnvSite (..), SetSite (..))
import Nix.Eval.Builtin (Builtin, builtinAtIndex, builtinIndex, builtinName)
import Nix.Eval.CAttrSet (CAttrSet, cattrsetEmpty, cattrsetFreeze, cattrsetGetKey, cattrsetGetPos, cattrsetGetValue, cattrsetIndex, cattrsetInsert, cattrsetKeys, cattrsetLookup, cattrsetNew, cattrsetRemoveKeys, cattrsetSetValue, cattrsetSize)
import Nix.Eval.CBytecode (cbcArg1, cbcArg2, cbcCountedPayload, cbcData, cbcOpcode, cbcShortArg, cbcString, cbcStringBytes, pattern OpList, pattern OpLitBool, pattern OpLitFloat, pattern OpLitInt, pattern OpLitNull, pattern OpLitPath, pattern OpLitUri, pattern OpResolvedVar, pattern OpStr)
import Nix.Eval.CCtxStr (CCtxStrPtr, cctxstrCtxCount, cctxstrElemHash, cctxstrElemName, cctxstrElemOutput, cctxstrElemTag, cctxstrNew, cctxstrSetAllOutputs, cctxstrSetDrvOutput, cctxstrSetPlain, cctxstrText)
import Nix.Eval.CData (CData (..), MonadCData (..), cdataIO, inspectForDisplay, runCDataPure)
import Nix.Eval.CEnv (NnEnv, cenvAllocWithScopes, cenvEmpty, cenvLazyScope, cenvLookupResolved, cenvNew, cenvNewScoped, cenvParent, cenvPushWith, cenvRootScope, cenvSlotCount, cenvSlots, cenvWithCount, cenvWithOwner, cenvWithScopes)
import Nix.Eval.CLambda (clambdaAllowExtra, clambdaBody, clambdaEntryDefault, clambdaEntryHasDefault, clambdaEntryName, clambdaEnv, clambdaFormalCount, clambdaFormalsType, clambdaLambdaNameSym, clambdaNameSym, clambdaNew, clambdaSetEntry)
import Nix.Eval.CList (CList (..), clistFromThunks, clistLen, clistThunks, emptyCList)
import Nix.Eval.CPrimop (CPrimopAppPtr, cprimopAppArg, cprimopAppForced, cprimopAppNew, cprimopAppPrimop, cprimopAppTaken)
import Nix.Eval.CStatus (cInvariantViolated, checkedCPtr)
import Nix.Eval.CStr (CStrPtr, cstrBytes, cstrFromText, cstrNew, cstrText)
import Nix.Eval.CThunk (CThunkPtr, cthunkBool, cthunkEmptyList, cthunkGetAttrs, cthunkGetBcIdx, cthunkGetBool, cthunkGetCtxStr, cthunkGetFloat, cthunkGetInt, cthunkGetLambda, cthunkGetList, cthunkGetPath, cthunkGetPrimop, cthunkGetPrimopApp, cthunkGetStr, cthunkNewBc, cthunkNewComputedAttrs, cthunkNewComputedCtxStr, cthunkNewComputedFloat, cthunkNewComputedInt, cthunkNewComputedLambda, cthunkNewComputedList, cthunkNewComputedPath, cthunkNewComputedPrimop, cthunkNewComputedPrimopApp, cthunkNewComputedStr, cthunkNull, cthunkPayload, cthunkState, cthunkValueTag)
import Nix.Eval.CallDepth (CallDepth, defaultMaxCallDepth, enterCallFrame, topLevelCallDepth)
import Nix.Eval.CanonPath (canonPathValue)
import Nix.Eval.Compile (compileExpr, compileFormalsToEval, compileParsed)
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..), lambdaNameFromSymbol, lambdaNameSymbol)
import Nix.Eval.Policy (EvalPolicy (..), unrestrictedPolicy)
import Nix.Eval.SourcePos (PosIdx, noPos)
import Nix.Eval.Symbol (Symbol (..), invalidSymbolId, symbolIntern, symbolText)
import Nix.Expr.Types (CaptureInfo (..), Expr (..), NixAtom (..))
import Nix.Parser (ParsedSource)
import Nix.Store.Path (BadStorePath, StorePath, badStorePathText)
import Nix.Store.Path.Internal (StorePath (StorePath))

-- ---------------------------------------------------------------------------
-- String context
-- ---------------------------------------------------------------------------

-- | A single element of string context, tracking where a string
-- references store paths.
data StringContextElement
  = -- | Plain store path reference (inputSrcs).
    SCPlain !StorePath
  | -- | Derivation output reference (inputDrvs): .drv path + output name.
    SCDrvOutput !StorePath !Text
  | -- | All outputs of a derivation (for drvPath itself).
    SCAllOutputs !StorePath
  deriving (Eq, Ord, Show)

-- | Context carried by Nix strings, tracking store path dependencies.
newtype StringContext = StringContext {unStringContext :: Set StringContextElement}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Empty string context (alias for 'mempty').
emptyContext :: StringContext
emptyContext = mempty

-- | Smart constructor for context-free strings from 'Text'.
-- Encodes to the UTF-8 bytes that ARE the string's value: a Nix string
-- is a byte string, and every Text-producing site goes through here.
mkStr :: Text -> NixValue
mkStr t = VStr (TE.encodeUtf8 t) emptyContext

-- | Smart constructor for context-free strings from raw bytes
-- (@builtins.readFile@, substring slices - anything already byte-shaped).
mkStrBytes :: ByteString -> NixValue
mkStrBytes b = VStr b emptyContext

-- | Decode string-value bytes for DISPLAY ONLY (trace output, error
-- messages, value pretty-printing): invalid UTF-8 becomes U+FFFD.
-- Never feed the result back into a value, a hash, or an attr name -
-- identity-bearing boundaries decode strictly and error instead.
bytesToTextLossy :: ByteString -> Text
bytesToTextLossy = TE.decodeUtf8With lenientDecode

-- ---------------------------------------------------------------------------
-- Thunks
-- ---------------------------------------------------------------------------

-- | A thunk: a C arena-allocated memoization cell.
--
-- Each thunk points to an @nn_thunk_t@ in the C arena.
-- On first force, the cell transitions PENDING to BLACKHOLE to COMPUTED,
-- which detects infinite recursion (BLACKHOLE) and overwrites the
-- pending code and env with the value, as upstream mutates a thunk in
-- place.
--
-- A pending cell holds its env as the C pointer, so the env exists before
-- the cell does: 'mkThunkBc' takes a built t'Env', and 'tieFrame' builds a
-- frame whose bindings point back at it before filling its slots.  Arena
-- pointers remain valid until 'Nix.Eval.CThunk.cthunkDestroy'.
newtype Thunk = Thunk {unThunk :: CThunkPtr}

-- | The cell's state and, once computed, its value, as they stand when
-- the string is forced ('inspectForDisplay').
instance Show Thunk where
  show (Thunk ptr) =
    case inspectForDisplay (cthunkState ptr) of
      ThunkComputed -> "Thunk (" ++ show (inspectForDisplay (readComputed ptr)) ++ ")"
      ThunkPending -> "Thunk <pending>"
      ThunkBlackhole -> "Thunk <blackhole>"
      _ -> "Thunk <unknown>"

-- | Equality: pointer identity fast path, then value comparison for
-- COMPUTED thunks, as they stand when compared ('inspectForDisplay').
-- Only used in tests on non-recursive structures.
instance Eq Thunk where
  (Thunk p1) == (Thunk p2)
    | p1 == p2 = True
    | otherwise = case inspectForDisplay ((,) <$> readThunkValue (Thunk p1) <*> readThunkValue (Thunk p2)) of
        (Just v1, Just v2) -> v1 == v2
        _ -> False

-- | Named constants for the C thunk-state byte and value-tag byte, mirroring
-- the @NN_THUNK_*@ / @NN_VALUE_*@ macros in @cbits\/nn_thunk.h@.  Bidirectional
-- pattern synonyms over literals, so a @case@ on a tag keeps the jump-table
-- compilation a bare-literal @case@ gets, while the state dispatches (the
-- 'forceThunk' of each evaluator) and the value dispatch ('readComputed')
-- share one source of truth instead of repeating literals.  These MUST stay
-- in lockstep with @cbits\/nn_thunk.h@.
pattern ThunkPending, ThunkComputed, ThunkBlackhole :: Word8
pattern ThunkPending = 0
pattern ThunkComputed = 1
pattern ThunkBlackhole = 2

-- | Value-kind tags for the C value layout, mirroring @cbits\/nn_thunk.h@.
-- The tag selects which payload a computed (VALUE) thunk carries.
pattern ValueInt, ValueFloat, ValueBool, ValueNull, ValueStr, ValuePath, ValueList, ValueAttrs, ValueCtxStr, ValueLambda, ValuePrimop, ValuePrimopApp :: Word8
pattern ValueInt = 0
pattern ValueFloat = 1
pattern ValueBool = 2
pattern ValueNull = 3
pattern ValueStr = 4
pattern ValuePath = 5
pattern ValueList = 6
pattern ValueAttrs = 7
pattern ValueCtxStr = 8
pattern ValueLambda = 9
pattern ValuePrimop = 10
pattern ValuePrimopApp = 11

-- | Whether a thunk is being forced right now, so it holds no value yet:
-- the cell's BLACKHOLE state.  An answer about this moment: the state
-- changes when the force finishes or is unwound.
thunkUnderEvaluation :: (MonadCData m) => Thunk -> m Bool
thunkUnderEvaluation (Thunk ptr) = (== ThunkBlackhole) <$> cthunkState ptr

-- | Read a COMPUTED thunk's value without forcing.
-- Returns 'Nothing' for PENDING or BLACKHOLE thunks.
readThunkValue :: (MonadCData m) => Thunk -> m (Maybe NixValue)
readThunkValue (Thunk ptr) = do
  state <- cthunkState ptr
  if state == ThunkComputed
    then Just <$> readComputed ptr
    else pure Nothing

-- | The value a COMPUTED cell holds, by its value tag: a scalar or a
-- builtin's index read in place, any other value from the C object the
-- cell points to.  Every value lives in C, so a tag outside the
-- @NN_VALUE_*@ constants is corrupt memory.
readComputed :: (MonadCData m) => CThunkPtr -> m NixValue
readComputed ptr = do
  tag <- cthunkValueTag ptr
  case tag of
    ValueInt -> VInt <$> cthunkGetInt ptr
    ValueFloat -> VFloat <$> cthunkGetFloat ptr
    ValueBool -> (\b -> VBool (b /= 0)) <$> cthunkGetBool ptr
    ValueNull -> pure VNull
    ValueStr -> cthunkGetStr ptr >>= heldString
    ValuePath -> do
      str <- cthunkGetPath ptr
      (`VPathHeld` HeldStr str) <$> cstrText str
    ValueList -> VList . CList . castPtr <$> cthunkGetList ptr
    ValueAttrs -> VAttrs . AttrSet . castPtr <$> cthunkGetAttrs ptr
    ValueCtxStr -> do
      csptr <- castPtr <$> cthunkGetCtxStr ptr
      (\(bytes, ctx) -> VStrHeld bytes ctx (HeldCtxStr csptr)) <$> unmarshalStringContext csptr
    ValueLambda -> cthunkGetLambda ptr >>= unmarshalLambdaValue
    ValuePrimop -> (`VBuiltin` []) <$> (cthunkGetPrimop ptr >>= knownBuiltin)
    ValuePrimopApp -> cthunkGetPrimopApp ptr >>= readPrimopApp . castPtr
    other -> cdataIO (cInvariantViolated "nn_thunk_value_tag" ("no value has tag " ++ show other))

-- | The builtin at an index a cell holds.
knownBuiltin :: (MonadCData m) => Word32 -> m Builtin
knownBuiltin index = maybe unknown pure (builtinAtIndex index)
  where
    unknown = cdataIO (cInvariantViolated "nn_thunk_get_primop" ("no builtin has index " ++ show index))

-- | A builtin's partial application, read from its C object: an argument
-- taken forced as the value its cell holds, any other as the cell.
readPrimopApp :: (MonadCData m) => CPrimopAppPtr -> m NixValue
readPrimopApp app = do
  builtin <- cprimopAppPrimop app >>= knownBuiltin
  taken <- cprimopAppTaken app
  forced <- cprimopAppForced app
  VBuiltin builtin <$> traverse (argumentAt forced) (take (fromIntegral taken) [0 ..])
  where
    argumentAt forced i = do
      cell <- cprimopAppArg app i
      if testBit forced (fromIntegral i)
        then ArgForced <$> (readThunkValue (Thunk cell) >>= maybe notComputed pure)
        else pure (ArgDeferred (Thunk cell))
    notComputed = cdataIO (cInvariantViolated "nn_primop_app_arg" "an argument taken forced is not COMPUTED")

-- | A Nix value - the result of evaluating an expression.
data NixValue
  = -- | 64-bit signed integer (matching Nix semantics).
    VInt !Int64
  | -- | Floating-point.
    VFloat !Double
  | -- | Boolean.
    VBool !Bool
  | -- | The null value.
    VNull
  | -- | String with dependency context, and where its bytes already are
    -- in C ('VStr' builds and matches one without).  The payload is a
    -- BYTE string, as upstream: @stringLength@/@substring@ index bytes,
    -- regexes match bytes, and @readFile@ carries raw file bytes (so
    -- invalid UTF-8 is representable).  Text enters via 'mkStr' (UTF-8
    -- encode) and leaves through explicit decode at boundaries (attr
    -- names strictly, display lossily via 'bytesToTextLossy').
    VStrHeld !ByteString !StringContext !HeldBytes
  | -- | Path, and where its bytes already are in C ('VPath' builds and
    -- matches one without).
    VPathHeld !Text !HeldBytes
  | -- | List of thunks (lazy elements), backed by C array.
    VList !CList
  | -- | Attribute set: unified lazy/eager representation.
    VAttrs !AttrSet
  | -- | Lambda closure: captures environment, formals, body bytecode index.
    VLambda !Env !EvalFormals !Word32
  | -- | Built-in function, with the arguments a partial application has
    -- taken so far, first to last.
    VBuiltin !Builtin ![BuiltinArg]
  deriving (Eq, Show)

-- | A string with dependency context: its bytes and their context.  Built
-- with it, a value's bytes are held in Haskell only, and a cell that
-- stores it takes a copy.
pattern VStr :: ByteString -> StringContext -> NixValue
pattern VStr bytes ctx <- VStrHeld bytes ctx _
  where
    VStr bytes ctx = VStrHeld bytes ctx NotHeld

-- | A path.  Built with it, a path's bytes are held in Haskell only, and
-- a cell that stores it takes a copy.
pattern VPath :: Text -> NixValue
pattern VPath path <- VPathHeld path _
  where
    VPath path = VPathHeld path NotHeld

{-# COMPLETE VInt, VFloat, VBool, VNull, VStr, VPath, VList, VAttrs, VLambda, VBuiltin #-}

-- | Where a string or path value's bytes already are in the C data
-- layer.  A value read from a cell, and a literal's value, carry the C
-- object their bytes are in, and a cell that stores the value points at
-- that object rather than at a copy, as upstream copies a @Value@ with
-- the pointer to its bytes (value.hh at 2.24.9): a string passed from
-- cell to cell takes its bytes once.  A value is built held only where
-- its bytes are read from the object it holds, and the objects never
-- change once made, so a value's bytes stay its object's.
data HeldBytes
  = -- | In Haskell only.
    NotHeld
  | -- | A context-free string's or a path's bytes.
    HeldStr !CStrPtr
  | -- | A string with context, whole.
    HeldCtxStr !CCtxStrPtr

-- | Every two are equal.  The held object is where a value's bytes live,
-- not part of the value, so two values with equal bytes and context are
-- equal wherever their bytes are.
instance Eq HeldBytes where
  _ == _ = True

instance Show HeldBytes where
  showsPrec _ held = showString $ case held of
    NotHeld -> "NotHeld"
    HeldStr _ -> "HeldStr"
    HeldCtxStr _ -> "HeldCtxStr"

-- | The value of a context-free string whose bytes are a C string: a
-- cell's, or a literal's in the code.
heldString :: (MonadCData m) => CStrPtr -> m NixValue
heldString str = (\bytes -> VStrHeld bytes emptyContext (HeldStr str)) <$> cstrBytes str

-- | An argument a builtin has taken.  A primop that forces an argument
-- before anything it does can be observed has it forced as it is
-- applied.  One that forces it later, after another argument or only on
-- some paths, has it held as the thunk it arrived as, and forces it where
-- upstream does.  An argument that arrives already forced is held forced
-- either way.
data BuiltinArg
  = ArgForced !NixValue
  | ArgDeferred !Thunk
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Attribute sets (C-backed sorted arrays)
-- ---------------------------------------------------------------------------

-- | Attribute set backed by a C-allocated sorted key-value array.
--
-- All keys are interned symbols; values are CThunkPtrs in a parallel
-- array.  O(log n) binary search on contiguous memory for lookup.
-- Replaces the former EagerAttrs\/LazyAttrs\/MappedAttrs\/CAttrs ADT
-- with a single C-backed representation - all attr set data lives off
-- the GHC heap, dramatically reducing GC pressure for large evaluations.
newtype AttrSet = AttrSet {unAttrSet :: CAttrSet}

-- | The same names bound to equal thunks, as the sets stand when compared
-- ('inspectForDisplay').
instance Eq AttrSet where
  a == b = inspectForDisplay ((==) <$> attrSetToMap a <*> attrSetToMap b)

instance Show AttrSet where
  show (AttrSet cset) =
    let n = inspectForDisplay (cattrsetSize cset)
     in "<attrset " ++ show n ++ " entries>"

-- | An attribute's value and the position it was defined at, as a set
-- holds them.
data AttrEntry = AttrEntry
  { entryPos :: {-# UNPACK #-} !PosIdx,
    entryThunk :: {-# UNPACK #-} !Thunk
  }

-- | Look up a single key via symbol interning + C binary search.
attrSetLookup :: (MonadCData m) => Text -> AttrSet -> m (Maybe Thunk)
attrSetLookup key (AttrSet cset) = do
  sym <- symbolIntern key
  fmap Thunk <$> cattrsetLookup cset sym

-- | 'attrSetLookup' with the attribute's position.
attrSetLookupEntry :: (MonadCData m) => Text -> AttrSet -> m (Maybe AttrEntry)
attrSetLookupEntry key (AttrSet cset) = do
  sym <- symbolIntern key
  midx <- cattrsetIndex cset sym
  traverse (\idx -> AttrEntry <$> cattrsetGetPos cset idx <*> (Thunk <$> cattrsetGetValue cset idx)) midx

-- | All keys, as symbol text from C, in the C array's order: by interned
-- symbol, not by name.
attrSetKeys :: (MonadCData m) => AttrSet -> m [Text]
attrSetKeys (AttrSet cset) = map symbolText <$> cattrsetKeys cset

-- | The name with the least symbol, the one a set holding them all would
-- hold first ('attrSetKeys').  A name not yet interned is interned here,
-- as 'attrSetLookup' interns a key it looks up.
firstInSymbolOrder :: (MonadCData m) => NonEmpty Text -> m Text
firstInSymbolOrder names = do
  symbols <- traverse symbolIntern names
  pure (snd (NE.head (NE.sortWith fst (NE.zip symbols names))))

-- | The name and value at an index of the C array, which holds a set's
-- attributes in interned-symbol order; 'Nothing' outside it.  A walk by
-- index reads one entry at a time, where 'attrSetToMap' copies them all.
attrSetAt :: (MonadCData m) => AttrSet -> Int -> m (Maybe (Symbol, Thunk))
attrSetAt attrs@(AttrSet cset) i = do
  size <- attrSetSize attrs
  if i < 0 || i >= size
    then pure Nothing
    else do
      let idx = fromIntegral i
      name <- cattrsetGetKey cset idx
      ptr <- cattrsetGetValue cset idx
      pure (Just (name, Thunk ptr))

-- | Check key membership without materializing thunks.
attrSetMember :: (MonadCData m) => Text -> AttrSet -> m Bool
attrSetMember key (AttrSet cset) = do
  sym <- symbolIntern key
  midx <- cattrsetIndex cset sym
  pure (case midx of Nothing -> False; Just _ -> True)

-- | Check if the attribute set is empty.
attrSetNull :: (MonadCData m) => AttrSet -> m Bool
attrSetNull (AttrSet cset) = (== 0) <$> cattrsetSize cset

-- | Number of attributes.
attrSetSize :: (MonadCData m) => AttrSet -> m Int
attrSetSize (AttrSet cset) = fromIntegral <$> cattrsetSize cset

-- | Full materialization: build a 'Map Text Thunk' from all entries.
-- Iterates the C array and builds a Haskell Map.  Expensive on large
-- sets - avoid on the hot path.
attrSetToMap :: (MonadCData m) => AttrSet -> m (Map Text Thunk)
attrSetToMap (AttrSet cset) = do
  n <- cattrsetSize cset
  if n == 0
    then pure Map.empty
    else Map.fromList <$> mapM materializeEntry [0 .. n - 1]
  where
    materializeEntry i = do
      sym <- cattrsetGetKey cset i
      ptr <- cattrsetGetValue cset i
      pure (symbolText sym, Thunk ptr)

-- | All thunk values (materialized).
attrSetElems :: (MonadCData m) => AttrSet -> m [Thunk]
attrSetElems attrs = Map.elems <$> attrSetToMap attrs

-- | Sorted key-value pairs (materialized).
attrSetToAscList :: (MonadCData m) => AttrSet -> m [(Text, Thunk)]
attrSetToAscList attrs = Map.toAscList <$> attrSetToMap attrs

-- | Map an action over all key-value pairs, building a new CAttrSet at a
-- site.  Materializes the source set, runs @f@ on each entry, and
-- rebuilds.
attrSetMapWithKey :: (MonadCData m) => SetSite -> (Text -> Thunk -> m Thunk) -> AttrSet -> m AttrSet
attrSetMapWithKey site f attrs = attrSetToMap attrs >>= Map.traverseWithKey f >>= attrSetFromMap site

-- | Remove a list of keys, returning a new CAttrSet made at a site.
-- Uses the C-side @nn_attrset_remove_keys@, which scans the keys for
-- each attribute.  Upstream
-- sizes the result for the set it removes from (primops.cc:2852 at
-- 2.24.9), so removing from an empty set gives 'emptyAttrSet' and
-- removing every attribute of another gives an empty set of its own.
-- Every key is interned either way, as upstream interns each name
-- whatever the set holds.
attrSetRemoveKeys :: (MonadCData m) => SetSite -> [Text] -> AttrSet -> m AttrSet
attrSetRemoveKeys site keys (AttrSet cset) = do
  syms <- mapM symbolIntern keys
  size <- cattrsetSize cset
  case size of
    0 -> pure emptyAttrSet
    _ -> AttrSet <$> cattrsetRemoveKeys site cset syms

-- | The set with no attributes: a C static ('cattrsetEmpty'), one address
-- for the life of the process, as upstream's @emptyBindings@ is one for
-- its evaluator (eval.hh:195 at 2.24.9).  Every set built with no
-- attributes is this one and is allocated nowhere, as upstream's
-- @allocBindings@ answers a size of 0 with @emptyBindings@
-- (attr-set.cc:16-17), except a set upstream sizes before it knows it
-- holds nothing ('attrSetFromEntriesSizedFor', 'attrSetRemoveKeys').
-- Upstream's printers enter a set by its bindings, so which empty sets
-- are this one shows where a value is printed.
emptyAttrSet :: AttrSet
emptyAttrSet = AttrSet cattrsetEmpty

-- | Build a CAttrSet at a site from a Haskell 'Map' of attributes defined
-- nowhere in source, as those of a set a builtin makes from scratch are;
-- a builtin that copies attributes keeps their positions with
-- 'attrSetFromEntries'.  The keys are interned in name order, so a name
-- no symbol holds yet takes its place in every set by name; a builtin
-- that meets new names in an order of its own interns each where
-- upstream's does and builds with 'attrSetFromSymbols'.  An empty map
-- gives 'emptyAttrSet'.
attrSetFromMap :: (MonadCData m) => SetSite -> Map Text Thunk -> m AttrSet
attrSetFromMap site m = AttrSet <$> newFrozenAttrSet site symbolIntern (const noPos) id m

-- | 'attrSetFromMap' for attributes that keep the positions they were
-- defined at, as an attribute set literal's and a copied attribute's do.
attrSetFromEntries :: (MonadCData m) => SetSite -> Map Text AttrEntry -> m AttrSet
attrSetFromEntries site m = AttrSet <$> newFrozenAttrSet site symbolIntern entryPos entryThunk m

-- | 'attrSetFromMap' over names already interned.
attrSetFromSymbols :: (MonadCData m) => SetSite -> Map Symbol Thunk -> m AttrSet
attrSetFromSymbols site m = AttrSet <$> newFrozenAttrSet site pure (const noPos) id m

-- | 'attrSetFromEntries' over names already interned.
attrSetFromSymbolEntries :: (MonadCData m) => SetSite -> Map Symbol AttrEntry -> m AttrSet
attrSetFromSymbolEntries site m = AttrSet <$> newFrozenAttrSet site pure entryPos entryThunk m

-- | 'attrSetFromEntries' for a set upstream sizes, before it knows what
-- the set holds, for the given number of attributes: a literal for its
-- names (eval.cc:1193 at 2.24.9), @intersectAttrs@ for the smaller of its
-- arguments (primops.cc:2945).  Upstream's @allocBindings@ shares its
-- empty set only for a size of 0 (attr-set.cc:16-17), so such a set
-- holding nothing is 'emptyAttrSet' when sized for none and an empty set
-- of its own otherwise.  A set holding attributes is made with room for
-- those alone, whatever the size.
attrSetFromEntriesSizedFor :: (MonadCData m) => SetSite -> Int -> Map Text AttrEntry -> m AttrSet
attrSetFromEntriesSizedFor site size m
  | Map.null m && size > 0 = ownEmptyAttrSet site
  | otherwise = attrSetFromEntries site m

-- | An empty set at a site that is not 'emptyAttrSet', made with room for
-- one entry: room for none asks @nn_attrset_new@ for its default room,
-- and is what the census counts as a set upstream would share.
ownEmptyAttrSet :: (MonadCData m) => SetSite -> m AttrSet
ownEmptyAttrSet site = do
  cset <- cattrsetNew site 1
  cattrsetFreeze cset
  pure (AttrSet cset)

-- | Insert every entry of a map into a new set at a site, under the
-- symbol, at the position and with the thunk the projections give it,
-- and freeze it; the shared empty set for an empty map.
{-# INLINE newFrozenAttrSet #-}
newFrozenAttrSet :: (MonadCData m) => SetSite -> (k -> m Symbol) -> (a -> PosIdx) -> (a -> Thunk) -> Map k a -> m CAttrSet
newFrozenAttrSet site symbolOf posOf thunkOf m
  | Map.null m = pure cattrsetEmpty
  | otherwise = do
      cset <- cattrsetNew site (Map.size m)
      forM_ (Map.toList m) $ \(key, entry) -> do
        sym <- symbolOf key
        cattrsetInsert cset sym (posOf entry) (unThunk (thunkOf entry))
      cattrsetFreeze cset
      pure cset

-- | Allocate a CAttrSet skeleton with keys only (NULL values), for an
-- environment's name table.
-- Used for two-phase construction in @rec {}@ and @let@ (knot-tying):
-- keys are known before thunks, so the CAttrSet is built first, then
-- values are filled via 'fillCAttrSetValues'.  The table is never a
-- value, so its entries carry no position.  A table of no keys is the
-- shared empty set, which 'fillCAttrSetValues' finds no key in.
buildCAttrSetKeys :: (MonadCData m) => [Text] -> m CAttrSet
buildCAttrSetKeys [] = pure cattrsetEmpty
buildCAttrSetKeys keys = do
  cset <- cattrsetNew SetScope (length keys)
  forM_ keys $ \key -> do
    sym <- symbolIntern key
    cattrsetInsert cset sym noPos nullPtr
  cattrsetFreeze cset
  pure cset

-- | Fill values into a pre-allocated CAttrSet (two-phase construction).
-- Looks up each key's index and writes the CThunkPtr at that position.
-- Keys not found in the CAttrSet are silently ignored (should not happen
-- in correct knot-tying code).
fillCAttrSetValues :: (MonadCData m) => CAttrSet -> Map Text Thunk -> m ()
fillCAttrSetValues cset thunkMap =
  forM_ (Map.toList thunkMap) $ \(key, Thunk ptr) -> do
    sym <- symbolIntern key
    midx <- cattrsetIndex cset sym
    case midx of
      Just idx -> cattrsetSetValue cset idx ptr
      Nothing -> pure ()

-- | Evaluation environment - C-backed scope chain.
--
-- An arena-allocated @nn_env_t@: its slots inline, then its parent when
-- it has one and its lazy scope and with-scopes when it has them
-- (@cbits/nn_env.h@).  Haskell holds only the pointer.  Variable lookup
-- has two paths:
--
-- * 'envLookupResolved': single C call for 'EResolvedVar'
-- * 'envLookup': name-based walk for 'EVar' (lazy scope + with-scopes)
newtype Env = Env (Ptr NnEnv)

-- | Pointer equality: two envs are equal iff they are the same C struct.
instance Eq Env where
  Env p1 == Env p2 = p1 == p2

-- | Compact show via C accessors ('inspectForDisplay').
instance Show Env where
  show env@(Env envPtr) =
    let sc = inspectForDisplay (cenvSlotCount envPtr)
        ls = inspectForDisplay (cenvLazyScope envPtr)
        par = inspectForDisplay (cenvParent envPtr)
        wc = inspectForDisplay (snd <$> envWithScopesRaw env)
     in "Env{"
          ++ show sc
          ++ " slots"
          ++ (if ls /= nullPtr then ", lazyScope" else "")
          ++ (if par /= nullPtr then ", parent" else "")
          ++ ", "
          ++ show wc
          ++ " withs}"

-- | Empty environment (no variables in scope): a C static, one address
-- for the life of the process.
emptyEnv :: Env
emptyEnv = Env cenvEmpty

-- | An env at a site holding the thunks in its slots, in order, under a
-- parent or none.  It sees its parent's with-scopes.
newEnv :: (MonadCData m) => EnvSite -> Maybe Env -> [Thunk] -> m Env
newEnv site parent thunks = do
  env <- newBlankEnv site parent (length thunks)
  fillSlots env thunks
  pure env

-- | An env at a site of empty slots under a parent or none, for
-- 'tieFrame' to fill.
newBlankEnv :: (MonadCData m) => EnvSite -> Maybe Env -> Int -> m Env
newBlankEnv site parent slotCount =
  Env <$> (checked "nn_env_new" =<< cenvNew site (fromIntegral slotCount) (parentPtr parent))

-- | An env at a site with no slots that binds the names of a lazy scope,
-- under a parent or none: the root env's builtins, scopedImport's names
-- over them, or a frame whose names are not all static.
newScopeEnv :: (MonadCData m) => EnvSite -> Maybe Env -> AttrSet -> m Env
newScopeEnv site parent (AttrSet cset) =
  Env <$> (checked "nn_env_new_scoped" =<< cenvNewScoped site 0 (parentPtr parent) (castPtr cset) nullPtr 0)

-- | The C pointer of a parent, NULL for none.
parentPtr :: Maybe Env -> Ptr NnEnv
parentPtr = maybe nullPtr (\(Env p) -> p)

-- | Look up a resolved variable by level and index.  Single C call:
-- O(level) parent hops in C, then O(1) array read.  The resolver chose
-- the level and index for this env's shape, so the NULL the C side
-- answers for one outside the env, or for a slot a frame has not filled
-- yet, is a defect in this library, raised rather than handed on as a
-- thunk the next force would dereference.
envLookupResolved :: (MonadCData m) => Int -> Int -> Env -> m Thunk
envLookupResolved level idx (Env envPtr) = do
  ptr <- cenvLookupResolved envPtr level idx
  if ptr == nullPtr
    then cdataIO (cInvariantViolated "nn_env_lookup_resolved" ("no thunk at level " ++ show level ++ ", index " ++ show idx))
    else pure (Thunk ptr)

-- | Name-based variable lookup: walk the parent chain checking
-- lazy scopes; fall back to with-scopes (from the starting env).
--
-- Positional slots are NOT searched here - used only for
-- 'EVar' lookups (let\/rec bindings, builtins, with-scopes).
envLookup :: (MonadCData m) => Text -> Env -> m (Maybe Thunk)
envLookup name (Env envPtr) = lexicalLookup envPtr
  where
    lexicalLookup ep = do
      ls <- cenvLazyScope ep
      scopeResult <-
        if ls /= nullPtr
          then attrSetLookup name (AttrSet (castPtr ls))
          else pure Nothing
      case scopeResult of
        Just val -> pure (Just val)
        Nothing -> do
          par <- cenvParent ep
          if par /= nullPtr
            then lexicalLookup par
            else do
              -- With-scopes from the STARTING env (C array)
              (startWiths, startWithCount) <- envWithScopesRaw (Env envPtr)
              lookupWithScopesC name startWiths startWithCount

-- | Walk with-scopes (C array) innermost to outermost.
-- Skips tagged lazy entries (bit 0 set) - those are thunk pointers
-- that can only be resolved by @evalWithVarScopes@ which has monadic
-- 'Nix.Eval.force'.  Defensive: an 'EVar' under a with is either a lexical
-- (barrier) binding or a static global, both found on the parent chain
-- before this fallback fires ('EVar' also blocks closure trimming, so
-- the chain is always intact) - but keep it for safety.
lookupWithScopesC :: (MonadCData m) => Text -> Ptr (Ptr ()) -> Word32 -> m (Maybe Thunk)
lookupWithScopesC name withArr count = go 0
  where
    go i
      | i >= fromIntegral count = pure Nothing
      | otherwise = do
          entry <- withScopeAt withArr i
          case entry of
            Left _ -> go (i + 1)
            Right scope -> do
              found <- attrSetLookup name scope
              case found of
                Just val -> pure (Just val)
                Nothing -> go (i + 1)

-- | Push a with-scope whose set is not evaluated yet onto the scope chain
-- (innermost position): the thunk that evaluates to it, tagged lazy
-- (bit 0 set) so that a lookup forces it on first use
-- ('resolveWithScopeAt').  Allocates a copy of the env, with a with-scopes
-- array one longer than the one it sees.
pushLazyWithScope :: (MonadCData m) => Thunk -> Env -> m Env
pushLazyWithScope (Thunk thunkPtr) (Env envPtr) =
  Env <$> (checked "nn_env_push_with" =<< cenvPushWith EnvWith envPtr (wordPtrToPtr (ptrToWordPtr (castPtr thunkPtr) .|. lazyWithScopeTag)))

-- | The bit that marks a with-scope entry as the thunk of a set not yet
-- evaluated, rather than the set: a thunk cell is word-aligned, so the
-- bit is otherwise zero.
lazyWithScopeTag :: WordPtr
lazyWithScopeTag = 1

-- | The entry at an index of a with-scopes array ('envWithScopesRaw'):
-- 'Left' the thunk of a set not yet evaluated ('pushLazyWithScope'), or
-- 'Right' the set.
withScopeAt :: (MonadCData m) => Ptr (Ptr ()) -> Int -> m (Either Thunk AttrSet)
withScopeAt withArr idx = classify . ptrToWordPtr <$> cdataIO (peekElemOff withArr idx)
  where
    classify word
      | word .&. lazyWithScopeTag /= 0 = Left (Thunk (wordPtrToPtr (word .&. complement lazyWithScopeTag)))
      | otherwise = Right (AttrSet (wordPtrToPtr word))

-- | Replace a lazy with-scope entry with the set its thunk evaluated to,
-- so later lookups read the set directly.  Every env sharing the array
-- would evaluate the thunk to the same set.
resolveWithScopeAt :: (MonadCData m) => Ptr (Ptr ()) -> Int -> AttrSet -> m ()
resolveWithScopeAt withArr idx (AttrSet cset) = cdataIO (pokeElemOff withArr idx (castPtr cset))

-- | The with-scopes an env sees, innermost first, and their count: those
-- of the first env on its parent chain, itself included, that has
-- with-scopes of its own (a with's env, or a trimmed closure's), or none.
envWithScopesRaw :: (MonadCData m) => Env -> m (Ptr (Ptr ()), Word32)
envWithScopesRaw (Env envPtr) = do
  owner <- cenvWithOwner envPtr
  if owner == nullPtr
    then pure (nullPtr, 0)
    else (,) <$> cenvWithScopes owner <*> cenvWithCount owner

-- | Build with-scopes for a trimmed env at a site that needs with-scope
-- access ('CapturesWithScopes').  Appends the root scope (builtins) as
-- the outermost entry so 'EWithVar' can fall back to builtins without
-- retaining the parent chain.  Returns C array + count.
withScopesForCapture :: (MonadCData m) => EnvSite -> Env -> m (Ptr (Ptr ()), Word32)
withScopesForCapture site (Env envPtr) = do
  rootPtr <- cenvRootScope envPtr
  (existingWiths, existingCount) <- envWithScopesRaw (Env envPtr)
  if rootPtr == nullPtr
    then pure (existingWiths, existingCount)
    else do
      -- existingCount sits far below 2^32 (the C side caps the array
      -- allocation at UINT32_MAX bytes), so the increment cannot wrap.
      let newCount = existingCount + 1
      arr <- checked "nn_env_alloc_with_scopes" =<< cenvAllocWithScopes site newCount
      cdataIO $ do
        forM_ [0 .. fromIntegral existingCount - 1] $ \i -> do
          val <- peekElemOff existingWiths i
          pokeElemOff arr i val
        -- The root scope goes last: it is the outermost.
        pokeElemOff arr (fromIntegral existingCount) rootPtr
      pure (arr, newCount)

-- | Compile an expression to bytecode, answering the index of its code.
-- Appends to the bytecode store; the same expression compiled twice
-- answers two indices running the same code.
compile :: (MonadCData m) => Expr -> m Word32
compile expr = cdataIO (compileExpr expr)

-- | 'compile' for parsed source, whose names are interned first, in the
-- order upstream's parser interns them ('compileParsed').
compileSource :: (MonadCData m) => ParsedSource -> m Word32
compileSource parsed = cdataIO (compileParsed parsed)

-- | Create an unevaluated thunk with a fresh C arena-allocated cell,
-- compiling the Expr to bytecode first.
mkThunk :: (MonadCData m) => Env -> Expr -> m Thunk
mkThunk env thunkExpr = compile thunkExpr >>= mkThunkBc env

-- | Like 'mkThunk' but avoids C arena allocation for trivial expressions.
-- Resolved variables reuse the existing thunk from the env (no wrapper).
-- Literals use inline C scalars.
-- Lambdas compile formals+body to bytecode and produce VLambda directly.
-- Everything else falls back to 'mkThunk'.
cheapThunk :: (MonadCData m) => Env -> Expr -> m Thunk
cheapThunk env (EResolvedVar level idx) = envLookupResolved level idx env
cheapThunk _ (ELit (NixInt n)) = Thunk <$> cthunkNewComputedInt n
cheapThunk _ (ELit (NixFloat n)) = Thunk <$> cthunkNewComputedFloat n
cheapThunk _ (ELit (NixBool b)) = pure (Thunk (cthunkBool b))
cheapThunk _ (ELit NixNull) = pure (Thunk cthunkNull)
cheapThunk env (ELambda formals body captureInfo) = do
  closureEnv <- buildCaptureEnv EnvClosure env captureInfo
  bodyBcIdx <- compile body
  evalFormals <- cdataIO (compileFormalsToEval formals)
  evaluated (VLambda closureEnv evalFormals bodyBcIdx)
cheapThunk env expr = mkThunk env expr

-- | The env, at a site, that a closure made in an env runs in, by its
-- capture info: 'NoCaptureInfo' keeps the env as it is, 'Captures' makes
-- an env of the captured slots with no parent, and 'CapturesWithScopes'
-- gives that env the with-scopes as its own ('withScopesForCapture').
buildCaptureEnv :: (MonadCData m) => EnvSite -> Env -> CaptureInfo -> m Env
buildCaptureEnv _ env NoCaptureInfo = pure env
buildCaptureEnv site env (Captures captureList) =
  capturedThunks env captureList >>= newEnv site Nothing
buildCaptureEnv site env (CapturesWithScopes captureList) = do
  thunks <- capturedThunks env captureList
  (withArr, withCount) <- withScopesForCapture site env
  trimmed <- Env <$> (checked "nn_env_new_scoped" =<< cenvNewScoped site (fromIntegral (length thunks)) nullPtr nullPtr withArr withCount)
  fillSlots trimmed thunks
  pure trimmed

-- | The thunks a closure captures, read from the env it is made in by
-- their resolved level and index.
capturedThunks :: (MonadCData m) => Env -> [(Int, Int)] -> m [Thunk]
capturedThunks env = mapM (\(lvl, idx) -> envLookupResolved lvl idx env)

-- | A pending thunk running the code at a bytecode index in an env: a
-- fresh PENDING cell holding the index and the env's C pointer, neither
-- of them on the GHC heap.  Forcing the t'Thunk' runs the code in that
-- env.  The env's slots need not be filled yet: 'tieFrame' builds a
-- recursive frame's env before the thunks that point at it and fills its
-- slots after.
mkThunkBc :: (MonadCData m) => Env -> Word32 -> m Thunk
mkThunkBc (Env envPtr) bcIdx = Thunk <$> cthunkNewBc bcIdx (castPtr envPtr)

-- | The code a PENDING or BLACKHOLE thunk runs, and the env it runs in, as
-- 'mkThunkBc' stored them.
pendingCode :: (MonadCData m) => CThunkPtr -> m (Env, Word32)
pendingCode ptr = (,) <$> pendingThunkEnv ptr <*> cthunkGetBcIdx ptr

-- | The env a PENDING or BLACKHOLE thunk's code runs in, as 'mkThunkBc'
-- stored it.
pendingThunkEnv :: (MonadCData m) => CThunkPtr -> m Env
pendingThunkEnv ptr = Env . castPtr <$> cthunkPayload ptr

-- | Like 'cheapThunk' but for bytecode indices (used inside evalBytecode).
-- Short-circuits for resolved vars and literals without creating a
-- pending thunk.  Everything else falls back to 'mkThunkBc'.
cheapThunkBc :: (MonadCData m) => Env -> Word32 -> m Thunk
cheapThunkBc env bcIdx =
  cbcOpcode bcIdx >>= \case
    OpResolvedVar -> do
      level <- cbcArg1 bcIdx
      idx <- cbcArg2 bcIdx
      envLookupResolved (fromIntegral level) (fromIntegral idx) env
    OpLitInt -> do
      w64 <- literalWord64 bcIdx
      Thunk <$> cthunkNewComputedInt (fromIntegral w64)
    OpLitFloat -> do
      w64 <- literalWord64 bcIdx
      Thunk <$> cthunkNewComputedFloat (castWord64ToDouble w64)
    OpLitBool -> do
      flag <- cbcShortArg bcIdx
      pure (Thunk (cthunkBool (flag /= 0)))
    OpLitNull -> pure (Thunk cthunkNull)
    OpLitUri -> Thunk <$> (cbcArg1 bcIdx >>= cbcString >>= cthunkNewComputedStr)
    OpStr ->
      literalStr bcIdx >>= \case
        Just str -> Thunk <$> cthunkNewComputedStr str
        Nothing -> mkThunkBc env bcIdx
    OpLitPath ->
      constantPathValue bcIdx >>= \case
        Just resolved -> Thunk <$> cthunkNewComputedPath resolved
        Nothing -> mkThunkBc env bcIdx
    OpList -> do
      empty <- emptyBcList bcIdx
      if empty
        then pure emptyListThunk
        else mkThunkBc env bcIdx
    _ -> mkThunkBc env bcIdx

-- | The one cell every empty list literal evaluates to, as upstream's
-- @vEmptyList@ is one value: a C static, like the @null@ cell, so it is
-- the same in every arena window.
emptyListThunk :: Thunk
emptyListThunk = Thunk cthunkEmptyList

-- | The 64 bits of an integer or float literal, split across the
-- instruction's two 32-bit arguments, low word first.
literalWord64 :: (MonadCData m) => Word32 -> m Word64
literalWord64 bcIdx = do
  lo <- cbcArg1 bcIdx
  hi <- cbcArg2 bcIdx
  pure (fromIntegral lo .|. (fromIntegral hi `shiftL` 32))

-- | The bytes of a path literal's value, when producing it is not work:
-- the literal's own in the code when they are already canonical, as they
-- are for any path written canonically, and a copy of the canonical form
-- otherwise.
--
-- Parsing resolves every relative path literal against the file's directory
-- ('Nix.Expr.Resolve.resolveRelativePaths'), so what reaches here is already
-- absolute and only wants the lexical canonicalisation every path value
-- carries.  The exception is a home-relative literal, which parsing leaves
-- alone: expanding it reads @HOME@, and that is an effect this cannot run.
constantPathValue :: (MonadCData m) => Word32 -> m (Maybe CStrPtr)
constantPathValue bcIdx = do
  code <- cbcArg1 bcIdx >>= cbcString
  text <- cstrText code
  let canonical = canonPathValue text
      held = if canonical == text then HeldStr code else NotHeld
  if T.isPrefixOf "~/" text
    then pure Nothing
    else Just <$> pathCell canonical held

-- | Whether a list expression has no elements, and so is the same empty list
-- however many times it is written.  Upstream hands back one shared
-- @vEmptyList@ for this rather than a thunk; here it is 'emptyListThunk'.
emptyBcList :: (MonadCData m) => Word32 -> m Bool
emptyBcList bcIdx = do
  (count, _) <- cbcCountedPayload bcIdx =<< cbcArg1 bcIdx
  pure (count == 0)

-- | @NN_STRPART_LIT@ from @cbits/nn_bytecode.h@: a string part that is
-- literal text rather than an interpolation.
strPartLiteral :: Word32
strPartLiteral = 0

-- | The string a string expression makes when every part of it is
-- literal, or 'Nothing' when one is an interpolation and producing the
-- value is work.  A string with no interpolation is one literal part,
-- since the lexer joins a run of characters and escapes into one chunk,
-- and its value is that part's own string in the code, which the cell
-- points at rather than a copy, as upstream's @ExprString@ hands out the
-- value it holds (@ExprString::maybeThunk@, eval.cc:1024-1028 at 2.24.9).
--
-- Upstream draws this line in the parser: a fully literal string folds into
-- an @ExprString@ carrying a ready-made value, and only a string with
-- interpolation survives as @ExprConcatStrings@.  The split is made here
-- instead by reading the parts back, because this compiler emits one opcode
-- for both shapes.
--
-- Deliberately not extended to @OpIndStr@, whose common indentation is
-- stripped during evaluation rather than in the parser, so its value is not
-- yet a constant at this point.  Upstream can treat one as a constant
-- because its parser has already done that stripping.
literalStr :: (MonadCData m) => Word32 -> m (Maybe CStrPtr)
literalStr bcIdx = do
  (count, dataOff) <- cbcCountedPayload bcIdx =<< cbcArg1 bcIdx
  partIndices count dataOff >>= \case
    Just [index] -> Just <$> cbcString index
    Just indices -> Just <$> (mapM cbcStringBytes indices >>= cstrNew . BS.concat)
    Nothing -> pure Nothing
  where
    partIndices 0 _ = pure (Just [])
    partIndices n off = do
      tag <- cbcData off
      if tag /= strPartLiteral
        then pure Nothing
        else do
          index <- cbcData (off + 1)
          fmap (index :) <$> partIndices (n - 1 :: Int) (off + 2)

-- | Wrap an already-computed value as a thunk.
-- Ints, floats and a builtin with no argument taken are inline in a new
-- cell; booleans and null are the shared static cells, as upstream hands
-- out one @vTrue@, @vFalse@ and @vNull@ (eval.cc at 2.24.9,
-- @EvalState::getBool@).  Every other value is a new cell pointing at its
-- C object, a string or a path over the bytes it holds or a copy of
-- them, and a partial application over a new object holding the cells
-- of its arguments.
evaluated :: (MonadCData m) => NixValue -> m Thunk
evaluated val =
  Thunk <$> case val of
    VInt n -> cthunkNewComputedInt n
    VFloat d -> cthunkNewComputedFloat d
    VBool b -> pure (cthunkBool b)
    VNull -> pure cthunkNull
    VAttrs (AttrSet cset) -> cthunkNewComputedAttrs (castPtr cset)
    VPathHeld p held -> pathCell p held >>= cthunkNewComputedPath
    VStrHeld t ctx held ->
      stringCell t ctx held >>= \case
        StrCell str -> cthunkNewComputedStr str
        CtxStrCell csptr -> cthunkNewComputedCtxStr (castPtr csptr)
    VList (CList clistPtr) -> cthunkNewComputedList (castPtr clistPtr)
    VLambda (Env envPtr) formals bodyBcIdx -> marshalLambda envPtr formals bodyBcIdx >>= cthunkNewComputedLambda
    VBuiltin builtin args ->
      primopCell builtin args >>= \case
        PrimopCell primop -> cthunkNewComputedPrimop primop
        PrimopAppCell app -> cthunkNewComputedPrimopApp (castPtr app)

-- | What a cell holding a builtin value holds.
data PrimopCell
  = -- | A builtin with no argument taken: its index, inline.
    PrimopCell !Word32
  | -- | A partial application.
    PrimopAppCell !CPrimopAppPtr

-- | The C form of a builtin value: its index, or a partial application
-- holding the cells of the arguments it has taken, an argument taken
-- forced in a new cell for its value and any other as the cell it is.
primopCell :: (MonadCData m) => Builtin -> [BuiltinArg] -> m PrimopCell
primopCell builtin [] = pure (PrimopCell (builtinIndex builtin))
primopCell builtin args = do
  cells <- traverse argumentCell args
  PrimopAppCell <$> cprimopAppNew (builtinIndex builtin) forced cells
  where
    argumentCell (ArgForced val) = unThunk <$> evaluated val
    argumentCell (ArgDeferred (Thunk cell)) = pure cell
    forced = foldl' (.|.) 0 [bit i | (i, ArgForced _) <- zip [0 ..] args]

-- | Check if two thunks share the same memoization cell (C pointer
-- equality): upstream's test of one @Value@'s address against another's,
-- which @==@ makes once both sides are forced.
thunkSameRef :: Thunk -> Thunk -> Bool
thunkSameRef (Thunk p1) (Thunk p2) = p1 == p2

-- | Extract the C pointer from a thunk (zero-cost, newtype unwrap).
thunkToCPtr :: Thunk -> CThunkPtr
thunkToCPtr (Thunk ptr) = ptr

-- | The C object a cell holding a string value points at.
data StringCell
  = -- | A context-free string's bytes.
    StrCell !CStrPtr
  | -- | A string with context.
    CtxStrCell !CCtxStrPtr

-- | The C object for a cell holding a string: the one its bytes are held
-- in, or a new one made from them.  A string with context held whole is
-- reused only while it still has context, and a context-free one only
-- while it is context-free, which is how a cell read gives them.
stringCell :: (MonadCData m) => ByteString -> StringContext -> HeldBytes -> m StringCell
stringCell bytes ctx held
  | ctx == emptyContext = StrCell <$> heldOr (cstrNew bytes) held
  | otherwise = case held of
      HeldCtxStr csptr -> pure (CtxStrCell csptr)
      _ -> CtxStrCell <$> marshalStringContext bytes ctx
  where
    heldOr _ (HeldStr str) = pure str
    heldOr make _ = make

-- | The C string for a cell holding a path: the one its bytes are held
-- in, or a copy of its text's bytes.
pathCell :: (MonadCData m) => Text -> HeldBytes -> m CStrPtr
pathCell _ (HeldStr str) = pure str
pathCell path _ = cstrFromText path

-- | Marshal Env + EvalFormals + body bc_idx to a C nn_lambda_t.
marshalLambda :: (MonadCData m) => Ptr NnEnv -> EvalFormals -> Word32 -> m (Ptr ())
marshalLambda envPtr formals bodyBcIdx = case formals of
  EFName name -> do
    Symbol nameSym <- symbolIntern name
    lam <- checked "nn_lambda_new" =<< clambdaNew envPtr bodyBcIdx 0 nameSym invalidSymbolId 0 0
    pure (castPtr lam)
  EFSet lambdaName entries allowExtra -> do
    lambdaSym <- lambdaNameSymbol lambdaName
    let count = fromIntegral (length entries) :: Word32
        extraFlag = if allowExtra then 1 else 0 :: Word8
    lam <- checked "nn_lambda_new" =<< clambdaNew envPtr bodyBcIdx 1 0 lambdaSym extraFlag count
    fillEntries lam 0 entries
    pure (castPtr lam)
  EFNamedSet lambdaName name entries allowExtra -> do
    lambdaSym <- lambdaNameSymbol lambdaName
    Symbol nameSym <- symbolIntern name
    let count = fromIntegral (length entries) :: Word32
        extraFlag = if allowExtra then 1 else 0 :: Word8
    lam <- checked "nn_lambda_new" =<< clambdaNew envPtr bodyBcIdx 2 nameSym lambdaSym extraFlag count
    fillEntries lam 0 entries
    pure (castPtr lam)
  where
    fillEntries _ _ [] = pure ()
    fillEntries lam !idx (EvalFormal name defBcIdx : rest) = do
      Symbol nameSym <- symbolIntern name
      let (hasDef, defIdx) = case defBcIdx of
            Nothing -> (0, 0)
            Just di -> (1, di)
      clambdaSetEntry lam idx nameSym hasDef defIdx
      fillEntries lam (idx + 1) rest

-- | Read a C nn_lambda_t back into VLambda.
-- Reconstructs Env (newtype wrap), EvalFormals (from symbol IDs), body bc_idx.
unmarshalLambdaValue :: (MonadCData m) => Ptr () -> m NixValue
unmarshalLambdaValue rawPtr = do
  let lamPtr = castPtr rawPtr
  envPtr <- clambdaEnv lamPtr
  bodyIdx <- clambdaBody lamPtr
  formalsType <- clambdaFormalsType lamPtr
  formals <- case formalsType of
    0 {- Name -} -> do
      nameSym <- clambdaNameSym lamPtr
      pure (EFName (symbolText (Symbol nameSym)))
    1 {- Set -} -> do
      lambdaSym <- clambdaLambdaNameSym lamPtr
      count <- clambdaFormalCount lamPtr
      extra <- clambdaAllowExtra lamPtr
      entries <- readLambdaEntries lamPtr count
      pure (EFSet (lambdaNameFromSymbol lambdaSym) entries (extra /= 0))
    _ {- NamedSet -} -> do
      lambdaSym <- clambdaLambdaNameSym lamPtr
      nameSym <- clambdaNameSym lamPtr
      count <- clambdaFormalCount lamPtr
      extra <- clambdaAllowExtra lamPtr
      entries <- readLambdaEntries lamPtr count
      pure (EFNamedSet (lambdaNameFromSymbol lambdaSym) (symbolText (Symbol nameSym)) entries (extra /= 0))
  pure (VLambda (Env envPtr) formals bodyIdx)
  where
    -- Zero-formal set patterns ({}:, { ... }:) store count 0 and a NULL
    -- entries array; count - 1 would underflow Word32.
    readLambdaEntries lamPtr count
      | count == 0 = pure []
      | otherwise = mapM (readOneEntry lamPtr) [0 .. count - 1]
    readOneEntry lamPtr idx = do
      nameSym <- clambdaEntryName lamPtr idx
      hasDef <- clambdaEntryHasDefault lamPtr idx
      defIdx <- clambdaEntryDefault lamPtr idx
      let defMaybe = if hasDef /= 0 then Just defIdx else Nothing
      pure (EvalFormal (symbolText (Symbol nameSym)) defMaybe)

-- | Marshal payload bytes + StringContext to a C nn_ctxstr_t, each a
-- string's bytes: the payload as it is, and the StorePath and output-name
-- fields as their text's UTF-8.
marshalStringContext :: (MonadCData m) => ByteString -> StringContext -> m CCtxStrPtr
marshalStringContext textVal (StringContext ctxSet) = do
  text <- cstrNew textVal
  let elems = Set.toAscList ctxSet
      count = fromIntegral (length elems) :: Word32
  ptr <- checked "nn_ctxstr_new" =<< cctxstrNew text count
  fillElems ptr 0 elems
  pure ptr
  where
    fillElems _ _ [] = pure ()
    fillElems ptr !idx (e : es) = do
      marshalElem ptr idx e
      fillElems ptr (idx + 1) es
    marshalElem eptr eidx (SCPlain (StorePath h n)) = do
      hash <- cstrFromText h
      name <- cstrFromText n
      cctxstrSetPlain eptr eidx hash name
    marshalElem eptr eidx (SCDrvOutput (StorePath h n) out) = do
      hash <- cstrFromText h
      name <- cstrFromText n
      output <- cstrFromText out
      cctxstrSetDrvOutput eptr eidx hash name output
    marshalElem eptr eidx (SCAllOutputs (StorePath h n)) = do
      hash <- cstrFromText h
      name <- cstrFromText n
      cctxstrSetAllOutputs eptr eidx hash name

-- | Unmarshal a C nn_ctxstr_t back to (payload bytes, StringContext).
unmarshalStringContext :: (MonadCData m) => CCtxStrPtr -> m (ByteString, StringContext)
unmarshalStringContext ptr = do
  textVal <- cctxstrText ptr >>= cstrBytes
  count <- cctxstrCtxCount ptr
  -- count - 1 underflows Word32 at 0 (defensive: marshal sites store
  -- only non-empty contexts).
  elems <-
    if count == 0
      then pure []
      else mapM (readElem ptr) [0 .. count - 1]
  pure (textVal, StringContext (Set.fromList elems))
  where
    readElem cptr idx = do
      tag <- cctxstrElemTag cptr idx
      hash <- cctxstrElemHash cptr idx >>= cstrText
      name <- cctxstrElemName cptr idx >>= cstrText
      -- Raw construction (Nix.Store.Path.Internal): provenance-safe -
      -- these strings were made from a validated StorePath's own fields
      -- by marshalStringContext, so this is a round-trip, not a parse
      -- boundary.
      let sp = StorePath hash name
      case tag of
        0 -> pure (SCPlain sp)
        1 -> SCDrvOutput sp <$> (cctxstrElemOutput cptr idx >>= cstrText)
        _ -> pure (SCAllOutputs sp)

-- | Tie a frame whose slots hold thunks that may point back at it: the
-- env of @slotCount@ empty slots under the parent, at the site given, is
-- allocated first, the thunks are made against it, and the slots are
-- filled last, as upstream allocates @env2@ before it fills the values
-- (@ExprLet::eval@, @ExprAttrs::eval@ and @EvalState::callFunction@,
-- eval.cc at 2.24.9).
-- @mkThunks@ answers one thunk per slot, in slot order, and must not
-- read the frame's own slots, which are not all filled until this
-- returns ('mkThunkBc' does not; 'envLookupResolved' refuses an unfilled
-- one).  Nothing is evaluated in the frame before then, which a with's
-- env relies on when it copies the slots of the env it is made in
-- ('pushLazyWithScope').  Answers the env and those thunks.
tieFrame :: (MonadCData m) => EnvSite -> Maybe Env -> Int -> (Env -> m [Thunk]) -> m (Env, [Thunk])
tieFrame site parent slotCount mkThunks = do
  env <- newBlankEnv site parent slotCount
  thunks <- mkThunks env
  fillSlots env (take slotCount thunks)
  pure (env, thunks)

-- | Write thunks into an env's slots, first to last from slot 0.  There
-- are no more thunks than slots.
fillSlots :: (MonadCData m) => Env -> [Thunk] -> m ()
fillSlots (Env envPtr) thunks = do
  slots <- cenvSlots envPtr
  cdataIO (zipWithM_ (\i thunk -> pokeElemOff slots i (thunkToCPtr thunk)) [0 ..] thunks)

-- | Reject the NULL a C constructor answers when it cannot allocate,
-- raising 'Nix.Eval.CStatus.CStatusError' naming the entry point.
checked :: (MonadCData m) => String -> Ptr a -> m (Ptr a)
checked site ptr = cdataIO (checkedCPtr site ptr)

-- | A value's type as an error message names it: upstream's @showType@ on
-- a value (eval.cc at 2.24.9, the same in 2.33.2), which every type error
-- there uses.  It names a primop and the string context the type alone
-- does not show.  Upstream's other cases (a thunk, a black hole, a
-- function application, an external value) have no 'NixValue': a value
-- here is always forced, and there are no plugins.
typeName :: NixValue -> Text
typeName val = case val of
  VInt _ -> "an integer"
  VFloat _ -> "a float"
  VBool _ -> "a Boolean"
  VNull -> "null"
  VStr _ ctx
    | ctx == emptyContext -> "a string"
    | otherwise -> "a string with context"
  VPath _ -> "a path"
  VList _ -> "a list"
  VAttrs _ -> "a set"
  VLambda {} -> "a function"
  VBuiltin builtin [] -> "the built-in function '" <> builtinName builtin <> "'"
  VBuiltin builtin _ -> "the partially applied built-in function '" <> builtinName builtin <> "'"

-- | @builtins.typeOf@: the name of upstream's @ValueType@, the
-- classification its type checks compare (a lambda and a primop are both
-- @nFunction@).
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

-- ---------------------------------------------------------------------------
-- Evaluation monad
-- ---------------------------------------------------------------------------

-- | What @builtins.pathExists@ asks of a path, decided from the
-- argument's spelling as upstream's @prim_pathExists@ decides it
-- (primops.cc at 2.24.9).
data PathExistence
  = -- | Whether an entry is there: the ancestors are resolved and the
    -- final component is checked in place, so a symlink exists whatever
    -- it points at (upstream's @SymlinkResolution::Ancestors@).
    ExistsAsEntry
  | -- | Whether a directory is there, asked by a string ending in @/@ or
    -- @/.@: every symlink is followed, the final one included
    -- (@SymlinkResolution::Full@), and a file answers false.
    ExistsAsDirectory
  deriving (Eq, Show)

-- | Effect class for Nix evaluation.  Core logic is polymorphic in @m@
-- so the same evaluator composes into t'PureEval' for tests or @IO@ for
-- real file-system access (e.g. @import@, @readFile@).  Every evaluator
-- runs the C data layer's actions in sequence with its own
-- ('MonadCData').
class (MonadCData m) => MonadEval m where
  -- | Raise an evaluation error (type error, missing attribute, IO
  -- failure).  NOT caught by @builtins.tryEval@: upstream Nix catches only
  -- ThrownError\/AssertionError there, and every other EvalError escapes.
  throwEvalError :: Text -> m a

  -- | Raise a catchable error - @builtins.throw@ and a failed @assert@.
  -- These are the only errors 'catchEvalError' (tryEval) recovers from.
  throwCatchableError :: Text -> m a

  -- | Abort evaluation (uncatchable by tryEval, matching real Nix).
  abortEvaluation :: Text -> m a

  -- | tryEval semantics: recover from a 'throwCatchableError'
  -- (throw\/assert); eval errors and aborts propagate.
  catchEvalError :: m a -> m (Either Text a)

  -- | Run the action; when it fails with ANY evaluation failure
  -- (catchable throw, eval error, or abort), run the cleanup and let
  -- the failure propagate unchanged.  The error half of a bracket -
  -- success runs no cleanup - for builtins that hold external state
  -- (a scratch directory) across throwing calls.  'catchEvalError'
  -- cannot express this: it recovers only catchable throws, and a
  -- failed fetch is an eval error.
  onEvalError :: m a -> m () -> m a

  -- | @addErrorTrace line action@ runs the action; a failure escaping it
  -- (a throw, an eval error or an abort) runs @line@ for a line of context
  -- and gains that line outside the lines it already carries, as
  -- upstream's @Error::addTrace@ adds one where a @catch@ rethrows.
  -- Nothing else about the failure changes: its message stays the same,
  -- and a throw stays catchable by @builtins.tryEval@.  The line is made
  -- only on failure, and a failure making it escapes in place of the
  -- action's, as an exception thrown inside upstream's @catch@ replaces
  -- the one caught (@prim_addErrorContext@, primops.cc:831-844 at 2.24.9).
  addErrorTrace :: m Text -> m a -> m a

  -- | Run an application one call frame deeper, or refuse it with
  -- upstream's "stack overflow; max-call-depth exceeded" when the frames
  -- already active exceed the ceiling ('Nix.Eval.CallDepth.enterCallFrame'
  -- is the rule).  An eval error, not a throw: upstream raises an
  -- EvalError, which its tryEval does not catch.  The frame ends with the
  -- action, on failure too, so an error unwinding through a tryEval
  -- leaves the count that tryEval was entered at.
  withCallFrame :: m a -> m a

  -- | Whether something is at the path, as @builtins.pathExists@ asks
  -- it; the query says how the final component is read.  A path the
  -- policy refuses is absent.
  doesPathExist :: PathExistence -> Text -> m Bool

  -- | List a directory, returning @(name, fileType)@ pairs.
  -- @fileType@ is one of @"regular"@, @"directory"@, @"symlink"@ or
  -- @"unknown"@ (matching Nix's @builtins.readDir@ semantics), and
  -- 'Nothing' for an entry whose lstat is refused with @EACCES@ or
  -- @EPERM@, which upstream's @readDirectory@ gives no type
  -- (posix-source-accessor.cc:138-149 at 2.24.9).
  listDirectory :: Text -> m [(Text, Maybe Text)]

  importFile :: Text -> m NixValue

  -- | Look up an environment variable by the bytes of its name, answering
  -- the bytes of its value, or @""@ if unset.
  getEnvVar :: ByteString -> m ByteString

  -- | Get the current epoch time (seconds since 1970-01-01).
  getCurrentTime :: m Int64

  -- | Write a named file to the store at upstream's text-path
  -- (@text:\<refs\>@ scheme, flat sha256 of the contents), returning the
  -- canonical store path.  The contents are raw bytes (a Nix string's
  -- payload) - hashed and stored exactly as given.  The refs are the
  -- contents' plain store-path references; they participate in the path
  -- computation exactly as upstream's addTextToStore.
  writeToStore :: Text -> ByteString -> [StorePath] -> m Text

  -- | Import a file with a custom scope overlaid on builtins.
  scopedImportFile :: [(Text, Thunk)] -> Text -> m NixValue

  -- | Read raw bytes from a file.  Used by @builtins.hashFile@.
  readFileBytes :: Text -> m ByteString

  -- | Classify a single filesystem path as @"regular"@, @"directory"@,
  -- @"symlink"@, or @"unknown"@: an lstat of the path itself, refused
  -- when a symlink sits anywhere above it, as upstream's accessor
  -- refuses one.  Used by @builtins.readFileType@ and, on a resolved
  -- root, by the source tree walk.
  getFileType :: Text -> m Text

  -- | Follow every symlink in a path, as upstream's
  -- @SourcePath::resolveSymlinks@ does before a tree is read
  -- (@addPath@, @prim_filterSource@): no component of the result is a
  -- symlink, so the lstats of the walk that follows never meet one
  -- above the entry they classify.
  resolveSymlinks :: Text -> m Text

  -- | Run an external process: @(command, args, stdin) -> (exitCode, stdout, stderr)@.
  runProcess :: Text -> [Text] -> Text -> m (Int, Text, Text)

  -- | Create a fetcher scratch directory under the system temp dir: the
  -- given name prefix plus an unpredictable suffix, exclusively created.
  -- A fixed scratch name in the shared temp dir gives any other local
  -- process a pre-place\/swap window between creation and the store
  -- copy, so the name must be unguessable and creation must fail rather
  -- than adopt an existing directory.
  createScratchDir :: Text -> m Text

  -- | Recursively remove a directory 'createScratchDir' created.
  removeScratchDir :: Text -> m ()

  -- | Resolve a path literal to a canonical path.
  -- In IO evaluation, @~/@ resolves against the home directory and other
  -- relative paths resolve against the current file's directory
  -- (@esBaseDir@); this ensures that path values captured in closures
  -- remain valid after the import scope ends.  In pure evaluation the
  -- text is not absolutized.  Every implementation ends with lexical
  -- canonicalization ('Nix.Eval.CanonPath.canonPath'), as upstream: no
  -- dot segment or repeated separator survives into a path value.
  resolvePathLiteral :: Text -> m Text

  -- | Copy a path (file or directory) to the store, returning the store
  -- path.  Content-addressed like upstream addToStore: recursive NAR
  -- sha256 under the given name.  Arguments: source path, store name,
  -- and an optional sha256 pin as (expected digest, refusal): a copy
  -- whose digest is another fails before anything is written, with the
  -- refusal of the digest it has.  @builtins.path@ and
  -- @builtins.fetchTarball@ share this pin.
  copyPathToStore :: Text -> Text -> Maybe (ByteString, ByteString -> Text) -> m Text

  -- | Recursive NAR sha256 digest of a path, without copying it anywhere.
  -- Used by @builtins.fetchGit@ to report @narHash@.
  narHashOfPath :: Text -> m ByteString

  -- | Whether a regular file carries the executable bit (NAR encodes it).
  isExecutableFile :: Text -> m Bool

  -- | Mark a regular file executable (mode bit on Unix, an alternate data
  -- stream on Windows). Run before the tree is copied to the store.
  setExecutableFile :: Text -> m ()

  -- | Look up a previously recorded fetch by a key naming exactly what was
  -- fetched. 'Nothing' when nothing was recorded, or the record no longer
  -- describes anything on disk.
  lookupFetchCache :: Text -> m (Maybe Text)

  -- | Take over a store path an earlier evaluation wrote: 'True' when it is
  -- still on disk, and recorded as this evaluation's own store write in
  -- that case. Reusing a path instead of rewriting it must still leave the
  -- registration behind, or the build driver never registers it and a
  -- derivation naming it fails with \"path \'...\' is not valid\".
  adoptStorePath :: Text -> m Bool

  -- | Record a fetch under that key. Failing to record is not an error -
  -- the cache is an optimisation.
  writeFetchCache :: Text -> Text -> m ()

  -- | Read a symlink's target WITHOUT following it.
  readSymlinkTarget :: Text -> m Text

  -- | Unpack a serialized NAR into the store under the given name at its
  -- canonical recursive fixed-output path (sha256 of the bytes),
  -- returning the store path text.  Used by @builtins.path@ and
  -- @builtins.filterSource@ with a filter, whose filtered tree exists
  -- only as a NAR value and never on the source filesystem.
  addSourceNar :: Text -> ByteString -> m Text

  -- | Write raw bytes to the store at a canonical flat fixed-output path
  -- (@makeFixedOutputPath name "sha256" FlatIngestion@), so a hash-pinned
  -- fetch lands at the same store path C++ Nix computes - reproducible and
  -- cacheable.
  addFixedOutputFile :: Text -> ByteString -> m Text

  -- | Print a trace/warning message.
  -- IO evaluators write to stderr; pure evaluators silently discard.
  traceMessage :: Text -> m ()

  -- | Force a thunk to a value, with memoization.
  -- IO evaluators should cache results (via per-thunk @IORef@).
  -- Pure evaluators re-evaluate each time.
  -- The first argument is the bytecode evaluation function (to break
  -- the Eval.Types to Eval circular dependency).
  forceThunk :: (Env -> Word32 -> m NixValue) -> Thunk -> m NixValue

  -- | Look up a cached derivation modulo-hash (hex) by its @.drv@ store
  -- path.  Populated bottom-up as each derivation is computed; used by the
  -- derivation-hash algorithm to substitute input derivations.  Pure
  -- evaluators have no cache and always return 'Nothing'.
  lookupDrvHash :: Text -> m (Maybe Text)

  -- | Cache a derivation's modulo hash (hex) under its @.drv@ store path.
  -- A no-op in pure evaluators (no memoization).
  cacheDrvHash :: Text -> Text -> m ()

  -- | Record a derivation's serialized @.drv@ ATerm (the exact bytes whose
  -- hash is its store path) under its @.drv@ store path, as each derivation
  -- is computed (bottom-up).  Unlike 'cacheDrvHash' (which stores only the
  -- modulo hash), this retains the full ATerm so the build driver can
  -- materialize the entire input-@.drv@ closure to the store before a
  -- dependency-aware build.  A no-op in pure evaluators.
  recordDrvAterm :: Text -> ByteString -> m ()

  -- | Read and parse a derivation from its @.drv@ in the store, or 'Nothing'
  -- if it is absent or unreadable.  Used to resolve an input derivation's
  -- modulo hash on a cache miss - a cross-session reference, or a path
  -- fabricated by @builtins.appendContext@ - where the referenced derivation
  -- was not evaluated this session.  Reading the store is an effect, so this
  -- is an IO-evaluator capability: pure evaluators return 'Nothing', which
  -- makes hashing a dependent derivation something only the IO evaluator can do.
  readStoreDerivation :: StorePath -> m (Maybe Derivation)

  -- | Look up a derivation evaluated earlier THIS session by its @.drv@ store
  -- path, or 'Nothing' if it was not.  The IO evaluator reads the ATerm it
  -- recorded bottom-up ('recordDrvAterm'); pure evaluators return 'Nothing'.
  -- Used to recover a referenced derivation's output names for an all-outputs
  -- (DrvDeep) context reference without a disk read - the in-session @.drv@ is
  -- not on disk during evaluation.
  lookupSessionDrv :: Text -> m (Maybe Derivation)

  -- | The references recorded when this session wrote a store path (a
  -- @builtins.toFile@'s text with context, a copied source's none), or
  -- 'Nothing' if this session did not write it.  The walk of a
  -- @drvPath@'s closure follows them as upstream's @computeFSClosure@
  -- follows its database's.  Pure evaluators write nothing.
  lookupSessionReferences :: StorePath -> m (Maybe [StorePath])

  -- | Compute the store path a source file/directory gets when copied into
  -- the store (recursive NAR sha256 to a @source@ fixed-output path), WITHOUT
  -- performing the copy.  Used when a path literal is coerced in a derivation
  -- argument or environment value.  Unavailable in pure evaluation.
  storeSourcePath :: Text -> m Text

  -- | The policy this evaluation runs under.  The builtins whose behaviour
  -- upstream changes by setting consult it: @storePath@ is refused and the
  -- fetchers demand a hash or revision under @pure-eval@, a search path
  -- miss is worded for the mode, and @traceVerbose@ traces only under
  -- @trace-verbose@.  The filesystem and URI gates are not decided here;
  -- they live in the instance, where the allowed set is.
  evalPolicy :: m EvalPolicy

  -- | Upstream @checkURI@: refuse a fetch of this URI under @restrict-eval@
  -- unless @allowed-uris@ admits it (a path or @file://@ URI is checked
  -- against the allowed paths instead).  A no-op under any other policy.
  -- Every fetcher calls it before anything is spawned or downloaded.
  checkUri :: Text -> m ()

  -- | A path named on the drive this evaluation reads it on
  -- ('Nix.Store.Path.qualifyPathText' against its store directory): on
  -- Windows a rooted path with no drive gains the store's.  For a
  -- fetcher handing a local path to another program, which would
  -- otherwise read it on whatever drive that program has current.
  qualifyPath :: Text -> m Text

-- | Unwrap a store-path construction result (the @makeStorePath@ family
-- in "Nix.Hash"), raising a refused name as upstream's @BadStorePath@
-- text.  The message names the path and not the builtin, as upstream's
-- does: upstream names the builtin in its trace.  Every eval-side path
-- construction goes through this, so a refused name is an eval error
-- before anything is written.
storePathOrThrow :: (MonadEval m) => Either BadStorePath StorePath -> m StorePath
storePathOrThrow = either (throwEvalError . badStorePathText) pure

-- | Error raised during pure evaluation.  'PThrow' (@builtins.throw@, a
-- failed @assert@) is the only kind @tryEval@ catches; 'PError' (type
-- errors, missing attributes) and 'PAbort' (@abort@, infinite recursion)
-- escape it, exactly as in C++ Nix.
data PureError = PThrow !Text | PError !Text | PAbort !Text

-- | Pure evaluation monad: given the call depth, a t'CData' action
-- answering @Either PureError@, so its only effect is the C data layer.
-- IO builtins ('readFile', 'import') are unavailable, and
-- everything else evaluates identically to the IO version, the
-- @max-call-depth@ ceiling included, at its upstream default.  It carries
-- no filesystem policy: there is no filesystem, environment or network
-- behind it to gate, so it reports 'unrestrictedPolicy' and every URI
-- check passes.  It never writes a cell it did not allocate: a force
-- reads a thunk and evaluates it without memoizing, so it marks no
-- blackhole either.
newtype PureEval a = PureEval (CallDepth -> CData (Either PureError a))

instance Functor PureEval where
  fmap f (PureEval run) = PureEval (fmap (fmap f) . run)

instance Applicative PureEval where
  pure a = PureEval (\_ -> pure (Right a))
  PureEval runF <*> PureEval runA = PureEval $ \depth ->
    runF depth >>= either (pure . Left) (\f -> fmap f <$> runA depth)

instance Monad PureEval where
  PureEval run >>= k = PureEval $ \depth ->
    run depth >>= either (pure . Left) (\a -> runPure (k a) depth)

instance MonadCData PureEval where
  liftCData act = PureEval (\_ -> Right <$> act)

-- | A pure evaluation run at a call depth.
runPure :: PureEval a -> CallDepth -> CData (Either PureError a)
runPure (PureEval run) = run

-- | Fail a pure evaluation.
pureFailure :: PureError -> PureEval a
pureFailure failure = PureEval (\_ -> pure (Left failure))

-- | Run a pure evaluation, flattening the internal error into 'Text': a
-- failure's message, as t'Nix.Eval.IO.EvalIO' reports it.  The run is a
-- value ('runCDataPure'): the evaluation writes only cells it allocates
-- and the with-scope entries it resolves ("Nix.Eval.CData" says what two
-- runs resolving one entry can show).
runPureEval :: PureEval a -> Either Text a
runPureEval action =
  case runCDataPure (runPure action (topLevelCallDepth defaultMaxCallDepth)) of
    Right a -> Right a
    Left (PThrow t) -> Left t
    Left (PError t) -> Left t
    Left (PAbort t) -> Left t

instance MonadEval PureEval where
  throwEvalError = pureFailure . PError
  throwCatchableError = pureFailure . PThrow
  abortEvaluation = pureFailure . PAbort

  -- Catch only a throw/assert (tryEval sees the error); eval errors,
  -- aborts, and infinite recursion propagate, matching EvalIO and C++ Nix.
  catchEvalError (PureEval action) = PureEval (fmap caught . action)
    where
      caught (Left (PThrow t)) = Right (Left t)
      caught (Left other) = Left other
      caught (Right a) = Right (Right a)

  -- Pure evaluation has no external state, so there is nothing to
  -- clean up; the failure passes through unchanged.
  onEvalError (PureEval action) _ = PureEval action

  -- 'runPureEval' reports a failure's message alone, so a line of context
  -- has nowhere to go.  The line is still made, since a failure making it
  -- replaces the action's.
  addErrorTrace (PureEval line) (PureEval action) =
    PureEval $ \depth ->
      action depth >>= \case
        Left failure -> (>> Left failure) <$> line depth
        Right a -> pure (Right a)

  -- The reader scopes the frame: the action runs one deeper, and whatever
  -- follows it sees the depth it started from, failure or not.
  withCallFrame (PureEval action) =
    PureEval $ \depth -> either (pure . Left . PError) action (enterCallFrame depth)
  doesPathExist _ _ = pure False
  listDirectory _ = throwEvalError "builtins.readDir: not available in pure evaluation"
  importFile _ = throwEvalError "import: not available in pure evaluation"
  getEnvVar _ = pure ""
  getCurrentTime = pure 0
  writeToStore _ _ _ = throwEvalError "toFile: not available in pure evaluation"
  scopedImportFile _ _ = throwEvalError "scopedImport: not available in pure evaluation"
  readFileBytes _ = throwEvalError "readFile: not available in pure evaluation"
  getFileType _ = throwEvalError "readFileType: not available in pure evaluation"

  -- No filesystem, so no symlinks: a path resolves to itself.
  resolveSymlinks = pure
  runProcess _ _ _ = throwEvalError "runProcess: not available in pure evaluation"
  createScratchDir _ = throwEvalError "createScratchDir: not available in pure evaluation"
  removeScratchDir _ = pure ()
  copyPathToStore _ _ _ = throwEvalError "builtins.path: not available in pure evaluation"
  narHashOfPath _ = throwEvalError "builtins.fetchGit: not available in pure evaluation"
  isExecutableFile _ = throwEvalError "builtins.path: not available in pure evaluation"
  setExecutableFile _ = throwEvalError "builtins.fetchGit: not available in pure evaluation"
  lookupFetchCache _ = pure Nothing
  adoptStorePath _ = pure False
  writeFetchCache _ _ = pure ()
  readSymlinkTarget _ = throwEvalError "builtins.path: not available in pure evaluation"
  addSourceNar _ _ = throwEvalError "builtins.path: not available in pure evaluation"
  addFixedOutputFile _ _ = throwEvalError "builtins.fetchurl: not available in pure evaluation"
  traceMessage _ = pure ()
  lookupDrvHash _ = pure Nothing
  cacheDrvHash _ _ = pure ()
  recordDrvAterm _ _ = pure ()

  -- Pure eval cannot read the store, so a dependent derivation's input hash
  -- is never recoverable here; the caller turns this into a loud error rather
  -- than a guessed hash.
  readStoreDerivation _ = pure Nothing

  -- Pure eval keeps no session drv closure, so an all-outputs reference's
  -- output names are never recoverable here either.
  lookupSessionDrv _ = pure Nothing
  lookupSessionReferences _ = pure Nothing

  -- Pure eval cannot read files: a path coerces to itself (no store copy);
  -- the real copy-to-store happens only under 'EvalIO'.
  storeSourcePath = pure
  evalPolicy = pure unrestrictedPolicy
  checkUri _ = pure ()

  -- No filesystem, so no drive to read on: a path names itself.
  qualifyPath = pure
  resolvePathLiteral = pure . canonPathValue

  -- Reads the cell and never writes it: no memoization, and no
  -- blackhole, so PureEval may force the same thunk again.
  forceThunk evalFn (Thunk ptr) =
    cthunkState ptr >>= \case
      ThunkComputed -> readComputed ptr
      -- Non-catchable: must escape tryEval like in C++ Nix
      ThunkBlackhole -> abortEvaluation "infinite recursion encountered"
      _ {- PENDING -} -> pendingCode ptr >>= uncurry evalFn
