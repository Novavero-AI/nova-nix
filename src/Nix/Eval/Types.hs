{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Shared types for the Nix evaluator.
--
-- Extracted into its own module so that 'Nix.Eval.Operator',
-- 'Nix.Eval.StringInterp', and 'Nix.Builtins' can all reference
-- 'NixValue', 'Thunk', and 'Env' without creating import cycles.
module Nix.Eval.Types
  ( -- * Values
    NixValue (..),
    CompiledRegex (..),
    Thunk (..),
    readThunkValue,

    -- * Attribute sets (C-backed sorted arrays)
    AttrSet (..),
    CAttrSet,
    attrSetLookup,
    attrSetKeys,
    attrSetToMap,
    attrSetFromMap,
    attrSetMember,
    attrSetNull,
    attrSetSize,
    attrSetElems,
    attrSetToAscList,
    attrSetMapWithKey,
    attrSetRemoveKeys,
    attrSetUnionWith,
    buildCAttrSetKeys,
    fillCAttrSetValues,

    -- * String context
    StringContextElement (..),
    StringContext (..),
    emptyContext,
    mkStr,
    marshalStringContext,
    unmarshalStringContext,

    -- * Environment
    Env (..),
    NnEnv,
    emptyEnv,
    envLookup,
    envLookupResolved,
    envFromSlots,
    pushWithScope,
    lookupWithScopes,
    withScopesForCapture,
    envWithScopesRaw,
    newCEnv,
    newMinimalEnv,

    -- * Eval-time formals (re-exported from EvalFormals)
    EvalFormals (..),
    EvalFormal (..),

    -- * Thunk operations
    mkThunk,
    mkSyntheticThunk,
    cheapThunk,
    cheapThunkBc,
    mkThunkBc,
    evaluated,
    thunkSameRef,
    thunkToCPtr,
    buildCSlots,
    allocCSlots,
    fillCSlots,

    -- * Display
    typeName,

    -- * Evaluation monad
    MonadEval (..),
    PureEval (..),
  )
where

import Control.Monad (forM_)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, newStablePtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Float (castWord64ToDouble)
import Nix.Derivation (Derivation)
import Nix.Eval.CAttrSet (CAttrSet, cattrsetFreeze, cattrsetGetKey, cattrsetGetValue, cattrsetIndex, cattrsetInsert, cattrsetKeys, cattrsetLookup, cattrsetNew, cattrsetRemoveKeys, cattrsetSetValue, cattrsetSize)
import Nix.Eval.CBytecode (cbcArg1, cbcArg2, cbcOpcode, cbcShortArg)
import Nix.Eval.CCtxStr (CCtxStrPtr, cctxstrCtxCount, cctxstrElemHash, cctxstrElemName, cctxstrElemOutput, cctxstrElemTag, cctxstrNew, cctxstrSetAllOutputs, cctxstrSetDrvOutput, cctxstrSetPlain, cctxstrText)
import Nix.Eval.CEnv (NnEnv, cenvAllocSlots, cenvAllocWithScopes, cenvEmpty, cenvFromSlots, cenvLazyScope, cenvLookupResolved, cenvNew, cenvNewMinimal, cenvParent, cenvPushWith, cenvRootScope, cenvSlotCount, cenvWithCount, cenvWithScopes)
import Nix.Eval.CList (clistToThunkList, thunkListToCList)
import Nix.Eval.CThunk (CThunkPtr, cthunkGetAttrs, cthunkGetBcIdx, cthunkGetBool, cthunkGetCtxStr, cthunkGetFloat, cthunkGetInt, cthunkGetList, cthunkGetPath, cthunkGetStr, cthunkNewBc, cthunkNewComputed, cthunkNewComputedAttrs, cthunkNewComputedBool, cthunkNewComputedCtxStr, cthunkNewComputedFloat, cthunkNewComputedInt, cthunkNewComputedList, cthunkNewComputedNull, cthunkNewComputedPath, cthunkNewComputedStr, cthunkPayload, cthunkState, cthunkValueTag)
import Nix.Eval.Compile (compileExpr, compileFormalsToEval)
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..))
import Nix.Eval.Symbol (Symbol (..), symbolIntern, symbolText)
import Nix.Expr.Types (CaptureInfo (..), Expr (..), NixAtom (..))
import Nix.Store.Path (StorePath (..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Regex.TDFA as RE

-- ---------------------------------------------------------------------------
-- Compiled regex
-- ---------------------------------------------------------------------------

-- | Compiled regex with Eq/Show based on source pattern text.
-- Carries the pre-compiled 'RE.Regex' alongside the original pattern
-- so that partial application of @builtins.match@ / @builtins.split@
-- avoids recompiling the same pattern on every invocation.
data CompiledRegex = CompiledRegex !Text RE.Regex

instance Eq CompiledRegex where
  CompiledRegex a _ == CompiledRegex b _ = a == b

instance Show CompiledRegex where
  show (CompiledRegex pat _) = "CompiledRegex " <> show pat

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

-- | Smart constructor for context-free strings.
mkStr :: Text -> NixValue
mkStr t = VStr t emptyContext

-- ---------------------------------------------------------------------------
-- Thunks
-- ---------------------------------------------------------------------------

-- | A thunk: a C arena-allocated memoization cell.
--
-- Each thunk points to an @nn_thunk_t@ in the C arena.
-- On first force, the cell transitions PENDING -> BLACKHOLE -> COMPUTED,
-- which detects infinite recursion (BLACKHOLE) and drops the Expr/Env
-- references — matching real Nix which mutates thunks in-place.
--
-- The C thunk is allocated via 'unsafePerformIO' in 'mkThunk' (same
-- pattern as the former IORef-based approach) so that knot-tying works
-- unchanged.  Arena pointers remain valid until 'cthunkDestroy'.
newtype Thunk = Thunk {unThunk :: CThunkPtr}

instance Show Thunk where
  show (Thunk ptr) =
    case unsafePerformIO (cthunkState ptr) of
      1 {- COMPUTED -} -> case readThunkValue (Thunk ptr) of
        Just val -> "Thunk (" ++ show val ++ ")"
        Nothing -> "Thunk <computed?>"
      0 -> "Thunk <pending>"
      2 -> "Thunk <blackhole>"
      _ -> "Thunk <unknown>"

-- | Equality: pointer identity fast path, then value comparison for
-- COMPUTED thunks.  Only used in tests on non-recursive structures.
instance Eq Thunk where
  (Thunk p1) == (Thunk p2)
    | p1 == p2 = True
    | otherwise = case (readThunkValue (Thunk p1), readThunkValue (Thunk p2)) of
        (Just v1, Just v2) -> v1 == v2
        _ -> False

-- | Read a COMPUTED thunk's value without forcing.
-- Returns 'Nothing' for PENDING or BLACKHOLE thunks.
-- Uses 'unsafePerformIO' — safe because C reads are idempotent.
readThunkValue :: Thunk -> Maybe NixValue
readThunkValue (Thunk ptr) =
  unsafePerformIO $ do
    state <- cthunkState ptr
    if state /= 1
      then pure Nothing
      else do
        tag <- cthunkValueTag ptr
        fmap Just $ case tag of
          0 {- INT -} -> VInt . fromIntegral <$> cthunkGetInt ptr
          1 {- FLOAT -} -> VFloat <$> cthunkGetFloat ptr
          2 {- BOOL -} -> (\b -> VBool (b /= 0)) <$> cthunkGetBool ptr
          3 {- NULL -} -> pure VNull
          4 {- STR -} -> (\sym -> VStr (symbolText (Symbol sym)) emptyContext) <$> cthunkGetStr ptr
          5 {- PATH -} -> VPath . symbolText . Symbol <$> cthunkGetPath ptr
          6 {- LIST -} -> do
            listPtr <- cthunkGetList ptr
            thunks <- clistToThunkList (castPtr listPtr)
            pure (VList (map Thunk thunks))
          7 {- ATTRS -} -> VAttrs . AttrSet . castPtr <$> cthunkGetAttrs ptr
          8 {- CTXSTR -} -> do
            csptr <- cthunkGetCtxStr ptr
            uncurry VStr <$> unmarshalStringContext (castPtr csptr)
          _ {- PTR -} -> do
            payload <- cthunkPayload ptr
            deRefStablePtr (castPtrToStablePtr payload)

-- | A Nix value — the result of evaluating an expression.
data NixValue
  = -- | Integer.
    VInt !Integer
  | -- | Floating-point.
    VFloat !Double
  | -- | Boolean.
    VBool !Bool
  | -- | The null value.
    VNull
  | -- | String with dependency context.
    VStr !Text !StringContext
  | -- | Path.
    VPath !Text
  | -- | List of thunks (lazy elements).
    VList ![Thunk]
  | -- | Attribute set: unified lazy/eager representation.
    VAttrs !AttrSet
  | -- | Lambda closure: captures environment, formals, body bytecode index.
    VLambda !Env !EvalFormals !Word32
  | -- | A realized derivation (build recipe).
    VDerivation !Derivation
  | -- | Built-in function, dispatched by name.
    -- Accumulated args support curried partial application.
    VBuiltin !Text ![NixValue]
  | -- | Pre-compiled regex carried in a partially-applied builtin.
    -- Internal only — never exposed to Nix code directly.
    VCompiledRegex !CompiledRegex
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Attribute sets (C-backed sorted arrays)
-- ---------------------------------------------------------------------------

-- | Attribute set backed by a C-allocated sorted key-value array.
--
-- All keys are interned symbols; values are CThunkPtrs in a parallel
-- array.  O(log n) binary search on contiguous memory for lookup.
-- Replaces the former EagerAttrs\/LazyAttrs\/MappedAttrs\/CAttrs ADT
-- with a single C-backed representation — all attr set data lives off
-- the GHC heap, dramatically reducing GC pressure for large evaluations.
newtype AttrSet = AttrSet {unAttrSet :: CAttrSet}

instance Eq AttrSet where
  a == b = attrSetToMap a == attrSetToMap b

instance Show AttrSet where
  show (AttrSet cset) =
    let n = unsafePerformIO (cattrsetSize cset)
     in "<attrset " ++ show n ++ " entries>"

-- | Look up a single key via symbol interning + C binary search.
-- Uses 'unsafePerformIO' — safe because symbolIntern and cattrsetLookup
-- are idempotent (same key always yields same symbol and same result).
attrSetLookup :: Text -> AttrSet -> Maybe Thunk
attrSetLookup key (AttrSet cset) =
  unsafePerformIO $ do
    sym <- symbolIntern key
    mptr <- cattrsetLookup cset sym
    pure (fmap Thunk mptr)

-- | All keys in sorted order (symbol text from C).
attrSetKeys :: AttrSet -> [Text]
attrSetKeys (AttrSet cset) =
  map symbolText (unsafePerformIO (cattrsetKeys cset))

-- | Check key membership without materializing thunks.
attrSetMember :: Text -> AttrSet -> Bool
attrSetMember key (AttrSet cset) =
  unsafePerformIO $ do
    sym <- symbolIntern key
    midx <- cattrsetIndex cset sym
    pure (case midx of Nothing -> False; Just _ -> True)

-- | Check if the attribute set is empty.
attrSetNull :: AttrSet -> Bool
attrSetNull (AttrSet cset) =
  unsafePerformIO (cattrsetSize cset) == 0

-- | Number of attributes.
attrSetSize :: AttrSet -> Int
attrSetSize (AttrSet cset) =
  fromIntegral (unsafePerformIO (cattrsetSize cset))

-- | Full materialization: build a 'Map Text Thunk' from all entries.
-- Iterates the C array and builds a Haskell Map.  Expensive on large
-- sets — avoid on the hot path.
attrSetToMap :: AttrSet -> Map Text Thunk
attrSetToMap (AttrSet cset) =
  unsafePerformIO $ do
    n <- cattrsetSize cset
    if n == 0
      then pure Map.empty
      else do
        pairs <- mapM materializeEntry [0 .. n - 1]
        pure (Map.fromList pairs)
  where
    materializeEntry i = do
      sym <- cattrsetGetKey cset i
      ptr <- cattrsetGetValue cset i
      pure (symbolText sym, Thunk ptr)

-- | All thunk values (materialized).
attrSetElems :: AttrSet -> [Thunk]
attrSetElems = Map.elems . attrSetToMap

-- | Sorted key-value pairs (materialized).
attrSetToAscList :: AttrSet -> [(Text, Thunk)]
attrSetToAscList = Map.toAscList . attrSetToMap

-- | Map a function over all key-value pairs, building a new CAttrSet.
-- Materializes the source set, applies @f@ to each entry, and rebuilds.
attrSetMapWithKey :: (Text -> Thunk -> Thunk) -> AttrSet -> AttrSet
attrSetMapWithKey f attrs = attrSetFromMap (Map.mapWithKey f (attrSetToMap attrs))

-- | Remove a list of keys, returning a new CAttrSet.
-- Uses the C-side @nn_attrset_remove_keys@ for O(n) removal.
{-# NOINLINE attrSetRemoveKeys #-}
attrSetRemoveKeys :: [Text] -> AttrSet -> AttrSet
attrSetRemoveKeys keys (AttrSet cset) = unsafePerformIO $ do
  syms <- mapM symbolIntern keys
  newSet <- cattrsetRemoveKeys cset syms
  pure (AttrSet newSet)

-- | Union two attribute sets with a combining function.
-- Materializes both to Maps, applies the combining function, rebuilds.
attrSetUnionWith :: (Thunk -> Thunk -> Thunk) -> AttrSet -> AttrSet -> AttrSet
attrSetUnionWith f a b = attrSetFromMap (Map.unionWith f (attrSetToMap a) (attrSetToMap b))

-- | Build a CAttrSet from a Haskell 'Map'.  Interns all keys as symbols,
-- inserts key-value pairs, freezes (sort + dedup).  The canonical entry
-- point for all attribute set construction.
--
-- Uses 'unsafePerformIO' with @NOINLINE@ — safe because C allocation
-- is idempotent and the resulting CAttrSet is referentially transparent.
{-# NOINLINE attrSetFromMap #-}
attrSetFromMap :: Map Text Thunk -> AttrSet
attrSetFromMap m = m `seq` unsafePerformIO $ do
  let pairs = Map.toList m
      n = fromIntegral (length pairs)
  cset <- cattrsetNew n
  forM_ pairs $ \(key, Thunk ptr) -> do
    sym <- symbolIntern key
    cattrsetInsert cset sym ptr
  cattrsetFreeze cset
  pure (AttrSet cset)

-- | Allocate a CAttrSet skeleton with keys only (NULL values).
-- Used for two-phase construction in @rec {}@ and @let@ (knot-tying):
-- keys are known before thunks, so the CAttrSet is built first, then
-- values are filled via 'fillCAttrSetValues'.
{-# NOINLINE buildCAttrSetKeys #-}
buildCAttrSetKeys :: [Text] -> CAttrSet
buildCAttrSetKeys keys = unsafePerformIO $ do
  let n = fromIntegral (length keys)
  cset <- cattrsetNew n
  forM_ keys $ \key -> do
    sym <- symbolIntern key
    cattrsetInsert cset sym nullPtr
  cattrsetFreeze cset
  pure cset

-- | Fill values into a pre-allocated CAttrSet (two-phase construction).
-- Looks up each key's index and writes the CThunkPtr at that position.
-- Keys not found in the CAttrSet are silently ignored (should not happen
-- in correct knot-tying code).
{-# NOINLINE fillCAttrSetValues #-}
fillCAttrSetValues :: CAttrSet -> Map Text Thunk -> ()
fillCAttrSetValues cset thunkMap = unsafePerformIO $
  forM_ (Map.toList thunkMap) $ \(key, Thunk ptr) -> do
    sym <- symbolIntern key
    midx <- cattrsetIndex cset sym
    case midx of
      Just idx -> cattrsetSetValue cset idx ptr
      Nothing -> pure ()

-- | Evaluation environment — C-backed scope chain.
--
-- Arena-allocated @nn_env_t@ struct (40 bytes, zero GC overhead).
-- All env data (slots, lazy scope, parent, with-scopes) lives in C.
-- Haskell holds only the pointer.  Variable lookup has two paths:
--
-- * 'envLookupResolved': single C call for 'EResolvedVar'
-- * 'envLookup': name-based walk for 'EVar' (lazy scope + with-scopes)
newtype Env = Env (Ptr NnEnv)

-- | Pointer equality: two envs are equal iff they are the same C struct.
instance Eq Env where
  Env p1 == Env p2 = p1 == p2

-- | Compact show via C accessors.
instance Show Env where
  show (Env envPtr) =
    let sc = unsafePerformIO (cenvSlotCount envPtr)
        ls = unsafePerformIO (cenvLazyScope envPtr)
        par = unsafePerformIO (cenvParent envPtr)
        wc = unsafePerformIO (cenvWithCount envPtr)
     in "Env{"
          ++ show sc
          ++ " slots"
          ++ (if ls /= nullPtr then ", lazyScope" else "")
          ++ (if par /= nullPtr then ", parent" else "")
          ++ ", "
          ++ show wc
          ++ " withs}"

-- | Empty environment (no variables in scope).
-- Points to a static global C struct — valid until 'arenaDestroy'.
{-# NOINLINE emptyEnv #-}
emptyEnv :: Env
emptyEnv = Env (unsafePerformIO cenvEmpty)

-- | General C-backed env constructor.
-- Takes Haskell-level types; converts Maybe to nullPtr internally.
{-# NOINLINE newCEnv #-}
newCEnv :: Ptr CThunkPtr -> Int -> Maybe AttrSet -> Maybe Env -> Ptr (Ptr ()) -> Word16 -> Env
newCEnv slots slotCount lazyScope parent withs withCount =
  Env
    ( unsafePerformIO
        ( cenvNew
            slots
            (fromIntegral slotCount)
            (case lazyScope of Nothing -> nullPtr; Just (AttrSet cset) -> castPtr cset)
            (case parent of Nothing -> nullPtr; Just (Env p) -> p)
            withs
            withCount
        )
    )

-- | Minimal env: slots only, no parent, no with-scopes, no lazy scope.
{-# NOINLINE newMinimalEnv #-}
newMinimalEnv :: Ptr CThunkPtr -> Int -> Env
newMinimalEnv slots n =
  Env (unsafePerformIO (cenvNewMinimal slots (fromIntegral n)))

-- | Look up a resolved variable by level and index.  Single C call:
-- O(level) parent hops in C, then O(1) array read.
envLookupResolved :: Int -> Int -> Env -> Thunk
envLookupResolved level idx (Env envPtr) =
  Thunk (unsafePerformIO (cenvLookupResolved envPtr level idx))

-- | Name-based variable lookup: walk the parent chain checking
-- lazy scopes; fall back to with-scopes (from the starting env).
--
-- Positional slots are NOT searched here — used only for
-- 'EVar' lookups (let\/rec bindings, builtins, with-scopes).
envLookup :: Text -> Env -> Maybe Thunk
envLookup name (Env envPtr) = lexicalLookup envPtr
  where
    -- With-scopes from the STARTING env (C array)
    startWiths = unsafePerformIO (cenvWithScopes envPtr)
    startWithCount = unsafePerformIO (cenvWithCount envPtr)
    lexicalLookup ep =
      let ls = unsafePerformIO (cenvLazyScope ep)
          scopeResult =
            if ls /= nullPtr
              then attrSetLookup name (AttrSet (castPtr ls))
              else Nothing
       in case scopeResult of
            Just val -> Just val
            Nothing ->
              let par = unsafePerformIO (cenvParent ep)
               in if par /= nullPtr
                    then lexicalLookup par
                    else lookupWithScopesC name startWiths startWithCount

-- | Walk with-scopes (C array) innermost to outermost.
lookupWithScopesC :: Text -> Ptr (Ptr ()) -> Word16 -> Maybe Thunk
lookupWithScopesC _ _ 0 = Nothing
lookupWithScopesC name withArr count = unsafePerformIO $ go 0
  where
    go i
      | i >= fromIntegral count = pure Nothing
      | otherwise = do
          scopePtr <- peekElemOff withArr i
          case attrSetLookup name (AttrSet (castPtr scopePtr)) of
            Just val -> pure (Just val)
            Nothing -> go (i + 1)

-- | Walk with-scopes as a Haskell list (backward-compatible signature).
lookupWithScopes :: Text -> [AttrSet] -> Maybe Thunk
lookupWithScopes _ [] = Nothing
lookupWithScopes name (scope : rest) =
  case attrSetLookup name scope of
    Just val -> Just val
    Nothing -> lookupWithScopes name rest

-- | Create a child env with positional slots (for lambda formals).
-- Inherits with-scopes from the parent.  Arena-allocated.
{-# NOINLINE envFromSlots #-}
envFromSlots :: Ptr CThunkPtr -> Int -> Env -> Env
envFromSlots slotsPtr slotCount (Env parentPtr) =
  Env (unsafePerformIO (cenvFromSlots slotsPtr (fromIntegral slotCount) parentPtr))

-- | Push a with-scope onto the scope chain (innermost position).
-- Allocates a new C env struct with extended with-scopes array.
{-# NOINLINE pushWithScope #-}
pushWithScope :: AttrSet -> Env -> Env
pushWithScope (AttrSet cset) (Env envPtr) =
  Env (unsafePerformIO (cenvPushWith envPtr (castPtr cset)))

-- | Read with-scopes array pointer and count from a C env.
{-# NOINLINE envWithScopesRaw #-}
envWithScopesRaw :: Env -> (Ptr (Ptr ()), Word16)
envWithScopesRaw (Env envPtr) = unsafePerformIO $ do
  withs <- cenvWithScopes envPtr
  count <- cenvWithCount envPtr
  pure (withs, count)

-- | Build with-scopes for a trimmed env that needs with-scope access
-- ('CapturesWithScopes').  Appends the root scope (builtins) as the
-- outermost entry so 'EWithVar' can fall back to builtins without
-- retaining the parent chain.  Returns C array + count.
{-# NOINLINE withScopesForCapture #-}
withScopesForCapture :: Env -> (Ptr (Ptr ()), Word16)
withScopesForCapture (Env envPtr) = unsafePerformIO $ do
  rootPtr <- cenvRootScope envPtr
  existingWiths <- cenvWithScopes envPtr
  existingCount <- cenvWithCount envPtr
  if rootPtr == nullPtr
    then pure (existingWiths, existingCount)
    else do
      let newCount = existingCount + 1
      arr <- cenvAllocWithScopes newCount
      -- Copy existing with-scopes
      forM_ [0 .. fromIntegral existingCount - 1] $ \i -> do
        val <- peekElemOff existingWiths i
        pokeElemOff arr i val
      -- Append root scope at end (outermost)
      pokeElemOff arr (fromIntegral existingCount) rootPtr
      pure (arr, newCount)

-- | Create an unevaluated thunk with a fresh C arena-allocated cell.
--
-- Compiles the Expr to bytecode and stores (bc_idx, env_ptr) in the
-- C thunk — no StablePtr, zero GHC heap pressure for pending thunks.
-- The cell is allocated via 'unsafePerformIO' — safe because C
-- allocation is a pure side effect, and the @NOINLINE@ + @seq@
-- pattern prevents GHC from floating the allocation to a shared
-- top-level CAF.
mkThunk :: Env -> Expr -> Thunk
mkThunk env thunkExpr =
  Thunk (newBcThunkPtr thunkExpr env)

-- | Like 'mkThunk' but for synthetic thunks that reuse the same 'Expr'
-- (e.g. @EApp (EResolvedVar 0 0) (EResolvedVar 0 1)@ in 'deferApply').
-- Uses the env pointer for cell uniqueness instead of the expression,
-- since GHC's full-laziness transform would otherwise float the shared
-- expr to a CAF and all thunks would get the same cell.
--
-- Must only be called with freshly-constructed envs (not knot-tied
-- recursive envs), since it forces the env pointer to WHNF.
mkSyntheticThunk :: Env -> Expr -> Thunk
mkSyntheticThunk env@(Env envPtr) thunkExpr =
  Thunk (newSyntheticBcThunkPtr envPtr thunkExpr env)

-- | Like 'mkThunk' but avoids C arena allocation for trivial expressions.
-- Resolved variables reuse the existing thunk from the env (no wrapper).
-- Literals use inline C scalars (no StablePtr).
-- Lambdas compile formals+body to bytecode and produce VLambda directly.
-- Everything else falls back to 'mkThunk'.
cheapThunk :: Env -> Expr -> Thunk
cheapThunk env (EResolvedVar level idx) = envLookupResolved level idx env
cheapThunk _ (ELit (NixInt n)) = Thunk (newComputedIntPtr (fromIntegral n))
cheapThunk _ (ELit (NixFloat n)) = Thunk (newComputedFloatPtr n)
cheapThunk _ (ELit (NixBool b)) = Thunk (newComputedBoolPtr (if b then 1 else 0))
cheapThunk _ (ELit NixNull) = Thunk newComputedNullPtr
cheapThunk env (ELambda formals body NoCaptureInfo) =
  let bodyBcIdx = unsafePerformIO (compileExpr body)
      evalFormals = unsafePerformIO (compileFormalsToEval formals)
   in evaluated (VLambda env evalFormals bodyBcIdx)
cheapThunk env (ELambda formals body (Captures captureList)) =
  let (slotsPtr, slotCount) = buildCSlots [envLookupResolved lvl idx env | (lvl, idx) <- captureList]
      trimmedEnv = newMinimalEnv slotsPtr slotCount
      bodyBcIdx = unsafePerformIO (compileExpr body)
      evalFormals = unsafePerformIO (compileFormalsToEval formals)
   in evaluated (VLambda trimmedEnv evalFormals bodyBcIdx)
cheapThunk env (ELambda formals body (CapturesWithScopes captureList)) =
  let (slotsPtr, slotCount) = buildCSlots [envLookupResolved lvl idx env | (lvl, idx) <- captureList]
      (withArr, withCount) = withScopesForCapture env
      trimmedEnv = newCEnv slotsPtr slotCount Nothing Nothing withArr withCount
      bodyBcIdx = unsafePerformIO (compileExpr body)
      evalFormals = unsafePerformIO (compileFormalsToEval formals)
   in evaluated (VLambda trimmedEnv evalFormals bodyBcIdx)
cheapThunk env expr = mkThunk env expr

-- | Create a pending thunk from a bytecode index (no compilation needed).
-- Used inside 'evalBytecode' where the bc_idx is already known.
-- The Env is captured LAZILY (for knot-tying in rec attrs / let / matchFormalSet).
mkThunkBc :: Env -> Word32 -> Thunk
mkThunkBc env bcIdx =
  Thunk (newBcThunkPtrLazy bcIdx env)

-- | Like 'cheapThunk' but for bytecode indices (used inside evalBytecode).
-- Short-circuits for resolved vars and literals without creating a
-- pending thunk.  Everything else falls back to 'mkThunkBc'.
cheapThunkBc :: Env -> Word32 -> Thunk
cheapThunkBc env bcIdx =
  let opcode = unsafePerformIO (cbcOpcode bcIdx)
   in case opcode of
        10 {- RESOLVED_VAR -} ->
          let level = fromIntegral (unsafePerformIO (cbcArg1 bcIdx))
              idx = fromIntegral (unsafePerformIO (cbcArg2 bcIdx))
           in envLookupResolved level idx env
        0 {- LIT_INT -} ->
          let lo = unsafePerformIO (cbcArg1 bcIdx)
              hi = unsafePerformIO (cbcArg2 bcIdx)
              w64 = fromIntegral lo .|. (fromIntegral hi `shiftL` 32) :: Word64
           in Thunk (newComputedIntPtr (fromIntegral (fromIntegral w64 :: Int64)))
        1 {- LIT_FLOAT -} ->
          let lo = unsafePerformIO (cbcArg1 bcIdx)
              hi = unsafePerformIO (cbcArg2 bcIdx)
              w64 = fromIntegral lo .|. (fromIntegral hi `shiftL` 32) :: Word64
           in Thunk (newComputedFloatPtr (castWord64ToDouble w64))
        2 {- LIT_BOOL -} ->
          let flag = unsafePerformIO (cbcShortArg bcIdx)
           in Thunk (newComputedBoolPtr (if flag /= 0 then 1 else 0))
        3 {- LIT_NULL -} -> Thunk newComputedNullPtr
        _ -> mkThunkBc env bcIdx

-- | Allocate a fresh C arena thunk cell with bytecode.
--
-- @NOINLINE@ prevents inlining so GHC can't see inside or CSE calls.
-- @seq@ on the expr creates a data dependency that prevents float-out
-- to a top-level CAF — without this, GHC would hoist the
-- @unsafePerformIO@ and share ONE cell across ALL thunks.
--
-- Compiles the Expr to bytecode and stores (bc_idx, StablePtr Env)
-- in the C thunk.  The Expr tree is eliminated from the GHC heap —
-- only the StablePtr Env (~16 bytes) remains for knot-tying laziness.
{-# NOINLINE newBcThunkPtr #-}
newBcThunkPtr :: Expr -> Env -> CThunkPtr
newBcThunkPtr expr env =
  unsafePerformIO $ expr `seq` do
    bcIdx <- compileExpr expr
    sp <- newStablePtr env
    cthunkNewBc bcIdx (castStablePtrToPtr sp)

-- | Like 'newBcThunkPtr' but keyed on the env's C pointer instead
-- of the expression.  Used by 'mkSyntheticThunk' where multiple thunks
-- share the same expression (e.g. 'deferApplyExpr').
{-# NOINLINE newSyntheticBcThunkPtr #-}
newSyntheticBcThunkPtr :: Ptr NnEnv -> Expr -> Env -> CThunkPtr
newSyntheticBcThunkPtr envKey expr env =
  unsafePerformIO $ envKey `seq` do
    bcIdx <- compileExpr expr
    sp <- newStablePtr env
    cthunkNewBc bcIdx (castStablePtrToPtr sp)

-- | Allocate a fresh C arena thunk cell from a known bytecode index.
-- No compilation needed — the bc_idx is already available.
--
-- Uses StablePtr for the Env so it stays lazy — essential for
-- knot-tying in recursive attrs, let bindings, and matchFormalSet.
-- The StablePtr points to an Env (newtype around Ptr NnEnv) — ~16
-- bytes on the GHC heap, negligible compared to the Expr trees we
-- eliminated.
{-# NOINLINE newBcThunkPtrLazy #-}
newBcThunkPtrLazy :: Word32 -> Env -> CThunkPtr
newBcThunkPtrLazy bcIdx env =
  unsafePerformIO $ bcIdx `seq` do
    sp <- newStablePtr env
    cthunkNewBc bcIdx (castStablePtrToPtr sp)

-- | Wrap an already-computed value as a thunk.
-- Scalars (int/float/bool/null) use inline C thunks (no StablePtr).
-- Attrs, paths, and context-free strings use C-native tags (no StablePtr).
-- Other complex values fall back to StablePtr-backed C thunks.
evaluated :: NixValue -> Thunk
evaluated (VInt n) = Thunk (newComputedIntPtr (fromIntegral n))
evaluated (VFloat d) = Thunk (newComputedFloatPtr d)
evaluated (VBool b) = Thunk (newComputedBoolPtr (if b then 1 else 0))
evaluated VNull = Thunk newComputedNullPtr
evaluated (VAttrs (AttrSet cset)) = Thunk (newComputedAttrsPtr cset)
evaluated (VPath p) = Thunk (newComputedPathPtr p)
evaluated (VStr t ctx)
  | ctx == emptyContext = Thunk (newComputedStrPtr t)
  | otherwise = Thunk (newComputedCtxStrPtr t ctx)
evaluated (VList thunks) = Thunk (newComputedListPtr thunks)
evaluated val = Thunk (newComputedThunkPtr val)

-- | Check if two thunks share the same memoization cell (C pointer
-- equality).  When true, both thunks will always produce the same value,
-- so deep equality can short-circuit to 'True' without forcing.
thunkSameRef :: Thunk -> Thunk -> Bool
thunkSameRef (Thunk p1) (Thunk p2) = p1 == p2

-- | Extract the C pointer from a thunk (zero-cost, newtype unwrap).
thunkToCPtr :: Thunk -> CThunkPtr
thunkToCPtr (Thunk ptr) = ptr

-- | Wrap an already-computed 'NixValue' in a C thunk (StablePtr path).
-- Arena-allocated (O(1)).  Used for complex values (string, list, attrs, etc.).
{-# NOINLINE newComputedThunkPtr #-}
newComputedThunkPtr :: NixValue -> CThunkPtr
newComputedThunkPtr val =
  unsafePerformIO $ val `seq` do
    sp <- newStablePtr val
    cthunkNewComputed (castStablePtrToPtr sp)

-- | Wrap an int64 in a pre-computed C thunk (inline, no StablePtr).
{-# NOINLINE newComputedIntPtr #-}
newComputedIntPtr :: Int64 -> CThunkPtr
newComputedIntPtr n = unsafePerformIO (cthunkNewComputedInt n)

-- | Wrap a double in a pre-computed C thunk (inline, no StablePtr).
{-# NOINLINE newComputedFloatPtr #-}
newComputedFloatPtr :: Double -> CThunkPtr
newComputedFloatPtr d = unsafePerformIO (cthunkNewComputedFloat d)

-- | Wrap a bool in a pre-computed C thunk (inline, no StablePtr).
{-# NOINLINE newComputedBoolPtr #-}
newComputedBoolPtr :: Word8 -> CThunkPtr
newComputedBoolPtr b = unsafePerformIO (cthunkNewComputedBool b)

-- | Wrap null in a pre-computed C thunk (inline, no StablePtr).
{-# NOINLINE newComputedNullPtr #-}
newComputedNullPtr :: CThunkPtr
newComputedNullPtr = unsafePerformIO cthunkNewComputedNull

-- | Wrap a context-free string in a pre-computed C thunk (interned symbol, no StablePtr).
{-# NOINLINE newComputedStrPtr #-}
newComputedStrPtr :: Text -> CThunkPtr
newComputedStrPtr t =
  unsafePerformIO $ t `seq` do
    Symbol sym <- symbolIntern t
    cthunkNewComputedStr sym

-- | Wrap a path in a pre-computed C thunk (interned symbol, no StablePtr).
{-# NOINLINE newComputedPathPtr #-}
newComputedPathPtr :: Text -> CThunkPtr
newComputedPathPtr p =
  unsafePerformIO $ p `seq` do
    Symbol sym <- symbolIntern p
    cthunkNewComputedPath sym

-- | Wrap a list of thunks in a pre-computed C thunk (C array, no StablePtr).
-- Converts [Thunk] to a C nn_list_t, stores pointer as payload.
{-# NOINLINE newComputedListPtr #-}
newComputedListPtr :: [Thunk] -> CThunkPtr
newComputedListPtr thunks =
  unsafePerformIO $
    thunks `seq` do
      let ptrs = map thunkToCPtr thunks
      clist <- thunkListToCList ptrs
      cthunkNewComputedList (castPtr clist)

-- | Wrap a string with context in a pre-computed C thunk (no StablePtr).
-- Interns the text and all StorePath fields as symbols, builds nn_ctxstr_t.
{-# NOINLINE newComputedCtxStrPtr #-}
newComputedCtxStrPtr :: Text -> StringContext -> CThunkPtr
newComputedCtxStrPtr t ctx =
  unsafePerformIO $
    t `seq`
      ctx `seq` do
        ptr <- marshalStringContext t ctx
        cthunkNewComputedCtxStr (castPtr ptr)

-- | Marshal Text + StringContext to a C nn_ctxstr_t.
marshalStringContext :: Text -> StringContext -> IO CCtxStrPtr
marshalStringContext textVal (StringContext ctxSet) = do
  Symbol textSym <- symbolIntern textVal
  let elems = Set.toAscList ctxSet
      count = fromIntegral (length elems) :: Word16
  ptr <- cctxstrNew textSym count
  fillElems ptr 0 elems
  pure ptr
  where
    fillElems _ _ [] = pure ()
    fillElems ptr !idx (e : es) = do
      marshalElem ptr idx e
      fillElems ptr (idx + 1) es
    marshalElem eptr eidx (SCPlain (StorePath h n)) = do
      Symbol hashSym <- symbolIntern h
      Symbol nameSym <- symbolIntern n
      cctxstrSetPlain eptr eidx hashSym nameSym
    marshalElem eptr eidx (SCDrvOutput (StorePath h n) out) = do
      Symbol hashSym <- symbolIntern h
      Symbol nameSym <- symbolIntern n
      Symbol outSym <- symbolIntern out
      cctxstrSetDrvOutput eptr eidx hashSym nameSym outSym
    marshalElem eptr eidx (SCAllOutputs (StorePath h n)) = do
      Symbol hashSym <- symbolIntern h
      Symbol nameSym <- symbolIntern n
      cctxstrSetAllOutputs eptr eidx hashSym nameSym

-- | Unmarshal a C nn_ctxstr_t back to (Text, StringContext).
unmarshalStringContext :: CCtxStrPtr -> IO (Text, StringContext)
unmarshalStringContext ptr = do
  textSym <- cctxstrText ptr
  count <- cctxstrCtxCount ptr
  let textVal = symbolText (Symbol textSym)
  elems <- mapM (readElem ptr) [0 .. count - 1]
  pure (textVal, StringContext (Set.fromList elems))
  where
    readElem cptr idx = do
      tag <- cctxstrElemTag cptr idx
      hashSym <- cctxstrElemHash cptr idx
      nameSym <- cctxstrElemName cptr idx
      let sp = StorePath (symbolText (Symbol hashSym)) (symbolText (Symbol nameSym))
      case tag of
        0 -> pure (SCPlain sp)
        1 -> do
          outSym <- cctxstrElemOutput cptr idx
          pure (SCDrvOutput sp (symbolText (Symbol outSym)))
        _ -> pure (SCAllOutputs sp)

-- | Wrap a CAttrSet in a pre-computed C thunk (pointer, no StablePtr).
{-# NOINLINE newComputedAttrsPtr #-}
newComputedAttrsPtr :: CAttrSet -> CThunkPtr
newComputedAttrsPtr cset =
  unsafePerformIO $
    cset `seq`
      cthunkNewComputedAttrs (castPtr cset)

-- | Build a C-allocated slot array from a list of 'Thunk' values.
-- Each thunk is converted to 'CThunkPtr' via 'thunkToCPtr'.
-- Returns the C array pointer and the slot count.
-- Uses 'unsafePerformIO' — safe because allocation is idempotent.
{-# NOINLINE buildCSlots #-}
buildCSlots :: [Thunk] -> (Ptr CThunkPtr, Int)
buildCSlots thunks = unsafePerformIO $ do
  let n = length thunks
  if n == 0
    then pure (nullPtr, 0)
    else do
      arr <- cenvAllocSlots (fromIntegral n)
      pokeSlots arr 0 thunks
      pure (arr, n)
  where
    pokeSlots _ _ [] = pure ()
    pokeSlots arr i (t : ts) = do
      pokeElemOff arr i (thunkToCPtr t)
      pokeSlots arr (i + 1) ts

-- | Allocate a C slot array of the given size WITHOUT filling it.
-- Returns 'nullPtr' for size 0.  Used by knot-tying sites (rec {},
-- let) where the Env must reference the slot pointer before the
-- thunks are materialized.  Caller MUST fill all slots via 'fillCSlots'
-- before any slot is read (e.g. before forcing any thunk).
{-# NOINLINE allocCSlots #-}
allocCSlots :: Int -> Ptr CThunkPtr
allocCSlots 0 = nullPtr
allocCSlots n = unsafePerformIO (cenvAllocSlots (fromIntegral n))

-- | Fill a pre-allocated C slot array with thunks.  Each thunk is
-- converted to 'CThunkPtr' via 'thunkToCPtr' and poked at its index.
-- The list length MUST equal the allocated array size.
-- Used after 'allocCSlots' in knot-tying contexts.
{-# NOINLINE fillCSlots #-}
fillCSlots :: Ptr CThunkPtr -> [Thunk] -> ()
fillCSlots arr thunks = unsafePerformIO (go 0 thunks)
  where
    go _ [] = pure ()
    go !i (t : ts) = do
      pokeElemOff arr i (thunkToCPtr t)
      go (i + 1) ts

-- | Human-readable type name for error messages.
typeName :: NixValue -> Text
typeName val = case val of
  VInt _ -> "an integer"
  VFloat _ -> "a float"
  VBool _ -> "a Boolean"
  VNull -> "null"
  VStr _ _ -> "a string"
  VPath _ -> "a path"
  VList _ -> "a list"
  VAttrs _ -> "a set"
  VLambda {} -> "a function"
  VDerivation _ -> "a derivation"
  VBuiltin _ _ -> "a built-in function"
  VCompiledRegex _ -> "a built-in function"

-- ---------------------------------------------------------------------------
-- Evaluation monad
-- ---------------------------------------------------------------------------

-- | Effect class for Nix evaluation.  Core logic is polymorphic in @m@
-- so the same evaluator composes into 'PureEval' for tests or @IO@ for
-- real file-system access (e.g. @import@, @readFile@).
class (Monad m) => MonadEval m where
  throwEvalError :: Text -> m a
  catchEvalError :: m a -> m (Either Text a)
  readFileText :: Text -> m Text
  doesPathExist :: Text -> m Bool

  -- | List a directory, returning @(name, fileType)@ pairs.
  -- @fileType@ is one of @"regular"@, @"directory"@, or @"symlink"@
  -- (matching Nix's @builtins.readDir@ semantics).
  listDirectory :: Text -> m [(Text, Text)]

  importFile :: Text -> m NixValue

  -- | Look up an environment variable.  Returns @""@ if unset.
  getEnvVar :: Text -> m Text

  -- | Get the current epoch time (seconds since 1970-01-01).
  getCurrentTime :: m Integer

  -- | Write a named file to the store, returning the store path.
  writeToStore :: Text -> Text -> m Text

  -- | Import a file with a custom scope overlaid on builtins.
  scopedImportFile :: [(Text, Thunk)] -> Text -> m NixValue

  -- | Read raw bytes from a file.  Used by @builtins.hashFile@.
  readFileBytes :: Text -> m ByteString

  -- | Classify a single filesystem path as @"regular"@, @"directory"@,
  -- @"symlink"@, or @"unknown"@.  Used by @builtins.readFileType@.
  getFileType :: Text -> m Text

  -- | Run an external process: @(command, args, stdin) -> (exitCode, stdout, stderr)@.
  runProcess :: Text -> [Text] -> Text -> m (Int, Text, Text)

  -- | Resolve a path literal to an absolute path.
  -- In IO evaluation, relative paths are resolved against the current
  -- file's directory (@esBaseDir@).  In pure evaluation, paths are
  -- returned unchanged.  This ensures that path values captured in
  -- closures remain valid after the import scope ends.
  resolvePathLiteral :: Text -> m Text

  -- | Copy a path (file or directory) to the store, returning the store path.
  -- First argument is the source path, second is the store name.
  copyPathToStore :: Text -> Text -> m Text

  -- | Print a trace/warning message.
  -- IO evaluators write to stderr; pure evaluators silently discard.
  traceMessage :: Text -> m ()

  -- | Force a thunk to a value, with memoization.
  -- IO evaluators should cache results (via per-thunk @IORef@).
  -- Pure evaluators re-evaluate each time.
  -- The first argument is the bytecode evaluation function (to break
  -- the Eval.Types → Eval circular dependency).
  forceThunk :: (Env -> Word32 -> m NixValue) -> Thunk -> m NixValue

-- | Pure evaluation monad — wraps 'Either Text'.
-- IO builtins ('readFile', 'import') are unavailable;
-- everything else evaluates identically to the IO version.
newtype PureEval a = PureEval {runPureEval :: Either Text a}
  deriving (Functor, Applicative, Monad)

instance MonadEval PureEval where
  throwEvalError msg = PureEval (Left msg)
  catchEvalError (PureEval action) = PureEval (Right action)
  readFileText _ = throwEvalError "readFile: not available in pure evaluation"
  doesPathExist _ = pure False
  listDirectory _ = throwEvalError "builtins.readDir: not available in pure evaluation"
  importFile _ = throwEvalError "import: not available in pure evaluation"
  getEnvVar _ = pure ""
  getCurrentTime = pure 0
  writeToStore _ _ = throwEvalError "toFile: not available in pure evaluation"
  scopedImportFile _ _ = throwEvalError "scopedImport: not available in pure evaluation"
  readFileBytes _ = throwEvalError "hashFile: not available in pure evaluation"
  getFileType _ = throwEvalError "readFileType: not available in pure evaluation"
  runProcess _ _ _ = throwEvalError "runProcess: not available in pure evaluation"
  copyPathToStore _ _ = throwEvalError "builtins.path: not available in pure evaluation"
  traceMessage _ = pure ()
  resolvePathLiteral = pure
  forceThunk evalFn (Thunk ptr) =
    -- Read the C thunk via unsafePerformIO — safe because reads are
    -- idempotent and PureEval never writes back (no memoization).
    -- Does NOT mark blackholes — PureEval may re-force the same thunk.
    -- Dispatches on val_tag for computed scalars (no StablePtr deref).
    case unsafePerformIO (cthunkState ptr) of
      1 {- COMPUTED -} ->
        let tag = unsafePerformIO (cthunkValueTag ptr)
         in case tag of
              0 {- INT -} -> pure (VInt (fromIntegral (unsafePerformIO (cthunkGetInt ptr))))
              1 {- FLOAT -} -> pure (VFloat (unsafePerformIO (cthunkGetFloat ptr)))
              2 {- BOOL -} -> pure (VBool (unsafePerformIO (cthunkGetBool ptr) /= 0))
              3 {- NULL -} -> pure VNull
              4 {- STR -} ->
                let sym = unsafePerformIO (cthunkGetStr ptr)
                 in pure (VStr (symbolText (Symbol sym)) emptyContext)
              5 {- PATH -} ->
                let sym = unsafePerformIO (cthunkGetPath ptr)
                 in pure (VPath (symbolText (Symbol sym)))
              6 {- LIST -} ->
                let listPtr = unsafePerformIO (cthunkGetList ptr)
                    thunks = unsafePerformIO (clistToThunkList (castPtr listPtr))
                 in pure (VList (map Thunk thunks))
              7 {- ATTRS -} ->
                let p = unsafePerformIO (cthunkGetAttrs ptr)
                 in pure (VAttrs (AttrSet (castPtr p)))
              8 {- CTXSTR -} ->
                let csptr = unsafePerformIO (cthunkGetCtxStr ptr)
                    (t, ctx) = unsafePerformIO (unmarshalStringContext (castPtr csptr))
                 in pure (VStr t ctx)
              _ {- PTR -} ->
                let payload = unsafePerformIO (cthunkPayload ptr)
                    val = unsafePerformIO (deRefStablePtr (castPtrToStablePtr payload))
                 in pure val
      _ {- PENDING or BLACKHOLE -} ->
        let bcIdx = unsafePerformIO (cthunkGetBcIdx ptr)
            envSp = unsafePerformIO (cthunkPayload ptr)
            env = unsafePerformIO (deRefStablePtr (castPtrToStablePtr envSp))
         in evalFn env bcIdx
