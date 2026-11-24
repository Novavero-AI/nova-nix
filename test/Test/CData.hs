{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the evaluator's C data layer (symbols, attribute sets,
-- thunks, values and the arena) and the bytecode it runs.
module Test.CData
  ( arenaGuardChildFlag,
    arenaGuardChild,
    testArenaGuard,
    testSymbol,
    testCAttrSet,
    testCThunk,
    testBytecodePerElement,
    testBytecodeCountSpill,
    testValueCountWidths,
    testLambdaShape,
    testUnknownTags,
    testBytecodeTags,
    testBytecodeCorrupt,
    testBytecodeCompile,
  )
where

import Control.Exception (ErrorCall (..), displayException, evaluate, finally, throwIO, try)
import Control.Monad (foldM, unless, void, when, (>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Functor ((<&>))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (find, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Foreign.C.Types (CChar, CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peekByteOff, poke, pokeByteOff, sizeOf)
import Nix.Builtins (builtinEnv)
import Nix.Eval (BuiltinArg (..), NixValue (..), StringContext (..), StringContextElement (..), Thunk (..), attrSetFromMap, attrSetLookup, emptyEnv, eval, evalBytecode, evalParsed, evaluated, mkStr, readThunkValue, runPureEval, unrestrictedPolicy)
import Nix.Eval.AllocSite (EnvSite (..), SetSite (..))
import Nix.Eval.Builtin (Builtin, builtinIndex, builtinName)
import qualified Nix.Eval.Builtin as Builtin
import Nix.Eval.CAttrSet (CAttrSetStep (..), cattrsetFind, cattrsetFreeze, cattrsetFromNames, cattrsetIndex, cattrsetInsert, cattrsetLookup, cattrsetMember, cattrsetNew, cattrsetRemoveKeys, cattrsetSetValue, cattrsetSize, cattrsetUnion, cattrsetUpdate, cattrsetWalk, cattrsetWalkNext, flatCAttrSet)
import Nix.Eval.CBytecode (attrkeyStatic, attrsRec, binaryAdd, bindNamed, captureNone, captureSlots, captureWithScopes, cbcArg1, cbcArg2, cbcArg3, cbcData, cbcDataCount, cbcEmit, cbcEmitData, cbcEmitString, cbcFlags, cbcOpCount, cbcOpcode, cbcShortArg, cbcStringText, formalName, formalNamedSet, formalSet, formalSharedDefault, strpartInterp, strpartLit, unaryNegate, pattern OpApp, pattern OpAssert, pattern OpAttrs, pattern OpBinary, pattern OpHasAttr, pattern OpIf, pattern OpIndStr, pattern OpLambda, pattern OpLet, pattern OpList, pattern OpLitBool, pattern OpLitFloat, pattern OpLitInt, pattern OpLitNull, pattern OpLitPath, pattern OpLitUri, pattern OpResolvedVar, pattern OpSelect, pattern OpStr, pattern OpUnary, pattern OpVar, pattern OpWith, pattern OpWithVar)
import Nix.Eval.CCtxStr (cctxstrNew)
import Nix.Eval.CLambda (clambdaAllowExtra, clambdaFormalCount, clambdaFormalsType, clambdaNew, clambdaShape, clambdaShapeAllowExtra, clambdaShapeFormalCount, clambdaShapeFormalsType)
import Nix.Eval.CList (CList (..), clistFromThunks)
import Nix.Eval.CPrimop (cprimopAppNew)
import Nix.Eval.CStats (SiteCounts (..))
import Nix.Eval.CStatus (CStatusError (..), arenaLive)
import Nix.Eval.CStr (cstrNew)
import Nix.Eval.CThunk (cthunkBool, cthunkGetBool, cthunkGetInt, cthunkMarkBlackhole, cthunkNewBc, cthunkNewComputedInt, cthunkNull, cthunkPayload, cthunkPrimop, cthunkSetComputedInt, cthunkSetComputedNull, cthunkState, cthunkValueTag)
import Nix.Eval.Compile (BcBinding (..), BcBindingCounts (..), BcBindings (..), BcCaptureInfo (..), BcInheritedName (..), BcInheritedNames, compileExpr, compileFormalsToEval, decodeBcCaptureInfo, decodeBinaryOp, decodeUnaryOp, encodeBinaryOp, encodeUnaryOp, foldBcBindings, foldBcCapturedSlots, foldBcInheritedNames, reassembleInt64)
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..), FormalDefault (..), LambdaName (..))
import Nix.Eval.SourcePos (PosIdx (..), RecordedPos (..), lookupPos, newPosRecorder, noPos, printPos, recordPos)
import Nix.Eval.Symbol (Symbol (..), invalidSymbolId, symbolCount, symbolIntern, symbolLen, symbolText)
import Nix.Eval.Types (AttrEntry (..), AttrSet (..), Env (..), attrSetAllPairs, attrSetFindName, attrSetFoldM, attrSetKeys, attrSetLookupSymbolEntry, attrSetMapWithKey, attrSetMemberSymbol, attrSetToMap, newEnv, readComputed, tieFrame, pattern ThunkComputed, pattern ValueBool, pattern ValueNull)
import Nix.Expr.Types
import Nix.Parser (parseNix)
import Nix.Store.Path.Internal (StorePath (..))
import System.Directory (getTemporaryDirectory)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as Proc
import Test.Harness (TestResult (..), andThen, assertEqual, assertEval, censusStoreHash, compiledBindings, decodeFormals, evalNix, evalNixIO, inTestSession, runTest, runTestM, setLayerRhsMax, setMaxLayers, sitesAdded, testOrigin, testPos)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Arena lifecycle guard (C FFI)
-- ---------------------------------------------------------------------------

-- | The argument that turns this executable into the child of
-- 'testArenaGuard'.  The child reaches the C layer outside any
-- evaluation session, which the suite cannot do in its own process:
-- 'main' runs every group in one session ('inTestSession').
arenaGuardChildFlag :: String
arenaGuardChildFlag = "--arena-guard-child"

-- | One way a library user reaches the C layer, with what the child must
-- report for it.
data ArenaGuardPath = ArenaGuardPath
  { -- | The child's argument.
    guardName :: !String,
    -- | The suite's name for the case.
    guardClaim :: !Text,
    -- | What the child must report.
    guardOutcome :: !GuardOutcome,
    -- | The child's body, run outside any evaluation session.
    guardAction :: !(IO ())
  }

-- | What a guard child must report for its path.
data GuardOutcome
  = -- | 'ArenaNotInitialized', naming this C entry point.
    RefusedAt !Text
  | -- | No exception: the path needs no arena.
    Served

-- | The line a guard child prints when its path raised nothing.
servedLine :: String
servedLine = "served without an exception"

-- | The paths a library user reaches the C layer by.  The first C call
-- of each of the first five lands in a different sub-arena, and of each
-- of the next four in a different tracker of the objects the arena's
-- teardown frees one by one (cbits/nn_tracker.h), where an object made
-- outside every session would be freed by the next session's end under
-- whoever held it.  One runs a whole session first, so both ends of the
-- window are covered.  One reads a symbol, which the table answers
-- outside the window as it answers an id it does not hold.  Two read
-- the shared @null@, @true@ and @false@ thunks, which must be valid in
-- every window: were they arena cells behind a process-lifetime CAF,
-- forcing one outside the window would poison it with
-- 'ArenaNotInitialized' for every later arena, and forcing one inside a
-- session would leave it pointing into memory the session's end frees,
-- a use-after-free the ASan job reports.  One evaluates an empty string
-- before anything else in a fresh arena: its payload of no words sits
-- at data offset 0, which the store does not hold yet, so a read there
-- fails the store's bounds assertion.  The last three show that a
-- session tears the arena down as it returns and as an exception leaves
-- it, and that a nested session leaves the arena to the one around it.
arenaGuardPaths :: [ArenaGuardPath]
arenaGuardPaths =
  [ noArena "compile" "nn_bc_emit" evalNamelessProgram,
    noArena "thunk" "nn_thunk_new_computed_int" (void (evaluated (VInt 1))),
    noArena "string" "nn_str_new" (void (evaluated (mkStr "a"))),
    noArena "symbol" "nn_symbol_intern" (void (symbolIntern "a")),
    noArena "env" "nn_env_new" (void (evaluated VNull >>= \nullCell -> newEnv EnvClosure Nothing [nullCell])),
    -- A plain lambda (formals type 0) with no env, builtin 0 applied to
    -- the shared null cell, and a context string with no text: each
    -- allocator stores what it is given without reading it, so the
    -- tracker's refusal is what stops it.
    noArena "set" "nn_attrset_new" (void (cattrsetNew SetLiteral 1)),
    noArena "lambda" "nn_lambda_new" (void (clambdaNew nullPtr 0 0 0 0 0 0)),
    noArena "partial-application" "nn_primop_app_new" (void (cprimopAppNew 0 0 [cthunkNull])),
    noArena "context-string" "nn_ctxstr_new" (void (cctxstrNew nullPtr 0)),
    noArena "after-session" "nn_bc_emit" (inTestSession (pure ()) >> evalNamelessProgram),
    noArena "symbol-read" "nn_symbol_text" (void (evaluate (symbolText (Symbol 1)))),
    ArenaGuardPath
      { guardName = "constants-outside-window",
        guardClaim = "the null, true and false thunks read correctly outside every session and in a later one",
        guardOutcome = Served,
        guardAction = readConstants >> inTestSession readConstants
      },
    ArenaGuardPath
      { guardName = "constants-across-cycles",
        guardClaim = "the null, true and false thunks read correctly across two sessions",
        guardOutcome = Served,
        guardAction = inTestSession readConstants >> inTestSession readConstants
      },
    ArenaGuardPath
      { guardName = "empty-string-first",
        guardClaim = "an empty string evaluated first in a fresh arena reads no data word",
        guardOutcome = Served,
        guardAction = inTestSession $ case runPureEval (eval emptyEnv (EStr [])) of
          Right (VStr bytes _) | BS.null bytes -> pure ()
          other -> throwIO (ErrorCall ("the empty string evaluated to " ++ show other))
      },
    ArenaGuardPath
      { guardName = "session-returns",
        guardClaim = "a session tears the arena down as it returns",
        guardOutcome = Served,
        guardAction = inTestSession (pure ()) >> arenaGone "a session that returned"
      },
    ArenaGuardPath
      { guardName = "session-throws",
        guardClaim = "a session tears the arena down as an exception leaves it",
        guardOutcome = Served,
        guardAction = do
          left <- try (inTestSession (throwIO (ErrorCall "inside the session")))
          case left of
            Left (ErrorCall _) -> arenaGone "a session an exception left"
            Right () -> throwIO (ErrorCall "the exception did not leave the session")
      },
    ArenaGuardPath
      { guardName = "nested-session",
        guardClaim = "a nested session leaves the arena to the session around it",
        guardOutcome = Served,
        guardAction = inTestSession $ do
          inTestSession (pure ())
          live <- arenaLive
          unless live (throwIO (ErrorCall "the nested session tore the outer session's arena down"))
          readConstants
      }
  ]
  where
    noArena name site = ArenaGuardPath name ("no arena: " <> T.pack name <> " raises ArenaNotInitialized at " <> site) (RefusedAt site)
    arenaGone what = do
      live <- arenaLive
      when live (throwIO (ErrorCall (what ++ " left the arena live")))
    -- A small program forced to its result.  It names nothing, so its
    -- first C call compiles it rather than interning a name.
    evalNamelessProgram = case parseNix "/tmp" (SourceText "<expr>") "5 * 2 + 1" of
      Left err -> throwIO (ErrorCall (show err))
      Right parsed -> void (evaluate (runPureEval (evalParsed (builtinEnv unrestrictedPolicy 0 []) parsed)))
    -- Read through the IO accessors: a pure read of a constant is itself
    -- a constant GHC may share between the cycles, and the second would
    -- then read nothing.  A wrong cell is a broken outcome rather than the
    -- guard under test, so it leaves the child by another exception.
    readConstants = do
      found <- mapM (evaluated >=> readCell . unThunk) [VNull, VBool True, VBool False]
      unless (found == [(ThunkComputed, ValueNull, 0), (ThunkComputed, ValueBool, 1), (ThunkComputed, ValueBool, 0)]) $
        throwIO (ErrorCall ("the constants read back as " ++ show found))
    readCell ptr = (,,) <$> cthunkState ptr <*> cthunkValueTag ptr <*> cthunkGetBool ptr

-- | Child body: run one path with no arena and print what it raised, or
-- 'servedLine'.  Exit 0 either way; a segfault, a sanitizer report or
-- any other exception type surfaces as the process's own exit code.
arenaGuardChild :: String -> IO ()
arenaGuardChild name = case [guardAction path | path <- arenaGuardPaths, guardName path == name] of
  [action] -> do
    outcome <- try action
    putStrLn $ case outcome of
      Left (err :: CStatusError) -> displayException err
      Right () -> servedLine
  _ -> hPutStrLn stderr ("unknown arena guard path: " ++ name) >> exitWith (ExitFailure 2)

-- ---------------------------------------------------------------------------
-- Tests: upstream's startup names, and the call, select and abort errors
-- ---------------------------------------------------------------------------

-- | The smallest slot count whose byte size exceeds @UINT32_MAX@.
-- The env pages refuse an env or a list's items array of that many
-- before touching a page, so inside a live arena it fails without
-- exhausting anything.
slotCountPastUInt32 :: Int
slotCountPastUInt32 = fromIntegral (maxBound :: Word32) `div` sizeOf (nullPtr :: Ptr ()) + 1

-- | One byte past the longest string the symbol table or a string arena
-- can hold.
lengthPastUInt32 :: Int
lengthPastUInt32 = fromIntegral (maxBound :: Word32) + 1

-- | Run an action on a ByteString whose length claims 'lengthPastUInt32'
-- bytes over one real byte, so no 4 GiB buffer is needed.  Sound only
-- because each C function it reaches refuses on the length before
-- reading: one that read would run out of bounds here.
withOversizedBytes :: (BS.ByteString -> IO a) -> IO a
withOversizedBytes use =
  alloca $ \byte -> do
    poke byte (0 :: CChar)
    use =<< BSU.unsafePackCStringLen (byte, lengthPastUInt32)

-- | The interner itself, for the one refusal no name reaches: the library
-- interns names given as text, and no test can build 4 GiB of text.
foreign import ccall unsafe "nn_symbol_intern"
  c_nn_symbol_intern :: Ptr CChar -> CSize -> IO Word32

-- | The list constructor itself, for the one refusal no list reaches: a
-- list is built from the cells it holds, and no test can hold the half a
-- billion a refused count needs.
foreign import ccall unsafe "nn_list_new"
  c_nn_list_new :: Ptr (Ptr ()) -> Word32 -> IO CInt

-- | Intern an oversized name through the C entry point, answering what it
-- returns.
internOversizedSymbol :: IO Word32
internOversizedSymbol =
  withOversizedBytes $ \bytes ->
    BSU.unsafeUseAsCStringLen bytes $ \(ptr, len) -> c_nn_symbol_intern ptr (fromIntegral len)

-- | Reaching the C layer outside an evaluation session raises
-- 'ArenaNotInitialized' naming the entry point, rather than a SIGSEGV.
-- Each such path runs in a child process.
-- The live side of the same check runs in this process, under the
-- suite's bracket: a refusal with the arena up is 'CAllocationFailed'.
testArenaGuard :: IO [Bool]
testArenaGuard = do
  putStrLn "arena/guard"
  self <- getExecutablePath
  children <-
    mapM
      ( \path -> runTestM (guardClaim path) $ do
          (code, out, err) <- Proc.readCreateProcessWithExitCode (Proc.proc self [arenaGuardChildFlag, guardName path]) ""
          let message = T.pack out
          pure $ case (code, guardOutcome path) of
            (ExitSuccess, RefusedAt site)
              | "withEvalSession" `T.isInfixOf` message && site `T.isInfixOf` message -> Pass
              | otherwise -> Fail ("message does not name withEvalSession and " <> site <> ": " <> message)
            (ExitSuccess, Served)
              | T.strip message == T.pack servedLine -> Pass
              | otherwise -> Fail ("expected no exception, got: " <> message)
            (ExitFailure n, _) -> Fail ("child exited " <> T.pack (show n) <> " (negative is a signal): " <> T.pack err)
      )
      arenaGuardPaths
  live <-
    sequence
      [ liveRefusal "an env whose slots pass UINT32_MAX bytes" "nn_env_new" (void (tieFrame EnvLet Nothing slotCountPastUInt32 (const (pure [])))),
        -- The list's items come from the same pages, so the same count is
        -- refused there, with a status: a NULL would read as the empty list.
        runTestM "live arena: a list past UINT32_MAX slot bytes is refused with a status" $ do
          status <- alloca (\out -> c_nn_list_new out (fromIntegral slotCountPastUInt32))
          pure (assertEqual "status" (-1) status),
        -- A length a string's uint32_t cannot hold, refused rather than
        -- truncated to a shorter string, in the value arena and in the
        -- code's.
        liveRefusal "a string longer than UINT32_MAX bytes" "nn_str_new" (void (withOversizedBytes cstrNew)),
        liveRefusal "a code string longer than UINT32_MAX bytes" "nn_bc_emit_string" (void (withOversizedBytes cbcEmitString))
      ]
  tableIntact <-
    runTestM "a symbol longer than UINT32_MAX bytes is refused, leaving the table unchanged" $ do
      before <- symbolCount
      refused <- internOversizedSymbol
      after <- symbolCount
      sym <- symbolIntern "after-a-refusal"
      pure $
        if refused == 0 && before == after && symbolText sym == "after-a-refusal"
          then Pass
          else Fail ("answer " <> T.pack (show refused) <> ", count " <> T.pack (show before) <> " -> " <> T.pack (show after) <> ", text " <> symbolText sym)
  pure (children ++ live ++ [tableIntact])
  where
    liveRefusal what site action =
      runTestM ("live arena: " <> what <> " raises CAllocationFailed at " <> site) $ do
        alive <- arenaLive
        outcome <- try action
        pure $ case (alive, outcome) of
          (True, Left (CAllocationFailed refused _)) | refused == T.unpack site -> Pass
          (True, Left err) -> Fail ("expected CAllocationFailed at " <> site <> ", got " <> T.pack (show err))
          (True, Right ()) -> Fail "no exception raised"
          (False, _) -> Fail "arenaLive is False inside the suite's bracket"

-- ---------------------------------------------------------------------------
-- Symbol interning (C FFI)
-- ---------------------------------------------------------------------------

testSymbol :: IO [Bool]
testSymbol = do
  putStrLn "symbol"
  sequence
    [ runTestM "intern returns non-zero" $ do
        sym <- symbolIntern "hello"
        pure (if unSymbol sym /= 0 then Pass else Fail "got symbol 0"),
      runTestM "intern same string returns same symbol" $ do
        sym1 <- symbolIntern "name"
        sym2 <- symbolIntern "name"
        pure (assertEqual "same symbol" sym1 sym2),
      runTestM "intern different strings returns different symbols" $ do
        sym1 <- symbolIntern "foo"
        sym2 <- symbolIntern "bar"
        pure (if sym1 /= sym2 then Pass else Fail "symbols should differ"),
      runTestM "symbolText round-trips" $ do
        sym <- symbolIntern "version"
        let txt = symbolText sym
        pure (assertEqual "text" "version" txt),
      runTestM "symbolLen correct" $ do
        sym <- symbolIntern "outputs"
        pure (assertEqual "len" 7 (symbolLen sym)),
      runTestM "empty string interns" $ do
        sym <- symbolIntern ""
        pure (assertEqual "text and length" ("", 0) (symbolText sym, symbolLen sym)),
      runTestM "the invalid symbol id reads as no name" $
        pure (assertEqual "text and length" ("", 0) (symbolText (Symbol invalidSymbolId), symbolLen (Symbol invalidSymbolId))),
      -- The table answers an id past its count as it answers the invalid
      -- one, so only the readers' check tells the defect from no name.
      runTestM "reading a symbol id past the table's count raises CInvariantViolated" $ do
        count <- symbolCount
        let unheld = Symbol (count + 1)
        text <- try (evaluate (symbolText unheld))
        len <- try (evaluate (symbolLen unheld))
        pure (assertEqual "sites" (Just "nn_symbol_text", Just "nn_symbol_len") (violatedAt text, violatedAt len)),
      runTestM "symbolCount tracks unique entries" $ do
        _ <- symbolIntern "alpha"
        _ <- symbolIntern "beta"
        _ <- symbolIntern "alpha"
        count <- symbolCount
        -- count includes all symbols interned in this bracket,
        -- so at least the ones from prior tests plus alpha + beta
        pure (if count >= 2 then Pass else Fail ("count too low: " <> T.pack (show count))),
      runTestM "many symbols (stress)" $ do
        let names = map (\i -> "pkg_" <> T.pack (show (i :: Int))) [1 .. 1000]
        syms <- mapM symbolIntern names
        -- All unique
        let unique = length (Set.fromList (map unSymbol syms))
        pure (assertEqual "1000 unique" 1000 unique)
    ]
  where
    violatedAt :: Either CStatusError a -> Maybe String
    violatedAt = \case
      Left (CInvariantViolated site _) -> Just site
      _ -> Nothing

-- ---------------------------------------------------------------------------
-- C attribute set (FFI)
-- ---------------------------------------------------------------------------

testCAttrSet :: IO [Bool]
testCAttrSet = do
  putStrLn "cattrset"
  sequence
    [ runTestM "a new set holds no entries" $ do
        set <- cattrsetNew SetLiteral 16
        cattrsetFreeze set
        n <- cattrsetSize set
        pure (assertEqual "size" 0 n),
      runTestM "insert + freeze + lookup" $ do
        set <- cattrsetNew SetLiteral 4
        kName <- symbolIntern "name"
        kVer <- symbolIntern "version"
        nameCell <- textCell "hello"
        verCell <- textCell "1.0"
        cattrsetInsert set kName noPos nameCell
        cattrsetInsert set kVer noPos verCell
        cattrsetFreeze set
        result <- cattrsetLookup set kName
        pure (assertEqual "lookup name" (Just nameCell) result),
      runTestM "lookup missing key returns Nothing" $ do
        set <- cattrsetNew SetLiteral 4
        kFoo <- symbolIntern "foo"
        kBar <- symbolIntern "bar"
        cattrsetInsert set kFoo noPos cthunkNull
        cattrsetFreeze set
        result <- cattrsetLookup set kBar
        pure (assertEqual "lookup bar" Nothing result),
      runTestM "size after freeze" $ do
        set <- cattrsetNew SetLiteral 4
        keys <- mapM symbolIntern ["a", "b", "c"]
        mapM_ (\key -> cattrsetInsert set key noPos cthunkNull) keys
        cattrsetFreeze set
        n <- cattrsetSize set
        pure (assertEqual "size" 3 n),
      runTestM "duplicate keys: last writer wins" $ do
        set <- cattrsetNew SetLiteral 4
        kName <- symbolIntern "name"
        first <- textCell "first"
        second <- textCell "second"
        cattrsetInsert set kName noPos first
        cattrsetInsert set kName noPos second
        cattrsetFreeze set
        n <- cattrsetSize set
        result <- cattrsetLookup set kName
        pure (assertEqual "size and value" (1, Just second) (n, result)),
      -- Inserted from the highest symbol id down, so freeze has to sort.
      runTestM "a walk reads the keys in symbol order" $ do
        set <- cattrsetNew SetLiteral 8
        inserted <- mapM symbolIntern ["zzz", "aaa", "mmm"]
        mapM_ (\key -> cattrsetInsert set key noPos cthunkNull) (sortOn Down inserted)
        cattrsetFreeze set
        keys <- map (\(name, _, _) -> name) <$> walkedEntries set
        pure (assertEqual "keys" (sort inserted) keys),
      runTestM "union right-biased" $ do
        setA <- cattrsetNew SetLiteral 4
        setB <- cattrsetNew SetLiteral 4
        kX <- symbolIntern "x"
        kY <- symbolIntern "y"
        kZ <- symbolIntern "z"
        fromA <- textCell "fromA"
        fromB <- textCell "fromB"
        onlyA <- textCell "onlyA"
        onlyB <- textCell "onlyB"
        cattrsetInsert setA kX noPos fromA
        cattrsetInsert setA kZ noPos onlyA
        cattrsetInsert setB kX noPos fromB
        cattrsetInsert setB kY noPos onlyB
        cattrsetFreeze setA
        cattrsetFreeze setB
        merged <- cattrsetUnion SetUpdate setA setB
        n <- cattrsetSize merged
        found <- mapM (cattrsetLookup merged) [kX, kY, kZ]
        pure (assertEqual "size and entries" (3, map Just [fromB, onlyB, onlyA]) (n, found)),
      runTestM "stress: 10k entries" $ do
        set <- cattrsetNew SetLiteral 1024
        syms <- mapM (\i -> symbolIntern ("key_" <> T.pack (show (i :: Int)))) [1 .. 10000]
        mapM_ (\sym -> cattrsetInsert set sym noPos cthunkNull) syms
        cattrsetFreeze set
        n <- cattrsetSize set
        -- Spot-check a lookup from the middle of the key range
        hit <- case drop 5000 syms of
          middleSym : _ -> cattrsetLookup set middleSym
          [] -> pure Nothing
        pure
          ( if n == 10000 && isJust hit
              then Pass
              else Fail ("size=" <> T.pack (show n))
          ),
      -- The index of an absent name is the C side's NN_ATTRSET_NOT_FOUND.
      runTestM "a name table's index finds each name at its sorted position and none for an absent name" $ do
        syms <- mapM (\i -> symbolIntern ("index_" <> T.pack (show (i :: Int)))) [1 .. 100]
        absent <- symbolIntern "index_absent"
        table <- cattrsetFromNames SetScope (reverse syms)
        found <- mapM (cattrsetIndex table) (sort syms)
        missing <- cattrsetIndex table absent
        pure
          ( if found == map Just [0 .. 99] && isNothing missing
              then Pass
              else Fail ("found=" <> T.pack (show found) <> " missing=" <> T.pack (show missing))
          ),
      -- A position travels with its entry: through both sorts freeze uses
      -- (insertion up to 64 entries, the tagged sort past that), the dedup
      -- that keeps a key's last insert, the growth from the default
      -- capacity that moves the positions within their block, a union
      -- that keeps the right side's on a clash, and a key removal.  The
      -- pool is small enough that inserts repeat keys and large enough to
      -- pass the 64-entry switch.
      runTestM "positions follow their entries through freeze, union and removal" $ do
        pool <- mapM (\i -> symbolIntern ("pos_" <> T.pack (show (i :: Int)))) [1 .. 100]
        let genInserts = QC.listOf ((,) <$> QC.elements pool <*> QC.arbitrary)
            build inserts = do
              set <- cattrsetNew SetLiteral 0
              mapM_ (\(key, pos) -> cattrsetInsert set key (PosIdx pos) cthunkNull) inserts
              cattrsetFreeze set
              pure set
            property =
              QC.forAll (QC.resize 200 ((,,) <$> genInserts <*> genInserts <*> QC.sublistOf pool)) $ \(left, right, removed) ->
                QC.ioProperty $ do
                  leftSet <- build left
                  rightSet <- build right
                  merged <- cattrsetUnion SetUpdate leftSet rightSet
                  kept <- cattrsetRemoveKeys SetRemoveAttrs leftSet removed
                  actual <- mapM entryPositions [leftSet, merged, kept]
                  let leftModel = Map.fromList left
                      rightModel = Map.fromList right
                  pure (actual QC.=== [leftModel, Map.union rightModel leftModel, Map.withoutKeys leftModel (Set.fromList removed)])
        quickCheckPasses property,
      -- lookup answers Nothing for a value not filled yet; find finds it.
      runTestM "a name table finds each name with no value until its value is written" $ do
        written <- symbolIntern "table_written"
        unwritten <- symbolIntern "table_unwritten"
        cell <- textCell "filled"
        table <- cattrsetFromNames SetScope [written, unwritten]
        let set = flatCAttrSet table
        before <- mapM (cattrsetFind set) [written, unwritten]
        cattrsetIndex table written >>= mapM_ (\i -> cattrsetSetValue table i cell)
        after <- mapM (cattrsetFind set) [written, unwritten]
        members <- mapM (cattrsetMember set) [written, unwritten]
        lookups <- mapM (cattrsetLookup set) [written, unwritten]
        pure
          ( assertEqual "before the write" [Just (noPos, nullPtr), Just (noPos, nullPtr)] before
              `andThen` assertEqual "after the write" [Just (noPos, cell), Just (noPos, nullPtr)] after
              `andThen` assertEqual "members" [True, True] members
              `andThen` assertEqual "lookups" [Just cell, Nothing] lookups
          ),
      -- A set holds the last insert of each name, in symbol order, which
      -- a map of the inserts models.  A walk reads every entry in that
      -- order, and a find of a name its entry, the null pointer of a name
      -- table not yet filled among the values.  The inserts repeat names
      -- and pass the 64 entries at which freeze changes sorts.
      runTestM "a walk of a set and a find of each name read the last insert of each name, in symbol order" $ do
        pool <- mapM (\i -> symbolIntern ("walk_" <> T.pack (show (i :: Int)))) [1 .. 100]
        absent <- symbolIntern "walk_absent"
        cells <- mapM textCell ["one", "two", "three"]
        let names = absent : pool
            asEntry entry = (entryPos entry, unThunk (entryThunk entry))
        quickCheckPasses $
          QC.forAll (QC.resize 200 ((,) <$> genValuedInserts pool (nullPtr : cells) <*> QC.sublistOf pool)) $ \(inserts, wanted) -> QC.ioProperty $ do
            set <- buildValued inserts
            walked <- walkedEntries set
            folded <- reverse <$> attrSetFoldM (\acc name entry -> pure ((name, entryPos entry, unThunk (entryThunk entry)) : acc)) [] (AttrSet set)
            keys <- attrSetKeys (AttrSet set)
            asMap <- attrSetToMap (AttrSet set)
            found <- mapM (cattrsetFind set) names
            entries <- mapM (\name -> fmap asEntry <$> attrSetLookupSymbolEntry name (AttrSet set)) names
            members <- mapM (cattrsetMember set) names
            setMembers <- mapM (\name -> attrSetMemberSymbol name (AttrSet set)) names
            firstWanted <- attrSetFindName (`elem` wanted) (AttrSet set)
            let model = insertsModel inserts
                byName = Map.fromList [(name, (pos, value)) | (name, pos, value) <- model]
                modelNames = [name | (name, _, _) <- model]
                modelFound = map (`Map.lookup` byName) names
                modelMembers = map (`Map.member` byName) names
            pure $
              (walked, folded, keys, asMap, found, entries, members, setMembers, firstWanted)
                QC.=== (model, model, map symbolText modelNames, Map.fromList [(symbolText name, Thunk value) | (name, _, value) <- model], modelFound, modelFound, modelMembers, modelMembers, find (`elem` wanted) modelNames),
      -- A chain of updates beside a flattened copy, made by unions, and a
      -- map of it: each update layers while the left's chain holds fewer
      -- than eight sets and the right at most 16 entries, as the copies it
      -- counts show, and every read of the chain, and of each set made
      -- from it, answers as the map's does.  The right operands cross 16
      -- entries, and the chains eight sets.
      runTestM "a chain of updates reads as its flattened copy and a map of it, and layers as upstream's does" $ do
        pool <- mapM (\i -> symbolIntern ("chain_" <> T.pack (show (i :: Int)))) [1 .. 40]
        absent <- symbolIntern "chain_absent"
        cells <- mapM textCell ["one", "two", "three"]
        let names = absent : pool
            sized n = QC.resize n (genValuedInserts pool cells)
            gen = (,,,) <$> sized 60 <*> (QC.chooseInt (1, 12) >>= \n -> QC.vectorOf n (sized 20)) <*> QC.sublistOf pool <*> sized 20
            modelOf inserts = Map.fromList [(name, (pos, value)) | (name, pos, value) <- insertsModel inserts]
            asWalk model = [(name, pos, value) | (name, (pos, value)) <- Map.toAscList model]
            step (set, copy, model, layers, steps) (right, rightModel) = do
              made <- newIORef set
              added <- sitesAdded (cattrsetUpdate SetUpdate set right >>= writeIORef made)
              updated <- readIORef made
              copied <- cattrsetUnion SetUpdate copy right
              let merged = Map.union rightModel model
                  layered = not (Map.null model) && not (Map.null rightModel) && layers < setMaxLayers && Map.size rightModel <= setLayerRhsMax
                  expected = if layered then Map.size rightModel else Map.size merged
                  chained = if layered then layers + 1 else 1
              pure (updated, copied, merged, chained, (fmap sitesUpdateCopied added, Right (fromIntegral expected)) : steps)
        quickCheckPasses $
          QC.forAll gen $ \(baseInserts, rightsInserts, removed, otherInserts) -> QC.ioProperty $ do
            base <- buildValued baseInserts
            rights <- mapM buildValued rightsInserts
            other <- buildValued otherInserts
            let otherModel = modelOf otherInserts
            (chain, copy, model, _, steps) <- foldM step (base, base, modelOf baseInserts, 1 :: Int, []) (zip rights (map modelOf rightsInserts))
            walked <- walkedEntries chain
            copyWalked <- walkedEntries copy
            size <- cattrsetSize chain
            found <- mapM (cattrsetFind chain) names
            looked <- mapM (cattrsetLookup chain) names
            members <- mapM (cattrsetMember chain) names
            keys <- attrSetKeys (AttrSet chain)
            asMap <- attrSetToMap (AttrSet chain)
            kept <- cattrsetRemoveKeys SetRemoveAttrs chain removed >>= walkedEntries
            unions <- mapM (>>= walkedEntries) [cattrsetUnion SetUpdate chain other, cattrsetUnion SetUpdate other chain]
            updates <- mapM (>>= walkedEntries) [cattrsetUpdate SetUpdate chain other, cattrsetUpdate SetUpdate other chain]
            mapped <- attrSetMapWithKey SetMapAttrs (const pure) (AttrSet chain) >>= walkedEntries . unAttrSet
            let counted = reverse (map fst steps)
                expectedCounts = reverse (map snd steps)
                both = [asWalk (Map.union otherModel model), asWalk (Map.union model otherModel)]
            pure $
              QC.conjoin
                [ QC.counterexample "the copies" (counted QC.=== expectedCounts),
                  QC.counterexample "the walk and the size" ((walked, copyWalked, fromIntegral size) QC.=== (asWalk model, asWalk model, Map.size model)),
                  QC.counterexample "the reads by name" ((found, looked, members) QC.=== (map (`Map.lookup` model) names, map (fmap snd . (`Map.lookup` model)) names, map (`Map.member` model) names)),
                  QC.counterexample "the keys and the map" ((keys, asMap) QC.=== (map symbolText (Map.keys model), Map.fromList [(symbolText name, Thunk value) | (name, (_, value)) <- Map.toList model])),
                  QC.counterexample "the removal" (kept QC.=== asWalk (Map.withoutKeys model (Set.fromList removed))),
                  QC.counterexample "the unions and the updates" ((unions, updates) QC.=== (both, both)),
                  QC.counterexample "mapAttrs" (mapped QC.=== [(name, noPos, value) | (name, (_, value)) <- Map.toAscList model])
                ],
      -- The action answers False the nth time it is asked, so a walk in
      -- lockstep asks of the first n pairs and no more, or of every pair
      -- when the sets end first.
      runTestM "a walk of two sets in lockstep asks of each pair in order and of none past the first it fails" $ do
        pool <- mapM (\i -> symbolIntern ("lockstep_" <> T.pack (show (i :: Int)))) [1 .. 30]
        cells <- mapM textCell ["one", "two"]
        quickCheckPasses $
          QC.forAll ((,,) <$> genValuedInserts pool cells <*> genValuedInserts pool cells <*> QC.chooseInt (1, 40)) $ \(leftInserts, rightInserts, failAt) -> QC.ioProperty $ do
            left <- buildValued leftInserts
            right <- buildValued rightInserts
            let leftEntries = insertsModel leftInserts
                rightEntries = insertsModel rightInserts
            asked <- newIORef []
            let holds pairLeft pairRight = do
                  modifyIORef' asked ((pairLeft, pairRight) :)
                  (/= failAt) . length <$> readIORef asked
                pairs = zip [(name, Thunk value) | (name, _, value) <- leftEntries] [(name, Thunk value) | (name, _, value) <- rightEntries]
            answer <- attrSetAllPairs holds (AttrSet left) (AttrSet right)
            askedPairs <- reverse <$> readIORef asked
            pure $
              (answer, askedPairs)
                QC.=== (failAt > length pairs && length leftEntries == length rightEntries, take failAt pairs)
    ]
  where
    -- A new cell per call, so a lookup's answer identifies the entry it found.
    textCell text = unThunk <$> evaluated (mkStr text)
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    entryPositions set = Map.fromList . map (\(name, PosIdx pos, _) -> (name, pos)) <$> walkedEntries set
    genValuedInserts pool values = QC.listOf ((,,) <$> QC.elements pool <*> (PosIdx <$> QC.arbitrary) <*> QC.elements values)
    buildValued inserts = do
      set <- cattrsetNew SetLiteral 0
      mapM_ (\(name, pos, value) -> cattrsetInsert set name pos value) inserts
      cattrsetFreeze set
      pure set
    -- The entries a set built from these inserts holds: the last insert
    -- of each name, in symbol order.
    insertsModel inserts = [(name, pos, value) | (name, (pos, value)) <- Map.toAscList (Map.fromList [(name, (pos, value)) | (name, pos, value) <- inserts])]
    walkedEntries set = cattrsetWalk set >>= walkFrom
    walkFrom walk =
      cattrsetWalkNext walk >>= \case
        CAttrSetDone -> pure []
        CAttrSetEntry name pos value rest -> ((name, pos, value) :) <$> walkFrom rest

-- ---------------------------------------------------------------------------
-- C thunk arena (FFI)
-- ---------------------------------------------------------------------------

testCThunk :: IO [Bool]
testCThunk = do
  putStrLn "cthunk"
  -- Arena is already initialized by main's bracket.
  -- Each test uses the shared arena (thunks accumulate - that's fine).
  sequence
    [ runTestM "new pending + state" $ do
        ptr <- cthunkNewBc 0 pendingEnv
        state <- cthunkState ptr
        pure (assertEqual "state" 0 state),
      runTestM "new computed + state" $ do
        ptr <- cthunkNewComputedInt 7
        state <- cthunkState ptr
        pure (assertEqual "state" 1 state),
      runTestM "payload round-trips (pending)" $ do
        ptr <- cthunkNewBc 0 pendingEnv
        payload <- cthunkPayload ptr
        pure (assertEqual "payload" pendingEnv payload),
      runTestM "an inline value round-trips (computed)" $ do
        ptr <- cthunkNewComputedInt 42
        val <- cthunkGetInt ptr
        pure (assertEqual "value" 42 val),
      runTestM "mark_blackhole succeeds on pending" $ do
        ptr <- cthunkNewBc 0 pendingEnv
        ok <- cthunkMarkBlackhole ptr
        state <- cthunkState ptr
        pure
          ( if ok && state == 2
              then Pass
              else Fail ("ok=" <> T.pack (show ok) <> " state=" <> T.pack (show state))
          ),
      runTestM "mark_blackhole fails on computed" $ do
        ptr <- cthunkNewComputedInt 1
        ok <- cthunkMarkBlackhole ptr
        pure (if not ok then Pass else Fail "should have failed"),
      runTestM "mark_blackhole fails on blackhole" $ do
        ptr <- cthunkNewBc 0 pendingEnv
        _ <- cthunkMarkBlackhole ptr
        ok <- cthunkMarkBlackhole ptr
        pure (if not ok then Pass else Fail "should have failed"),
      runTestM "set_computed stores into a blackhole" $ do
        ptr <- cthunkNewBc 0 pendingEnv
        _ <- cthunkMarkBlackhole ptr
        stored <- cthunkSetComputedInt ptr 5
        state <- cthunkState ptr
        newVal <- cthunkGetInt ptr
        pure
          ( if stored && newVal == 5 && state == 1
              then Pass
              else Fail ("stored=" <> T.pack (show stored) <> " new=" <> T.pack (show newVal) <> " state=" <> T.pack (show state))
          ),
      -- set_computed accepts PENDING (direct memoization, no blackhole step)
      runTestM "set_computed stores into a pending thunk" $ do
        ptr <- cthunkNewBc 0 pendingEnv
        stored <- cthunkSetComputedInt ptr 3
        pure (if stored then Pass else Fail "refused a PENDING thunk"),
      runTestM "set_computed refuses a computed thunk and keeps its value" $ do
        ptr <- cthunkNewComputedInt 1
        stored <- cthunkSetComputedInt ptr 2
        kept <- cthunkGetInt ptr
        pure
          ( if not stored && kept == 1
              then Pass
              else Fail ("stored=" <> T.pack (show stored) <> " kept=" <> T.pack (show kept))
          ),
      -- The shared cells are one static each for the whole process, so a
      -- transition that ran on one would change every null, boolean or
      -- builtin bound under its own name.
      runTestM "the shared null, boolean and builtin cells refuse every transition" $ do
        builtinCells <- mapM (cthunkPrimop . builtinIndex) [Builtin.Abort, Builtin.ZipAttrsWith]
        let cells = [cthunkNull, cthunkBool True, cthunkBool False] ++ builtinCells
        marked <- mapM cthunkMarkBlackhole cells
        stored <- mapM cthunkSetComputedNull cells
        values <- mapM (readThunkValue . Thunk) cells
        let expected = map Just [VNull, VBool True, VBool False, VBuiltin Builtin.Abort [], VBuiltin Builtin.ZipAttrsWith []]
        pure
          ( if not (or (marked ++ stored)) && values == expected
              then Pass
              else Fail ("accepted=" <> T.pack (show (marked, stored)) <> " values=" <> T.pack (show values))
          ),
      -- Past the arena's first block, so allocation chains new ones and
      -- every cell keeps its own value across them.
      runTestM "stress: 100k thunks" $ do
        let values = [1 .. 100000]
        cells <- mapM cthunkNewComputedInt values
        held <- mapM cthunkGetInt cells
        pure (assertEqual "cells holding their own value" (length values) (length (filter id (zipWith (==) held values)))),
      runTestM "every builtin's own cell holds that builtin, with no argument taken" $ do
        held <- mapM (\builtin -> (,) builtin <$> (cthunkPrimop (builtinIndex builtin) >>= readThunkValue . Thunk)) [minBound .. maxBound]
        let wrong = [builtinName builtin | (builtin, value) <- held, value /= Just (VBuiltin builtin [])]
        pure (if null wrong then Pass else Fail ("wrong cells: " <> T.intercalate ", " wrong)),
      runTestM "a builtin value a cell holds reads back as itself, each argument as it was taken" $
        quickCheckPasses $
          QC.forAll genBuiltinValue $ \(builtin, arguments) -> QC.ioProperty $ do
            taken <- traverse takenArgument arguments
            let value = VBuiltin builtin taken
            stored <- evaluated value >>= readThunkValue
            pure (stored QC.=== Just value),
      -- Each binding's thunk memoizes a partial application in its cell:
      -- f holds a function taken forced, g also an argument held as a
      -- thunk, m a pattern, and t is a builtin with nothing taken.  Each
      -- is read back from its cell at every call.
      runTestM "a partial application a binding memoizes is applied as it was taken" $ do
        tmpBase <- getTemporaryDirectory
        result <-
          evalNixIO
            tmpBase
            "let f = builtins.foldl' (a: b: a + b); g = f 10; m = builtins.match \"a(.)\"; t = builtins.tryEval; in [ (g [ 1 2 ]) (g [ ]) (f 0 [ 5 ]) (m \"ab\") (m \"x\") (t (throw \"x\")).success ] == [ 13 10 5 [ \"b\" ] null false ]"
        pure (assertEqual "result" (Right (VBool True)) result),
      runTestM "a handle equals only the C object it names" $ do
        one <- evaluated (VInt 1)
        copy <- evaluated (VInt 1)
        set <- attrSetFromMap SetLiteral (Map.singleton "a" one)
        setCopy <- attrSetFromMap SetLiteral (Map.singleton "a" one)
        list <- clistFromThunks [unThunk one]
        listCopy <- clistFromThunks [unThunk one]
        env <- newEnv EnvClosure Nothing [one]
        envCopy <- newEnv EnvClosure Nothing [one]
        found <- attrSetLookup "a" set
        storedSet <- evaluated (VAttrs set) >>= readThunkValue
        storedList <- evaluated (VList list) >>= readThunkValue
        pure
          ( assertEqual "read back from C" (Just one, Just (VAttrs set), Just (VList list)) (found, storedSet, storedList)
              `andThen` assertEqual "a copy" (False, False, False, False) (one == copy, set == setCopy, list == listCopy, env == envCopy)
          ),
      -- No C object lives at the null address, so a show that read one
      -- would fault here.
      runTest "a handle shows as its address without reading C memory" $
        let address = show (nullPtr :: Ptr ())
         in assertEqual
              "shown"
              ("Thunk {unThunk = " <> address <> "}", "AttrSet {unAttrSet = " <> address <> "}", "CList {unCList = " <> address <> "}", "Env " <> address)
              (show (Thunk nullPtr), show (AttrSet nullPtr), show (CList nullPtr), show (Env nullPtr))
    ]
  where
    -- These cells are never forced, so the shared empty env serves as the
    -- env their code would run in.
    pendingEnv = case emptyEnv of Env envPtr -> castPtr envPtr
    takenArgument (Left n) = pure (ArgForced (VInt n))
    takenArgument (Right n) = ArgDeferred . Thunk <$> cthunkNewComputedInt n
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | A builtin and the arguments a partial application of it holds: each
-- an integer taken forced ('Left') or held in a cell of its own
-- ('Right').  Up to two, the most any builtin's partial application
-- takes, and any builtin, since a cell holds what it is given.
genBuiltinValue :: QC.Gen (Builtin, [Either Int64 Int64])
genBuiltinValue = do
  builtin <- QC.elements [minBound .. maxBound]
  count <- QC.chooseInt (0, 2)
  arguments <- QC.vectorOf count (QC.oneof [Left <$> QC.arbitrary, Right <$> QC.arbitrary])
  pure (builtin, arguments)

-- ---------------------------------------------------------------------------
-- Bytecode shared across elements
-- ---------------------------------------------------------------------------

-- | The instructions and data words evaluating a source adds to the
-- bytecode store, which only the session's end frees.  The source must
-- evaluate to an integer computed from everything it builds, so each
-- element has been forced when the counts are read.
bytecodeGrowth :: Text -> IO (Either Text (Word32, Word32))
bytecodeGrowth source = do
  opsBefore <- cbcOpCount
  dataBefore <- cbcDataCount
  forced <- case evalNix source of
    Right (VInt n) -> Right <$> evaluate n
    Right other -> pure (Left ("expected an integer, got " <> T.pack (show other)))
    Left err -> pure (Left err)
  opsAfter <- cbcOpCount
  dataAfter <- cbcDataCount
  pure ((opsAfter - opsBefore, dataAfter - dataBefore) <$ forced)

-- | A source builds one element or two hundred, and the bytecode it adds
-- is the same: the code each element runs is shared, never compiled per
-- element.  A first run at one element is discarded, so anything compiled
-- once per process is not counted against either size.
assertNoPerElementBytecode :: (Text -> Text) -> IO TestResult
assertNoPerElementBytecode sourceFor = do
  _ <- bytecodeGrowth (sourceFor "1")
  one <- bytecodeGrowth (sourceFor "1")
  many <- bytecodeGrowth (sourceFor "200")
  pure $ case (one, many) of
    (Right atOne, Right atMany) ->
      assertEqual "instructions and data words added at 1 element and at 200" atOne atMany
    (Left err, _) -> Fail err
    (_, Left err) -> Fail err

testBytecodePerElement :: IO [Bool]
testBytecodePerElement = do
  putStrLn "bytecode/per-element"
  let summed body n = "builtins.foldl' (a: b: a + b) 0 (" <> body n <> ")"
      indices n = "builtins.genList (i: i) " <> n
  sequence
    [ runTestM "map adds no bytecode per element" $
        assertNoPerElementBytecode (summed (\n -> "builtins.map (x: x + 1) (" <> indices n <> ")")),
      runTestM "genList adds no bytecode per element" $
        assertNoPerElementBytecode (summed ("builtins.genList (i: i * 2) " <>)),
      runTestM "concatMap adds no bytecode per element" $
        assertNoPerElementBytecode (summed (\n -> "builtins.concatMap (x: [ x x ]) (" <> indices n <> ")")),
      runTestM "mapAttrs adds no bytecode per attribute" $
        assertNoPerElementBytecode
          ( summed
              (\n -> "builtins.attrValues (builtins.mapAttrs (name: v: v + 1) (builtins.listToAttrs (builtins.genList (i: { name = toString i; value = i; }) " <> n <> ")))")
          ),
      runTestM "zipAttrsWith adds no bytecode per attribute" $
        assertNoPerElementBytecode
          ( summed
              (\n -> "builtins.attrValues (builtins.zipAttrsWith (name: vs: builtins.length vs) (builtins.genList (i: { ${toString i} = i; }) " <> n <> "))")
          ),
      runTestM "inherit (from) in an attr set adds no bytecode per evaluation" $
        assertNoPerElementBytecode (summed ("builtins.genList (i: { inherit ({ v = i; }) v; }.v) " <>)),
      runTestM "inherit (from) in a recursive attr set adds no bytecode per evaluation" $
        assertNoPerElementBytecode (summed ("builtins.genList (i: rec { inherit ({ v = i; }) v; }.v) " <>)),
      runTestM "inherit (from) in a let adds no bytecode per evaluation" $
        assertNoPerElementBytecode (summed ("builtins.genList (i: let inherit ({ v = i; }) v; in v) " <>))
    ]

-- | Bytecode short_arg spill: op-level payload counts at or above the
-- 0xFFFF sentinel move to the first data word, so literals are not
-- capped at 65535 elements.  The two exact-boundary list cases pin the
-- inline maximum (65534) and the first spilled count (65535); the rest
-- drive each counted-op kind (list, attrs, let, string parts, attr path)
-- well past the inline maximum.
testBytecodeCountSpill :: IO [Bool]
testBytecodeCountSpill = do
  putStrLn "bytecode/count-spill"
  let listOf n = "builtins.length [ " <> T.unwords (replicate n "1") <> " ]"
      bigAttrsBody = T.concat [T.pack ("a" <> show i <> " = " <> show i <> "; ") | i <- [0 :: Int .. 69999]]
  sequence
    [ runTest "list literal at the inline count maximum (65534)" $
        assertEval "spill-list-inline-max" (listOf 65534) (VInt 65534),
      runTest "list literal at the first spilled count (65535)" $
        assertEval "spill-list-first-spill" (listOf 65535) (VInt 65535),
      runTest "spilled list preserves element order" $
        assertEval
          "spill-list-order"
          ("builtins.elemAt [ " <> T.unwords (map (T.pack . show) [0 :: Int .. 69999]) <> " ] 69999")
          (VInt 69999),
      runTest "spilled attrset literal binds every attribute" $
        assertEval
          "spill-attrs-count"
          ("builtins.length (builtins.attrNames { " <> bigAttrsBody <> "})")
          (VInt 70000),
      runTest "spilled attrset lookup reads the right value" $
        assertEval
          "spill-attrs-lookup"
          ("{ " <> bigAttrsBody <> "}.a69999")
          (VInt 69999),
      runTest "spilled let binds every name" $
        assertEval
          "spill-let"
          ("let " <> T.concat [T.pack ("v" <> show i <> " = " <> show i <> "; ") | i <- [0 :: Int .. 69999]] <> "in v69999")
          (VInt 69999),
      runTest "spilled interpolated string keeps every part" $
        assertEval
          "spill-string-parts"
          ("builtins.stringLength \"" <> T.concat (replicate 70000 "${\"x\"}") <> "\"")
          (VInt 70000),
      runTest "spilled attr path walks (set ? long.path)" $
        assertEval
          "spill-attrpath"
          ("{ } ? " <> T.intercalate "." (map (\i -> T.pack ("p" <> show i)) [0 :: Int .. 69999]))
          (VBool False)
    ]

-- | C value structs carry element counts as a full uint32; a narrower
-- count would wrap at 65536 and size the C array smaller than the fill
-- loop that follows it.  Both cases marshal counts past that boundary
-- and force the value twice - the first force writes the C struct (the
-- marshal side), the second reads it back off the computed thunk cell
-- (the unmarshal side).
testValueCountWidths :: IO [Bool]
testValueCountWidths = do
  putStrLn "value/count-widths"
  let formalsBody = T.intercalate ", " [T.pack ("a" <> show i) | i <- [0 :: Int .. 69999]]
  sequence
    [ runTest "lambda with 70000 formals round-trips through the C closure" $
        assertEval
          "width-lambda-formals"
          ( "let f = { "
              <> formalsBody
              <> " }: 1; in builtins.seq (builtins.functionArgs f) (builtins.length (builtins.attrNames (builtins.functionArgs f)))"
          )
          (VInt 70000),
      runTest "string context with 70000 elements round-trips through the C thunk" $
        assertEval
          "width-ctxstr-elems"
          ( "let s = builtins.appendContext \"x\" (builtins.listToAttrs (builtins.genList (i: { "
              <> "name = \"/nix/store/00000000000000000000000000000000-p\" + builtins.toString i; "
              <> "value = { path = true; }; }) 70000)); "
              <> "in builtins.seq s (builtins.length (builtins.attrNames (builtins.getContext s)))"
          )
          (VInt 70000)
    ]

-- ---------------------------------------------------------------------------
-- Bytecode tags: what the compiler writes decodes back, and nothing else does
-- ---------------------------------------------------------------------------

-- | Where an operator's tag pair fails to be inverses: each operator whose
-- tag decodes to something else, then each byte that decodes to an
-- operator written as another byte.  Every constructor and every byte is
-- checked, as both domains are finite.
operatorTagMismatches :: (Bounded op, Enum op, Eq op, Show op) => (op -> Word8) -> (Word8 -> Maybe op) -> [Text]
operatorTagMismatches encode decode =
  [ T.pack (show op) <> " reads back from its tag as " <> T.pack (show (decode (encode op)))
  | op <- [minBound .. maxBound],
    decode (encode op) /= Just op
  ]
    ++ [ "byte " <> T.pack (show tag) <> " reads as " <> T.pack (show op) <> ", which is written as " <> T.pack (show (encode op))
       | tag <- [minBound .. maxBound],
         Just op <- [decode tag],
         encode op /= tag
       ]

-- | A compiled child read back as the expression the round trips below
-- compile it from: an integer literal or a resolved variable.  Any other
-- opcode reads as a variable naming it, which nothing generated equals.
childExpr :: Word32 -> IO Expr
childExpr idx = do
  op <- cbcOpcode idx
  lo <- cbcArg1 idx
  hi <- cbcArg2 idx
  pure $ case op of
    OpLitInt -> ELit (NixInt (reassembleInt64 lo hi))
    OpResolvedVar -> EResolvedVar (fromIntegral lo) (fromIntegral hi)
    _ -> EVar ("<opcode " <> T.pack (show op) <> ">")

-- | A recorded position read back in the test origin.  One that does not
-- read back as text reads as line 0, which nothing generated has.
srcPosBack :: PosIdx -> IO SrcPos
srcPosBack idx =
  lookupPos idx <&> \case
    Just (PosInText line column) -> testPos line column
    _ -> testPos 0 0

-- | Decoded formals read back as the parser's formals they compile from.
formalsBack :: EvalFormals -> IO Formals
formalsBack formals = case formals of
  EFName name -> pure (FormalName (symbolText name))
  EFSet _ entries ellipsis -> do
    back <- traverse formalBack entries
    pure (FormalSet back ellipsis)
  EFNamedSet _ name entries ellipsis -> do
    back <- traverse formalBack entries
    pure (FormalNamedSet (symbolText name) back ellipsis)
  where
    formalBack (EvalFormal name def) = Formal (symbolText name) <$> traverse childExpr (defaultCode def)
    defaultCode = \case
      NoDefault -> Nothing
      Default code -> Just code
      SharedDefault code -> Just code

-- | A binding read from the bytecode read back as the binding it compiles
-- from.  An inherited name's select is the compiler's own and has no
-- counterpart.
bindingBack :: BcBinding -> IO Binding
bindingBack binding = case binding of
  BcNamed pos sym valIdx -> NamedBinding <$> srcPosBack pos <*> pure (StaticKey (symbolText (Symbol sym))) <*> childExpr valIdx
  BcDynamic pos keyIdx valIdx -> NamedBinding <$> srcPosBack pos <*> (DynamicKey <$> childExpr keyIdx) <*> childExpr valIdx
  BcInherit pos sym valIdx -> Inherit <$> srcPosBack pos <*> pure (symbolText (Symbol sym)) <*> childExpr valIdx
  BcInheritFrom fromIdx names -> do
    from <- childExpr fromIdx
    inherited <- inheritedNameList names
    InheritFrom from <$> sequence [(`InheritedName` symbolText (Symbol sym)) <$> srcPosBack pos | BcInheritedName pos sym _ <- inherited]

-- | Capture info read from the bytecode read back as the capture info it
-- compiles from.
capturesBack :: BcCaptureInfo -> IO CaptureInfo
capturesBack info = case info of
  BcNoCaptureInfo -> pure NoCaptureInfo
  BcCaptures slots -> Captures <$> capturedPairs slots
  BcCapturesWithScopes slots -> CapturesWithScopes <$> capturedPairs slots
  where
    capturedPairs slots = reverse <$> foldBcCapturedSlots (\acc level idx -> pure ((fromIntegral level, fromIntegral idx) : acc)) [] slots

-- | Bindings in source order, or the first binding tag the compiler never
-- writes.
bindingList :: BcBindings -> IO (Either Word32 [BcBinding])
bindingList = fmap (fmap reverse) . foldBcBindings (pure . Left) (\acc binding -> pure ((binding :) <$> acc)) (Right [])

-- | The names of an @inherit (from)@ binding in source order.
inheritedNameList :: BcInheritedNames -> IO [BcInheritedName]
inheritedNameList = fmap reverse . foldBcInheritedNames (\acc name -> pure (name : acc)) []

-- | A closure keeps its formals type, ellipsis flag and formal count in
-- one word (cbits/nn_lambda.h): the type in the top two bits, the flag
-- below it and the count in the 29 bits left.  Each field reads back as
-- packed at both ends of its range, and a count the word cannot hold is
-- refused before anything is allocated.
testLambdaShape :: IO [Bool]
testLambdaShape = do
  putStrLn "cbits/lambda-shape"
  let maxFormals = clambdaShapeFormalCount maxBound
      unpack shape = (clambdaShapeFormalsType shape, clambdaShapeAllowExtra shape, clambdaShapeFormalCount shape)
      fields = [(formalsType, flag, count) | formalsType <- [0, 1, 2], flag <- [0, 1], count <- [0, 1, maxFormals - 1, maxFormals]]
  sequence
    [ runTest "the count takes the 29 bits below the flag" $
        assertEqual "largest count" (2 ^ (29 :: Int) - 1) maxFormals,
      runTest "every type and flag with a count at either end of its range reads back as packed" $
        assertEqual "mismatches" [] [packed | packed@(formalsType, flag, count) <- fields, unpack (clambdaShape formalsType flag count) /= packed],
      runTest "a nonzero flag packs as 1" $
        assertEqual "flag" 1 (clambdaShapeAllowExtra (clambdaShape 2 255 0)),
      runTestM "a closure reads back the shape it was made with" $ do
        lam <- clambdaNew nullPtr 0 2 0 0 1 3
        formalsType <- clambdaFormalsType lam
        flag <- clambdaAllowExtra lam
        count <- clambdaFormalCount lam
        pure (assertEqual "shape" (2, 1, 3) (formalsType, flag, count)),
      runTestM "a count the shape cannot hold is refused" $ do
        outcomes <-
          mapM
            try
            [ clambdaNew nullPtr 0 1 0 0 0 (maxFormals + 1),
              clambdaNew nullPtr 0 1 0 0 0 maxBound
            ]
        let site = \case
              Left (CAllocationFailed at _) -> Just at
              _ -> Nothing
        pure (assertEqual "refusals" (replicate 2 (Just "nn_lambda_new")) (map site outcomes))
    ]

-- | A closure's formals type, a formal's default kind and a string
-- context element's tag each hold one of the values the C side names
-- (@NN_FORMALS_*@, the kinds cbits/nn_lambda.h lists for a formal entry,
-- and @NN_SCE_*@).  Only memory written past the library holds another,
-- so each test writes one in place, at the field's offset in the struct
-- cbits/nn_lambda.h or cbits/nn_ctxstr.h lays out, and restores the
-- field once the read has refused it.
testUnknownTags :: IO [Bool]
testUnknownTags = do
  putStrLn "cbits/unknown-tags"
  sequence
    [ runTestM "a closure whose shape holds no formals type raises CInvariantViolated" $ do
        name <- symbolIntern "x"
        cell <- evaluated (VLambda emptyEnv (EFName name) 0)
        lam <- cthunkPayload (unThunk cell)
        outcome <- withFieldAt lam lambdaShapeOffset (clambdaShape unnamedTag 0 0) (readComputed (unThunk cell))
        pure (assertEqual "site" (Just "nn_lambda_formals_type") (violatedAt outcome)),
      runTestM "a closure whose formal holds no default kind raises CInvariantViolated" $ do
        name <- symbolIntern "x"
        cell <- evaluated (VLambda emptyEnv (EFSet AnonymousLambda [EvalFormal name NoDefault] False) 0)
        lam <- cthunkPayload (unThunk cell)
        entries <- peekByteOff lam pointerBytes
        outcome <- withFieldAt entries entryKindOffset (unknownTag :: Word32) (readComputed (unThunk cell))
        pure (assertEqual "site" (Just "nn_lambda_entry_kind") (violatedAt outcome)),
      runTestM "a string context element with no known tag raises CInvariantViolated" $ do
        cell <- evaluated (VStr "ztext" (StringContext (Set.singleton (SCPlain (StorePath censusStoreHash "zname")))))
        ctxstr <- cthunkPayload (unThunk cell)
        outcome <- withFieldAt ctxstr firstElementOffset unnamedTag (readComputed (unThunk cell))
        pure (assertEqual "site" (Just "nn_ctxstr_elem_tag") (violatedAt outcome))
    ]
  where
    -- No NN_FORMALS_* or NN_SCE_* tag is 3, the one value of a shape's
    -- two bits of formals type the tags leave unnamed.
    unnamedTag = 3 :: Word8
    pointerBytes = sizeOf (nullPtr :: Ptr ())
    -- An nn_lambda_t's shape follows its two pointers and three words,
    -- and its entries are its second pointer.
    lambdaShapeOffset = 2 * pointerBytes + 3 * sizeOf (0 :: Word32)
    -- A formal entry's kind follows its name's symbol.
    entryKindOffset = sizeOf (0 :: Word32)
    -- An nn_ctxstr_t's elements follow its text pointer and its count,
    -- padded to their pointer alignment, and an element's tag is its
    -- first byte.
    firstElementOffset = 2 * pointerBytes
    withFieldAt :: (Storable a) => Ptr () -> Int -> a -> IO b -> IO (Either CStatusError b)
    withFieldAt ptr offset value act = do
      original <- peekByteOff ptr offset
      pokeByteOff ptr offset (value `asTypeOf` original)
      try act `finally` pokeByteOff ptr offset original
    violatedAt = \case
      Left (CInvariantViolated site _) -> Just site
      _ -> Nothing

testBytecodeTags :: IO [Bool]
testBytecodeTags = do
  putStrLn "bytecode/tags"
  sequence
    [ runTest "every unary operator's tag reads back as it, and no other byte reads as an operator" $
        assertEqual "mismatches" [] (operatorTagMismatches encodeUnaryOp decodeUnaryOp),
      runTest "every binary operator's tag reads back as it, and no other byte reads as an operator" $
        assertEqual "mismatches" [] (operatorTagMismatches encodeBinaryOp decodeBinaryOp),
      runTestM "a compiled lambda's formals and capture info read back as compiled" . quickCheckPasses $
        QC.forAll ((,) <$> genFormals <*> genCaptureInfo) $ \(formals, captures) -> QC.ioProperty $ do
          idx <- compileExpr (ELambda formals (ELit NixNull) captures)
          flags <- cbcFlags idx
          formalsOff <- cbcArg1 idx
          captureOff <- cbcArg3 idx
          decodedFormals <- decodeFormals flags formalsOff >>= traverse formalsBack
          decodedCaptures <- decodeBcCaptureInfo captureOff >>= traverse capturesBack
          pure ((decodedFormals, decodedCaptures) QC.=== (Just formals, Right captures)),
      runTestM "a lambda stored in a cell reads back with the symbols it was stored with" . quickCheckPasses $
        QC.forAll ((,) <$> genFormals <*> QC.oneof [pure Nothing, Just <$> genName]) $ \(formals, binding) -> QC.ioProperty $ do
          name <- maybe (pure AnonymousLambda) (fmap NamedLambda . symbolIntern) binding
          lambda <- (\stored -> VLambda emptyEnv stored 0) . namedBy name <$> compileFormalsToEval formals
          cell <- evaluated lambda
          (QC.=== lambda) <$> readComputed (unThunk cell),
      -- Upstream's addAttr names the function an attribute path's last
      -- name binds, and ExprLambda::setName the function that is its body
      -- (parser-state.hh:176-177, nixexpr.cc:573-577 at 2.24.9).
      runTestM "a set pattern bound to a static name reads back named, as does the one its body is" $ do
        let inner = ELambda (FormalSet [Formal "a" Nothing] False) (ELit NixNull) NoCaptureInfo
            outer = ELambda (FormalNamedSet "args" [] True) inner NoCaptureInfo
            boundBy key = ELet [NamedBinding (testPos 1 1) key outer] (ELit NixNull) NoCaptureInfo
            -- The two names a binding's lambda and its body decode with.
            lambdaNames expr = do
              bindings <- compileExpr expr >>= compiledBindings >>= bindingList
              case bindings of
                Right [binding]
                  | Just outerIdx <- boundValue binding -> do
                      outerFormals <- formalsAt outerIdx
                      innerFormals <- formalsAt =<< cbcArg2 outerIdx
                      pure (lambdaNameOf =<< outerFormals, lambdaNameOf =<< innerFormals)
                _ -> pure (Nothing, Nothing)
            -- The value a static or a dynamic name is bound to.
            boundValue = \case
              BcNamed _ _ valIdx -> Just valIdx
              BcDynamic _ _ valIdx -> Just valIdx
              _ -> Nothing
            formalsAt idx = do
              flags <- cbcFlags idx
              decodeFormals flags =<< cbcArg1 idx
            lambdaNameOf = \case
              EFSet name _ _ -> Just name
              EFNamedSet name _ _ _ -> Just name
              EFName _ -> Nothing
        named <- lambdaNames (boundBy (StaticKey "f"))
        dynamic <- lambdaNames (boundBy (DynamicKey (ELit (NixInt 0))))
        bare <- formalsAt =<< compileExpr inner
        f <- NamedLambda <$> symbolIntern "f"
        pure $
          assertEqual "static" (Just f, Just f) named
            `andThen` assertEqual "dynamic" (Just AnonymousLambda, Just AnonymousLambda) dynamic
            `andThen` assertEqual "unbound" (Just AnonymousLambda) (lambdaNameOf =<< bare),
      runTestM "a compiled let's bindings, their counts and its capture info read back as compiled" . quickCheckPasses $
        QC.forAll ((,) <$> QC.listOf genBinding <*> genCaptureInfo) $ \(bindings, captures) -> QC.ioProperty $ do
          idx <- compileExpr (ELet bindings (ELit NixNull) captures)
          compiled@(BcBindings (BcBindingCounts names dynamics) _ _ _) <- compiledBindings idx
          decodedBindings <- bindingList compiled >>= traverse (traverse bindingBack)
          decodedCaptures <- cbcArg3 idx >>= decodeBcCaptureInfo >>= traverse capturesBack
          pure ((decodedBindings, (names, dynamics), decodedCaptures) QC.=== (Right bindings, nameCounts bindings, Right captures))
    ]
  where
    -- A set pattern under the name a binding gives it.
    namedBy name = \case
      EFSet _ entries ellipsis -> EFSet name entries ellipsis
      EFNamedSet _ arg entries ellipsis -> EFNamedSet name arg entries ellipsis
      plain@(EFName _) -> plain
    -- The names bindings bind statically, one per static key, inherited
    -- variable and inherited name, and the bindings with a dynamic name.
    nameCounts bindings = (sum (map staticNames bindings), length [() | NamedBinding _ (DynamicKey _) _ <- bindings])
    staticNames = \case
      NamedBinding _ (StaticKey _) _ -> 1
      NamedBinding _ (DynamicKey _) _ -> 0
      Inherit {} -> 1
      InheritFrom _ inherited -> length inherited
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    genName = QC.elements ["a", "b", "x", "self", "args"]
    genChild =
      QC.oneof
        [ ELit . NixInt <$> QC.arbitrary,
          EResolvedVar <$> QC.chooseInt (0, 8) <*> QC.chooseInt (0, 64)
        ]
    genPos = testPos <$> QC.chooseInt (1, 4096) <*> QC.chooseInt (1, 4096)
    genCaptureInfo =
      let pairs = QC.listOf ((,) <$> QC.chooseInt (0, 64) <*> QC.chooseInt (0, 4096))
       in QC.oneof [pure NoCaptureInfo, Captures <$> pairs, CapturesWithScopes <$> pairs]
    genFormals =
      let entries = QC.listOf (Formal <$> genName <*> QC.oneof [pure Nothing, Just <$> genChild])
       in QC.oneof
            [ FormalName <$> genName,
              FormalSet <$> entries <*> QC.arbitrary,
              FormalNamedSet <$> genName <*> entries <*> QC.arbitrary
            ]
    genBinding =
      QC.oneof
        [ NamedBinding <$> genPos <*> QC.oneof [StaticKey <$> genName, DynamicKey <$> genChild] <*> genChild,
          Inherit <$> genPos <*> genName <*> genChild,
          InheritFrom <$> genChild <*> QC.listOf (InheritedName <$> genPos <*> genName)
        ]

-- | A tag none of the compiler's encoders writes, in any of the fields the
-- corrupt-bytecode tests put it in.
unknownTag :: (Num a) => a
unknownTag = 200

-- | Whether hand-built code at a bytecode index fails to evaluate with the
-- corrupt-bytecode error for @what@ holding 'unknownTag'.
assertCorruptBytecode :: Text -> Word32 -> IO TestResult
assertCorruptBytecode what = assertCorruptBytecodeAt what unknownTag

-- | Whether hand-built code at a bytecode index fails to evaluate with the
-- corrupt-bytecode error for @what@ holding @value@.  The outcome, and a
-- value it holds (which the failure message shows), are forced inside the
-- action, so an exception a decoder raises instead, as the evaluator runs
-- or once the value is inspected, reaches 'runTestM' as a failure rather
-- than escaping after it.
assertCorruptBytecodeAt :: Text -> Int -> Word32 -> IO TestResult
assertCorruptBytecodeAt what value idx = do
  outcome <- evaluate (runPureEval (evalBytecode emptyEnv idx))
  evaluate $ case outcome of
    Left err -> assertEqual "error" ("corrupt bytecode: unknown " <> what <> " " <> T.pack (show value)) err
    Right val -> Fail ("expected the corrupt-bytecode error but got: " <> T.pack (show val))

testBytecodeCorrupt :: IO [Bool]
testBytecodeCorrupt = do
  putStrLn "bytecode/corrupt"
  sequence
    [ runTestM "an unknown opcode is a corrupt-bytecode error" $
        cbcEmit unknownTag 0 0 0 0 0 >>= assertCorruptBytecode "opcode",
      runTestM "an unknown unary operator tag is a corrupt-bytecode error" $ do
        operand <- literalOne
        cbcEmit OpUnary unknownTag 0 operand 0 0 >>= assertCorruptBytecode "unary operator tag",
      runTestM "an unknown binary operator tag is a corrupt-bytecode error" $ do
        left <- literalOne
        right <- literalOne
        cbcEmit OpBinary unknownTag 0 left right 0 >>= assertCorruptBytecode "binary operator tag",
      runTestM "an unknown formals type is a corrupt-bytecode error" $
        lambdaWith unknownTag captureNone >>= assertCorruptBytecode "formals type",
      runTestM "an unknown capture info tag in a lambda is a corrupt-bytecode error" $
        lambdaWith formalName unknownTag >>= assertCorruptBytecode "capture info tag",
      runTestM "an unknown capture info tag in a recursive attribute set is a corrupt-bytecode error" $ do
        bindingsOff <- bindingWords []
        captureOff <- cbcEmitData unknownTag
        cbcEmit OpAttrs 1 0 bindingsOff captureOff 0 >>= assertCorruptBytecode "capture info tag",
      runTestM "an unknown binding tag in an attribute set is a corrupt-bytecode error" $ do
        bindingsOff <- bindingWords [unknownTag]
        cbcEmit OpAttrs 0 1 bindingsOff 0 0 >>= assertCorruptBytecode "binding tag",
      runTestM "an unknown attribute set flag is a corrupt-bytecode error" $ do
        bindingsOff <- bindingWords []
        cbcEmit OpAttrs unknownTag 0 bindingsOff 0 0 >>= assertCorruptBytecode "attribute set flag",
      runTestM "an unknown binding tag in a let is a corrupt-bytecode error" $ do
        bindingsOff <- bindingWords [unknownTag]
        captureOff <- cbcEmitData captureNone
        body <- literalOne
        cbcEmit OpLet 0 1 bindingsOff body captureOff >>= assertCorruptBytecode "binding tag",
      -- A let of one slot, which no binding fills, with one share.
      runTestM "a share naming a slot past its frame is a corrupt-bytecode error" $ do
        let sharing slot target = do
              bindingsOff <- dataWords 1 [0, 1, slot, target]
              captureOff <- cbcEmitData captureNone
              body <- literalOne
              cbcEmit OpLet 0 0 bindingsOff body captureOff
        sharer <- sharing unknownTag 0 >>= assertCorruptBytecode "share slot"
        shared <- sharing 0 unknownTag >>= assertCorruptBytecode "share slot"
        pure (sharer `andThen` shared),
      -- A recursive set whose counts give it a dynamic name, so its names
      -- are in a name table, and whose one binding is a static name.
      runTestM "a share naming a symbol its frame does not bind is a corrupt-bytecode error" $ do
        Symbol bound <- symbolIntern "zcorruptBound"
        let sharing name target = do
              value <- literalOne
              bindingsOff <- dataWords 1 [1, 1, name, target, bindNamed, attrkeyStatic, bound, value, 0]
              captureOff <- cbcEmitData captureNone
              cbcEmit OpAttrs attrsRec 1 bindingsOff captureOff 0
        sharer <- sharing unknownTag bound >>= assertCorruptBytecode "share symbol"
        shared <- sharing bound unknownTag >>= assertCorruptBytecode "share symbol"
        pure (sharer `andThen` shared),
      runTestM "an unknown formal default kind is a corrupt-bytecode error" $ do
        Symbol name <- symbolIntern "zcorruptFormal"
        body <- literalOne
        formalsOff <- dataWords invalidSymbolId [1, 0, name, unknownTag, body]
        captureOff <- cbcEmitData captureNone
        cbcEmit OpLambda formalSet 0 formalsOff body captureOff >>= assertCorruptBytecode "formal default kind",
      -- Two formals whose defaults each share the other's slot, called
      -- with an argument that holds neither.  The walk from slot 0 passes
      -- through both formals' defaults and fails on slot 1, the next.
      runTestM "a cycle of shared defaults is a corrupt-bytecode error" $ do
        Symbol first <- symbolIntern "zcycleFirst"
        Symbol second <- symbolIntern "zcycleSecond"
        toSecond <- cbcEmit OpResolvedVar 0 0 0 1 0
        toFirst <- cbcEmit OpResolvedVar 0 0 0 0 0
        body <- literalOne
        formalsOff <- dataWords invalidSymbolId [2, 0, first, formalSharedDefault, toSecond, second, formalSharedDefault, toFirst]
        captureOff <- cbcEmitData captureNone
        lambda <- cbcEmit OpLambda formalSet 0 formalsOff body captureOff
        argument <- bindingWords [] >>= \bindingsOff -> cbcEmit OpAttrs 0 0 bindingsOff 0 0
        cbcEmit OpApp 0 0 lambda argument 0 >>= assertCorruptBytecodeAt "shared default slot" 1
    ]
  where
    literalOne = cbcEmit OpLitInt 0 0 1 0 0
    -- Data words built by hand, first to last; answers the offset of the
    -- first.
    dataWords firstWord rest = cbcEmitData firstWord <* mapM_ cbcEmitData rest
    -- @x: 1@, built by hand with the given formals type and capture tag.
    lambdaWith formalsType captureTag = do
      Symbol name <- symbolIntern "x"
      formalsOff <- cbcEmitData name
      captureOff <- cbcEmitData captureTag
      body <- literalOne
      cbcEmit OpLambda formalsType 0 formalsOff body captureOff
    -- A binding list's words, built by hand: its three counts zero, then
    -- the words given.  Answers the offset of the first.
    bindingWords = dataWords 0 . (0 :) . (0 :)

-- | An inherited name and whether its select reads that name, as the one
-- path segment, from slot 0 of the env it runs in.
inheritedSelect :: BcInheritedName -> IO (Text, Bool)
inheritedSelect (BcInheritedName _ sym selectIdx) = do
  op <- cbcOpcode selectIdx
  pathLen <- cbcShortArg selectIdx
  target <- cbcArg1 selectIdx
  pathOff <- cbcArg2 selectIdx
  targetOp <- cbcOpcode target
  level <- cbcArg1 target
  slot <- cbcArg2 target
  keyTag <- cbcData pathOff
  keySym <- cbcData (pathOff + 1)
  pure
    ( symbolText (Symbol sym),
      op == OpSelect && pathLen == 1 && targetOp == OpResolvedVar && level == 0 && slot == 0 && keyTag == attrkeyStatic && keySym == sym
    )

testBytecodeCompile :: IO [Bool]
testBytecodeCompile = do
  putStrLn "bytecode"
  -- Arena (including bytecode store) already initialized by main's bracket.
  sequence
    [ runTestM "compile ELit NixInt" $ do
        idx <- compileExpr (ELit (NixInt 42))
        op <- cbcOpcode idx
        a1 <- cbcArg1 idx
        a2 <- cbcArg2 idx
        pure
          ( if op == OpLitInt && a1 == 42 && a2 == 0
              then Pass
              else Fail ("op=" <> T.pack (show op) <> " a1=" <> T.pack (show a1))
          ),
      runTestM "compile ELit NixInt negative" $ do
        idx <- compileExpr (ELit (NixInt (-1)))
        op <- cbcOpcode idx
        a1 <- cbcArg1 idx
        a2 <- cbcArg2 idx
        -- -1 as uint64 = 0xFFFFFFFFFFFFFFFF, lo=0xFFFFFFFF, hi=0xFFFFFFFF
        pure
          ( if op == OpLitInt && a1 == 0xFFFFFFFF && a2 == 0xFFFFFFFF
              then Pass
              else
                Fail
                  ( "a1="
                      <> T.pack (show a1)
                      <> " a2="
                      <> T.pack (show a2)
                  )
          ),
      runTestM "compile ELit NixBool" $ do
        idxT <- compileExpr (ELit (NixBool True))
        idxF <- compileExpr (ELit (NixBool False))
        opT <- cbcOpcode idxT
        opF <- cbcOpcode idxF
        saT <- cbcShortArg idxT
        saF <- cbcShortArg idxF
        pure
          ( if opT == OpLitBool && saT == 1 && opF == OpLitBool && saF == 0
              then Pass
              else Fail "bool encoding mismatch"
          ),
      runTestM "compile ELit NixNull" $ do
        idx <- compileExpr (ELit NixNull)
        op <- cbcOpcode idx
        pure (assertEqual "opcode" OpLitNull op),
      runTestM "compile EResolvedVar" $ do
        idx <- compileExpr (EResolvedVar 3 7)
        op <- cbcOpcode idx
        a1 <- cbcArg1 idx
        a2 <- cbcArg2 idx
        pure
          ( if op == OpResolvedVar && a1 == 3 && a2 == 7
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " a1="
                      <> T.pack (show a1)
                      <> " a2="
                      <> T.pack (show a2)
                  )
          ),
      runTestM "compile EVar" $ do
        idx <- compileExpr (EVar "hello")
        op <- cbcOpcode idx
        sym <- cbcArg1 idx
        let symText = symbolText (Symbol sym)
        pure
          ( if op == OpVar && symText == "hello"
              then Pass
              else Fail ("op=" <> T.pack (show op) <> " sym=" <> symText)
          ),
      runTestM "compile EApp" $ do
        idx <- compileExpr (EApp (EVar "f") (ELit (NixInt 1)))
        op <- cbcOpcode idx
        funcIdx <- cbcArg1 idx
        argIdx <- cbcArg2 idx
        funcOp <- cbcOpcode funcIdx
        argOp <- cbcOpcode argIdx
        pure
          ( if op == OpApp && funcOp == OpVar && argOp == OpLitInt
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " funcOp="
                      <> T.pack (show funcOp)
                      <> " argOp="
                      <> T.pack (show argOp)
                  )
          ),
      runTestM "compile EIf" $ do
        idx <-
          compileExpr
            (EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2)))
        op <- cbcOpcode idx
        condIdx <- cbcArg1 idx
        thenIdx <- cbcArg2 idx
        elseIdx <- cbcArg3 idx
        condOp <- cbcOpcode condIdx
        thenA1 <- cbcArg1 thenIdx
        elseA1 <- cbcArg1 elseIdx
        pure
          ( if op == OpIf && condOp == OpLitBool && thenA1 == 1 && elseA1 == 2
              then Pass
              else Fail "if structure mismatch"
          ),
      runTestM "compile EBinary" $ do
        idx <-
          compileExpr
            (EBinary OpAdd (ELit (NixInt 10)) (ELit (NixInt 20)))
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        leftIdx <- cbcArg1 idx
        rightIdx <- cbcArg2 idx
        leftA1 <- cbcArg1 leftIdx
        rightA1 <- cbcArg1 rightIdx
        pure
          ( if op == OpBinary && fl == binaryAdd && leftA1 == 10 && rightA1 == 20
              then Pass
              else Fail "binary structure mismatch"
          ),
      runTestM "compile EUnary" $ do
        idx <- compileExpr (EUnary OpNegate (ELit (NixInt 5)))
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        operandIdx <- cbcArg1 idx
        operandA1 <- cbcArg1 operandIdx
        pure
          ( if op == OpUnary && fl == unaryNegate && operandA1 == 5
              then Pass
              else Fail "unary structure mismatch"
          ),
      runTestM "compile EList" $ do
        idx <-
          compileExpr
            (EList [ELit (NixInt 1), ELit (NixInt 2), ELit (NixInt 3)])
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        dataOff <- cbcArg1 idx
        c0 <- cbcData dataOff
        c1 <- cbcData (dataOff + 1)
        c2 <- cbcData (dataOff + 2)
        c0a1 <- cbcArg1 c0
        c1a1 <- cbcArg1 c1
        c2a1 <- cbcArg1 c2
        pure
          ( if op == OpList && count == 3 && c0a1 == 1 && c1a1 == 2 && c2a1 == 3
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " count="
                      <> T.pack (show count)
                  )
          ),
      runTestM "compile EStr with interpolation" $ do
        idx <-
          compileExpr
            (EStr [StrLit "hello ", StrInterp (EVar "name"), StrLit "!"])
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        dataOff <- cbcArg1 idx
        -- 3 parts x 2 words each = 6 data words
        tag0 <- cbcData dataOff
        _val0 <- cbcData (dataOff + 1)
        tag1 <- cbcData (dataOff + 2)
        val1 <- cbcData (dataOff + 3)
        tag2 <- cbcData (dataOff + 4)
        -- tag0=0(lit), tag1=1(interp), tag2=0(lit)
        interpOp <- cbcOpcode val1
        pure
          ( if op == OpStr
              && count == 3
              && tag0 == strpartLit
              && tag1 == strpartInterp
              && tag2 == strpartLit
              && interpOp == OpVar
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " count="
                      <> T.pack (show count)
                      <> " tag0="
                      <> T.pack (show tag0)
                      <> " tag1="
                      <> T.pack (show tag1)
                  )
          ),
      runTestM "compile EWith" $ do
        idx <- compileExpr (EWith (EVar "lib") (EVar "x"))
        op <- cbcOpcode idx
        scopeIdx <- cbcArg1 idx
        bodyIdx <- cbcArg2 idx
        scopeOp <- cbcOpcode scopeIdx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpWith && scopeOp == OpVar && bodyOp == OpVar
              then Pass
              else Fail "with structure mismatch"
          ),
      runTestM "compile EAssert" $ do
        idx <- compileExpr (EAssert (ELit (NixBool True)) (ELit (NixInt 1)))
        op <- cbcOpcode idx
        condIdx <- cbcArg1 idx
        bodyIdx <- cbcArg2 idx
        condOp <- cbcOpcode condIdx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpAssert && condOp == OpLitBool && bodyOp == OpLitInt
              then Pass
              else Fail "assert structure mismatch"
          ),
      runTestM "compile ELambda (FormalName)" $ do
        idx <-
          compileExpr
            (ELambda (FormalName "x") (EResolvedVar 0 0) NoCaptureInfo)
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        bodyIdx <- cbcArg2 idx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpLambda && fl == formalName && bodyOp == OpResolvedVar
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " flags="
                      <> T.pack (show fl)
                  )
          ),
      runTestM "compile ELet" $ do
        idx <-
          compileExpr
            ( ELet
                [NamedBinding (testPos 1 5) (StaticKey "x") (ELit (NixInt 42))]
                (EResolvedVar 0 0)
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        bodyIdx <- cbcArg2 idx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpLet && count == 1 && bodyOp == OpResolvedVar
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " count="
                      <> T.pack (show count)
                  )
          ),
      runTestM "compile EAttrs" $ do
        idx <-
          compileExpr
            ( EAttrs
                False
                [NamedBinding (testPos 1 3) (StaticKey "a") (ELit (NixInt 1))]
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        count <- cbcShortArg idx
        pure
          ( if op == OpAttrs && fl == 0 && count == 1
              then Pass
              else Fail "attrs structure mismatch"
          ),
      runTestM "compile EAttrs recursive" $ do
        idx <-
          compileExpr
            ( EAttrs
                True
                [ NamedBinding (testPos 1 7) (StaticKey "x") (ELit (NixInt 1)),
                  NamedBinding (testPos 1 14) (StaticKey "y") (EResolvedVar 0 0)
                ]
                (Captures [(0, 0)])
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        count <- cbcShortArg idx
        pure
          ( if op == OpAttrs && fl == 1 && count == 2
              then Pass
              else
                Fail
                  ( "flags="
                      <> T.pack (show fl)
                      <> " count="
                      <> T.pack (show count)
                  )
          ),
      runTestM "compile ESelect" $ do
        idx <-
          compileExpr
            (ESelect (EVar "x") [StaticKey "a"] Nothing)
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        pure
          ( if op == OpSelect && fl == 0
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile ESelect with default" $ do
        idx <-
          compileExpr
            (ESelect (EVar "x") [StaticKey "a"] (Just (ELit NixNull)))
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        defIdx <- cbcArg3 idx
        defOp <- cbcOpcode defIdx
        pure
          ( if op == OpSelect && fl == 1 && defOp == OpLitNull
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile EHasAttr" $ do
        idx <-
          compileExpr
            (EHasAttr (EVar "x") [StaticKey "a", StaticKey "b"])
        op <- cbcOpcode idx
        pathLen <- cbcShortArg idx
        pure
          ( if op == OpHasAttr && pathLen == 2
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " pathLen="
                      <> T.pack (show pathLen)
                  )
          ),
      runTestM "op_count grows after compilation" $ do
        before <- cbcOpCount
        _ <- compileExpr (EApp (EVar "f") (ELit (NixInt 1)))
        after <- cbcOpCount
        pure
          ( if after > before
              then Pass
              else
                Fail
                  ( "before="
                      <> T.pack (show before)
                      <> " after="
                      <> T.pack (show after)
                  )
          ),
      runTestM "compile ELit NixFloat" $ do
        idx <- compileExpr (ELit (NixFloat 3.14))
        op <- cbcOpcode idx
        pure (assertEqual "opcode" OpLitFloat op),
      runTestM "compile ELit NixUri" $ do
        idx <- compileExpr (ELit (NixUri "https://example.com"))
        op <- cbcOpcode idx
        uri <- cbcArg1 idx >>= cbcStringText
        pure
          ( if op == OpLitUri && uri == "https://example.com"
              then Pass
              else Fail "uri mismatch"
          ),
      runTestM "compile ELit NixPath" $ do
        idx <- compileExpr (ELit (NixPath "/nix/store/foo"))
        op <- cbcOpcode idx
        path <- cbcArg1 idx >>= cbcStringText
        pure
          ( if op == OpLitPath && path == "/nix/store/foo"
              then Pass
              else Fail "path mismatch"
          ),
      runTestM "compile inherit-from binding" $ do
        idx <-
          compileExpr
            ( EAttrs
                False
                [InheritFrom (EVar "s") [InheritedName (testPos 1 15) "x", InheritedName (testPos 1 17) "y"]]
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        bindings <- compiledBindings idx >>= bindingList
        case bindings of
          Right [BcInheritFrom _ names]
            | op == OpAttrs && count == 1 -> do
                selects <- inheritedNameList names >>= mapM inheritedSelect
                pure (assertEqual "inherited names and their selects" [("x", True), ("y", True)] selects)
          _ -> pure (Fail "inherit binding mismatch"),
      -- A plain inherit has no lookup of its own: it carries the variable,
      -- bound like any other, for the evaluator to read in the outer env.
      runTestM "compile plain inherit with the variable it copies" $ do
        idx <- compileExpr (EAttrs False [Inherit (testPos 1 11) "x" (EResolvedVar 0 0)] NoCaptureInfo)
        bindings <- compiledBindings idx >>= bindingList
        case bindings of
          Right [BcInherit _ sym valIdx] -> do
            valOp <- cbcOpcode valIdx
            pure $
              if symbolText (Symbol sym) == "x" && valOp == OpResolvedVar
                then Pass
                else Fail "expected x to carry the resolved variable"
          _ -> pure (Fail "expected one inherit binding"),
      -- Every kind of binding carries the position it was parsed at
      -- through the bytecode, and reads it back with its file.
      runTestM "compile records each binding's position" $ do
        let at = SrcPos (SourceFile "/positions/set.nix")
        idx <-
          compileExpr
            ( EAttrs
                False
                [ NamedBinding (at 2 3) (StaticKey "a") (ELit (NixInt 1)),
                  Inherit (at 3 11) "x" (EResolvedVar 0 0),
                  InheritFrom (EResolvedVar 0 0) [InheritedName (at 4 22) "y"]
                ]
                NoCaptureInfo
            )
        bindings <- compiledBindings idx >>= bindingList
        let positionsOf binding = case binding of
              BcNamed pos _ _ -> pure [pos]
              BcDynamic pos _ _ -> pure [pos]
              BcInherit pos _ _ -> pure [pos]
              BcInheritFrom _ names -> map (\(BcInheritedName pos _ _) -> pos) <$> inheritedNameList names
        readBack <- traverse (mapM positionsOf >=> mapM lookupPos . concat) bindings
        pure $
          assertEqual
            "positions read back"
            (Right (map Just [PosInFile "/positions/set.nix" 2 3, PosInFile "/positions/set.nix" 3 11, PosInFile "/positions/set.nix" 4 22]))
            readBack,
      -- Recording a position and reading it back keeps its file, line and
      -- column, text with no file reads back as such, and a line or
      -- column past 32 bits is unrecordable, as it is in upstream's table.
      -- One recorder sees every case, so the origin it remembers changes
      -- under it as a compilation's never does.
      runTestM "a recorded position reads back as it was recorded" $ do
        recorder <- newPosRecorder
        let wordCeiling = fromIntegral (maxBound :: Word32)
            genOrigin =
              QC.oneof
                [ SourceFile <$> QC.elements ["/a.nix", "/nix/store/00000000000000000000000000000000-s/default.nix", "C:\\work\\f.nix", "/caf\233.nix"],
                  pure testOrigin
                ]
            genCoordinate = QC.oneof [QC.chooseInt (1, 4096), QC.chooseInt (1, wordCeiling), QC.chooseInt (wordCeiling + 1, maxBound)]
            expected (SrcPos origin line column)
              | line > wordCeiling || column > wordCeiling = Nothing
              | otherwise = Just $ case origin of
                  SourceFile path -> PosInFile path line column
                  SourceText _ -> PosInText line column
            property =
              QC.forAll (SrcPos <$> genOrigin <*> genCoordinate <*> genCoordinate) $ \pos ->
                QC.ioProperty (recordPos recorder pos >>= lookupPos <&> (QC.=== expected pos))
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      -- Upstream's Pos::print with its origin (position.cc:69-83 at
      -- 2.24.9) writes a file by its path and text with no file as
      -- string in guillemets, then the line and the column; noPos is its
      -- empty Pos (nixexpr.cc:608-612), line 0 and column 0, in none in
      -- guillemets, and a column of 0 is left out.
      runTestM "a position prints as upstream's messages write it" $ do
        recorder <- newPosRecorder
        inFile <- recordPos recorder (SrcPos (SourceFile "/positions/set.nix") 2 3)
        inText <- recordPos recorder (testPos 12 345)
        printed <- mapM printPos [inFile, inText, noPos]
        pure (assertEqual "printed" ["/positions/set.nix:2:3", "\x00ABstring\x00BB:12:345", "\x00ABnone\x00BB:0"] printed),
      runTestM "compile ELambda (FormalSet)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalSet [Formal "a" Nothing, Formal "b" (Just (ELit (NixInt 0)))] False)
                (EResolvedVar 0 0)
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        pure
          ( if op == OpLambda && fl == formalSet
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile ELambda (FormalNamedSet)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalNamedSet "args" [Formal "x" Nothing] True)
                (EResolvedVar 0 0)
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        pure
          ( if op == OpLambda && fl == formalNamedSet
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile CaptureInfo (Captures)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalName "x")
                (EResolvedVar 0 0)
                (Captures [(1, 2), (3, 4)])
            )
        op <- cbcOpcode idx
        capOff <- cbcArg3 idx
        capTag <- cbcData capOff
        capCount <- cbcData (capOff + 1)
        capL0 <- cbcData (capOff + 2)
        capI0 <- cbcData (capOff + 3)
        capL1 <- cbcData (capOff + 4)
        capI1 <- cbcData (capOff + 5)
        pure
          ( if op == OpLambda
              && capTag == captureSlots
              && capCount == 2
              && capL0 == 1
              && capI0 == 2
              && capL1 == 3
              && capI1 == 4
              then Pass
              else
                Fail
                  ( "capTag="
                      <> T.pack (show capTag)
                      <> " capCount="
                      <> T.pack (show capCount)
                  )
          ),
      runTestM "compile CaptureInfo (CapturesWithScopes)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalName "x")
                (EResolvedVar 0 0)
                (CapturesWithScopes [(0, 0)])
            )
        capOff <- cbcArg3 idx
        capTag <- cbcData capOff
        pure (assertEqual "captureTag" captureWithScopes capTag),
      runTestM "compile EWithVar" $ do
        idx <- compileExpr (EWithVar "dynamic")
        op <- cbcOpcode idx
        sym <- cbcArg1 idx
        pure
          ( if op == OpWithVar && symbolText (Symbol sym) == "dynamic"
              then Pass
              else Fail "withvar mismatch"
          ),
      runTestM "compile EIndStr" $ do
        idx <- compileExpr (EIndStr [StrLit "indented"])
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        pure
          ( if op == OpIndStr && count == 1
              then Pass
              else Fail "indstr mismatch"
          )
    ]
