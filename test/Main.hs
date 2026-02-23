module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Derivation (Platform (..), currentPlatform)
import Nix.Eval (Env (..), emptyEnv)
import Nix.Expr.Types
import Nix.Store.Path (StoreDir (..), StorePath (..), defaultStoreDir, storePathToFilePath, windowsStoreDir)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- ---------------------------------------------------------------------------
-- Test harness (same pattern as gbnet-hs, nova-cache)
-- ---------------------------------------------------------------------------

data TestResult = Pass | Fail !Text

runTest :: Text -> TestResult -> IO Bool
runTest name result = case result of
  Pass -> do
    putStrLn $ "  PASS  " ++ T.unpack name
    pure True
  Fail msg -> do
    putStrLn $ "  FAIL  " ++ T.unpack name ++ ": " ++ T.unpack msg
    pure False

assertEqual :: (Eq a, Show a) => Text -> a -> a -> TestResult
assertEqual label expected actual
  | expected == actual = Pass
  | otherwise =
      Fail $
        label
          <> ": expected "
          <> T.pack (show expected)
          <> " but got "
          <> T.pack (show actual)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

testExprTypes :: IO [Bool]
testExprTypes = do
  putStrLn "expr/types"
  sequence
    [ runTest "int literal" $
        assertEqual "ELit NixInt" (ELit (NixInt 42)) (ELit (NixInt 42)),
      runTest "bool literal" $
        assertEqual "ELit NixBool" (ELit (NixBool True)) (ELit (NixBool True)),
      runTest "null literal" $
        assertEqual "ELit NixNull" (ELit NixNull) (ELit NixNull),
      runTest "var" $
        assertEqual "EVar" (EVar "x") (EVar "x"),
      runTest "string parts" $
        let parts = [StrLit "hello ", StrInterp (EVar "name")]
         in assertEqual "EStr" (EStr parts) (EStr parts),
      runTest "binary op" $
        let expr = EBinary OpAdd (ELit (NixInt 1)) (ELit (NixInt 2))
         in assertEqual "EBinary" expr expr,
      runTest "lambda" $
        let expr = ELambda (FormalName "x") (EVar "x")
         in assertEqual "ELambda" expr expr,
      runTest "let binding" $
        let expr = ELet [NamedBinding [StaticKey "x"] (ELit (NixInt 1))] (EVar "x")
         in assertEqual "ELet" expr expr,
      runTest "attrs" $
        let expr = EAttrs False [NamedBinding [StaticKey "a"] (ELit (NixInt 1))]
         in assertEqual "EAttrs" expr expr,
      runTest "if-then-else" $
        let expr = EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2))
         in assertEqual "EIf" expr expr
    ]

testStorePaths :: IO [Bool]
testStorePaths = do
  putStrLn "store/path"
  let sp = StorePath {spHash = "s66mzxpvicwk07gjbjfw9izjfa797vsw", spName = "hello-2.12.1"}
  sequence
    [ runTest "default store dir" $
        assertEqual "defaultStoreDir" "/nix/store" (unStoreDir defaultStoreDir),
      runTest "windows store dir" $
        assertEqual "windowsStoreDir" "C:\\nix\\store" (unStoreDir windowsStoreDir),
      runTest "store path to filepath" $
        assertEqual
          "storePathToFilePath"
          "/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1"
          (storePathToFilePath defaultStoreDir sp),
      runTest "store path ordering" $
        let sp2 = StorePath {spHash = "zzz", spName = "later"}
         in assertEqual "Ord" True (sp < sp2)
    ]

testDerivation :: IO [Bool]
testDerivation = do
  putStrLn "derivation"
  sequence
    [ runTest "current platform is known" $
        case currentPlatform of
          OtherPlatform _ -> Fail "currentPlatform returned OtherPlatform"
          _ -> Pass
    ]

testEval :: IO [Bool]
testEval = do
  putStrLn "eval"
  sequence
    [ runTest "empty env" $
        assertEqual "emptyEnv" 0 (length ((\(Nix.Eval.Env m) -> m) emptyEnv))
    ]

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "nova-nix test suite"
  putStrLn "==================="
  results <-
    concat
      <$> sequence
        [ testExprTypes,
          testStorePaths,
          testDerivation,
          testEval
        ]
  let total = length results
      passed = length (filter id results)
      failed = total - passed
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " passed"
  if failed > 0
    then do
      putStrLn $ show failed ++ " FAILED"
      exitFailure
    else do
      putStrLn "All tests passed."
      exitSuccess
