-- | Built-in function environment for the Nix evaluator.
--
-- Every Nix expression has access to a @builtins@ attribute set containing
-- ~100 functions.  This module assembles the initial 'Env' from the
-- central registry in "Nix.Eval" and adds standard constants
-- (@true@, @false@, @null@, @storeDir@, @currentTime@,
-- @currentSystem@, etc.).
module Nix.Builtins
  ( -- * Builtin registration
    builtinEnv,
    builtinEnvWithScope,

    -- * NIX_PATH parsing
    parseNixPath,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval (Env (..), NixValue (..), Thunk (..), attrSetFromMap, builtinNames, currentSystemStr, evaluated)
import Nix.Eval.Types (mkStr)
import Nix.Store.Path (defaultStoreDirText)

-- | The initial environment containing all builtins.
--
-- Real Nix exposes a subset of builtins at the top level without
-- the @builtins.@ prefix.  These are the functions most commonly
-- used unqualified in nixpkgs and user code.
--
-- @currentTime@ is an integer constant (seconds since epoch),
-- passed in at startup.  In tests, pass @0@.
--
-- @searchPaths@ populates @builtins.nixPath@.  Parsed from @NIX_PATH@
-- by 'parseNixPath'.  In tests, pass @[]@.
builtinEnv :: Integer -> [Thunk] -> Env
builtinEnv timestamp searchPaths =
  Env
    { envSlots = mempty,
      envLazyScope =
        Just $
          attrSetFromMap $
            Map.fromList $
              -- Values
              [ ("true", evaluated (VBool True)),
                ("false", evaluated (VBool False)),
                ("null", evaluated VNull),
                ("builtins", evaluated (builtinsAttrSet timestamp searchPaths))
              ]
                -- Top-level builtin functions (available without builtins. prefix)
                ++ map topLevelBuiltin topLevelBuiltinNames,
      envParent = Nothing,
      envWithScopes = []
    }

-- | Builtins exposed at the top level (without @builtins.@ prefix).
-- This matches real Nix — nixpkgs uses these unqualified everywhere.
topLevelBuiltinNames :: [Text]
topLevelBuiltinNames =
  [ "abort",
    "baseNameOf",
    "derivation",
    "dirOf",
    "fetchGit",
    "fetchTarball",
    "fetchurl",
    "import",
    "isNull",
    "map",
    "placeholder",
    "removeAttrs",
    "scopedImport",
    "throw",
    "toFile",
    "toString"
  ]

-- | Create a top-level binding for a builtin function.
topLevelBuiltin :: Text -> (Text, Thunk)
topLevelBuiltin name = (name, evaluated (VBuiltin name []))

-- | Like 'builtinEnv' but with additional scope bindings overlaid on
-- the top-level environment.  Used by @scopedImport@.
builtinEnvWithScope :: Integer -> [Thunk] -> [(Text, Thunk)] -> Env
builtinEnvWithScope timestamp searchPaths scope =
  let base = builtinEnv timestamp searchPaths
      scopeMap = Map.fromList scope
   in Env {envSlots = mempty, envLazyScope = Just (attrSetFromMap scopeMap), envParent = Just base, envWithScopes = []}

-- | The @builtins@ attribute set, derived from the central registry.
builtinsAttrSet :: Integer -> [Thunk] -> NixValue
builtinsAttrSet timestamp searchPaths =
  VAttrs $ attrSetFromMap $ Map.union builtinEntries (standardEntries timestamp searchPaths)
  where
    builtinEntries =
      Map.fromList [(name, evaluated (VBuiltin name [])) | name <- builtinNames]

standardEntries :: Integer -> [Thunk] -> Map.Map Text Thunk
standardEntries timestamp searchPaths =
  Map.fromList
    [ ("true", evaluated (VBool True)),
      ("false", evaluated (VBool False)),
      ("null", evaluated VNull),
      ("storeDir", evaluated (mkStr defaultStoreDirText)),
      ("nixVersion", evaluated (mkStr "2.24.0")),
      ("langVersion", evaluated (VInt 6)),
      ("nixPath", evaluated (VList searchPaths)),
      ("currentTime", evaluated (VInt timestamp)),
      ("currentSystem", evaluated (mkStr currentSystemStr))
    ]

-- ---------------------------------------------------------------------------
-- NIX_PATH parsing
-- ---------------------------------------------------------------------------

-- | Parse a @NIX_PATH@-formatted string into a list of search path entry
-- thunks.  Each entry becomes a @{ prefix, path }@ attrset.
--
-- Format: colon-separated entries, each either @name=path@ or plain @path@.
-- A plain path gets an empty prefix (matching real Nix behaviour).
--
-- >>> parseNixPath "nixpkgs=/home/user/nixpkgs:custom=/opt/custom"
-- [Evaluated (VAttrs {"prefix": "nixpkgs", "path": "/home/user/nixpkgs"}), ...]
parseNixPath :: Text -> [Thunk]
parseNixPath raw
  | T.null raw = []
  | otherwise = map parseEntry (splitNixPath raw)
  where
    parseEntry entry =
      let (prefix, path) = case T.breakOn "=" entry of
            (before, after)
              | T.null after -> ("", before)
              | otherwise -> (before, T.drop 1 after)
       in evaluated
            ( VAttrs
                ( attrSetFromMap $
                    Map.fromList
                      [ ("prefix", evaluated (mkStr prefix)),
                        ("path", evaluated (mkStr path))
                      ]
                )
            )

-- | Split a NIX_PATH string on colon separators, respecting Windows
-- drive letters.  A colon followed by @\\@ or @/@ (e.g. @C:\\@) is
-- part of a path, not a separator.
splitNixPath :: Text -> [Text]
splitNixPath = go T.empty
  where
    go acc remaining = case T.uncons remaining of
      Nothing -> [acc | not (T.null acc)]
      Just (':', rest)
        | isDriveSep rest -> go (acc <> ":" <> T.take 1 rest) (T.drop 1 rest)
        | otherwise -> acc : go T.empty rest
      Just (c, rest) -> go (T.snoc acc c) rest
    -- After a colon, if the next char is \ or /, it's a drive letter
    isDriveSep t = case T.uncons t of
      Just ('\\', _) -> True
      Just ('/', _) -> True
      _ -> False
