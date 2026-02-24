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
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Eval (Env (..), NixValue (..), Thunk (..), builtinNames, currentSystemStr, evaluated)
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
builtinEnv :: Integer -> Env
builtinEnv timestamp =
  Env
    { envBindings =
        Map.fromList $
          -- Values
          [ ("true", evaluated (VBool True)),
            ("false", evaluated (VBool False)),
            ("null", evaluated VNull),
            ("builtins", evaluated (builtinsAttrSet timestamp))
          ]
            -- Top-level builtin functions (available without builtins. prefix)
            ++ map topLevelBuiltin topLevelBuiltinNames,
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
builtinEnvWithScope :: Integer -> [(Text, Thunk)] -> Env
builtinEnvWithScope timestamp scope =
  let base = builtinEnv timestamp
      scopeMap = Map.fromList scope
   in base {envBindings = Map.union scopeMap (envBindings base)}

-- | The @builtins@ attribute set, derived from the central registry.
builtinsAttrSet :: Integer -> NixValue
builtinsAttrSet timestamp =
  VAttrs $ Map.union builtinEntries (standardEntries timestamp)
  where
    builtinEntries =
      Map.fromList [(name, evaluated (VBuiltin name [])) | name <- builtinNames]

standardEntries :: Integer -> Map.Map Text Thunk
standardEntries timestamp =
  Map.fromList
    [ ("true", evaluated (VBool True)),
      ("false", evaluated (VBool False)),
      ("null", evaluated VNull),
      ("storeDir", evaluated (mkStr defaultStoreDirText)),
      ("nixVersion", evaluated (mkStr "2.24.0")),
      ("langVersion", evaluated (VInt 6)),
      ("nixPath", evaluated (VList [])),
      ("currentTime", evaluated (VInt timestamp)),
      ("currentSystem", evaluated (mkStr currentSystemStr))
    ]
