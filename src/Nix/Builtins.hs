-- | Built-in functions for the Nix evaluator.
--
-- Every Nix expression has access to a @builtins@ attribute set containing
-- ~100 functions.  This module provides the initial environment with the
-- subset implemented so far: type checks, basic list/string operations,
-- control flow, and @currentSystem@.
module Nix.Builtins
  ( -- * Builtin registration
    builtinEnv,
    builtinEnvWithScope,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Eval (Env (..), NixValue (..), Thunk (..), builtinNames, evaluated)

-- | The initial environment containing all builtins.
--
-- The top-level scope gets @true@, @false@, @null@, @import@,
-- @derivation@, and @builtins@.
-- The @builtins@ attrset itself also contains @true@, @false@, and
-- @null@ (matching real Nix behavior).
--
-- @currentTime@ is an integer constant (seconds since epoch),
-- passed in at startup.  In tests, pass @0@.
builtinEnv :: Integer -> Env
builtinEnv timestamp =
  Env
    { envBindings =
        Map.fromList
          [ ("true", val (VBool True)),
            ("false", val (VBool False)),
            ("null", val VNull),
            ("import", val (VBuiltin "import" [])),
            ("derivation", val (VBuiltin "derivation" [])),
            ("builtins", val (builtinsAttrSet timestamp))
          ],
      envWithScopes = []
    }

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
      Map.fromList [(name, val (VBuiltin name [])) | name <- builtinNames]

standardEntries :: Integer -> Map.Map Text Thunk
standardEntries timestamp =
  Map.fromList
    [ ("true", val (VBool True)),
      ("false", val (VBool False)),
      ("null", val VNull),
      ("storeDir", val (VStr "/nix/store")),
      ("nixVersion", val (VStr "2.24.0")),
      ("langVersion", val (VInt 6)),
      ("nixPath", val (VList [])),
      ("currentTime", val (VInt timestamp))
    ]

-- | Wrap a value as an already-evaluated thunk.
val :: NixValue -> Thunk
val = evaluated
