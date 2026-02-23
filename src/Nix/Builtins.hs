-- | Built-in functions for the Nix evaluator.
--
-- Every Nix expression has access to a @builtins@ attribute set containing
-- ~100 functions.  This module provides the initial environment with the
-- subset implemented so far: type checks, basic list/string operations,
-- control flow, and @currentSystem@.
module Nix.Builtins
  ( -- * Builtin registration
    builtinEnv,
  )
where

import qualified Data.Map.Strict as Map
import Nix.Eval (Env (..), NixValue (..), Thunk (..), builtinNames, evaluated)

-- | The initial environment containing all builtins.
--
-- The top-level scope gets @true@, @false@, @null@, and @builtins@.
-- The @builtins@ attrset itself also contains @true@, @false@, and
-- @null@ (matching real Nix behavior).
builtinEnv :: Env
builtinEnv =
  Env
    { envBindings =
        Map.fromList
          [ ("true", val (VBool True)),
            ("false", val (VBool False)),
            ("null", val VNull),
            ("builtins", val builtinsAttrSet)
          ],
      envWithScopes = []
    }

-- | The @builtins@ attribute set, derived from the central registry.
builtinsAttrSet :: NixValue
builtinsAttrSet =
  VAttrs $ Map.union builtinEntries standardEntries
  where
    builtinEntries =
      Map.fromList [(name, val (VBuiltin name [])) | name <- builtinNames]
    standardEntries =
      Map.fromList
        [ ("true", val (VBool True)),
          ("false", val (VBool False)),
          ("null", val VNull),
          ("storeDir", val (VStr "/nix/store")),
          ("nixVersion", val (VStr "2.24.0")),
          ("langVersion", val (VInt 6)),
          ("nixPath", val (VList []))
        ]

-- | Wrap a value as an already-evaluated thunk.
val :: NixValue -> Thunk
val = evaluated
