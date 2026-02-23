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
import Data.Text (Text)
import Nix.Eval (Env (..), NixValue (..), Thunk (..), evaluated)

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

-- | The @builtins@ attribute set.
builtinsAttrSet :: NixValue
builtinsAttrSet =
  VAttrs $
    Map.fromList
      -- Type checking builtins
      [ builtin "typeOf",
        builtin "isNull",
        builtin "isInt",
        builtin "isFloat",
        builtin "isBool",
        builtin "isString",
        builtin "isList",
        builtin "isAttrs",
        builtin "isFunction",
        -- List operations
        builtin "length",
        builtin "head",
        builtin "tail",
        -- String operations
        builtin "toString",
        builtin "stringLength",
        -- Control
        builtin "throw",
        builtin "abort",
        -- System
        builtin "currentSystem",
        -- Standard values
        ("true", val (VBool True)),
        ("false", val (VBool False)),
        ("null", val VNull)
      ]

-- | Create a builtin function entry (name -> VBuiltin thunk).
builtin :: Text -> (Text, Thunk)
builtin name = (name, val (VBuiltin name))

-- | Wrap a value as an already-evaluated thunk.
val :: NixValue -> Thunk
val = evaluated
