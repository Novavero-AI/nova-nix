-- | Built-in functions for the Nix evaluator.
--
-- == What are builtins?
--
-- Every Nix expression has access to a @builtins@ attribute set containing
-- ~100 functions.  These are the "standard library" of Nix.  They cannot
-- be expressed in the Nix language itself — they require access to hashing,
-- filesystem, fetching, and other capabilities.
--
-- Some highlights:
--
-- * @builtins.derivation@ — THE fundamental builtin.  Takes an attrset
--   (name, builder, system, etc.) and returns a derivation.  Every
--   package in nixpkgs ultimately calls this.
--
-- * @builtins.import@ — Evaluate another .nix file and return its value.
--   This is how nixpkgs is loaded: @import \<nixpkgs\> {}@.
--
-- * @builtins.fetchurl@ / @fetchTarball@ / @fetchGit@ — Download sources.
--   These are "fixed-output derivations": the output hash is known in
--   advance, so the download is reproducible (if the hash matches, the
--   content is correct regardless of where/when it was fetched).
--
-- * @builtins.map@, @filter@, @foldl'@, @sort@ — Functional list operations.
--   Nix is a functional language — these are used everywhere in nixpkgs.
--
-- * @builtins.hashString@, @hashFile@ — Compute hashes (used internally
--   for content-addressing).
--
-- * @builtins.currentSystem@ — Returns the platform string:
--   @"x86_64-linux"@, @"x86_64-darwin"@, or for us: @"x86_64-windows"@.
--   This is how packages know what platform they're building on/for.
--
-- * @builtins.toJSON@, @fromJSON@ — JSON serialization (used by NixOps,
--   flake metadata, etc.).
--
-- * @builtins.trace@ — Debug printing during evaluation.
--
-- * @builtins.throw@, @abort@ — Evaluation errors with messages.
--
-- * @builtins.tryEval@ — Catch evaluation errors (returns @{ success, value }@).
module Nix.Builtins
  ( -- * Builtin registration
    builtinEnv,
  )
where

import qualified Data.Map.Strict as Map
import Nix.Eval (Env (..), NixValue (..))

-- | The initial environment containing all builtins.
--
-- This is injected at the top level of every evaluation.
-- @builtins.currentSystem@ will return @"x86_64-windows"@ on Windows,
-- @"x86_64-linux"@ on Linux, etc.
builtinEnv :: Env
builtinEnv =
  Env $
    Map.fromList
      [ ("builtins", VAttrs Map.empty), -- TODO: populate with all builtins
        ("true", VBool True),
        ("false", VBool False),
        ("null", VNull)
      ]
