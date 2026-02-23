-- | Re-exports the Nix expression AST.
--
-- The Nix language is a lazy, pure, functional language. It has:
--
-- * Attribute sets (like JSON objects, but with functions and recursion)
-- * Functions (single-argument lambdas with pattern matching)
-- * Let bindings and with-scoping
-- * String interpolation (@"hello ${name}"@)
-- * Path literals (@./foo/bar@, @\/nix\/store\/...@)
-- * List, integer, float, bool, null
-- * Operators: arithmetic, boolean, list concat (++), attrset merge (//)
--
-- Every @.nix@ file is a single expression. There are no statements.
-- @import ./foo.nix@ is a function call that evaluates a file and returns
-- its value.  @nixpkgs@ is just a giant attribute set of derivations.
module Nix.Expr
  ( module Nix.Expr.Types,
  )
where

import Nix.Expr.Types
