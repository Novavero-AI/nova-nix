-- | Eval-time formals for lambda parameters.
--
-- Separates the evaluation representation (defaults are bytecode indices)
-- from the parser representation ('Nix.Expr.Types.Formal', defaults are
-- 'Expr').  This avoids circular dependencies: both 'Nix.Eval.Types'
-- (for 'VLambda') and 'Nix.Eval.Compile' (for encoding\/decoding)
-- import this leaf module.
module Nix.Eval.EvalFormals
  ( EvalFormals (..),
    EvalFormal (..),
  )
where

import Data.Text (Text)
import Data.Word (Word32)

-- | A single formal parameter with an optional default (bytecode index).
data EvalFormal = EvalFormal
  { efName :: !Text,
    efDefault :: !(Maybe Word32)
  }
  deriving (Eq, Show)

-- | Formal parameter specification for a lambda.
-- Mirrors 'Nix.Expr.Types.Formals' but with bytecode indices for defaults.
data EvalFormals
  = -- | @x: body@ — single named parameter
    EFName !Text
  | -- | @{ a, b, ... }: body@ — destructuring set pattern
    EFSet ![EvalFormal] !Bool
  | -- | @args\@{ a, b, ... }: body@ — named set pattern
    EFNamedSet !Text ![EvalFormal] !Bool
  deriving (Eq, Show)
