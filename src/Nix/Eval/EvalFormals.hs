-- | Eval-time formals for lambda parameters.
--
-- Separates the evaluation representation (defaults are bytecode indices)
-- from the parser representation ('Nix.Expr.Types.Formal', defaults are
-- t'Nix.Expr.Types.Expr').  This avoids circular dependencies: both "Nix.Eval.Types"
-- (for 'Nix.Eval.Types.VLambda') and "Nix.Eval.Compile" (for encoding\/decoding)
-- import this leaf module.
module Nix.Eval.EvalFormals
  ( EvalFormals (..),
    EvalFormal (..),
    LambdaName (..),
    lambdaNameText,
    lambdaNameSymbol,
    lambdaNameFromSymbol,
  )
where

import Data.Text (Text)
import Data.Word (Word32)
import Nix.Eval.CData (MonadCData)
import Nix.Eval.Symbol (Symbol (..), invalidSymbolId, symbolIntern, symbolText)

-- | A single formal parameter with an optional default (bytecode index).
data EvalFormal = EvalFormal
  { efName :: !Text,
    efDefault :: !(Maybe Word32)
  }
  deriving (Eq, Show)

-- | The name a function's call errors give it.  Upstream names a function
-- after the attribute a set or @let@ binds it to, and names a function
-- that is its body the same (@addAttr@, parser-state.hh:176-177, and
-- @ExprLambda::setName@, nixexpr.cc:573-577 at 2.24.9).  Only a set
-- pattern raises such an error, so only its formals carry the name.
data LambdaName
  = -- | Bound to no static attribute name.
    AnonymousLambda
  | -- | Bound to this attribute name.
    NamedLambda !Text
  deriving (Eq, Show)

-- | The name as a call error spells it (eval.cc:1550-1551 at 2.24.9).
lambdaNameText :: LambdaName -> Text
lambdaNameText AnonymousLambda = "anonymous lambda"
lambdaNameText (NamedLambda name) = name

-- | The symbol a set pattern's bytecode and a C lambda hold for the name:
-- the name's own, or the invalid symbol for an anonymous function, which
-- no name interns to.  Interning a name a binding has already interned
-- only reads its symbol.
lambdaNameSymbol :: (MonadCData m) => LambdaName -> m Word32
lambdaNameSymbol AnonymousLambda = pure invalidSymbolId
lambdaNameSymbol (NamedLambda name) = unSymbol <$> symbolIntern name

-- | The name 'lambdaNameSymbol' stored as a symbol.
lambdaNameFromSymbol :: Word32 -> LambdaName
lambdaNameFromSymbol sid
  | sid == invalidSymbolId = AnonymousLambda
  | otherwise = NamedLambda (symbolText (Symbol sid))

-- | Formal parameter specification for a lambda.
-- Mirrors 'Nix.Expr.Types.Formals' but with bytecode indices for defaults.
data EvalFormals
  = -- | @x: body@ - single named parameter
    EFName !Text
  | -- | @{ a, b, ... }: body@ - destructuring set pattern
    EFSet !LambdaName ![EvalFormal] !Bool
  | -- | @args\@{ a, b, ... }: body@ - named set pattern
    EFNamedSet !LambdaName !Text ![EvalFormal] !Bool
  deriving (Eq, Show)
