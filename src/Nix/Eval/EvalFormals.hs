-- | Eval-time formals for lambda parameters.
--
-- Separates the evaluation representation (defaults are bytecode indices)
-- from the parser representation ('Nix.Expr.Types.Formal', defaults are
-- t'Nix.Expr.Types.Expr').  This avoids circular dependencies: both "Nix.Eval.Types"
-- (for 'Nix.Eval.Types.VLambda') and "Nix.Eval.Compile" (for encoding\/decoding)
-- import this leaf module.
--
-- Every name here is the t'Symbol' the bytecode and a C lambda hold, as
-- upstream's @Formal@ and @ExprLambda@ hold theirs (nixexpr.hh:294-299
-- and :326-332 at 2.24.9): a call finds its formals in its argument by
-- symbol, and a name becomes text only where a message or a printed
-- value spells it.
module Nix.Eval.EvalFormals
  ( EvalFormals (..),
    EvalFormal (..),
    FormalDefault (..),
    hasDefault,
    LambdaName (..),
    lambdaNameText,
    lambdaNameSymbol,
    lambdaNameFromSymbol,
  )
where

import Data.Text (Text)
import Data.Word (Word32)
import Nix.Eval.Symbol (Symbol (..), invalidSymbolId, symbolText)

-- | A single formal parameter and its default.
data EvalFormal = EvalFormal
  { efName :: !Symbol,
    efDefault :: !FormalDefault
  }
  deriving (Eq, Show)

-- | A formal's default, as the compiler decides it.
data FormalDefault
  = -- | None: a call's argument must hold the formal.
    NoDefault
  | -- | The code at a bytecode index, made in the call's frame.
    Default !Word32
  | -- | A variable naming a slot of the call's frame, the code at a
    -- bytecode index, whose cell the formal's slot shares once the frame
    -- is filled, as upstream's @maybeThunk@ shares the cell of a slot it
    -- has filled before the formal's (eval.cc:1014-1021 and :1539-1563 at
    -- 2.24.9).
    SharedDefault !Word32
  deriving (Eq, Show)

-- | Whether a call can leave the formal out of its argument.
hasDefault :: FormalDefault -> Bool
hasDefault NoDefault = False
hasDefault (Default _) = True
hasDefault (SharedDefault _) = True

-- | The name a function's call errors give it.  Upstream names a function
-- after the attribute a set or @let@ binds it to, and names a function
-- that is its body the same (@addAttr@, parser-state.hh:176-177, and
-- @ExprLambda::setName@, nixexpr.cc:573-577 at 2.24.9).  Only a set
-- pattern raises such an error, so only its formals carry the name.
data LambdaName
  = -- | Bound to no static attribute name.
    AnonymousLambda
  | -- | Bound to this attribute name.
    NamedLambda !Symbol
  deriving (Eq, Show)

-- | The name as a call error spells it (eval.cc:1550-1551 at 2.24.9).
lambdaNameText :: LambdaName -> Text
lambdaNameText AnonymousLambda = "anonymous lambda"
lambdaNameText (NamedLambda name) = symbolText name

-- | The symbol a set pattern's bytecode and a C lambda hold for the name:
-- the name's own, or the invalid symbol for an anonymous function, which
-- no name interns to.
lambdaNameSymbol :: LambdaName -> Word32
lambdaNameSymbol AnonymousLambda = invalidSymbolId
lambdaNameSymbol (NamedLambda name) = unSymbol name

-- | The name 'lambdaNameSymbol' stored as a symbol.
lambdaNameFromSymbol :: Word32 -> LambdaName
lambdaNameFromSymbol sid
  | sid == invalidSymbolId = AnonymousLambda
  | otherwise = NamedLambda (Symbol sid)

-- | Formal parameter specification for a lambda.
-- Mirrors 'Nix.Expr.Types.Formals' but with each default as compiled.
data EvalFormals
  = -- | @x: body@ - single named parameter
    EFName !Symbol
  | -- | @{ a, b, ... }: body@ - destructuring set pattern
    EFSet !LambdaName ![EvalFormal] !Bool
  | -- | @args\@{ a, b, ... }: body@ - named set pattern
    EFNamedSet !LambdaName !Symbol ![EvalFormal] !Bool
  deriving (Eq, Show)
