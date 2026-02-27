-- | Core AST types for the Nix expression language.
--
-- Every Nix source file parses into an 'Expr'. The AST is a direct
-- representation of the language grammar — no desugaring at parse time.
-- The evaluator ('Nix.Eval') reduces expressions to values.
module Nix.Expr.Types
  ( -- * Expressions
    Expr (..),

    -- * Atoms
    NixAtom (..),

    -- * Attribute paths
    AttrPath,
    AttrKey (..),

    -- * Bindings
    Binding (..),

    -- * Formals (function parameters)
    Formals (..),
    Formal (..),

    -- * Closure capture info
    CaptureInfo (..),

    -- * Operators
    UnaryOp (..),
    BinaryOp (..),

    -- * String parts (interpolation)
    StringPart (..),
  )
where

import Data.Text (Text)

-- | Atomic (literal) values.
data NixAtom
  = NixInt !Integer
  | NixFloat !Double
  | NixBool !Bool
  | NixNull
  | NixUri !Text
  | NixPath !Text
  deriving (Eq, Show)

-- | A part of an interpolated string (@"hello ${name}"@).
data StringPart
  = -- | Literal text.
    StrLit !Text
  | -- | Interpolated expression (@${expr}@).
    StrInterp !Expr
  deriving (Eq, Show)

-- | An attribute key: either a static identifier or a dynamic expression.
data AttrKey
  = -- | Static key: @{ foo = val; }@
    StaticKey !Text
  | -- | Dynamic key: @{ ${expr} = val; }@
    DynamicKey !Expr
  deriving (Eq, Show)

-- | Attribute path: @a.b.c@ is @[StaticKey "a", StaticKey "b", StaticKey "c"]@.
type AttrPath = [AttrKey]

-- | A binding in an attribute set or let expression.
data Binding
  = -- | @path = expr;@
    NamedBinding !AttrPath !Expr
  | -- | @inherit expr;@ or @inherit (from) attrs;@
    Inherit !(Maybe Expr) ![Text]
  deriving (Eq, Show)

-- | A single formal parameter with optional default.
data Formal = Formal
  { fName :: !Text,
    fDefault :: !(Maybe Expr)
  }
  deriving (Eq, Show)

-- | Function parameter pattern.
data Formals
  = -- | Single identifier: @x: body@
    FormalName !Text
  | -- | Attribute set pattern with optional ellipsis: @{ a, b }: body@
    FormalSet ![Formal] !Bool
  | -- | Named set pattern: @args\@{ a, b }: body@
    FormalNamedSet !Text ![Formal] !Bool
  deriving (Eq, Show)

-- | Closure capture information for lambda trimming.
--
-- After 'Nix.Expr.ClosureTrim.trimClosures', lambdas that only reference
-- a subset of their outer scope carry a 'Captures' list.  At eval time
-- this drives extraction of exactly those thunks into a flat 'Env',
-- breaking the retention chain to the full parent scope.
data CaptureInfo
  = -- | No capture analysis performed (lambda untrimmed).
    NoCaptureInfo
  | -- | Sorted list of @(level, index)@ pairs to extract from the parent env.
    Captures ![(Int, Int)]
  deriving (Eq, Show)

-- | Unary operators.
data UnaryOp
  = OpNot
  | OpNegate
  deriving (Eq, Show)

-- | Binary operators.
data BinaryOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpAnd
  | OpOr
  | OpImpl
  | OpEq
  | OpNeq
  | OpLt
  | OpLte
  | OpGt
  | OpGte
  | OpConcat
  | OpUpdate
  deriving (Eq, Show)

-- | The Nix expression AST.
--
-- This covers the full Nix language as documented in the Nix manual.
-- Every constructor is strict in its children to prevent thunk buildup
-- during parsing.
data Expr
  = -- | Literal value.
    ELit !NixAtom
  | -- | String with possible interpolations.
    EStr ![StringPart]
  | -- | Indented string (double single-quoted).
    EIndStr ![StringPart]
  | -- | Variable reference.
    EVar !Text
  | -- | Attribute set: @{ bindings }@ or @rec { bindings }@.
    EAttrs !Bool ![Binding]
  | -- | List: @[ e1 e2 e3 ]@.
    EList ![Expr]
  | -- | Attribute selection: @expr.attrpath@ or @expr.attrpath or default@.
    ESelect !Expr !AttrPath !(Maybe Expr)
  | -- | Has attribute: @expr ? attrpath@.
    EHasAttr !Expr !AttrPath
  | -- | Function application: @f x@.
    EApp !Expr !Expr
  | -- | Lambda: @formals: body@.
    --
    -- The 'CaptureInfo' field is populated by 'Nix.Expr.ClosureTrim.trimClosures'
    -- after variable resolution.  'NoCaptureInfo' means untrimmed (eval
    -- captures the full parent env); 'Captures' lists exactly which outer
    -- slots to extract into a flat env.
    ELambda !Formals !Expr !CaptureInfo
  | -- | Let binding: @let bindings in body@.
    ELet ![Binding] !Expr
  | -- | If-then-else: @if cond then t else f@.
    EIf !Expr !Expr !Expr
  | -- | With expression: @with expr; body@.
    EWith !Expr !Expr
  | -- | Assert: @assert cond; body@.
    EAssert !Expr !Expr
  | -- | Unary operator application.
    EUnary !UnaryOp !Expr
  | -- | Binary operator application.
    EBinary !BinaryOp !Expr !Expr
  | -- | Search path lookup: @\<nixpkgs\>@ is @ESearchPath "nixpkgs"@.
    ESearchPath !Text
  | -- | De Bruijn-style resolved variable: @(level, index)@.
    -- Produced by 'Nix.Expr.Resolve.resolveVars' for variables
    -- bound by lambda formals.  @level@ counts parent-chain hops;
    -- @index@ is the positional slot within that env's 'envSlots'.
    EResolvedVar !Int !Int
  deriving (Eq, Show)
