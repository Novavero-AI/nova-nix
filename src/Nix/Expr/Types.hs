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

    -- * Operators
    UnaryOp (..),
    BinaryOp (..),

    -- * String parts (interpolation)
    StringPart (..),

    -- * Source locations
    SrcPos (..),
    SrcSpan (..),

    -- * Free variable analysis
    freeVars,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Source position for error reporting.
data SrcPos = SrcPos
  { spLine :: !Int,
    spCol :: !Int
  }
  deriving (Eq, Show)

-- | Source span (start to end).
data SrcSpan = SrcSpan
  { ssFile :: !Text,
    ssStart :: !SrcPos,
    ssEnd :: !SrcPos
  }
  deriving (Eq, Show)

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
    ELambda !Formals !Expr
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
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Free variable analysis
-- ---------------------------------------------------------------------------

-- | Compute the set of free variables in an expression.
--
-- A variable is /free/ if it is referenced but not bound by an enclosing
-- @let@, @rec {}@, or lambda.  The result is a sound over-approximation:
-- it may report variables that are not actually used at runtime (e.g.
-- variables in dead branches of @if@), but it never misses one.  This
-- property is critical — under-approximation would cause 'mkThunk' to
-- trim a binding the thunk actually needs.
--
-- The @with@ construct is handled conservatively: we cannot statically
-- determine which names a @with@ scope provides, so body free variables
-- are left intact (not subtracted).
freeVars :: Expr -> Set Text
freeVars = go
  where
    go (ELit _) = Set.empty
    go (ESearchPath _) = Set.empty
    go (EVar name) = Set.singleton name
    go (EStr parts) = freeVarsStringParts parts
    go (EIndStr parts) = freeVarsStringParts parts
    go (EList exprs) = Set.unions (map go exprs)
    go (EUnary _ operand) = go operand
    go (EBinary _ left right) = go left <> go right
    go (EApp func arg) = go func <> go arg
    go (EIf cond thenE elseE) = go cond <> go thenE <> go elseE
    go (EAssert cond body) = go cond <> go body
    go (EWith scope body) = go scope <> go body
    go (ESelect target path defE) =
      go target <> freeVarsAttrPath path <> maybe Set.empty go defE
    go (EHasAttr target path) = go target <> freeVarsAttrPath path
    go (ELambda formals body) =
      let (bound, defaultFvs) = formalsBoundAndFree formals
       in (go body <> defaultFvs) `Set.difference` bound
    go (ELet bindings body) =
      let bound = boundNames bindings
       in (go body <> freeVarsBindingBodies bindings <> freeVarsDynamicKeys bindings)
            `Set.difference` bound
    go (EAttrs True bindings) =
      -- rec {}: bindings see each other, subtract bound names
      let bound = boundNames bindings
       in (freeVarsBindingBodies bindings <> freeVarsDynamicKeys bindings)
            `Set.difference` bound
    go (EAttrs False bindings) =
      -- non-rec {}: bindings only see outer scope, nothing subtracted
      freeVarsBindingBodies bindings <> freeVarsDynamicKeys bindings

    -- \| Collect free variables from string interpolation parts.
    freeVarsStringParts :: [StringPart] -> Set Text
    freeVarsStringParts = foldMap stringPartFvs
      where
        stringPartFvs (StrLit _) = Set.empty
        stringPartFvs (StrInterp e) = go e

    -- \| Collect free variables from dynamic keys in an attribute path.
    freeVarsAttrPath :: AttrPath -> Set Text
    freeVarsAttrPath = foldMap keyFvs
      where
        keyFvs (StaticKey _) = Set.empty
        keyFvs (DynamicKey e) = go e

    -- \| Collect free variables from the value expressions in bindings.
    freeVarsBindingBodies :: [Binding] -> Set Text
    freeVarsBindingBodies = foldMap bindingBodyFvs
      where
        bindingBodyFvs (NamedBinding _ bodyExpr) = go bodyExpr
        bindingBodyFvs (Inherit Nothing names) = Set.fromList names
        bindingBodyFvs (Inherit (Just fromExpr) _) = go fromExpr

    -- \| Collect free variables from dynamic keys in bindings.
    freeVarsDynamicKeys :: [Binding] -> Set Text
    freeVarsDynamicKeys = foldMap bindingKeyFvs
      where
        bindingKeyFvs (NamedBinding path _) = freeVarsAttrPath path
        bindingKeyFvs (Inherit _ _) = Set.empty

    -- \| Extract bound names from a binding list.
    -- For @NamedBinding (key : _) _@, the first key is the bound name.
    -- For @Inherit Nothing names@, each name is bound.
    -- For @Inherit (Just _) names@, each name is bound.
    boundNames :: [Binding] -> Set Text
    boundNames = foldMap bindingBound
      where
        bindingBound (NamedBinding (StaticKey k : _) _) = Set.singleton k
        bindingBound (NamedBinding _ _) = Set.empty
        bindingBound (Inherit _ names) = Set.fromList names

    -- \| Extract bound parameter names and free variables from defaults.
    formalsBoundAndFree :: Formals -> (Set Text, Set Text)
    formalsBoundAndFree (FormalName n) = (Set.singleton n, Set.empty)
    formalsBoundAndFree (FormalSet fs _) =
      ( Set.fromList (map fName fs),
        foldMap (maybe Set.empty go . fDefault) fs
      )
    formalsBoundAndFree (FormalNamedSet n fs _) =
      ( Set.insert n (Set.fromList (map fName fs)),
        foldMap (maybe Set.empty go . fDefault) fs
      )
