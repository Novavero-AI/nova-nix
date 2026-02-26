-- | Variable resolution pass: replaces 'EVar' with 'EResolvedVar'
-- for variables bound by lambda formals.
--
-- Lambda formals get positional (de Bruijn-style) indices.  Let\/rec
-- bindings act as name barriers (preventing resolution through them)
-- since they use 'envLazyScope' at runtime, which is already zero-cost.
-- With-scopes and builtins remain name-based.
--
-- Called once at parse time ('Nix.Parser.parseNix').
module Nix.Expr.Resolve
  ( resolveVars,
  )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.Expr.Types

-- | Scope entry for static variable resolution.
data ScopeEntry
  = -- | Lambda formals: name → positional index.
    LexicalScope !(Map Text Int)
  | -- | Let/rec binding names: blocks resolution (handled by name at runtime).
    NameBarrier !(Set Text)

-- | Resolve variables in an expression.  Replaces 'EVar' with
-- 'EResolvedVar' where the variable is lexically bound by a lambda
-- formal.  All other variables remain as 'EVar' for name-based lookup.
resolveVars :: Expr -> Expr
resolveVars = resolve []

-- | Walk the AST, maintaining a scope stack.
resolve :: [ScopeEntry] -> Expr -> Expr
resolve stack expr = case expr of
  ELit _ -> expr
  EStr parts -> EStr (map (resolvePart stack) parts)
  EIndStr parts -> EIndStr (map (resolvePart stack) parts)
  EVar name -> resolveVar stack 0 name
  EResolvedVar _ _ -> expr
  EAttrs True bindings ->
    -- Recursive: binding RHSes see their own names (as NameBarrier).
    let names = collectBindingNames bindings
        newStack = NameBarrier names : stack
     in EAttrs True (map (resolveBinding newStack) bindings)
  EAttrs False bindings ->
    -- Non-recursive: bindings use the outer scope.
    EAttrs False (map (resolveBinding stack) bindings)
  EList elems -> EList (map (resolve stack) elems)
  ESelect target path defExpr ->
    ESelect (resolve stack target) (map (resolveKey stack) path) (fmap (resolve stack) defExpr)
  EHasAttr target path ->
    EHasAttr (resolve stack target) (map (resolveKey stack) path)
  EApp f x -> EApp (resolve stack f) (resolve stack x)
  ELambda formals body ->
    let scope = lexicalScopeFromFormals formals
        newStack = scope : stack
     in ELambda (resolveFormalsDefaults newStack formals) (resolve newStack body)
  ELet bindings body ->
    -- Both body and binding RHSes see the let names (as NameBarrier).
    let names = collectBindingNames bindings
        newStack = NameBarrier names : stack
     in ELet (map (resolveBinding newStack) bindings) (resolve newStack body)
  EIf c t f -> EIf (resolve stack c) (resolve stack t) (resolve stack f)
  EWith scope body ->
    -- with-scope names are unknown statically; no scope change.
    EWith (resolve stack scope) (resolve stack body)
  EAssert cond body ->
    EAssert (resolve stack cond) (resolve stack body)
  EUnary op operand -> EUnary op (resolve stack operand)
  EBinary op l r -> EBinary op (resolve stack l) (resolve stack r)
  ESearchPath _ -> expr

-- | Resolve a variable by walking the scope stack.
--
-- @level@ counts how many scope entries we've crossed (each corresponds
-- to one parent hop at runtime).  If the name hits a 'LexicalScope',
-- we return 'EResolvedVar'.  If it hits a 'NameBarrier', we stop and
-- leave it as 'EVar' (name-based lookup at runtime).  If not found,
-- 'EVar' is returned unchanged (looked up via with-scopes or builtins).
resolveVar :: [ScopeEntry] -> Int -> Text -> Expr
resolveVar [] _ name = EVar name
resolveVar (LexicalScope scope : rest) level name =
  case Map.lookup name scope of
    Just idx -> EResolvedVar level idx
    Nothing -> resolveVar rest (level + 1) name
resolveVar (NameBarrier names : rest) level name =
  if Set.member name names
    then EVar name
    else resolveVar rest (level + 1) name

-- | Build a 'LexicalScope' from lambda formals.
--
-- Index assignment:
--
-- * @FormalName n@ → @[0: n]@
-- * @FormalSet [a, b, c] _@ → @[0: a, 1: b, 2: c]@ (declaration order)
-- * @FormalNamedSet n [a, b, c] _@ → @[0: n, 1: a, 2: b, 3: c]@ (\@ name first)
lexicalScopeFromFormals :: Formals -> ScopeEntry
lexicalScopeFromFormals (FormalName name) =
  LexicalScope (Map.singleton name 0)
lexicalScopeFromFormals (FormalSet formals _) =
  LexicalScope (Map.fromList (zip (map fName formals) [0 ..]))
lexicalScopeFromFormals (FormalNamedSet name formals _) =
  LexicalScope (Map.fromList ((name, 0) : zip (map fName formals) [1 ..]))

-- | Collect all top-level binding names for a 'NameBarrier'.
collectBindingNames :: [Binding] -> Set Text
collectBindingNames = foldl' addNames Set.empty
  where
    addNames acc (NamedBinding (StaticKey name : _) _) = Set.insert name acc
    addNames acc (NamedBinding _ _) = acc
    addNames acc (Inherit _ names) = foldl' (flip Set.insert) acc names

-- | Resolve variables inside string parts.
resolvePart :: [ScopeEntry] -> StringPart -> StringPart
resolvePart _ p@(StrLit _) = p
resolvePart stack (StrInterp e) = StrInterp (resolve stack e)

-- | Resolve variables inside attribute keys.
resolveKey :: [ScopeEntry] -> AttrKey -> AttrKey
resolveKey _ k@(StaticKey _) = k
resolveKey stack (DynamicKey e) = DynamicKey (resolve stack e)

-- | Resolve variables inside a binding's RHS.
resolveBinding :: [ScopeEntry] -> Binding -> Binding
resolveBinding stack (NamedBinding path bodyExpr) =
  NamedBinding (map (resolveKey stack) path) (resolve stack bodyExpr)
resolveBinding stack (Inherit (Just fromExpr) names) =
  Inherit (Just (resolve stack fromExpr)) names
resolveBinding _ b@(Inherit Nothing _) = b

-- | Resolve variables inside formal default expressions.
resolveFormalsDefaults :: [ScopeEntry] -> Formals -> Formals
resolveFormalsDefaults _ f@(FormalName _) = f
resolveFormalsDefaults stack (FormalSet formals ellipsis) =
  FormalSet (map (resolveFormal stack) formals) ellipsis
resolveFormalsDefaults stack (FormalNamedSet name formals ellipsis) =
  FormalNamedSet name (map (resolveFormal stack) formals) ellipsis

-- | Resolve variables inside a single formal's default expression.
resolveFormal :: [ScopeEntry] -> Formal -> Formal
resolveFormal stack (Formal name defExpr) =
  Formal name (fmap (resolve stack) defExpr)
