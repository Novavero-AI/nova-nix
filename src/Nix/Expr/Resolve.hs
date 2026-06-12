-- | Variable resolution pass: replaces 'EVar' with 'EResolvedVar'
-- for variables bound by lambda formals and let\/rec bindings.
--
-- Lambda formals and eligible let\/rec bindings get positional
-- (de Bruijn-style) indices via 'LexicalScope'.  Let\/rec blocks with
-- dynamic keys or nested paths fall back to 'NameBarrier' (name-based
-- lookup at runtime).  With-scopes and builtins remain name-based.
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
  = -- | Lambda formals: name -> positional index.
    LexicalScope !(Map Text Int)
  | -- | Let/rec binding names: blocks resolution (handled by name at runtime).
    NameBarrier !(Set Text)
  | -- | Marks a with-scope boundary on the stack.
    -- Does NOT increment the de Bruijn level (with doesn't create a
    -- parent env level at runtime).  Variables not found in any lexical
    -- scope below a WithBarrier are upgraded to 'EWithVar'.
    WithBarrier

-- | Resolve variables in an expression.  Replaces 'EVar' with
-- 'EResolvedVar' where the variable is lexically bound by a lambda
-- formal or an eligible let\/rec binding.  Variables in with-scopes,
-- builtins, and fallback let\/rec blocks remain as 'EVar'.
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
  EAttrs True bindings _captureInfo
    | allStaticSingleKey bindings ->
        -- Positional: all bindings are single static keys or inherits.
        let scope = lexicalScopeFromBindings bindings
            innerStack = scope : stack
         in EAttrs True (concatMap (resolveLetBinding stack innerStack) bindings) NoCaptureInfo
    | otherwise ->
        -- Fallback: dynamic keys or nested paths - use NameBarrier.
        let names = collectBindingNames bindings
            newStack = NameBarrier names : stack
         in EAttrs True (concatMap (resolveBinding newStack) bindings) NoCaptureInfo
  EAttrs False bindings _captureInfo ->
    -- Non-recursive: bindings use the outer scope.
    EAttrs False (concatMap (resolveBinding stack) bindings) NoCaptureInfo
  EList elems -> EList (map (resolve stack) elems)
  ESelect target path defExpr ->
    ESelect (resolve stack target) (map (resolveKey stack) path) (fmap (resolve stack) defExpr)
  EHasAttr target path ->
    EHasAttr (resolve stack target) (map (resolveKey stack) path)
  EApp f x -> EApp (resolve stack f) (resolve stack x)
  ELambda formals body _captures ->
    let scope = lexicalScopeFromFormals formals
        newStack = scope : stack
     in ELambda (resolveFormalsDefaults newStack formals) (resolve newStack body) NoCaptureInfo
  ELet bindings body _captureInfo
    | allStaticSingleKey bindings ->
        -- Positional: all bindings are single static keys or inherits.
        let scope = lexicalScopeFromBindings bindings
            innerStack = scope : stack
         in ELet (concatMap (resolveLetBinding stack innerStack) bindings) (resolve innerStack body) NoCaptureInfo
    | otherwise ->
        -- Fallback: dynamic keys or nested paths - use NameBarrier.
        let names = collectBindingNames bindings
            newStack = NameBarrier names : stack
         in ELet (concatMap (resolveBinding newStack) bindings) (resolve newStack body) NoCaptureInfo
  EIf c t f -> EIf (resolve stack c) (resolve stack t) (resolve stack f)
  EWithVar _ -> expr
  EWith scope body ->
    -- Push WithBarrier for the body so that unresolved names
    -- inside a with-scope are upgraded to EWithVar.
    EWith (resolve stack scope) (resolve (WithBarrier : stack) body)
  EAssert cond body ->
    EAssert (resolve stack cond) (resolve stack body)
  EUnary op operand -> EUnary op (resolve stack operand)
  EBinary op l r -> EBinary op (resolve stack l) (resolve stack r)
  -- Desugar: <name> becomes __findFile __nixPath "name"
  -- Matches C++ Nix's parser desugaring.  __findFile and __nixPath are
  -- in the root scope (Builtins.hs), so they resolve via name-based
  -- lookup at runtime.  This ensures closure trimming captures the
  -- implicit builtins dependency.
  ESearchPath name ->
    resolve
      stack
      ( EApp
          (EApp (EVar "__findFile") (EVar "__nixPath"))
          (EStr [StrLit name])
      )

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
resolveVar (WithBarrier : rest) level name =
  -- WithBarrier does NOT increment level (with doesn't create an env level).
  -- Continue searching lexical scopes below.  If the name is found in a
  -- lower LexicalScope, use that.  If it reaches the bottom as EVar
  -- (not found lexically), upgrade to EWithVar.
  case resolveVar rest level name of
    EVar _ -> EWithVar name
    resolved -> resolved

-- | Build a 'LexicalScope' from lambda formals.
--
-- Index assignment:
--
-- * @FormalName n@ becomes @[0: n]@
-- * @FormalSet [a, b, c] _@ becomes @[0: a, 1: b, 2: c]@ (declaration order)
-- * @FormalNamedSet n [a, b, c] _@ becomes @[0: n, 1: a, 2: b, 3: c]@ (\@ name first)
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
--
-- 'Inherit Nothing' is desugared into 'NamedBinding' entries so that
-- each inherited name goes through normal variable resolution.  This
-- is necessary because @inherit x@ does a name-based lookup at runtime,
-- but lambda formals are stored in positional 'envSlots' (no names).
-- Desugaring @inherit x@ to @x = x;@ lets the RHS 'EVar' resolve to
-- 'EResolvedVar' when @x@ is a lambda formal.
resolveBinding :: [ScopeEntry] -> Binding -> [Binding]
resolveBinding stack (NamedBinding path bodyExpr) =
  [NamedBinding (map (resolveKey stack) path) (resolve stack bodyExpr)]
resolveBinding stack (Inherit (Just fromExpr) names) =
  [Inherit (Just (resolve stack fromExpr)) names]
resolveBinding stack (Inherit Nothing names) =
  -- Desugar: inherit x y; becomes x = x; y = y;
  [NamedBinding [StaticKey name] (resolve stack (EVar name)) | name <- names]

-- | Check if all bindings are eligible for positional resolution:
-- each binding must be either a single static key or an inherit.
-- Blocks with dynamic keys (@${expr} = val@) or nested paths
-- (@a.b = val@) are ineligible and fall back to 'NameBarrier'.
allStaticSingleKey :: [Binding] -> Bool
allStaticSingleKey = all isEligible
  where
    isEligible (NamedBinding [StaticKey _] _) = True
    isEligible (Inherit _ _) = True
    isEligible _ = False

-- | Build a 'LexicalScope' from let\/rec bindings, assigning positional
-- indices in declaration order.  Inherits are expanded in-place (each
-- inherited name gets its own index).  Later duplicates win (via
-- 'Map.fromList' right-bias), matching Nix's last-definition-wins
-- semantics.
lexicalScopeFromBindings :: [Binding] -> ScopeEntry
lexicalScopeFromBindings bindings =
  LexicalScope (Map.fromList (zip names [0 ..]))
  where
    names = concatMap bindingNames bindings
    bindingNames (NamedBinding [StaticKey name] _) = [name]
    bindingNames (Inherit _ inheritNames) = inheritNames
    -- Unreachable: allStaticSingleKey guards this path.
    bindingNames _ = []

-- | Resolve bindings in a let\/rec block that uses positional resolution.
-- Takes two stacks: @outerStack@ (before the let scope) and @innerStack@
-- (with the let's 'LexicalScope' pushed).
--
-- Regular bindings resolve their RHS against @innerStack@ (recursive).
-- @inherit x@ desugars to @x = x@ where the RHS resolves against
-- @outerStack@ - the inherited name must reference the enclosing scope,
-- not the let scope being defined.
resolveLetBinding :: [ScopeEntry] -> [ScopeEntry] -> Binding -> [Binding]
resolveLetBinding _ innerStack (NamedBinding path bodyExpr) =
  [NamedBinding (map (resolveKey innerStack) path) (resolve innerStack bodyExpr)]
resolveLetBinding _ innerStack (Inherit (Just fromExpr) names) =
  [Inherit (Just (resolve innerStack fromExpr)) names]
resolveLetBinding outerStack _ (Inherit Nothing names) =
  -- Desugar @inherit x y;@ becomes @x = x; y = y;@, resolving each RHS against the
  -- outer scope so it names the enclosing binding, not the one defined here.
  -- 'shiftInheritLevel' corrects for the inner env the resulting thunk runs in.
  [NamedBinding [StaticKey name] (shiftInheritLevel (resolve outerStack (EVar name))) | name <- names]

-- | A desugared positional @inherit@ RHS is resolved against the outer scope
-- but evaluated in the inner (let\/rec) env - one extra parent-chain hop - so
-- its de Bruijn level is one too shallow.  Bump it.  'EVar'\/'EWithVar' are
-- name-based and need no adjustment.
shiftInheritLevel :: Expr -> Expr
shiftInheritLevel (EResolvedVar level idx) = EResolvedVar (level + 1) idx
shiftInheritLevel other = other

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
