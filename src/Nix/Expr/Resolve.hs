-- | The resolution passes run over a parsed expression before it is
-- evaluated: variables to positional slots, and relative path literals to
-- absolute ones.
--
-- Variable resolution replaces 'EVar' with 'EResolvedVar'
-- for variables bound by lambda formals and let\/rec bindings.
--
-- Lambda formals and eligible let\/rec bindings get positional
-- (de Bruijn-style) indices via 'LexicalScope'.  Let\/rec blocks with
-- dynamic keys or nested paths fall back to 'NameBarrier' (name-based
-- lookup at runtime).  With-scopes and builtins remain name-based.
--
-- Path resolution rewrites a relative path literal to an absolute one, so
-- what it names is fixed by the file it was written in.
--
-- Both are called once at parse time ('Nix.Parser.parseNix').
module Nix.Expr.Resolve
  ( resolveVars,
    resolveRelativePaths,

    -- * Static global names (exported for the sync test)
    staticGlobalNames,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Expr.Types
import System.FilePath (isRelative, (</>))

-- | Scope entry for static variable resolution.
data ScopeEntry
  = -- | Lambda formals: name -> positional index.
    LexicalScope !(Map Text Int)
  | -- | Let/rec binding names: blocks resolution (handled by name at runtime).
    NameBarrier !(Set Text)
  | -- | Marks a with-scope boundary on the stack.
    -- Does NOT increment the de Bruijn level (with doesn't create a
    -- parent env level at runtime).  Variables bound by no 'LexicalScope'
    -- or 'NameBarrier' anywhere on the stack - and not static globals -
    -- are upgraded to 'EWithVar' when at least one WithBarrier encloses
    -- them; lexical bindings and globals always win over with-scopes.
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
  EPathStr parts -> EPathStr (map (resolvePart stack) parts)
  EVar name -> resolveVar stack 0 name
  EResolvedVar _ _ -> expr
  EAttrs True bindings _captureInfo
    | allStaticSingleKey bindings ->
        -- Positional: all bindings are single static keys or inherits.
        let scope = lexicalScopeFromBindings bindings
            innerStack = scope : stack
         in EAttrs True (concatMap (resolveLetBinding stack innerStack) bindings) NoCaptureInfo
    | otherwise ->
        -- Fallback: dynamic keys or nested paths - use NameBarrier.  Bindings
        -- resolve against newStack (siblings visible), but a plain @inherit x@
        -- must reference the OUTER scope - resolveLetBinding handles that, so
        -- the barrier does not turn @inherit x@ into a self-reference.
        let names = collectBindingNames bindings
            newStack = NameBarrier names : stack
         in EAttrs True (concatMap (resolveLetBinding stack newStack) bindings) NoCaptureInfo
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
        -- Fallback: dynamic keys or nested paths - use NameBarrier.  As above,
        -- resolveLetBinding resolves a plain @inherit x@ against the outer scope
        -- so the barrier does not make @x@ self-referential.
        let names = collectBindingNames bindings
            newStack = NameBarrier names : stack
         in ELet (concatMap (resolveLetBinding stack newStack) bindings) (resolve newStack body) NoCaptureInfo
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
-- to one parent hop at runtime).  A 'LexicalScope' hit yields
-- 'EResolvedVar'; a 'NameBarrier' hit yields 'EVar' (name-based lookup at
-- runtime).  Both are LEXICAL bindings, so a hit ends the walk no matter
-- how many 'WithBarrier's were crossed on the way out - in Nix a
-- with-scope never shadows a binding introduced by other means.
--
-- Only a name bound by NEITHER becomes a with-variable, and then only if
-- it is not a static global: like C++ Nix's staticBaseEnv, a global
-- (@map@, @toString@, @builtins@, ...) binds at parse time, so
-- @with { map = 42; }; map@ is the builtin upstream and here.
resolveVar :: [ScopeEntry] -> Int -> Text -> Expr
resolveVar fullStack startLevel name = go fullStack startLevel False
  where
    go [] _ crossedWith
      | crossedWith && not (Set.member name staticGlobalNames) = EWithVar name
      | otherwise = EVar name
    go (LexicalScope scope : rest) level crossedWith =
      case Map.lookup name scope of
        Just idx -> EResolvedVar level idx
        Nothing -> go rest (level + 1) crossedWith
    go (NameBarrier names : rest) level crossedWith
      | Set.member name names = EVar name
      | otherwise = go rest (level + 1) crossedWith
    -- WithBarrier does NOT increment level (with doesn't create an env
    -- level at runtime); it only records that an enclosing with exists.
    go (WithBarrier : rest) level _ = go rest level True

-- | Names statically bound in the root environment
-- ('Nix.Builtins.builtinEnv'): the value constants, @builtins@, the
-- search-path plumbing, and the top-level builtins that upstream Nix also
-- exposes unprefixed (its staticBaseEnv).  A name in this set is never a
-- with-variable - the global binds at parse time and an enclosing @with@
-- cannot shadow it.
--
-- @fetchurl@ and @toFile@ are absent on purpose: upstream exposes them
-- only under @builtins.@, and nixpkgs relies on @with pkgs; fetchurl@
-- binding @pkgs.fetchurl@.  The root env matches (they are not bound
-- there either).
--
-- Layering keeps this module from importing 'Nix.Builtins'; a test
-- asserts this set stays in sync with the root env's actual bindings.
staticGlobalNames :: Set Text
staticGlobalNames =
  Set.fromList
    [ "true",
      "false",
      "null",
      "builtins",
      "__findFile",
      "__nixPath",
      "abort",
      "baseNameOf",
      "break",
      "derivation",
      "derivationStrict",
      "dirOf",
      "fetchGit",
      "fetchTarball",
      "fromTOML",
      "import",
      "isNull",
      "map",
      "placeholder",
      "removeAttrs",
      "scopedImport",
      "throw",
      "toString"
    ]

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

-- | Resolve bindings in a let\/rec block, for both the positional
-- ('LexicalScope') and fallback ('NameBarrier') paths.  Takes two stacks:
-- @outerStack@ (before the block) and @innerStack@ (with the block's scope
-- entry pushed).
--
-- Regular bindings resolve their RHS against @innerStack@ (recursive).
-- @inherit x@ desugars to @x = x@ where the RHS resolves against @outerStack@:
-- the inherited name must reference the enclosing scope, not the block being
-- defined - otherwise the pushed scope\/barrier makes @x@ a self-reference and
-- forcing it recurses forever.
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

-- ---------------------------------------------------------------------------
-- Path resolution
-- ---------------------------------------------------------------------------

-- | Rewrite every relative path literal to an absolute one, against the
-- directory of the file the expression was parsed from.
--
-- This is where upstream does it too, and it has to be: a path literal names
-- a location relative to the file it is written in, and nothing downstream
-- of the parser knows which file that was.  Resolving later means resolving
-- against whatever directory happens to be current when the value is forced,
-- which for a literal captured in a closure and forced inside an @import@ is
-- a different file's directory.
--
-- Upstream 2.24 spells it @absPath(path, state->basePath.path.abs())@ in the
-- @PATH@ production and 2.35 spells it @CanonPath(literal,
-- state->basePath.path).abs()@; the two are the same operation.
--
-- A @~\/@ literal is not handled here: it reaches the evaluator, which
-- expands it against the home directory.  Upstream expands it in the parser
-- and has to guard that with a pure-eval check, because reading @HOME@ while
-- parsing is an impurity.
resolveRelativePaths :: FilePath -> Expr -> Expr
resolveRelativePaths dir = goExpr
  where
    goExpr expr = case expr of
      ELit (NixPath p)
        -- A ~/ literal names a location under the home directory, not one
        -- relative to this file.  Joining it to the base would bury the
        -- tilde mid-path, where nothing expands it and the result names
        -- a directory literally called "~".  The evaluator resolves it
        -- against HOME instead; upstream does it in the parser and has to
        -- guard that with a pure-eval check for reading the environment.
        | homeRelative p -> expr
        | isRelative (T.unpack p) ->
            ELit (NixPath (T.pack (dir </> T.unpack p)))
      ELit _ -> expr
      EStr parts -> EStr (map goPart parts)
      EIndStr parts -> EIndStr (map goPart parts)
      -- The head piece of an interpolated path is static text and gets
      -- the same absolutization as a plain literal, for the same
      -- closure-capture reason; the interpolated pieces only recurse.
      EPathStr (StrLit headPiece : rest)
        | not (homeRelative headPiece),
          isRelative (T.unpack headPiece) ->
            EPathStr (StrLit (T.pack (dir </> T.unpack headPiece)) : map goPart rest)
      EPathStr parts -> EPathStr (map goPart parts)
      EVar _ -> expr
      EWithVar _ -> expr
      EResolvedVar _ _ -> expr
      EAttrs isRec bindings captureInfo -> EAttrs isRec (map goBinding bindings) captureInfo
      EList elems -> EList (map goExpr elems)
      ESelect target path mDef ->
        ESelect (goExpr target) (map goKey path) (fmap goExpr mDef)
      EHasAttr target path -> EHasAttr (goExpr target) (map goKey path)
      EApp f x -> EApp (goExpr f) (goExpr x)
      ELambda formals body captures -> ELambda (goFormals formals) (goExpr body) captures
      ELet bindings body captureInfo -> ELet (map goBinding bindings) (goExpr body) captureInfo
      EIf c t f -> EIf (goExpr c) (goExpr t) (goExpr f)
      EWith scope body -> EWith (goExpr scope) (goExpr body)
      EAssert cond body -> EAssert (goExpr cond) (goExpr body)
      EUnary op e -> EUnary op (goExpr e)
      EBinary op l r -> EBinary op (goExpr l) (goExpr r)
      ESearchPath _ -> expr

    goPart part = case part of
      StrLit _ -> part
      StrInterp e -> StrInterp (goExpr e)

    goBinding binding = case binding of
      NamedBinding path e -> NamedBinding (map goKey path) (goExpr e)
      Inherit from names -> Inherit (fmap goExpr from) names

    goKey key = case key of
      StaticKey _ -> key
      DynamicKey e -> DynamicKey (goExpr e)

    goFormals formals = case formals of
      FormalName _ -> formals
      FormalSet fs ellipsis -> FormalSet (map goFormal fs) ellipsis
      FormalNamedSet n fs ellipsis -> FormalNamedSet n (map goFormal fs) ellipsis

    goFormal (Formal n mDef) = Formal n (fmap goExpr mDef)

    homeRelative = T.isPrefixOf "~/"
