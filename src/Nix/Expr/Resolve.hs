-- | The resolution passes run over a parsed expression before it is
-- evaluated: variables to positional slots, and relative path literals to
-- absolute ones.
--
-- Variable resolution is upstream's bindVars.  Every variable is bound
-- before anything is evaluated, and one that no enclosing scope binds and
-- no enclosing @with@ could supply fails the whole expression then, even on
-- a branch evaluation would never take.
--
-- Lambda formals and eligible let\/rec bindings get positional
-- (de Bruijn-style) indices via 'LexicalScope' and become 'EResolvedVar'.
-- Let\/rec blocks with dynamic keys or nested paths fall back to
-- 'NameBarrier' (name-based lookup at runtime), as do the names bound
-- around the whole expression.
-- A name only a @with@ could supply becomes 'EWithVar'.
--
-- Path resolution rewrites a relative path literal to an absolute one, so
-- what it names is fixed by the file it was written in.
--
-- Both are called once at parse time ('Nix.Parser.parseNix').
module Nix.Expr.Resolve
  ( resolveVars,
    resolveRelativePaths,
    undefinedVariableMessage,

    -- * The names bound around every expression
    staticGlobalNames,
    impureOnlyGlobalNames,
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
  | -- | Names looked up by name at runtime: a let\/rec block's bindings, and
    -- at the bottom of the stack the names bound around the expression.
    NameBarrier !(Set Text)
  | -- | Marks a with-scope boundary on the stack.
    -- Does NOT increment the de Bruijn level (with doesn't create a
    -- parent env level at runtime).  Variables bound by no 'LexicalScope'
    -- or 'NameBarrier' anywhere on the stack are upgraded to 'EWithVar'
    -- when at least one WithBarrier encloses them; lexical bindings and
    -- globals always win over with-scopes.
    WithBarrier

-- | Bind every variable in an expression, given the names bound around it:
-- the root environment's ('staticGlobalNames', less
-- 'impureOnlyGlobalNames' under @pure-eval@), plus a @scopedImport@
-- scope's.  Replaces 'EVar' with 'EResolvedVar' where a lambda formal or an
-- eligible let\/rec binding binds the variable, and with 'EWithVar' where
-- only an enclosing @with@ could.  'Left' names the first unbound variable
-- the walk meets: the first in the source, unless attribute paths split
-- across bindings were merged.
resolveVars :: Set Text -> Expr -> Either Text Expr
resolveVars globals = resolve [NameBarrier globals]

-- | Upstream's wording for a variable nothing binds, the same whether
-- binding finds it before evaluation or a @with@ lookup misses it during
-- (UndefinedVarError in ExprVar::bindVars and in lookupVar).
undefinedVariableMessage :: Text -> Text
undefinedVariableMessage name = "undefined variable '" <> name <> "'"

-- | Walk the AST, maintaining a scope stack.
resolve :: [ScopeEntry] -> Expr -> Either Text Expr
resolve stack expr = case expr of
  ELit _ -> Right expr
  EStr parts -> EStr <$> traverse (resolvePart stack) parts
  EIndStr parts -> EIndStr <$> traverse (resolvePart stack) parts
  EPathStr parts -> EPathStr <$> traverse (resolvePart stack) parts
  EVar name -> resolveVar stack name
  EResolvedVar _ _ -> Right expr
  EAttrs True bindings _captureInfo
    | allStaticSingleKey bindings ->
        -- Positional: all bindings are single static keys or inherits.
        let innerStack = lexicalScopeFromBindings bindings : stack
         in recAttrs <$> traverse (resolveLetBinding stack innerStack) bindings
    | otherwise ->
        -- Fallback: dynamic keys or nested paths - use NameBarrier.  Bindings
        -- resolve against newStack (siblings visible), but a plain @inherit x@
        -- must reference the OUTER scope - resolveLetBinding handles that, so
        -- the barrier does not turn @inherit x@ into a self-reference.
        let newStack = NameBarrier (collectBindingNames bindings) : stack
         in recAttrs <$> traverse (resolveLetBinding stack newStack) bindings
  EAttrs False bindings _captureInfo ->
    -- Non-recursive: bindings use the outer scope.
    (\resolved -> EAttrs False resolved NoCaptureInfo) <$> traverse (resolveBinding stack) bindings
  EList elems -> EList <$> traverse (resolve stack) elems
  ESelect target path defExpr ->
    ESelect
      <$> resolve stack target
      <*> traverse (resolveKey stack) path
      <*> traverse (resolve stack) defExpr
  EHasAttr target path ->
    EHasAttr <$> resolve stack target <*> traverse (resolveKey stack) path
  EApp f x -> EApp <$> resolve stack f <*> resolve stack x
  EDeferredApp f x -> EDeferredApp <$> resolve stack f <*> resolve stack x
  ELambda formals body _captures ->
    let newStack = lexicalScopeFromFormals formals : stack
     in (\resolvedFormals resolvedBody -> ELambda resolvedFormals resolvedBody NoCaptureInfo)
          <$> resolveFormalsDefaults newStack formals
          <*> resolve newStack body
  ELet bindings body _captureInfo
    | allStaticSingleKey bindings ->
        -- Positional: all bindings are single static keys or inherits.
        let innerStack = lexicalScopeFromBindings bindings : stack
         in letIn
              <$> traverse (resolveLetBinding stack innerStack) bindings
              <*> resolve innerStack body
    | otherwise ->
        -- Fallback: dynamic keys or nested paths - use NameBarrier.  As above,
        -- resolveLetBinding resolves a plain @inherit x@ against the outer scope
        -- so the barrier does not make @x@ self-referential.
        let newStack = NameBarrier (collectBindingNames bindings) : stack
         in letIn
              <$> traverse (resolveLetBinding stack newStack) bindings
              <*> resolve newStack body
  EIf c t f -> EIf <$> resolve stack c <*> resolve stack t <*> resolve stack f
  EWithVar _ -> Right expr
  EWith scope body ->
    -- Push WithBarrier for the body so that unresolved names
    -- inside a with-scope are upgraded to EWithVar.
    EWith <$> resolve stack scope <*> resolve (WithBarrier : stack) body
  EAssert cond body ->
    EAssert <$> resolve stack cond <*> resolve stack body
  EUnary op operand -> EUnary op <$> resolve stack operand
  EBinary op l r -> EBinary op <$> resolve stack l <*> resolve stack r
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
  where
    recAttrs resolved = EAttrs True resolved NoCaptureInfo
    letIn resolved body = ELet resolved body NoCaptureInfo

-- | Resolve a variable by walking the scope stack.
--
-- The level counts how many scope entries we've crossed (each corresponds
-- to one parent hop at runtime).  A 'LexicalScope' hit yields
-- 'EResolvedVar'; a 'NameBarrier' hit yields 'EVar' (name-based lookup at
-- runtime).  Both are LEXICAL bindings, so a hit ends the walk no matter
-- how many 'WithBarrier's were crossed on the way out - in Nix a
-- with-scope never shadows a binding introduced by other means.  That
-- includes the globals at the bottom of the stack: like C++ Nix's
-- staticBaseEnv, a global (@map@, @toString@, @builtins@, ...) binds at
-- parse time, so @with { map = 42; }; map@ is the builtin upstream and
-- here.
--
-- Only a name bound by NEITHER becomes a with-variable, and only under a
-- @with@; with none to supply it, the name is undefined.
resolveVar :: [ScopeEntry] -> Text -> Either Text Expr
resolveVar fullStack name = go fullStack 0 False
  where
    go [] _ crossedWith
      | crossedWith = Right (EWithVar name)
      | name == curPosName = Right (EVar name)
      | otherwise = Left name
    go (LexicalScope scope : rest) level crossedWith =
      case Map.lookup name scope of
        Just idx -> Right (EResolvedVar level idx)
        Nothing -> go rest (level + 1) crossedWith
    go (NameBarrier names : rest) level crossedWith
      | Set.member name names = Right (EVar name)
      | otherwise = go rest (level + 1) crossedWith
    -- WithBarrier does NOT increment level (with doesn't create an env
    -- level at runtime); it only records that an enclosing with exists.
    go (WithBarrier : rest) level _ = go rest level True

-- | A temporary accommodation.  Upstream's parser reads @__curPos@ as a
-- position expression, never a variable (parser.y at 2.24.9, expr_simple),
-- so binding never sees it.  Here it still parses as a variable, because
-- building the position needs to know whether the source is a file
-- (upstream's position set) or a string (upstream's null), and 'parseNix'
-- carries only a name.  Until it does, the name is exempt from the check and
-- fails when forced, as before binding checked anything; nixpkgs uses it in
-- files every NixOS evaluation imports.  Delete the exemption when
-- 'parseNix' carries the file-or-string origin and the parser builds the
-- position.
curPosName :: Text
curPosName = "__curPos"

-- | The names upstream binds in its base environment by default, which is
-- every name the root environment ('Nix.Builtins.builtinEnv') binds and
-- every name binding accepts with nothing else in scope.  Upstream installs
-- each primop and constant under the name it registers (@addPrimOp@ and
-- @addConstant@, eval.cc at 2.24.9) and under that name less a @__@ prefix
-- in @builtins@, so @__typeOf@ and @builtins.typeOf@ are one value.  The
-- set is @createBaseEnv@'s (primops.cc) with every @RegisterPrimOp@ not
-- gated on an experimental feature, which leaves out @__fetchClosure@,
-- @__outputOf@, @fetchTree@ and the flake builtins, and with
-- @__importNative@ and @__exec@, which need
-- @allow-unsafe-native-code-during-evaluation@, left out too.
-- 'impureOnlyGlobalNames' are dropped from it under @pure-eval@.
--
-- A name here is never a with-variable: the global binds at parse time and
-- an enclosing @with@ cannot shadow it.  @fetchurl@ and @toFile@ are
-- @__@-prefixed upstream, so nixpkgs' @with pkgs; fetchurl@ binds
-- @pkgs.fetchurl@.
--
-- Layering keeps this module from importing 'Nix.Builtins', which builds
-- the root environment from this set; a test checks it against the list
-- recorded from upstream's source.
staticGlobalNames :: Set Text
staticGlobalNames =
  Set.fromList (unprefixedGlobals ++ map ("__" <>) prefixedGlobals)
  where
    unprefixedGlobals =
      [ "abort",
        "baseNameOf",
        "break",
        "builtins",
        "derivation",
        "derivationStrict",
        "dirOf",
        "false",
        "fetchGit",
        "fetchMercurial",
        "fetchTarball",
        "fromTOML",
        "import",
        "isNull",
        "map",
        "null",
        "placeholder",
        "removeAttrs",
        "scopedImport",
        "throw",
        "toString",
        "true"
      ]
    prefixedGlobals =
      [ "add",
        "addDrvOutputDependencies",
        "addErrorContext",
        "all",
        "any",
        "appendContext",
        "attrNames",
        "attrValues",
        "bitAnd",
        "bitOr",
        "bitXor",
        "catAttrs",
        "ceil",
        "compareVersions",
        "concatLists",
        "concatMap",
        "concatStringsSep",
        "convertHash",
        "currentSystem",
        "currentTime",
        "deepSeq",
        "div",
        "elem",
        "elemAt",
        "fetchurl",
        "filter",
        "filterSource",
        "findFile",
        "floor",
        "foldl'",
        "fromJSON",
        "functionArgs",
        "genList",
        "genericClosure",
        "getAttr",
        "getContext",
        "getEnv",
        "groupBy",
        "hasAttr",
        "hasContext",
        "hashFile",
        "hashString",
        "head",
        "intersectAttrs",
        "isAttrs",
        "isBool",
        "isFloat",
        "isFunction",
        "isInt",
        "isList",
        "isPath",
        "isString",
        "langVersion",
        "length",
        "lessThan",
        "listToAttrs",
        "mapAttrs",
        "match",
        "mul",
        "nixPath",
        "nixVersion",
        "parseDrvName",
        "partition",
        "path",
        "pathExists",
        "readDir",
        "readFile",
        "readFileType",
        "replaceStrings",
        "seq",
        "sort",
        "split",
        "splitVersion",
        "storeDir",
        "storePath",
        "stringLength",
        "sub",
        "substring",
        "tail",
        "toFile",
        "toJSON",
        "toPath",
        "toXML",
        "trace",
        "traceVerbose",
        "tryEval",
        "typeOf",
        "unsafeDiscardOutputDependency",
        "unsafeDiscardStringContext",
        "unsafeGetAttrPos",
        "warn",
        "zipAttrsWith"
      ]

-- | Upstream's @impureOnly@ constants, absent from its base environment
-- under @pure-eval@ (@addConstant@, eval.cc at 2.24.9), so binding rejects
-- them there as an undefined variable.
impureOnlyGlobalNames :: Set Text
impureOnlyGlobalNames = Set.fromList ["__currentSystem", "__currentTime"]

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
    addNames acc (Inherit name _) = Set.insert name acc
    addNames acc (InheritFrom _ names) = foldl' (flip Set.insert) acc names

-- | Resolve variables inside string parts.
resolvePart :: [ScopeEntry] -> StringPart -> Either Text StringPart
resolvePart _ p@(StrLit _) = Right p
resolvePart _ p@(StrEsc _) = Right p
resolvePart stack (StrInterp e) = StrInterp <$> resolve stack e

-- | Resolve variables inside attribute keys.
resolveKey :: [ScopeEntry] -> AttrKey -> Either Text AttrKey
resolveKey _ k@(StaticKey _) = Right k
resolveKey stack (DynamicKey e) = DynamicKey <$> resolve stack e

-- | Resolve variables inside a non-recursive set's binding.  The set adds
-- no env level, so an inherited variable resolves where the set stands,
-- like every other value.
resolveBinding :: [ScopeEntry] -> Binding -> Either Text Binding
resolveBinding stack (NamedBinding path bodyExpr) =
  NamedBinding <$> traverse (resolveKey stack) path <*> resolve stack bodyExpr
resolveBinding stack (Inherit name var) = Inherit name <$> resolve stack var
resolveBinding stack (InheritFrom fromExpr names) =
  (`InheritFrom` names) <$> resolve stack fromExpr

-- | Check if all bindings are eligible for positional resolution:
-- each binding must be either a single static key or an inherit.
-- Blocks with dynamic keys (@${expr} = val@) or nested paths
-- (@a.b = val@) are ineligible and fall back to 'NameBarrier'.
allStaticSingleKey :: [Binding] -> Bool
allStaticSingleKey = all isEligible
  where
    isEligible (NamedBinding [StaticKey _] _) = True
    isEligible (Inherit _ _) = True
    isEligible (InheritFrom _ _) = True
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
    bindingNames (Inherit name _) = [name]
    bindingNames (InheritFrom _ inheritNames) = inheritNames
    -- Unreachable: allStaticSingleKey guards this path.
    bindingNames _ = []

-- | Resolve bindings in a let\/rec block, for both the positional
-- ('LexicalScope') and fallback ('NameBarrier') paths.  Takes two stacks:
-- @outerStack@ (before the block) and @innerStack@ (with the block's scope
-- entry pushed).
--
-- Regular bindings resolve their RHS against @innerStack@ (recursive).
-- An @inherit x@ variable resolves against @outerStack@, and the evaluator
-- evaluates it in the enclosing env, as upstream's @ExprAttrs::eval@ and
-- @ExprLet::eval@ evaluate an Inherited attribute (eval.cc at 2.24.9):
-- the inherited name references the enclosing scope, never the block being
-- defined, whose own @x@ would make it a self-reference.
resolveLetBinding :: [ScopeEntry] -> [ScopeEntry] -> Binding -> Either Text Binding
resolveLetBinding _ innerStack (NamedBinding path bodyExpr) =
  NamedBinding <$> traverse (resolveKey innerStack) path <*> resolve innerStack bodyExpr
resolveLetBinding _ innerStack (InheritFrom fromExpr names) =
  (`InheritFrom` names) <$> resolve innerStack fromExpr
resolveLetBinding outerStack _ (Inherit name var) =
  Inherit name <$> resolve outerStack var

-- | Resolve variables inside formal default expressions.
resolveFormalsDefaults :: [ScopeEntry] -> Formals -> Either Text Formals
resolveFormalsDefaults _ f@(FormalName _) = Right f
resolveFormalsDefaults stack (FormalSet formals ellipsis) =
  (`FormalSet` ellipsis) <$> traverse (resolveFormal stack) formals
resolveFormalsDefaults stack (FormalNamedSet name formals ellipsis) =
  (\resolved -> FormalNamedSet name resolved ellipsis) <$> traverse (resolveFormal stack) formals

-- | Resolve variables inside a single formal's default expression.
resolveFormal :: [ScopeEntry] -> Formal -> Either Text Formal
resolveFormal stack (Formal name defExpr) =
  Formal name <$> traverse (resolve stack) defExpr

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
      EDeferredApp f x -> EDeferredApp (goExpr f) (goExpr x)
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
      StrEsc _ -> part
      StrInterp e -> StrInterp (goExpr e)

    goBinding binding = case binding of
      NamedBinding path e -> NamedBinding (map goKey path) (goExpr e)
      Inherit name var -> Inherit name (goExpr var)
      InheritFrom from names -> InheritFrom (goExpr from) names

    goKey key = case key of
      StaticKey _ -> key
      DynamicKey e -> DynamicKey (goExpr e)

    goFormals formals = case formals of
      FormalName _ -> formals
      FormalSet fs ellipsis -> FormalSet (map goFormal fs) ellipsis
      FormalNamedSet n fs ellipsis -> FormalNamedSet n (map goFormal fs) ellipsis

    goFormal (Formal n mDef) = Formal n (fmap goExpr mDef)

    homeRelative = T.isPrefixOf "~/"
