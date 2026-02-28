-- | Closure trimming pass: rewrites lambdas, let blocks, and recursive
-- attr sets to capture only the free variables they actually use.
--
-- Without trimming, every 'VLambda' and every let\/rec binding thunk
-- retains the ENTIRE parent 'Env' chain.  A scope using 3 variables
-- retains a 30k-entry nixpkgs scope.  This pass, run after
-- 'resolveVars', attaches a 'Captures' list to each trimmable
-- 'ELambda', 'ELet', and recursive 'EAttrs' so the evaluator can
-- build a flat env with @envParent = Nothing@, breaking the retention
-- chain.
--
-- A scope is trimmable when its body contains only 'EResolvedVar'
-- references to outer scopes (no 'EVar' that resolves through the
-- parent chain).  Scopes with outer 'EVar' references (with-scope
-- lookups, non-positional bindings) cannot be trimmed because those
-- require walking the full parent chain at runtime.
module Nix.Expr.ClosureTrim
  ( trimClosures,
  )
where

import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.Expr.Types

-- | Walk the AST and attach 'Captures' info to trimmable scopes
-- (lambdas, let blocks, and recursive attr sets).
trimClosures :: Expr -> Expr
trimClosures = trimExpr Set.empty

-- | Walk expression, tracking names bound in enclosing inner scopes
-- (let/rec/lambda formals) so we can tell if an 'EVar' escapes to
-- an outer scope.
--
-- @innerNames@ accumulates names from scopes INSIDE the current
-- lambda being analyzed.  It resets to empty when we enter a new
-- lambda (since that lambda's body is the new analysis target).
trimExpr :: Set Text -> Expr -> Expr
trimExpr innerNames expr = case expr of
  ELit {} -> expr
  EStr parts -> EStr (map (trimPart innerNames) parts)
  EIndStr parts -> EIndStr (map (trimPart innerNames) parts)
  EVar {} -> expr
  EResolvedVar {} -> expr
  EAttrs True bindings captureInfo ->
    let names = collectBindingNames bindings
        innerNamesRec = Set.union innerNames names
        trimmedBindings = map (trimBinding innerNamesRec) bindings
     in case captureInfo of
          Captures {} -> EAttrs True trimmedBindings captureInfo
          NoCaptureInfo ->
            case trimOneRecAttrs trimmedBindings of
              Left unchanged -> EAttrs True unchanged NoCaptureInfo
              Right (captureList, rewrittenBindings) ->
                EAttrs True rewrittenBindings (Captures captureList)
  EAttrs False bindings _captureInfo ->
    EAttrs False (map (trimBinding innerNames) bindings) NoCaptureInfo
  EList elems -> EList (map (trimExpr innerNames) elems)
  ESelect target path defExpr ->
    ESelect
      (trimExpr innerNames target)
      (map (trimKey innerNames) path)
      (fmap (trimExpr innerNames) defExpr)
  EHasAttr target path ->
    EHasAttr (trimExpr innerNames target) (map (trimKey innerNames) path)
  EApp f x -> EApp (trimExpr innerNames f) (trimExpr innerNames x)
  ELambda formals body captures ->
    -- First, recursively trim nested lambdas in the body.
    -- Reset innerNames since we're entering a new lambda scope.
    let formalNames = formalsNames formals
        trimmedFormals = trimFormals formals
        trimmedBody = trimExpr formalNames body
     in case captures of
          Captures {} -> ELambda trimmedFormals trimmedBody captures
          NoCaptureInfo ->
            case trimOneLambda trimmedFormals trimmedBody of
              Left unchanged -> unchanged
              Right (captureList, rewrittenFormals, rewrittenBody) ->
                ELambda rewrittenFormals rewrittenBody (Captures captureList)
  ELet bindings body captureInfo ->
    let names = collectBindingNames bindings
        innerNamesLet = Set.union innerNames names
        trimmedBindings = map (trimBinding innerNamesLet) bindings
        trimmedBody = trimExpr innerNamesLet body
     in case captureInfo of
          Captures {} -> ELet trimmedBindings trimmedBody captureInfo
          NoCaptureInfo ->
            case trimOneLetBlock trimmedBindings trimmedBody of
              Left (unchangedBindings, unchangedBody) ->
                ELet unchangedBindings unchangedBody NoCaptureInfo
              Right (captureList, rewrittenBindings, rewrittenBody) ->
                ELet rewrittenBindings rewrittenBody (Captures captureList)
  EIf c t f ->
    EIf (trimExpr innerNames c) (trimExpr innerNames t) (trimExpr innerNames f)
  EWith scope body ->
    EWith (trimExpr innerNames scope) (trimExpr innerNames body)
  EAssert cond body ->
    EAssert (trimExpr innerNames cond) (trimExpr innerNames body)
  EUnary op operand -> EUnary op (trimExpr innerNames operand)
  EBinary op l r -> EBinary op (trimExpr innerNames l) (trimExpr innerNames r)
  ESearchPath {} -> expr

trimPart :: Set Text -> StringPart -> StringPart
trimPart _ p@(StrLit _) = p
trimPart innerNames (StrInterp e) = StrInterp (trimExpr innerNames e)

trimKey :: Set Text -> AttrKey -> AttrKey
trimKey _ k@(StaticKey _) = k
trimKey innerNames (DynamicKey e) = DynamicKey (trimExpr innerNames e)

trimBinding :: Set Text -> Binding -> Binding
trimBinding innerNames (NamedBinding path bodyExpr) =
  NamedBinding (map (trimKey innerNames) path) (trimExpr innerNames bodyExpr)
trimBinding innerNames (Inherit (Just fromExpr) names) =
  Inherit (Just (trimExpr innerNames fromExpr)) names
trimBinding _ b@(Inherit Nothing _) = b

trimFormals :: Formals -> Formals
trimFormals f@(FormalName _) = f
trimFormals (FormalSet formals ellipsis) =
  FormalSet (map trimFormal formals) ellipsis
trimFormals (FormalNamedSet name formals ellipsis) =
  FormalNamedSet name (map trimFormal formals) ellipsis

trimFormal :: Formal -> Formal
trimFormal (Formal name defExpr) =
  -- Default expressions are evaluated in the lambda's own scope,
  -- so they get fresh innerNames (just the formals themselves).
  -- But we already trimmed nested lambdas, and defaults are part
  -- of the lambda's body — they'll be captured correctly.
  Formal name (fmap (trimExpr Set.empty) defExpr)

-- | Analyze a single lambda body and formals defaults, produce a capture
-- list + rewritten body + rewritten formals, or return the original
-- lambda unchanged if untrimable.
--
-- Formals defaults must be included because they are evaluated in the
-- lambda's env: a default like @{ x ? outerVar }: x@ references the
-- closure env, which for a trimmed lambda is the flat capture env.
-- Missing captures for defaults would cause out-of-bounds slot lookups.
trimOneLambda :: Formals -> Expr -> Either Expr ([(Int, Int)], Formals, Expr)
trimOneLambda formals body =
  let (bodyVars, bodyHasEVar) = collectFreeVars 0 body
      (formalVars, formalHasEVar) = foldFormalsDefaults 0 formals
      freeVars = Set.union bodyVars formalVars
      hasOuterEVar = bodyHasEVar || formalHasEVar
   in if hasOuterEVar || Set.null freeVars
        then Left (ELambda formals body NoCaptureInfo)
        else
          let captureList = sortBy (comparing fst <> comparing snd) (Set.toList freeVars)
              captureMap = Map.fromList (zip captureList [0 ..])
              rewrittenBody = rewriteBody 0 captureMap body
              rewrittenFormals = rewriteFormals 0 captureMap formals
           in Right (captureList, rewrittenFormals, rewrittenBody)

-- | Collect free variable references from a lambda body.
--
-- Returns @(freeVars, hasOuterEVar)@ where:
-- * @freeVars@ is the set of @(level, index)@ pairs for 'EResolvedVar'
--   references that point outside the lambda (level > current depth)
-- * @hasOuterEVar@ is 'True' if there's any 'EVar' that could reference
--   the outer scope (not shadowed by inner let/rec/lambda scopes)
--
-- @depth@ tracks how many scope-creating constructs we've entered
-- inside the lambda body (nested lambdas, let, rec attrs).
collectFreeVars :: Int -> Expr -> (Set (Int, Int), Bool)
collectFreeVars depth expr = case expr of
  ELit {} -> (Set.empty, False)
  EStr parts -> foldParts depth parts
  EIndStr parts -> foldParts depth parts
  EVar _ ->
    -- Any EVar in the body means we need the parent chain for name lookup.
    -- This makes the lambda untrimable.
    (Set.empty, True)
  EResolvedVar level idx
    | level > depth -> (Set.singleton (level - depth - 1, idx), False)
    | otherwise -> (Set.empty, False)
  EAttrs True bindings captureInfo ->
    -- Recursive attrs create a scope (depth + 1)
    let (vs1, h1) = foldBindings (depth + 1) bindings
        vs2 = case captureInfo of
          NoCaptureInfo -> Set.empty
          Captures caps ->
            Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth]
     in (Set.union vs1 vs2, h1)
  EAttrs False bindings _captureInfo ->
    -- Non-recursive attrs: no new scope
    foldBindings depth bindings
  EList elems -> foldExprs depth elems
  ESelect target path defExpr ->
    let (vs1, h1) = collectFreeVars depth target
        (vs2, h2) = foldKeys depth path
        (vs3, h3) = case defExpr of
          Nothing -> (Set.empty, False)
          Just e -> collectFreeVars depth e
     in (Set.unions [vs1, vs2, vs3], h1 || h2 || h3)
  EHasAttr target path ->
    let (vs1, h1) = collectFreeVars depth target
        (vs2, h2) = foldKeys depth path
     in (Set.union vs1 vs2, h1 || h2)
  EApp f x ->
    let (vs1, h1) = collectFreeVars depth f
        (vs2, h2) = collectFreeVars depth x
     in (Set.union vs1 vs2, h1 || h2)
  ELambda formals body_ captures ->
    -- Nested lambda creates a LexicalScope (depth + 1).
    -- If the inner lambda was already trimmed (has Captures), its capture
    -- coordinates are outer references that we must propagate upward —
    -- otherwise an outer lambda won't know it needs those variables.
    let (vs1, h1) = foldFormalsDefaults (depth + 1) formals
        (vs2, h2) = collectFreeVars (depth + 1) body_
        vs3 = case captures of
          NoCaptureInfo -> Set.empty
          Captures caps ->
            Set.fromList
              [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth]
     in (Set.unions [vs1, vs2, vs3], h1 || h2)
  ELet bindings body_ captureInfo ->
    -- Let creates a scope (depth + 1)
    let (vs1, h1) = foldBindings (depth + 1) bindings
        (vs2, h2) = collectFreeVars (depth + 1) body_
        vs3 = case captureInfo of
          NoCaptureInfo -> Set.empty
          Captures caps ->
            Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth]
     in (Set.unions [vs1, vs2, vs3], h1 || h2)
  EIf c t f ->
    let (vs1, h1) = collectFreeVars depth c
        (vs2, h2) = collectFreeVars depth t
        (vs3, h3) = collectFreeVars depth f
     in (Set.unions [vs1, vs2, vs3], h1 || h2 || h3)
  EWith scope body_ ->
    -- With does NOT create a scope frame
    let (vs1, h1) = collectFreeVars depth scope
        (vs2, h2) = collectFreeVars depth body_
     in (Set.union vs1 vs2, h1 || h2)
  EAssert cond body_ ->
    let (vs1, h1) = collectFreeVars depth cond
        (vs2, h2) = collectFreeVars depth body_
     in (Set.union vs1 vs2, h1 || h2)
  EUnary _ operand -> collectFreeVars depth operand
  EBinary _ l r ->
    let (vs1, h1) = collectFreeVars depth l
        (vs2, h2) = collectFreeVars depth r
     in (Set.union vs1 vs2, h1 || h2)
  ESearchPath {} -> (Set.empty, False)

foldExprs :: Int -> [Expr] -> (Set (Int, Int), Bool)
foldExprs depth = foldl' combine (Set.empty, False)
  where
    combine (!acc, !h) e =
      let (vs, hv) = collectFreeVars depth e
       in (Set.union acc vs, h || hv)

foldParts :: Int -> [StringPart] -> (Set (Int, Int), Bool)
foldParts depth = foldl' combine (Set.empty, False)
  where
    combine (!acc, !h) (StrLit _) = (acc, h)
    combine (!acc, !h) (StrInterp e) =
      let (vs, hv) = collectFreeVars depth e
       in (Set.union acc vs, h || hv)

foldKeys :: Int -> [AttrKey] -> (Set (Int, Int), Bool)
foldKeys depth = foldl' combine (Set.empty, False)
  where
    combine (!acc, !h) (StaticKey _) = (acc, h)
    combine (!acc, !h) (DynamicKey e) =
      let (vs, hv) = collectFreeVars depth e
       in (Set.union acc vs, h || hv)

foldBindings :: Int -> [Binding] -> (Set (Int, Int), Bool)
foldBindings depth = foldl' combine (Set.empty, False)
  where
    combine (!acc, !h) (NamedBinding path bodyExpr) =
      let (vs1, h1) = foldKeys depth path
          (vs2, h2) = collectFreeVars depth bodyExpr
       in (Set.unions [acc, vs1, vs2], h || h1 || h2)
    combine (!acc, !h) (Inherit (Just fromExpr) _) =
      let (vs, hv) = collectFreeVars depth fromExpr
       in (Set.union acc vs, h || hv)
    combine (!acc, !h) (Inherit Nothing _) = (acc, h)

foldFormalsDefaults :: Int -> Formals -> (Set (Int, Int), Bool)
foldFormalsDefaults _ (FormalName _) = (Set.empty, False)
foldFormalsDefaults depth (FormalSet formals _) = foldFormals depth formals
foldFormalsDefaults depth (FormalNamedSet _ formals _) = foldFormals depth formals

foldFormals :: Int -> [Formal] -> (Set (Int, Int), Bool)
foldFormals depth = foldl' combine (Set.empty, False)
  where
    combine (!acc, !h) (Formal _ Nothing) = (acc, h)
    combine (!acc, !h) (Formal _ (Just defExpr)) =
      let (vs, hv) = collectFreeVars depth defExpr
       in (Set.union acc vs, h || hv)

-- | Rewrite 'EResolvedVar' references in a lambda body to point at
-- capture positions instead of parent-chain levels.
--
-- An outer reference @EResolvedVar level idx@ where @level > depth@
-- maps to @EResolvedVar (depth + 1) captureIdx@.  The @depth + 1@
-- accounts for the fact that 'matchFormals' creates an argEnv with
-- @envParent = trimmedEnv@, so level 0 = formals, level 1 = captures.
rewriteBody :: Int -> Map (Int, Int) Int -> Expr -> Expr
rewriteBody depth captureMap expr = case expr of
  ELit {} -> expr
  EStr parts -> EStr (map (rewritePart depth captureMap) parts)
  EIndStr parts -> EIndStr (map (rewritePart depth captureMap) parts)
  EVar {} -> expr
  EResolvedVar level idx
    | level > depth ->
        let outerKey = (level - depth - 1, idx)
         in case Map.lookup outerKey captureMap of
              Just captureIdx -> EResolvedVar (depth + 1) captureIdx
              -- Unreachable: collectFreeVars found all outer refs
              Nothing -> expr
    | otherwise -> expr
  EAttrs True bindings captureInfo ->
    EAttrs True (map (rewriteBinding (depth + 1) captureMap) bindings) (rewriteCaptures depth captureMap captureInfo)
  EAttrs False bindings captureInfo ->
    EAttrs False (map (rewriteBinding depth captureMap) bindings) captureInfo
  EList elems -> EList (map (rewriteBody depth captureMap) elems)
  ESelect target path defExpr ->
    ESelect
      (rewriteBody depth captureMap target)
      (map (rewriteKey depth captureMap) path)
      (fmap (rewriteBody depth captureMap) defExpr)
  EHasAttr target path ->
    EHasAttr
      (rewriteBody depth captureMap target)
      (map (rewriteKey depth captureMap) path)
  EApp f x ->
    EApp (rewriteBody depth captureMap f) (rewriteBody depth captureMap x)
  ELambda formals body_ captures ->
    ELambda
      (rewriteFormals (depth + 1) captureMap formals)
      (rewriteBody (depth + 1) captureMap body_)
      (rewriteCaptures depth captureMap captures)
  ELet bindings body_ captureInfo ->
    ELet
      (map (rewriteBinding (depth + 1) captureMap) bindings)
      (rewriteBody (depth + 1) captureMap body_)
      (rewriteCaptures depth captureMap captureInfo)
  EIf c t f ->
    EIf
      (rewriteBody depth captureMap c)
      (rewriteBody depth captureMap t)
      (rewriteBody depth captureMap f)
  EWith scope body_ ->
    EWith (rewriteBody depth captureMap scope) (rewriteBody depth captureMap body_)
  EAssert cond body_ ->
    EAssert (rewriteBody depth captureMap cond) (rewriteBody depth captureMap body_)
  EUnary op operand -> EUnary op (rewriteBody depth captureMap operand)
  EBinary op l r ->
    EBinary op (rewriteBody depth captureMap l) (rewriteBody depth captureMap r)
  ESearchPath {} -> expr

rewritePart :: Int -> Map (Int, Int) Int -> StringPart -> StringPart
rewritePart _ _ p@(StrLit _) = p
rewritePart depth captureMap (StrInterp e) =
  StrInterp (rewriteBody depth captureMap e)

rewriteKey :: Int -> Map (Int, Int) Int -> AttrKey -> AttrKey
rewriteKey _ _ k@(StaticKey _) = k
rewriteKey depth captureMap (DynamicKey e) =
  DynamicKey (rewriteBody depth captureMap e)

rewriteBinding :: Int -> Map (Int, Int) Int -> Binding -> Binding
rewriteBinding depth captureMap (NamedBinding path bodyExpr) =
  NamedBinding
    (map (rewriteKey depth captureMap) path)
    (rewriteBody depth captureMap bodyExpr)
rewriteBinding depth captureMap (Inherit (Just fromExpr) names) =
  Inherit (Just (rewriteBody depth captureMap fromExpr)) names
rewriteBinding _ _ b@(Inherit Nothing _) = b

rewriteFormals :: Int -> Map (Int, Int) Int -> Formals -> Formals
rewriteFormals _ _ f@(FormalName _) = f
rewriteFormals depth captureMap (FormalSet formals ellipsis) =
  FormalSet (map (rewriteFormal depth captureMap) formals) ellipsis
rewriteFormals depth captureMap (FormalNamedSet name formals ellipsis) =
  FormalNamedSet name (map (rewriteFormal depth captureMap) formals) ellipsis

rewriteFormal :: Int -> Map (Int, Int) Int -> Formal -> Formal
rewriteFormal depth captureMap (Formal name defExpr) =
  Formal name (fmap (rewriteBody depth captureMap) defExpr)

-- | Rewrite capture coordinates on an already-trimmed inner lambda.
--
-- When an outer lambda is trimmed, inner lambdas that were trimmed in a
-- previous pass hold capture coordinates relative to the old env layout.
-- Those coordinates must be remapped to point at the outer lambda's
-- capture slots instead.
rewriteCaptures :: Int -> Map (Int, Int) Int -> CaptureInfo -> CaptureInfo
rewriteCaptures _ _ NoCaptureInfo = NoCaptureInfo
rewriteCaptures depth captureMap (Captures caps) =
  Captures (map rewriteOne caps)
  where
    rewriteOne (lvl, idx)
      | lvl > depth =
          let outerKey = (lvl - depth - 1, idx)
           in case Map.lookup outerKey captureMap of
                Just captureIdx -> (depth + 1, captureIdx)
                -- Unreachable: collectFreeVars propagated all inner captures
                Nothing -> (lvl, idx)
      | otherwise = (lvl, idx)

-- | Extract names introduced by lambda formals.
formalsNames :: Formals -> Set Text
formalsNames (FormalName name) = Set.singleton name
formalsNames (FormalSet formals _) =
  Set.fromList (map fName formals)
formalsNames (FormalNamedSet name formals _) =
  Set.insert name (Set.fromList (map fName formals))

-- | Analyze a let block's binding bodies + body for parent references,
-- produce a capture list + rewritten bindings + body, or return the
-- originals unchanged if untrimable.
--
-- Level 0 = self-references within the let scope (untouched).
-- Level 1+ = parent chain references (trimmed).
trimOneLetBlock :: [Binding] -> Expr -> Either ([Binding], Expr) ([(Int, Int)], [Binding], Expr)
trimOneLetBlock bindings body =
  let (bindingVars, bindingHasEVar) = foldBindings 0 bindings
      (bodyVars, bodyHasEVar) = collectFreeVars 0 body
      freeVars = Set.union bindingVars bodyVars
      hasOuterEVar = bindingHasEVar || bodyHasEVar
   in if hasOuterEVar || Set.null freeVars
        then Left (bindings, body)
        else
          let captureList = sortBy (comparing fst <> comparing snd) (Set.toList freeVars)
              captureMap = Map.fromList (zip captureList [0 ..])
              rewrittenBindings = map (rewriteBinding 0 captureMap) bindings
              rewrittenBody = rewriteBody 0 captureMap body
           in Right (captureList, rewrittenBindings, rewrittenBody)

-- | Analyze a recursive attr set's bindings for parent references,
-- produce a capture list + rewritten bindings, or return the originals
-- unchanged if untrimable.
trimOneRecAttrs :: [Binding] -> Either [Binding] ([(Int, Int)], [Binding])
trimOneRecAttrs bindings =
  let (bindingVars, bindingHasEVar) = foldBindings 0 bindings
   in if bindingHasEVar || Set.null bindingVars
        then Left bindings
        else
          let captureList = sortBy (comparing fst <> comparing snd) (Set.toList bindingVars)
              captureMap = Map.fromList (zip captureList [0 ..])
              rewrittenBindings = map (rewriteBinding 0 captureMap) bindings
           in Right (captureList, rewrittenBindings)

-- | Collect top-level binding names (mirrors 'Nix.Expr.Resolve.collectBindingNames').
collectBindingNames :: [Binding] -> Set Text
collectBindingNames = foldl' addNames Set.empty
  where
    addNames acc (NamedBinding (StaticKey name : _) _) = Set.insert name acc
    addNames acc (NamedBinding _ _) = acc
    addNames acc (Inherit _ names) = foldl' (flip Set.insert) acc names
