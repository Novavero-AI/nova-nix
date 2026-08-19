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

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Nix.Expr.Types

-- | Walk the AST and attach 'Captures' info to trimmable scopes
-- (lambdas, let blocks, and recursive attr sets).
trimClosures :: Expr -> Expr
trimClosures = trimExpr

-- | Walk the expression tree, dispatching each trimmable scope to
-- 'trimOneLambda' and friends.  Escape analysis happens per scope in
-- those helpers; the walk itself carries no state.
trimExpr :: Expr -> Expr
trimExpr expr = case expr of
  ELit {} -> expr
  EStr parts -> EStr (map trimPart parts)
  EIndStr parts -> EIndStr (map trimPart parts)
  EVar {} -> expr
  EWithVar {} -> expr
  EResolvedVar {} -> expr
  EAttrs True bindings captureInfo ->
    let trimmedBindings = map trimBinding bindings
     in case captureInfo of
          Captures {} -> EAttrs True trimmedBindings captureInfo
          CapturesWithScopes {} -> EAttrs True trimmedBindings captureInfo
          NoCaptureInfo ->
            case trimOneRecAttrs trimmedBindings of
              Left unchanged -> EAttrs True unchanged NoCaptureInfo
              Right (captureList, rewrittenBindings, needsWithScopes) ->
                let ci =
                      if needsWithScopes
                        then CapturesWithScopes captureList
                        else Captures captureList
                 in EAttrs True rewrittenBindings ci
  EAttrs False bindings _captureInfo ->
    EAttrs False (map trimBinding bindings) NoCaptureInfo
  EList elems -> EList (map trimExpr elems)
  ESelect target path defExpr ->
    ESelect
      (trimExpr target)
      (map trimKey path)
      (fmap trimExpr defExpr)
  EHasAttr target path ->
    EHasAttr (trimExpr target) (map trimKey path)
  EApp f x -> EApp (trimExpr f) (trimExpr x)
  ELambda formals body captures ->
    -- First, recursively trim nested lambdas in the body.
    let trimmedFormals = trimFormals formals
        trimmedBody = trimExpr body
     in case captures of
          Captures {} -> ELambda trimmedFormals trimmedBody captures
          CapturesWithScopes {} -> ELambda trimmedFormals trimmedBody captures
          NoCaptureInfo ->
            case trimOneLambda trimmedFormals trimmedBody of
              Left unchanged -> unchanged
              Right (captureList, rewrittenFormals, rewrittenBody, needsWithScopes) ->
                let captureInfo =
                      if needsWithScopes
                        then CapturesWithScopes captureList
                        else Captures captureList
                 in ELambda rewrittenFormals rewrittenBody captureInfo
  ELet bindings body captureInfo ->
    let trimmedBindings = map trimBinding bindings
        trimmedBody = trimExpr body
     in case captureInfo of
          Captures {} -> ELet trimmedBindings trimmedBody captureInfo
          CapturesWithScopes {} -> ELet trimmedBindings trimmedBody captureInfo
          NoCaptureInfo ->
            case trimOneLetBlock trimmedBindings trimmedBody of
              Left (unchangedBindings, unchangedBody) ->
                ELet unchangedBindings unchangedBody NoCaptureInfo
              Right (captureList, rewrittenBindings, rewrittenBody, needsWithScopes) ->
                let ci =
                      if needsWithScopes
                        then CapturesWithScopes captureList
                        else Captures captureList
                 in ELet rewrittenBindings rewrittenBody ci
  EIf c t f ->
    EIf (trimExpr c) (trimExpr t) (trimExpr f)
  EWith scope body ->
    EWith (trimExpr scope) (trimExpr body)
  EAssert cond body ->
    EAssert (trimExpr cond) (trimExpr body)
  EUnary op operand -> EUnary op (trimExpr operand)
  EBinary op l r -> EBinary op (trimExpr l) (trimExpr r)
  ESearchPath {} -> expr

trimPart :: StringPart -> StringPart
trimPart p@(StrLit _) = p
trimPart (StrInterp e) = StrInterp (trimExpr e)

trimKey :: AttrKey -> AttrKey
trimKey k@(StaticKey _) = k
trimKey (DynamicKey e) = DynamicKey (trimExpr e)

trimBinding :: Binding -> Binding
trimBinding (NamedBinding path bodyExpr) =
  NamedBinding (map trimKey path) (trimExpr bodyExpr)
trimBinding (Inherit (Just fromExpr) names) =
  Inherit (Just (trimExpr fromExpr)) names
trimBinding b@(Inherit Nothing _) = b

trimFormals :: Formals -> Formals
trimFormals f@(FormalName _) = f
trimFormals (FormalSet formals ellipsis) =
  FormalSet (map trimFormal formals) ellipsis
trimFormals (FormalNamedSet name formals ellipsis) =
  FormalNamedSet name (map trimFormal formals) ellipsis

trimFormal :: Formal -> Formal
trimFormal (Formal name defExpr) =
  -- Default expressions are part of the lambda's own body; nested
  -- lambdas inside them were already trimmed by the walk.
  Formal name (fmap trimExpr defExpr)

-- | Analyze a single lambda body and formals defaults, produce a capture
-- list + rewritten body + rewritten formals, or return the original
-- lambda unchanged if untrimable.
--
-- Formals defaults must be included because they are evaluated in the
-- lambda's env: a default like @{ x ? outerVar }: x@ references the
-- closure env, which for a trimmed lambda is the flat capture env.
-- Missing captures for defaults would cause out-of-bounds slot lookups.
trimOneLambda :: Formals -> Expr -> Either Expr ([(Int, Int)], Formals, Expr, Bool)
trimOneLambda formals body =
  let (bodyVars, bodyHasEVar, bodyHasWithVar) = collectFreeVars 0 body
      (formalVars, formalHasEVar, formalHasWithVar) = foldFormalsDefaults 0 formals
      freeVars = Set.union bodyVars formalVars
      hasOuterEVar = bodyHasEVar || formalHasEVar
      needsWithScopes = bodyHasWithVar || formalHasWithVar
   in if hasOuterEVar || (Set.null freeVars && not needsWithScopes)
        then Left (ELambda formals body NoCaptureInfo)
        else
          let captureList = sortBy (comparing fst <> comparing snd) (Set.toList freeVars)
              captureMap = Map.fromList (zip captureList [0 ..])
              rewrittenBody = rewriteBody 0 captureMap body
              rewrittenFormals = rewriteFormals 0 captureMap formals
           in Right (captureList, rewrittenFormals, rewrittenBody, needsWithScopes)

-- | Collect free variable references from a lambda body.
--
-- Returns @(freeVars, hasOuterEVar, hasOuterWithVar)@ where:
-- * @freeVars@ is the set of @(level, index)@ pairs for 'EResolvedVar'
--   references that point outside the lambda (level > current depth)
-- * @hasOuterEVar@ is 'True' if there's any 'EVar' that could reference
--   the outer scope (not shadowed by inner let/rec/lambda scopes)
-- * @hasOuterWithVar@ is 'True' if there's any 'EWithVar' that needs
--   with-scope access (doesn't block trimming, but env must preserve
--   'envWithScopes' with builtins appended)
--
-- @depth@ tracks how many scope-creating constructs we've entered
-- inside the lambda body (nested lambdas, let, rec attrs).
collectFreeVars :: Int -> Expr -> (Set (Int, Int), Bool, Bool)
collectFreeVars depth expr = case expr of
  ELit {} -> (Set.empty, False, False)
  EStr parts -> foldParts depth parts
  EIndStr parts -> foldParts depth parts
  EVar _ ->
    -- Any EVar in the body means we need the parent chain for name lookup.
    -- This makes the lambda untrimable.
    (Set.empty, True, False)
  EWithVar _ ->
    -- Needs with-scopes, but doesn't block trimming.
    (Set.empty, False, True)
  EResolvedVar level idx
    | level > depth -> (Set.singleton (level - depth - 1, idx), False, False)
    | otherwise -> (Set.empty, False, False)
  EAttrs True bindings captureInfo ->
    -- Recursive attrs create a scope (depth + 1)
    let (vs1, h1, w1) = foldBindings (depth + 1) bindings
        (vs2, innerWithVar) = case captureInfo of
          NoCaptureInfo -> (Set.empty, False)
          Captures caps ->
            (Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth], False)
          CapturesWithScopes caps ->
            (Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth], True)
     in (Set.union vs1 vs2, h1, w1 || innerWithVar)
  EAttrs False bindings _captureInfo ->
    -- Non-recursive attrs: no new scope
    foldBindings depth bindings
  EList elems -> foldExprs depth elems
  ESelect target path defExpr ->
    let (vs1, h1, w1) = collectFreeVars depth target
        (vs2, h2, w2) = foldKeys depth path
        (vs3, h3, w3) = case defExpr of
          Nothing -> (Set.empty, False, False)
          Just e -> collectFreeVars depth e
     in (Set.unions [vs1, vs2, vs3], h1 || h2 || h3, w1 || w2 || w3)
  EHasAttr target path ->
    let (vs1, h1, w1) = collectFreeVars depth target
        (vs2, h2, w2) = foldKeys depth path
     in (Set.union vs1 vs2, h1 || h2, w1 || w2)
  EApp f x ->
    let (vs1, h1, w1) = collectFreeVars depth f
        (vs2, h2, w2) = collectFreeVars depth x
     in (Set.union vs1 vs2, h1 || h2, w1 || w2)
  ELambda formals body_ captures ->
    -- Nested lambda creates a LexicalScope (depth + 1).
    -- If the inner lambda was already trimmed (has Captures), its capture
    -- coordinates are outer references that we must propagate upward -
    -- otherwise an outer lambda won't know it needs those variables.
    let (vs1, h1, w1) = foldFormalsDefaults (depth + 1) formals
        (vs2, h2, w2) = collectFreeVars (depth + 1) body_
        (vs3, innerWithVar) = case captures of
          NoCaptureInfo -> (Set.empty, False)
          Captures caps ->
            (Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth], False)
          CapturesWithScopes caps ->
            (Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth], True)
     in (Set.unions [vs1, vs2, vs3], h1 || h2, w1 || w2 || innerWithVar)
  ELet bindings body_ captureInfo ->
    -- Let creates a scope (depth + 1)
    let (vs1, h1, w1) = foldBindings (depth + 1) bindings
        (vs2, h2, w2) = collectFreeVars (depth + 1) body_
        (vs3, innerWithVar) = case captureInfo of
          NoCaptureInfo -> (Set.empty, False)
          Captures caps ->
            (Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth], False)
          CapturesWithScopes caps ->
            (Set.fromList [(lvl - depth - 1, idx) | (lvl, idx) <- caps, lvl > depth], True)
     in (Set.unions [vs1, vs2, vs3], h1 || h2, w1 || w2 || innerWithVar)
  EIf c t f ->
    let (vs1, h1, w1) = collectFreeVars depth c
        (vs2, h2, w2) = collectFreeVars depth t
        (vs3, h3, w3) = collectFreeVars depth f
     in (Set.unions [vs1, vs2, vs3], h1 || h2 || h3, w1 || w2 || w3)
  EWith scope body_ ->
    -- With does NOT create a scope frame
    let (vs1, h1, w1) = collectFreeVars depth scope
        (vs2, h2, w2) = collectFreeVars depth body_
     in (Set.union vs1 vs2, h1 || h2, w1 || w2)
  EAssert cond body_ ->
    let (vs1, h1, w1) = collectFreeVars depth cond
        (vs2, h2, w2) = collectFreeVars depth body_
     in (Set.union vs1 vs2, h1 || h2, w1 || w2)
  EUnary _ operand -> collectFreeVars depth operand
  EBinary _ l r ->
    let (vs1, h1, w1) = collectFreeVars depth l
        (vs2, h2, w2) = collectFreeVars depth r
     in (Set.union vs1 vs2, h1 || h2, w1 || w2)
  ESearchPath {} -> (Set.empty, False, False)

foldExprs :: Int -> [Expr] -> (Set (Int, Int), Bool, Bool)
foldExprs depth = foldl' combine (Set.empty, False, False)
  where
    combine (!acc, !h, !w) e =
      let (vs, hv, wv) = collectFreeVars depth e
       in (Set.union acc vs, h || hv, w || wv)

foldParts :: Int -> [StringPart] -> (Set (Int, Int), Bool, Bool)
foldParts depth = foldl' combine (Set.empty, False, False)
  where
    combine (!acc, !h, !w) (StrLit _) = (acc, h, w)
    combine (!acc, !h, !w) (StrInterp e) =
      let (vs, hv, wv) = collectFreeVars depth e
       in (Set.union acc vs, h || hv, w || wv)

foldKeys :: Int -> [AttrKey] -> (Set (Int, Int), Bool, Bool)
foldKeys depth = foldl' combine (Set.empty, False, False)
  where
    combine (!acc, !h, !w) (StaticKey _) = (acc, h, w)
    combine (!acc, !h, !w) (DynamicKey e) =
      let (vs, hv, wv) = collectFreeVars depth e
       in (Set.union acc vs, h || hv, w || wv)

foldBindings :: Int -> [Binding] -> (Set (Int, Int), Bool, Bool)
foldBindings depth = foldl' combine (Set.empty, False, False)
  where
    combine (!acc, !h, !w) (NamedBinding path bodyExpr) =
      let (vs1, h1, w1) = foldKeys depth path
          (vs2, h2, w2) = collectFreeVars depth bodyExpr
       in (Set.unions [acc, vs1, vs2], h || h1 || h2, w || w1 || w2)
    combine (!acc, !h, !w) (Inherit (Just fromExpr) _) =
      let (vs, hv, wv) = collectFreeVars depth fromExpr
       in (Set.union acc vs, h || hv, w || wv)
    combine (!acc, !h, !w) (Inherit Nothing _) = (acc, h, w)

foldFormalsDefaults :: Int -> Formals -> (Set (Int, Int), Bool, Bool)
foldFormalsDefaults _ (FormalName _) = (Set.empty, False, False)
foldFormalsDefaults depth (FormalSet formals _) = foldFormals depth formals
foldFormalsDefaults depth (FormalNamedSet _ formals _) = foldFormals depth formals

foldFormals :: Int -> [Formal] -> (Set (Int, Int), Bool, Bool)
foldFormals depth = foldl' combine (Set.empty, False, False)
  where
    combine (!acc, !h, !w) (Formal _ Nothing) = (acc, h, w)
    combine (!acc, !h, !w) (Formal _ (Just defExpr)) =
      let (vs, hv, wv) = collectFreeVars depth defExpr
       in (Set.union acc vs, h || hv, w || wv)

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
  EWithVar {} -> expr
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
rewriteCaptures depth captureMap (CapturesWithScopes caps) =
  CapturesWithScopes (map rewriteOne caps)
  where
    rewriteOne (lvl, idx)
      | lvl > depth =
          let outerKey = (lvl - depth - 1, idx)
           in case Map.lookup outerKey captureMap of
                Just captureIdx -> (depth + 1, captureIdx)
                Nothing -> (lvl, idx)
      | otherwise = (lvl, idx)

-- | Analyze a let block's binding bodies + body for parent references,
-- produce a capture list + rewritten bindings + body, or return the
-- originals unchanged if untrimable.
--
-- Level 0 = self-references within the let scope (untouched).
-- Level 1+ = parent chain references (trimmed).
trimOneLetBlock :: [Binding] -> Expr -> Either ([Binding], Expr) ([(Int, Int)], [Binding], Expr, Bool)
trimOneLetBlock bindings body =
  let (bindingVars, bindingHasEVar, bindingHasWithVar) = foldBindings 0 bindings
      (bodyVars, bodyHasEVar, bodyHasWithVar) = collectFreeVars 0 body
      freeVars = Set.union bindingVars bodyVars
      hasOuterEVar = bindingHasEVar || bodyHasEVar
      needsWithScopes = bindingHasWithVar || bodyHasWithVar
   in if hasOuterEVar || (Set.null freeVars && not needsWithScopes)
        then Left (bindings, body)
        else
          let captureList = sortBy (comparing fst <> comparing snd) (Set.toList freeVars)
              captureMap = Map.fromList (zip captureList [0 ..])
              rewrittenBindings = map (rewriteBinding 0 captureMap) bindings
              rewrittenBody = rewriteBody 0 captureMap body
           in Right (captureList, rewrittenBindings, rewrittenBody, needsWithScopes)

-- | Analyze a recursive attr set's bindings for parent references,
-- produce a capture list + rewritten bindings, or return the originals
-- unchanged if untrimable.
trimOneRecAttrs :: [Binding] -> Either [Binding] ([(Int, Int)], [Binding], Bool)
trimOneRecAttrs bindings =
  let (bindingVars, bindingHasEVar, bindingHasWithVar) = foldBindings 0 bindings
   in if bindingHasEVar || (Set.null bindingVars && not bindingHasWithVar)
        then Left bindings
        else
          let captureList = sortBy (comparing fst <> comparing snd) (Set.toList bindingVars)
              captureMap = Map.fromList (zip captureList [0 ..])
              rewrittenBindings = map (rewriteBinding 0 captureMap) bindings
           in Right (captureList, rewrittenBindings, bindingHasWithVar)
