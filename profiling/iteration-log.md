# Memory Optimization Iteration Log

## Context
Target: `builtins.length (builtins.attrNames (import <nixpkgs> {}))` with `-M4G` cap.
nixpkgs 24.11 channel. GHC 9.6.7, Windows.

## Profiling Workflow

### Quick reference
```bash
# 1. Build (non-prof for -hT, -prof for -hc/-hd/-hr)
cabal build                                                      # non-prof
cabal build --enable-profiling --ghc-options="-fprof-auto-top"   # prof

# 2. Run benchmark
./profiling/bench.sh <label>              # baseline (-s only)
./profiling/bench.sh <label> -hT          # type breakdown (non-prof)
./profiling/bench.sh <label> -hc          # cost centre (prof build)
./profiling/bench.sh <label> -hr          # retainer (prof build)
./profiling/bench.sh <label> -hT -M8G    # custom heap cap

# 3. Compare runs
./profiling/compare.sh <before> <after>

# Results stored in: profiling/results/<label>/
```

### Profiling modes explained (per Simon Marlow's methodology)

| Flag | Build | What it tells you | When to use |
|------|-------|-------------------|-------------|
| `-hT` | non-prof | Heap by **type** (Env, THUNK, Map.Bin) | First look — what's big? |
| `-hd` | prof | Heap by **closure description** (sat_s1234) | Which specific closures? |
| `-hc` | prof | Heap by **cost centre** (function name) | Which functions allocate? |
| `-hr` | prof | Heap by **retainer** set | **KEY for space leaks**: what KEEPS data alive? |
| `-hb` | prof | Heap by **biography** (lag/drag/void) | Is data created too early or kept too late? |

**Space leak diagnosis order** (Marlow's approach):
1. `-hT` to identify the types growing unboundedly
2. `-hr` to find what retains those types (the "who keeps it alive" question)
3. `-hc` to confirm which functions are responsible
4. Fix, re-bench with `-hT`, compare

### Key insight
Allocation (`-hc`) is NOT the same as retention (`-hr`). A function can allocate
very little but retain a lot (by closing over it). For space leaks, `-hr` is
the critical tool — it answers "why can't the GC collect this?"

### Scaling test (space leak detector)
Run the same benchmark at 4G, 6G, 8G caps. If max residency grows linearly with
the cap, you have a space leak (GC pressure keeps otherwise-collectable data alive).
```bash
./profiling/bench.sh scaling-4g -hT -M4G
./profiling/bench.sh scaling-6g -hT -M6G
./profiling/bench.sh scaling-8g -hT -M8G
```

---

## Iteration 0: Pre de Bruijn (2026-02-26)
- Map-based Env with Text key lookup
- Map.Bin: 1.88 GB, Env: 670 MB, MUT_VAR_CLEAN: 273 MB
- OOM at 8 GB

## Iteration 1: De Bruijn indices (v0.1.6.0)
- Replaced Map-based Env with SmallArray positional slots
- **Results**: Map.Bin 88% down (228 MB), Env 41% down (394 MB), IORef 53% down
- Still OOM at 4G: 45 GB alloc, 4.21 GB max residency, 32% productivity

## Iteration 2: cheapThunk (2026-02-27, commit 9e8a85a)
- Avoid IORef for trivial exprs (resolved vars, literals, lambdas)
- **Results**: IORef 30% down (129->91 MB), ThunkRef 32% down (121->82 MB)
- Total residency UNCHANGED at 4.21 GB (savings reshuffled into other categories)

## Iteration 3: Profiling deep-dive (2026-02-27)

### -hT (type breakdown at peak, ~2.7 GB snapshot)
| MB | Type |
|----|------|
| 394 | Env |
| 352 | STACK |
| 298 | SmallArray |
| 282 | THUNK_2_0 |
| 257 | FUN |
| 228 | Map.Bin |
| 212 | THUNK |
| 203 | THUNK_1_0 |
| 129 | MUT_VAR_CLEAN |
| 121 | ThunkRef |
| 93 | Just |
| 51 | VLambda |

### -hd (closure description, profiling build)
| MB | Closure |
|----|---------|
| 355.9 | STACK |
| 319.1 | Env |
| 241.6 | SmallArray |
| 184.9 | Map.Bin |
| 91.2 | Nix.Eval.Types.sat (mkSyntheticThunk related) |
| 85.5 | Nix.Eval.l (eval closures) |
| 75.2 | Just |
| 70.7 | MUT_VAR_CLEAN |
| 63.8 | ThunkRef |

### -hc (cost centre, THE KEY DATA)
| MB | Function | What |
|----|----------|------|
| 257.6 | attrSetMapWithKeyLazy (path 1) | Lazy map-over-attrs for overlays |
| 255.5 | SYSTEM | GHC runtime (STACK, GC) |
| 210.2 | eval/runEvalIO | Main eval loop closures |
| 93.0 | newSyntheticCell/mkSyntheticThunk | Synthetic thunks (nested attrs) |
| 80.5 | runSmallArray/createSmallArray | envSlots allocation |
| 57.3 | attrSetMapWithKeyLazy (path 2) | Same, different call site |
| 56.4 | matchFormals/forceThunk | Function arg binding |
| 56.4 | matchFormals/builtinConcatLists | Same, via concatLists |
| 54.6 | forceThunk/force/eval | Thunk forcing |
| 25.9 | evalIf | If/then/else |
| 25.0 | evalSelect | Attribute selection |
| 25.0 | matchFormals/walkAttrPath | Formal binding via attr walk |
| 24.2 | buildLazyBindingMap | Building lazy attr bindings |

### Root cause of #1 target (315 MB)
`attrSetMapWithKeyLazy` on line 353 of Types.hs eagerly materializes EVERY
binding and creates a new Env+SmallArray+mkSyntheticThunk per key. For nixpkgs
overlay application (~30k keys), this is 30k * (IORef + Env + SmallArray + Thunk).

The function was introduced to prevent OOM (it used to fully materialize the whole
set), but it still does too much work upfront.

---

## Iteration 4: Lazy PreBuilt (2026-02-27)
- Removed bang from PreBuilt field to defer materialization in attrSetMapWithKeyLazy
- **IORef -28%, ThunkRef -31%** — deferred work confirmed
- But total residency UNCHANGED: balloon effect, other categories grew ~6%

## Iteration 5: Discovery — it's a SPACE LEAK (2026-02-27)

Scaling test reveals the truth:
| Cap | Max Residency | Total Alloc |
|-----|---------------|-------------|
| 4G  | 4.2 GB        | 46 GB       |
| 6G  | 6.3 GB        | 69 GB       |
| 8G  | 8.4 GB        | 92 GB       |

Live data scales linearly with heap cap. Classic space leak.
C++ Nix also struggles here — `nix-env -qaP` uses ~10 GB RSS (NixOS/nix#8621).
Simpler evals (single NixOS config) use ~1 GB. No verified number for
`builtins.attrNames (import <nixpkgs> {})` specifically.

### Root cause: Env chain retention
`ELambda formals body -> pure (VLambda env formals body)` (Eval.hs:160)
captures the ENTIRE Env chain. A lambda using 3 variables retains the full
scope of 30k+ entries. nixpkgs has thousands of lambdas nested in overlays,
each holding onto the entire package set.

C++ Nix does **closure trimming** — only captures variables the body uses.
We need the same. The resolution pass already knows which variables each
lambda uses (de Bruijn indices). We just need to trim the Env when creating
VLambda.

## Iteration 6: Retainer profiling (2026-02-27)

### -hr (retainer set breakdown)
| MB | Retainer | What retains it |
|----|----------|----------------|
| 1019.6 | SYSTEM | GHC runtime (GC roots, stacks) |
| 404.9 | SYSTEM | GHC runtime (secondary) |
| 376.1 | newSyntheticCell | mkSyntheticThunk: IORef(Pending expr env) |
| 337.5 | eval | Unevaluated closures in eval loop |
| 237.0 | forceThunk,evalSelect | Force→select chain, mid-eval Env alive |
| 149.3 | newSyntheticCell | mkSyntheticThunk (second retainer set) |
| 134.0 | eval | Eval closures (second set) |
| 94.1 | forceThunk,evalSelect | Force→select (second set) |
| 88.9 | newMemoCell,eval,newSyntheticCell | Mixed thunk+eval chain |
| 69.5 | newSyntheticCell | mkSyntheticThunk (third set) |
| 68.3 | walkAttrPath | Attribute path traversal |
| 56.7 | builtinConcatLists | concatLists retaining list thunks |

### Analysis
- **newSyntheticCell total: ~595 MB** — #1 retainer. Two call sites:
  - `deferApply` (Eval.hs:1529) — deferred builtins.map application
  - `builtinMapAttrs` (Eval.hs:1838) — overlay application over 30k keys
- **eval total: ~471 MB** — unevaluated closures holding Env chains
- **forceThunk+evalSelect: ~331 MB** — intermediate Envs during attr select

### Key insight
Even though synthetic Envs have `envParent = Nothing`, the VALUES in their
slots (especially VLambda closures) carry full parent Env chains. The leak is
in what's captured inside the slots, not the synthetic Env structure itself.

### Confirmed fix target: closure trimming
VLambda captures the entire Env chain. A lambda using 3 vars retains 30k
entries. nixpkgs overlays create thousands of lambdas, each holding the full
package set. C++ Nix does closure trimming — only captures used variables.

---

## Iteration 7: VLambda closure trimming (2026-02-28, commit e7c8cbd)

Added `ClosureTrim` pass that analyses lambda bodies for free `EResolvedVar`
references and annotates `ELambda` with `CaptureInfo`. At eval time,
`buildCaptureEnv` extracts only referenced slots into a flat Env (no parent chain).

### Implementation
- `Nix.Expr.ClosureTrim.trimClosures` — post-resolution pass
- `CaptureInfo` type: `NoCaptureInfo | Captures [(Int,Int)]`
- `cheapThunk` fast-path for trimmed lambdas (no IORef allocation)
- `collectFreeVars` computes `(Set (level, index), hasOuterEVar)` per expr

### Results (-hT at 6G cap)
| MB | Type | Delta |
|----|------|-------|
| 952 | Env | baseline |
| 813 | SmallArray | baseline |
| 682 | STACK | baseline |
| 109 | VLambda | measured |

Trimming fires on lambdas, but the dominant retainers are in
`mkSyntheticThunk`/`eval`/`force` chains — not VLambda closures.

---

## Iteration 8: Let/rec parent chain trimming (2026-02-28, commit a25a9dc)

Extended closure trimming to `ELet` and `EAttrs True` (recursive attr sets).
These also create new Env scopes that retain the full parent chain.

### Implementation
- `trimOneLetBlock` — analyses let bindings + body for free vars
- `trimOneRecAttrs` — analyses rec attr set bindings
- Positional let/rec resolution in evaluator
- `ELet` and `EAttrs` carry `CaptureInfo` field

### Results (-hT at 6G cap)
| MB | Type | Delta from iter7 |
|----|------|-------------------|
| 940 | Env | -12 (-1.3%) |

**Barely moved the needle.** Root cause: nixpkgs uses `with lib;` pervasively
(~2,658 `with` expressions in pkgs/top-level alone). Every let/rec block
inside a `with` scope has `EVar` references that need name-based runtime
lookup through with-scopes. The trimmer sees `hasOuterEVar = True` and
bails out — trimming almost never fires on real nixpkgs code.

---

## Iteration 9: Static with-resolution (2026-03-01, commit 8e1f000)

**Breakthrough:** Resolve `with`-scoped variables statically during the
resolve pass. Introduced `EWithVar !Text` — a variable known to be resolved
through `envWithScopes` at runtime, not the parent chain. The trimmer treats
`EWithVar` differently from `EVar`: it doesn't block trimming, but signals
that the trimmed env must preserve `envWithScopes`.

### Implementation
- `EWithVar !Text` — new AST constructor for with-scoped variables
- `CapturesWithScopes ![(Int,Int)]` — capture info that preserves with-scopes
- `WithBarrier` scope entry in resolver — doesn't increment de Bruijn level
- `withScopesForCapture` — appends root scope (builtins) to `envWithScopes` at capture time
- `evalWithVar` — checks with-scopes first, falls back to parent chain

### Key design decisions
1. `WithBarrier` does NOT increment de Bruijn level (with doesn't create envParent level)
2. `resolveVar` continues past `WithBarrier` — lexical scopes below still checked
3. Initially added `envRootScope` as an Env field — benchmarking showed +142 MB overhead
   (~18M Envs × 8 bytes). Refactored to function + `withScopesForCapture` helper.
4. Builtins appended to `envWithScopes` only at capture time (zero per-Env cost)

### Results (-hT at 6G cap)
| MB | Type | Delta from iter7 |
|----|------|-------------------|
| 590 | Env | **-362 (-38%)** |
| 536 | SmallArray | -277 (-34%) |
| 448 | STACK | -234 (-34%) |

**~1.3 GB total reduction.** Trimming now fires on the vast majority of
let/rec/lambda blocks in nixpkgs since `EWithVar` doesn't block trimming.

### Files changed (7 files, +281 -108)
- `Nix.Expr.Types` — `EWithVar`, `CapturesWithScopes`
- `Nix.Expr.Resolve` — `WithBarrier`, `EWithVar` emission
- `Nix.Expr.ClosureTrim` — triple return from `collectFreeVars`, `needsWithScopes` flag
- `Nix.Eval` — `evalWithVar`, `buildCaptureEnv` for `CapturesWithScopes`
- `Nix.Eval.Types` — `envRootScope` function, `withScopesForCapture`, `cheapThunk` update
- `Nix.Eval.IO` — `EWithVar` in `resolveRelativePaths`
- `test/Main.hs` — 9 new tests

---

## Next: Synthetic thunk trimming
`mkSyntheticThunk` creates `Pending expr env` capturing the FULL Env.
- `attrSetMapWithKeyLazy`: 30k synthetic thunks per overlay, each with full scope
- `deferApply`: deferred function application with full scope
Trim Env in `Pending expr env` the same way lambdas/lets are trimmed.
Expected impact: ~683+ MB retained by mkSyntheticThunk chains.
