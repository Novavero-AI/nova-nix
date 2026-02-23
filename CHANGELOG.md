# Changelog

## 0.1.0.0 — 2026-02-23

### Added
- Full Nix expression parser (hand-rolled recursive descent, 13 precedence levels)
- Lazy evaluator with thunk-based evaluation, knot-tying for recursive bindings
- 73 pure builtins (type checks, arithmetic, bitwise, strings, lists, attrsets, JSON, hashing, version parsing, control flow)
- MonadEval typeclass — evaluator is polymorphic in its effect monad (PureEval for tests, IO for real evaluation)
- Content-addressed store path types with Windows/Unix support
- Derivation types and platform detection
- Hash integration via nova-cache
- 306 tests, zero framework dependencies
- CI pipeline: HLint, Ormolu, build with -Werror, test, Hackage publish on tags
