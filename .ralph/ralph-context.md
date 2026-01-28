# Ralph Loop Context

## Project
intent-cli — AI-native specification and planning system in Gleam. Transforms vague requirements into crystal-clear CUE specs, then decomposes them into atomic "beads" (5-30min work units) that AI agents can implement deterministically.

Core workflow: Interview → CUE Spec → KIRK Analysis (quality, coverage, gaps, inversion, effects) → Beads → AI Implementation → Feedback Loop.

## Key Concepts
- **CUE Specs**: Strongly-typed API contracts (features, behaviors, checks, rules, anti-patterns, ai_hints)
- **EARS**: Structured requirements syntax (ubiquitous, event-driven, state-driven, optional, unwanted, complex)
- **KIRK**: Design-by-Contract analysis (preconditions, postconditions, invariants)
- **Beads**: Atomic work units generated from specs — so specific that AI implementation is mechanical
- **Mental Lattices**: Inversion, second-order effects, coverage, gap detection, quality scoring
- **Interview**: Stateful multi-round engine that builds specs through structured questioning

## Skills to Invoke
Load these skills at the start of each phase:
- `skill({ name: "coding-rigor" })` — TDD-first, TCR, ATDD, functional core / imperative shell, ≤25 line functions
- `skill({ name: "bitter-truth" })` — Velocity-first, contract-driven, disposable code, regenerate > maintain
- `skill({ name: "red-queen" })` — Adversarial QA, regression gates, attack-fix-regress loop

## Key Constraints
- Gleam style: Result types, exhaustive matching, pipelines, small functions
- All commands output valid JSON (except help)
- Exit codes: 0=success, 1=test-failure, 2=runtime-error, 3=invalid-input, 4=system-error
- ~37k lines across 86+ modules on the BEAM VM
- External dep: CUE CLI for spec parsing/validation

## Build/Test
```bash
gleam build && gleam test
gleam run -- <command> <args>
```

## Beads Workflow
```bash
bd ready --json          # Find work
bd close <id> --reason   # Complete work
bd create --title --type --priority --description  # Create issues
bd sync                  # Sync at end
```
