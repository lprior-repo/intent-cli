# Phase 1: Fix Open Beads

You are working on intent-cli, an AI-native specification and planning system written in Gleam. It transforms requirements into CUE specs, analyzes them with KIRK mental lattices (quality, coverage, gaps, inversion, effects), and decomposes them into atomic beads for AI implementation.

## First: Load Skills

```
skill({ name: "coding-rigor" })
skill({ name: "bitter-truth" })
```

These enforce TDD-first development (RED→GREEN→REFACTOR), ≤25 line functions, functional core / imperative shell, and contract-driven validation.

## Your Task

Close ALL open beads (bugs) found by previous Red Queen review.

## Workflow

1. Run `bd ready --json` to get open issues
2. For EACH issue:
   - Read the bead description — it has EARS requirements, where to look, and reproduction steps
   - RED: Write a failing test that reproduces the bug
   - GREEN: Implement the minimum fix (≤25 lines per function)
   - REFACTOR: Clean up, ensure functional core / imperative shell
   - Verify: `gleam test` passes + reproduction command works
   - Close: `bd close <id> --reason "Fixed: <brief>"`
   - Commit: `git add -A && git commit -m "fix: <description>"`

## Implementation Rules

- TDD strictly: RED -> GREEN -> REFACTOR (coding-rigor)
- Gleam idioms: Result types, exhaustive matching, pipelines (`|>`)
- Small focused functions (≤25 lines), no over-engineering
- Every fix MUST have a corresponding test
- `gleam build` after each change, `gleam test` before closing
- Ship fast, validate ruthlessly (bitter-truth)

## Completion Signal

When ALL beads are closed and `gleam test` passes:
COMPLETE
