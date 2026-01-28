# Phase 1: Fix Open Beads

You are working on intent-cli, an AI-native specification and planning system written in Gleam. It transforms requirements into CUE specs, analyzes them with KIRK mental lattices (quality, coverage, gaps, inversion, effects), and decomposes them into atomic beads for AI implementation.

## Engineering Discipline (coding-rigor + bitter-truth)

**Non-negotiable rules:**
- Zero implementation without failing test first (RED → GREEN → REFACTOR)
- ≤25 lines per function, ≤5 parameters
- Functional core (pure logic, no I/O) / Imperative shell (side effects at edges)
- One behavior per test, one concept per commit
- GREEN → commit immediately. RED → revert immediately (TCR)
- Ship fast, validate ruthlessly — contract validation is absolute
- Gleam idioms: Result types, exhaustive matching, pipelines (`|>`)

## Your Task

Close ALL open beads (bugs) found by previous Red Queen review.

## Workflow

1. Run `bd ready --json` to get open issues
2. For EACH issue:
   - Read the bead description — it has EARS requirements, where to look, and reproduction steps
   - **RED**: Write a failing test that reproduces the bug
   - **GREEN**: Implement the minimum fix (≤25 lines per function)
   - **REFACTOR**: Clean up, ensure functional core / imperative shell
   - Verify: `gleam test` passes + reproduction command works
   - Close: `bd close <id> --reason "Fixed: <brief>"`
   - Commit: `git add -A && git commit -m "fix: <description>"`
3. After all beads closed, run full test suite: `gleam test`

## Completion Signal

When ALL beads are closed and `gleam test` passes:
COMPLETE
