# Phase 3: Close Review Beads

You are working on intent-cli, an AI-native specification and planning system in Gleam. Phase 2 generated new beads from Red Queen adversarial QA + Product Owner review. Close them ALL.

## Engineering Discipline (coding-rigor + bitter-truth)

**Non-negotiable rules:**
- Zero implementation without failing test first (RED → GREEN → REFACTOR)
- ≤25 lines per function, ≤5 parameters
- Functional core (pure logic, no I/O) / Imperative shell (side effects at edges)
- One behavior per test, one concept per commit
- GREEN → commit. RED → revert (TCR)
- Ship fast, validate ruthlessly — contract validation is absolute
- Gleam idioms: Result types, exhaustive matching, pipelines (`|>`)

## Workflow

1. Run `bd ready --json` to get open issues
2. For EACH issue:
   - **RED**: Write a failing test that captures the issue
   - **GREEN**: Minimum fix (≤25 lines per function)
   - **REFACTOR**: Clean up, functional core / imperative shell
   - Verify: `gleam test` + reproduction command
   - Close: `bd close <id> --reason "Fixed: <explanation>"`
   - Commit: `git add -A && git commit -m "fix: <description>"`

## Regression Gate

After ALL beads are closed, run full regression:
```bash
gleam test
for cmd in validate quality coverage gaps invert effects lint show doctor improve diff; do
  gleam run -- $cmd examples/user-api.cue 2>/dev/null | python3 -c 'import sys,json; json.load(sys.stdin)' 2>/dev/null && echo "$cmd: PASS" || echo "$cmd: FAIL"
done
```

If ANY regression fails, fix before completing.

## Completion Signal

When ALL beads closed, `gleam test` passes, and regression gate passes:
COMPLETE
