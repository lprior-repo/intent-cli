# Phase 3: Close Review Beads

You are working on intent-cli, an AI-native specification and planning system in Gleam. Phase 2 generated new beads from Red Queen adversarial QA + Product Owner review. Close them ALL.

## First: Load Skills

```
skill({ name: "coding-rigor" })
skill({ name: "bitter-truth" })
```

## Workflow

1. Run `bd ready --json` to get open issues
2. For EACH issue:
   - RED: Write a failing test (coding-rigor: test specifies behavior)
   - GREEN: Minimum fix (≤25 lines per function)
   - REFACTOR: Clean up, functional core / imperative shell
   - Verify: `gleam test` + reproduction command
   - Close: `bd close <id> --reason "Fixed: <explanation>"`
   - Commit: `git add -A && git commit -m "fix: <description>"`

## Implementation Rules

- TDD: RED -> GREEN -> REFACTOR (no shortcuts)
- Gleam idioms: Result types, exhaustive matching, pipelines, small functions
- Every fix needs a test
- No over-engineering — ship fast (bitter-truth)
- Contract validation is absolute — if tests pass, ship it

## Regression Gate

After ALL beads are closed, run the full regression:
```bash
gleam test
# Re-run all Red Queen attacks from Phase 2
for cmd in validate quality coverage gaps invert effects lint show doctor improve diff; do
  gleam run -- $cmd examples/user-api.cue 2>/dev/null | python3 -c 'import sys,json; json.load(sys.stdin)' 2>/dev/null && echo "$cmd: PASS" || echo "$cmd: FAIL"
done
```

If ANY regression fails, fix before completing.

## Completion Signal

When ALL beads closed, `gleam test` passes, and regression gate passes:
COMPLETE
