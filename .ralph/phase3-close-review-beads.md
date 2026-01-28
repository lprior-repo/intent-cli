# Phase 3: Close Review Beads (TDD15)

You are working on intent-cli, an AI-native specification and planning system in Gleam. Phase 2 generated new beads from Red Queen + Product Owner review. Close them ALL.

## Workflow

1. Run `bd ready --json` to get open issues
2. For EACH issue:
   - RED: Write a failing test
   - GREEN: Minimum fix
   - REFACTOR: Clean up
   - Verify: `gleam test` + reproduction command
   - Close: `bd close <id> --reason "Fixed: <explanation>"`
   - Commit: `git add -A && git commit -m "fix: <description>"`

## Implementation Rules

- TDD15: RED -> GREEN -> REFACTOR (no shortcuts)
- Gleam idioms: Result types, exhaustive matching, pipelines, small functions
- Every fix needs a test
- No over-engineering

## Final Verification

```bash
gleam test
bd list --status=open --json  # Should be empty or future-work only
```

## Completion Signal

When ALL review beads are closed and `gleam test` passes:
COMPLETE
