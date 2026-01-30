# Intent CLI

Contract-driven API testing in Gleam. CUE specs → HTTP tests → verification.

**Documentation:** See `.beads/CLAUDE.jsonl` for complete reference (JSONL format for token efficiency).

**Quick start:**
```bash
bd ready                    # Find work
bd update <id> --status in_progress  # Claim
bd close <id> --reason '...'         # Complete
bv --robot-triage          # AI triage
gleam build && gleam test  # Build + test
```
