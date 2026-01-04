# Claude Instructions for Intent CLI

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads) for issue tracking. Use `bd` commands instead of markdown TODOs. See AGENTS.md for workflow details.

## Quick Reference

```bash
bd ready --json           # Find ready work
bd update <id> --status in_progress --json  # Claim work
bd close <id> --reason "Done" --json        # Complete work
```

## Beads Viewer (bv) - Robot Commands Only

**CRITICAL**: Use ONLY `--robot-*` flags. Bare `bv` launches an interactive TUI that blocks your session.

```bash
bv --robot-triage          # Comprehensive analysis with recommendations
bv --robot-next            # Single top pick with claim command
bv --robot-plan            # Parallel execution tracks
bv --robot-insights        # Full metrics (PageRank, critical path)
bv --robot-graph --graph-format=json  # Dependency graph export
```

## Project Context

This is **Intent**, a contract-driven API testing CLI written in Gleam. It:
- Parses CUE specification files
- Validates them against a schema
- Executes HTTP requests against target APIs
- Verifies responses match expected behaviors

## Key Files

- `src/intent.gleam` - CLI entry point with glint commands
- `src/intent/checker.gleam` - Response validation (largest module, ~900 lines)
- `src/intent/parser.gleam` - JSON parsing with shared `dynamic_to_json` utility
- `src/intent_ffi.erl` - Erlang FFI for system operations

## Development Commands

```bash
gleam build    # Compile
gleam test     # Run tests
gleam run -- check examples/user-api.cue --target http://localhost:8080
```

## Code Style

- Use Result types for error handling
- Pattern match exhaustively
- Keep functions small and focused
- Prefer pipelines (`|>`) for data transformation
