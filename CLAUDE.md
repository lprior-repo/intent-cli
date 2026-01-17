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

## Spec Format Requirements

All fields in Intent specifications are **required**. No backwards compatibility defaults are provided:

### Required Spec Fields
- `name` - Spec name
- `description` - Human-readable description
- `audience` - Target users of the API
- `version` - Semantic version
- `success_criteria` - List of acceptance criteria
- `config` - Configuration with `base_url`, `timeout_ms`, and `headers`
- `features` - List of feature specifications
- `rules` - Global validation rules
- `anti_patterns` - Anti-patterns to avoid
- `ai_hints` - Implementation guidance

### Required Feature Fields
- `name` - Feature name
- `description` - Feature description
- `behaviors` - List of behavior specifications (cannot be empty)

### Required Behavior Fields
- `name` - Behavior identifier
- `intent` - What this behavior demonstrates
- `request` - HTTP request with `method`, `path`, `headers`, `query`, `body`
- `response` - Expected response with `status`, `example`, `checks`, `headers`
- `notes` - Implementation notes (can be empty string)
- `requires` - Behavior dependencies (can be empty list)
- `tags` - Classification tags (can be empty list)
- `captures` - Output values for later use (can be empty dict)

### Required Check Fields
- `rule` - Validation rule expression
- `why` - Explanation of why this check matters

All optional-looking fields (like empty strings, empty lists) must be explicitly provided in CUE specs.

## Code Style

- Use Result types for error handling
- Pattern match exhaustively
- Keep functions small and focused
- Prefer pipelines (`|>`) for data transformation
