# Intent CLI - AI Agent Instructions

## Project Overview

**Intent** is a contract-driven API testing framework with the tagline: "Human-writes, AI-verifies, AI-implements". It enables developers to write API specifications in CUE language that describe API behaviors and validate running APIs against these specifications.

**Tech Stack**: Gleam (compiles to Erlang/OTP), CUE for specifications

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
bd create "Subtask" --parent <epic-id> --json  # Hierarchical subtask (gets ID like epic-id.1)
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issues.jsonl` file together with the code changes so issue state stays in sync with code state

### Viewing Issues with bv (beads viewer)

**CRITICAL**: Use ONLY `--robot-*` flags. Bare `bv` launches an interactive TUI that blocks your session.

**AI Agent Commands (non-blocking JSON output):**
```bash
bv --robot-triage          # Comprehensive analysis with recommendations
bv --robot-next            # Single top pick with claim command
bv --robot-plan            # Parallel execution tracks with unblock lists
bv --robot-insights        # Full metrics (PageRank, betweenness, critical path)
bv --robot-label-health    # Per-label health assessment
bv --robot-graph --graph-format=json  # Dependency graph export
```

**Human-only (interactive TUI):**
```bash
bv              # Launch interactive viewer - DO NOT USE as AI agent
```

The viewer provides:
- Split-pane dashboard
- Kanban board view
- Graph visualization
- Insights panels

Install: `curl -fsSL "https://raw.githubusercontent.com/Dicklesworthstone/beads_viewer/main/install.sh" | bash`

### CLI Help

Run `bd <command> --help` to see all available flags for any command.
For example: `bd create --help` shows `--parent`, `--deps`, `--assignee`, etc.

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

## Project Structure

```
intent-cli/
├── src/
│   ├── intent.gleam         # CLI entry point
│   ├── intent_ffi.erl       # Erlang FFI for halt/timing
│   └── intent/
│       ├── types.gleam      # Core type definitions
│       ├── parser.gleam     # JSON to Gleam type parsing
│       ├── loader.gleam     # CUE file loading and validation
│       ├── runner.gleam     # Test execution orchestrator
│       ├── checker.gleam    # Response validation engine
│       ├── http_client.gleam # HTTP request execution
│       ├── resolver.gleam   # Behavior dependency resolution
│       ├── rules_engine.gleam # Global rule evaluation
│       ├── anti_patterns.gleam # Anti-pattern detection
│       ├── rule.gleam       # Rule expression parser
│       ├── interpolate.gleam # Variable interpolation
│       └── output.gleam     # Result formatting
├── test/                    # Test suite
├── examples/                # Example specifications
├── schema/                  # CUE schema definitions
└── gleam.toml              # Package configuration
```

## Build Commands

```bash
gleam build    # Compile the project
gleam test     # Run tests
gleam run      # Run the CLI
```

## CLI Commands

```bash
intent check <spec.cue> --target <url>  # Run spec against target
intent validate <spec.cue>               # Validate CUE syntax only
intent show <spec.cue>                   # Pretty print spec
intent export <spec.cue>                 # Export to JSON
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
