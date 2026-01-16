# Intent CLI - AI Agent Workflow Guide

## Project Overview

**Intent** is a requirements engineering and API testing CLI written in Gleam that transforms vague requirements into deterministic, atomic work items. It combines:

- **Requirements Engineering**: AI-driven interviews, EARS patterns, mental lattice frameworks
- **API Testing**: CUE spec parsing, HTTP request execution, response validation
- **KIRK Analysis**: Design by Contract (quality, inversion, coverage, gap detection)
- **Bead Generation**: Atomic work item synthesis with dependency tracking
- **Plan Mode**: Collaborative specification refinement with human approval gates

**Tech Stack**: Gleam (BEAM/Erlang VM), CUE for specifications, glint for CLI

**Vision**: `Human writes requirements → CLI interviews → CUE controls AI → Atomic beads → AI executes deterministically`

## Issue Tracking with bd (beads)

**CRITICAL**: This project uses **bd (beads)** exclusively for issue tracking. Do NOT use markdown TODOs, task lists, GitHub issues, or other tracking methods.

### Why bd?

- **Dependency-aware**: Track blockers and relationships between issues
- **Git-friendly**: Auto-syncs to JSONL (`.beads/issues.jsonl`) for version control
- **Agent-optimized**: JSON output, ready work detection, discovered-from links
- **Session persistence**: Survives context resets and handoffs
- **Prevents duplication**: Single source of truth for all work

### Core Commands

**Finding work:**
```bash
bd ready --json                    # Show unblocked issues ready to work
bd list --status=open --json       # All open issues
bd list --status=in_progress --json # Your active work
bd show <id> --json                # Detailed issue view with dependencies
```

**Creating issues:**
```bash
bd create --title="Issue title" --type=task|bug|feature --priority=2 --json
bd create --title="Subtask" --parent <epic-id> --json  # Hierarchical subtask
bd create --title="Found bug" --priority=1 --deps discovered-from:<parent-id> --json
```

**Updating and completing:**
```bash
bd update <id> --status=in_progress --json    # Claim work
bd update <id> --priority=1 --json             # Update priority
bd close <id> --reason "Completed" --json      # Complete single issue
bd close <id1> <id2> <id3> --json             # Bulk close (more efficient)
```

**Dependencies:**
```bash
bd dep add <issue> <depends-on> --json         # Add dependency (issue blocks depends-on)
bd blocked --json                              # Show all blocked issues
```

**Project health:**
```bash
bd stats --json                                # Open/closed/blocked counts
bd doctor --json                               # Check for sync problems
```

### Issue Types

- `bug` - Something broken that needs fixing
- `feature` - New functionality or major enhancement
- `task` - Work item (tests, docs, refactoring, small improvements)
- `epic` - Large feature broken into subtasks
- `chore` - Maintenance (dependencies, tooling, infrastructure)

### Priority Levels

- `0` (P0) - **Critical**: Security vulnerabilities, data loss, broken builds
- `1` (P1) - **High**: Major features, important bugs affecting core functionality
- `2` (P2) - **Medium**: Default priority, nice-to-have features
- `3` (P3) - **Low**: Polish, optimization, minor improvements
- `4` (P4) - **Backlog**: Future ideas, long-term enhancements

**IMPORTANT**: Use numeric priorities (0-4) or P0-P4 format. NOT "high"/"medium"/"low".

### AI Agent Workflow

1. **Check for ready work**
   ```bash
   bd ready --json
   # or use bv for intelligent recommendations
   bv --robot-next
   ```

2. **Claim your task**
   ```bash
   bd update <id> --status=in_progress --json
   ```

3. **Work on implementation**
   - Follow the 7 Commandments of Gleam (see CLAUDE.md)
   - Write tests first or alongside implementation
   - Use pipelines (`|>`) for data transformation
   - Handle all errors with `Result` types
   - Format code with `gleam format`

4. **Discover new work?**
   - Create linked issues with dependency tracking:
   ```bash
   bd create --title="Found edge case bug" --type=bug --priority=1 --deps discovered-from:<parent-id> --json
   ```

5. **Complete the work**
   ```bash
   bd close <id> --reason "Implemented with tests" --json
   ```

6. **Commit code changes**
   - Stage and commit your code changes
   - Daemon auto-syncs `.beads/issues.jsonl` separately
   - Push your code to remote
   ```bash
   git add src/intent/new_feature.gleam test/intent/new_feature_test.gleam
   git commit -m "Implement new feature (closes <id>)"
   git push
   ```

**Note**: The bd daemon handles beads sync automatically (auto-commit + auto-push + auto-pull). You only manage your code commits.

## Beads Viewer (bv) - AI Agent Commands

**CRITICAL**: Use ONLY `--robot-*` flags. Bare `bv` launches an interactive TUI that will block your session.

### Robot Commands (Non-Blocking, JSON Output)

```bash
# Intelligent work recommendation
bv --robot-triage          # Comprehensive analysis with priority recommendations
bv --robot-next            # Single top pick with ready-to-run claim command

# Strategic planning
bv --robot-plan            # Parallel execution tracks with unblock lists
bv --robot-insights        # Full metrics (PageRank, betweenness, critical path)
bv --robot-label-health    # Per-label health assessment

# Dependency analysis
bv --robot-graph --graph-format=json  # Export dependency graph as JSON
```

### Usage Tips

- Start with `bv --robot-next` for a single high-value task recommendation
- Use `bv --robot-plan` to see parallel execution opportunities
- Use `bv --robot-insights` to understand critical paths and bottlenecks
- Never use bare `bv` - it launches an interactive terminal UI

### Installation

```bash
curl -fsSL "https://raw.githubusercontent.com/Dicklesworthstone/beads_viewer/main/install.sh" | bash
```

## Project Structure

```
intent-cli/
├── src/
│   ├── intent.gleam              # CLI entry point with glint commands
│   ├── intent_ffi.erl            # Erlang FFI for system operations
│   └── intent/
│       ├── types.gleam           # Core type definitions
│       ├── errors.gleam          # Error type definitions
│       │
│       # Interview System
│       ├── interview.gleam       # Interview orchestration engine
│       ├── interview_questions.gleam  # Question database
│       ├── interview_storage.gleam    # Session persistence
│       ├── question_types.gleam       # Question type definitions
│       ├── question_loader.gleam      # CUE question loading
│       ├── answer_loader.gleam        # Answer processing
│       │
│       # KIRK Analysis
│       ├── kirk/
│       │   ├── quality_analyzer.gleam    # 5-dimension quality scoring
│       │   ├── inversion_checker.gleam   # Failure mode analysis
│       │   ├── coverage_analyzer.gleam   # Test coverage metrics
│       │   ├── gap_detector.gleam        # Mental lattice gap detection
│       │   ├── ears_parser.gleam         # EARS requirements parsing
│       │   ├── effects_analyzer.gleam    # Side effect analysis
│       │   └── compact_format.gleam      # Compact spec format
│       │
│       # API Testing
│       ├── checker.gleam         # Response validation engine
│       ├── runner.gleam          # Test execution orchestrator
│       ├── http_client.gleam     # HTTP request execution
│       ├── rules_engine.gleam    # Global rule evaluation
│       ├── anti_patterns.gleam   # Anti-pattern detection
│       │
│       # Core Infrastructure
│       ├── parser.gleam          # JSON to Gleam type parsing
│       ├── loader.gleam          # CUE file loading via system calls
│       ├── resolver.gleam        # Behavior dependency resolution
│       ├── validator.gleam       # Spec validation
│       ├── interpolate.gleam     # Variable interpolation
│       ├── output.gleam          # Result formatting
│       ├── formats.gleam         # Output formatting utilities
│       │
│       # Bead System
│       ├── bead_templates.gleam  # Atomic work item generation
│       ├── bead_feedback.gleam   # Iterative refinement
│       ├── spec_builder.gleam    # Spec construction from beads
│       │
│       # Utilities
│       ├── rule.gleam            # Rule expression parser
│       ├── stdin.gleam           # Standard input handling
│       ├── cli_ui.gleam          # Terminal UI components
│       ├── security.gleam        # Security validation
│       ├── spec_linter.gleam     # Spec quality linting
│       ├── improver.gleam        # Spec improvement suggestions
│       ├── plan_mode.gleam       # Plan mode orchestration
│       ├── case_insensitive.gleam # Case-insensitive utilities
│       └── array_indexing.gleam  # Array indexing utilities
│
├── test/                         # Test suite (mirrors src structure)
├── examples/                     # Example CUE specifications
├── schema/                       # CUE schema definitions
│   ├── intent.cue               # Core spec schema
│   ├── kirk.cue                 # KIRK contract types
│   ├── questions.cue            # Interview questions database
│   └── ai_protocol.cue          # AI directive schemas
│
├── .beads/
│   ├── issues.jsonl             # Issue database (auto-synced by daemon)
│   └── last-touched             # Daemon tracking file
│
├── gleam.toml                   # Package configuration
├── CLAUDE.md                    # This file - AI agent instructions
├── AGENTS.md                    # Workflow guide
└── README.md                    # Project overview
```

## Development Commands

```bash
# Build and test
gleam build                      # Compile the project
gleam test                       # Run test suite
gleam format                     # Format code (mandatory before commit)
gleam run -- <command>           # Run CLI with arguments

# Core CLI commands
gleam run -- check examples/user-api.cue --target http://localhost:8080
gleam run -- validate examples/user-api.cue
gleam run -- show examples/user-api.cue
gleam run -- export examples/user-api.cue

# Interview and beads
gleam run -- interview --profile api
gleam run -- beads <session-id>
gleam run -- sessions

# KIRK analysis
gleam run -- quality examples/user-api.cue
gleam run -- invert examples/user-api.cue
gleam run -- coverage examples/user-api.cue
gleam run -- gaps examples/user-api.cue
gleam run -- ears requirements.md --output cue
```

## The 7 Commandments of Gleam

Every piece of Gleam code must follow these principles. For full details and examples, see CLAUDE.md.

1. **Explicitness Over Implicitness**
   - No implicit conversions, operator overloading, or exceptions
   - `int.to_string(42)`, not type coercion
   - `+` for Int, `+.` for Float, `<>` for String concatenation

2. **Immutability by Default**
   - Variables are labels, not buckets
   - Use shadowing for transformations
   - All data structures are immutable

3. **Type-First Design**
   - Define custom types before logic
   - Use `Option(T)` instead of null
   - Union types for states, opaque types for validation

4. **Exhaustive Pattern Matching**
   - Prefer `case` over `if`
   - Compiler enforces completeness
   - Tuple matching for complex conditions

5. **Pipeline Flow**
   - Transform data with `|>` operator
   - Subject-first parameter order
   - Use `_` capture for non-first arguments

6. **Railway-Oriented Error Handling**
   - Return `Result(value, error)` for fallible operations
   - Chain with `use` expression or `result.try`
   - Map errors to domain types

7. **Strict Naming Conventions**
   - Variables/functions: `snake_case`
   - Types/Constructors: `PascalCase`
   - Modules: `snake_case` (matches filename)
   - Constants: `SCREAMING_SNAKE_CASE`

## Anti-Patterns to Avoid

1. **Bool Blindness**: Use `Result` or union types, not `Bool` for complex states
2. **Stringly Typed**: Use custom types, not strings for enums
3. **Index Iteration**: Use `list.map`/`fold`, not manual indexing (lists are linked, not arrays)
4. **Primitive Obsession**: Wrap domain IDs in opaque types
5. **Manual Recursion**: Prefer standard library functions
6. **Panic in Libraries**: Return `Result`, never use `panic` for validation

## Testing Strategy

### Test Structure
- Test files in `test/` mirror `src/` structure
- Test modules end in `_test.gleam`
- Test functions are `pub fn` and end in `_test`
- Use `gleeunit/should` for assertions

### Testing Best Practices
```gleam
import gleeunit/should
import intent/parser

pub fn parse_valid_spec_test() {
  let json = "{\"name\": \"test\", ...}"
  parser.parse_spec(json)
  |> should.be_ok
}

pub fn parse_invalid_spec_test() {
  let json = "{\"invalid\": true}"
  parser.parse_spec(json)
  |> should.be_error
}

pub fn pipeline_transformation_test() {
  "  HELLO  "
  |> string.trim
  |> string.lowercase
  |> should.equal("hello")
}
```

### Testing Guidelines
- Write tests before or alongside implementation (TDD)
- Test happy path and error cases
- Keep tests fast (< 5 seconds)
- Use higher-order functions for test doubles (no mocking framework)
- Test public APIs, not internal implementation details

## Documentation Standards

### Module Documentation
```gleam
//// This module provides HTTP request execution with automatic
//// retry logic and timeout handling for API testing.
////
//// All functions return Result types for proper error handling.

import gleam/http/request
import gleam/result
```

### Function Documentation
```gleam
/// Execute an HTTP request with the given configuration.
///
/// Automatically retries on transient failures (network errors, timeouts).
/// Returns the response body and status code on success.
///
/// ## Examples
///
/// ```gleam
/// let request = Request(method: Get, url: "https://api.example.com")
/// case execute(request) {
///   Ok(response) -> io.println(response.body)
///   Error(err) -> io.println("Failed: " <> describe_error(err))
/// }
/// ```
pub fn execute(request: Request) -> Result(Response, HttpError) {
  // Implementation
}
```

### Documentation Guidelines
- Use `////` for module-level docs
- Use `///` for public function docs
- Use `//` for implementation comments
- Document the "why", not just the "what"
- Include examples for non-obvious usage
- Document error cases and edge conditions

## Git Workflow

### Standard Development Flow

1. **Find work**: `bd ready --json` or `bv --robot-next`
2. **Claim task**: `bd update <id> --status=in_progress --json`
3. **Create branch** (optional): `git checkout -b feature/<id>-description`
4. **Implement with tests**:
   - Write tests first or alongside code
   - Follow the 7 Commandments
   - Use pipelines and Result types
   - Format with `gleam format`
5. **Run tests**: `gleam test`
6. **Commit code**:
   ```bash
   git add src/intent/new_feature.gleam test/intent/new_feature_test.gleam
   git commit -m "Implement new feature

   - Add HTTP retry logic with exponential backoff
   - Handle timeout and network errors gracefully
   - Add comprehensive tests for error cases

   Closes <bead-id>"
   ```
7. **Close bead**: `bd close <id> --reason "Implemented with tests" --json`
8. **Push**: `git push origin main` (or feature branch)

### Commit Message Format

```
<Short summary (50 chars or less)>

<Detailed explanation if needed>
- Bullet points for key changes
- Explain the "why" for non-obvious decisions
- Reference any relevant beads

Closes <bead-id>
```

### Important Notes

- **Daemon handles beads sync**: `.beads/issues.jsonl` is auto-committed and pushed by daemon
- **You manage code commits**: Stage, commit, and push your code changes
- **No manual bead sync**: Don't run `bd sync` - daemon handles it
- **Close before push**: Close beads before pushing code to keep state in sync

## Parallel Work with bd

### Creating Multiple Issues

For efficiency, create multiple related issues at once:

```bash
# Create feature and related tasks
bd create --title="Implement retry logic" --type=feature --priority=1 --json
bd create --title="Add retry tests" --type=task --priority=1 --json
bd create --title="Document retry behavior" --type=task --priority=2 --json

# Add dependencies (tests depend on feature)
bd dep add <test-id> <feature-id> --json
```

### Batch Operations

Close multiple completed issues at once:

```bash
bd close <id1> <id2> <id3> --reason "All completed" --json
```

### Using bv for Parallel Planning

```bash
# See what can be done in parallel
bv --robot-plan

# Output shows independent work tracks:
# Track 1: [id-1, id-2, id-3]  (can work sequentially)
# Track 2: [id-4, id-5]        (independent, can do in parallel)
# Track 3: [id-6]              (independent)
```

## When Adding New Features

### Checklist

1. **Check for planned work**: `bd ready --json`
2. **Understand requirements**: Read bead description, check dependencies
3. **Define types first**: Create custom types before logic
4. **Write test file**: Create `test/intent/feature_test.gleam`
5. **Implement using pipelines**: Transform data with `|>`
6. **Handle all errors**: Return `Result`, never `panic` in libraries
7. **Format code**: `gleam format` before commit
8. **Update documentation**: Add `///` docs for public functions
9. **Run tests**: `gleam test` must pass
10. **Close bead**: `bd close <id> --reason "Done" --json`

### Type-First Development Pattern

```gleam
// 1. Define types
pub type RetryConfig {
  RetryConfig(max_attempts: Int, backoff_ms: Int)
}

pub type RetryError {
  MaxRetriesExceeded
  PermanentFailure(String)
}

// 2. Define function signatures (return Result)
pub fn execute_with_retry(
  request: Request,
  config: RetryConfig,
) -> Result(Response, RetryError)

// 3. Write tests
pub fn retry_succeeds_on_second_attempt_test() { ... }
pub fn retry_fails_after_max_attempts_test() { ... }

// 4. Implement using pipelines and pattern matching
pub fn execute_with_retry(request, config) {
  execute_attempt(request, config, 1)
}

fn execute_attempt(request, config, attempt) {
  case http_client.execute(request) {
    Ok(response) -> Ok(response)
    Error(NetworkError) if attempt < config.max_attempts ->
      execute_attempt(request, config, attempt + 1)
    Error(_) -> Error(MaxRetriesExceeded)
  }
}
```

## Working with CUE Specifications

### CUE File Structure

All fields are **required** (no defaults):

```cue
{
  name: "User API"
  description: "User management endpoints"
  audience: "Frontend developers"
  version: "1.0.0"
  success_criteria: ["Users can register", "Users can login"]

  config: {
    base_url: "http://localhost:8080"
    timeout_ms: 5000
    headers: {"Content-Type": "application/json"}
  }

  features: [{
    name: "Authentication"
    description: "User authentication flows"
    behaviors: [{
      name: "register-user"
      intent: "Allow new users to create accounts"
      request: {
        method: "POST"
        path: "/users"
        headers: {}
        query: {}
        body: "{\"email\": \"test@example.com\", \"password\": \"secret\"}"
      }
      response: {
        status: 201
        example: "{\"id\": 1, \"email\": \"test@example.com\"}"
        checks: [{
          rule: "body.id > 0"
          why: "User ID must be positive"
        }]
        headers: {}
      }
      notes: ""
      requires: []
      tags: ["auth", "registration"]
      captures: {}
    }]
  }]

  rules: ["All responses must have Content-Type header"]
  anti_patterns: ["Don't return passwords in responses"]
  ai_hints: {
    implementation: ["Use bcrypt for password hashing"]
    entities: ["User"]
    security: ["Validate email format", "Enforce password strength"]
  }
}
```

### Validating Specs

```bash
# Validate CUE syntax
gleam run -- validate examples/user-api.cue

# Check quality
gleam run -- quality examples/user-api.cue

# Find gaps
gleam run -- gaps examples/user-api.cue

# Analyze coverage
gleam run -- coverage examples/user-api.cue
```

## Important Rules

### ✅ DO

- Use `bd` for ALL task tracking
- Always use `--json` flag with bd commands for programmatic use
- Link discovered work with `discovered-from` dependencies
- Check `bd ready` before asking "what should I work on?"
- Follow the 7 Commandments of Gleam religiously
- Write tests for all new code
- Format code with `gleam format` before committing
- Document public APIs with `///` comments
- Use `Result` types for all fallible operations
- Use pipelines (`|>`) for data transformation
- Define types before writing logic
- Close beads when work is complete

### ❌ DO NOT

- Create markdown TODO lists or task comments in code
- Use external issue trackers (GitHub issues, etc.)
- Duplicate tracking systems
- Use bare `bv` command (use `--robot-*` flags only)
- Skip tests or commit failing tests
- Use `panic` for error handling in library code
- Use implicit conversions or type coercion
- Write manual recursive loops (use `list.map`/`fold`)
- Use boolean flags for complex states
- Use strings for enums
- Forget to format code with `gleam format`

## Resources

### Gleam
- [Gleam Language Tour](https://tour.gleam.run/)
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/)
- [Gleam Book](https://gleam.run/book/)

### Tools
- [bd (beads) Documentation](https://github.com/steveyegge/beads)
- [beads viewer (bv)](https://github.com/Dicklesworthstone/beads_viewer)
- [CUE Language](https://cuelang.org/)

### Project Documentation
- `CLAUDE.md` - Full AI agent instructions with Gleam commandments
- `README.md` - Project overview and vision
- `REVERSE_PROMPT.md` - Reverse prompt implementation guide
