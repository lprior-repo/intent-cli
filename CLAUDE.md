# Claude Instructions for Intent CLI

## 🚨 MANDATORY: Moon CI/CD Only

**CRITICAL**: This project uses moon for ALL build, test, and development tasks. You MUST use moon commands exclusively.

- ❌ **NEVER** run `gleam build`, `gleam test`, `gleam format` directly
- ✅ **ALWAYS** use `moon run :build`, `moon run :test`, `moon run :format`
- ✅ **ALWAYS** use `moon run :ci` before commits
- ✅ **ALWAYS** use `moon run :install` to install the binary system-wide

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads) for issue tracking. Use `bd` commands instead of markdown TODOs. See AGENTS.md for workflow details.

## Quick Reference

### Moon CI/CD (MANDATORY)
```bash
moon run :ci              # Run full pipeline before commits
moon run :install         # Build and install binary system-wide
moon run :format          # Auto-format code
moon run :test            # Run test suite
```

### Beads Workflow
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

**Intent** is a requirements engineering and API testing CLI that transforms vague specifications into deterministic work items. Written in Gleam (BEAM/Erlang VM).

### What Intent Does

1. **Requirements Engineering**
   - AI-driven interviews to gather complete requirements
   - EARS (Easy Approach to Requirements Syntax) pattern parsing
   - Mental lattice frameworks for completeness checking
   - Gap detection via inversion/second-order thinking

2. **API Contract Testing**
   - CUE specification parsing and validation
   - HTTP request execution against target APIs
   - Response validation with custom rules engine
   - Behavior dependency resolution

3. **KIRK Analysis** (Design by Contract)
   - Quality scoring (5 dimensions: completeness, clarity, testability, coverage, correctness)
   - Inversion checking (what could fail?)
   - Coverage analysis (HTTP methods/status codes)
   - Effects analysis (side effects and state changes)

4. **Bead Generation**
   - Atomic work item synthesis from requirements
   - Dependency tracking between beads
   - Feedback loop for iterative refinement
   - CUE-formatted output for AI consumption

5. **Plan Mode**
   - Collaborative specification refinement
   - Iterative feedback cycles
   - Human approval gates

### The Vision

```
Human writes requirements → CLI interviews systematically →
CUE schemas control AI → Atomic beads generated → AI executes deterministically
```

CUE is the center: requirements, interview state, AI directives, beads, and feedback all flow through validated CUE schemas.

## Key Files

### Core Entry Point
- `src/intent.gleam` - CLI with glint commands (check, validate, interview, beads, quality, etc.)

### Interview System
- `src/intent/interview.gleam` - Interview orchestration engine
- `src/intent/interview_questions.gleam` - Question database
- `src/intent/interview_storage.gleam` - Session persistence
- `src/intent/question_loader.gleam` - CUE question loading
- `src/intent/answer_loader.gleam` - Answer processing

### KIRK Analysis
- `src/intent/kirk/quality_analyzer.gleam` - 5-dimension quality scoring
- `src/intent/kirk/inversion_checker.gleam` - Failure mode analysis
- `src/intent/kirk/coverage_analyzer.gleam` - Test coverage metrics
- `src/intent/kirk/gap_detector.gleam` - Mental lattice gap detection
- `src/intent/kirk/ears_parser.gleam` - EARS requirements parsing
- `src/intent/kirk/effects_analyzer.gleam` - Side effect analysis

### API Testing
- `src/intent/checker.gleam` - Response validation (largest module)
- `src/intent/runner.gleam` - Test execution orchestrator
- `src/intent/http_client.gleam` - HTTP request execution
- `src/intent/rules_engine.gleam` - Global rule evaluation
- `src/intent/anti_patterns.gleam` - Anti-pattern detection

### Core Infrastructure
- `src/intent/types.gleam` - Custom type definitions (Spec, Behavior, Request, Response, Check)
- `src/intent/parser.gleam` - JSON to Gleam type parsing
- `src/intent/loader.gleam` - CUE file loading via system calls
- `src/intent/resolver.gleam` - Behavior dependency resolution
- `src/intent/validator.gleam` - Spec validation
- `src/intent/interpolate.gleam` - Variable interpolation
- `src/intent/output.gleam` - Result formatting

### Bead System
- `src/intent/bead_templates.gleam` - Atomic work item generation
- `src/intent/bead_feedback.gleam` - Iterative refinement
- `src/intent/spec_builder.gleam` - Spec construction from beads

### Utilities
- `src/intent/rule.gleam` - Rule expression parser
- `src/intent/formats.gleam` - Output formatting
- `src/intent/errors.gleam` - Error type definitions
- `src/intent_ffi.erl` - Erlang FFI for system operations
- `src/intent/stdin.gleam` - Standard input handling
- `src/intent/cli_ui.gleam` - Terminal UI components
- `src/intent/security.gleam` - Security validation

## Development Commands

**CRITICAL**: Use moon exclusively. Direct gleam commands are prohibited.

```bash
# Moon CI/CD commands (REQUIRED)
moon run :ci             # Full pipeline: format → build → test
moon run :format         # Auto-format code
moon run :build          # Compile project
moon run :test           # Run test suite (1201+ tests)
moon run :install        # Build and install binary to ~/.local/bin
moon run :dev            # Run local development server

# Run CLI commands (after install)
intent check examples/user-api.cue --target http://localhost:8080
intent interview --profile api
intent quality examples/user-api.cue
intent invert examples/user-api.cue
intent beads <session-id>
```

## Moon CI/CD Pipeline

This project uses [moon](https://moonrepo.dev/) for local CI/CD pipelines and task orchestration. Moon ensures code quality through automated checks before committing.

### Installation

Moon is already installed in `~/.moon/bin`. Add to your PATH:

```bash
export PATH="$HOME/.moon/bin:$PATH"
```

### Core Commands

```bash
# Run complete CI pipeline (format → build → test)
moon run :ci

# Run individual tasks
moon run :format-check    # Check formatting
moon run :build           # Compile project
moon run :test            # Run test suite
moon run :escript         # Build single binary

# Development
moon run :format          # Auto-format code
moon run :dev             # Run local development server

# Quality checks
moon check                # Validate moon configuration
```

### Pipeline Tasks

The pipeline automatically runs tasks in dependency order:

1. **format-check**: Ensures code is formatted with `gleam format --check`
2. **build**: Compiles the project (`gleam build --target erlang`)
3. **test**: Runs the full test suite (1201+ tests)
4. **escript**: Builds single binary executable at `dist/intent/intent`

### Smart Caching

Moon uses hash-based caching to skip unchanged tasks:

```bash
# First run: builds everything
moon run :ci
▪▪▪▪ intent-cli:format-check (726ms)
▪▪▪▪ intent-cli:build (2s 747ms)
▪▪▪▪ intent-cli:test (6s 836ms)

# Second run: uses cache
moon run :ci
▪▪▪▪ intent-cli:format-check (cached)
▪▪▪▪ intent-cli:build (cached)
▪▪▪▪ intent-cli:test (cached)
```

### Building Single Binary

Compile to standalone executable:

```bash
moon run :escript
# Creates: dist/intent/intent (2.1MB escript)
```

The binary requires Erlang/OTP installed on the target system.

### Installing Binary to PATH

To build and install the `intent` binary system-wide:

```bash
# Single command to build and install
moon run :install

# Add ~/.local/bin to PATH (if not already present)
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc

# Reload shell or source config
source ~/.bashrc  # or source ~/.zshrc
```

The `:install` task automatically:
1. Runs the full CI pipeline (format-check → build → test)
2. Builds the escript binary at `dist/intent/intent`
3. Creates `~/.local/bin` directory
4. Copies binary to `~/.local/bin/intent`
5. Sets executable permissions

Verify installation:

```bash
intent --help
```

### Configuration Files

- **`.moon/workspace.yml`**: Workspace-level configuration
- **`moon.yml`**: Project-level tasks and dependencies
- **`dist/`**: Output directory for compiled binaries (gitignored)

### Pre-Commit Quality Gates

Before committing, moon ensures:
- ✅ No formatting violations
- ✅ No compilation errors
- ✅ No warnings
- ✅ All tests passing (1201+ tests)
- ✅ No type errors

**MANDATORY**: Run the full pipeline before commits:

```bash
moon run :ci && git commit -m "Your message"
```

Never commit without running `moon run :ci` first.

### Task Dependencies

Tasks run in topological order based on dependencies:

```
format-check → build → test → escript → install
                ↓
              check (type checking)
```

The `:install` task depends on `:escript`, which depends on `:test`, ensuring the binary is always built from passing code.

Moon automatically parallelizes independent tasks for optimal performance.

## The 7 Commandments of Gleam

When writing or modifying Gleam code, strictly follow these principles:

### 1. Explicitness Over Implicitness
- **No implicit type conversions**: `int.to_string(42)` not `42` as string
- **No operator overloading**: `+` for Int, `+.` for Float, `<>` for String concatenation
- **No exceptions for control flow**: Use `Result(a, e)` types
- **Explicit transformations**: Every conversion is a function call

```gleam
// Bad (would not compile)
let x = 42
let y = x + 3.14  // Type error!

// Good
let x = 42
let y = int.to_float(x) +. 3.14
```

### 2. Immutability by Default
- Variables are **labels for values**, not buckets that change
- "Mutation" is creating a new binding with the same name (shadowing)
- Lists, records, all data structures are immutable

```gleam
// Idiomatic shadowing pattern
let user = "  alice  "
let user = string.trim(user)
let user = string.lowercase(user)
io.println(user)  // "alice"
```

### 3. Type-First Design
- Define custom types before writing logic
- **No null/nil/undefined** - use `Option(T)` for optional values
- **Union types** for states (not boolean flags or integer codes)
- **Opaque types** for domain entities requiring validation

```gleam
// Model states as unions, not flags
pub type ConnectionState {
  Disconnected
  Connecting(attempt: Int)
  Connected(ip: String, port: Int)
}

// Opaque wrapper for validation
pub opaque type Email {
  Email(String)
}

pub fn new(value: String) -> Result(Email, String) {
  case string.contains(value, "@") {
    True -> Ok(Email(value))
    False -> Error("Invalid email format")
  }
}
```

### 4. Exhaustive Pattern Matching with case
- Prefer `case` over `if` for almost all logic
- Compiler enforces exhaustiveness
- Use tuple matching for complex conditions
- Guards limited to comparisons (no function calls)

```gleam
// Idiomatic: tuple matching
case user.role, is_authenticated {
  Admin, True -> admin_dashboard()
  User, True -> user_home()
  _, False -> login_page()
}

// Guards with safe operations only
case list {
  [x, ..] if x % 2 == 0 -> "Even"
  [x, ..] -> "Odd"
  [] -> "Empty"
}
```

### 5. Pipeline Flow
- Transform data with `|>` operator
- Subject-first parameter order in all functions
- Prefer pipelines over nested function calls
- Use `_` capture syntax for non-first argument position

```gleam
// Idiomatic pipeline
raw_input
|> string.trim
|> string.lowercase
|> string.split(on: ",")
|> list.map(string.trim)
|> list.filter(fn(s) { s != "" })

// Capture syntax for non-first argument
10
|> int.add(5, _)  // Passes 10 as second argument
```

### 6. Railway-Oriented Error Handling
- **Never use exceptions** for control flow
- Return `Result(value, error)` for all fallible operations
- Chain with `result.try` or `use` expression
- Map errors to domain types with `result.map_error`

```gleam
// The 'use' expression flattens Result chains
pub fn load_config() -> Result(Config, AppError) {
  use content <- result.try(
    simplifile.read("config.json")
    |> result.map_error(FileError)
  )

  use data <- result.try(
    json.decode(content, config_decoder)
    |> result.map_error(fn(_) { ParseError("Invalid JSON") })
  )

  Ok(data)
}
```

### 7. Strict Naming Conventions
- **Variables/functions**: `snake_case` (e.g., `user_id`, `parse_request`)
- **Types/Constructors**: `PascalCase` (e.g., `User`, `HttpRequest`, `Ok`, `Error`)
- **Modules**: `snake_case` matching filename (e.g., `intent/http_client`)
- **Constants**: `SCREAMING_SNAKE_CASE` (e.g., `MAX_RETRIES`)
- Casing is **semantic** and enforced by the compiler

```gleam
// Custom type definition
pub type User {
  User(id: Int, name: String, email: Option(String))
}

// Function using the type
pub fn create_user(id: Int, name: String) -> User {
  User(id: id, name: name, email: option.None)
}
```

## Anti-Patterns to Avoid

1. **Bool Blindness**: Don't use `Bool` for complex states
   - Bad: `fn check_login() -> Bool`
   - Good: `fn check_login() -> Result(User, LoginError)`

2. **Stringly Typed**: Don't use strings for enums
   - Bad: `status = "connected"`
   - Good: `status = Connected`

3. **Index Iteration**: Lists are linked lists, not arrays
   - Bad: Loop with `list.length` and indexing (O(n²))
   - Good: Use `list.map`, `list.fold`, pattern matching

4. **Primitive Obsession**: Don't pass raw `Int`/`String` everywhere
   - Bad: `fn get_user(id: Int)`
   - Good: `pub opaque type UserId { UserId(Int) }`

5. **Manual Recursion**: Prefer standard library over hand-written loops
   - Bad: Write recursive functions for everything
   - Good: Use `list.map`, `list.fold`, `list.filter`

6. **Panic in Libraries**: Never use `panic` for validation in library code
   - Bad: `panic as "Invalid input"`
   - Good: Return `Result(T, Error)`

## Spec Format Requirements

All fields in Intent CUE specifications are **required** (no defaults):

### Required Spec Fields
- `name`, `description`, `audience`, `version`
- `success_criteria` - List of acceptance criteria
- `config` - `base_url`, `timeout_ms`, `headers`
- `features` - List of feature specifications (non-empty)
- `rules` - Global validation rules
- `anti_patterns` - Patterns to avoid
- `ai_hints` - Implementation guidance

### Required Feature Fields
- `name`, `description`
- `behaviors` - List of behavior specifications (non-empty)

### Required Behavior Fields
- `name`, `intent`
- `request` - `method`, `path`, `headers`, `query`, `body`
- `response` - `status`, `example`, `checks`, `headers`
- `notes` - Implementation notes (can be empty string)
- `requires` - Behavior dependencies (can be empty list)
- `tags` - Classification tags (can be empty list)
- `captures` - Output values (can be empty dict)

### Required Check Fields
- `rule` - Validation rule expression
- `why` - Explanation of why this check matters

## Testing Conventions

- Test files in `test/` directory mirror `src/` structure
- Test modules end in `_test.gleam`
- Test functions are `pub fn` and end in `_test`
- Use `gleeunit/should` for assertions: `should.equal`, `should.be_ok`
- Keep tests fast (EUnit default timeout: 5 seconds)
- Use higher-order functions for mocking (no traditional mocking framework)

```gleam
import gleeunit/should

pub fn parse_valid_email_test() {
  email.new("user@example.com")
  |> should.be_ok
}

pub fn parse_invalid_email_test() {
  email.new("not-an-email")
  |> should.be_error
}
```

## Documentation Standards

- Module docs: `////` at top of file
- Function docs: `///` immediately before `pub fn`
- Regular comments: `//` for implementation details
- Always document public APIs
- Explain the "why", not just the "what"

```gleam
//// This module provides HTTP request execution with retry logic
//// and automatic timeout handling for API testing.

/// Execute an HTTP request with the given configuration.
///
/// Returns the response body and status code on success.
/// Returns an error if the request fails or times out.
pub fn execute(request: Request) -> Result(Response, HttpError) {
  // Implementation
}
```

## Module Organization

- **File = Module**: One-to-one mapping
- **No circular dependencies**: Extract shared types to separate module
- **Private by default**: Use `pub` explicitly for public APIs
- **Opaque types**: Use `pub opaque type` for encapsulation

```
src/intent/
  types.gleam          # Shared types (imported by many modules)
  parser.gleam         # Imports types
  checker.gleam        # Imports types and parser
  http_client.gleam    # Imports types only
```

## Git Workflow with bd

1. **Find work**: `bd ready` or `bv --robot-next`
2. **Claim task**: `bd update <id> --status in_progress`
3. **Implement with commits**: Stage code changes
4. **Beads auto-sync**: Daemon handles `.beads/issues.jsonl` automatically
5. **Complete work**: `bd close <id> --reason "Done"`
6. **Push code**: `git push` (beads already synced by daemon)

**IMPORTANT**: Daemon auto-syncs beads. You only commit/push your code changes.

## When Adding New Features

1. **Check for beads**: `bd ready` for planned work
2. **Define types first**: Create custom types before logic
3. **Write tests**: Test file in `test/` directory
4. **Implement using pipelines**: Transform data with `|>`
5. **Handle all errors**: Return `Result`, never panic
6. **Run CI pipeline**: `moon run :ci` (includes formatting, building, testing)
7. **Update docs**: Add `///` comments for public functions
8. **Commit changes**: `git add . && git commit -m "message"`
9. **Close bead**: `bd close <id>` when complete
10. **Install binary**: `moon run :install` to update system-wide binary

## Resources

- [Gleam Language Tour](https://tour.gleam.run/)
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/)
- [bd (beads) Documentation](https://github.com/steveyegge/beads)
- [CUE Language](https://cuelang.org/)
