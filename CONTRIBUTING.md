# Contributing to Intent CLI

Thank you for your interest in contributing to Intent CLI! This guide will help you get started with development, testing, and submitting contributions.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [Working with Beads](#working-with-beads)
- [Project Structure](#project-structure)
- [Common Tasks](#common-tasks)

## Getting Started

### Prerequisites

Before you begin, ensure you have the following installed:

- **Gleam** (1.0 or later) - [Installation Guide](https://gleam.run/getting-started/installing-gleam/)
- **Erlang/OTP 27+** - Required by Gleam
- **Git** - For version control
- **bd (Beads)** - For issue tracking (optional but recommended)

Verify your installation:

```bash
# Check Gleam
gleam --version

# Check Erlang
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

# Check bd (optional)
bd --version
```

### Initial Setup

1. **Fork and clone the repository:**

```bash
git clone https://github.com/YOUR_USERNAME/intent-cli.git
cd intent-cli
```

2. **Build the project:**

```bash
gleam build
```

3. **Run tests to verify everything works:**

```bash
gleam test
```

4. **Install the binary locally (optional):**

```bash
gleam build
# The binary will be at: build/dev/erlang/intent/intent
# Add to PATH or create an alias
```

## Development Workflow

### Finding Work

This project uses **Beads** for issue tracking. Issues live in `.beads/issues.jsonl` right in the repository.

```bash
# List all ready-to-work issues
bd ready

# Show details of a specific issue
bd show <issue-id>

# Get AI-friendly recommendations (requires bv tool)
bv --robot-next    # Get single top pick
bv --robot-triage  # Get comprehensive analysis
```

### Claiming Work

Before starting work on an issue:

```bash
# Claim the issue
bd update <issue-id> --status in_progress
```

### Making Changes

1. **Create a feature branch:**

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/bug-description
```

2. **Make your changes following our [coding standards](#coding-standards)**

3. **Write or update tests** - see [Testing](#testing)

4. **Format your code:**

```bash
gleam format
```

5. **Run the full test suite:**

```bash
gleam test
```

6. **Build to ensure no compilation errors:**

```bash
gleam build
```

### Committing Changes

We use conventional commits for clarity:

```bash
# Format: <type>(<scope>): <description>
#
# Types: feat, fix, docs, test, refactor, chore, style

git add .
git commit -m "feat(interview): add session resumption support"
```

**Important:** The pre-commit hook automatically syncs Beads issues to `.beads/issues.jsonl`. You don't need to manually sync.

### Completing Work

Once your work is done:

```bash
# Close the bead
bd close <issue-id> --reason "Implemented feature X with tests"

# Push your changes
git push origin your-branch-name
```

## Coding Standards

Intent CLI is written in **Gleam**, a type-safe functional language. We follow strict conventions to maintain code quality.

### The 7 Gleam Commandments

#### 1. Explicitness
- No implicit type conversions
- No operator overloading
- Explicit transformations always

```gleam
// Good
int.to_string(42)

// Bad (doesn't exist in Gleam)
string(42)
```

#### 2. Immutability
- Variables are labels for values
- Shadowing creates new bindings
- All data structures are immutable

```gleam
// Good - shadowing creates new binding
let user = get_user()
let user = string.trim(user.name)

// Values themselves never change
```

#### 3. Type-First Design
- Define custom types before logic
- Use `Option(T)` instead of null/nil
- Union types for states
- Opaque types for validation

```gleam
// Good - clear states
pub type ConnectionState {
  Connecting
  Connected(session_id: String)
  Disconnected(reason: String)
}

// Good - no null values
pub type User {
  User(name: String, email: Option(String))
}
```

#### 4. Exhaustive Pattern Matching
- Prefer `case` over `if`
- Compiler enforces exhaustiveness
- Use tuple matching for complex conditions

```gleam
// Good
case user.role, is_authenticated {
  Admin, True -> allow_admin_access()
  User, True -> allow_user_access()
  _, False -> require_login()
}

// Avoid excessive if/else chains
```

#### 5. Pipeline Flow
- Transform with `|>` operator
- Subject-first parameter order
- Pipelines over nested calls

```gleam
// Good
raw_input
|> string.trim
|> string.lowercase
|> validate_email

// Bad - nested calls are hard to read
validate_email(string.lowercase(string.trim(raw_input)))
```

#### 6. Railway-Oriented Errors
- **NEVER use exceptions or panic in library code**
- Always return `Result(value, error)`
- Chain with `result.try` or `use`
- Map errors to domain types

```gleam
// Good
pub fn load_spec(path: String) -> Result(Spec, SpecError) {
  use content <- result.try(simplifile.read(path))
  use parsed <- result.try(parse_cue(content))
  validate_spec(parsed)
}

// Bad - panic crashes the program
pub fn load_spec(path: String) -> Spec {
  case simplifile.read(path) {
    Ok(content) -> parse_or_panic(content)
    Error(_) -> panic as "File not found"
  }
}
```

#### 7. Strict Naming Conventions
- Variables/functions: `snake_case`
- Types/Constructors: `PascalCase`
- Modules: `snake_case`
- Constants: `SCREAMING_SNAKE_CASE`

```gleam
// Good
pub type UserRole {
  AdminRole
  StandardRole
}

pub fn get_user_role(user: User) -> UserRole {
  // ...
}

const MAX_RETRIES = 3
```

### Anti-Patterns to Avoid

#### Boolean Blindness
```gleam
// Bad - what does false mean?
fn check_login() -> Bool

// Good - explicit error information
fn check_login() -> Result(User, LoginError)
```

#### Stringly Typed
```gleam
// Bad - strings are error-prone
let status = "connected"

// Good - use types
let status = Connected
```

#### Index Iteration
```gleam
// Bad - lists are linked lists, O(n²) performance
let items = [1, 2, 3, 4, 5]
list.range(0, list.length(items) - 1)
|> list.map(fn(i) {
  let assert Ok(item) = list.at(items, i)
  item * 2
})

// Good - use functional operations
items
|> list.map(fn(item) { item * 2 })
```

#### Primitive Obsession
```gleam
// Bad - any Int could be a user ID
pub fn get_user(id: Int) -> Result(User, Error)

// Good - opaque type prevents misuse
pub opaque type UserId {
  UserId(Int)
}

pub fn new_user_id(id: Int) -> UserId {
  UserId(id)
}

pub fn get_user(id: UserId) -> Result(User, Error)
```

### Documentation

- Use `////` for **module documentation** (top of file)
- Use `///` for **public function documentation** (before `pub fn`)
- Use `//` for **inline comments** (explain why, not what)
- Always document public APIs
- Explain the reasoning, not just the behavior

```gleam
//// This module handles interview session management.
////
//// Sessions track the state of ongoing requirement interviews,
//// allowing resumption across context boundaries.

/// Load an interview session from disk.
///
/// Returns an error if the session file is corrupted or missing.
/// Use `create_session` to start a new session instead.
pub fn load_session(session_id: String) -> Result(Session, SessionError) {
  // Implementation uses simplifile for cross-platform compatibility
  // rather than direct file_ffi calls
  simplifile.read(session_path(session_id))
  |> result.map_error(FileNotFound)
  |> result.try(decode_session)
}
```

## Testing

Intent CLI has a comprehensive test suite (1201+ tests). Tests mirror the source structure.

### Test Structure

```
test/
├── intent_test.gleam           # Mirrors src/intent.gleam
├── interview_test.gleam        # Mirrors src/intent/interview.gleam
├── anti_patterns_test.gleam    # Mirrors src/intent/anti_patterns.gleam
└── ...
```

### Writing Tests

Use `gleeunit` and the `should` module:

```gleam
import gleeunit/should

pub fn parse_valid_spec_test() {
  let input = "..."

  parser.parse_spec(input)
  |> should.be_ok
  |> should.equal(expected_spec)
}

pub fn parse_invalid_spec_test() {
  let input = "..."

  parser.parse_spec(input)
  |> should.be_error
}

pub fn transform_pipeline_test() {
  "  HELLO  "
  |> string.trim
  |> string.lowercase
  |> should.equal("hello")
}
```

### Running Tests

```bash
# Run all tests
gleam test

# Run tests with output
gleam test -- --verbose

# Run specific test file (requires changing test runner)
gleam test
```

### Test Guidelines

- **Keep tests fast** - tests should complete in under 5 seconds total
- **One assertion per test** - tests should be focused
- **Use descriptive names** - `test_name_should_do_something_test()`
- **Test error cases** - don't just test the happy path
- **Avoid external dependencies** - mock HTTP calls, file I/O, etc.

## Submitting Changes

### Before Submitting

Run through this checklist:

- [ ] Code follows the [7 Gleam Commandments](#the-7-gleam-commandments)
- [ ] All tests pass (`gleam test`)
- [ ] Code is formatted (`gleam format`)
- [ ] Code builds without errors (`gleam build`)
- [ ] New functions have documentation comments (`///`)
- [ ] Public APIs have comprehensive docs
- [ ] Changes are committed with descriptive messages
- [ ] Beads issue is closed with `bd close <issue-id>`

### Pull Request Process

1. **Push your branch:**

```bash
git push origin your-branch-name
```

2. **Create a Pull Request on GitHub:**
   - Use a clear title describing the change
   - Reference the bead ID in the description (e.g., "Closes intent-cli-abc123")
   - Describe what changed and why
   - Note any breaking changes

3. **PR Template:**

```markdown
## Description
Brief description of changes

## Related Issues
- Closes intent-cli-abc123

## Changes
- Added feature X
- Fixed bug Y
- Refactored Z for clarity

## Testing
- Added tests for new functionality
- All existing tests pass
- Manually tested against examples/user-api.cue

## Breaking Changes
None / Describe any breaking changes
```

4. **Review Process:**
   - A maintainer will review your PR
   - Address any feedback
   - Once approved, your PR will be merged

## Working with Beads

Beads is our AI-native issue tracker that lives in the repository.

### Core Commands

```bash
# Create a new issue
bd create "Add feature X"

# List all issues
bd list

# List ready-to-work issues
bd ready

# Show issue details
bd show <issue-id>

# Update status
bd update <issue-id> --status in_progress
bd update <issue-id> --status done

# Close issue with reason
bd close <issue-id> --reason "Implemented with tests"

# Sync with remote (usually automatic via git hooks)
bd sync
```

### AI-Friendly Commands

If you have `bv` installed:

```bash
# Get single top priority pick
bv --robot-next

# Get comprehensive analysis
bv --robot-triage

# Get parallel execution plan
bv --robot-plan

# Get full metrics
bv --robot-insights
```

**Warning:** Never run bare `bv` - it launches an interactive TUI that will block your session.

### Beads Workflow

1. Find work: `bd ready` or `bv --robot-next`
2. Claim task: `bd update <id> --status in_progress`
3. Implement changes
4. Commit code (beads auto-sync via git hooks)
5. Complete: `bd close <id> --reason "Done"`
6. Push code: `git push` (beads already synced by daemon)

The `.beads/issues.jsonl` file is automatically synced by git hooks - you don't need to manually manage it.

## Project Structure

Understanding the codebase organization:

```
intent-cli/
├── src/intent/
│   ├── interview.gleam              # Interview engine core
│   ├── interview_questions.gleam    # Question library
│   ├── interview_storage.gleam      # Session persistence
│   ├── answer_loader.gleam          # Answer processing
│   ├── question_loader.gleam        # Question loading
│   │
│   ├── kirk/                        # KIRK analysis subsystem
│   │   ├── quality_analyzer.gleam   # Quality scoring
│   │   ├── inversion_checker.gleam  # Risk analysis
│   │   ├── coverage_analyzer.gleam  # Test coverage
│   │   ├── gap_detector.gleam       # Gap detection
│   │   ├── ears_parser.gleam        # EARS parsing
│   │   └── effects_analyzer.gleam   # Side effect analysis
│   │
│   ├── checker.gleam                # API testing core
│   ├── runner.gleam                 # Test execution
│   ├── http_client.gleam            # HTTP client
│   ├── rules_engine.gleam           # Validation rules
│   │
│   ├── types.gleam                  # Core type definitions
│   ├── parser.gleam                 # CUE parsing
│   ├── loader.gleam                 # Spec loading
│   ├── resolver.gleam               # Variable resolution
│   ├── validator.gleam              # Spec validation
│   ├── interpolate.gleam            # String interpolation
│   ├── output.gleam                 # Output formatting
│   │
│   ├── bead_templates.gleam         # Bead generation
│   ├── spec_builder.gleam           # Spec construction
│   │
│   └── cli_ui.gleam                 # CLI interface
│
├── test/                            # Test suite (mirrors src/)
├── examples/                        # Example specifications
├── docs/                            # Documentation
├── schema/                          # CUE schema definitions
└── .beads/                          # Beads issue tracking
```

### Key Subsystems

- **Core Entry:** `src/intent.gleam` - CLI entry point
- **Interview:** Session-based requirement gathering
- **KIRK Analysis:** Quality, coverage, gap, and risk analysis
- **API Testing:** HTTP test execution and validation
- **Core Infrastructure:** Parsing, loading, validation
- **Bead System:** Work item generation and management

## Common Tasks

### Adding a New Command

1. Define the command in `src/intent.gleam`
2. Create a handler function
3. Add tests in `test/intent_test.gleam`
4. Update documentation

### Adding a New Analysis Type

1. Create module in `src/intent/kirk/`
2. Define input/output types
3. Implement analysis logic with `Result` return types
4. Add comprehensive tests
5. Update KIRK documentation

### Fixing a Bug

1. Find or create a bead: `bd create "Fix: description"`
2. Claim it: `bd update <id> --status in_progress`
3. Write a failing test that reproduces the bug
4. Fix the bug
5. Verify the test passes
6. Close the bead: `bd close <id> --reason "Fixed with test"`

### Adding Documentation

1. Module docs: `////` at top of file
2. Function docs: `///` before `pub fn`
3. Update relevant `.md` files in `docs/`
4. Add examples where helpful

## Getting Help

- **Documentation:** See `docs/` directory
- **Examples:** Check `examples/` directory
- **Issues:** Use `bd list` to see existing issues
- **Discussions:** Open a GitHub discussion
- **Discord/Slack:** [Link if available]

## Code of Conduct

Be respectful, constructive, and collaborative. We're all here to build something useful.

## License

By contributing, you agree that your contributions will be licensed under the Apache 2.0 License.

---

**Thank you for contributing to Intent CLI!** Your work helps make requirements engineering and API testing more deterministic and AI-friendly.
