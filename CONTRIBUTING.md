# Contributing to Intent CLI

We welcome contributions from the community! This document provides guidelines for contributing to Intent CLI.

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Setup](#development-setup)
4. [Making Changes](#making-changes)
5. [Testing](#testing)
6. [Submitting Changes](#submitting-changes)
7. [Coding Standards](#coding-standards)
8. [Documentation](#documentation)

## Code of Conduct

Intent CLI is committed to providing a welcoming and inclusive environment for all contributors. Please:

- Be respectful and professional in all interactions
- Welcome people of all backgrounds and experience levels
- Provide constructive feedback
- Report harassment or unacceptable behavior to the maintainers

## Getting Started

### Fork and Clone

```bash
# Fork the repository on GitHub, then clone your fork
git clone https://github.com/yourusername/intent-cli
cd intent-cli

# Add upstream remote for syncing
git remote add upstream https://github.com/original/intent-cli
```

### Create a Branch

```bash
# Update to latest main
git fetch upstream
git checkout main
git reset --hard upstream/main

# Create a feature branch
git checkout -b feature/your-feature-name
```

Branch naming conventions:
- `feature/description` - New features
- `fix/description` - Bug fixes
- `docs/description` - Documentation
- `test/description` - Tests
- `refactor/description` - Code refactoring

## Development Setup

### Prerequisites

- Gleam 1.0 or later
- Erlang/OTP 27
- Git
- A code editor (VS Code, Vim, Emacs, etc.)

### Installation

```bash
# Clone and setup
git clone https://github.com/yourusername/intent-cli
cd intent-cli

# Install dependencies
gleam build

# Run tests to verify setup
gleam test

# You should see: "11 tests, 0 failures"
```

### Project Structure

```
intent-cli/
├── src/intent/              # Main source code
│   ├── anti_patterns.gleam   # Anti-pattern detection
│   ├── checker.gleam         # Response validation
│   ├── http_client.gleam     # HTTP execution
│   ├── parser.gleam          # CUE parsing
│   ├── runner.gleam          # Test execution engine
│   ├── types.gleam           # Type definitions
│   └── ...                   # Other modules
├── test/                    # Test files
├── docs/                    # Documentation
├── examples/                # Example specifications
├── gleam.toml              # Project manifest
└── README.md               # Project README
```

## Making Changes

### Code Organization

- Keep functions small and focused (typically < 20 lines)
- Use descriptive names for functions and variables
- Prefer pipelines (`|>`) for data transformation
- Use pattern matching for control flow
- Keep modules organized by functionality

### Example: Adding a New Feature

If you're adding a new validation rule:

1. Add the rule logic to `src/intent/checker.gleam`
2. Add tests to `test/intent_test.gleam`
3. Update documentation in `docs/SPEC_FORMAT.md`
4. Run tests and ensure no warnings
5. Commit with a clear message

### Best Practices

**Use Result types for error handling:**

```gleam
pub fn validate_email(email: String) -> Result(String, String) {
  case string.contains(email, "@") {
    True -> Ok(email)
    False -> Error("Invalid email address")
  }
}
```

**Pattern match exhaustively:**

```gleam
case method {
  Get -> "GET"
  Post -> "POST"
  Put -> "PUT"
  // Cover all cases
}
```

**Use descriptive variable names:**

```gleam
// Good
let validated_responses = list.filter(responses, validate_response)

// Bad
let r = list.filter(resp, fn(x) { check(x) })
```

**Avoid unnecessary comments:**

```gleam
// Good: The code is clear
let filtered = list.filter(items, fn(item) { item.status == "active" })

// Bad: Comment adds no value
// Filter active items
let filtered = list.filter(items, fn(item) { item.status == "active" })
```

## Testing

### Running Tests

```bash
# Run all tests
gleam test

# Watch for changes and re-run
gleam test --watch

# Run with verbose output
gleam test -- --verbose
```

### Writing Tests

Tests should be in `test/` directory with `_test` suffix:

```gleam
import gleeunit
import gleeunit/should
import intent/parser

pub fn main() {
  gleeunit.main()
}

pub fn parser_handles_valid_input_test() {
  let input = "{ \"name\": \"Test\" }"
  let result = parser.parse(input)

  case result {
    Ok(data) -> data
      |> should.equal("Test")
    Error(_) -> should.fail()
  }
}
```

### Test Guidelines

1. **Test both happy path and error cases**
   ```gleam
   pub fn validator_accepts_valid_uuid_test() { /* ... */ }
   pub fn validator_rejects_invalid_uuid_test() { /* ... */ }
   ```

2. **Use descriptive test names**
   - Good: `parser_handles_nested_objects_test`
   - Bad: `test1_test`

3. **Test edge cases**
   - Empty inputs
   - Very large inputs
   - Special characters
   - Null/nil values

4. **Ensure tests are isolated**
   - Don't depend on external services
   - Don't modify global state
   - Clean up resources

## Submitting Changes

### Commit Messages

Write clear, concise commit messages following this format:

```
[Type] Brief summary (50 chars or less)

More detailed explanation if needed. Wrap at 72 characters.
Explain what the change does and why.

Fixes #123
See also: #456
```

Types:
- `feat:` - New feature
- `fix:` - Bug fix
- `docs:` - Documentation
- `test:` - Tests
- `refactor:` - Code refactoring
- `perf:` - Performance improvement

Example:

```
feat: Add regex pattern caching with ETS

Implement caching of compiled regex patterns using Erlang ETS
to improve rule validation performance. Patterns are memoized
on first use and reused for subsequent validations.

Fixes #42
```

### Creating a Pull Request

1. **Push your branch:**
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Create a PR on GitHub** with:
   - Clear title and description
   - Reference to related issues
   - Summary of changes
   - Testing instructions

3. **PR Template:**
   ```markdown
   ## Description
   Brief explanation of what this PR does.

   ## Related Issues
   Fixes #123

   ## Changes
   - Change 1
   - Change 2
   - Change 3

   ## Testing
   - How to test these changes
   - Expected behavior

   ## Checklist
   - [ ] Tests pass
   - [ ] No compiler warnings
   - [ ] Documentation updated
   - [ ] Code follows style guidelines
   ```

### Review Process

- A maintainer will review your PR
- Address any feedback or suggestions
- Re-request review once changes are made
- PR will be merged once approved

## Coding Standards

### Gleam Style Guide

Follow the [Gleam Style Guide](https://gleam.run/guide/frequently-asked-questions/#how-should-i-format-my-code):

- Use 2-space indentation
- Maximum line length of 100 characters
- Use snake_case for functions and variables
- Use PascalCase for types and modules
- Add type annotations to top-level functions

### Module Organization

```gleam
// Imports at the top
import gleam/list
import gleam/option.{Option}

// Type definitions
pub type MyType {
  Variant1
  Variant2
}

// Public functions
pub fn public_function(arg: String) -> String {
  // Implementation
}

// Internal functions
fn internal_function(arg: String) -> String {
  // Implementation
}
```

### Comments

- Use `///` for documentation comments on public items
- Use `//` for implementation comments
- Keep comments concise and useful

```gleam
/// Validates an email address format.
/// Returns Ok if valid, Error with message if not.
pub fn validate_email(email: String) -> Result(String, String) {
  // Check for @ symbol
  case string.contains(email, "@") {
    True -> Ok(email)
    False -> Error("Email must contain @")
  }
}
```

## Documentation

### Updating Documentation

When making changes:

1. **Update relevant doc files:**
   - `docs/USER_GUIDE.md` - User-facing changes
   - `docs/SPEC_FORMAT.md` - Specification format changes
   - `docs/INSTALLATION.md` - Installation/setup changes
   - `README.md` - Major feature changes

2. **Add comments to code:**
   - Document public functions with `///`
   - Explain complex logic with `//` comments
   - Add examples where helpful

3. **Update examples:**
   - If you add a new feature, add an example in `examples/`
   - Update existing examples if behavior changes

### Documentation Style

- Use clear, simple language
- Provide concrete examples
- Include both happy path and error cases
- Link to related documentation
- Keep documentation in sync with code

## Reporting Issues

When reporting issues:

1. **Check if the issue exists** - Search closed and open issues
2. **Provide clear reproduction steps**
3. **Include environment information:**
   ```
   - Gleam version: 1.0.0
   - Erlang/OTP version: 27
   - Operating system: macOS/Linux/Windows
   - Intent CLI version: (if applicable)
   ```
4. **Include expected vs actual behavior**
5. **Add relevant logs or error messages**

## Performance Considerations

When optimizing code:

1. **Measure before optimizing** - Use actual numbers
2. **Avoid premature optimization** - Keep code clear first
3. **Document performance improvements** - Explain the benefit
4. **Consider memory usage** - Erlang memory is precious
5. **Use ETS for caching** - When beneficial (like regex patterns)

## Getting Help

- **Issues:** Open a GitHub issue for bugs or features
- **Discussions:** Ask questions in GitHub Discussions
- **Documentation:** Read the docs and examples
- **Maintainers:** Tag maintainers if you need help

## Thank You!

Thank you for contributing to Intent CLI! Your efforts help make the project better for everyone.

---

Happy coding! 🎉
