# Testing Guide

Comprehensive testing guide for the Intent CLI project. This project has **1200+ tests** providing extensive coverage of all functionality.

## Quick Start

```bash
# Run all tests (recommended)
gleam test

# Run tests with specific target
gleam test --target erlang

# Build before testing
gleam build && gleam test
```

## Test Organization

Tests mirror the source structure:

```
src/
├── intent.gleam              → test/intent_test.gleam
├── intent/
│   ├── parser.gleam          → test/parser_test.gleam
│   ├── validator.gleam       → test/validator_test.gleam
│   ├── anti_patterns.gleam   → test/anti_patterns_test.gleam
│   └── intent/
│       ├── parser.gleam      → test/intent/parser_test.gleam
│       ├── security.gleam    → test/intent/security_test.gleam
│       └── ...               → test/intent/...
```

**Rule**: Each `.gleam` source file has a corresponding `_test.gleam` file in the same relative path under `test/`.

## Test Structure

### Test File Template

```gleam
//// Comprehensive tests for intent/module_name.gleam
//// Tests cover all functionality and edge cases

import gleeunit
import gleeunit/should
import intent/module_name

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Section Name Tests
// ============================================================================

/// Test description
pub fn function_name_basic_test() {
  // Arrange
  let input = "test data"

  // Act
  let result = module_name.function_name(input)

  // Assert
  result
  |> should.equal(expected_value)
}

/// Test edge case
pub fn function_name_edge_case_test() {
  // Test implementation
}
```

### Naming Conventions

All test functions must follow this pattern:

- **Function name**: `pub fn descriptive_name_test()`
- **Suffix**: Always end with `_test`
- **Visibility**: Always `pub`
- **Parameters**: No parameters
- **Return type**: Implicit `Nil`

Examples:
```gleam
pub fn parser_parses_valid_json_test() { ... }
pub fn validator_rejects_empty_string_test() { ... }
pub fn resolver_handles_circular_deps_test() { ... }
```

### Test Sections

Organize tests into logical sections using comments:

```gleam
// ============================================================================
// Happy Path Tests
// ============================================================================

pub fn basic_functionality_test() { ... }

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

pub fn empty_input_test() { ... }
pub fn null_input_test() { ... }

// ============================================================================
// Integration Tests
// ============================================================================

pub fn end_to_end_workflow_test() { ... }
```

## Using gleeunit Assertions

Intent uses [gleeunit](https://hexdocs.pm/gleeunit/) for testing:

```gleam
import gleeunit/should

// Equality
result |> should.equal(expected)
result |> should.not_equal(unexpected)

// Boolean
condition |> should.be_true()
condition |> should.be_false()

// Result types
result |> should.be_ok()
result |> should.be_error()

// Explicit failure
should.fail()  // Useful in pattern matching
```

### Pattern Matching with Assertions

```gleam
pub fn result_returns_ok_test() {
  let result = some_function()

  case result {
    Ok(value) -> {
      value |> should.equal("expected")
    }
    Error(_) -> should.fail()
  }
}

pub fn list_contains_item_test() {
  let items = get_items()

  case list.first(items) {
    Ok(first) -> {
      first.name |> should.equal("expected")
    }
    Error(_) -> should.fail()
  }
}
```

## Test Helpers and Factories

Use `/home/lewis/src/intent-cli/test/test_helpers.gleam` for reusable test data:

```gleam
import test_helpers

// Create test behaviors
let behavior = test_helpers.make_test_behavior("login", [])
let behavior_with_deps = test_helpers.make_test_behavior("logout", ["login"])

// Create test features
let feature = test_helpers.make_test_feature("Auth", [behavior1, behavior2])

// Create test specs
let spec = test_helpers.make_test_spec([feature1, feature2])
let spec_from_behaviors = test_helpers.make_test_spec_from_behaviors([b1, b2])

// Create test configs
let config = test_helpers.make_test_config()
let custom_config = test_helpers.make_test_config_with_url("http://api.example.com")

// Create test requests
let request = test_helpers.make_test_request("/users")
```

### Custom Factories in Test Files

For module-specific test data, create local factories:

```gleam
// ============================================================================
// Test Factories
// ============================================================================

/// Create minimal valid ExecutionResult
fn make_execution_result(status: Int, body: Json) -> ExecutionResult {
  ExecutionResult(
    status: status,
    headers: dict.new(),
    body: body,
    raw_body: json.to_string(body),
    elapsed_ms: 100,
    request_method: Get,
    request_path: "/test",
  )
}

/// Create an anti-pattern with bad and good examples
fn make_anti_pattern(name: String, description: String) -> AntiPattern {
  AntiPattern(
    name: name,
    description: description,
    bad_example: json.object([#("password", json.string("secret"))]),
    good_example: json.object([#("token", json.string("abc123"))]),
    why: "Security best practice",
  )
}
```

## Writing Effective Tests

### The 7 Gleam Commandments Apply to Tests

1. **Explicitness**: Make test intent crystal clear
2. **Immutability**: Don't mutate test data
3. **Type-First**: Use custom types in test helpers
4. **Exhaustive Matching**: Cover all cases
5. **Pipeline Flow**: Use `|>` for assertions
6. **Railway-Oriented**: Test both Ok and Error paths
7. **Strict Naming**: Follow `function_name_scenario_test` pattern

### Test Coverage Guidelines

For each function, test:

1. **Happy path**: Valid input produces expected output
2. **Edge cases**: Empty strings, zero, negative numbers, very large values
3. **Error cases**: Invalid input returns proper errors
4. **Boundary conditions**: First item, last item, null/None
5. **Integration**: Works with other modules

Example:
```gleam
// Happy path
pub fn parse_valid_json_test() { ... }

// Edge cases
pub fn parse_empty_string_test() { ... }
pub fn parse_null_test() { ... }
pub fn parse_very_large_json_test() { ... }

// Error cases
pub fn parse_invalid_json_returns_error_test() { ... }
pub fn parse_malformed_json_returns_error_test() { ... }

// Boundary conditions
pub fn parse_empty_object_test() { ... }
pub fn parse_empty_array_test() { ... }

// Integration
pub fn parse_then_validate_test() { ... }
```

### Keep Tests Fast

Intent has **1200+ tests** - they must run quickly:

```gleam
// Good: Fast, focused
pub fn validate_email_format_test() {
  validate_email("user@example.com")
  |> should.be_ok()
}

// Bad: Slow, does too much
pub fn full_registration_flow_test() {
  // Avoid: HTTP calls, file I/O, sleeps in unit tests
  // Save for integration tests
}
```

**Guidelines**:
- Unit tests: < 1ms each
- Module tests: < 100ms total
- Full suite: < 5 seconds
- No external dependencies (HTTP, database, file system) in unit tests
- Use factories instead of fixtures

## Debugging Tests

### Run Specific Test Files

```bash
# Gleam runs all tests, but you can filter by rebuilding
gleam build
gleam test

# For debugging, use IO in tests temporarily
import gleam/io

pub fn debug_test() {
  let value = compute_something()
  io.debug(value)  // Print for debugging
  value |> should.equal(expected)
}
```

### Understanding Test Failures

```
Test output format:
  ✓ parser_valid_json_test
  ✗ parser_invalid_json_test

Error details show:
  - File and line number
  - Expected vs actual values
  - Stack trace for panics
```

### Common Test Failures

1. **Assertion mismatch**:
   ```
   Error: Assertion failed
   Expected: "hello"
   Got: "goodbye"
   ```
   Fix: Check your logic or test expectation

2. **Pattern match failure**:
   ```
   Error: No case clause matched
   ```
   Fix: Add missing case or use exhaustive matching

3. **Type errors**:
   ```
   Error: Type mismatch
   ```
   Fix: Check factory functions return correct types

## Test-Driven Development (TDD)

Intent follows TDD practices:

### Red-Green-Refactor Cycle

```bash
# 1. RED: Write failing test
gleam test  # Fails

# 2. GREEN: Write minimal code to pass
gleam test  # Passes

# 3. REFACTOR: Improve code quality
gleam test  # Still passes
```

### TDD Example Workflow

```gleam
// Step 1: Write the test (RED)
pub fn validate_email_rejects_invalid_test() {
  validate_email("not-an-email")
  |> should.be_error()
}

// Step 2: Implement minimal solution (GREEN)
pub fn validate_email(email: String) -> Result(String, String) {
  case string.contains(email, "@") {
    True -> Ok(email)
    False -> Error("Invalid email")
  }
}

// Step 3: Refactor for robustness
pub fn validate_email(email: String) -> Result(String, ValidationError) {
  // More sophisticated validation
  // ...
}

// Step 4: Add more tests
pub fn validate_email_rejects_multiple_at_signs_test() { ... }
```

## Integration Testing

For testing complete workflows:

```gleam
// ============================================================================
// Integration Tests
// ============================================================================

pub fn full_spec_parsing_and_validation_test() {
  // Load spec
  let assert Ok(content) = simplifile.read("examples/user-api.cue")

  // Parse
  let assert Ok(spec) = parser.parse_spec(content)

  // Validate
  let assert Ok(valid_spec) = validator.validate_spec(spec)

  // Verify end-to-end
  valid_spec.name
  |> should.equal("User Management API")
}
```

### Testing with Files

```gleam
import simplifile

pub fn loader_reads_spec_file_test() {
  // Use example files in tests
  let result = loader.load_spec("examples/user-api.cue")

  result
  |> should.be_ok()
}
```

## CI Integration

### Pre-Commit Checks

Before committing, always run:

```bash
# Format code
gleam format

# Build
gleam build

# Run all tests
gleam test

# Or all at once (recommended for commits)
gleam format && gleam build && gleam test
```

### CI Pipeline

The CI pipeline runs:

1. **Format check**: `gleam format --check`
2. **Build**: `gleam build --target erlang`
3. **Test**: `gleam test`

All must pass before merging.

## Test Metrics

Current test metrics (as of latest):

- **Total test files**: 32
- **Total test functions**: 1200+
- **Average tests per file**: ~40
- **Test coverage**: Extensive (all public APIs)
- **Test execution time**: < 5 seconds

## Best Practices Summary

1. **Mirror source structure**: `src/intent/parser.gleam` → `test/parser_test.gleam`
2. **Name tests clearly**: `function_name_scenario_test()`
3. **Use test helpers**: Leverage `test_helpers.gleam` factories
4. **Keep tests fast**: No I/O, network, or sleeps
5. **Test all paths**: Happy path, edge cases, errors
6. **Section your tests**: Use comment dividers
7. **Document intent**: Use `///` for test descriptions
8. **Run before commit**: `gleam format && gleam build && gleam test`

## Common Test Patterns

### Testing Result Types

```gleam
pub fn function_returns_ok_test() {
  let result = some_function()
  result |> should.be_ok()
}

pub fn function_returns_error_test() {
  let result = some_function_with_bad_input()
  result |> should.be_error()
}

pub fn function_returns_correct_value_test() {
  let assert Ok(value) = some_function()
  value |> should.equal("expected")
}
```

### Testing Option Types

```gleam
pub fn function_returns_some_test() {
  let result = find_item("id-123")

  case result {
    Some(item) -> item.name |> should.equal("Test")
    None -> should.fail()
  }
}

pub fn function_returns_none_test() {
  let result = find_item("nonexistent")
  result |> should.equal(None)
}
```

### Testing Lists

```gleam
pub fn list_has_correct_length_test() {
  let items = get_items()
  items
  |> list.length
  |> should.equal(3)
}

pub fn list_contains_item_test() {
  let items = get_items()
  items
  |> list.any(fn(item) { item.name == "target" })
  |> should.be_true()
}

pub fn list_is_sorted_test() {
  let items = get_sorted_items()
  let assert [first, second, third] = items

  first.id |> should.equal(1)
  second.id |> should.equal(2)
  third.id |> should.equal(3)
}
```

### Testing Dict Operations

```gleam
import gleam/dict

pub fn dict_contains_key_test() {
  let map = dict.from_list([#("key1", "value1")])

  map
  |> dict.has_key("key1")
  |> should.be_true()
}

pub fn dict_get_returns_value_test() {
  let map = dict.from_list([#("key1", "value1")])

  map
  |> dict.get("key1")
  |> should.equal(Ok("value1"))
}
```

## Resources

- [Gleam Testing Documentation](https://gleam.run/writing-gleam/testing/)
- [gleeunit Hex Docs](https://hexdocs.pm/gleeunit/)
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/)
- Project examples: See `test/` directory for real-world patterns

## Getting Help

- Review existing tests: `test/` directory has 1200+ examples
- Check test helpers: `test/test_helpers.gleam`
- Read source code: Tests mirror `src/` structure
- Ask questions: Open an issue with the `testing` label
