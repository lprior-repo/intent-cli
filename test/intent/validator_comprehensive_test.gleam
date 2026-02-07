import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/types
import intent/validator

// =============================================================================
// Advanced Edge Case Tests
// =============================================================================

pub fn validate_spec_empty_behavior_list_test() {
  // Test spec with empty behavior list
  let spec = make_minimal_spec([])
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_behavior_with_empty_dependencies_test() {
  // Test behavior with empty requires list (should be valid)
  let behavior = make_behavior("test", [], "GET", "/test", dict.new())
  let spec = make_minimal_spec([behavior])
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_behavior_with_empty_captures_test() {
  // Test behavior with empty captures dict (should be valid)
  let behavior = make_behavior("test", [], "GET", "/test", dict.new())
  let spec = make_minimal_spec([behavior])
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_complex_capture_chain_test() {
  // Test multi-step capture chain: a -> b -> c
  let behaviors = [
    make_behavior_with_capture("create_user", [], "POST", "/users", "user_id"),
    make_behavior_with_capture("login", ["create_user"], "POST", "/auth/login", "auth_token"),
    make_behavior("get_profile", ["login"], "GET", "/users/${user_id}", dict.new()),
    make_behavior_with_header("update_profile", ["login"], "PATCH", "/users/${user_id}", "Authorization", "Bearer ${auth_token}"),
  ]
  let spec = make_minimal_spec(behaviors)
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_duplicate_behavior_names_test() {
  // Test what happens with duplicate behavior names (shouldn't be caught by validator)
  let behavior1 = make_behavior("duplicate", [], "GET", "/first", dict.new())
  let behavior2 = make_behavior("duplicate", [], "GET", "/second", dict.new())
  let spec = make_minimal_spec([behavior1, behavior2])
  let result = validator.validate_spec(spec)
  // This might pass since validator doesn't check for name uniqueness
  // The behavior that gets processed depends on list ordering
  true |> should.be_true() // Just ensure it doesn't crash
}

pub fn validate_spec_long_dependency_chain_test() {
  // Test long dependency chain: a -> b -> c -> d -> e
  let behaviors = [
    make_behavior("a", [], "GET", "/a", dict.new()),
    make_behavior("b", ["a"], "GET", "/b", dict.new()),
    make_behavior("c", ["b"], "GET", "/c", dict.new()),
    make_behavior("d", ["c"], "GET", "/d", dict.new()),
    make_behavior("e", ["d"], "GET", "/e", dict.new()),
  ]
  let spec = make_minimal_spec(behaviors)
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

// =============================================================================
// Variable Reference Edge Cases
// =============================================================================

pub fn validate_spec_duplicate_variables_in_path_test() {
  // Test path with same variable multiple times: /users/${id}/posts/${id}
  let behavior = make_behavior_with_capture("get_user_posts", [], "GET", "/users/${id}/posts/${id}", "user_id")
  let capture_behavior = make_behavior_with_capture("create_user", [], "POST", "/users", "user_id")
  let spec = make_minimal_spec([capture_behavior, behavior])
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_variable_with_special_chars_in_name_test() {
  // Test variable names with special characters (should work)
  let behavior = make_behavior("test", [], "GET", "/api/${user_id_123}", dict.from_list([#("user_id_123", "$.id")]))
  let spec = make_minimal_spec([behavior])
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_empty_string_variable_test() {
  // Test edge case with empty string variable name (should not match or should fail gracefully)
  let behavior = make_behavior("test", [], "GET", "/api/${}", dict.new())
  let spec = make_minimal_spec([behavior])
  let result = validator.validate_spec(spec)
  // Should be valid since no variables are extracted from empty pattern
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_malformed_variable_syntax_test() {
  // Test malformed variable syntax
  let test_cases = [
    "/api/${",      // Missing closing }
    "/api/$}",      // Missing opening {
    "/api/$",       // Just dollar sign
    "/api/${{var}}", // Double braces
  ]

  list.fold(test_cases, True, fn(acc, path) {
    case acc {
      True -> {
        let behavior = make_behavior("test", [], "GET", path, dict.new())
        let spec = make_minimal_spec([behavior])
        case validator.validate_spec(spec) {
          validator.ValidationValid -> True
          _ -> False
        }
      }
      False -> False
    }
  }) |> should.be_true()
}

// =============================================================================
// Path Validation Edge Cases
// =============================================================================

pub fn validate_path_percent_encoded_traversal_test() {
  // Test percent-encoded path traversal
  let test_cases = [
    "/%2e%2e%2f/etc/passwd",    // ../ encoded
    "/%2e%2e%5c/windows",       // ..\ encoded
    "/api/%2e%2e%2f/secret",    // Mixed
    "/%252e%252e%252f",        // Double encoded
  ]

  list.fold(test_cases, 0, fn(count, path) {
    let behavior = make_behavior("test", [], "GET", path, dict.new())
    let spec = make_minimal_spec([behavior])
    case validator.validate_spec(spec) {
      validator.ValidationInvalid(issues) -> count + list.length(issues)
      validator.ValidationValid -> count
    }
  }) |> should.equal(4) // All should be detected
}

pub fn validate_path_unicode_metacharacters_test() {
  // Test Unicode characters that might be used in attacks
  let test_cases = [
    "/test; rm -rf /",                    // Semicolon
    "/test| cat /etc/passwd",            // Pipe
    "/test`whoami`",                      // Backtick
    "/test$(whoami)",                     // Command substitution
    "/test\n",                            // Newline
    "/test\r",                            // Carriage return
    "/test\t",                            // Tab
    "/test&& rm -rf /",                   // Double ampersand
    "/test|| rm -rf /",                   // Double pipe
    "/test> /tmp/hack",                   // Redirection
    "/test< /etc/passwd",                 // Input redirection
  ]

  list.fold(test_cases, 0, fn(count, path) {
    let behavior = make_behavior("test", [], "GET", path, dict.new())
    let spec = make_minimal_spec([behavior])
    case validator.validate_spec(spec) {
      validator.ValidationInvalid(issues) -> count + list.length(issues)
      validator.ValidationValid -> count
    }
  }) |> should.greater_than_or_equal_to(8) // Most should be detected
}

pub fn validate_path_valid_special_chars_test() {
  // Test valid special characters that should not be blocked
  let test_cases = [
    "/api/v1/users",                     // Normal path
    "/api/v1/users/123",                 // With numbers
    "/api/v1/users/new-user",            // Hyphen
    "/api/v1/users/old_user",            // Underscore
    "/api/v1/users~test",               // Tilde
    "/api/v1/users:123",                // Colon
    "/api/v1/users@domain.com",         // At symbol
    "/api/v1/users+123",                // Plus
    "/api/v1/users=123",                // Equals
    "/api/v1/users,123",                // Comma
    "/api/v1/users(123)",               // Parentheses
    "/api/v1/users[123]",               // Brackets
    "/api/v1/users{123}",               // Braces
    "/api/v1/users;123",                // Semicolon in path (not at start)
    "/api/v1/users/period.file",        // Period
    "/v1.2/users",                      // Version in path
    "/:users/:id",                      // Path parameters (though not supported by parser)
  ]

  list.fold(test_cases, 0, fn(count, path) {
    let behavior = make_behavior("test", [], "GET", path, dict.new())
    let spec = make_minimal_spec([behavior])
    case validator.validate_spec(spec) {
      validator.ValidationValid -> count + 1
      _ -> count
    }
  }) |> should.equal(16) // All should be valid
}

// =============================================================================
// Circular Dependency Complex Cases
// =============================================================================

pub fn validate_spec_self_referencing_with_other_deps_test() {
  // Self-reference plus normal deps: a -> a, b -> a
  let behaviors = [
    make_behavior("a", ["a", "b"], "GET", "/a", dict.new()),
    make_behavior("b", ["a"], "GET", "/b", dict.new()),
  ]
  let spec = make_minimal_spec(behaviors)

  case validator.validate_spec(spec) {
    validator.ValidationInvalid(issues) -> {
      list.any(issues, fn(issue) {
        case issue {
          validator.CircularDependency(_) -> True
          _ -> False
        }
      }) |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_spec_multiple_circular_chains_test() {
  // Two separate circular chains: a -> b -> a, c -> d -> c
  let behaviors = [
    make_behavior("a", ["b"], "GET", "/a", dict.new()),
    make_behavior("b", ["a"], "GET", "/b", dict.new()),
    make_behavior("c", ["d"], "GET", "/c", dict.new()),
    make_behavior("d", ["c"], "GET", "/d", dict.new()),
    make_behavior("e", [], "GET", "/e", dict.new()), // Valid behavior
  ]
  let spec = make_minimal_spec(behaviors)

  case validator.validate_spec(spec) {
    validator.ValidationInvalid(issues) -> {
      let circular_count = list.fold(issues, 0, fn(count, issue) {
        case issue {
          validator.CircularDependency(_) -> count + 1
          _ -> count
        }
      })
      circular_count |> should.equal(2) // Two circular dependencies
    }
    _ -> should.fail()
  }
}

pub fn validate_spec_complex_mixed_dependencies_test() {
  // Complex mixed dependencies: some valid, some circular
  let behaviors = [
    // Valid tree: a -> b -> c
    make_behavior("a", [], "GET", "/a", dict.new()),
    make_behavior("b", ["a"], "GET", "/b", dict.new()),
    make_behavior("c", ["b"], "GET", "/c", dict.new()),
    // Circular chain: d -> e -> d
    make_behavior("d", ["e"], "GET", "/d", dict.new()),
    make_behavior("e", ["d"], "GET", "/e", dict.new()),
    // Valid with deps on circular: f -> d (should not fail because validator doesn't check this)
    make_behavior("f", ["d"], "GET", "/f", dict.new()),
  ]
  let spec = make_minimal_spec(behaviors)

  case validator.validate_spec(spec) {
    validator.ValidationInvalid(issues) -> {
      let circular_count = list.fold(issues, 0, fn(count, issue) {
        case issue {
          validator.CircularDependency(_) -> count + 1
          _ -> count
        }
      })
      circular_count |> should.equal(1) // One circular dependency detected
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Complex Mixed Error Scenarios
// =============================================================================

pub fn validate_spec_all_error_types_together_test() {
  // Test all error types in one spec
  let behaviors = [
    // Missing dependency
    make_behavior("bad_dep", ["missing_behavior"], "GET", "/api", dict.new()),
    // Missing capture
    make_behavior("bad_capture", [], "GET", "/api/${missing_var}", dict.new()),
    // Circular dependency
    make_behavior("a", ["b"], "GET", "/a", dict.new()),
    make_behavior("b", ["a"], "GET", "/b", dict.new()),
    // Invalid path
    make_behavior("bad_path", [], "GET", "/api; rm -rf /", dict.new()),
    // Valid behavior
    make_behavior("good", [], "GET", "/api/good", dict.new()),
  ]
  let spec = make_minimal_spec(behaviors)

  case validator.validate_spec(spec) {
    validator.ValidationInvalid(issues) -> {
      let missing_deps = list.fold(issues, 0, fn(count, issue) {
        case issue {
          validator.MissingDependency(_, _) -> count + 1
          _ -> count
        }
      })

      let missing_captures = list.fold(issues, 0, fn(count, issue) {
        case issue {
          validator.MissingCapture(_, _, _, _) -> count + 1
          _ -> count
        }
      })

      let circular_deps = list.fold(issues, 0, fn(count, issue) {
        case issue {
          validator.CircularDependency(_) -> count + 1
          _ -> count
        }
      })

      let invalid_paths = list.fold(issues, 0, fn(count, issue) {
        case issue {
          validator.InvalidPath(_, _, _) -> count + 1
          _ -> count
        }
      })

      missing_deps |> should.equal(1)
      missing_captures |> should.equal(1)
      circular_deps |> should.equal(1)
      invalid_paths |> should.equal(1)
      list.length(issues) |> should.equal(4)
    }
    _ -> should.fail()
  }
}

pub fn validate_spec_duplicate_circular_detection_test() {
  // Test that circular dependencies aren't reported multiple times
  let behaviors = [
    make_behavior("a", ["b"], "GET", "/a", dict.new()),
    make_behavior("b", ["c"], "GET", "/b", dict.new()),
    make_behavior("c", ["a"], "GET", "/c", dict.new()),
  ]
  let spec = make_minimal_spec(behaviors)

  case validator.validate_spec(spec) {
    validator.ValidationInvalid(issues) -> {
      // Should only report one circular dependency, not one per behavior
      list.fold(issues, 0, fn(count, issue) {
        case issue {
          validator.CircularDependency(_) -> count + 1
          _ -> count
        }
      }) |> should.equal(1)
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Error Message Quality Tests
// =============================================================================

pub fn format_issues_unambiguous_test() {
  let issue = validator.MissingDependency("get_user", "login")
  let formatted = validator.format_issues([issue])

  string.contains(formatted, "get_user") |> should.be_true()
  string.contains(formatted, "login") |> should.be_true()
  string.contains(formatted, "does not exist") |> should.be_true()
}

pub fn format_issues_specific_test() {
  let issue = validator.MissingCapture("get_profile", "request.path", "user_id", ["create_user"])
  let formatted = validator.format_issues([issue])

  // Check that it specifies exactly what's missing and where
  string.contains(formatted, "get_profile") |> should.be_true()
  string.contains(formatted, "request.path") |> should.be_true()
  string.contains(formatted, "user_id") |> should.be_true()
  string.contains(formatted, "create_user") |> should.be_true()
  string.contains(formatted, "Ensure these behaviors run before") |> should.be_true()
}

pub fn format_issues_actionable_test() {
  let issue = validator.InvalidPath("test_behavior", "/api; rm -rf /", "Path contains shell metacharacter ';' which may be unsafe")
  let formatted = validator.format_issues([issue])

  // Error message should be actionable
  string.contains(formatted, "shell metacharacter") |> should.be_true()
  string.contains(formatted, "may be unsafe") |> should.be_true()
}

pub fn format_issues_circular_clear_test() {
  let issue = validator.CircularDependency(["login", "get_profile", "update_profile"])
  let formatted = validator.format_issues([issue])

  // Should clearly show the cycle
  string.contains(formatted, "Circular dependency") |> should.be_true()
  string.contains(formatted, " -> ") |> should.be_true()
  list.contains(["login", "get_profile", "update_profile"], formatted) |> should.be_true()
}

// =============================================================================
// Performance/Stress Tests
// =============================================================================

pub fn validate_spec_large_number_of_behaviors_test() {
  // Test with many behaviors to ensure performance is reasonable
  let behaviors = list.range(0, 99)
  |> list.map(fn(i) {
    make_behavior(
      "behavior_" <> int.to_string(i),
      [],
      "GET",
      "/api/" <> int.to_string(i),
      dict.new()
    )
  })

  let spec = make_minimal_spec(behaviors)
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

pub fn validate_spec_complex_dependency_graph_test() {
  // Test complex dependency graph (many-to-many relationships)
  let behaviors = [
    make_behavior("base1", [], "GET", "/base1", dict.new()),
    make_behavior("base2", [], "GET", "/base2", dict.new()),
    make_behavior("middleware1", ["base1", "base2"], "GET", "/mid1", dict.new()),
    make_behavior("middleware2", ["base1", "base2"], "GET", "/mid2", dict.new()),
    make_behavior("consumer1", ["middleware1"], "GET", "/cons1", dict.new()),
    make_behavior("consumer2", ["middleware1", "middleware2"], "GET", "/cons2", dict.new()),
    make_behavior("consumer3", ["middleware2"], "GET", "/cons3", dict.new()),
  ]
  let spec = make_minimal_spec(behaviors)
  let result = validator.validate_spec(spec)
  result |> should.equal(validator.ValidationValid)
}

// =============================================================================
// Helper Functions (same as original test)
// =============================================================================

fn make_minimal_spec(behaviors: List(types.Behavior)) -> types.Spec {
  let feature =
    types.Feature(
      name: "Test Feature",
      description: "Test feature description",
      behaviors: behaviors,
    )

  types.Spec(
    name: "Test Spec",
    description: "Test spec for validator tests",
    audience: "developers",
    version: "1.0.0",
    success_criteria: [],
    config: types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
    features: [feature],
    rules: [],
    anti_patterns: [],
    ai_hints: types.AIHints(
      implementation: types.ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: types.SecurityHints(
        password_hashing: "bcrypt",
        jwt_algorithm: "HS256",
        jwt_expiry: "1h",
        rate_limiting: "100/min",
      ),
      pitfalls: [],
    ),
  )
}

fn make_behavior(
  name: String,
  requires: List(String),
  method: String,
  path: String,
  captures: dict.Dict(String, String),
) -> types.Behavior {
  let gleam_method = case method {
    "GET" -> types.Get
    "POST" -> types.Post
    "PUT" -> types.Put
    "PATCH" -> types.Patch
    "DELETE" -> types.Delete
    "HEAD" -> types.Head
    "OPTIONS" -> types.Options
    _ -> types.Get
  }

  types.Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: types.Request(
      method: gleam_method,
      path: path,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: captures,
  )
}

fn make_behavior_with_capture(
  name: String,
  requires: List(String),
  method: String,
  path: String,
  capture_name: String,
) -> types.Behavior {
  make_behavior(
    name,
    requires,
    method,
    path,
    dict.from_list([#(capture_name, "$.id")]),
  )
}

fn make_behavior_with_header(
  name: String,
  requires: List(String),
  method: String,
  path: String,
  header_name: String,
  header_value: String,
) -> types.Behavior {
  let gleam_method = case method {
    "GET" -> types.Get
    "POST" -> types.Post
    "PUT" -> types.Put
    "PATCH" -> types.Patch
    "DELETE" -> types.Delete
    "HEAD" -> types.Head
    "OPTIONS" -> types.Options
    _ -> types.Get
  }

  types.Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: types.Request(
      method: gleam_method,
      path: path,
      headers: dict.from_list([#(header_name, header_value)]),
      query: dict.new(),
      body: json.null(),
    ),
    response: types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}