//// Comprehensive tests for the validator module
//// Tests pre-execution validation of specs including:
//// - Missing dependencies
//// - Circular dependencies
//// - Variable references and captures
//// - Format issues output

import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string
import gleeunit/should
import intent/types.{
  type Behavior, type Feature, type Spec, AIHints, Behavior, Config,
  Feature, ImplementationHints, Request, Response, SecurityHints, Spec,
}
import intent/validator.{
  CircularDependency,
  MissingCapture, MissingDependency, ValidationInvalid, ValidationValid,
}

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a minimal behavior for testing
fn make_test_behavior(
  name: String,
  requires: List(String),
) -> Behavior {
  Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: Request(
      method: types.Get,
      path: "/" <> name,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

/// Create a behavior with a path containing variables
fn make_behavior_with_path(
  name: String,
  path: String,
  requires: List(String),
) -> Behavior {
  Behavior(
    ..make_test_behavior(name, requires),
    request: Request(
      method: types.Get,
      path: path,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
  )
}

/// Create a behavior with captures
fn make_behavior_with_captures(
  name: String,
  requires: List(String),
  captures: List(#(String, String)),
) -> Behavior {
  Behavior(
    ..make_test_behavior(name, requires),
    captures: dict.from_list(captures),
  )
}

/// Create a behavior with header variables
fn make_behavior_with_headers(
  name: String,
  requires: List(String),
  headers: List(#(String, String)),
) -> Behavior {
  Behavior(
    ..make_test_behavior(name, requires),
    request: Request(
      method: types.Get,
      path: "/" <> name,
      headers: dict.from_list(headers),
      query: dict.new(),
      body: json.null(),
    ),
  )
}

/// Create a feature from behaviors
fn make_test_feature(name: String, behaviors: List(Behavior)) -> Feature {
  Feature(name: name, description: "Test feature", behaviors: behaviors)
}

/// Create a minimal spec from features
fn make_test_spec(features: List(Feature)) -> Spec {
  Spec(
    name: "Test Spec",
    description: "Test specification",
    audience: "developers",
    version: "1.0.0",
    success_criteria: [],
    config: Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
    features: features,
    rules: [],
    anti_patterns: [],
    ai_hints: AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
      codebase: None,
    ),
  )
}

// ============================================================================
// Valid Spec Tests (Happy Path)
// ============================================================================

pub fn validator_valid_spec_no_deps_test() {
  // Spec with no dependencies should be valid
  let b1 = make_test_behavior("get-users", [])
  let b2 = make_test_behavior("get-products", [])
  let spec = make_test_spec([make_test_feature("API", [b1, b2])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_valid_spec_linear_deps_test() {
  // Spec with valid linear dependencies
  let b1 = make_test_behavior("create-user", [])
  let b2 = make_test_behavior("get-user", ["create-user"])
  let b3 = make_test_behavior("delete-user", ["get-user"])
  let spec = make_test_spec([make_test_feature("Users", [b1, b2, b3])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_valid_spec_diamond_deps_test() {
  // Diamond dependency pattern: D depends on B and C, both depend on A
  let a = make_test_behavior("setup", [])
  let b = make_test_behavior("path-b", ["setup"])
  let c = make_test_behavior("path-c", ["setup"])
  let d = make_test_behavior("final", ["path-b", "path-c"])
  let spec = make_test_spec([make_test_feature("Diamond", [a, b, c, d])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_valid_spec_cross_feature_deps_test() {
  // Dependencies across different features
  let b1 = make_test_behavior("auth-login", [])
  let b2 = make_test_behavior("get-profile", ["auth-login"])
  let f1 = make_test_feature("Auth", [b1])
  let f2 = make_test_feature("Profile", [b2])
  let spec = make_test_spec([f1, f2])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_empty_spec_test() {
  // Empty spec with no behaviors should be valid
  let spec = make_test_spec([])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_empty_feature_test() {
  // Feature with no behaviors should be valid
  let spec = make_test_spec([make_test_feature("Empty", [])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

// ============================================================================
// Missing Dependency Tests
// ============================================================================

pub fn validator_missing_dependency_single_test() {
  // Behavior depends on non-existent behavior
  let b1 = make_test_behavior("get-user", ["nonexistent"])
  let spec = make_test_spec([make_test_feature("Users", [b1])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      list.length(issues)
      |> should.equal(1)

      case list.first(issues) {
        Ok(MissingDependency(behavior, depends_on)) -> {
          behavior
          |> should.equal("get-user")
          depends_on
          |> should.equal("nonexistent")
        }
        _ -> should.fail()
      }
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_missing_dependency_multiple_test() {
  // Behavior depends on multiple non-existent behaviors
  let b1 = make_test_behavior("final", ["missing-a", "missing-b"])
  let spec = make_test_spec([make_test_feature("Test", [b1])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      // Should have 2 missing dependency issues
      list.length(issues)
      |> should.equal(2)

      // Verify both are MissingDependency type
      issues
      |> list.all(fn(issue) {
        case issue {
          MissingDependency(_, _) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_missing_dependency_typo_test() {
  // Common typo scenario - almost correct name
  let b1 = make_test_behavior("create-user", [])
  let b2 = make_test_behavior("get-user", ["create-users"])
  // Typo: "create-users" instead of "create-user"
  let spec = make_test_spec([make_test_feature("Users", [b1, b2])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      case list.first(issues) {
        Ok(MissingDependency(_, depends_on)) -> {
          depends_on
          |> should.equal("create-users")
        }
        _ -> should.fail()
      }
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_missing_dependency_partial_chain_test() {
  // Valid chain with one missing link
  let b1 = make_test_behavior("first", [])
  let b2 = make_test_behavior("second", ["first"])
  let b3 = make_test_behavior("third", ["missing-middle"])
  let spec = make_test_spec([make_test_feature("Chain", [b1, b2, b3])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      list.length(issues)
      |> should.equal(1)
    }
    ValidationValid -> should.fail()
  }
}

// ============================================================================
// Circular Dependency Tests
// ============================================================================

pub fn validator_circular_self_loop_test() {
  // Behavior depends on itself
  let b1 = make_test_behavior("self-loop", ["self-loop"])
  let spec = make_test_spec([make_test_feature("Test", [b1])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          CircularDependency(_) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_circular_two_cycle_test() {
  // A -> B -> A cycle
  let a = make_test_behavior("alpha", ["beta"])
  let b = make_test_behavior("beta", ["alpha"])
  let spec = make_test_spec([make_test_feature("Test", [a, b])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          CircularDependency(_) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_circular_three_cycle_test() {
  // A -> B -> C -> A cycle
  let a = make_test_behavior("alpha", ["gamma"])
  let b = make_test_behavior("beta", ["alpha"])
  let c = make_test_behavior("gamma", ["beta"])
  let spec = make_test_spec([make_test_feature("Test", [a, b, c])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          CircularDependency(_) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_circular_with_valid_behaviors_test() {
  // Mix of circular and valid behaviors
  // Valid: d1 -> d2
  // Circular: c1 -> c2 -> c1
  let d1 = make_test_behavior("dep-first", [])
  let d2 = make_test_behavior("dep-second", ["dep-first"])
  let c1 = make_test_behavior("cycle-a", ["cycle-b"])
  let c2 = make_test_behavior("cycle-b", ["cycle-a"])
  let spec = make_test_spec([make_test_feature("Mixed", [d1, d2, c1, c2])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      // Should detect circular but not complain about valid deps
      issues
      |> list.any(fn(issue) {
        case issue {
          CircularDependency(_) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

// ============================================================================
// Variable Reference / Capture Tests
// ============================================================================

pub fn validator_valid_capture_reference_test() {
  // B uses variable captured by A (which runs first)
  let a =
    make_behavior_with_captures("create-user", [], [#("user_id", "$.id")])
  let b = make_behavior_with_path("get-user", "/users/${user_id}", ["create-user"])
  let spec = make_test_spec([make_test_feature("Users", [a, b])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_missing_capture_in_path_test() {
  // B uses variable that was never captured
  let a = make_test_behavior("create-user", [])
  let b = make_behavior_with_path("get-user", "/users/${user_id}", ["create-user"])
  let spec = make_test_spec([make_test_feature("Users", [a, b])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture(_, "request.path", "user_id", _) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_missing_capture_in_header_test() {
  // Variable in header that was never captured
  let a = make_test_behavior("login", [])
  let b =
    make_behavior_with_headers(
      "get-profile",
      ["login"],
      [#("Authorization", "Bearer ${token}")],
    )
  let spec = make_test_spec([make_test_feature("Auth", [a, b])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture(_, "request.headers", "token", _) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_capture_from_later_behavior_test() {
  // B tries to use variable captured by C, but C runs AFTER B
  // Order: A -> B -> C, but B needs C's capture
  let a = make_test_behavior("setup", [])
  let b = make_behavior_with_path("middle", "/items/${item_id}", ["setup"])
  let c =
    make_behavior_with_captures("create-item", ["setup"], [#("item_id", "$.id")])
  // B and C both depend on A, but B tries to use C's capture
  let spec = make_test_spec([make_test_feature("Items", [a, b, c])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture(behavior, _, "item_id", captured_by) -> {
            behavior == "middle" && list.contains(captured_by, "create-item")
          }
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_multiple_variables_in_path_test() {
  // Path with multiple variables, some missing
  let a =
    make_behavior_with_captures("setup", [], [#("org_id", "$.org")])
  let b =
    make_behavior_with_path(
      "get-user-in-org",
      "/orgs/${org_id}/users/${user_id}",
      ["setup"],
    )
  let spec = make_test_spec([make_test_feature("Orgs", [a, b])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      // Should find user_id missing but org_id is ok
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture(_, _, "user_id", _) -> True
          _ -> False
        }
      })
      |> should.be_true

      // Should NOT complain about org_id
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture(_, _, "org_id", _) -> True
          _ -> False
        }
      })
      |> should.be_false
    }
    ValidationValid -> should.fail()
  }
}

pub fn validator_no_variables_in_path_test() {
  // Path without any variables - should always be valid
  let b = make_behavior_with_path("static-path", "/api/v1/status", [])
  let spec = make_test_spec([make_test_feature("API", [b])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_first_behavior_no_captures_available_test() {
  // First behavior can't use any captures (none exist yet)
  let a = make_behavior_with_path("first", "/users/${user_id}", [])
  let spec = make_test_spec([make_test_feature("Test", [a])])

  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture("first", _, "user_id", captured_by) -> {
            // Hint should show no behavior captures this
            list.is_empty(captured_by)
          }
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

// ============================================================================
// Format Issues Tests
// ============================================================================

pub fn validator_format_single_missing_dep_test() {
  let issues = [MissingDependency("get-user", "create-user")]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("1 issue")
  |> should.be_true

  formatted
  |> string.contains("get-user")
  |> should.be_true

  formatted
  |> string.contains("create-user")
  |> should.be_true
}

pub fn validator_format_circular_dep_test() {
  let issues = [CircularDependency(["alpha", "beta", "alpha"])]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("Circular dependency")
  |> should.be_true
}

pub fn validator_format_missing_capture_with_hint_test() {
  let issues = [MissingCapture("get-user", "request.path", "user_id", ["create-user", "register-user"])]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("user_id")
  |> should.be_true

  formatted
  |> string.contains("create-user")
  |> should.be_true

  formatted
  |> string.contains("Hint")
  |> should.be_true
}

pub fn validator_format_missing_capture_no_hint_test() {
  let issues = [MissingCapture("get-user", "request.path", "unknown_var", [])]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("unknown_var")
  |> should.be_true

  // Should suggest checking spelling when no behavior captures it
  formatted
  |> string.contains("spelling")
  |> should.be_true
}

pub fn validator_format_multiple_issues_test() {
  let issues = [
    MissingDependency("a", "missing-a"),
    MissingDependency("b", "missing-b"),
    CircularDependency(["x", "y"]),
  ]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("3 issue")
  |> should.be_true
}

// ============================================================================
// Edge Cases and Boundary Tests
// ============================================================================

pub fn validator_behavior_with_empty_requires_test() {
  // Explicitly empty requires list
  let b = make_test_behavior("standalone", [])
  let spec = make_test_spec([make_test_feature("Test", [b])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_long_dependency_chain_test() {
  // Long linear chain: a -> b -> c -> d -> e -> f
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["b"])
  let d = make_test_behavior("d", ["c"])
  let e = make_test_behavior("e", ["d"])
  let f = make_test_behavior("f", ["e"])
  let spec = make_test_spec([make_test_feature("Chain", [a, b, c, d, e, f])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_complex_capture_chain_test() {
  // Chain of captures: each behavior captures a value used by the next
  let a = make_behavior_with_captures("create-org", [], [#("org_id", "$.id")])
  let b_req = Request(
    method: types.Post,
    path: "/orgs/${org_id}/users",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )
  let b =
    Behavior(
      ..make_test_behavior("create-user", ["create-org"]),
      request: b_req,
      captures: dict.from_list([#("user_id", "$.id")]),
    )
  let c = make_behavior_with_path("get-user", "/orgs/${org_id}/users/${user_id}", ["create-user"])
  let spec = make_test_spec([make_test_feature("Chain", [a, b, c])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_special_chars_in_variable_name_test() {
  // Variable names with underscores and dashes
  let a =
    make_behavior_with_captures(
      "setup",
      [],
      [#("user_auth_token", "$.token"), #("api-key", "$.key")],
    )
  let b =
    make_behavior_with_path(
      "api-call",
      "/api?token=${user_auth_token}&key=${api-key}",
      ["setup"],
    )
  let spec = make_test_spec([make_test_feature("API", [a, b])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_duplicate_capture_names_test() {
  // Multiple behaviors capture the same variable name
  let a = make_behavior_with_captures("create-a", [], [#("id", "$.id")])
  let b = make_behavior_with_captures("create-b", ["create-a"], [#("id", "$.id")])
  let c = make_behavior_with_path("use-id", "/items/${id}", ["create-b"])
  let spec = make_test_spec([make_test_feature("Dupes", [a, b, c])])

  // Should be valid - last capture wins or any capture satisfies
  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_unclosed_variable_syntax_test() {
  // Path with unclosed ${variable (no closing brace)
  let a = make_behavior_with_path("broken", "/users/${user_id", [])
  let spec = make_test_spec([make_test_feature("Test", [a])])

  // The extract_variables function should handle this gracefully
  // (returns empty list for malformed variables)
  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}

pub fn validator_empty_variable_name_test() {
  // Path with empty variable: ${}
  let a = make_behavior_with_path("empty-var", "/users/${}", [])
  let spec = make_test_spec([make_test_feature("Test", [a])])

  // Empty variable name - extract_variables returns "" which won't match any capture
  case validator.validate_spec(spec) {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture(_, _, "", _) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> {
      // Also acceptable if it ignores empty variable names
      should.be_ok(Ok(Nil))
    }
  }
}

pub fn validator_consecutive_variables_test() {
  // Path with consecutive variables: ${a}${b}
  let a =
    make_behavior_with_captures(
      "setup",
      [],
      [#("prefix", "$.prefix"), #("suffix", "$.suffix")],
    )
  let b = make_behavior_with_path("concat", "/items/${prefix}${suffix}", ["setup"])
  let spec = make_test_spec([make_test_feature("Concat", [a, b])])

  validator.validate_spec(spec)
  |> should.equal(ValidationValid)
}
