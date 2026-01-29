//// Tests for the validator module
//// Tests pre-execution static validation of specs including:
//// - Rule syntax validation
//// - Variable reference validation
//// - Dependency validation
//// - Circular dependency detection
//// - Issue formatting

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/types.{
  type Behavior, type Check, Behavior, Check, Request, Response,
}
import intent/validator.{
  CircularDependency, MissingCapture, MissingDependency, ValidationInvalid,
  ValidationValid,
}
import test_helpers.{
  make_test_behavior, make_test_feature, make_test_spec,
  make_test_spec_from_behaviors,
}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Happy Path Tests - Valid Specs
// ============================================================================

/// Test that a valid spec with no dependencies passes validation
pub fn valid_spec_no_dependencies_passes_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", [])
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test that a valid spec with proper dependencies passes validation
pub fn valid_spec_with_dependencies_passes_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["b"])
  let spec = make_test_spec_from_behaviors([a, b, c])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test that an empty spec passes validation
pub fn empty_spec_passes_test() {
  let spec = make_test_spec([])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test that a spec with multiple features passes validation
pub fn valid_spec_multiple_features_passes_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let feature1 = make_test_feature("Feature1", [a])
  let feature2 = make_test_feature("Feature2", [b])
  let spec = make_test_spec([feature1, feature2])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test that a spec with diamond dependencies passes validation
pub fn valid_spec_diamond_dependencies_passes_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["a"])
  let d = make_test_behavior("d", ["b", "c"])
  let spec = make_test_spec_from_behaviors([a, b, c, d])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test that behaviors with checks but valid rules pass validation
pub fn valid_spec_with_checks_passes_test() {
  let behavior =
    make_behavior_with_checks(
      "test",
      [],
      dict.from_list([
        #("status", Check(rule: "== 200", why: "Success response")),
        #("body.id", Check(rule: "> 0", why: "ID must be positive")),
      ]),
    )
  let spec = make_test_spec_from_behaviors([behavior])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

// ============================================================================
// Missing Dependency Tests
// ============================================================================

/// Test that a spec with a missing dependency fails validation
pub fn missing_dependency_fails_test() {
  let a = make_test_behavior("a", ["nonexistent"])
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      list.length(issues)
      |> should.equal(1)
      let assert [MissingDependency(behavior, depends_on)] = issues
      behavior
      |> should.equal("a")
      depends_on
      |> should.equal("nonexistent")
    }
    ValidationValid -> should.fail()
  }
}

/// Test that multiple missing dependencies are all reported
pub fn multiple_missing_dependencies_fails_test() {
  let a = make_test_behavior("a", ["missing1", "missing2"])
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      // Should have 2 missing dependency issues
      list.length(issues)
      |> should.equal(2)
      // Both should be MissingDependency
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

/// Test that missing dependency in middle of chain is detected
pub fn missing_dependency_in_chain_fails_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a", "missing"])
  let c = make_test_behavior("c", ["b"])
  let spec = make_test_spec_from_behaviors([a, b, c])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingDependency("b", "missing") -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

/// Test missing dependency across features
pub fn missing_dependency_across_features_fails_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["nonexistent"])
  let feature1 = make_test_feature("Feature1", [a])
  let feature2 = make_test_feature("Feature2", [b])
  let spec = make_test_spec([feature1, feature2])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingDependency("b", "nonexistent") -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

// ============================================================================
// Circular Dependency Tests
// ============================================================================

/// Test that simple circular dependency A -> B -> A is detected
pub fn simple_circular_dependency_fails_test() {
  let a = make_test_behavior("a", ["b"])
  let b = make_test_behavior("b", ["a"])
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  case result {
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

/// Test that three-way circular dependency A -> B -> C -> A is detected
pub fn three_way_circular_dependency_fails_test() {
  let a = make_test_behavior("a", ["c"])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["b"])
  let spec = make_test_spec_from_behaviors([a, b, c])

  let result = validator.validate_spec(spec)

  case result {
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

/// Test that self-dependency A -> A is detected
pub fn self_dependency_fails_test() {
  let a = make_test_behavior("a", ["a"])
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          CircularDependency(behaviors) -> list.contains(behaviors, "a")
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

/// Test circular dependency in complex graph with valid behaviors
pub fn circular_in_complex_graph_fails_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  // Create circular dependency between c and d
  let c = make_test_behavior("c", ["d"])
  let d = make_test_behavior("d", ["c"])
  let spec = make_test_spec_from_behaviors([a, b, c, d])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      // Should detect circular dependency involving c and d
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
// Variable Reference Tests - Missing Captures
// ============================================================================

/// Test that using a variable not captured by any behavior fails
pub fn missing_capture_in_path_fails_test() {
  let a = make_behavior_with_path("a", [], "/users/${user_id}")
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture("a", "request.path", "user_id", _) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

/// Test that variable captured by a behavior before current is valid
pub fn captured_variable_available_passes_test() {
  let a = make_behavior_with_capture("create_user", [], "user_id", "body.id")
  let b =
    make_behavior_with_path("get_user", ["create_user"], "/users/${user_id}")
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test that variable captured by a behavior after current is invalid
pub fn capture_not_available_before_fails_test() {
  // The capture happens in the second behavior, but first behavior tries to use it
  let a = make_behavior_with_path("get_user", [], "/users/${user_id}")
  let b = make_behavior_with_capture("create_user", [], "user_id", "body.id")
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture("get_user", "request.path", "user_id", captured_by) -> {
            // The hint should mention create_user as the behavior that captures this
            list.contains(captured_by, "create_user")
          }
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

/// Test missing variable in request headers
pub fn missing_capture_in_headers_fails_test() {
  let a = make_behavior_with_header("a", [], "Authorization", "Bearer ${token}")
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      issues
      |> list.any(fn(issue) {
        case issue {
          MissingCapture("a", "request.headers", "token", _) -> True
          _ -> False
        }
      })
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

/// Test that multiple variables in path are all validated
pub fn multiple_variables_in_path_fails_test() {
  let a = make_behavior_with_path("a", [], "/orgs/${org_id}/users/${user_id}")
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      // Should have issues for both org_id and user_id
      let missing_vars =
        issues
        |> list.filter_map(fn(issue) {
          case issue {
            MissingCapture(_, _, var_name, _) -> Ok(var_name)
            _ -> Error(Nil)
          }
        })
      list.contains(missing_vars, "org_id")
      |> should.be_true
      list.contains(missing_vars, "user_id")
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

// ============================================================================
// Format Issues Tests
// ============================================================================

/// Test formatting MissingDependency issue
pub fn format_missing_dependency_issue_test() {
  let issues = [MissingDependency("behavior_a", "missing_dep")]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("Validation failed with 1 issue")
  |> should.be_true
  formatted
  |> string.contains("behavior_a")
  |> should.be_true
  formatted
  |> string.contains("missing_dep")
  |> should.be_true
  formatted
  |> string.contains("does not exist")
  |> should.be_true
}

/// Test formatting CircularDependency issue
pub fn format_circular_dependency_issue_test() {
  let issues = [CircularDependency(["a", "b", "c"])]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("Circular dependency")
  |> should.be_true
  formatted
  |> string.contains("a")
  |> should.be_true
  formatted
  |> string.contains("b")
  |> should.be_true
  formatted
  |> string.contains("c")
  |> should.be_true
}

/// Test formatting MissingCapture issue with no capturers
pub fn format_missing_capture_no_capturers_test() {
  let issues = [MissingCapture("get_user", "request.path", "user_id", [])]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("get_user")
  |> should.be_true
  formatted
  |> string.contains("user_id")
  |> should.be_true
  formatted
  |> string.contains("No behavior captures this variable")
  |> should.be_true
}

/// Test formatting MissingCapture issue with capturers
pub fn format_missing_capture_with_capturers_test() {
  let issues = [
    MissingCapture("get_user", "request.path", "user_id", ["create_user"]),
  ]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("get_user")
  |> should.be_true
  formatted
  |> string.contains("user_id")
  |> should.be_true
  formatted
  |> string.contains("create_user")
  |> should.be_true
  formatted
  |> string.contains("Ensure these behaviors run before")
  |> should.be_true
}

/// Test formatting RuleSyntaxError issue
pub fn format_rule_syntax_error_issue_test() {
  let issues = [
    validator.RuleSyntaxError("behavior_a", "status", "= 200", "Expected =="),
  ]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("behavior_a")
  |> should.be_true
  formatted
  |> string.contains("status")
  |> should.be_true
  formatted
  |> string.contains("= 200")
  |> should.be_true
  formatted
  |> string.contains("Expected ==")
  |> should.be_true
}

/// Test formatting UndefinedVariable issue
pub fn format_undefined_variable_issue_test() {
  let issues = [
    validator.UndefinedVariable(
      "behavior_a",
      "body.name",
      "usrname",
      "Did you mean 'username'?",
    ),
  ]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("behavior_a")
  |> should.be_true
  formatted
  |> string.contains("usrname")
  |> should.be_true
  formatted
  |> string.contains("not defined")
  |> should.be_true
  formatted
  |> string.contains("Did you mean 'username'?")
  |> should.be_true
}

/// Test formatting InvalidPath issue
pub fn format_invalid_path_issue_test() {
  let issues = [
    validator.InvalidPath(
      "behavior_a",
      "invalid//path",
      "Double slashes not allowed",
    ),
  ]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("behavior_a")
  |> should.be_true
  formatted
  |> string.contains("invalid//path")
  |> should.be_true
  formatted
  |> string.contains("Double slashes not allowed")
  |> should.be_true
}

/// Test formatting multiple issues
pub fn format_multiple_issues_test() {
  let issues = [
    MissingDependency("a", "missing"),
    CircularDependency(["b", "c"]),
    MissingCapture("d", "request.path", "var", []),
  ]
  let formatted = validator.format_issues(issues)

  formatted
  |> string.contains("Validation failed with 3 issue")
  |> should.be_true
}

// ============================================================================
// Edge Cases
// ============================================================================

/// Test behavior with empty requires list is valid
pub fn empty_requires_list_passes_test() {
  let a = make_test_behavior("a", [])
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test behavior with empty string in path (no variables)
pub fn path_without_variables_passes_test() {
  let a = make_behavior_with_path("a", [], "/users/list")
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test behavior with incomplete variable syntax in path
pub fn incomplete_variable_syntax_passes_test() {
  // ${var without closing brace - should not be extracted as variable
  let a = make_behavior_with_path("a", [], "/users/${incomplete")
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  // Should pass since ${incomplete is not a valid variable reference
  result
  |> should.equal(ValidationValid)
}

/// Test spec with single behavior passes
pub fn single_behavior_spec_passes_test() {
  let a = make_test_behavior("single", [])
  let spec = make_test_spec_from_behaviors([a])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test feature with no behaviors passes
pub fn empty_feature_passes_test() {
  let feature = make_test_feature("EmptyFeature", [])
  let spec = make_test_spec([feature])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test large spec with many behaviors and valid dependencies
pub fn large_valid_spec_passes_test() {
  // Create a chain of 20 behaviors
  let behaviors =
    list.range(0, 19)
    |> list.map(fn(i) {
      case i {
        0 -> make_test_behavior("b0", [])
        _ -> {
          let name = "b" <> string.inspect(i)
          let prev = "b" <> string.inspect(i - 1)
          make_test_behavior(name, [prev])
        }
      }
    })
  let spec = make_test_spec_from_behaviors(behaviors)

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test spec with behavior having multiple valid dependencies
pub fn multiple_valid_dependencies_passes_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", [])
  let c = make_test_behavior("c", [])
  let d = make_test_behavior("d", ["a", "b", "c"])
  let spec = make_test_spec_from_behaviors([a, b, c, d])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test that validation collects all issues, not just the first
pub fn validation_collects_all_issues_test() {
  // Create spec with multiple problems
  let a = make_test_behavior("a", ["missing1"])
  let b = make_test_behavior("b", ["missing2"])
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      // Should have at least 2 issues (one for each missing dependency)
      { list.length(issues) >= 2 }
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

/// Test variable in path with special characters
pub fn variable_with_underscore_in_path_test() {
  let a = make_behavior_with_capture("setup", [], "my_var_name", "body.value")
  let b = make_behavior_with_path("use", ["setup"], "/items/${my_var_name}")
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

/// Test multiple variables captured by same behavior
pub fn multiple_captures_from_same_behavior_test() {
  let a =
    make_behavior_with_captures("setup", [], [
      #("user_id", "body.user.id"),
      #("token", "body.token"),
    ])
  let b =
    make_behavior_with_path_and_header(
      "use",
      ["setup"],
      "/users/${user_id}",
      "Authorization",
      "Bearer ${token}",
    )
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  result
  |> should.equal(ValidationValid)
}

// ============================================================================
// Combined Error Tests
// ============================================================================

/// Test spec with both missing dependency and circular dependency
pub fn combined_missing_and_circular_fails_test() {
  let a = make_test_behavior("a", ["missing"])
  let b = make_test_behavior("b", ["c"])
  let c = make_test_behavior("c", ["b"])
  let spec = make_test_spec_from_behaviors([a, b, c])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      // Should have both types of issues
      let has_missing =
        list.any(issues, fn(issue) {
          case issue {
            MissingDependency(_, _) -> True
            _ -> False
          }
        })
      let has_circular =
        list.any(issues, fn(issue) {
          case issue {
            CircularDependency(_) -> True
            _ -> False
          }
        })
      has_missing
      |> should.be_true
      has_circular
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

/// Test spec with missing dependency and missing capture
pub fn combined_missing_dep_and_capture_fails_test() {
  let a = make_test_behavior("a", ["nonexistent"])
  let b = make_behavior_with_path("b", [], "/users/${unknown_var}")
  let spec = make_test_spec_from_behaviors([a, b])

  let result = validator.validate_spec(spec)

  case result {
    ValidationInvalid(issues) -> {
      let has_missing_dep =
        list.any(issues, fn(issue) {
          case issue {
            MissingDependency(_, _) -> True
            _ -> False
          }
        })
      let has_missing_capture =
        list.any(issues, fn(issue) {
          case issue {
            MissingCapture(_, _, _, _) -> True
            _ -> False
          }
        })
      has_missing_dep
      |> should.be_true
      has_missing_capture
      |> should.be_true
    }
    ValidationValid -> should.fail()
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create a behavior with a custom request path
fn make_behavior_with_path(
  name: String,
  requires: List(String),
  path: String,
) -> Behavior {
  Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: Request(
      method: types.Get,
      path: path,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(status: 200, example: json.null(), checks: dict.new()),
    captures: dict.new(),
  )
}

/// Create a behavior with a capture
fn make_behavior_with_capture(
  name: String,
  requires: List(String),
  capture_name: String,
  capture_path: String,
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
    response: Response(status: 200, example: json.null(), checks: dict.new()),
    captures: dict.from_list([#(capture_name, capture_path)]),
  )
}

/// Create a behavior with multiple captures
fn make_behavior_with_captures(
  name: String,
  requires: List(String),
  captures: List(#(String, String)),
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
    response: Response(status: 200, example: json.null(), checks: dict.new()),
    captures: dict.from_list(captures),
  )
}

/// Create a behavior with a custom header
fn make_behavior_with_header(
  name: String,
  requires: List(String),
  header_name: String,
  header_value: String,
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
      headers: dict.from_list([#(header_name, header_value)]),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(status: 200, example: json.null(), checks: dict.new()),
    captures: dict.new(),
  )
}

/// Create a behavior with custom checks
fn make_behavior_with_checks(
  name: String,
  requires: List(String),
  checks: dict.Dict(String, Check),
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
    response: Response(status: 200, example: json.null(), checks: checks),
    captures: dict.new(),
  )
}

/// Create a behavior with both custom path and header
fn make_behavior_with_path_and_header(
  name: String,
  requires: List(String),
  path: String,
  header_name: String,
  header_value: String,
) -> Behavior {
  Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: Request(
      method: types.Get,
      path: path,
      headers: dict.from_list([#(header_name, header_value)]),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(status: 200, example: json.null(), checks: dict.new()),
    captures: dict.new(),
  )
}
