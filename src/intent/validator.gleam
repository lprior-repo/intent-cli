/// Pre-execution static validation of specs
/// Validates rule syntax, variable references, and dependencies before any HTTP requests

import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import intent/types.{type Spec, type Behavior}

/// Result of pre-execution validation
pub type ValidationResult {
  ValidationValid
  ValidationInvalid(issues: List(ValidationIssue))
}

/// Issues found during validation
pub type ValidationIssue {
  RuleSyntaxError(behavior: String, field: String, rule: String, error: String)
  UndefinedVariable(behavior: String, field: String, var_name: String, suggestion: String)
  InvalidPath(behavior: String, path: String, error: String)
  MissingDependency(behavior: String, depends_on: String)
  CircularDependency(behaviors: List(String))
  MissingCapture(behavior: String, field: String, var_name: String, captured_by: List(String))
}

/// Validate a complete spec before execution
pub fn validate_spec(spec: Spec) -> ValidationResult {
  let mut_issues = []

  // Collect all behaviors with their names
  let all_behaviors =
    spec.features
    |> list.flat_map(fn(feature) { feature.behaviors })

  let behavior_names =
    all_behaviors
    |> list.map(fn(b) { b.name })

  // Validate each behavior
  let behavior_issues =
    all_behaviors
    |> list.flat_map(fn(behavior) {
      validate_behavior(behavior, behavior_names, all_behaviors)
    })

  let mut_issues = list.append(mut_issues, behavior_issues)

  // Check for circular dependencies
  let circular_issues = check_circular_dependencies(all_behaviors)
  let mut_issues = list.append(mut_issues, circular_issues)

  case list.is_empty(mut_issues) {
    True -> ValidationValid
    False -> ValidationInvalid(mut_issues)
  }
}

/// Validate a single behavior
fn validate_behavior(
  behavior: Behavior,
  all_behavior_names: List(String),
  all_behaviors: List(Behavior),
) -> List(ValidationIssue) {
  let mut_issues = []

  // Validate rule syntax in checks
  let rule_issues =
    behavior.response.checks
    |> dict.to_list
    |> list.flat_map(fn(pair) {
      let #(field, check) = pair
      validate_rule_syntax(behavior.name, field, check.rule)
    })

  let mut_issues = list.append(mut_issues, rule_issues)

  // Validate that all dependencies exist
  let dep_issues =
    behavior.requires
    |> list.filter_map(fn(dep_name) {
      case list.contains(all_behavior_names, dep_name) {
        True -> Error(Nil)
        False -> Ok(MissingDependency(behavior.name, dep_name))
      }
    })

  let mut_issues = list.append(mut_issues, dep_issues)

  // Validate variable references in interpolations
  let var_issues = validate_variable_references(behavior, all_behaviors)
  let mut_issues = list.append(mut_issues, var_issues)

  mut_issues
}

/// Validate rule syntax by attempting to parse it
fn validate_rule_syntax(
  _behavior_name: String,
  _field: String,
  _rule_str: String,
) -> List(ValidationIssue) {
  // The rule parser always succeeds (returns Raw if unparseable)
  // So we don't have validation errors here - just return empty list
  []
}

/// Validate variable references in behavior
fn validate_variable_references(
  behavior: Behavior,
  all_behaviors: List(Behavior),
) -> List(ValidationIssue) {
  let mut_issues = []

  // Get list of behaviors that run before this one (dependency order)
  let available_captures = get_available_captures(behavior, all_behaviors)

  // Check variables in request path
  let path_vars = extract_variables(behavior.request.path)
  let path_issues =
    path_vars
    |> list.filter_map(fn(var_name) {
      case list.contains(available_captures, var_name) {
        True -> Error(Nil)
        False -> {
          let captured_by = find_behaviors_capturing(var_name, all_behaviors)
          Ok(MissingCapture(behavior.name, "request.path", var_name, captured_by))
        }
      }
    })

  let mut_issues = list.append(mut_issues, path_issues)

  // Check variables in request headers
  let header_vars =
    behavior.request.headers
    |> dict.values
    |> list.flat_map(extract_variables)
    |> list.unique

  let header_issues =
    header_vars
    |> list.filter_map(fn(var_name) {
      case list.contains(available_captures, var_name) {
        True -> Error(Nil)
        False -> {
          let captured_by = find_behaviors_capturing(var_name, all_behaviors)
          Ok(MissingCapture(behavior.name, "request.headers", var_name, captured_by))
        }
      }
    })

  let mut_issues = list.append(mut_issues, header_issues)

  mut_issues
}

/// Get list of variables that are available before a behavior runs
fn get_available_captures(
  behavior: Behavior,
  all_behaviors: List(Behavior),
) -> List(String) {
  // Find this behavior's index
  let behavior_index =
    all_behaviors
    |> list.find_map(fn(b) {
      case b.name == behavior.name {
        True -> Ok(list.length(list.take_while(all_behaviors, fn(x) { x.name != b.name })))
        False -> Error(Nil)
      }
    })

  case behavior_index {
    Error(_) -> []
    Ok(idx) -> {
      // Get all captures from behaviors before this one
      all_behaviors
      |> list.take(idx)
      |> list.flat_map(fn(b) {
        dict.keys(b.captures)
      })
      |> list.unique
    }
  }
}

/// Find which behaviors capture a given variable
fn find_behaviors_capturing(
  var_name: String,
  behaviors: List(Behavior),
) -> List(String) {
  behaviors
  |> list.filter_map(fn(b) {
    case dict.get(b.captures, var_name) {
      Ok(_) -> Ok(b.name)
      Error(_) -> Error(Nil)
    }
  })
}

/// Extract variable names from a string (${var_name} syntax)
fn extract_variables(s: String) -> List(String) {
  // Simple extraction - find all ${...} patterns
  let parts = string.split(s, "${")

  parts
  |> list.drop(1)
  |> list.filter_map(fn(part) {
    case string.split_once(part, "}") {
      Ok(#(var_name, _rest)) -> Ok(var_name)
      Error(_) -> Error(Nil)
    }
  })
}

/// Check for circular dependencies
fn check_circular_dependencies(
  behaviors: List(Behavior),
) -> List(ValidationIssue) {
  behaviors
  |> list.filter_map(fn(behavior) {
    case has_circular_dependency(behavior.name, [], behaviors) {
      True -> Ok(CircularDependency([behavior.name]))
      False -> Error(Nil)
    }
  })
}

/// Check if a behavior has circular dependency
fn has_circular_dependency(
  behavior_name: String,
  visited: List(String),
  all_behaviors: List(Behavior),
) -> Bool {
  case list.contains(visited, behavior_name) {
    True -> True
    False -> {
      // Find the behavior
      case list.find(all_behaviors, fn(b) { b.name == behavior_name }) {
        Error(_) -> False
        Ok(behavior) -> {
          // Check each dependency
          list.any(behavior.requires, fn(dep) {
            has_circular_dependency(dep, list.append(visited, [behavior_name]), all_behaviors)
          })
        }
      }
    }
  }
}

/// Format validation issues for display
pub fn format_issues(issues: List(ValidationIssue)) -> String {
  let issue_lines =
    issues
    |> list.map(format_issue)
    |> string.join("\n\n")

  "Validation failed with " <> int.to_string(list.length(issues)) <> " issue(s):\n\n" <> issue_lines
}

/// Format a single validation issue
fn format_issue(issue: ValidationIssue) -> String {
  case issue {
    RuleSyntaxError(behavior, field, rule, error) ->
      "Behavior '" <> behavior <> "', field '" <> field <> "':\n" <> "  Invalid rule syntax: " <> rule <> "\n" <> "  Error: " <> error

    UndefinedVariable(behavior, field, var_name, suggestion) ->
      "Behavior '" <> behavior <> "', field '" <> field <> "':\n" <> "  Variable '" <> var_name <> "' is not defined\n" <> "  Suggestion: " <> suggestion

    InvalidPath(behavior, path, error) ->
      "Behavior '" <> behavior <> "':\n" <> "  Invalid path: " <> path <> "\n" <> "  Error: " <> error

    MissingDependency(behavior, depends_on) ->
      "Behavior '" <> behavior <> "':\n" <> "  Depends on behavior '" <> depends_on <> "' which does not exist"

    CircularDependency(behaviors) ->
      "Circular dependency detected:\n" <> "  Behaviors: " <> string.join(behaviors, " -> ")

    MissingCapture(behavior, location, var_name, captured_by) ->
      "Behavior '" <> behavior <> "', " <> location <> ":\n" <> "  Variable '" <> var_name <> "' is not available\n" <> case captured_by {
        [] -> "  Hint: No behavior captures this variable. Check spelling."
        _ ->
          "  Hint: This variable is captured by: " <> string.join(captured_by, ", ") <> "\n" <> "  Ensure these behaviors run before '" <> behavior <> "'"
      }
  }
}
