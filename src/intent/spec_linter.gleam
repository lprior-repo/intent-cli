/// Spec linting - proactive detection of anti-patterns and quality issues
/// Checks response examples for anti-patterns before execution

import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import intent/types.{type Spec, type AntiPattern, type Behavior}

/// Linting result
pub type LintResult {
  LintValid
  LintWarnings(warnings: List(LintWarning))
}

/// A warning about the spec
pub type LintWarning {
  AntiPatternDetected(behavior: String, pattern_name: String, details: String)
  VagueRule(behavior: String, field: String, rule: String)
  MissingExample(behavior: String)
  UnusedAntiPattern(pattern_name: String)
  NamingConvention(behavior: String, suggestion: String)
}

/// Lint a complete spec
pub fn lint_spec(spec: Spec) -> LintResult {
  let mut_warnings = []

  let behaviors =
    spec.features
    |> list.flat_map(fn(f) { f.behaviors })

  // Check for anti-patterns in response examples
  let antipattern_warnings =
    behaviors
    |> list.flat_map(fn(behavior) {
      check_anti_patterns(behavior, spec.anti_patterns)
    })

  let mut_warnings = list.append(mut_warnings, antipattern_warnings)

  // Check for vague rules
  let vague_warnings =
    behaviors
    |> list.flat_map(check_for_vague_rules)

  let mut_warnings = list.append(mut_warnings, vague_warnings)

  // Check for missing examples
  let example_warnings =
    behaviors
    |> list.filter_map(fn(b) {
      case b.response.example == json.null() {
        True -> Ok(MissingExample(b.name))
        False -> Error(Nil)
      }
    })

  let mut_warnings = list.append(mut_warnings, example_warnings)

  // Check for naming conventions
  let naming_warnings =
    behaviors
    |> list.filter_map(check_naming_convention)

  let mut_warnings = list.append(mut_warnings, naming_warnings)

  // Check for unused anti-patterns
  let used_patterns =
    behaviors
    |> list.flat_map(fn(b) {
      spec.anti_patterns
      |> list.filter(fn(ap) {
        b.response.example != json.null()
        && contains_anti_pattern_keys(b.response.example, ap)
      })
      |> list.map(fn(ap) { ap.name })
    })
    |> list.unique

  let unused_warnings =
    spec.anti_patterns
    |> list.filter_map(fn(ap) {
      case list.contains(used_patterns, ap.name) {
        True -> Error(Nil)
        False -> Ok(UnusedAntiPattern(ap.name))
      }
    })

  let mut_warnings = list.append(mut_warnings, unused_warnings)

  case list.is_empty(mut_warnings) {
    True -> LintValid
    False -> LintWarnings(mut_warnings)
  }
}

/// Check for anti-patterns in a behavior's response example
fn check_anti_patterns(
  behavior: Behavior,
  patterns: List(AntiPattern),
) -> List(LintWarning) {
  case behavior.response.example == json.null() {
    True -> []
    False ->
      patterns
      |> list.filter_map(fn(pattern) {
        case contains_anti_pattern_keys(behavior.response.example, pattern) {
          False -> Error(Nil)
          True ->
            Ok(
              AntiPatternDetected(
                behavior.name,
                pattern.name,
                "Response example contains keys from anti-pattern: " <> pattern.description,
              ),
            )
        }
      })
  }
}

/// Check if a JSON example contains the bad pattern keys
fn contains_anti_pattern_keys(example: Json, pattern: AntiPattern) -> Bool {
  let bad_keys = extract_all_keys(pattern.bad_example)
  let example_keys = extract_all_keys(example)

  // Check if any bad keys are in the example
  list.any(bad_keys, fn(key) { list.contains(example_keys, key) })
}

/// Extract all keys from a JSON object (recursively)
fn extract_all_keys(json: Json) -> List(String) {
  let json_str = json.to_string(json)

  case json.decode(json_str, dynamic.dict(dynamic.string, dynamic.dynamic)) {
    Ok(obj) -> {
      dict.keys(obj)
    }
    Error(_) -> []
  }
}

/// Check for vague rules in a behavior
fn check_for_vague_rules(behavior: Behavior) -> List(LintWarning) {
  behavior.response.checks
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    let #(field, check) = pair
    let rule_lower = string.lowercase(check.rule)

    let has_valid_keyword = string.contains(rule_lower, "valid")
    let has_email_keyword = string.contains(rule_lower, "email")
    let has_uuid_keyword = string.contains(rule_lower, "uuid")
    let has_iso_keyword = string.contains(rule_lower, "iso")
    let has_jwt_keyword = string.contains(rule_lower, "jwt")
    let has_uri_keyword = string.contains(rule_lower, "uri")
    let has_correct_format = string.contains(rule_lower, "correct format")
    let has_proper_format = string.contains(rule_lower, "proper format")

    let is_vague =
      {
        has_valid_keyword
        && !has_email_keyword
        && !has_uuid_keyword
        && !has_iso_keyword
        && !has_jwt_keyword
        && !has_uri_keyword
      }
      || has_correct_format
      || has_proper_format

    case is_vague {
      False -> Error(Nil)
      True ->
        Ok(
          VagueRule(
            behavior.name,
            field,
            check.rule <> " (too vague - be specific)",
          ),
        )
    }
  })
}

/// Check naming conventions for behaviors
fn check_naming_convention(behavior: Behavior) -> Result(LintWarning, Nil) {
  // Suggest kebab-case for behavior names
  case has_invalid_name_chars(behavior.name) {
    False -> Error(Nil)
    True ->
      Ok(
        NamingConvention(
          behavior.name,
          "Use kebab-case for behavior names (e.g., 'get-user-by-id')",
        ),
      )
  }
}

/// Check if a name has invalid characters (not alphanumeric, hyphen, underscore)
fn has_invalid_name_chars(name: String) -> Bool {
  string.to_graphemes(name)
  |> list.any(fn(c) {
    case string.contains("abcdefghijklmnopqrstuvwxyz0123456789-_", c) {
      True -> False
      False -> True
    }
  })
}

/// Format lint warnings for display
pub fn format_warnings(warnings: List(LintWarning)) -> String {
  let warning_lines =
    warnings
    |> list.map(format_warning)
    |> string.join("\n")

  "Linting found " <> int.to_string(list.length(warnings)) <> " warning(s):\n\n" <> warning_lines
}

/// Format a single lint warning
fn format_warning(warning: LintWarning) -> String {
  case warning {
    AntiPatternDetected(behavior, pattern, details) ->
      "Behavior '" <> behavior <> "':\n" <> "  Anti-pattern: " <> pattern <> "\n" <> "  " <> details

    VagueRule(behavior, field, rule) ->
      "Behavior '" <> behavior <> "', field '" <> field <> "':\n" <> "  " <> rule

    MissingExample(behavior) ->
      "Behavior '" <> behavior <> "':\n" <> "  Missing response example (helps AI understand intent)"

    UnusedAntiPattern(pattern) ->
      "Anti-pattern '" <> pattern <> "' is not tested by any behavior"

    NamingConvention(behavior, suggestion) ->
      "Behavior '" <> behavior <> "':\n" <> "  " <> suggestion
  }
}
