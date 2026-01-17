/// Global rules engine for checking responses against spec-wide rules

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/regexp
import gleam/string
import intent/http_client.{type ExecutionResult}
import intent/parser
import intent/types.{type Rule, type RuleCheck, type When}

/// Result of checking a global rule
pub type RuleResult {
  RulePassed(rule_name: String)
  RuleFailed(
    rule_name: String,
    description: String,
    violations: List(RuleViolation),
  )
}

/// A single violation of a rule
pub type RuleViolation {
  BodyContains(forbidden: String, found_in: String)
  BodyMissing(required: String)
  FieldMissing(field: String)
  FieldPresent(field: String)
  HeaderMissing(header: String)
  HeaderPresent(header: String)
}

/// Check all global rules against a response
pub fn check_rules(
  rules: List(Rule),
  response: ExecutionResult,
  behavior_name: String,
) -> List(RuleResult) {
  rules
  |> list.filter(fn(rule) { rule_applies(rule, response) })
  |> list.map(fn(rule) { check_rule(rule, response, behavior_name) })
}

/// Check if a rule applies based on its `when` conditions
fn rule_applies(rule: Rule, response: ExecutionResult) -> Bool {
  check_when_conditions(rule.when, response)
}

fn check_when_conditions(when: When, response: ExecutionResult) -> Bool {
  // Check status condition
  let status_ok =
    check_status_condition(when.status, response.status)

  // Check method condition
  let method_ok = response.request_method == when.method

  // Check path condition
  let path_ok = check_path_pattern(when.path, response.request_path)

  // All conditions must pass
  status_ok && method_ok && path_ok
}

fn check_path_pattern(pattern: String, path: String) -> Bool {
  // Try exact match first
  case pattern == path {
    True -> True
    False -> {
      // Try regex match
      case regexp.from_string(pattern) {
        Ok(re) -> regexp.check(re, path)
        Error(_) -> False
      }
    }
  }
}

fn check_status_condition(expr: String, status: Int) -> Bool {
  let expr = string.trim(expr)

  case string.starts_with(expr, ">= ") {
    True -> {
      let n_str = string.drop_left(expr, 3)
      case int.parse(n_str) {
        Ok(n) -> status >= n
        Error(_) -> False
      }
    }
    False ->
      case string.starts_with(expr, "> ") {
        True -> {
          let n_str = string.drop_left(expr, 2)
          case int.parse(n_str) {
            Ok(n) -> status > n
            Error(_) -> False
          }
        }
        False ->
          case string.starts_with(expr, "<= ") {
            True -> {
              let n_str = string.drop_left(expr, 3)
              case int.parse(n_str) {
                Ok(n) -> status <= n
                Error(_) -> False
              }
            }
            False ->
              case string.starts_with(expr, "< ") {
                True -> {
                  let n_str = string.drop_left(expr, 2)
                  case int.parse(n_str) {
                    Ok(n) -> status < n
                    Error(_) -> False
                  }
                }
                False ->
                  case string.starts_with(expr, "== ") {
                    True -> {
                      let n_str = string.drop_left(expr, 3)
                      case int.parse(n_str) {
                        Ok(n) -> status == n
                        Error(_) -> False
                      }
                    }
                    False ->
                      case int.parse(expr) {
                        Ok(n) -> status == n
                        Error(_) -> False
                      }
                  }
              }
          }
      }
  }
}

/// Check a single rule against a response
fn check_rule(
  rule: Rule,
  response: ExecutionResult,
  _behavior_name: String,
) -> RuleResult {
  let violations = collect_violations(rule.check, response)

  case list.is_empty(violations) {
    True -> RulePassed(rule.name)
    False -> RuleFailed(rule.name, rule.description, violations)
  }
}

fn collect_violations(
  check: RuleCheck,
  response: ExecutionResult,
) -> List(RuleViolation) {
  let violations = []

  // Check body_must_not_contain
  let violations =
    list.fold(
      check.body_must_not_contain,
      violations,
      fn(acc, forbidden) {
        case contains_string(response.raw_body, forbidden) {
          True -> [BodyContains(forbidden, "response body"), ..acc]
          False -> acc
        }
      },
    )

  // Check body_must_contain
  let violations =
    list.fold(
      check.body_must_contain,
      violations,
      fn(acc, required) {
        case contains_string(response.raw_body, required) {
          True -> acc
          False -> [BodyMissing(required), ..acc]
        }
      },
    )

  // Check fields_must_exist
  let violations =
    list.fold(
      check.fields_must_exist,
      violations,
      fn(acc, field) {
        case field_exists(response.body, field) {
          True -> acc
          False -> [FieldMissing(field), ..acc]
        }
      },
    )

  // Check fields_must_not_exist
  let violations =
    list.fold(
      check.fields_must_not_exist,
      violations,
      fn(acc, field) {
        case field_exists(response.body, field) {
          True -> [FieldPresent(field), ..acc]
          False -> acc
        }
      },
    )

  // Check header_must_exist
  let violations = case check.header_must_exist {
    "" -> violations
    required_header ->
      case header_exists(response.headers, required_header) {
        True -> violations
        False -> [HeaderMissing(required_header), ..violations]
      }
  }

  // Check header_must_not_exist
  let violations = case check.header_must_not_exist {
    "" -> violations
    forbidden_header ->
      case header_exists(response.headers, forbidden_header) {
        True -> [HeaderPresent(forbidden_header), ..violations]
        False -> violations
      }
  }

  violations
}

fn contains_string(body: String, needle: String) -> Bool {
  string.contains(string.lowercase(body), string.lowercase(needle))
}

fn field_exists(body: Json, field_path: String) -> Bool {
  let parts = string.split(field_path, ".")
  navigate_and_check(body, parts)
}

fn navigate_and_check(value: Json, path: List(String)) -> Bool {
  case path {
    [] -> True
    [key, ..rest] -> {
      let json_str = json.to_string(value)
      case
        json.decode(json_str, dynamic.dict(dynamic.string, dynamic.dynamic))
      {
        Ok(obj) ->
          case dict.get(obj, key) {
            Ok(next) -> {
              let next_json = parser.dynamic_to_json(next)
              navigate_and_check(next_json, rest)
            }
            Error(_) -> False
          }
        Error(_) -> False
      }
    }
  }
}

fn header_exists(headers: Dict(String, String), header_name: String) -> Bool {
  let lower_name = string.lowercase(header_name)
  headers
  |> dict.to_list
  |> list.any(fn(pair) { string.lowercase(pair.0) == lower_name })
}

/// Format a rule violation as a human-readable string
pub fn format_violation(violation: RuleViolation) -> String {
  case violation {
    BodyContains(forbidden, location) ->
      "Found forbidden string '" <> forbidden <> "' in " <> location
    BodyMissing(required) ->
      "Required string '" <> required <> "' not found in response body"
    FieldMissing(field) -> "Required field '" <> field <> "' not found"
    FieldPresent(field) ->
      "Forbidden field '" <> field <> "' is present in response"
    HeaderMissing(header) -> "Required header '" <> header <> "' not found"
    HeaderPresent(header) ->
      "Forbidden header '" <> header <> "' is present in response"
  }
}
