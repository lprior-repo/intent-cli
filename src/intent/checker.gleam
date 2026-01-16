/// Check/assertion engine for validating responses against rules
///
/// This module provides the public API for response checking.
/// Internal implementation is split into sub-modules:
/// - checker/types: Shared type definitions
/// - checker/json: JSON field access and helpers
/// - checker/headers: HTTP header checking
/// - checker/rules: Rule expression evaluation
import gleam/dict
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import intent/checker/headers as header_validation
import intent/checker/json as field_validation
import intent/checker/rules as rule_evaluation
import intent/checker/types as checker_types
import intent/http_client.{type ExecutionResult}
import intent/interpolate.{type Context}
import intent/rule
import intent/types.{type Check, type Response}

/// Result of checking a single field
/// Re-exported from checker/types
pub type CheckResult {
  CheckPassed(field: String, rule: String)
  CheckFailed(
    field: String,
    rule: String,
    expected: String,
    actual: String,
    explanation: String,
  )
}

/// Result of checking all fields in a response
/// Re-exported from checker/types
pub type ResponseCheckResult {
  ResponseCheckResult(
    passed: List(CheckResult),
    failed: List(CheckResult),
    status_ok: Bool,
    status_expected: Int,
    status_actual: Int,
  )
}

/// Check an execution result against expected response
pub fn check_response(
  expected: Response,
  actual: ExecutionResult,
  ctx: Context,
) -> ResponseCheckResult {
  // Check status code
  let status_ok = actual.status == expected.status

  // Check all field rules
  let #(body_passed, body_failed) =
    expected.checks
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(field, check) = pair
      check_field(field, check, actual.body, ctx)
    })
    |> list.partition(fn(result) {
      case result {
        checker_types.CheckPassed(_, _) -> True
        checker_types.CheckFailed(_, _, _, _, _) -> False
      }
    })

  // Check expected headers
  let #(header_passed, header_failed) =
    expected.headers
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(header_name, expected_value) = pair
      header_validation.check_header(
        header_name,
        expected_value,
        actual.headers,
      )
    })
    |> list.partition(fn(result) {
      case result {
        checker_types.CheckPassed(_, _) -> True
        checker_types.CheckFailed(_, _, _, _, _) -> False
      }
    })

  // Convert internal types to public types
  let passed =
    list.append(body_passed, header_passed)
    |> list.map(convert_check_result)
  let failed =
    list.append(body_failed, header_failed)
    |> list.map(convert_check_result)

  ResponseCheckResult(
    passed: passed,
    failed: failed,
    status_ok: status_ok,
    status_expected: expected.status,
    status_actual: actual.status,
  )
}

/// Convert internal CheckResult to public CheckResult
fn convert_check_result(result: checker_types.CheckResult) -> CheckResult {
  case result {
    checker_types.CheckPassed(field, rule) -> CheckPassed(field, rule)
    checker_types.CheckFailed(field, rule, expected, actual, explanation) ->
      CheckFailed(field, rule, expected, actual, explanation)
  }
}

/// Check a single field against its rule
fn check_field(
  field: String,
  check: Check,
  body: Json,
  ctx: Context,
) -> checker_types.CheckResult {
  let parsed_rule = rule.parse(check.rule)

  // Handle interpolated rules first
  let interpolated_rule = interpolate_rule(check.rule, ctx)

  case parsed_rule {
    rule.Absent -> check_absent(field, body)
    rule.Present -> check_present(field, body)
    _ -> check_rule(field, interpolated_rule, body, ctx)
  }
}

fn interpolate_rule(rule_str: String, ctx: Context) -> String {
  case interpolate.interpolate_string(ctx, rule_str) {
    Ok(interpolated) -> interpolated
    Error(_) -> rule_str
  }
}

fn check_absent(field: String, body: Json) -> checker_types.CheckResult {
  case field_validation.get_field_value(body, field) {
    None -> checker_types.CheckPassed(field, "absent")
    Some(_) ->
      checker_types.CheckFailed(
        field: field,
        rule: "absent",
        expected: "field to be absent",
        actual: "field exists",
        explanation: "Field '" <> field <> "' should not be present in response",
      )
  }
}

fn check_present(field: String, body: Json) -> checker_types.CheckResult {
  case field_validation.get_field_value(body, field) {
    Some(_) -> checker_types.CheckPassed(field, "present")
    None ->
      checker_types.CheckFailed(
        field: field,
        rule: "present",
        expected: "field to be present",
        actual: "field missing",
        explanation: "Field '" <> field <> "' must be present in response",
      )
  }
}

fn check_rule(
  field: String,
  rule_str: String,
  body: Json,
  ctx: Context,
) -> checker_types.CheckResult {
  let parsed = rule.parse(rule_str)

  case field_validation.get_field_value(body, field) {
    None ->
      checker_types.CheckFailed(
        field: field,
        rule: rule_str,
        expected: rule_str,
        actual: "field missing",
        explanation: "Field '" <> field <> "' not found in response",
      )
    Some(value) ->
      rule_evaluation.evaluate_rule(field, rule_str, parsed, value, ctx)
  }
}
