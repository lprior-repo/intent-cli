/// Check/assertion engine for validating responses against rules

import gleam/dict
import gleam/dynamic
import gleam/float
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/string
import intent/formats
import intent/http_client.{type ExecutionResult}
import intent/interpolate.{type Context}
import intent/parser
import intent/rule.{type RuleExpr}
import intent/types.{type Check, type Response}

/// Result of checking a single field
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
        CheckPassed(_, _) -> True
        CheckFailed(_, _, _, _, _) -> False
      }
    })

  // Check expected headers
  let #(header_passed, header_failed) =
    expected.headers
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(header_name, expected_value) = pair
      check_header(header_name, expected_value, actual.headers)
    })
    |> list.partition(fn(result) {
      case result {
        CheckPassed(_, _) -> True
        CheckFailed(_, _, _, _, _) -> False
      }
    })

  ResponseCheckResult(
    passed: list.append(body_passed, header_passed),
    failed: list.append(body_failed, header_failed),
    status_ok: status_ok,
    status_expected: expected.status,
    status_actual: actual.status,
  )
}

/// Check a response header against expected value
fn check_header(
  header_name: String,
  expected_value: String,
  actual_headers: dict.Dict(String, String),
) -> CheckResult {
  let lower_name = string.lowercase(header_name)
  // Find the header (case-insensitive)
  let actual_value =
    actual_headers
    |> dict.to_list
    |> list.find(fn(pair) { string.lowercase(pair.0) == lower_name })

  case actual_value {
    Ok(#(_, value)) ->
      case value == expected_value {
        True -> CheckPassed("header:" <> header_name, "equals " <> expected_value)
        False ->
          CheckFailed(
            field: "header:" <> header_name,
            rule: "equals " <> expected_value,
            expected: expected_value,
            actual: value,
            explanation: "Header '"
              <> header_name
              <> "' expected '"
              <> expected_value
              <> "' but got '"
              <> value
              <> "'",
          )
      }
    Error(_) ->
      CheckFailed(
        field: "header:" <> header_name,
        rule: "present",
        expected: "header to be present",
        actual: "header missing",
        explanation: "Expected header '" <> header_name <> "' not found in response",
      )
  }
}

/// Check a single field against its rule
fn check_field(
  field: String,
  check: Check,
  body: Json,
  ctx: Context,
) -> CheckResult {
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

fn check_absent(field: String, body: Json) -> CheckResult {
  case get_field_value(body, field) {
    None -> CheckPassed(field, "absent")
    Some(_) ->
      CheckFailed(
        field: field,
        rule: "absent",
        expected: "field to be absent",
        actual: "field exists",
        explanation: "Field '" <> field <> "' should not be present in response",
      )
  }
}

fn check_present(field: String, body: Json) -> CheckResult {
  case get_field_value(body, field) {
    Some(_) -> CheckPassed(field, "present")
    None ->
      CheckFailed(
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
) -> CheckResult {
  let parsed = rule.parse(rule_str)

  case get_field_value(body, field) {
    None ->
      CheckFailed(
        field: field,
        rule: rule_str,
        expected: rule_str,
        actual: "field missing",
        explanation: "Field '" <> field <> "' not found in response",
      )
    Some(value) -> evaluate_rule(field, rule_str, parsed, value, ctx)
  }
}

fn evaluate_rule(
  field: String,
  rule_str: String,
  parsed: RuleExpr,
  value: Json,
  ctx: Context,
) -> CheckResult {
  let result = case parsed {
    rule.Equals(expected) -> check_equals_string(value, expected)
    rule.EqualsVariable(var_name) -> {
      case interpolate.get_variable(ctx, var_name) {
        Some(expected) -> check_equals_json(value, expected)
        None -> Error("Variable '" <> var_name <> "' not found")
      }
    }
    rule.EqualsInt(expected) -> check_equals_int(value, expected)
    rule.EqualsFloat(expected) -> check_equals_float(value, expected)
    rule.EqualsBool(expected) -> check_equals_bool(value, expected)

    rule.IsString -> check_is_string(value)
    rule.IsInteger -> check_is_integer(value)
    rule.IsNumber -> check_is_number(value)
    rule.IsBoolean -> check_is_boolean(value)
    rule.IsArray -> check_is_array(value)
    rule.IsObject -> check_is_object(value)
    rule.IsNull -> check_is_null(value)

    rule.NonEmptyString -> check_non_empty_string(value)
    rule.StringMatching(pattern) -> check_string_matching(value, pattern)
    rule.StringStartingWith(prefix) -> check_string_starting_with(value, prefix)
    rule.StringEndingWith(suffix) -> check_string_ending_with(value, suffix)
    rule.StringContaining(substring) ->
      check_string_containing(value, substring)

    rule.IsEmail -> check_is_email(value)
    rule.IsUuid -> check_is_uuid(value)
    rule.IsUri -> check_is_uri(value)
    rule.IsJwt -> check_is_jwt(value)
    rule.IsIso8601 -> check_is_iso8601(value)
    rule.ValidJwt -> check_valid_jwt(value)
    rule.ValidIso8601 -> check_is_iso8601(value)

    rule.IntegerGte(n) -> check_integer_gte(value, n)
    rule.IntegerGt(n) -> check_integer_gt(value, n)
    rule.IntegerLte(n) -> check_integer_lte(value, n)
    rule.IntegerLt(n) -> check_integer_lt(value, n)
    rule.IntegerBetween(low, high) -> check_integer_between(value, low, high)
    rule.NumberBetween(low, high) -> check_number_between(value, low, high)

    rule.NotNull -> check_not_null(value)
    rule.NonEmptyArray -> check_non_empty_array(value)
    rule.ArrayOfLength(n) -> check_array_of_length(value, n)
    rule.ArrayWithMinItems(n) -> check_array_min_items(value, n)
    rule.ArrayWithMaxItems(n) -> check_array_max_items(value, n)

    rule.OneOf(options) -> check_one_of(value, options)

    rule.ContainsVariable(var_name) -> {
      case interpolate.get_variable(ctx, var_name) {
        Some(expected) -> check_string_contains_json(value, expected)
        None -> Error("Variable '" <> var_name <> "' not found")
      }
    }

    rule.ArrayWhereEach(inner_rule) ->
      check_array_where_each(value, inner_rule, ctx)

    rule.Raw(raw) -> {
      // Handle "equals X" that wasn't parsed (for interpolated values)
      case string.starts_with(raw, "equals ") {
        True -> {
          let expected = string.drop_left(raw, 7)
          check_equals_string(value, expected)
        }
        False -> Error("Unknown rule: " <> raw)
      }
    }

    _ -> Error("Rule not implemented: " <> rule.to_string(parsed))
  }

  case result {
    Ok(_) -> CheckPassed(field, rule_str)
    Error(explanation) ->
      CheckFailed(
        field: field,
        rule: rule_str,
        expected: rule_str,
        actual: json_to_display_string(value),
        explanation: explanation,
      )
  }
}

/// Get a field value from JSON using dot notation
fn get_field_value(body: Json, field: String) -> Option(Json) {
  navigate_json_path(body, string.split(field, "."))
}

fn navigate_json_path(value: Json, path: List(String)) -> Option(Json) {
  case path {
    [] -> Some(value)
    [key, ..rest] -> {
      // Need to decode the JSON to navigate it
      let json_str = json.to_string(value)
      case json.decode(json_str, dynamic.dict(dynamic.string, dynamic.dynamic)) {
        Ok(obj) ->
          case dict.get(obj, key) {
            Ok(next) -> {
              let next_json = parser.dynamic_to_json(next)
              navigate_json_path(next_json, rest)
            }
            Error(_) -> None
          }
        Error(_) -> None
      }
    }
  }
}

// Individual check functions

fn check_equals_string(value: Json, expected: String) -> Result(Nil, String) {
  let actual = json_to_raw_string(value)
  case actual == expected {
    True -> Ok(Nil)
    False -> Error("Expected '" <> expected <> "' but got '" <> actual <> "'")
  }
}

fn check_equals_json(value: Json, expected: Json) -> Result(Nil, String) {
  let actual_str = json.to_string(value)
  let expected_str = json.to_string(expected)
  case actual_str == expected_str {
    True -> Ok(Nil)
    False ->
      Error("Expected " <> expected_str <> " but got " <> actual_str)
  }
}

fn check_equals_int(value: Json, expected: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual == expected {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected "
            <> int.to_string(expected)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_equals_float(value: Json, expected: Float) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.float) {
    Ok(actual) ->
      case actual == expected {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected "
            <> float.to_string(expected)
            <> " but got "
            <> float.to_string(actual),
          )
      }
    Error(_) -> Error("Expected float but got " <> json.to_string(value))
  }
}

fn check_equals_bool(value: Json, expected: Bool) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.bool) {
    Ok(actual) ->
      case actual == expected {
        True -> Ok(Nil)
        False -> {
          let expected_str = case expected {
            True -> "true"
            False -> "false"
          }
          let actual_str = case actual {
            True -> "true"
            False -> "false"
          }
          Error("Expected " <> expected_str <> " but got " <> actual_str)
        }
      }
    Error(_) -> Error("Expected boolean but got " <> json.to_string(value))
  }
}

fn check_is_string(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_integer(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_is_number(value: Json) -> Result(Nil, String) {
  let json_str = json.to_string(value)
  case json.decode(json_str, dynamic.int) {
    Ok(_) -> Ok(Nil)
    Error(_) ->
      case json.decode(json_str, dynamic.float) {
        Ok(_) -> Ok(Nil)
        Error(_) -> Error("Expected number but got " <> json_str)
      }
  }
}

fn check_is_boolean(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.bool) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected boolean but got " <> json.to_string(value))
  }
}

fn check_is_array(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_is_object(value: Json) -> Result(Nil, String) {
  case
    json.decode(json.to_string(value), dynamic.dict(dynamic.string, dynamic.dynamic))
  {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected object but got " <> json.to_string(value))
  }
}

fn check_is_null(value: Json) -> Result(Nil, String) {
  case json.to_string(value) {
    "null" -> Ok(Nil)
    other -> Error("Expected null but got " <> other)
  }
}

fn check_non_empty_string(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.is_empty(s) {
        True -> Error("Expected non-empty string but got empty string")
        False -> Ok(Nil)
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_matching(value: Json, pattern: String) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case get_or_compile_regex(pattern) {
        Ok(re) ->
          case regexp.check(re, s) {
            True -> Ok(Nil)
            False ->
              Error("String '" <> s <> "' does not match pattern /" <> pattern <> "/")
          }
        Error(_) -> Error("Invalid regex pattern: " <> pattern)
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_starting_with(
  value: Json,
  prefix: String,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.starts_with(s, prefix) {
        True -> Ok(Nil)
        False ->
          Error("String '" <> s <> "' does not start with '" <> prefix <> "'")
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_ending_with(value: Json, suffix: String) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.ends_with(s, suffix) {
        True -> Ok(Nil)
        False ->
          Error("String '" <> s <> "' does not end with '" <> suffix <> "'")
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_containing(
  value: Json,
  substring: String,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.contains(s, substring) {
        True -> Ok(Nil)
        False ->
          Error("String '" <> s <> "' does not contain '" <> substring <> "'")
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_email(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> formats.validate_email(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_uuid(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> formats.validate_uuid(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_uri(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> formats.validate_uri(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_jwt(value: Json) -> Result(Nil, String) {
  check_valid_jwt(value)
}

fn check_valid_jwt(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> {
      // JWT has 3 parts separated by dots: header.payload.signature
      let parts = string.split(s, ".")
      case parts {
        [header, payload, _signature] -> {
          // Validate header is valid Base64URL-encoded JSON with "alg" field
          case validate_jwt_part(header, "header") {
            Ok(_) ->
              case validate_jwt_header_has_alg(header) {
                Ok(_) ->
                  // Validate payload is valid Base64URL-encoded JSON
                  validate_jwt_part(payload, "payload")
                Error(e) -> Error(e)
              }
            Error(e) -> Error(e)
          }
        }
        _ ->
          Error(
            "'"
            <> s
            <> "' is not a valid JWT (expected 3 dot-separated parts)",
          )
      }
    }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn validate_jwt_part(part: String, name: String) -> Result(Nil, String) {
  case base64_url_decode(part) {
    Ok(decoded) ->
      // Check if it's valid JSON
      case json.decode(decoded, dynamic.dynamic) {
        Ok(_) -> Ok(Nil)
        Error(_) ->
          Error("JWT " <> name <> " is not valid JSON after Base64 decoding")
      }
    Error(_) -> Error("JWT " <> name <> " is not valid Base64URL encoding")
  }
}

fn validate_jwt_header_has_alg(header: String) -> Result(Nil, String) {
  case base64_url_decode(header) {
    Ok(decoded) ->
      case json.decode(decoded, dynamic.dict(dynamic.string, dynamic.dynamic)) {
        Ok(obj) ->
          case dict.has_key(obj, "alg") {
            True -> Ok(Nil)
            False -> Error("JWT header missing required 'alg' field")
          }
        Error(_) -> Error("JWT header is not a valid JSON object")
      }
    Error(_) -> Error("JWT header is not valid Base64URL encoding")
  }
}

@external(erlang, "intent_ffi", "base64_url_decode")
fn base64_url_decode_ffi(input: String) -> Result(String, String)

fn base64_url_decode(input: String) -> Result(String, String) {
  base64_url_decode_ffi(input)
}

fn check_is_iso8601(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> formats.validate_iso8601(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_integer_gte(value: Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual >= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer >= "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_gt(value: Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual > n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer > "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_lte(value: Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual <= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer <= "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_lt(value: Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual < n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer < "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_between(value: Json, low: Int, high: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual >= low && actual <= high {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer between "
            <> int.to_string(low)
            <> " and "
            <> int.to_string(high)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_number_between(
  value: Json,
  low: Float,
  high: Float,
) -> Result(Nil, String) {
  let json_str = json.to_string(value)
  case json.decode(json_str, dynamic.float) {
    Ok(actual) ->
      case actual >=. low && actual <=. high {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected number between "
            <> float.to_string(low)
            <> " and "
            <> float.to_string(high)
            <> " but got "
            <> float.to_string(actual),
          )
      }
    Error(_) ->
      case json.decode(json_str, dynamic.int) {
        Ok(i) -> {
          let actual = int.to_float(i)
          case actual >=. low && actual <=. high {
            True -> Ok(Nil)
            False ->
              Error(
                "Expected number between "
                <> float.to_string(low)
                <> " and "
                <> float.to_string(high)
                <> " but got "
                <> float.to_string(actual),
              )
          }
        }
        Error(_) -> Error("Expected number but got " <> json_str)
      }
  }
}

fn check_not_null(value: Json) -> Result(Nil, String) {
  case json.to_string(value) {
    "null" -> Error("Expected non-null value but got null")
    _ -> Ok(Nil)
  }
}

fn check_non_empty_array(value: Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) ->
      case list.is_empty(arr) {
        True -> Error("Expected non-empty array but got empty array")
        False -> Ok(Nil)
      }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_array_of_length(value: Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) -> {
      let actual_len = list.length(arr)
      case actual_len == n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected array of length "
            <> int.to_string(n)
            <> " but got length "
            <> int.to_string(actual_len),
          )
      }
    }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_array_min_items(value: Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) -> {
      let actual_len = list.length(arr)
      case actual_len >= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected array with at least "
            <> int.to_string(n)
            <> " items but got "
            <> int.to_string(actual_len),
          )
      }
    }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_array_max_items(value: Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) -> {
      let actual_len = list.length(arr)
      case actual_len <= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected array with at most "
            <> int.to_string(n)
            <> " items but got "
            <> int.to_string(actual_len),
          )
      }
    }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_one_of(value: Json, options: List(String)) -> Result(Nil, String) {
  let actual = json_to_raw_string(value)
  case list.contains(options, actual) {
    True -> Ok(Nil)
    False ->
      Error(
        "Expected one of ["
        <> string.join(options, ", ")
        <> "] but got '"
        <> actual
        <> "'",
      )
  }
}

fn check_string_contains_json(
  value: Json,
  expected: Json,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> {
      let expected_str = json_to_raw_string(expected)
      case string.contains(s, expected_str) {
        True -> Ok(Nil)
        False ->
          Error(
            "String '"
            <> s
            <> "' does not contain '"
            <> expected_str
            <> "'",
          )
      }
    }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_array_where_each(
  value: Json,
  inner_rule: rule.RuleExpr,
  ctx: Context,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(items) -> {
      let failures =
        items
        |> list.index_map(fn(item, idx) {
          let item_json = parser.dynamic_to_json(item)
          let result = case inner_rule {
            rule.IsString -> check_is_string(item_json)
            rule.IsInteger -> check_is_integer(item_json)
            rule.IsNumber -> check_is_number(item_json)
            rule.IsBoolean -> check_is_boolean(item_json)
            rule.IsObject -> check_is_object(item_json)
            rule.StringMatching(pattern) ->
              check_string_matching(item_json, pattern)
            rule.IsEmail -> check_is_email(item_json)
            rule.IsUuid -> check_is_uuid(item_json)
            _ -> {
              // For complex rules, recursively evaluate
              case
                evaluate_rule(
                  "item[" <> int.to_string(idx) <> "]",
                  rule.to_string(inner_rule),
                  inner_rule,
                  item_json,
                  ctx,
                )
              {
                CheckPassed(_, _) -> Ok(Nil)
                CheckFailed(_, _, _, _, explanation) -> Error(explanation)
              }
            }
          }
          case result {
            Ok(_) -> None
            Error(msg) -> Some(#(idx, msg))
          }
        })
        |> list.filter_map(fn(opt) {
          case opt {
            Some(pair) -> Ok(pair)
            None -> Error(Nil)
          }
        })

      case failures {
        [] -> Ok(Nil)
        [#(idx, msg), ..] ->
          Error(
            "Array item at index "
            <> int.to_string(idx)
            <> " failed: "
            <> msg,
          )
      }
    }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

// Helper functions

fn json_to_raw_string(value: Json) -> String {
  let encoded = json.to_string(value)
  // Remove quotes from strings
  case string.starts_with(encoded, "\"") && string.ends_with(encoded, "\"") {
    True ->
      encoded
      |> string.drop_left(1)
      |> string.drop_right(1)
    False -> encoded
  }
}

fn json_to_display_string(value: Json) -> String {
  json.to_string(value)
}

/// Get or compile a regex pattern with Erlang-level caching
/// Uses ETS table for caching compiled patterns across calls
@external(erlang, "intent_checker", "get_or_compile_regex")
fn get_or_compile_regex(pattern: String) -> Result(regexp.Regexp, Nil)
