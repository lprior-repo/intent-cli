/// Header validation - validates HTTP response headers
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import intent/checker/types.{type CheckResult, CheckFailed, CheckPassed}

/// Pre-compute a lowercase header index for O(1) lookups
/// Maps lowercase header name -> original value
pub fn build_header_index(headers: Dict(String, String)) -> Dict(String, String) {
  headers
  |> dict.to_list
  |> list.map(fn(pair) { #(string.lowercase(pair.0), pair.1) })
  |> dict.from_list
}

/// Check a response header against expected value using pre-computed index
pub fn check_header_with_index(
  header_name: String,
  expected_value: String,
  header_index: Dict(String, String),
) -> CheckResult {
  let lower_name = string.lowercase(header_name)

  case dict.get(header_index, lower_name) {
    Ok(value) ->
      case value == expected_value {
        True ->
          CheckPassed("header:" <> header_name, "equals " <> expected_value)
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
        explanation: "Expected header '"
          <> header_name
          <> "' not found in response",
      )
  }
}

/// Check a response header against expected value (builds index per call - use check_header_with_index for multiple checks)
pub fn check_header(
  header_name: String,
  expected_value: String,
  actual_headers: Dict(String, String),
) -> CheckResult {
  let header_index = build_header_index(actual_headers)
  check_header_with_index(header_name, expected_value, header_index)
}
