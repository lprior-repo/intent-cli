/// Header validation - validates HTTP response headers

import gleam/dict
import gleam/list
import gleam/string
import intent/checker/types.{type CheckResult, CheckFailed, CheckPassed}

/// Check a response header against expected value
pub fn check_header(
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
