/// Header validation - validates HTTP response headers
import gleam/dict
import gleam/list
import gleam/string
import intent/checker/types.{type CheckResult, CheckFailed, CheckPassed}

/// Cache of lowercased headers for O(1) case-insensitive lookups
/// Created once per response, reused for all header checks
fn lowercase_headers(
  headers: dict.Dict(String, String),
) -> dict.Dict(String, String) {
  headers
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(key, value) = pair
    #(string.lowercase(key), value)
  })
  |> dict.from_list
}

/// Check a response header against expected value
/// Uses cached lowercase headers for O(1) case-insensitive lookup
pub fn check_header(
  header_name: String,
  expected_value: String,
  actual_headers: dict.Dict(String, String),
) -> CheckResult {
  let lower_name = string.lowercase(header_name)
  let cached = lowercase_headers(actual_headers)

  case dict.get(cached, lower_name) {
    Ok(actual_value) ->
      case actual_value == expected_value {
        True ->
          CheckPassed("header:" <> header_name, "equals " <> expected_value)
        False ->
          CheckFailed(
            field: "header:" <> header_name,
            rule: "equals " <> expected_value,
            expected: expected_value,
            actual: actual_value,
            explanation: "Header '"
              <> header_name
              <> "' expected '"
              <> expected_value
              <> "' but got '"
              <> actual_value
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
