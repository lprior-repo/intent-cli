/// Anti-pattern detection engine
/// Checks responses for known anti-patterns defined in the spec
import gleam/dict
import gleam/dynamic
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import intent/http_client.{type ExecutionResult}
import intent/types.{type AntiPattern}

/// Result of checking anti-patterns
pub type AntiPatternResult {
  NoAntiPatterns
  AntiPatternDetected(
    pattern_name: String,
    description: String,
    found: String,
    bad_example: Json,
    good_example: Json,
  )
}

/// Check a response for all defined anti-patterns
pub fn check_anti_patterns(
  patterns: List(AntiPattern),
  response: ExecutionResult,
  _behavior_name: String,
) -> List(AntiPatternResult) {
  patterns
  |> list.filter_map(fn(pattern) {
    case detect_pattern(pattern, response) {
      Some(result) -> Ok(result)
      None -> Error(Nil)
    }
  })
}

/// Detect a single anti-pattern in a response
fn detect_pattern(
  pattern: AntiPattern,
  response: ExecutionResult,
) -> Option(AntiPatternResult) {
  // Get the keys/structure from the bad example that shouldn't be present
  let bad_keys = extract_problem_keys(pattern)

  // Check if any of these keys are in the response
  let found_problems = check_for_bad_keys(response.body, bad_keys)

  case list.is_empty(found_problems) {
    True -> None
    False ->
      Some(AntiPatternDetected(
        pattern_name: pattern.name,
        description: pattern.description,
        found: "Response contains: " <> string.join(found_problems, ", "),
        bad_example: pattern.bad_example,
        good_example: pattern.good_example,
      ))
  }
}

/// Extract the keys from bad_example that make it bad
/// (keys present in bad but not in good)
fn extract_problem_keys(pattern: AntiPattern) -> Set(String) {
  let bad_keys = get_all_keys(pattern.bad_example)
  let good_keys = get_all_keys(pattern.good_example)

  // Keys that are in bad but not in good are the problem keys
  set.filter(bad_keys, fn(key) { !set.contains(good_keys, key) })
}

/// Get all keys from a JSON object (recursively)
fn get_all_keys(value: Json) -> Set(String) {
  let json_str = json.to_string(value)
  case json.decode(json_str, dynamic.dict(dynamic.string, dynamic.dynamic)) {
    Ok(obj) -> {
      obj
      |> dict.to_list
      |> list.flat_map(fn(pair) {
        let #(key, val) = pair
        let nested = get_all_keys_from_dynamic(val)
        [key, ..set.to_list(nested)]
      })
      |> set.from_list
    }
    Error(_) -> set.new()
  }
}

fn get_all_keys_from_dynamic(data: dynamic.Dynamic) -> Set(String) {
  case dynamic.dict(dynamic.string, dynamic.dynamic)(data) {
    Ok(obj) -> {
      obj
      |> dict.to_list
      |> list.flat_map(fn(pair) {
        let #(key, val) = pair
        let nested = get_all_keys_from_dynamic(val)
        [key, ..set.to_list(nested)]
      })
      |> set.from_list
    }
    Error(_) -> set.new()
  }
}

/// Check if any bad keys are present in the response
fn check_for_bad_keys(body: Json, bad_keys: Set(String)) -> List(String) {
  let response_keys = get_all_keys(body)

  bad_keys
  |> set.to_list
  |> list.filter(fn(key) { set.contains(response_keys, key) })
}

/// Format an anti-pattern result as a human-readable string
pub fn format_anti_pattern(result: AntiPatternResult) -> String {
  case result {
    NoAntiPatterns -> "No anti-patterns detected"
    AntiPatternDetected(name, description, found, bad, good) ->
      "Anti-pattern detected: "
      <> name
      <> "\n"
      <> "Description: "
      <> description
      <> "\n"
      <> "Found: "
      <> found
      <> "\n"
      <> "Bad example: "
      <> json.to_string(bad)
      <> "\n"
      <> "Good example: "
      <> json.to_string(good)
  }
}
