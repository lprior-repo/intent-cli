//// Test coverage for intent/kirk/compact_format.gleam
////
//// Tests token-efficient compact format conversion:
//// - Spec to CompactSpec conversion
//// - Compact format text generation
//// - Token estimation and comparison
//// - Prototext generation
//// - Grammar compliance (SPEC/F/B/R/A prefix syntax)
////
//// DbC Postconditions Verified:
//// - CompactSpec has same number of features/behaviors as original
//// - format_compact output starts with "SPEC "
//// - estimate_tokens returns length/4
//// - compare_token_usage returns valid #(full, compact, savings%)
//// - Roundtrip spec → compact → string is deterministic

import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/compact_format
import intent/types.{Get, Post, Put}
import test_helpers

// =============================================================================
// EMPTY SPEC TESTS (Baseline)
// =============================================================================

pub fn spec_to_compact_empty_test() {
  // GIVEN: An empty spec with no features
  let spec = test_helpers.make_test_spec([])

  // WHEN: Converting to compact format
  let compact = compact_format.spec_to_compact(spec)

  // THEN: CompactSpec has same structure
  compact.name
  |> should.equal("Test Spec")

  compact.version
  |> should.equal("1.0.0")

  // THEN: Features list is empty
  list.is_empty(compact.features)
  |> should.be_true()
}

pub fn format_compact_empty_spec_test() {
  // GIVEN: An empty compact spec
  let spec = test_helpers.make_test_spec([])
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: DbC postcondition - output starts with "SPEC "
  string.starts_with(output, "SPEC ")
  |> should.be_true()

  // THEN: Contains spec name
  string.contains(output, "Test Spec")
  |> should.be_true()
}

// =============================================================================
// BEHAVIOR CONVERSION TESTS
// =============================================================================

pub fn spec_to_compact_behavior_test() {
  // GIVEN: A spec with one behavior
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Converting to compact
  let compact = compact_format.spec_to_compact(spec)

  // THEN: DbC postcondition - same number of features
  list.length(compact.features)
  |> should.equal(1)

  // THEN: DbC postcondition - same number of behaviors
  case list.first(compact.features) {
    Ok(feature) ->
      list.length(feature.behaviors)
      |> should.equal(1)
    Error(_) -> should.fail()
  }
}

pub fn spec_to_compact_method_string_test() {
  // GIVEN: Behaviors with different HTTP methods
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("update-user", Put, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Converting to compact
  let compact = compact_format.spec_to_compact(spec)

  // THEN: Request strings have correct method prefix
  case list.first(compact.features) {
    Ok(feature) -> {
      let requests = list.map(feature.behaviors, fn(b) { b.request })

      // THEN: GET request starts with "GET "
      case list.first(requests) {
        Ok(req) ->
          string.starts_with(req, "GET ")
          |> should.be_true()
        Error(_) -> should.fail()
      }

      // THEN: POST request starts with "POST "
      case list.drop(requests, 1) |> list.first() {
        Ok(req) ->
          string.starts_with(req, "POST ")
          |> should.be_true()
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn spec_to_compact_request_includes_path_test() {
  // GIVEN: A behavior with specific path
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Converting to compact
  let compact = compact_format.spec_to_compact(spec)

  // THEN: Request string contains path
  case list.first(compact.features) {
    Ok(feature) ->
      case list.first(feature.behaviors) {
        Ok(behavior) ->
          string.contains(behavior.request, "/get-user")
          |> should.be_true()
        Error(_) -> should.fail()
      }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// DEPENDENCY (REQUIRES) TESTS
// =============================================================================

pub fn spec_to_compact_with_requires_test() {
  // GIVEN: A behavior with dependencies
  let behaviors = [
    test_helpers.make_test_behavior("login", []),
    test_helpers.make_test_behavior("get-profile", ["login"]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Converting to compact
  let compact = compact_format.spec_to_compact(spec)

  // THEN: Dependencies are preserved
  case list.first(compact.features) {
    Ok(feature) ->
      case list.drop(feature.behaviors, 1) |> list.first() {
        Ok(behavior) ->
          list.contains(behavior.requires, "login")
          |> should.be_true()
        Error(_) -> should.fail()
      }
    Error(_) -> should.fail()
  }
}

pub fn format_compact_with_requires_test() {
  // GIVEN: A behavior with dependencies
  let behaviors = [
    test_helpers.make_test_behavior("login", []),
    test_helpers.make_test_behavior("get-profile", ["login", "auth"]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: Output contains dependency marker "<-"
  string.contains(output, "<-")
  |> should.be_true()

  // THEN: Output contains dependency list
  string.contains(output, "login")
  |> should.be_true()
}

// =============================================================================
// CAPTURES TESTS
// =============================================================================

pub fn spec_to_compact_with_captures_test() {
  // GIVEN: A behavior with captures
  let behavior = test_helpers.make_test_behavior("create-user", [])
  let behavior_with_captures =
    types.Behavior(
      ..behavior,
      captures: dict.from_list([
        #("user_id", "response.body.id"),
        #("token", "response.headers.auth-token"),
      ]),
    )

  let spec =
    test_helpers.make_test_spec_from_behaviors([behavior_with_captures])

  // WHEN: Converting to compact
  let compact = compact_format.spec_to_compact(spec)

  // THEN: Captures are preserved
  case list.first(compact.features) {
    Ok(feature) ->
      case list.first(feature.behaviors) {
        Ok(behavior) ->
          list.length(behavior.captures)
          |> should.equal(2)
        Error(_) -> should.fail()
      }
    Error(_) -> should.fail()
  }
}

pub fn format_compact_with_captures_test() {
  // GIVEN: A behavior with captures
  let behavior = test_helpers.make_test_behavior("create-user", [])
  let behavior_with_captures =
    types.Behavior(
      ..behavior,
      captures: dict.from_list([#("user_id", "response.body.id")]),
    )

  let spec =
    test_helpers.make_test_spec_from_behaviors([behavior_with_captures])
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: Output contains capture marker ">>"
  string.contains(output, ">>")
  |> should.be_true()

  // THEN: Output contains capture variable
  string.contains(output, "user_id")
  |> should.be_true()
}

// =============================================================================
// FORMAT OUTPUT TESTS
// =============================================================================

pub fn format_compact_output_test() {
  // GIVEN: A spec with features and behaviors
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: Output matches grammar - starts with "SPEC "
  string.starts_with(output, "SPEC ")
  |> should.be_true()

  // THEN: Not empty
  { string.length(output) > 0 }
  |> should.be_true()
}

pub fn format_compact_features_test() {
  // GIVEN: A spec with features
  let feature =
    test_helpers.make_test_feature("User Management", [
      test_helpers.make_test_behavior("get-user", []),
    ])
  let spec = test_helpers.make_test_spec([feature])
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: Features formatted with "F" prefix (per grammar)
  string.contains(output, "F \"")
  |> should.be_true()

  // THEN: Feature name appears
  string.contains(output, "User Management")
  |> should.be_true()
}

pub fn format_compact_behaviors_test() {
  // GIVEN: A spec with behaviors
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: Behaviors formatted with "B " prefix (per grammar)
  string.contains(output, "B ")
  |> should.be_true()

  // THEN: Behavior name appears
  string.contains(output, "get-user")
  |> should.be_true()
}

// =============================================================================
// TOKEN ESTIMATION TESTS
// =============================================================================

pub fn estimate_tokens_test() {
  // GIVEN: A string with known length
  let text = "This is a test string"
  let length = string.length(text)

  // WHEN: Estimating tokens
  let tokens = compact_format.estimate_tokens(text)

  // THEN: DbC postcondition - estimate is length/4
  let expected = length / 4
  tokens
  |> should.equal(expected)
}

pub fn estimate_tokens_empty_test() {
  // GIVEN: Empty string
  let text = ""

  // WHEN: Estimating tokens
  let tokens = compact_format.estimate_tokens(text)

  // THEN: Returns 0
  tokens
  |> should.equal(0)
}

pub fn estimate_tokens_long_text_test() {
  // GIVEN: Long text
  let text = string.repeat("x", 1000)

  // WHEN: Estimating tokens
  let tokens = compact_format.estimate_tokens(text)

  // THEN: Returns approximately 1000/4 = 250
  tokens
  |> should.equal(250)
}

// =============================================================================
// COMPARE TOKEN USAGE TESTS
// =============================================================================

pub fn compare_token_usage_test() {
  // GIVEN: A spec with some behaviors
  let behaviors = [
    test_helpers.make_test_behavior("get-user", []),
    test_helpers.make_test_behavior("create-user", []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Comparing token usage
  let #(full_tokens, compact_tokens, savings) =
    compact_format.compare_token_usage(spec)

  // THEN: DbC postcondition - returns valid tuple
  { full_tokens >= 0 }
  |> should.be_true()

  { compact_tokens >= 0 }
  |> should.be_true()

  // THEN: Compact should have fewer tokens than full
  { compact_tokens <= full_tokens }
  |> should.be_true()

  // THEN: Savings should be non-negative percentage
  { savings >=. 0.0 }
  |> should.be_true()
}

pub fn compare_token_usage_empty_spec_test() {
  // GIVEN: Empty spec
  let spec = test_helpers.make_test_spec([])

  // WHEN: Comparing token usage
  let #(full_tokens, compact_tokens, _savings) =
    compact_format.compare_token_usage(spec)

  // THEN: Should not crash
  { full_tokens >= 0 }
  |> should.be_true()

  { compact_tokens >= 0 }
  |> should.be_true()
}

pub fn compare_token_usage_savings_calculation_test() {
  // GIVEN: A spec
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Comparing token usage
  let #(full_tokens, compact_tokens, savings) =
    compact_format.compare_token_usage(spec)

  // THEN: Savings formula is correct: (full - compact) / full * 100
  let expected_savings = case full_tokens {
    0 -> 0.0
    _ ->
      int.to_float(full_tokens - compact_tokens)
      /. int.to_float(full_tokens)
      *. 100.0
  }

  // Allow small floating point difference
  let diff = savings -. expected_savings
  let abs_diff = case diff >=. 0.0 {
    True -> diff
    False -> -1.0 *. diff
  }

  { abs_diff <. 0.01 }
  |> should.be_true()
}

// =============================================================================
// PROTOTEXT GENERATION TESTS
// =============================================================================

pub fn spec_to_prototext_test() {
  // GIVEN: A spec with behaviors
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Converting to prototext
  let prototext = compact_format.spec_to_prototext(spec)

  // THEN: DbC postcondition - output is valid prototext (non-empty)
  { string.length(prototext) > 0 }
  |> should.be_true()
}

pub fn spec_to_prototext_empty_spec_test() {
  // GIVEN: Empty spec
  let spec = test_helpers.make_test_spec([])

  // WHEN: Converting to prototext
  let prototext = compact_format.spec_to_prototext(spec)

  // THEN: Should not crash and produce output
  { string.length(prototext) > 0 }
  |> should.be_true()
}

pub fn spec_to_prototext_contains_spec_name_test() {
  // GIVEN: A spec with specific name
  let spec = test_helpers.make_test_spec_with_name("MyAPI", [])

  // WHEN: Converting to prototext
  let prototext = compact_format.spec_to_prototext(spec)

  // THEN: Output contains spec name
  string.contains(prototext, "MyAPI")
  |> should.be_true()
}

// =============================================================================
// DETERMINISM TESTS
// =============================================================================

pub fn format_compact_deterministic_test() {
  // GIVEN: A spec
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Converting twice
  let compact1 = compact_format.spec_to_compact(spec)
  let output1 = compact_format.format_compact(compact1)

  let compact2 = compact_format.spec_to_compact(spec)
  let output2 = compact_format.format_compact(compact2)

  // THEN: DbC invariant - roundtrip is deterministic
  output1
  |> should.equal(output2)
}

// =============================================================================
// EDGE CASE TESTS
// =============================================================================

pub fn spec_to_compact_long_description_test() {
  // GIVEN: A spec with very long description
  let long_desc = string.repeat("A very long description. ", 10)
  let spec =
    types.Spec(..test_helpers.make_test_spec([]), description: long_desc)

  // WHEN: Converting to compact
  let compact = compact_format.spec_to_compact(spec)

  // THEN: Description is truncated (per truncate_description logic)
  let desc_length = string.length(compact.description)

  // Should be <= 103 (100 + "...")
  { desc_length <= 103 }
  |> should.be_true()
}

pub fn format_compact_behavior_with_status_test() {
  // GIVEN: A behavior with specific status
  let behaviors = [
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: Status appears in output
  string.contains(output, "404")
  |> should.be_true()
}

pub fn spec_to_compact_multiple_features_test() {
  // GIVEN: A spec with multiple features
  let feature1 =
    test_helpers.make_test_feature("Auth", [
      test_helpers.make_test_behavior("login", []),
    ])
  let feature2 =
    test_helpers.make_test_feature("Users", [
      test_helpers.make_test_behavior("get-user", []),
    ])
  let spec = test_helpers.make_test_spec([feature1, feature2])

  // WHEN: Converting to compact
  let compact = compact_format.spec_to_compact(spec)

  // THEN: DbC postcondition - preserves feature count
  list.length(compact.features)
  |> should.equal(2)
}

pub fn format_compact_multiple_features_test() {
  // GIVEN: A spec with multiple features
  let feature1 =
    test_helpers.make_test_feature("Auth", [
      test_helpers.make_test_behavior("login", []),
    ])
  let feature2 =
    test_helpers.make_test_feature("Users", [
      test_helpers.make_test_behavior("get-user", []),
    ])
  let spec = test_helpers.make_test_spec([feature1, feature2])
  let compact = compact_format.spec_to_compact(spec)

  // WHEN: Formatting to string
  let output = compact_format.format_compact(compact)

  // THEN: Both features appear in output
  string.contains(output, "Auth")
  |> should.be_true()

  string.contains(output, "Users")
  |> should.be_true()
}
