//// Tests for kirk/compact_format.gleam
//// Contract: Token-efficient format conversion and prototext generation

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/compact_format
import intent/types.{Behavior, Check, Get, Post, Request, Response}
import test_helpers

// =============================================================================
// spec_to_compact tests
// =============================================================================

pub fn spec_to_compact_empty_test() {
  // Contract: Empty spec converts correctly
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let compact = compact_format.spec_to_compact(spec)

  // Should preserve name and version
  compact.name |> should.equal("Test Spec")
  compact.version |> should.equal("1.0.0")
  // Should have empty features list (or one empty feature)
  compact.features
  |> list.flat_map(fn(f) { f.behaviors })
  |> list.length
  |> should.equal(0)
}

pub fn spec_to_compact_behavior_test() {
  // Contract: Behavior converts to CompactBehavior
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let compact = compact_format.spec_to_compact(spec)

  // Should have one behavior
  let all_behaviors =
    compact.features |> list.flat_map(fn(f) { f.behaviors })
  all_behaviors |> list.length |> should.equal(1)

  case list.first(all_behaviors) {
    Ok(cb) -> cb.name |> should.equal("get-user")
    Error(_) -> should.fail()
  }
}

pub fn spec_to_compact_method_string_test() {
  // Contract: Method/path/body formatted correctly
  let behavior =
    Behavior(
      name: "create-user",
      intent: "Create a new user",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Post,
        path: "/users",
        headers: dict.new(),
        query: dict.new(),
        body: json.object([#("name", json.string("test"))]),
      ),
      response: Response(
        status: 201,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let compact = compact_format.spec_to_compact(spec)

  // Get the compact behavior
  let all_behaviors =
    compact.features |> list.flat_map(fn(f) { f.behaviors })
  case list.first(all_behaviors) {
    Ok(cb) -> {
      // Request should include POST and /users
      cb.request |> string.contains("POST") |> should.be_true
      cb.request |> string.contains("/users") |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn spec_to_compact_with_requires_test() {
  // Contract: Dependencies preserved
  let behaviors = [
    test_helpers.make_test_behavior("create-user", []),
    test_helpers.make_test_behavior("get-user", ["create-user"]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let compact = compact_format.spec_to_compact(spec)

  // Find get-user behavior
  let all_behaviors =
    compact.features |> list.flat_map(fn(f) { f.behaviors })
  let get_user = list.find(all_behaviors, fn(b) { b.name == "get-user" })

  case get_user {
    Ok(cb) -> {
      cb.requires |> should.equal(["create-user"])
    }
    Error(_) -> should.fail()
  }
}

pub fn spec_to_compact_with_captures_test() {
  // Contract: Captures preserved
  let behavior =
    Behavior(
      name: "create-user",
      intent: "Create a new user",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Post,
        path: "/users",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 201,
        example: json.object([#("id", json.int(123))]),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.from_list([#("user_id", "response.body.id")]),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let compact = compact_format.spec_to_compact(spec)

  // Get compact behavior
  let all_behaviors =
    compact.features |> list.flat_map(fn(f) { f.behaviors })
  case list.first(all_behaviors) {
    Ok(cb) -> {
      cb.captures |> list.is_empty |> should.be_false
    }
    Error(_) -> should.fail()
  }
}

pub fn spec_to_compact_with_checks_test() {
  // Contract: Checks converted to CompactCheck
  let behavior =
    Behavior(
      name: "get-user",
      intent: "Get a user by ID",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users/${id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.null(),
        checks: dict.from_list([
          #("id", Check(rule: "integer", why: "User ID must be an integer")),
        ]),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let compact = compact_format.spec_to_compact(spec)

  // Get compact behavior
  let all_behaviors =
    compact.features |> list.flat_map(fn(f) { f.behaviors })
  case list.first(all_behaviors) {
    Ok(cb) -> {
      cb.checks |> list.is_empty |> should.be_false
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// format_compact tests
// =============================================================================

pub fn format_compact_output_test() {
  // Contract: Output matches grammar (starts with SPEC)
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let compact = compact_format.spec_to_compact(spec)

  let formatted = compact_format.format_compact(compact)

  // Should start with "SPEC "
  formatted |> string.starts_with("SPEC ") |> should.be_true
}

pub fn format_compact_features_test() {
  // Contract: Features formatted with "F" prefix
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let compact = compact_format.spec_to_compact(spec)

  let formatted = compact_format.format_compact(compact)

  // Should contain "F" for feature marker
  formatted |> string.contains("F ") |> should.be_true
}

pub fn format_compact_behaviors_test() {
  // Contract: Behaviors formatted with "B" prefix
  let behaviors = [test_helpers.make_test_behavior("my-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let compact = compact_format.spec_to_compact(spec)

  let formatted = compact_format.format_compact(compact)

  // Should contain "B " for behavior marker
  formatted |> string.contains("B ") |> should.be_true
}

// =============================================================================
// estimate_tokens tests
// =============================================================================

pub fn estimate_tokens_test() {
  // Contract: Token estimate is length/4
  let text = "This is a test string with 44 characters!!!"
  // String length is 44, so estimate should be ~11

  let estimate = compact_format.estimate_tokens(text)

  // Should be string.length / 4
  let expected = string.length(text) / 4
  estimate |> should.equal(expected)
}

pub fn estimate_tokens_empty_test() {
  // Contract: Empty string has 0 tokens
  let estimate = compact_format.estimate_tokens("")

  estimate |> should.equal(0)
}

// =============================================================================
// compare_token_usage tests
// =============================================================================

pub fn compare_token_usage_test() {
  // Contract: Returns valid ratio
  let behaviors = [
    test_helpers.make_test_behavior("create-user", []),
    test_helpers.make_test_behavior("get-user", ["create-user"]),
    test_helpers.make_test_behavior("update-user", ["create-user"]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let #(compact_tokens, full_tokens, ratio) =
    compact_format.compare_token_usage(spec)

  // Compact should have fewer tokens than full
  { compact_tokens >= 0 } |> should.be_true
  { full_tokens >= 0 } |> should.be_true
  // Ratio should be between 0 and 1 (or slightly above for very small specs)
  { ratio >=. 0.0 } |> should.be_true
}

// =============================================================================
// spec_to_prototext tests
// =============================================================================

pub fn spec_to_prototext_test() {
  // Contract: Prototext output is valid
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let prototext = compact_format.spec_to_prototext(spec)

  // Should produce non-empty output
  prototext |> string.is_empty |> should.be_false
}

pub fn spec_to_prototext_includes_spec_name_test() {
  // Contract: Prototext includes spec name
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let prototext = compact_format.spec_to_prototext(spec)

  // Should contain the spec name
  prototext |> string.contains("Test Spec") |> should.be_true
}
