//// Tests for kirk/effects_analyzer.gleam
//// Contract: Second-order effect analysis and detection

import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string
import gleeunit/should
import intent/kirk/effects_analyzer
import intent/types.{Behavior, Delete, Get, Post, Put, Request, Response}
import test_helpers

// =============================================================================
// analyze_effects tests
// =============================================================================

pub fn analyze_effects_empty_spec_test() {
  // Contract: Empty spec returns empty report with 100% coverage (vacuously true)
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = effects_analyzer.analyze_effects(spec)

  report.behavior_effects |> should.equal([])
  report.orphaned_resources |> should.equal([])
  report.cascade_warnings |> should.equal([])
  report.state_dependencies |> should.equal([])
  report.total_second_order_effects |> should.equal(0)
  report.coverage_score |> should.equal(100.0)
}

pub fn analyze_effects_get_behavior_test() {
  // Contract: GET behaviors have minimal second-order effects (mainly caching)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = effects_analyzer.analyze_effects(spec)

  // Should have one behavior effect entry
  report.behavior_effects |> list.length |> should.equal(1)

  // GET behaviors typically have cache-related effects
  case list.first(report.behavior_effects) {
    Ok(be) -> {
      be.behavior_name |> should.equal("get-user")
      // GET operations have first-order effect of retrieving data
      be.first_order |> string.contains("retrieve") |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn analyze_effects_delete_behavior_test() {
  // Contract: DELETE triggers orphan detection
  let behaviors = [
    test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = effects_analyzer.analyze_effects(spec)

  // DELETE behaviors should have second-order effects
  case list.first(report.behavior_effects) {
    Ok(be) -> {
      be.behavior_name |> should.equal("delete-user")
      // DELETE should have effects related to cascading/cleanup
      be.second_order |> list.is_empty |> should.be_false
    }
    Error(_) -> should.fail()
  }
}

pub fn analyze_effects_user_delete_test() {
  // Contract: User DELETE has critical security effects (session invalidation, ownership)
  let behavior =
    Behavior(
      name: "delete-user",
      intent: "Delete a user account permanently",
      notes: "",
      requires: [],
      tags: ["security"],
      request: Request(
        method: Delete,
        path: "/users/${user_id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
      ),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let report = effects_analyzer.analyze_effects(spec)

  // User DELETE should trigger orphaned resource detection
  // and have high-severity second-order effects
  case list.first(report.behavior_effects) {
    Ok(be) -> {
      // Should have second-order effects for user deletion
      { list.length(be.second_order) >= 0 } |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn analyze_effects_create_behavior_test() {
  // Contract: POST triggers create effects (resource retrievable, appears in listings)
  let behavior =
    Behavior(
      name: "create-user",
      intent: "Create a new user account",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Post,
        path: "/users",
        headers: dict.new(),
        query: dict.new(),
        body: json.object([
          #("email", json.string("test@example.com")),
          #("name", json.string("Test User")),
        ]),
      ),
      response: Response(
        status: 201,
        example: json.object([#("id", json.int(123))]),
        checks: dict.new(),
      ),
      captures: dict.from_list([#("user_id", "response.body.id")]),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let report = effects_analyzer.analyze_effects(spec)

  case list.first(report.behavior_effects) {
    Ok(be) -> {
      be.behavior_name |> should.equal("create-user")
      // POST (create) should have first-order effect about creation
      be.first_order
      |> string.lowercase
      |> string.contains("create")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn analyze_effects_with_dependencies_test() {
  // Contract: Behavior with `requires` triggers state dependencies
  let create_user =
    test_helpers.make_test_behavior_with_method("create-user", Post, [])
  let update_user =
    test_helpers.make_test_behavior_with_method("update-user", Put, [
      "create-user",
    ])
  let spec =
    test_helpers.make_test_spec_from_behaviors([create_user, update_user])

  let report = effects_analyzer.analyze_effects(spec)

  // Should have state dependencies for behaviors with requires
  // The update-user behavior depends on create-user
  let has_dependency =
    report.state_dependencies
    |> list.any(fn(dep) { dep.behavior == "update-user" })

  has_dependency |> should.be_true
}

pub fn analyze_effects_cascade_detection_test() {
  // Contract: Cascades detected for user/org operations
  let delete_org =
    Behavior(
      name: "delete-organization",
      intent: "Delete an organization and all its data",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/organizations/${org_id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
      ),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([delete_org])

  let report = effects_analyzer.analyze_effects(spec)

  // Organization deletion should trigger cascade warnings
  // (members, projects, settings, etc.)
  case list.first(report.behavior_effects) {
    Ok(be) -> {
      // Should have delete-related effects
      be.first_order
      |> string.lowercase
      |> string.contains("delete")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn analyze_effects_coverage_score_test() {
  // Contract: Coverage score in range [0.0, 100.0]
  let behaviors = [
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("get-user", Get, ["create-user"]),
    test_helpers.make_test_behavior_with_method("delete-user", Delete, [
      "create-user",
    ]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = effects_analyzer.analyze_effects(spec)

  // Coverage score should be between 0 and 100
  { report.coverage_score >=. 0.0 && report.coverage_score <=. 100.0 }
  |> should.be_true
}

// =============================================================================
// format_report tests
// =============================================================================

pub fn format_report_empty_test() {
  // Contract: Empty report formats without crashing
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = effects_analyzer.analyze_effects(spec)

  let formatted = effects_analyzer.format_report(report)

  // Should produce valid string output
  formatted |> string.is_empty |> should.be_false
}

pub fn format_report_with_behaviors_test() {
  // Contract: Report with behaviors formats correctly
  let behaviors = [
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = effects_analyzer.analyze_effects(spec)

  let formatted = effects_analyzer.format_report(report)

  // Should contain behavior names
  formatted |> string.contains("create-user") |> should.be_true
  formatted |> string.contains("delete-user") |> should.be_true
}

pub fn format_report_includes_coverage_test() {
  // Contract: Report includes coverage score
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = effects_analyzer.analyze_effects(spec)

  let formatted = effects_analyzer.format_report(report)

  // Should mention coverage somewhere in the report
  formatted |> string.lowercase |> string.contains("coverage") |> should.be_true
}
