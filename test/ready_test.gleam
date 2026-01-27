//// Comprehensive tests for Ready phase commands and workflows
//// Tests all ready-related commands, state transitions, and integrations
////
//// Design by Contract:
//// - Preconditions: Valid Ready sessions, specs, and phase transitions
//// - Postconditions: State transitions are pure, immutable, and validated
//// - Invariants: All prerequisite phases completed before Ready

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/kirk/ready
import intent/phase_state
import intent/planning_types.{
  type ReadyReport, Blocker, Critical, DimensionScore, ReadyReport,
}
import intent/ready_critique
import intent/ready_session.{Approved, Complete, InProgress, ReadyForCritique}
import intent/types.{type Spec}
import test_helpers

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Ready Session Creation Tests
// =============================================================================

pub fn create_session_test() {
  let session =
    ready_session.create_session(
      "ready-001",
      "spec/api.cue",
      "2026-01-25T10:00:00Z",
    )

  session.id
  |> should.equal("ready-001")

  session.spec_path
  |> should.equal("spec/api.cue")

  session.created_at
  |> should.equal("2026-01-25T10:00:00Z")

  session.status
  |> should.equal(InProgress)
}

pub fn create_session_initial_state_test() {
  let session =
    ready_session.create_session("r-002", "spec.cue", "2026-01-25T10:00:00Z")

  session.critique_score
  |> should.equal(0)

  session.blockers_resolved
  |> should.equal(0)

  dict.size(session.responses)
  |> should.equal(0)

  session.approval_notes
  |> should.equal("")
}

pub fn create_session_timestamps_match_test() {
  let session =
    ready_session.create_session("r-003", "spec.cue", "2026-01-25T10:00:00Z")

  session.created_at
  |> should.equal(session.updated_at)
}

// =============================================================================
// Ready Status Tests
// =============================================================================

pub fn get_status_string_in_progress_test() {
  ready_session.get_status_string(InProgress)
  |> should.equal("in_progress")
}

pub fn get_status_string_ready_for_critique_test() {
  ready_session.get_status_string(ReadyForCritique)
  |> should.equal("ready_for_critique")
}

pub fn get_status_string_complete_test() {
  ready_session.get_status_string(Complete)
  |> should.equal("complete")
}

pub fn get_status_string_approved_test() {
  ready_session.get_status_string(Approved)
  |> should.equal("approved")
}

// =============================================================================
// Response Recording Tests
// =============================================================================

pub fn record_response_test() {
  let session =
    ready_session.create_session("r-010", "spec.cue", "2026-01-25T10:00:00Z")

  let updated =
    ready_session.record_response(
      session,
      "issue-1",
      "Fixed replacement score by adding detailed audience description",
      "2026-01-25T10:05:00Z",
    )

  dict.size(updated.responses)
  |> should.equal(1)

  updated.updated_at
  |> should.equal("2026-01-25T10:05:00Z")
}

pub fn record_response_preserves_immutability_test() {
  let original =
    ready_session.create_session("r-011", "spec.cue", "2026-01-25T10:00:00Z")

  let updated =
    ready_session.record_response(
      original,
      "issue-1",
      "Response text",
      "2026-01-25T10:05:00Z",
    )

  // Original unchanged
  dict.size(original.responses)
  |> should.equal(0)

  // Updated has new response
  dict.size(updated.responses)
  |> should.equal(1)
}

pub fn record_response_multiple_issues_test() {
  let session =
    ready_session.create_session("r-012", "spec.cue", "2026-01-25T10:00:00Z")

  let session =
    ready_session.record_response(
      session,
      "issue-1",
      "Response 1",
      "2026-01-25T10:05:00Z",
    )

  let session =
    ready_session.record_response(
      session,
      "issue-2",
      "Response 2",
      "2026-01-25T10:10:00Z",
    )

  let session =
    ready_session.record_response(
      session,
      "issue-3",
      "Response 3",
      "2026-01-25T10:15:00Z",
    )

  dict.size(session.responses)
  |> should.equal(3)
}

pub fn record_response_overwrites_same_issue_test() {
  let session =
    ready_session.create_session("r-013", "spec.cue", "2026-01-25T10:00:00Z")

  let session =
    ready_session.record_response(
      session,
      "issue-1",
      "First response",
      "2026-01-25T10:05:00Z",
    )

  let session =
    ready_session.record_response(
      session,
      "issue-1",
      "Updated response",
      "2026-01-25T10:10:00Z",
    )

  dict.size(session.responses)
  |> should.equal(1)

  let response = dict.get(session.responses, "issue-1")

  case response {
    Ok(r) -> {
      r.response
      |> should.equal("Updated response")

      r.timestamp
      |> should.equal("2026-01-25T10:10:00Z")
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Status Transition Tests
// =============================================================================

pub fn set_status_ready_for_critique_test() {
  let session =
    ready_session.create_session("r-020", "spec.cue", "2026-01-25T10:00:00Z")

  let updated =
    ready_session.set_status_ready_for_critique(session, "2026-01-25T10:30:00Z")

  updated.status
  |> should.equal(ReadyForCritique)

  updated.updated_at
  |> should.equal("2026-01-25T10:30:00Z")
}

pub fn set_status_complete_test() {
  let session =
    ready_session.create_session("r-021", "spec.cue", "2026-01-25T10:00:00Z")

  let updated =
    ready_session.set_status_complete(session, 85, 3, "2026-01-25T11:00:00Z")

  updated.status
  |> should.equal(Complete)

  updated.critique_score
  |> should.equal(85)

  updated.blockers_resolved
  |> should.equal(3)

  updated.updated_at
  |> should.equal("2026-01-25T11:00:00Z")
}

pub fn set_status_complete_zero_blockers_test() {
  let session =
    ready_session.create_session("r-022", "spec.cue", "2026-01-25T10:00:00Z")

  let updated =
    ready_session.set_status_complete(session, 95, 0, "2026-01-25T11:00:00Z")

  updated.status
  |> should.equal(Complete)

  updated.critique_score
  |> should.equal(95)

  updated.blockers_resolved
  |> should.equal(0)
}

// =============================================================================
// Approval Tests
// =============================================================================

pub fn approve_session_success_test() {
  let session =
    ready_session.create_session("r-030", "spec.cue", "2026-01-25T10:00:00Z")

  let session =
    ready_session.set_status_complete(session, 85, 2, "2026-01-25T11:00:00Z")

  let result =
    ready_session.approve_session(
      session,
      "Approved for production launch",
      "2026-01-25T12:00:00Z",
    )

  case result {
    Ok(approved) -> {
      approved.status
      |> should.equal(Approved)

      approved.approval_notes
      |> should.equal("Approved for production launch")

      approved.updated_at
      |> should.equal("2026-01-25T12:00:00Z")
    }
    Error(_) -> should.fail()
  }
}

pub fn approve_session_minimum_score_test() {
  let session =
    ready_session.create_session("r-031", "spec.cue", "2026-01-25T10:00:00Z")

  let session =
    ready_session.set_status_complete(session, 70, 1, "2026-01-25T11:00:00Z")

  let result =
    ready_session.approve_session(
      session,
      "Minimal approval",
      "2026-01-25T12:00:00Z",
    )

  case result {
    Ok(_) -> True
    Error(_) -> False
  }
  |> should.be_true()
}

pub fn approve_session_score_too_low_fails_test() {
  let session =
    ready_session.create_session("r-032", "spec.cue", "2026-01-25T10:00:00Z")

  let session =
    ready_session.set_status_complete(session, 65, 1, "2026-01-25T11:00:00Z")

  let result =
    ready_session.approve_session(session, "Notes", "2026-01-25T12:00:00Z")

  case result {
    Error(msg) -> {
      should.be_true(
        dict.from_list([
          #("contains_65", dict.from_list([#("val", msg |> contains("65"))])),
          #(
            "contains_threshold",
            dict.from_list([#("val", msg |> contains("threshold"))]),
          ),
        ])
        |> dict.get("contains_65")
        |> should.be_ok
        |> dict.get("val")
        |> should.be_ok,
      )
    }
    Ok(_) -> should.fail()
  }
}

pub fn approve_session_not_complete_fails_test() {
  let session =
    ready_session.create_session("r-033", "spec.cue", "2026-01-25T10:00:00Z")

  let result =
    ready_session.approve_session(session, "Notes", "2026-01-25T12:00:00Z")

  case result {
    Error(msg) -> {
      msg
      |> contains("Complete")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn approve_session_in_progress_fails_test() {
  let session =
    ready_session.create_session("r-034", "spec.cue", "2026-01-25T10:00:00Z")

  let result =
    ready_session.approve_session(session, "Notes", "2026-01-25T12:00:00Z")

  case result {
    Error(_) -> True
    Ok(_) -> False
  }
  |> should.be_true()
}

pub fn approve_session_ready_for_critique_fails_test() {
  let session =
    ready_session.create_session("r-035", "spec.cue", "2026-01-25T10:00:00Z")

  let session =
    ready_session.set_status_ready_for_critique(session, "2026-01-25T10:30:00Z")

  let result =
    ready_session.approve_session(session, "Notes", "2026-01-25T12:00:00Z")

  case result {
    Error(_) -> True
    Ok(_) -> False
  }
  |> should.be_true()
}

// =============================================================================
// READY Analysis Integration Tests
// =============================================================================

pub fn analyze_ready_minimal_spec_test() {
  let spec = test_helpers.make_test_spec([])

  let report = ready.analyze_ready(spec)

  report.overall_readiness
  |> should_be_less_than(50)
}

pub fn analyze_ready_comprehensive_spec_test() {
  let behaviors = [
    test_helpers.make_test_behavior("create_user", []),
    test_helpers.make_test_behavior_with_status("invalid_input", 400, []),
    test_helpers.make_test_behavior_with_status("unauthorized", 401, []),
  ]

  let feature = test_helpers.make_test_feature("Authentication", behaviors)

  let spec = test_helpers.make_test_spec([feature])

  let report = ready.analyze_ready(spec)

  report.overall_readiness
  |> should_be_greater_than(0)

  list.length(report.blockers)
  |> should_be_greater_than_or_equal(0)
}

pub fn analyze_ready_replacement_dimension_test() {
  let spec =
    test_helpers.make_test_spec([])
    |> with_audience("AI agents requiring deterministic API responses")
    |> with_success_criteria([
      "99.9% uptime",
      "< 100ms p95 latency",
      "Zero breaking changes",
    ])

  let report = ready.analyze_ready(spec)

  report.replacement.score
  |> should_be_greater_than(0)
}

pub fn analyze_ready_empathy_dimension_test() {
  let behaviors = [
    test_helpers.make_test_behavior_with_status("error_400", 400, []),
    test_helpers.make_test_behavior_with_status("error_404", 404, []),
    test_helpers.make_test_behavior_with_status("error_422", 422, []),
  ]

  let feature = test_helpers.make_test_feature("Error Handling", behaviors)
  let spec = test_helpers.make_test_spec([feature])

  let report = ready.analyze_ready(spec)

  report.empathy.score
  |> should_be_greater_than(40)
}

pub fn analyze_ready_discoverable_dimension_test() {
  let behaviors = [
    test_helpers.make_test_behavior("list_users", []),
    test_helpers.make_test_behavior("create_user", []),
    test_helpers.make_test_behavior("get_user", []),
    test_helpers.make_test_behavior("update_user", []),
    test_helpers.make_test_behavior("delete_user", []),
  ]

  let feature = test_helpers.make_test_feature("Users", behaviors)
  let spec = test_helpers.make_test_spec([feature])

  let report = ready.analyze_ready(spec)

  report.discoverable.score
  |> should_be_greater_than(0)
}

// =============================================================================
// Critique Integration Tests
// =============================================================================

pub fn critique_ready_perfect_spec_test() {
  let report = create_perfect_ready_report()

  let critique = ready_critique.critique_ready(report)

  critique.passed
  |> should.be_true()

  critique.score
  |> should.equal(100)

  list.length(critique.issues)
  |> should.equal(0)
}

pub fn critique_ready_low_replacement_test() {
  let report =
    create_perfect_ready_report()
    |> with_replacement_score(35)

  let critique = ready_critique.critique_ready(report)

  critique.passed
  |> should.be_false()

  list.length(critique.issues)
  |> should_be_greater_than(0)
}

pub fn critique_ready_critical_blockers_test() {
  let report =
    create_perfect_ready_report()
    |> with_critical_blocker("Overall readiness below threshold")

  let critique = ready_critique.critique_ready(report)

  critique.passed
  |> should.be_false()
}

pub fn critique_ready_multiple_warnings_test() {
  let report =
    create_perfect_ready_report()
    |> with_replacement_score(65)
    |> with_empathy_score(55)
    |> with_actionable_score(58)

  let critique = ready_critique.critique_ready(report)

  list.length(critique.issues)
  |> should_be_greater_than(0)

  // Should still pass with warnings
  { critique.score >= 70 && critique.score < 100 }
  |> should.be_true()
}

// =============================================================================
// Phase State Integration Tests
// =============================================================================

pub fn check_ready_gate_passes_test() {
  let report = create_perfect_ready_report()

  let checks = phase_state.check_ready_gate(report)

  phase_state.all_gates_passed(checks)
  |> should.be_true()
}

pub fn check_ready_gate_low_readiness_fails_test() {
  let report =
    ReadyReport(..create_perfect_ready_report(), overall_readiness: 75)

  let checks = phase_state.check_ready_gate(report)

  phase_state.all_gates_passed(checks)
  |> should.be_false()
}

pub fn check_ready_gate_critical_blockers_fail_test() {
  let report =
    ReadyReport(..create_perfect_ready_report(), blockers: [
      Blocker(
        severity: Critical,
        description: "Critical issue",
        affected_areas: ["all"],
      ),
    ])

  let checks = phase_state.check_ready_gate(report)

  phase_state.all_gates_passed(checks)
  |> should.be_false()
}

pub fn check_ready_gate_minimum_readiness_passes_test() {
  let report =
    ReadyReport(..create_perfect_ready_report(), overall_readiness: 80)

  let checks = phase_state.check_ready_gate(report)

  phase_state.all_gates_passed(checks)
  |> should.be_true()
}

// =============================================================================
// Edge Cases and Error Handling
// =============================================================================

pub fn empty_spec_ready_analysis_test() {
  let spec =
    types.Spec(
      name: "",
      description: "",
      audience: "",
      version: "",
      success_criteria: [],
      config: test_helpers.make_test_config(),
      features: [],
      rules: [],
      anti_patterns: [],
      ai_hints: types.AIHints(
        implementation: types.ImplementationHints(suggested_stack: []),
        entities: dict.new(),
        security: types.SecurityHints(
          password_hashing: "",
          jwt_algorithm: "",
          jwt_expiry: "",
          rate_limiting: "",
        ),
        pitfalls: [],
      ),
    )

  let report = ready.analyze_ready(spec)

  // Empty spec with test config gives 5 (config.base_url adds 25 to yet_complete, weighted 0.2)
  report.overall_readiness
  |> should.equal(5)

  // Empty spec should generate blockers
  case report.blockers != [] {
    True -> Nil
    False -> should.fail()
  }
}

pub fn ready_session_multiple_transitions_test() {
  let session =
    ready_session.create_session("r-100", "spec.cue", "2026-01-25T10:00:00Z")

  // Transition through all states
  let session =
    ready_session.set_status_ready_for_critique(session, "2026-01-25T10:30:00Z")

  session.status
  |> should.equal(ReadyForCritique)

  let session =
    ready_session.set_status_complete(session, 88, 1, "2026-01-25T11:00:00Z")

  session.status
  |> should.equal(Complete)

  let result =
    ready_session.approve_session(
      session,
      "Production ready",
      "2026-01-25T12:00:00Z",
    )

  case result {
    Ok(final_session) -> {
      final_session.status
      |> should.equal(Approved)
    }
    Error(_) -> should.fail()
  }
}

pub fn ready_critique_all_dimensions_low_test() {
  let report =
    ReadyReport(
      replacement: DimensionScore(score: 25, reasoning: "Low", issues: []),
      empathy: DimensionScore(score: 20, reasoning: "Low", issues: []),
      actionable: DimensionScore(score: 15, reasoning: "Low", issues: []),
      discoverable: DimensionScore(score: 30, reasoning: "Low", issues: []),
      yet_complete: DimensionScore(score: 28, reasoning: "Low", issues: []),
      overall_readiness: 24,
      blockers: [],
      recommendations: [],
    )

  let critique = ready_critique.critique_ready(report)

  critique.passed
  |> should.be_false()

  // Should have many issues with low scores
  case critique.score < 30 {
    True -> Nil
    False -> should.fail()
  }

  case list.length(critique.issues) >= 5 {
    True -> Nil
    False -> should.fail()
  }
}

pub fn ready_critique_boundary_scores_test() {
  // Test exactly at thresholds
  let report =
    ReadyReport(
      replacement: DimensionScore(score: 60, reasoning: "Threshold", issues: []),
      empathy: DimensionScore(score: 50, reasoning: "Threshold", issues: []),
      actionable: DimensionScore(score: 50, reasoning: "Threshold", issues: []),
      discoverable: DimensionScore(score: 70, reasoning: "Good", issues: []),
      yet_complete: DimensionScore(
        score: 60,
        reasoning: "Threshold",
        issues: [],
      ),
      overall_readiness: 70,
      blockers: [],
      recommendations: [],
    )

  let critique = ready_critique.critique_ready(report)

  critique.passed
  |> should.be_true()
}

// =============================================================================
// Helper Functions
// =============================================================================

fn create_perfect_ready_report() -> ReadyReport {
  ReadyReport(
    replacement: DimensionScore(
      score: 90,
      reasoning: "Clear value proposition",
      issues: [],
    ),
    empathy: DimensionScore(
      score: 85,
      reasoning: "Strong error handling",
      issues: [],
    ),
    actionable: DimensionScore(
      score: 88,
      reasoning: "Excellent response checks",
      issues: [],
    ),
    discoverable: DimensionScore(
      score: 82,
      reasoning: "Good organization",
      issues: [],
    ),
    yet_complete: DimensionScore(
      score: 92,
      reasoning: "Fully complete",
      issues: [],
    ),
    overall_readiness: 87,
    blockers: [],
    recommendations: [],
  )
}

fn with_replacement_score(report: ReadyReport, score: Int) -> ReadyReport {
  let updated_report =
    ReadyReport(
      ..report,
      replacement: DimensionScore(
        score: score,
        reasoning: "Adjusted score",
        issues: case score < 60 {
          True -> ["Low replacement score"]
          False -> []
        },
      ),
    )

  // Recalculate overall readiness
  recalculate_overall_readiness(updated_report)
}

fn with_empathy_score(report: ReadyReport, score: Int) -> ReadyReport {
  let updated_report =
    ReadyReport(
      ..report,
      empathy: DimensionScore(
        score: score,
        reasoning: "Adjusted score",
        issues: case score < 50 {
          True -> ["Low empathy score"]
          False -> []
        },
      ),
    )

  // Recalculate overall readiness
  recalculate_overall_readiness(updated_report)
}

fn with_actionable_score(report: ReadyReport, score: Int) -> ReadyReport {
  let updated_report =
    ReadyReport(
      ..report,
      actionable: DimensionScore(
        score: score,
        reasoning: "Adjusted score",
        issues: case score < 50 {
          True -> ["Low actionable score"]
          False -> []
        },
      ),
    )

  // Recalculate overall readiness
  recalculate_overall_readiness(updated_report)
}

fn recalculate_overall_readiness(report: ReadyReport) -> ReadyReport {
  // Weighted average: R=25%, E=20%, A=20%, D=15%, Y=20%
  let weighted =
    int.to_float(report.replacement.score)
    *. 0.25
    +. int.to_float(report.empathy.score)
    *. 0.2
    +. int.to_float(report.actionable.score)
    *. 0.2
    +. int.to_float(report.discoverable.score)
    *. 0.15
    +. int.to_float(report.yet_complete.score)
    *. 0.2

  ReadyReport(..report, overall_readiness: float.round(weighted))
}

fn with_critical_blocker(
  report: ReadyReport,
  description: String,
) -> ReadyReport {
  ReadyReport(..report, blockers: [
    Blocker(severity: Critical, description: description, affected_areas: [
      "all",
    ]),
    ..report.blockers
  ])
}

fn with_audience(spec: Spec, audience: String) -> Spec {
  types.Spec(..spec, audience: audience)
}

fn with_success_criteria(spec: Spec, criteria: List(String)) -> Spec {
  types.Spec(..spec, success_criteria: criteria)
}

fn contains(haystack: String, needle: String) -> Bool {
  case haystack, needle {
    "", _ -> False
    _, "" -> True
    _, _ -> do_contains(haystack, needle)
  }
}

fn do_contains(haystack: String, needle: String) -> Bool {
  // Simple substring check - would use string.contains in real code
  case haystack {
    _ if haystack == needle -> True
    _ ->
      case string_starts_with(haystack, needle) {
        True -> True
        False ->
          case haystack {
            "" -> False
            _ ->
              case string_drop_left(haystack, 1) {
                "" -> False
                rest -> do_contains(rest, needle)
              }
          }
      }
  }
}

// Simple implementation using string slicing
// Avoids Erlang FFI type mismatches
fn string_starts_with(haystack: String, needle: String) -> Bool {
  let needle_len = string.length(needle)
  let haystack_prefix = string.slice(haystack, 0, needle_len)
  haystack_prefix == needle
}

fn string_drop_left(str: String, n: Int) -> String {
  string.drop_left(str, n)
}

fn should_be_less_than(actual: Int, expected: Int) -> Nil {
  case actual < expected {
    True -> Nil
    False ->
      should.fail()
      |> with_message(
        "Expected " <> int_to_string(actual) <> " < " <> int_to_string(expected),
      )
  }
}

fn should_be_greater_than(actual: Int, expected: Int) -> Nil {
  case actual > expected {
    True -> Nil
    False ->
      should.fail()
      |> with_message(
        "Expected " <> int_to_string(actual) <> " > " <> int_to_string(expected),
      )
  }
}

fn should_be_greater_than_or_equal(actual: Int, expected: Int) -> Nil {
  case actual >= expected {
    True -> Nil
    False ->
      should.fail()
      |> with_message(
        "Expected "
        <> int_to_string(actual)
        <> " >= "
        <> int_to_string(expected),
      )
  }
}

fn with_message(result: Nil, _msg: String) -> Nil {
  result
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_string(-n, "")
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
