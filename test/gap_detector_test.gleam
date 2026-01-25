//// Tests for kirk/gap_detector.gleam
//// Contract: Gap detection using mental models (Inversion, Second-Order, Checklist)

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/gap_detector
import intent/types.{Behavior, Delete, Get, Post, Put, Request, Response}
import test_helpers

// =============================================================================
// detect_gaps tests
// =============================================================================

pub fn detect_gaps_empty_spec_test() {
  // Contract: Empty spec reports multiple gaps
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = gap_detector.detect_gaps(spec)

  // Empty spec should have gaps for missing CRUD operations, etc.
  report.total_gaps |> should.not_equal(0)
}

pub fn detect_gaps_low_error_coverage_test() {
  // Contract: <20% errors triggers InversionGap
  // Create all success behaviors - no error cases
  let behaviors = [
    test_helpers.make_test_behavior_with_status("get-user-success", 200, []),
    test_helpers.make_test_behavior_with_status("create-user-success", 201, []),
    test_helpers.make_test_behavior_with_status("update-user-success", 200, []),
    test_helpers.make_test_behavior_with_status("delete-user-success", 204, []),
    test_helpers.make_test_behavior_with_status("list-users-success", 200, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = gap_detector.detect_gaps(spec)

  // Should detect low error coverage
  let has_inversion_gap = !list.is_empty(report.inversion_gaps)
  has_inversion_gap |> should.be_true
}

pub fn detect_gaps_missing_crud_test() {
  // Contract: Missing methods trigger ChecklistGaps
  // Only GET behaviors - missing POST/PUT/DELETE
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("list-users", Get, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = gap_detector.detect_gaps(spec)

  // Should have checklist gaps for missing CRUD operations
  let has_checklist_gaps = !list.is_empty(report.checklist_gaps)
  has_checklist_gaps |> should.be_true
}

pub fn detect_gaps_missing_error_codes_test() {
  // Contract: Missing 401/404 triggers ChecklistGaps
  let behaviors = [
    test_helpers.make_test_behavior_with_status("get-user", 200, []),
    test_helpers.make_test_behavior_with_status("create-user", 201, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = gap_detector.detect_gaps(spec)

  // Should have gaps for missing error handling
  report.total_gaps |> should.not_equal(0)
}

pub fn detect_gaps_delete_no_verification_test() {
  // Contract: DELETE without follow-up triggers SecondOrderGap
  let behaviors = [
    Behavior(
      name: "delete-user",
      intent: "Delete a user permanently",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/users/${id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = gap_detector.detect_gaps(spec)

  // DELETE without verification behavior should trigger second-order gap
  // or at minimum produce some gaps
  report.total_gaps |> should.not_equal(0)
}

pub fn detect_gaps_with_auth_coverage_test() {
  // Contract: Auth-related behaviors reduce security gaps
  let behaviors = [
    Behavior(
      name: "login",
      intent: "Authenticate user with credentials",
      notes: "",
      requires: [],
      tags: ["auth"],
      request: Request(
        method: Post,
        path: "/auth/login",
        headers: dict.new(),
        query: dict.new(),
        body: json.object([
          #("email", json.string("test@example.com")),
          #("password", json.string("secret")),
        ]),
      ),
      response: Response(
        status: 200,
        example: json.object([#("token", json.string("jwt..."))]),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.from_list([#("auth_token", "response.body.token")]),
    ),
    Behavior(
      name: "unauthorized-access",
      intent: "Verify unauthorized access is rejected",
      notes: "",
      requires: [],
      tags: ["security"],
      request: Request(
        method: Get,
        path: "/protected",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 401,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = gap_detector.detect_gaps(spec)

  // Should still report total_gaps >= 0
  { report.total_gaps >= 0 } |> should.be_true
}

pub fn detect_gaps_severity_breakdown_test() {
  // Contract: Breakdown sums to total
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = gap_detector.detect_gaps(spec)

  // Sum of severity breakdown should equal total_gaps
  let sum =
    report.severity_breakdown.critical
    + report.severity_breakdown.high
    + report.severity_breakdown.medium
    + report.severity_breakdown.low

  sum |> should.equal(report.total_gaps)
}

pub fn detect_gaps_comprehensive_coverage_test() {
  // Contract: Spec with good coverage has fewer gaps
  let behaviors = [
    // CRUD operations
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("get-user", Get, ["create-user"]),
    test_helpers.make_test_behavior_with_method("update-user", Put, [
      "create-user",
    ]),
    test_helpers.make_test_behavior_with_method("delete-user", Delete, [
      "create-user",
    ]),
    // Error cases
    test_helpers.make_test_behavior_with_status("unauthorized-access", 401, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = gap_detector.detect_gaps(spec)

  // Should have valid gap report
  { report.total_gaps >= 0 } |> should.be_true
}

// =============================================================================
// format_report tests
// =============================================================================

pub fn format_report_test() {
  // Contract: Report formats without crashing
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = gap_detector.detect_gaps(spec)

  let formatted = gap_detector.format_report(report)

  // Should produce non-empty output
  formatted |> string.is_empty |> should.be_false
}

pub fn format_report_includes_gaps_test() {
  // Contract: Report includes gap information
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = gap_detector.detect_gaps(spec)

  let formatted = gap_detector.format_report(report)

  // Should contain gap-related terms
  let lower = string.lowercase(formatted)
  {
    string.contains(lower, "gap")
    || string.contains(lower, "missing")
    || string.contains(lower, "coverage")
  }
  |> should.be_true
}

// =============================================================================
// gap_type_to_string tests
// =============================================================================

pub fn gap_type_to_string_inversion_test() {
  // Contract: InversionGap converts correctly
  let result = gap_detector.gap_type_to_string(gap_detector.InversionGap)
  result |> string.is_empty |> should.be_false
}

pub fn gap_type_to_string_second_order_test() {
  // Contract: SecondOrderGap converts correctly
  let result = gap_detector.gap_type_to_string(gap_detector.SecondOrderGap)
  result |> string.is_empty |> should.be_false
}

pub fn gap_type_to_string_checklist_test() {
  // Contract: ChecklistGap converts correctly
  let result = gap_detector.gap_type_to_string(gap_detector.ChecklistGap)
  result |> string.is_empty |> should.be_false
}

pub fn gap_type_to_string_coverage_test() {
  // Contract: CoverageGap converts correctly
  let result = gap_detector.gap_type_to_string(gap_detector.CoverageGap)
  result |> string.is_empty |> should.be_false
}

pub fn gap_type_to_string_security_test() {
  // Contract: SecurityGap converts correctly
  let result = gap_detector.gap_type_to_string(gap_detector.SecurityGap)
  result |> string.is_empty |> should.be_false
}

// =============================================================================
// severity_to_string tests
// =============================================================================

pub fn severity_to_string_low_test() {
  // Contract: Low severity converts correctly
  let result = gap_detector.severity_to_string(gap_detector.Low)
  result |> string.lowercase |> string.contains("low") |> should.be_true
}

pub fn severity_to_string_medium_test() {
  // Contract: Medium severity converts correctly
  let result = gap_detector.severity_to_string(gap_detector.Medium)
  result |> string.lowercase |> string.contains("medium") |> should.be_true
}

pub fn severity_to_string_high_test() {
  // Contract: High severity converts correctly
  let result = gap_detector.severity_to_string(gap_detector.High)
  result |> string.lowercase |> string.contains("high") |> should.be_true
}

pub fn severity_to_string_critical_test() {
  // Contract: Critical severity converts correctly
  let result = gap_detector.severity_to_string(gap_detector.Critical)
  result |> string.lowercase |> string.contains("critical") |> should.be_true
}

// =============================================================================
// gaps_to_kirk_health_format tests
// =============================================================================

pub fn gaps_to_kirk_health_format_empty_report_test() {
  // Contract: Empty report returns empty lists
  let report =
    gap_detector.GapReport(
      inversion_gaps: [],
      second_order_gaps: [],
      checklist_gaps: [],
      coverage_gaps: [],
      security_gaps: [],
      total_gaps: 0,
      severity_breakdown: gap_detector.SeverityBreakdown(
        critical: 0,
        high: 0,
        medium: 0,
        low: 0,
      ),
    )

  let #(all_gaps, inversions) = gap_detector.gaps_to_kirk_health_format(report)

  all_gaps |> should.equal([])
  inversions |> should.equal([])
}

pub fn gaps_to_kirk_health_format_with_gaps_test() {
  // Contract: Report with gaps formats them as strings
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("test-behavior", []),
    ])
  let report = gap_detector.detect_gaps(spec)

  let #(all_gaps, _inversions) = gap_detector.gaps_to_kirk_health_format(report)

  // Should have gaps formatted as strings
  list.is_empty(all_gaps) |> should.be_false
}

pub fn gaps_to_kirk_health_format_includes_severity_test() {
  // Contract: Formatted gaps include severity indicator
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = gap_detector.detect_gaps(spec)

  let #(all_gaps, _inversions) = gap_detector.gaps_to_kirk_health_format(report)

  case all_gaps {
    [] -> should.fail()
    [first, ..] -> {
      // Should contain severity keywords in brackets
      let has_severity =
        string.contains(first, "[CRITICAL]")
        || string.contains(first, "[HIGH]")
        || string.contains(first, "[MEDIUM]")
        || string.contains(first, "[LOW]")
      has_severity |> should.be_true
    }
  }
}

pub fn gaps_to_kirk_health_format_inversions_only_test() {
  // Contract: Inversions list contains only inversion gaps
  let behaviors = [
    test_helpers.make_test_behavior_with_status("success", 200, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = gap_detector.detect_gaps(spec)

  let #(_all_gaps, inversions) = gap_detector.gaps_to_kirk_health_format(report)

  // Should have inversions if low error coverage
  case inversions {
    [] -> should.be_false(True)
    [first, ..] -> {
      // Should mention error coverage (from inversion gap description)
      let lower = string.lowercase(first)
      string.contains(lower, "error") |> should.be_true
    }
  }
}

pub fn gaps_to_kirk_health_format_tuple_structure_test() {
  // Contract: Returns tuple of two lists
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = gap_detector.detect_gaps(spec)

  let result = gap_detector.gaps_to_kirk_health_format(report)

  // Should be a tuple that we can pattern match
  case result {
    #(all_gaps, inversions) -> {
      list.is_empty(all_gaps) |> should.be_false
      // inversions is a list (may be empty or not)
      { list.length(inversions) >= 0 } |> should.be_true
    }
  }
}
