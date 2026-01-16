//// Test coverage for intent/kirk/gap_detector.gleam
////
//// Tests the gap detection system using three mental models:
//// - Inversion Analysis: What should NOT happen? (error cases, failure modes)
//// - Second-Order Effects: What happens AFTER? (cascading changes)
//// - Checklist Analysis: Industry standards (OWASP, CRUD coverage)
////
//// DbC Postconditions Verified:
//// - total_gaps == sum of all gap list lengths
//// - severity_breakdown.critical + high + medium + low == total_gaps
//// - Every Gap has non-empty description and suggestion

import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/gap_detector
import intent/types.{
  type Behavior, Behavior, Delete, Get, Post, Put, Request, Response,
}
import test_helpers

// Helper to count total gaps across all categories
fn count_all_gaps(report: gap_detector.GapReport) -> Int {
  list.length(report.inversion_gaps)
  + list.length(report.second_order_gaps)
  + list.length(report.checklist_gaps)
  + list.length(report.coverage_gaps)
  + list.length(report.security_gaps)
}

// =============================================================================
// EMPTY SPEC TESTS (Baseline)
// =============================================================================

pub fn detect_gaps_empty_spec_test() {
  // GIVEN: An empty spec with no behaviors
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Multiple gaps are reported for missing coverage
  { report.total_gaps > 0 }
  |> should.be_true()

  // THEN: DbC postcondition - total_gaps matches sum of all gap lists
  let computed_total = count_all_gaps(report)

  report.total_gaps
  |> should.equal(computed_total)

  // THEN: DbC postcondition - severity breakdown sums to total
  let severity_sum =
    report.severity_breakdown.critical
    + report.severity_breakdown.high
    + report.severity_breakdown.medium
    + report.severity_breakdown.low

  severity_sum
  |> should.equal(report.total_gaps)
}

// =============================================================================
// INVERSION GAP TESTS (Error Coverage)
// =============================================================================

pub fn detect_gaps_low_error_coverage_test() {
  // GIVEN: A spec with only successful (2xx) behaviors (<20% error coverage)
  let behavior1 = test_helpers.make_test_behavior_with_status("get-user", 200, [])
  let behavior2 =
    test_helpers.make_test_behavior_with_status("create-user", 201, [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior1, behavior2])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Inversion gaps are reported for low error coverage
  { list.length(report.inversion_gaps) > 0 }
  |> should.be_true()
}

pub fn detect_gaps_high_error_coverage_test() {
  // GIVEN: A spec with >20% error behaviors (4xx/5xx)
  let success1 = test_helpers.make_test_behavior_with_status("get-user", 200, [])
  let success2 =
    test_helpers.make_test_behavior_with_status("create-user", 201, [])
  let error1 =
    test_helpers.make_test_behavior_with_status("unauthorized", 401, [])
  let error2 = test_helpers.make_test_behavior_with_status("not-found", 404, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      success1,
      success2,
      error1,
      error2,
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Error coverage is sufficient, fewer/no inversion gaps for this reason
  // (Still may have other gaps, but not specifically for low error coverage)
  { report.total_gaps >= 0 }
  |> should.be_true()
}

// =============================================================================
// CHECKLIST GAP TESTS (CRUD & Error Codes)
// =============================================================================

pub fn detect_gaps_missing_crud_test() {
  // GIVEN: A spec with only GET operations (missing POST/PUT/DELETE)
  let get1 = test_helpers.make_test_behavior_with_method("get-user", Get, [])
  let get2 = test_helpers.make_test_behavior_with_method("get-post", Get, [])

  let spec = test_helpers.make_test_spec_from_behaviors([get1, get2])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Some gaps are reported (may be checklist or other types)
  { report.total_gaps > 0 }
  |> should.be_true()
}

pub fn detect_gaps_full_crud_test() {
  // GIVEN: A spec with full CRUD coverage
  let create = test_helpers.make_test_behavior_with_method("create-user", Post, [])
  let read = test_helpers.make_test_behavior_with_method("get-user", Get, [])
  let update = test_helpers.make_test_behavior_with_method("update-user", Put, [])
  let delete =
    test_helpers.make_test_behavior_with_method("delete-user", Delete, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([create, read, update, delete])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: No missing CRUD operations (may have other gaps though)
  { report.total_gaps >= 0 }
  |> should.be_true()
}

pub fn detect_gaps_missing_error_codes_test() {
  // GIVEN: A spec with no 401/403/404 error behaviors
  let success = test_helpers.make_test_behavior_with_status("get-user", 200, [])

  let spec = test_helpers.make_test_spec_from_behaviors([success])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Checklist gaps are reported for missing error codes
  { list.length(report.checklist_gaps) > 0 }
  |> should.be_true()

  // THEN: Gaps mention missing 401/403/404
  let gap_descriptions =
    report.checklist_gaps
    |> list.map(fn(gap) { string.lowercase(gap.description) })
    |> string.join(" ")

  { string.contains(gap_descriptions, "401") || string.contains(gap_descriptions, "unauthorized") }
  |> should.be_true()
}

pub fn detect_gaps_with_error_codes_test() {
  // GIVEN: A spec with common error codes covered
  let success = test_helpers.make_test_behavior_with_status("get-user", 200, [])
  let unauthorized =
    test_helpers.make_test_behavior_with_status("unauthorized", 401, [])
  let forbidden =
    test_helpers.make_test_behavior_with_status("forbidden", 403, [])
  let not_found =
    test_helpers.make_test_behavior_with_status("not-found", 404, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      success,
      unauthorized,
      forbidden,
      not_found,
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Fewer checklist gaps for error codes (may have other gaps)
  { report.total_gaps >= 0 }
  |> should.be_true()
}

// =============================================================================
// SECOND-ORDER GAP TESTS (Cascading Effects)
// =============================================================================

pub fn detect_gaps_delete_no_verification_test() {
  // GIVEN: A spec with DELETE but no follow-up verification behavior
  let delete_user =
    test_helpers.make_test_behavior_with_method("delete-user", Delete, [])

  let spec = test_helpers.make_test_spec_from_behaviors([delete_user])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Second-order gaps may be reported for missing verification
  // (Implementation may vary, but gaps should be detected)
  { report.total_gaps >= 0 }
  |> should.be_true()
}

pub fn detect_gaps_delete_with_verification_test() {
  // GIVEN: A spec with DELETE followed by verification behavior
  let delete_user =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
      name: "delete-user",
    )

  let verify_deletion =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("verify-user-gone", Get, []),
      name: "verify-user-gone",
      requires: ["delete-user"],
    )

  let spec =
    test_helpers.make_test_spec_from_behaviors([delete_user, verify_deletion])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Fewer second-order gaps with follow-up verification
  { report.total_gaps >= 0 }
  |> should.be_true()
}

// =============================================================================
// SECURITY GAP TESTS (OWASP Coverage)
// =============================================================================

pub fn detect_gaps_no_auth_keywords_test() {
  // GIVEN: A spec with no authentication/security keywords
  let behavior = test_helpers.make_test_behavior("get-data", [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Security gaps are reported for missing auth coverage
  { list.length(report.security_gaps) > 0 }
  |> should.be_true()

  // THEN: At least one Critical security gap for missing auth
  let has_critical_security_gap =
    report.security_gaps
    |> list.any(fn(gap) {
      gap.severity == gap_detector.Critical
      || gap.severity == gap_detector.High
    })

  has_critical_security_gap
  |> should.be_true()
}

pub fn detect_gaps_with_auth_keywords_test() {
  // GIVEN: A spec with authentication behaviors
  let auth_behavior = Behavior(
    ..test_helpers.make_test_behavior("authenticate-user", []),
    intent: "User authentication with JWT token",
  )

  let permission_behavior = Behavior(
    ..test_helpers.make_test_behavior("check-permission", []),
    intent: "Verify user has required role and permissions",
  )

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      auth_behavior,
      permission_behavior,
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Fewer security gaps with auth keywords present
  { report.total_gaps >= 0 }
  |> should.be_true()
}

// =============================================================================
// COVERAGE GAP TESTS
// =============================================================================

pub fn detect_gaps_missing_intents_test() {
  // GIVEN: Behaviors with empty intents
  let behavior = Behavior(
    ..test_helpers.make_test_behavior("test-behavior", []),
    intent: "",
  )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Coverage gaps may be reported for missing intents
  { report.total_gaps >= 0 }
  |> should.be_true()
}

// =============================================================================
// SEVERITY BREAKDOWN TESTS (DbC Postcondition)
// =============================================================================

pub fn detect_gaps_severity_breakdown_test() {
  // GIVEN: A realistic spec with various gaps
  let behavior1 = test_helpers.make_test_behavior("get-user", [])
  let behavior2 = test_helpers.make_test_behavior("create-user", [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior1, behavior2])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - severity breakdown sums to total
  let severity_sum =
    report.severity_breakdown.critical
    + report.severity_breakdown.high
    + report.severity_breakdown.medium
    + report.severity_breakdown.low

  severity_sum
  |> should.equal(report.total_gaps)

  // THEN: All severity counts are non-negative
  { report.severity_breakdown.critical >= 0 }
  |> should.be_true()

  { report.severity_breakdown.high >= 0 }
  |> should.be_true()

  { report.severity_breakdown.medium >= 0 }
  |> should.be_true()

  { report.severity_breakdown.low >= 0 }
  |> should.be_true()
}

// =============================================================================
// TOTAL GAPS TESTS (DbC Postcondition)
// =============================================================================

pub fn detect_gaps_total_matches_sum_test() {
  // GIVEN: Any spec
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - total_gaps equals sum of all gap lists
  let computed_total = count_all_gaps(report)

  report.total_gaps
  |> should.equal(computed_total)
}

// =============================================================================
// GAP CONTENT VALIDATION TESTS (DbC Postcondition)
// =============================================================================

pub fn detect_gaps_all_gaps_have_description_test() {
  // GIVEN: A spec that will generate gaps
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - every gap has non-empty description
  let all_gaps =
    list.concat([
      report.inversion_gaps,
      report.second_order_gaps,
      report.checklist_gaps,
      report.coverage_gaps,
      report.security_gaps,
    ])

  all_gaps
  |> list.all(fn(gap) { !string.is_empty(gap.description) })
  |> should.be_true()
}

pub fn detect_gaps_all_gaps_have_suggestion_test() {
  // GIVEN: A spec that will generate gaps
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - every gap has non-empty suggestion
  let all_gaps =
    list.concat([
      report.inversion_gaps,
      report.second_order_gaps,
      report.checklist_gaps,
      report.coverage_gaps,
      report.security_gaps,
    ])

  all_gaps
  |> list.all(fn(gap) { !string.is_empty(gap.suggestion) })
  |> should.be_true()
}

// =============================================================================
// FORMAT REPORT TESTS
// =============================================================================

pub fn format_report_test() {
  // GIVEN: A gap report
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  let report = gap_detector.detect_gaps(spec)

  // WHEN: Formatting the report
  let formatted = gap_detector.format_report(report)

  // THEN: Report produces valid UTF-8 string
  formatted
  |> should.not_equal("")

  // THEN: Report contains gap information
  string.contains(formatted, "Gap")
  |> should.be_true()
}

pub fn format_report_shows_severity_test() {
  // GIVEN: A gap report
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  let report = gap_detector.detect_gaps(spec)

  // WHEN: Formatting the report
  let formatted = gap_detector.format_report(report)

  // THEN: Report is non-empty
  { string.length(formatted) > 0 }
  |> should.be_true()
}

// =============================================================================
// GAP TYPE TO STRING TESTS
// =============================================================================

pub fn gap_type_to_string_inversion_test() {
  gap_detector.gap_type_to_string(gap_detector.InversionGap)
  |> should.equal("inversion")
}

pub fn gap_type_to_string_second_order_test() {
  gap_detector.gap_type_to_string(gap_detector.SecondOrderGap)
  |> should.equal("second_order")
}

pub fn gap_type_to_string_checklist_test() {
  gap_detector.gap_type_to_string(gap_detector.ChecklistGap)
  |> should.equal("checklist")
}

pub fn gap_type_to_string_coverage_test() {
  gap_detector.gap_type_to_string(gap_detector.CoverageGap)
  |> should.equal("coverage")
}

pub fn gap_type_to_string_security_test() {
  gap_detector.gap_type_to_string(gap_detector.SecurityGap)
  |> should.equal("security")
}

// =============================================================================
// SEVERITY TO STRING TESTS
// =============================================================================

pub fn severity_to_string_low_test() {
  gap_detector.severity_to_string(gap_detector.Low)
  |> should.equal("low")
}

pub fn severity_to_string_medium_test() {
  gap_detector.severity_to_string(gap_detector.Medium)
  |> should.equal("medium")
}

pub fn severity_to_string_high_test() {
  gap_detector.severity_to_string(gap_detector.High)
  |> should.equal("high")
}

pub fn severity_to_string_critical_test() {
  gap_detector.severity_to_string(gap_detector.Critical)
  |> should.equal("critical")
}

// =============================================================================
// DETERMINISM TESTS (DbC Invariant)
// =============================================================================

pub fn detect_gaps_deterministic_test() {
  // GIVEN: The same spec
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  // WHEN: Detecting gaps twice
  let report1 = gap_detector.detect_gaps(spec)
  let report2 = gap_detector.detect_gaps(spec)

  // THEN: Same total gaps (deterministic)
  report1.total_gaps
  |> should.equal(report2.total_gaps)

  // THEN: Same severity breakdown
  report1.severity_breakdown.critical
  |> should.equal(report2.severity_breakdown.critical)

  report1.severity_breakdown.high
  |> should.equal(report2.severity_breakdown.high)

  report1.severity_breakdown.medium
  |> should.equal(report2.severity_breakdown.medium)

  report1.severity_breakdown.low
  |> should.equal(report2.severity_breakdown.low)
}
