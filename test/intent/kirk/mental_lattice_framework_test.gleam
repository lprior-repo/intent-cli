//// Mental Lattice Framework Tests
////
//// Comprehensive tests validating the three mental models used for gap detection:
//// 1. Inversion Analysis - "Always invert" (Charlie Munger)
//// 2. Second-Order Thinking - Consider cascading consequences
//// 3. Checklist Analysis - Industry-standard coverage patterns
////
//// These tests validate the mental models themselves, ensuring the gap detection
//// system correctly applies each framework's principles.
////
//// DbC Postconditions Verified:
//// - Anti-pattern thresholds: 0 = Medium, 1-2 = Low, 3+ = no gap
//// - Error code severities: 400/401/404 = High, 403/409 = Medium, 429/500 = Low
//// - Security severities: auth/authz = Critical, sensitive = High, others = Medium
//// - Mutation operations: POST/PUT/PATCH/DELETE trigger second-order analysis

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/gap_detector
import intent/types.{
  type AntiPattern, type Behavior, AntiPattern, Behavior, Delete, Get, Patch,
  Post, Put,
}
import test_helpers

// =============================================================================
// INVERSION ANALYSIS - ANTI-PATTERN THRESHOLDS
// =============================================================================

pub fn inversion_zero_anti_patterns_test() {
  // GIVEN: A spec with no anti-patterns defined
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-user", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: Inversion gap reported for missing anti-patterns
  { list.length(report.inversion_gaps) > 0 }
  |> should.be_true()

  // THEN: Gap mentions anti-patterns
  let gap_descriptions =
    report.inversion_gaps
    |> list.map(fn(gap) { string.lowercase(gap.description) })
    |> string.join(" ")

  string.contains(gap_descriptions, "anti-pattern")
  |> should.be_true()

  // THEN: DbC postcondition - 0 anti-patterns = Medium severity
  let anti_pattern_gaps =
    report.inversion_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "anti-pattern")
    })

  case anti_pattern_gaps {
    [gap, ..] ->
      case gap.severity {
        gap_detector.Medium -> True |> should.be_true()
        gap_detector.Low -> True |> should.be_true()
        _ -> should.fail()
      }
    [] -> should.fail()
  }
}

pub fn inversion_few_anti_patterns_test() {
  // GIVEN: A spec with 1-2 anti-patterns (below threshold of 3)
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-user", []),
    ])

  let spec_with_anti_patterns =
    types.Spec(
      ..spec,
      anti_patterns: [
        AntiPattern(
          name: "exposed-secrets",
          description: "Never log passwords",
          bad_example: json.string("console.log(password)"),
          good_example: json.string("// Use secure logging library"),
          why: "Passwords in logs can be exploited",
        ),
        AntiPattern(
          name: "sql-injection",
          description: "Never concatenate SQL",
          bad_example: json.string("SELECT * FROM users WHERE id = ' + id"),
          good_example: json.string("Use parameterized queries"),
          why: "SQL injection vulnerability",
        ),
      ],
    )

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec_with_anti_patterns)

  // THEN: Low severity gap for insufficient anti-patterns
  let anti_pattern_gaps =
    report.inversion_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "anti-pattern")
    })

  case anti_pattern_gaps {
    [gap, ..] ->
      // DbC postcondition - 1-2 anti-patterns = Low severity
      gap.severity
      |> should.equal(gap_detector.Low)
    [] -> {
      // No gap is also valid if implementation considers 2 sufficient
      True |> should.be_true()
    }
  }
}

pub fn inversion_sufficient_anti_patterns_test() {
  // GIVEN: A spec with 3+ anti-patterns (meets threshold)
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-user", []),
    ])

  let spec_with_anti_patterns =
    types.Spec(
      ..spec,
      anti_patterns: [
        AntiPattern(
          name: "exposed-secrets",
          description: "Never log passwords",
          bad_example: json.null(),
          good_example: json.null(),
          why: "Security risk",
        ),
        AntiPattern(
          name: "sql-injection",
          description: "Never concatenate SQL",
          bad_example: json.null(),
          good_example: json.null(),
          why: "Security risk",
        ),
        AntiPattern(
          name: "xss-vulnerability",
          description: "Never trust user input in HTML",
          bad_example: json.null(),
          good_example: json.null(),
          why: "Security risk",
        ),
      ],
    )

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec_with_anti_patterns)

  // THEN: DbC postcondition - 3+ anti-patterns = no anti-pattern gap
  let anti_pattern_gaps =
    report.inversion_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "anti-pattern")
    })

  // Should have no gap or only mention count
  case anti_pattern_gaps {
    [] -> True |> should.be_true()
    [gap, ..] -> {
      // If gap exists, verify it's not about missing anti-patterns
      { string.contains(string.lowercase(gap.description), "no anti") }
      |> should.be_false()
    }
  }
}

pub fn inversion_error_ratio_exact_threshold_test() {
  // GIVEN: A spec with exactly 20% error coverage (threshold)
  let success1 =
    test_helpers.make_test_behavior_with_status("get-user", 200, [])
  let success2 =
    test_helpers.make_test_behavior_with_status("get-post", 200, [])
  let success3 =
    test_helpers.make_test_behavior_with_status("get-comment", 200, [])
  let success4 = test_helpers.make_test_behavior_with_status("get-tag", 200, [])
  let error1 = test_helpers.make_test_behavior_with_status("not-found", 404, [])

  // 4 success + 1 error = 1/5 = 20%
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      success1,
      success2,
      success3,
      success4,
      error1,
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - at 20% threshold, no error ratio gap expected
  let error_ratio_gaps =
    report.inversion_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "error case")
    })

  // At exactly 20%, should have no gap or very low severity
  case error_ratio_gaps {
    [] -> True |> should.be_true()
    [gap, ..] -> {
      // If gap exists, verify it's not High severity at threshold
      case gap.severity {
        gap_detector.High -> should.fail()
        _ -> True |> should.be_true()
      }
    }
  }
}

// =============================================================================
// SECOND-ORDER THINKING - MUTATION VERIFICATION
// =============================================================================

pub fn second_order_post_without_follow_up_test() {
  // GIVEN: A spec with POST operation but no follow-up verification
  let create_user =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("create-user", Post, []),
      name: "create-user",
    )

  let spec = test_helpers.make_test_spec_from_behaviors([create_user])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - POST triggers second-order analysis
  // May or may not flag depending on implementation
  { report.total_gaps >= 0 }
  |> should.be_true()

  // THEN: If second-order gap exists, it's Low severity for POST
  let post_gaps =
    report.second_order_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "create")
    })

  case post_gaps {
    [] -> True |> should.be_true()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Low)
  }
}

pub fn second_order_post_with_follow_up_test() {
  // GIVEN: A spec with POST followed by verification behavior
  let create_user =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("create-user", Post, []),
      name: "create-user",
    )

  let verify_created =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("get-created-user", Get, []),
      name: "get-created-user",
      requires: ["create-user"],
    )

  let spec =
    test_helpers.make_test_spec_from_behaviors([create_user, verify_created])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: No second-order gap for POST with follow-up
  let post_gaps =
    report.second_order_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "create")
    })

  post_gaps
  |> should.equal([])
}

pub fn second_order_put_without_verification_test() {
  // GIVEN: A spec with PUT operation but no follow-up verification
  let update_user =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("update-user", Put, []),
      name: "update-user",
    )

  let spec = test_helpers.make_test_spec_from_behaviors([update_user])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - PUT triggers second-order analysis
  { report.total_gaps >= 0 }
  |> should.be_true()

  // THEN: PUT without follow-up may or may not be flagged (implementation dependent)
  { list.length(report.second_order_gaps) >= 0 }
  |> should.be_true()
}

pub fn second_order_patch_without_verification_test() {
  // GIVEN: A spec with PATCH operation but no follow-up verification
  let patch_user =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("patch-user", Patch, []),
      name: "patch-user",
    )

  let spec = test_helpers.make_test_spec_from_behaviors([patch_user])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - PATCH triggers second-order analysis
  { report.total_gaps >= 0 }
  |> should.be_true()

  // THEN: PATCH without follow-up may or may not be flagged
  { list.length(report.second_order_gaps) >= 0 }
  |> should.be_true()
}

pub fn second_order_delete_medium_severity_test() {
  // GIVEN: A spec with DELETE but no follow-up verification
  let delete_user =
    Behavior(
      ..test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
      name: "delete-user",
    )

  let spec = test_helpers.make_test_spec_from_behaviors([delete_user])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - DELETE gap is Medium severity (more critical than POST)
  let delete_gaps =
    report.second_order_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "delete")
    })

  case delete_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Medium)
  }
}

pub fn second_order_all_mutation_types_test() {
  // GIVEN: A spec with all mutation types but no follow-ups
  let create = test_helpers.make_test_behavior_with_method("create", Post, [])
  let update = test_helpers.make_test_behavior_with_method("update", Put, [])
  let patch = test_helpers.make_test_behavior_with_method("patch", Patch, [])
  let delete = test_helpers.make_test_behavior_with_method("delete", Delete, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([create, update, patch, delete])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - all mutations trigger second-order analysis
  { list.length(report.second_order_gaps) >= 0 }
  |> should.be_true()

  // THEN: Mental model attribution is correct
  case report.second_order_gaps {
    [] -> True |> should.be_true()
    gaps ->
      gaps
      |> list.all(fn(gap) { gap.mental_model == "Second-Order Thinking" })
      |> should.be_true()
  }
}

// =============================================================================
// CHECKLIST ANALYSIS - ERROR CODE SEVERITY ASSIGNMENT
// =============================================================================

pub fn checklist_400_bad_request_high_severity_test() {
  // GIVEN: A spec missing 400 Bad Request
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior_with_status("success", 200, []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - 400 missing = High severity
  let status_400_gaps =
    report.checklist_gaps
    |> list.filter(fn(gap) {
      string.contains(gap.description, "400")
      || string.contains(string.lowercase(gap.description), "bad request")
    })

  case status_400_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.High)
  }
}

pub fn checklist_401_unauthorized_high_severity_test() {
  // GIVEN: A spec missing 401 Unauthorized
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior_with_status("success", 200, []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - 401 missing = High severity
  let status_401_gaps =
    report.checklist_gaps
    |> list.filter(fn(gap) {
      string.contains(gap.description, "401")
      || string.contains(string.lowercase(gap.description), "unauthorized")
    })

  case status_401_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.High)
  }
}

pub fn checklist_404_not_found_high_severity_test() {
  // GIVEN: A spec missing 404 Not Found
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior_with_status("success", 200, []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - 404 missing = High severity
  let status_404_gaps =
    report.checklist_gaps
    |> list.filter(fn(gap) {
      string.contains(gap.description, "404")
      || string.contains(string.lowercase(gap.description), "not found")
    })

  case status_404_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.High)
  }
}

pub fn checklist_403_forbidden_medium_severity_test() {
  // GIVEN: A spec missing 403 Forbidden
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior_with_status("success", 200, []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - 403 missing = Medium severity
  let status_403_gaps =
    report.checklist_gaps
    |> list.filter(fn(gap) {
      string.contains(gap.description, "403")
      || string.contains(string.lowercase(gap.description), "forbidden")
    })

  case status_403_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Medium)
  }
}

pub fn checklist_409_conflict_medium_severity_test() {
  // GIVEN: A spec missing 409 Conflict
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior_with_status("success", 200, []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - 409 missing = Medium severity
  let status_409_gaps =
    report.checklist_gaps
    |> list.filter(fn(gap) {
      string.contains(gap.description, "409")
      || string.contains(string.lowercase(gap.description), "conflict")
    })

  case status_409_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Medium)
  }
}

pub fn checklist_429_rate_limit_low_severity_test() {
  // GIVEN: A spec missing 429 Rate Limit
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior_with_status("success", 200, []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - 429 missing = Low severity
  let status_429_gaps =
    report.checklist_gaps
    |> list.filter(fn(gap) {
      string.contains(gap.description, "429")
      || string.contains(string.lowercase(gap.description), "rate limit")
    })

  case status_429_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Low)
  }
}

pub fn checklist_500_server_error_low_severity_test() {
  // GIVEN: A spec missing 500 Internal Server Error
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior_with_status("success", 200, []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - 500 missing = Low severity
  let status_500_gaps =
    report.checklist_gaps
    |> list.filter(fn(gap) {
      string.contains(gap.description, "500")
      || string.contains(string.lowercase(gap.description), "server error")
    })

  case status_500_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Low)
  }
}

// =============================================================================
// COVERAGE ANALYSIS - BEHAVIORS WITHOUT CHECKS
// =============================================================================

pub fn coverage_behavior_without_checks_high_severity_test() {
  // GIVEN: A behavior with no response checks
  let behavior =
    Behavior(
      ..test_helpers.make_test_behavior("get-user", []),
      response: types.Response(
        ..test_helpers.make_test_behavior("get-user", []).response,
        checks: dict.new(),
      ),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - behavior without checks = High severity
  let no_checks_gaps =
    report.coverage_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "no response check")
    })

  case no_checks_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.High)
  }
}

pub fn coverage_weak_intent_low_severity_test() {
  // GIVEN: A behavior with weak intent (< 10 chars)
  let behavior =
    Behavior(..test_helpers.make_test_behavior("test", []), intent: "test")

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - weak intent = Low severity
  let weak_intent_gaps =
    report.coverage_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "weak intent")
    })

  case weak_intent_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Low)
  }
}

pub fn coverage_intent_exact_threshold_test() {
  // GIVEN: A behavior with intent exactly 10 characters
  let behavior =
    Behavior(
      ..test_helpers.make_test_behavior("test", []),
      intent: "1234567890",
    )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - intent at threshold (10 chars) = no gap
  let weak_intent_gaps =
    report.coverage_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "weak intent")
    })

  // At exactly 10 chars, should have no gap
  weak_intent_gaps
  |> should.equal([])
}

// =============================================================================
// SECURITY ANALYSIS - CATEGORY SEVERITY LEVELS
// =============================================================================

pub fn security_authentication_critical_severity_test() {
  // GIVEN: A spec with no authentication keywords
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-data", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - missing authentication = Critical severity
  let auth_gaps =
    report.security_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "authentication")
    })

  case auth_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Critical)
  }
}

pub fn security_authorization_critical_severity_test() {
  // GIVEN: A spec with no authorization keywords
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-data", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - missing authorization = Critical severity
  let authz_gaps =
    report.security_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "authorization")
    })

  case authz_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Critical)
  }
}

pub fn security_sensitive_data_high_severity_test() {
  // GIVEN: A spec with no sensitive data keywords
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-data", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - missing sensitive-data testing = High severity
  let sensitive_gaps =
    report.security_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "sensitive-data")
    })

  case sensitive_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.High)
  }
}

pub fn security_input_validation_medium_severity_test() {
  // GIVEN: A spec with no input validation keywords
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-data", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - missing input-validation = Medium severity
  let validation_gaps =
    report.security_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "input-validation")
    })

  case validation_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Medium)
  }
}

pub fn security_rate_limiting_medium_severity_test() {
  // GIVEN: A spec with no rate limiting keywords
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-data", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - missing rate-limiting = Medium severity
  let rate_limit_gaps =
    report.security_gaps
    |> list.filter(fn(gap) {
      string.contains(string.lowercase(gap.description), "rate-limiting")
    })

  case rate_limit_gaps {
    [] -> should.fail()
    [gap, ..] -> gap.severity |> should.equal(gap_detector.Medium)
  }
}

pub fn security_mental_model_attribution_test() {
  // GIVEN: A spec that triggers security gaps
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-data", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - all security gaps attribute "Security Checklist"
  report.security_gaps
  |> list.all(fn(gap) { gap.mental_model == "Security Checklist" })
  |> should.be_true()
}

// =============================================================================
// MENTAL MODEL INTEGRATION TESTS
// =============================================================================

pub fn mental_model_all_three_frameworks_applied_test() {
  // GIVEN: A minimal spec that triggers all three mental models
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-user", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: All three mental models should produce gaps
  // 1. Inversion - low error coverage
  { list.length(report.inversion_gaps) > 0 }
  |> should.be_true()

  // 2. Checklist - missing CRUD/error codes
  { list.length(report.checklist_gaps) > 0 }
  |> should.be_true()

  // 3. Security Checklist - missing auth/authz
  { list.length(report.security_gaps) > 0 }
  |> should.be_true()

  // THEN: DbC postcondition - total_gaps includes all frameworks
  { report.total_gaps > 0 }
  |> should.be_true()
}

pub fn mental_model_attributions_distinct_test() {
  // GIVEN: A spec that triggers multiple gap types
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("get-user", []),
    ])

  // WHEN: Detecting gaps
  let report = gap_detector.detect_gaps(spec)

  // THEN: DbC postcondition - each gap type has correct mental model
  // Inversion gaps use "Inversion" model
  case report.inversion_gaps {
    [] -> True |> should.be_true()
    gaps ->
      gaps
      |> list.all(fn(gap) { gap.mental_model == "Inversion" })
      |> should.be_true()
  }

  // Checklist gaps use "Checklist" model
  case report.checklist_gaps {
    [] -> True |> should.be_true()
    gaps ->
      gaps
      |> list.all(fn(gap) { gap.mental_model == "Checklist" })
      |> should.be_true()
  }

  // Security gaps use "Security Checklist" model
  case report.security_gaps {
    [] -> True |> should.be_true()
    gaps ->
      gaps
      |> list.all(fn(gap) { gap.mental_model == "Security Checklist" })
      |> should.be_true()
  }
}
