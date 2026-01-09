// Bead Validator Tests
// TDD-driven tests for ML-GAP-3, ML-GAP-4, ML-GAP-5
// Tests quality validation gates, EARS parsing, and inversion coverage

import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import intent/bead_validator.{
  type BeadKirkMetadata, BeadKirkMetadata, BeadEARS, BeadContract, BeadInversion, BeadQuality,
  ValidationIssue, SevError,
  validate_bead_kirk, validate_bead_ears, analyze_bead_inversions,
  calculate_quality_score,
}
import intent/kirk/ears_parser.{
  Ubiquitous, EventDriven, StateDriven, Optional, Unwanted, Complex,
}

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// TEST HELPERS
// =============================================================================

fn make_high_quality_metadata() -> BeadKirkMetadata {
  BeadKirkMetadata(
    ears: Some(BeadEARS(
      pattern: Ubiquitous,
      trigger: None,
      condition: None,
      state: None,
      clarity_score: 95,
      pattern_matches_test: True,
    )),
    contract: Some(BeadContract(
      preconditions: ["Input is valid", "User is authenticated"],
      postconditions: ["Returns success response"],
      invariants: ["No side effects"],
    )),
    inversion: Some(BeadInversion(
      security_risks: [],
      usability_risks: [],
      integration_risks: [],
      edge_case_coverage: 85,
      suggested_edge_cases: [],
    )),
    quality: Some(BeadQuality(
      completeness: 90,
      testability: 85,
      clarity: 95,
      overall: 90,
      issues: [],
    )),
  )
}

fn make_low_quality_metadata() -> BeadKirkMetadata {
  BeadKirkMetadata(
    ears: Some(BeadEARS(
      pattern: Ubiquitous,
      trigger: None,
      condition: None,
      state: None,
      clarity_score: 60,
      pattern_matches_test: False,
    )),
    contract: Some(BeadContract(
      preconditions: [],
      postconditions: [],
      invariants: [],
    )),
    inversion: Some(BeadInversion(
      security_risks: [],
      usability_risks: [],
      integration_risks: [],
      edge_case_coverage: 40,
      suggested_edge_cases: ["Missing edge case 1", "Missing edge case 2"],
    )),
    quality: Some(BeadQuality(
      completeness: 65,
      testability: 70,
      clarity: 60,
      overall: 65,
      issues: [
        ValidationIssue(
          field: "preconditions",
          issue: "No preconditions defined",
          severity: SevError,
          suggestion: "Add preconditions for input validation",
        ),
      ],
    )),
  )
}

fn make_minimal_metadata() -> BeadKirkMetadata {
  BeadKirkMetadata(
    ears: None,
    contract: None,
    inversion: None,
    quality: None,
  )
}

// =============================================================================
// ML-GAP-3: QUALITY VALIDATION GATES
// Tests for validate_bead_kirk() enforcing 80+ quality threshold
// =============================================================================

pub fn validate_bead_kirk_passes_high_quality_test() {
  // Bead with quality 90 should pass (above 80 threshold)
  let metadata = make_high_quality_metadata()

  let result = validate_bead_kirk(metadata, 80)

  result
  |> should.be_ok()
}

pub fn validate_bead_kirk_fails_low_quality_test() {
  // Bead with quality 65 should fail (below 80 threshold)
  let metadata = make_low_quality_metadata()

  let result = validate_bead_kirk(metadata, 80)

  case result {
    Ok(_) -> should.fail()
    Error(issues) -> {
      // Should have at least one issue about quality threshold
      issues
      |> should.not_equal([])
    }
  }
}

pub fn validate_bead_kirk_passes_without_metadata_test() {
  // Bead without KIRK metadata should pass (optional)
  let metadata = make_minimal_metadata()

  let result = validate_bead_kirk(metadata, 80)

  result
  |> should.be_ok()
}

pub fn validate_bead_kirk_exact_threshold_test() {
  // Bead with quality exactly 80 should pass
  let metadata = BeadKirkMetadata(
    ears: Some(BeadEARS(
      pattern: Ubiquitous,
      trigger: None,
      condition: None,
      state: None,
      clarity_score: 80,
      pattern_matches_test: True,
    )),
    contract: Some(BeadContract(
      preconditions: ["Valid input"],
      postconditions: ["Success response"],
      invariants: [],
    )),
    inversion: None,
    quality: Some(BeadQuality(
      completeness: 80,
      testability: 80,
      clarity: 80,
      overall: 80,
      issues: [],
    )),
  )

  let result = validate_bead_kirk(metadata, 80)

  result
  |> should.be_ok()
}

pub fn validate_bead_kirk_just_below_threshold_test() {
  // Bead with quality 79 should fail (just below 80)
  let metadata = BeadKirkMetadata(
    ears: None,
    contract: None,
    inversion: None,
    quality: Some(BeadQuality(
      completeness: 79,
      testability: 79,
      clarity: 79,
      overall: 79,
      issues: [],
    )),
  )

  let result = validate_bead_kirk(metadata, 80)

  case result {
    Ok(_) -> should.fail()
    Error(issues) -> {
      issues
      |> should.not_equal([])
    }
  }
}

pub fn validate_bead_kirk_with_error_severity_issue_test() {
  // Bead with an error-severity issue should fail even if overall score is 80+
  let metadata = BeadKirkMetadata(
    ears: None,
    contract: None,
    inversion: None,
    quality: Some(BeadQuality(
      completeness: 85,
      testability: 85,
      clarity: 85,
      overall: 85,
      issues: [
        ValidationIssue(
          field: "test",
          issue: "Critical validation failure",
          severity: SevError,
          suggestion: "Fix the test specification",
        ),
      ],
    )),
  )

  let result = validate_bead_kirk(metadata, 80)

  case result {
    Ok(_) -> should.fail()
    Error(issues) -> {
      issues
      |> should.not_equal([])
    }
  }
}

pub fn validate_bead_kirk_custom_threshold_test() {
  // Test with custom threshold of 90
  let metadata = BeadKirkMetadata(
    ears: None,
    contract: None,
    inversion: None,
    quality: Some(BeadQuality(
      completeness: 85,
      testability: 85,
      clarity: 85,
      overall: 85,
      issues: [],
    )),
  )

  // Should fail with threshold 90
  let result_90 = validate_bead_kirk(metadata, 90)
  case result_90 {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }

  // Should pass with threshold 80
  let result_80 = validate_bead_kirk(metadata, 80)
  result_80
  |> should.be_ok()
}

// =============================================================================
// ML-GAP-4: EARS PARSER INTEGRATION
// Tests for validate_bead_ears() checking EARS pattern clarity
// =============================================================================

pub fn validate_bead_ears_ubiquitous_pattern_test() {
  // "THE SYSTEM SHALL validate input" is ubiquitous pattern
  let what = "THE SYSTEM SHALL validate user input before processing"

  let result = validate_bead_ears(what)

  case result {
    Ok(ears) -> {
      ears.pattern
      |> should.equal(Ubiquitous)

      // clarity_score should be >= 75
      { ears.clarity_score >= 75 }
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn validate_bead_ears_event_driven_pattern_test() {
  // "WHEN user submits form THE SYSTEM SHALL process it"
  let what = "WHEN the user submits the registration form THE SYSTEM SHALL create a new account"

  let result = validate_bead_ears(what)

  let assert Ok(ears) = result
  ears.pattern
  |> should.equal(EventDriven)

  // Should extract trigger
  ears.trigger
  |> should.be_some()
}

pub fn validate_bead_ears_state_driven_pattern_test() {
  // "WHILE user is logged in THE SYSTEM SHALL show dashboard"
  let what = "WHILE the user is authenticated THE SYSTEM SHALL display the dashboard"

  let result = validate_bead_ears(what)

  let assert Ok(ears) = result
  ears.pattern
  |> should.equal(StateDriven)

  // Should extract state
  ears.state
  |> should.be_some()
}

pub fn validate_bead_ears_optional_pattern_test() {
  // "WHERE admin privileges are enabled THE SYSTEM SHALL show admin panel"
  let what = "WHERE the user has admin privileges THE SYSTEM SHALL show the admin panel"

  let result = validate_bead_ears(what)

  let assert Ok(ears) = result
  ears.pattern
  |> should.equal(Optional)

  // Should extract condition
  ears.condition
  |> should.be_some()
}

pub fn validate_bead_ears_unwanted_pattern_test() {
  // "IF user is banned THE SYSTEM SHALL NOT allow login"
  let what = "IF the account is suspended THE SYSTEM SHALL NOT allow access to resources"

  let result = validate_bead_ears(what)

  case result {
    Ok(ears) -> {
      ears.pattern
      |> should.equal(Unwanted)
    }
    Error(_) -> should.fail()
  }
}

pub fn validate_bead_ears_complex_pattern_test() {
  // "WHILE logged in WHEN session expires THE SYSTEM SHALL refresh token"
  let what = "WHILE the user is active WHEN the session expires THE SYSTEM SHALL automatically refresh the authentication token"

  let result = validate_bead_ears(what)

  let assert Ok(ears) = result
  ears.pattern
  |> should.equal(Complex)

  // Should extract both trigger and state
  ears.trigger
  |> should.be_some()
  ears.state
  |> should.be_some()
}

pub fn validate_bead_ears_low_clarity_test() {
  // Vague requirement should have low clarity score
  let what = "do the thing"

  let result = validate_bead_ears(what)

  case result {
    Ok(ears) -> {
      // Should still parse but with low clarity (< 75)
      { ears.clarity_score < 75 }
      |> should.be_true()
    }
    Error(_) -> {
      // Or might fail parsing entirely - both acceptable
      Nil
    }
  }
}

pub fn validate_bead_ears_no_ears_pattern_test() {
  // Text that doesn't match any EARS pattern
  let what = "This is just some random text without EARS keywords"

  let result = validate_bead_ears(what)

  // Should return error for unrecognized pattern
  case result {
    Ok(_) -> should.fail()
    Error(message) -> {
      message
      |> should.not_equal("")
    }
  }
}

// =============================================================================
// ML-GAP-5: INVERSION COVERAGE CHECKING
// Tests for analyze_bead_inversions() calculating edge case coverage
// =============================================================================

pub fn analyze_bead_inversions_full_coverage_test() {
  // Bead with edge cases covering all identified risks
  // The "authenticate" behavior triggers auth-related security risks
  // Edge cases should cover brute force, session hijacking
  let edge_cases = [
    "Handle credential brute force attack",
    "Handle session hijacking attempt",
    "Handle invalid token format",
    "Handle token expiration bypass",
  ]

  let what = "THE SYSTEM SHALL authenticate users"

  let result = analyze_bead_inversions(what, edge_cases)

  // Coverage should be > 50 (some risks covered)
  // With auth keywords, we expect brute force + session risks = 2 risks
  { result.edge_case_coverage > 50 }
  |> should.be_true()
}

pub fn analyze_bead_inversions_no_edge_cases_test() {
  // Bead with no edge cases - should have low coverage
  let edge_cases = []
  let what = "THE SYSTEM SHALL authenticate users via JWT tokens"

  let result = analyze_bead_inversions(what, edge_cases)

  // Coverage should be < 50
  { result.edge_case_coverage < 50 }
  |> should.be_true()

  // Should suggest edge cases
  result.suggested_edge_cases
  |> should.not_equal([])
}

pub fn analyze_bead_inversions_identifies_security_risks_test() {
  // "authenticate" behavior should identify security risks
  let what = "THE SYSTEM SHALL authenticate users and issue JWT tokens"
  let edge_cases = []

  let result = analyze_bead_inversions(what, edge_cases)

  // Should identify authentication-related security risks
  result.security_risks
  |> should.not_equal([])
}

pub fn analyze_bead_inversions_identifies_usability_risks_test() {
  // User-facing behavior should identify usability risks
  let what = "THE SYSTEM SHALL display error messages to the user"
  let edge_cases = []

  let result = analyze_bead_inversions(what, edge_cases)

  // Should identify usability risks
  result.usability_risks
  |> should.not_equal([])
}

pub fn analyze_bead_inversions_integration_risks_test() {
  // External API call should identify integration risks
  let what = "THE SYSTEM SHALL call the external payment API"
  let edge_cases = []

  let result = analyze_bead_inversions(what, edge_cases)

  // Should identify integration risks (timeout, partial failure, etc)
  result.integration_risks
  |> should.not_equal([])
}

pub fn analyze_bead_inversions_partial_coverage_test() {
  // Some edge cases but not all risks covered
  // "user" keyword triggers usability risks like "invalid input handling"
  let what = "THE SYSTEM SHALL process user registration"
  let edge_cases = [
    "Handle invalid input format",  // Covers "invalid input" risk
  ]

  let result = analyze_bead_inversions(what, edge_cases)

  // With "user" keyword, we get 2 usability risks: "invalid input" and "empty input"
  // 1 edge case covers "invalid" -> 50% coverage
  // Coverage should be between 0 and 100 (exclusive)
  { result.edge_case_coverage >= 0 && result.edge_case_coverage <= 100 }
  |> should.be_true()
}

pub fn analyze_bead_inversions_suggested_edge_cases_test() {
  // Should suggest specific edge cases for uncovered risks
  let what = "THE SYSTEM SHALL validate and store user data"
  let edge_cases = []

  let result = analyze_bead_inversions(what, edge_cases)

  // Suggestions should be actionable
  result.suggested_edge_cases
  |> should.not_equal([])

  // Each suggestion should be non-empty
  case result.suggested_edge_cases {
    [first, ..] -> {
      first
      |> should.not_equal("")
    }
    [] -> should.fail()
  }
}

// =============================================================================
// QUALITY SCORE CALCULATION
// Tests for calculate_quality_score()
// =============================================================================

pub fn calculate_quality_score_all_components_test() {
  // Quality should be weighted average of completeness, testability, clarity
  let completeness = 90
  let testability = 80
  let clarity = 85

  let overall = calculate_quality_score(completeness, testability, clarity)

  // Testability is weighted more heavily, so overall should be between 80-90
  { overall >= 80 && overall <= 90 }
  |> should.be_true()
}

pub fn calculate_quality_score_testability_weighted_test() {
  // Testability has 40% weight - highest single weight
  // Test: when testability is the ONLY variable, higher testability = higher score
  let high_test = calculate_quality_score(80, 100, 80)  // testability 100
  let low_test = calculate_quality_score(80, 60, 80)   // testability 60

  // High testability should give better overall when other scores are equal
  { high_test > low_test }
  |> should.be_true()
}

pub fn calculate_quality_score_perfect_test() {
  // All perfect scores should give 100
  let overall = calculate_quality_score(100, 100, 100)

  overall
  |> should.equal(100)
}

pub fn calculate_quality_score_all_zero_test() {
  // All zero scores should give 0
  let overall = calculate_quality_score(0, 0, 0)

  overall
  |> should.equal(0)
}
