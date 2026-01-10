// Bead Validator Module
// Implements ML-GAP-3, ML-GAP-4, ML-GAP-5 from Mental Lattice beads
//
// - validate_bead_kirk(): Enforces quality threshold (80+ by default)
// - validate_bead_ears(): Validates EARS pattern clarity using ears_parser
// - analyze_bead_inversions(): Calculates edge case coverage using inversion_checker

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/kirk/ears_parser.{
  type EarsPattern, type EarsRequirement, Complex, EventDriven, Optional,
  StateDriven, Ubiquitous, Unwanted,
}

// =============================================================================
// TYPES
// =============================================================================

/// KIRK metadata attached to a bead for quality validation
pub type BeadKirkMetadata {
  BeadKirkMetadata(
    ears: Option(BeadEARS),
    contract: Option(BeadContract),
    inversion: Option(BeadInversion),
    quality: Option(BeadQuality),
  )
}

/// EARS analysis for a bead's "what" field
pub type BeadEARS {
  BeadEARS(
    pattern: EarsPattern,
    trigger: Option(String),
    condition: Option(String),
    state: Option(String),
    clarity_score: Int,
    pattern_matches_test: Bool,
  )
}

/// Design by Contract for a bead
pub type BeadContract {
  BeadContract(
    preconditions: List(String),
    postconditions: List(String),
    invariants: List(String),
  )
}

/// Inversion analysis results
pub type BeadInversion {
  BeadInversion(
    security_risks: List(IdentifiedRisk),
    usability_risks: List(IdentifiedRisk),
    integration_risks: List(IdentifiedRisk),
    edge_case_coverage: Int,
    suggested_edge_cases: List(String),
  )
}

/// An identified risk from inversion analysis
pub type IdentifiedRisk {
  IdentifiedRisk(risk: String, severity: RiskSeverity, mitigation: String)
}

/// Risk severity levels
pub type RiskSeverity {
  Low
  Medium
  High
  Critical
}

/// Quality scores for a bead
pub type BeadQuality {
  BeadQuality(
    completeness: Int,
    testability: Int,
    clarity: Int,
    overall: Int,
    issues: List(ValidationIssue),
  )
}

/// A validation issue found during analysis
pub type ValidationIssue {
  ValidationIssue(
    field: String,
    issue: String,
    severity: IssueSeverity,
    suggestion: String,
  )
}

/// Issue severity levels
pub type IssueSeverity {
  Info
  Warning
  SevError
}

// =============================================================================
// ML-GAP-3: QUALITY VALIDATION GATES
// Enforces quality threshold before bead execution
// =============================================================================

/// Validate bead KIRK metadata against quality threshold
/// Returns Ok(Nil) if validation passes, Error(List(ValidationIssue)) if fails
///
/// Validation rules:
/// 1. Beads without KIRK metadata pass (metadata is optional)
/// 2. If quality.overall is present, it must be >= threshold
/// 3. If any issue has severity SevError, validation fails
pub fn validate_bead_kirk(
  metadata: BeadKirkMetadata,
  threshold: Int,
) -> Result(Nil, List(ValidationIssue)) {
  let issues = collect_validation_issues(metadata, threshold)

  case list.is_empty(issues) {
    True -> Ok(Nil)
    False -> Error(issues)
  }
}

fn collect_validation_issues(
  metadata: BeadKirkMetadata,
  threshold: Int,
) -> List(ValidationIssue) {
  let quality_issues = case metadata.quality {
    None -> []
    Some(quality) -> {
      let threshold_issues = case quality.overall < threshold {
        True -> [
          ValidationIssue(
            field: "quality.overall",
            issue: "Quality score "
              <> int.to_string(quality.overall)
              <> " is below threshold "
              <> int.to_string(threshold),
            severity: SevError,
            suggestion: "Improve bead quality by adding preconditions, edge cases, and clearer requirements",
          ),
        ]
        False -> []
      }

      // Check for error-severity issues in quality.issues
      let error_issues =
        quality.issues
        |> list.filter(fn(issue) { issue.severity == SevError })

      list.concat([threshold_issues, error_issues])
    }
  }

  // Check EARS if present
  let ears_issues = case metadata.ears {
    None -> []
    Some(ears) -> {
      case ears.pattern_matches_test {
        True -> []
        False -> [
          ValidationIssue(
            field: "ears.pattern_matches_test",
            issue: "EARS pattern does not match test specification",
            severity: Warning,
            suggestion: "Ensure the requirement pattern aligns with how the test verifies it",
          ),
        ]
      }
    }
  }

  // Check contract if present
  let contract_issues = case metadata.contract {
    None -> []
    Some(contract) -> {
      case list.is_empty(contract.preconditions) {
        True -> [
          ValidationIssue(
            field: "contract.preconditions",
            issue: "No preconditions defined",
            severity: Warning,
            suggestion: "Add preconditions to specify what must be true before execution",
          ),
        ]
        False -> []
      }
    }
  }

  list.concat([quality_issues, ears_issues, contract_issues])
}

// =============================================================================
// ML-GAP-4: EARS PARSER INTEGRATION
// Validates bead "what" field clarity using EARS patterns
// =============================================================================

/// Validate EARS pattern of a bead's "what" field
/// Returns Ok(BeadEARS) with extracted pattern and clarity score
/// Returns Error(String) if no EARS pattern can be identified
pub fn validate_bead_ears(what: String) -> Result(BeadEARS, String) {
  let parse_result = ears_parser.parse(what)

  case parse_result.requirements {
    [] -> {
      // No EARS pattern found - check if it's completely unparseable
      case list.is_empty(parse_result.errors) {
        True ->
          Error(
            "No EARS pattern found in requirement. Use patterns like 'THE SYSTEM SHALL' or 'WHEN [trigger] THE SYSTEM SHALL'",
          )
        False -> {
          let error_messages =
            parse_result.errors
            |> list.map(fn(e) { e.message })
            |> string.join("; ")
          Error("Failed to parse EARS pattern: " <> error_messages)
        }
      }
    }
    [req, ..] -> {
      // Successfully parsed - extract EARS data
      let clarity_score = calculate_clarity_score(req, what)

      Ok(BeadEARS(
        pattern: req.pattern,
        trigger: req.trigger,
        condition: req.condition,
        state: req.state,
        clarity_score: clarity_score,
        pattern_matches_test: True,
      ))
    }
  }
}

/// Calculate clarity score (0-100) based on requirement characteristics
fn calculate_clarity_score(req: EarsRequirement, raw_text: String) -> Int {
  let base_score = 50

  // Bonus for having clear pattern keywords
  let pattern_bonus = case req.pattern {
    Ubiquitous -> 15
    EventDriven -> 20
    StateDriven -> 20
    Optional -> 20
    Unwanted -> 25
    Complex -> 25
  }

  // Bonus for adequate length (not too short, not too long)
  let length = string.length(raw_text)
  let length_bonus = case True {
    _ if length < 20 -> 0
    _ if length >= 20 && length <= 200 -> 15
    _ -> 10
  }

  // Bonus for having specific behavioral content
  let behavior_length = string.length(req.system_shall)
  let behavior_bonus = case True {
    _ if behavior_length < 10 -> 0
    _ if behavior_length >= 10 && behavior_length <= 100 -> 10
    _ -> 5
  }

  // Penalty for vague words
  let lower = string.lowercase(raw_text)
  let vague_penalty = case
    string.contains(lower, "thing")
    || string.contains(lower, "stuff")
    || string.contains(lower, "somehow")
    || string.contains(lower, "maybe")
  {
    True -> -15
    False -> 0
  }

  int.min(
    100,
    base_score + pattern_bonus + length_bonus + behavior_bonus + vague_penalty,
  )
}

// =============================================================================
// ML-GAP-5: INVERSION COVERAGE CHECKING
// Analyzes edge case coverage using inversion thinking
// =============================================================================

/// Analyze bead for inversion risks and edge case coverage
/// Returns BeadInversion with identified risks, coverage percentage, and suggestions
pub fn analyze_bead_inversions(
  what: String,
  edge_cases: List(String),
) -> BeadInversion {
  let lower_what = string.lowercase(what)

  // Identify security risks based on keywords
  let security_risks = identify_security_risks(lower_what)

  // Identify usability risks
  let usability_risks = identify_usability_risks(lower_what)

  // Identify integration risks
  let integration_risks = identify_integration_risks(lower_what)

  // Calculate coverage
  let all_risks =
    list.length(security_risks)
    + list.length(usability_risks)
    + list.length(integration_risks)

  let covered_risks =
    count_covered_risks(
      edge_cases,
      security_risks,
      usability_risks,
      integration_risks,
    )

  let coverage = case all_risks {
    0 -> 100
    _ -> { covered_risks * 100 } / all_risks
  }

  // Generate suggestions for uncovered risks
  let suggested =
    generate_edge_case_suggestions(
      edge_cases,
      security_risks,
      usability_risks,
      integration_risks,
    )

  BeadInversion(
    security_risks: security_risks,
    usability_risks: usability_risks,
    integration_risks: integration_risks,
    edge_case_coverage: coverage,
    suggested_edge_cases: suggested,
  )
}

/// Identify security risks based on behavior keywords
fn identify_security_risks(what: String) -> List(IdentifiedRisk) {
  let risks = []

  // Authentication-related risks
  let risks = case
    string.contains(what, "auth")
    || string.contains(what, "login")
    || string.contains(what, "password")
  {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "Credential brute force",
            severity: High,
            mitigation: "Implement rate limiting and account lockout",
          ),
          IdentifiedRisk(
            risk: "Session hijacking",
            severity: High,
            mitigation: "Use secure session tokens with short expiry",
          ),
        ],
      ])
    False -> risks
  }

  // Token-related risks
  let risks = case
    string.contains(what, "jwt") || string.contains(what, "token")
  {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "Token expiration bypass",
            severity: High,
            mitigation: "Validate token expiration on every request",
          ),
          IdentifiedRisk(
            risk: "Invalid token format",
            severity: Medium,
            mitigation: "Validate token structure before processing",
          ),
        ],
      ])
    False -> risks
  }

  // Data storage risks
  let risks = case
    string.contains(what, "store")
    || string.contains(what, "save")
    || string.contains(what, "create")
  {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "SQL injection",
            severity: Critical,
            mitigation: "Use parameterized queries",
          ),
        ],
      ])
    False -> risks
  }

  risks
}

/// Identify usability risks
fn identify_usability_risks(what: String) -> List(IdentifiedRisk) {
  let risks = []

  // Error display risks
  let risks = case
    string.contains(what, "error")
    || string.contains(what, "display")
    || string.contains(what, "show")
  {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "Unhelpful error messages",
            severity: Medium,
            mitigation: "Provide clear, actionable error messages",
          ),
        ],
      ])
    False -> risks
  }

  // User interaction risks
  let risks = case
    string.contains(what, "user")
    || string.contains(what, "form")
    || string.contains(what, "input")
  {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "Invalid input handling",
            severity: High,
            mitigation: "Validate and sanitize all user input",
          ),
          IdentifiedRisk(
            risk: "Empty input handling",
            severity: Medium,
            mitigation: "Define behavior for empty/whitespace input",
          ),
        ],
      ])
    False -> risks
  }

  // Validation risks
  let risks = case string.contains(what, "validat") {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "Missing required field handling",
            severity: High,
            mitigation: "Return clear errors for missing required fields",
          ),
        ],
      ])
    False -> risks
  }

  risks
}

/// Identify integration risks
fn identify_integration_risks(what: String) -> List(IdentifiedRisk) {
  let risks = []

  // External API risks
  let risks = case
    string.contains(what, "api")
    || string.contains(what, "external")
    || string.contains(what, "call")
  {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "External service timeout",
            severity: High,
            mitigation: "Implement timeout and retry logic",
          ),
          IdentifiedRisk(
            risk: "Partial failure in distributed call",
            severity: Medium,
            mitigation: "Implement idempotency and rollback",
          ),
        ],
      ])
    False -> risks
  }

  // Database risks
  let risks = case
    string.contains(what, "database")
    || string.contains(what, "query")
    || string.contains(what, "fetch")
  {
    True ->
      list.concat([
        risks,
        [
          IdentifiedRisk(
            risk: "Database connection failure",
            severity: High,
            mitigation: "Implement connection pooling and retry",
          ),
        ],
      ])
    False -> risks
  }

  risks
}

/// Count how many identified risks are covered by edge cases
fn count_covered_risks(
  edge_cases: List(String),
  security_risks: List(IdentifiedRisk),
  usability_risks: List(IdentifiedRisk),
  integration_risks: List(IdentifiedRisk),
) -> Int {
  let edge_cases_lower =
    edge_cases
    |> list.map(string.lowercase)

  let all_risks =
    list.concat([
      list.map(security_risks, fn(r) { r.risk }),
      list.map(usability_risks, fn(r) { r.risk }),
      list.map(integration_risks, fn(r) { r.risk }),
    ])

  all_risks
  |> list.filter(fn(risk) {
    let risk_lower = string.lowercase(risk)
    let risk_words =
      risk_lower
      |> string.split(" ")
      |> list.filter(fn(w) { string.length(w) > 3 })

    // Check if any edge case covers this risk
    list.any(edge_cases_lower, fn(ec) {
      list.any(risk_words, fn(word) { string.contains(ec, word) })
    })
  })
  |> list.length()
}

/// Generate edge case suggestions for uncovered risks
fn generate_edge_case_suggestions(
  edge_cases: List(String),
  security_risks: List(IdentifiedRisk),
  usability_risks: List(IdentifiedRisk),
  integration_risks: List(IdentifiedRisk),
) -> List(String) {
  let edge_cases_lower =
    edge_cases
    |> list.map(string.lowercase)

  let all_risks =
    list.concat([security_risks, usability_risks, integration_risks])

  all_risks
  |> list.filter_map(fn(risk) {
    let risk_lower = string.lowercase(risk.risk)
    let risk_words =
      risk_lower
      |> string.split(" ")
      |> list.filter(fn(w) { string.length(w) > 3 })

    // Check if this risk is NOT covered
    let is_covered =
      list.any(edge_cases_lower, fn(ec) {
        list.any(risk_words, fn(word) { string.contains(ec, word) })
      })

    case is_covered {
      True -> Error(Nil)
      False -> Ok("Handle " <> risk.risk <> ": " <> risk.mitigation)
    }
  })
  |> list.take(5)
}

// =============================================================================
// QUALITY SCORE CALCULATION
// Weighted average with emphasis on testability
// =============================================================================

/// Calculate overall quality score from components
/// Weights: completeness 25%, testability 40%, clarity 35%
pub fn calculate_quality_score(
  completeness: Int,
  testability: Int,
  clarity: Int,
) -> Int {
  // Testability is weighted most heavily as per user decision
  let weighted =
    int.to_float(completeness)
    *. 0.25
    +. int.to_float(testability)
    *. 0.4
    +. int.to_float(clarity)
    *. 0.35

  float.round(weighted)
}

// =============================================================================
// SEVERITY CONVERSION
// =============================================================================

pub fn severity_to_string(severity: IssueSeverity) -> String {
  case severity {
    Info -> "info"
    Warning -> "warning"
    SevError -> "error"
  }
}

pub fn risk_severity_to_string(severity: RiskSeverity) -> String {
  case severity {
    Low -> "low"
    Medium -> "medium"
    High -> "high"
    Critical -> "critical"
  }
}
