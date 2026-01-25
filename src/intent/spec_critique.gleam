/// Spec Critique Protocol - Adversarial QA Persona
///
/// This module implements validation logic for the Spec phase following
/// the "Adversarial QA" critique protocol from INTENT_4_PLAN.md
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import intent/types.{type Spec}

// =============================================================================
// Types - Spec Phase Specific
// =============================================================================

/// The three critique questions from Adversarial QA
pub type CritiqueQuestion {
  CoverageGaps
  EdgeCaseGaps
  FailureBlastRadius
}

/// Issue severity levels
pub type Severity {
  Critical
  Warning
}

/// A single critique issue with context and suggestion
pub type CritiqueIssue {
  CritiqueIssue(
    question: CritiqueQuestion,
    severity: Severity,
    message: String,
    suggestion: String,
  )
}

/// Overall critique result with pass/fail and scoring
pub type CritiqueResult {
  CritiqueResult(passed: Bool, issues: List(CritiqueIssue), score: Int)
}

// =============================================================================
// Constants
// =============================================================================

const min_behaviors = 2

const min_edge_case_ratio = 0.3

const min_anti_patterns = 1

const pass_threshold = 70

const critical_penalty = 25

const warning_penalty = 5

// Edge case indicator keywords
const edge_case_keywords = [
  "edge", "edge-case", "boundary", "limit", "max", "empty", "null", "duplicate",
  "validation", "invalid",
]

// =============================================================================
// Helper Functions (DRY)
// =============================================================================

/// Add an issue to the issues list (eliminates duplication)
fn add_issue(
  issues: List(CritiqueIssue),
  question: CritiqueQuestion,
  severity: Severity,
  message: String,
  suggestion: String,
) -> List(CritiqueIssue) {
  list.append(issues, [CritiqueIssue(question, severity, message, suggestion)])
}

/// Get all behaviors from a spec
fn get_all_behaviors(spec: Spec) -> List(types.Behavior) {
  spec.features
  |> list.flat_map(fn(feature) { feature.behaviors })
}

/// Count unique HTTP methods used
fn count_unique_methods(behaviors: List(types.Behavior)) -> Int {
  behaviors
  |> list.map(fn(b) { types.method_to_string(b.request.method) })
  |> list.unique
  |> list.length
}

/// Count behaviors with error status codes (4xx or 5xx)
fn count_error_behaviors(behaviors: List(types.Behavior)) -> Int {
  behaviors
  |> list.filter(fn(b) { b.response.status >= 400 })
  |> list.length
}

/// Check if a behavior has edge case tags
fn has_edge_case_tag(behavior: types.Behavior) -> Bool {
  behavior.tags
  |> list.any(fn(tag) {
    edge_case_keywords
    |> list.any(fn(keyword) { string.contains(string.lowercase(tag), keyword) })
  })
}

// =============================================================================
// Coverage Gaps Validation
// =============================================================================

/// Validate "What's NOT tested?"
pub fn validate_coverage_gaps(spec: Spec) -> List(CritiqueIssue) {
  let issues = []
  let behaviors = get_all_behaviors(spec)
  let behavior_count = list.length(behaviors)

  // Check for minimum behaviors
  let issues = case behavior_count {
    0 ->
      add_issue(
        issues,
        CoverageGaps,
        Critical,
        "No behaviors defined in spec",
        "Add at least "
          <> int.to_string(min_behaviors)
          <> " behaviors covering happy path and error cases",
      )
    1 ->
      add_issue(
        issues,
        CoverageGaps,
        Critical,
        "Only 1 behavior defined, need at least "
          <> int.to_string(min_behaviors),
        "Add behaviors for different scenarios, including error cases",
      )
    _ -> issues
  }

  // Check for HTTP method diversity
  let issues = case behavior_count > 0 {
    True -> {
      let method_count = count_unique_methods(behaviors)
      case method_count {
        1 ->
          add_issue(
            issues,
            CoverageGaps,
            Warning,
            "Only 1 HTTP method tested (found "
              <> int.to_string(behavior_count)
              <> " behavior(s))",
            "Add behaviors testing different HTTP methods (GET, POST, PUT, DELETE, etc.)",
          )
        _ -> issues
      }
    }
    False -> issues
  }

  // Check for error behavior coverage
  let issues = case behavior_count > 0 {
    True -> {
      let error_count = count_error_behaviors(behaviors)
      case error_count {
        0 ->
          add_issue(
            issues,
            CoverageGaps,
            Critical,
            "No error behaviors tested (all status codes < 400)",
            "Add behaviors testing error cases: 404 Not Found, 422 Validation Error, 409 Conflict, 500 Server Error, etc.",
          )
        _ -> issues
      }
    }
    False -> issues
  }

  // Check for features with insufficient behaviors
  let issues =
    spec.features
    |> list.fold(issues, fn(acc, feature) {
      let feature_behavior_count = list.length(feature.behaviors)
      case feature_behavior_count {
        0 ->
          add_issue(
            acc,
            CoverageGaps,
            Critical,
            "Feature '" <> feature.name <> "' has no behaviors",
            "Add at least 2 behaviors per feature (happy path + error case)",
          )
        1 ->
          add_issue(
            acc,
            CoverageGaps,
            Warning,
            "Feature '" <> feature.name <> "' has only 1 behavior",
            "Add error case behaviors to validate failure modes",
          )
        _ -> acc
      }
    })

  issues
}

// =============================================================================
// Edge Case Validation
// =============================================================================

/// Validate "What edge cases break this?"
pub fn validate_edge_cases(spec: Spec) -> List(CritiqueIssue) {
  let issues = []
  let behaviors = get_all_behaviors(spec)
  let behavior_count = list.length(behaviors)

  // Check for edge case tags
  let issues = case behavior_count > 0 {
    True -> {
      let edge_case_count =
        behaviors
        |> list.filter(has_edge_case_tag)
        |> list.length

      let edge_case_ratio = case behavior_count {
        0 -> 0.0
        n -> int.to_float(edge_case_count) /. int.to_float(n)
      }

      case edge_case_ratio <. min_edge_case_ratio {
        True ->
          add_issue(
            issues,
            EdgeCaseGaps,
            Warning,
            "Only "
              <> int.to_string(edge_case_count)
              <> " of "
              <> int.to_string(behavior_count)
              <> " behaviors have edge case tags ("
              <> int.to_string(float.round(edge_case_ratio *. 100.0))
              <> "%, need "
              <> int.to_string(float.round(min_edge_case_ratio *. 100.0))
              <> "%)",
            "Add tags to behaviors that test edge cases: 'edge-case', 'boundary', 'validation', 'empty', 'null', 'duplicate', etc.",
          )
        False -> issues
      }
    }
    False -> issues
  }

  // Check for anti-patterns
  let issues = case list.length(spec.anti_patterns) {
    0 ->
      add_issue(
        issues,
        EdgeCaseGaps,
        Critical,
        "No anti-patterns defined",
        "Document at least "
          <> int.to_string(min_anti_patterns)
          <> " anti-pattern(s) showing bad vs good examples of common mistakes",
      )
    _ -> issues
  }

  issues
}

// =============================================================================
// Failure Blast Radius Validation
// =============================================================================

/// Validate "What's the blast radius of failures?"
pub fn validate_failure_blast_radius(spec: Spec) -> List(CritiqueIssue) {
  let issues = []
  let behaviors = get_all_behaviors(spec)

  // Check for error behaviors (already covered in coverage_gaps but critical for blast radius)
  let error_count = count_error_behaviors(behaviors)
  let issues = case error_count {
    0 ->
      add_issue(
        issues,
        FailureBlastRadius,
        Critical,
        "No error behaviors defined - cannot assess failure impact",
        "Add behaviors that test error responses (4xx client errors, 5xx server errors) to understand failure propagation",
      )
    _ -> issues
  }

  // Check for AI hints about pitfalls
  let issues = case spec.ai_hints.pitfalls {
    [] ->
      add_issue(
        issues,
        FailureBlastRadius,
        Warning,
        "No pitfalls documented in ai_hints",
        "Document common failure modes, edge cases, and gotchas in ai_hints.pitfalls",
      )
    _ -> issues
  }

  // Check for behaviors with dependency chains (requires)
  let has_dependencies =
    behaviors
    |> list.any(fn(b) { b.requires != [] })

  let issues = case has_dependencies {
    False ->
      add_issue(
        issues,
        FailureBlastRadius,
        Warning,
        "No behaviors define dependencies (requires field)",
        "Use the 'requires' field to document behavior dependencies and failure cascades",
      )
    True -> issues
  }

  issues
}

// =============================================================================
// Overall Critique
// =============================================================================

/// Run all critique validators and aggregate results with scoring
pub fn critique_spec(spec: Spec) -> CritiqueResult {
  let coverage_issues = validate_coverage_gaps(spec)
  let edge_case_issues = validate_edge_cases(spec)
  let blast_radius_issues = validate_failure_blast_radius(spec)

  let all_issues =
    list.flatten([coverage_issues, edge_case_issues, blast_radius_issues])

  let critical_count =
    all_issues
    |> list.filter(fn(issue) {
      case issue.severity {
        Critical -> True
        Warning -> False
      }
    })
    |> list.length

  let warning_count =
    all_issues
    |> list.filter(fn(issue) {
      case issue.severity {
        Warning -> True
        Critical -> False
      }
    })
    |> list.length

  let raw_score =
    100
    - { critical_count * critical_penalty }
    - { warning_count * warning_penalty }
  let score = case raw_score < 0 {
    True -> 0
    False -> raw_score
  }

  let passed = score >= pass_threshold

  CritiqueResult(passed: passed, issues: all_issues, score: score)
}
