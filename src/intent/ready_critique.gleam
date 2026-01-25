/// Ready Critique Protocol - Pre-Launch Auditor Persona
///
/// This module implements validation logic for the Ready phase following
/// the "Pre-Launch Auditor" critique protocol from INTENT_4_PLAN.md
import gleam/int
import gleam/list
import gleam/result
import intent/planning_types.{
  type ReadyReport, Critical as BlockerCritical, High as BlockerHigh,
  Medium as BlockerMedium,
}

// =============================================================================
// Types - Ready Phase Specific
// =============================================================================

/// The three critique questions from Pre-Launch Auditor
pub type CritiqueQuestion {
  VisionAlignment
  SuccessCriteria
  RollbackPlan
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

const min_replacement_score = 60

const min_empathy_score = 50

const min_actionable_score = 50

const min_yet_complete_score = 60

const min_overall_readiness = 70

const caution_overall_readiness = 80

const max_critical_blockers = 0

const max_high_blockers = 2

const pass_threshold = 70

const critical_penalty = 25

const warning_penalty = 5

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

// =============================================================================
// Vision Alignment Validation - "Did we stay true to the vision?"
// =============================================================================

/// Validate that the implementation stayed true to the original vision
pub fn validate_vision_alignment(ready: ReadyReport) -> List(CritiqueIssue) {
  let issues = []

  // Check Replacement dimension (VORP/value proposition)
  let issues = case ready.replacement.score {
    s if s < 40 ->
      add_issue(
        issues,
        VisionAlignment,
        Critical,
        "Replacement score critically low ("
          <> int.to_string(s)
          <> "%) - value proposition unclear or lost",
        "Revisit the original vision VORP and ensure the spec clearly addresses the core problem with measurable improvements",
      )
    s if s < min_replacement_score ->
      add_issue(
        issues,
        VisionAlignment,
        Warning,
        "Replacement score below threshold ("
          <> int.to_string(s)
          <> "% < "
          <> int.to_string(min_replacement_score)
          <> "%) - value proposition may have drifted",
        "Review vision alignment: ensure audience, success criteria, and description match original VORP promise",
      )
    _ -> issues
  }

  // Check if Replacement has specific issues
  let issues = case list.is_empty(ready.replacement.issues) {
    False ->
      add_issue(
        issues,
        VisionAlignment,
        Warning,
        "Replacement dimension has "
          <> int.to_string(list.length(ready.replacement.issues))
          <> " issue(s): "
          <> list.first(ready.replacement.issues)
        |> result.unwrap("unknown"),
        "Address replacement issues to ensure vision alignment remains strong",
      )
    True -> issues
  }

  issues
}

// =============================================================================
// Success Criteria Validation - "Are all success criteria met?"
// =============================================================================

/// Validate that all success criteria from the vision are met
pub fn validate_success_criteria(ready: ReadyReport) -> List(CritiqueIssue) {
  let issues = []

  // Check Yet-complete dimension (completeness of implementation)
  let issues = case ready.yet_complete.score {
    s if s < 40 ->
      add_issue(
        issues,
        SuccessCriteria,
        Critical,
        "Yet-complete score critically low ("
          <> int.to_string(s)
          <> "%) - specification incomplete",
        "Complete missing sections: features, behaviors, rules, and AI hints before proceeding to implementation",
      )
    s if s < min_yet_complete_score ->
      add_issue(
        issues,
        SuccessCriteria,
        Warning,
        "Yet-complete score below threshold ("
          <> int.to_string(s)
          <> "% < "
          <> int.to_string(min_yet_complete_score)
          <> "%) - gaps in implementation readiness",
        "Fill gaps in specification to ensure all success criteria can be validated",
      )
    _ -> issues
  }

  // Check for critical blockers
  let critical_blocker_count =
    ready.blockers
    |> list.filter(fn(blocker) {
      case blocker.severity {
        BlockerCritical -> True
        _ -> False
      }
    })
    |> list.length

  let issues = case critical_blocker_count > max_critical_blockers {
    True ->
      add_issue(
        issues,
        SuccessCriteria,
        Critical,
        int.to_string(critical_blocker_count)
          <> " critical blocker(s) prevent launch",
        "Resolve all critical blockers before proceeding - these represent fundamental gaps in readiness",
      )
    False -> issues
  }

  // Check Empathy and Actionable (user experience criteria)
  let issues = case
    ready.empathy.score < min_empathy_score
    || ready.actionable.score < min_actionable_score
  {
    True ->
      add_issue(
        issues,
        SuccessCriteria,
        Warning,
        "User experience dimensions (Empathy: "
          <> int.to_string(ready.empathy.score)
          <> "%, Actionable: "
          <> int.to_string(ready.actionable.score)
          <> "%) below thresholds",
        "Improve error handling, validation behaviors, and response checks to meet UX success criteria",
      )
    False -> issues
  }

  issues
}

// =============================================================================
// Rollback Plan Validation - "What's the rollback plan?"
// =============================================================================

/// Validate that there's a clear path forward or ability to rollback
pub fn validate_rollback_plan(ready: ReadyReport) -> List(CritiqueIssue) {
  let issues = []

  // Check overall readiness
  let issues = case ready.overall_readiness {
    r if r < min_overall_readiness ->
      add_issue(
        issues,
        RollbackPlan,
        Critical,
        "Overall readiness critically low ("
          <> int.to_string(r)
          <> "% < "
          <> int.to_string(min_overall_readiness)
          <> "%) - not ready for production",
        "Do not proceed with implementation - address critical gaps first. Consider breaking into smaller MVPs or deferring scope",
      )
    r if r < caution_overall_readiness ->
      add_issue(
        issues,
        RollbackPlan,
        Warning,
        "Overall readiness below caution threshold ("
          <> int.to_string(r)
          <> "% < "
          <> int.to_string(caution_overall_readiness)
          <> "%) - proceed with extreme caution",
        "Plan for beta/experimental launch with clear rollback criteria and monitoring. Document what success looks like and when to abort",
      )
    _ -> issues
  }

  // Check for high-severity blockers
  let high_blocker_count =
    ready.blockers
    |> list.filter(fn(blocker) {
      case blocker.severity {
        BlockerHigh -> True
        BlockerMedium -> True
        _ -> False
      }
    })
    |> list.length

  let issues = case high_blocker_count > max_high_blockers {
    True ->
      add_issue(
        issues,
        RollbackPlan,
        Warning,
        int.to_string(high_blocker_count)
          <> " high/medium blocker(s) increase risk",
        "Reduce blocker count to "
          <> int.to_string(max_high_blockers)
          <> " or less before launch, or document explicit rollback triggers",
      )
    False -> issues
  }

  // Check Discoverable dimension (operational safety)
  let issues = case ready.discoverable.score {
    s if s < 40 ->
      add_issue(
        issues,
        RollbackPlan,
        Warning,
        "Discoverable score low ("
          <> int.to_string(s)
          <> "%) - poor organization increases rollback complexity",
        "Improve naming, tagging, and organization to make rollback and debugging easier",
      )
    _ -> issues
  }

  issues
}

// =============================================================================
// Overall Critique
// =============================================================================

/// Run all critique validators and aggregate results with scoring
pub fn critique_ready(ready: ReadyReport) -> CritiqueResult {
  let vision_issues = validate_vision_alignment(ready)
  let criteria_issues = validate_success_criteria(ready)
  let rollback_issues = validate_rollback_plan(ready)

  let all_issues =
    list.flatten([vision_issues, criteria_issues, rollback_issues])

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
