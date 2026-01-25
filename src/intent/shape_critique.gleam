/// Shape Critique Protocol - Pragmatic Tech Lead Persona
///
/// This module implements validation logic for the Shape phase following
/// the "Pragmatic Tech Lead" critique protocol from INTENT_4_PLAN.md
import gleam/int
import gleam/list
import gleam/string
import intent/planning_types.{type ShapeSection}

// =============================================================================
// Types - Shape Phase Specific
// =============================================================================

/// The three critique questions from Pragmatic Tech Lead
pub type CritiqueQuestion {
  MVPMinimalism
  ConceptValidation
  AchievabilityCheck
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

const min_features = 1

const max_mvp_features = 5

const min_shortcuts = 1

const min_critical_path_steps = 2

const min_validation_length = 30

const min_post_mvp_items = 1

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
// MVP Minimalism Validation - "Can we cut more?"
// =============================================================================

/// Validate that the MVP is truly minimal with appropriate shortcuts
pub fn validate_mvp_minimalism(shape: ShapeSection) -> List(CritiqueIssue) {
  let issues = []

  // Check for empty features
  let issues = case list.length(shape.features) {
    0 ->
      add_issue(
        issues,
        MVPMinimalism,
        Critical,
        "No features defined",
        "Define at least "
          <> int.to_string(min_features)
          <> " feature(s) needed to validate the concept",
      )
    _ -> issues
  }

  // Check for too many features (not minimal)
  let feature_count = list.length(shape.features)
  let issues = case feature_count > max_mvp_features {
    True ->
      add_issue(
        issues,
        MVPMinimalism,
        Warning,
        "Too many features for MVP ("
          <> int.to_string(feature_count)
          <> " features, max recommended: "
          <> int.to_string(max_mvp_features)
          <> ")",
        "Cut non-essential features - MVP should be the absolute minimum to validate the concept",
      )
    False -> issues
  }

  // Check for missing shortcuts
  let shortcut_count = list.length(shape.mvp_slice.shortcuts)
  let issues = case shortcut_count < min_shortcuts {
    True ->
      add_issue(
        issues,
        MVPMinimalism,
        Critical,
        "No shortcuts defined (found "
          <> int.to_string(shortcut_count)
          <> ", need at least "
          <> int.to_string(min_shortcuts)
          <> ")",
        "Identify what can be faked, hardcoded, or deferred - if there are no shortcuts, the scope is probably not minimal",
      )
    False -> issues
  }

  // Check for empty MVP description
  let issues = case string.trim(shape.mvp_slice.description) {
    "" ->
      add_issue(
        issues,
        MVPMinimalism,
        Critical,
        "MVP description is empty",
        "Write a clear 1-2 sentence description of the absolute minimum needed to validate the concept",
      )
    _ -> issues
  }

  issues
}

// =============================================================================
// Concept Validation - "Will this actually validate the concept?"
// =============================================================================

/// Validate that the MVP will truly validate the core concept
pub fn validate_concept_validation(shape: ShapeSection) -> List(CritiqueIssue) {
  let issues = []

  // Check for empty validation moment
  let issues = case string.trim(shape.validation_moment) {
    "" ->
      add_issue(
        issues,
        ConceptValidation,
        Critical,
        "Validation moment is empty",
        "Define the specific 'aha' moment that proves the concept works - be concrete about what you'll see/do",
      )
    _ -> issues
  }

  // Check for vague validation moment
  let validation_len = string.length(string.trim(shape.validation_moment))
  let issues = case
    validation_len > 0 && validation_len < min_validation_length
  {
    True ->
      add_issue(
        issues,
        ConceptValidation,
        Warning,
        "Validation moment is too vague or short",
        "Expand to include specific actions, observable outcomes, and success criteria - what exactly will you do and see?",
      )
    False -> issues
  }

  // Check for empty critical path
  let issues = case list.length(shape.critical_path) {
    0 ->
      add_issue(
        issues,
        ConceptValidation,
        Critical,
        "Critical path is empty",
        "Define the must-have steps needed to reach the north star - what's absolutely required?",
      )
    1 ->
      add_issue(
        issues,
        ConceptValidation,
        Warning,
        "Critical path has only 1 step, need at least "
          <> int.to_string(min_critical_path_steps),
        "Break down the validation into concrete steps - what are the key milestones?",
      )
    _ -> issues
  }

  issues
}

// =============================================================================
// Achievability Check - "Is this scope achievable?"
// =============================================================================

/// Validate that the scope is realistic and well-bounded
pub fn validate_achievability(shape: ShapeSection) -> List(CritiqueIssue) {
  let issues = []

  // Check for missing post-MVP items (scope boundary)
  let issues = case list.length(shape.post_mvp) {
    0 ->
      add_issue(
        issues,
        AchievabilityCheck,
        Warning,
        "No post-MVP items defined",
        "Identify at least "
          <> int.to_string(min_post_mvp_items)
          <> " item(s) explicitly deferred - this shows you've thought about boundaries",
      )
    _ -> issues
  }

  // Check for empty MVP description (duplicate from minimalism but important for achievability)
  let issues = case string.trim(shape.mvp_slice.description) {
    "" ->
      add_issue(
        issues,
        AchievabilityCheck,
        Critical,
        "MVP description is empty",
        "Describe the MVP scope clearly - what's the concrete deliverable?",
      )
    _ -> issues
  }

  // Check that MVP features list is non-empty
  let issues = case list.length(shape.mvp_slice.features) {
    0 ->
      add_issue(
        issues,
        AchievabilityCheck,
        Critical,
        "MVP slice has no features listed",
        "List the specific features included in the MVP slice",
      )
    _ -> issues
  }

  issues
}

// =============================================================================
// Overall Critique
// =============================================================================

/// Run all critique validators and aggregate results with scoring
pub fn critique_shape(shape: ShapeSection) -> CritiqueResult {
  let minimalism_issues = validate_mvp_minimalism(shape)
  let validation_issues = validate_concept_validation(shape)
  let achievability_issues = validate_achievability(shape)

  let all_issues =
    list.flatten([minimalism_issues, validation_issues, achievability_issues])

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
