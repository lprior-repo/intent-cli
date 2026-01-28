/// KIRK Quality Analyzer
/// Analyzes spec quality across 5 dimensions: completeness, consistency, testability, clarity, security
import gleam/dict
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/types.{type Spec}

pub type QualityReport {
  QualityReport(
    completeness: Float,
    consistency: Float,
    testability: Float,
    clarity: Float,
    security: Float,
    overall: Float,
    issues: List(QualityIssue),
    suggestions: List(String),
  )
}

pub type QualityIssue {
  QualityIssue(field: String, issue: String, severity: Severity)
}

pub type Severity {
  Info
  Warning
  Error
  Critical
}

/// Analyze spec quality across 5 dimensions
pub fn analyze_quality(spec: Spec) -> QualityReport {
  let behaviors =
    spec.features
    |> list.flat_map(fn(f) { f.behaviors })

  let completeness = calculate_completeness(spec, behaviors)
  let consistency = calculate_consistency(spec, behaviors)
  let testability = calculate_testability(spec, behaviors)
  let clarity = calculate_clarity(spec, behaviors)
  let security = calculate_security(spec, behaviors)

  // Weighted average: 0.2*completeness + 0.2*consistency + 0.25*testability + 0.15*clarity + 0.2*security
  let overall =
    completeness
    *. 0.2
    +. consistency
    *. 0.2
    +. testability
    *. 0.25
    +. clarity
    *. 0.15
    +. security
    *. 0.2

  let issues = detect_quality_issues(spec, behaviors)
  let suggestions = generate_suggestions(issues)

  QualityReport(
    completeness: completeness,
    consistency: consistency,
    testability: testability,
    clarity: clarity,
    security: security,
    overall: overall,
    issues: issues,
    suggestions: suggestions,
  )
}

/// Alias for analyze_quality
pub fn analyze_spec(spec: Spec) -> QualityReport {
  analyze_quality(spec)
}

/// Calculate completeness score (0-100)
/// Measures whether all required fields and sections are present
fn calculate_completeness(spec: Spec, behaviors: List(types.Behavior)) -> Float {
  let base = 50.0

  // Has features
  let has_features = case spec.features {
    [] -> 0.0
    _ -> 10.0
  }

  // Has behaviors
  let has_behaviors = case behaviors {
    [] -> 0.0
    _ -> 15.0
  }

  // Has success criteria
  let has_criteria = case spec.success_criteria {
    [] -> 0.0
    _ -> 10.0
  }

  // Has rules or anti-patterns
  let has_rules = case spec.rules, spec.anti_patterns {
    [], [] -> 0.0
    _, _ -> 10.0
  }

  // Has AI hints
  let has_ai_hints = case spec.ai_hints {
    Some(hints) ->
      case hints.implementation.suggested_stack, hints.pitfalls {
        [], [] -> 0.0
        _, _ -> 5.0
      }
    None -> 0.0
  }

  float.min(
    100.0,
    base
      +. has_features
      +. has_behaviors
      +. has_criteria
      +. has_rules
      +. has_ai_hints,
  )
}

/// Calculate consistency score (0-100)
/// Measures naming consistency and structure
fn calculate_consistency(spec: Spec, behaviors: List(types.Behavior)) -> Float {
  let base = 80.0

  // Check for duplicate behavior names
  let behavior_names = list.map(behaviors, fn(b) { b.name })
  let unique_names =
    behavior_names
    |> list.unique
    |> list.length
  let total_names = list.length(behavior_names)

  let duplicate_penalty = case total_names {
    0 -> 0.0
    _ -> {
      let dup_count = total_names - unique_names
      int.to_float(dup_count) *. 10.0
    }
  }

  // Check for duplicate feature names
  let feature_names = list.map(spec.features, fn(f) { f.name })
  let unique_feature_names =
    feature_names
    |> list.unique
    |> list.length
  let total_feature_names = list.length(feature_names)

  let feature_dup_penalty = case total_feature_names {
    0 -> 0.0
    _ -> {
      let dup_count = total_feature_names - unique_feature_names
      int.to_float(dup_count) *. 10.0
    }
  }

  float.max(0.0, base -. duplicate_penalty -. feature_dup_penalty)
}

/// Calculate testability score (0-100)
/// Measures how well-structured for testing
fn calculate_testability(_spec: Spec, behaviors: List(types.Behavior)) -> Float {
  let base = 60.0

  // Behaviors with checks
  let with_checks =
    behaviors
    |> list.filter(fn(b) { !dict.is_empty(b.response.checks) })
    |> list.length

  let checks_bonus = case list.length(behaviors) {
    0 -> 0.0
    total -> {
      let ratio = int.to_float(with_checks) /. int.to_float(total)
      ratio *. 20.0
    }
  }

  // Behaviors with examples
  let with_examples =
    behaviors
    |> list.filter(fn(b) { b.response.example != json.null() })
    |> list.length

  let examples_bonus = case list.length(behaviors) {
    0 -> 0.0
    total -> {
      let ratio = int.to_float(with_examples) /. int.to_float(total)
      ratio *. 15.0
    }
  }

  // Behaviors with captures (for state management)
  let with_captures =
    behaviors
    |> list.filter(fn(b) { !dict.is_empty(b.captures) })
    |> list.length

  let captures_bonus = float.min(5.0, int.to_float(with_captures) *. 2.5)

  float.min(100.0, base +. checks_bonus +. examples_bonus +. captures_bonus)
}

/// Calculate clarity score (0-100)
/// Measures documentation quality
fn calculate_clarity(_spec: Spec, behaviors: List(types.Behavior)) -> Float {
  let base = 70.0

  // Behaviors with meaningful intent (> 10 chars)
  let with_intent =
    behaviors
    |> list.filter(fn(b) { string.length(b.intent) > 10 })
    |> list.length

  let intent_bonus = case list.length(behaviors) {
    0 -> 0.0
    total -> {
      let ratio = int.to_float(with_intent) /. int.to_float(total)
      ratio *. 15.0
    }
  }

  // Checks with 'why' explanations
  let total_checks =
    behaviors
    |> list.flat_map(fn(b) { dict.values(b.response.checks) })
    |> list.length

  let checks_with_why =
    behaviors
    |> list.flat_map(fn(b) { dict.values(b.response.checks) })
    |> list.filter(fn(c) { string.length(c.why) > 0 })
    |> list.length

  let why_bonus = case total_checks {
    0 -> 0.0
    _ -> {
      let ratio = int.to_float(checks_with_why) /. int.to_float(total_checks)
      ratio *. 15.0
    }
  }

  float.min(100.0, base +. intent_bonus +. why_bonus)
}

/// Calculate security score (0-100)
/// Measures security testing coverage
fn calculate_security(spec: Spec, behaviors: List(types.Behavior)) -> Float {
  let base = 50.0

  // Error status codes tested (4xx, 5xx)
  let error_tests =
    behaviors
    |> list.filter(fn(b) { b.response.status >= 400 })
    |> list.length

  let error_bonus = float.min(20.0, int.to_float(error_tests) *. 5.0)

  // Security-related tags or names
  let security_keywords = ["auth", "security", "401", "403", "unauthorized"]
  let security_tests =
    behaviors
    |> list.filter(fn(b) {
      let name_lower = string.lowercase(b.name)
      let intent_lower = string.lowercase(b.intent)
      let tags_lower = list.map(b.tags, string.lowercase)

      list.any(security_keywords, fn(keyword) {
        string.contains(name_lower, keyword)
        || string.contains(intent_lower, keyword)
        || list.contains(tags_lower, keyword)
      })
    })
    |> list.length

  let security_test_bonus = float.min(15.0, int.to_float(security_tests) *. 7.5)

  // Anti-patterns defined
  let anti_pattern_bonus = case spec.anti_patterns {
    [] -> 0.0
    _ -> 10.0
  }

  // Security hints provided
  let security_hints_bonus = case spec.ai_hints {
    Some(hints) ->
      case
        string.length(hints.security.password_hashing),
        string.length(hints.security.jwt_algorithm)
      {
        0, 0 -> 0.0
        _, _ -> 5.0
      }
    None -> 0.0
  }

  float.min(
    100.0,
    base
      +. error_bonus
      +. security_test_bonus
      +. anti_pattern_bonus
      +. security_hints_bonus,
  )
}

/// Detect quality issues in spec
fn detect_quality_issues(
  spec: Spec,
  behaviors: List(types.Behavior),
) -> List(QualityIssue) {
  []
  |> add_completeness_issues(spec, behaviors)
  |> add_consistency_issues(spec, behaviors)
  |> add_testability_issues(spec, behaviors)
  |> add_clarity_issues(spec, behaviors)
  |> add_security_issues(spec, behaviors)
}

fn add_completeness_issues(
  issues: List(QualityIssue),
  spec: Spec,
  behaviors: List(types.Behavior),
) -> List(QualityIssue) {
  let mut_issues = issues

  // Check for missing features
  let mut_issues = case spec.features {
    [] -> [
      QualityIssue(
        field: "features",
        issue: "No features defined",
        severity: Critical,
      ),
      ..mut_issues
    ]
    _ -> mut_issues
  }

  // Check for missing behaviors
  let mut_issues = case behaviors {
    [] -> [
      QualityIssue(
        field: "behaviors",
        issue: "No behaviors defined",
        severity: Critical,
      ),
      ..mut_issues
    ]
    _ -> mut_issues
  }

  // Check for missing success criteria
  let mut_issues = case spec.success_criteria {
    [] -> [
      QualityIssue(
        field: "success_criteria",
        issue: "No success criteria defined",
        severity: Warning,
      ),
      ..mut_issues
    ]
    _ -> mut_issues
  }

  mut_issues
}

fn add_consistency_issues(
  issues: List(QualityIssue),
  _spec: Spec,
  behaviors: List(types.Behavior),
) -> List(QualityIssue) {
  let behavior_names = list.map(behaviors, fn(b) { b.name })
  let unique_names =
    behavior_names
    |> list.unique
    |> list.length
  let total_names = list.length(behavior_names)

  case total_names == unique_names {
    True -> issues
    False -> [
      QualityIssue(
        field: "behaviors",
        issue: "Duplicate behavior names found",
        severity: Error,
      ),
      ..issues
    ]
  }
}

fn add_testability_issues(
  issues: List(QualityIssue),
  _spec: Spec,
  behaviors: List(types.Behavior),
) -> List(QualityIssue) {
  let mut_issues = issues

  // Check for behaviors without checks
  let without_checks =
    behaviors
    |> list.filter(fn(b) { dict.is_empty(b.response.checks) })
    |> list.length

  let mut_issues = case without_checks > 0 {
    True -> [
      QualityIssue(
        field: "response.checks",
        issue: "Some behaviors have no validation checks",
        severity: Warning,
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  // Check for behaviors without examples
  let without_examples =
    behaviors
    |> list.filter(fn(b) { b.response.example == json.null() })
    |> list.length

  let mut_issues = case without_examples > 0 {
    True -> [
      QualityIssue(
        field: "response.example",
        issue: "Some behaviors have no response examples",
        severity: Info,
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  mut_issues
}

fn add_clarity_issues(
  issues: List(QualityIssue),
  _spec: Spec,
  behaviors: List(types.Behavior),
) -> List(QualityIssue) {
  let mut_issues = issues

  // Check for short intents
  let short_intents =
    behaviors
    |> list.filter(fn(b) { string.length(b.intent) < 10 })
    |> list.length

  let mut_issues = case short_intents > 0 {
    True -> [
      QualityIssue(
        field: "intent",
        issue: "Some behaviors have very short intent descriptions",
        severity: Warning,
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  // Check for missing 'why' in checks
  let checks_without_why =
    behaviors
    |> list.flat_map(fn(b) { dict.values(b.response.checks) })
    |> list.filter(fn(c) { string.length(c.why) == 0 })
    |> list.length

  let mut_issues = case checks_without_why > 0 {
    True -> [
      QualityIssue(
        field: "response.checks.why",
        issue: "Some validation checks lack 'why' explanations",
        severity: Warning,
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  mut_issues
}

fn add_security_issues(
  issues: List(QualityIssue),
  spec: Spec,
  behaviors: List(types.Behavior),
) -> List(QualityIssue) {
  let mut_issues = issues

  // Check for error status tests
  let has_error_tests =
    behaviors
    |> list.any(fn(b) { b.response.status >= 400 })

  let mut_issues = case has_error_tests {
    False -> [
      QualityIssue(
        field: "behaviors",
        issue: "No error status code tests (4xx, 5xx)",
        severity: Warning,
      ),
      ..mut_issues
    ]
    True -> mut_issues
  }

  // Check for authentication tests
  let security_keywords = ["auth", "security", "401", "403"]
  let has_auth_tests =
    behaviors
    |> list.any(fn(b) {
      let name_lower = string.lowercase(b.name)
      let intent_lower = string.lowercase(b.intent)
      list.any(security_keywords, fn(keyword) {
        string.contains(name_lower, keyword)
        || string.contains(intent_lower, keyword)
      })
    })

  let mut_issues = case has_auth_tests {
    False -> [
      QualityIssue(
        field: "behaviors",
        issue: "No authentication/authorization tests found",
        severity: Warning,
      ),
      ..mut_issues
    ]
    True -> mut_issues
  }

  // Check for anti-patterns
  let mut_issues = case spec.anti_patterns {
    [] -> [
      QualityIssue(
        field: "anti_patterns",
        issue: "No anti-patterns defined",
        severity: Info,
      ),
      ..mut_issues
    ]
    _ -> mut_issues
  }

  mut_issues
}

/// Generate suggestions for improvement
fn generate_suggestions(issues: List(QualityIssue)) -> List(String) {
  list.map(issues, fn(issue) {
    case issue.field {
      "features" -> "Add at least one feature to structure your behaviors"
      "behaviors" -> "Add behaviors to test your API endpoints"
      "success_criteria" -> "Define success criteria to clarify project goals"
      "response.checks" ->
        "Add validation checks to verify response correctness"
      "response.example" -> "Add response examples to document expected outputs"
      "intent" ->
        "Write detailed intent descriptions to explain what each behavior tests"
      "response.checks.why" ->
        "Add 'why' explanations to validation rules for clarity"
      "anti_patterns" -> "Document anti-patterns to guide implementation"
      _ -> "Improve " <> issue.field <> " quality"
    }
  })
  |> list.unique
}

/// Convert severity to string
pub fn severity_to_string(s: Severity) -> String {
  case s {
    Info -> "info"
    Warning -> "warning"
    Error -> "error"
    Critical -> "critical"
  }
}

/// Format quality report for display
pub fn format_report(report: QualityReport) -> String {
  "Quality Report\n"
  <> "  Completeness: "
  <> float.to_string(report.completeness)
  <> "\n"
  <> "  Consistency:  "
  <> float.to_string(report.consistency)
  <> "\n"
  <> "  Testability:  "
  <> float.to_string(report.testability)
  <> "\n"
  <> "  Clarity:      "
  <> float.to_string(report.clarity)
  <> "\n"
  <> "  Security:     "
  <> float.to_string(report.security)
  <> "\n"
  <> "  Overall:      "
  <> float.to_string(report.overall)
}
