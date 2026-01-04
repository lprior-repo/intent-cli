/// Spec quality analysis and scoring
/// Analyzes completeness, clarity, testability, and AI readiness

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/string
import intent/types.{type Spec, type Behavior, type Rule}

/// Quality metrics for a spec
pub type QualityReport {
  QualityReport(
    coverage_score: Int,
    clarity_score: Int,
    testability_score: Int,
    ai_readiness_score: Int,
    overall_score: Int,
    issues: List(QualityIssue),
    suggestions: List(String),
  )
}

/// Quality issues found
pub type QualityIssue {
  MissingErrorTests
  MissingAuthenticationTest
  MissingEdgeCases
  VagueRules
  NoExamples
  MissingExplanations
  UntestedRules
  MissingAIHints
}

/// Analyze spec quality
pub fn analyze_spec(spec: Spec) -> QualityReport {
  let behaviors =
    spec.features
    |> list.flat_map(fn(f) { f.behaviors })

  let coverage_score = calculate_coverage_score(behaviors, spec.rules)
  let clarity_score = calculate_clarity_score(behaviors)
  let testability_score = calculate_testability_score(behaviors)
  let ai_readiness_score = calculate_ai_readiness_score(spec, behaviors)

  let overall_score = {
    let sum = coverage_score + clarity_score + testability_score + ai_readiness_score
    sum / 4
  }

  let issues = find_quality_issues(behaviors, spec.rules)
  let suggestions = generate_suggestions(issues, behaviors, spec.rules)

  QualityReport(
    coverage_score: coverage_score,
    clarity_score: clarity_score,
    testability_score: testability_score,
    ai_readiness_score: ai_readiness_score,
    overall_score: overall_score,
    issues: issues,
    suggestions: suggestions,
  )
}

/// Calculate coverage score (0-100)
/// Measures how many error cases and edge cases are tested
fn calculate_coverage_score(
  behaviors: List(Behavior),
  rules: List(Rule),
) -> Int {
  let base = 50

  // Count error status codes tested
  let error_statuses =
    behaviors
    |> list.filter(fn(b) {
      let status = b.response.status
      status >= 400 && status < 600
    })
    |> list.length

  let error_bonus = int.min(50, error_statuses * 10)


  // Check if authentication tested
  let has_auth_test =
    behaviors
    |> list.any(fn(b) {
      string.contains(string.lowercase(b.name), "auth")
      || string.contains(string.lowercase(b.intent), "auth")
    })

  let auth_bonus = case has_auth_test {
    True -> 10
    False -> 0
  }

  // Check for edge cases (empty, max length, invalid)
  let has_edge_cases =
    behaviors
    |> list.any(fn(b) {
      string.contains(string.lowercase(b.name), "empty")
      || string.contains(string.lowercase(b.name), "invalid")
      || string.contains(string.lowercase(b.name), "max")
      || string.contains(string.lowercase(b.intent), "edge")
    })

  let edge_bonus = case has_edge_cases {
    True -> 10
    False -> 0
  }

  // Check for anti-pattern coverage
  let antipattern_bonus = int.min(5, list.length(rules) * 2)

  let coverage_total = base + error_bonus + auth_bonus + edge_bonus + antipattern_bonus
  int.min(100, coverage_total)
}

/// Calculate clarity score (0-100)
/// Measures how well documented the spec is
fn calculate_clarity_score(behaviors: List(Behavior)) -> Int {
  let base = 60

  // Count behaviors with intent descriptions
  let with_intent =
    behaviors
    |> list.filter(fn(b) { !string.is_empty(b.intent) })
    |> list.length

  let intent_ratio = case list.length(behaviors) {
    0 -> 0
    n -> {
      let ratio = with_intent * 100
      ratio / n
    }
  }

  let intent_bonus = int.min(10, intent_ratio / 10)

  // Count behaviors with notes
  let with_notes =
    behaviors
    |> list.filter(fn(b) { !string.is_empty(b.notes) })
    |> list.length

  let notes_bonus = case list.length(behaviors) {
    0 -> 0
    n -> {
      let bonus_calc = with_notes * 10
      bonus_calc / n
    }
  }

  // Check for vague language
  let has_vague_rules =
    behaviors
    |> list.any(fn(b) {
      b.response.checks
      |> dict.values
      |> list.any(fn(check) {
        let rule_lower = string.lowercase(check.rule)
        string.contains(rule_lower, "valid")
        && !string.contains(rule_lower, "email")
        && !string.contains(rule_lower, "uuid")
        && !string.contains(rule_lower, "iso")
      })
    })

  let vague_penalty = case has_vague_rules {
    True -> -10
    False -> 0
  }

  let clarity_total = base + intent_bonus + notes_bonus + vague_penalty
  int.max(0, int.min(100, clarity_total))
}

/// Calculate testability score (0-100)
/// Measures how well structured for execution
fn calculate_testability_score(behaviors: List(Behavior)) -> Int {
  let base = 70

  // Count behaviors with captures
  let with_captures =
    behaviors
    |> list.filter(fn(b) { !dict.is_empty(b.captures) })
    |> list.length

  let capture_bonus = int.min(10, with_captures * 5)

  // Check for well-defined dependencies
  let with_dependencies =
    behaviors
    |> list.filter(fn(b) { !list.is_empty(b.requires) })
    |> list.length

  let deps_bonus = int.min(10, with_dependencies * 5)

  // Check for examples
  let with_examples =
    behaviors
    |> list.filter(fn(b) {
      case b.response.example {
        None -> False
        _ -> True
      }
    })
    |> list.length

  let example_bonus = int.min(5, with_examples / int.max(1, list.length(behaviors) / 2))

  let testability_total = base + capture_bonus + deps_bonus + example_bonus
  int.min(100, testability_total)
}

/// Calculate AI readiness score (0-100)
/// Measures how much guidance is available for AI
fn calculate_ai_readiness_score(spec: Spec, behaviors: List(Behavior)) -> Int {
  let base = 50

  // Check for AI hints
  let has_ai_hints = case spec.ai_hints {
    None -> False
    _ -> True
  }

  let hints_bonus = case has_ai_hints {
    True -> 20
    False -> -10
  }

  // Count behaviors with 'why' explanations
  let with_why =
    behaviors
    |> list.flat_map(fn(b) { dict.values(b.response.checks) })
    |> list.filter(fn(c) { !string.is_empty(c.why) })
    |> list.length

  let total_checks =
    behaviors
    |> list.flat_map(fn(b) { dict.values(b.response.checks) })
    |> list.length

  let why_bonus = case total_checks {
    0 -> 0
    _ -> {
      let why_calc = with_why * 30
      why_calc / int.max(1, total_checks)
    }
  }

  // Count example responses
  let with_examples =
    behaviors
    |> list.filter(fn(b) {
      case b.response.example {
        None -> False
        _ -> True
      }
    })
    |> list.length

  let example_bonus = int.min(10, with_examples * 5)

  let ai_readiness_total = base + hints_bonus + why_bonus + example_bonus
  int.max(0, int.min(100, ai_readiness_total))
}

/// Find quality issues in spec
fn find_quality_issues(
  behaviors: List(Behavior),
  rules: List(Rule),
) -> List(QualityIssue) {
  let mut_issues = []

  // Check for error tests
  let has_error_tests =
    list.any(behaviors, fn(b) { b.response.status >= 400 })

  let mut_issues = case has_error_tests {
    True -> mut_issues
    False -> [MissingErrorTests, ..mut_issues]
  }

  // Check for auth tests
  let has_auth_test =
    list.any(behaviors, fn(b) {
      string.contains(string.lowercase(b.name), "auth")
    })

  let mut_issues = case has_auth_test {
    True -> mut_issues
    False -> [MissingAuthenticationTest, ..mut_issues]
  }

  // Check for edge cases
  let has_edge_cases =
    list.any(behaviors, fn(b) {
      string.contains(string.lowercase(b.name), "empty")
      || string.contains(string.lowercase(b.name), "invalid")
    })

  let mut_issues = case has_edge_cases {
    True -> mut_issues
    False -> [MissingEdgeCases, ..mut_issues]
  }

  // Check for vague rules
  let has_vague =
    list.any(behaviors, fn(b) {
      b.response.checks
      |> dict.values
      |> list.any(fn(check) {
        let rule_lower = string.lowercase(check.rule)
        string.contains(rule_lower, "valid data")
        || string.contains(rule_lower, "correct format")
      })
    })

  let mut_issues = case has_vague {
    False -> mut_issues
    True -> [VagueRules, ..mut_issues]
  }

  // Check for examples
  let has_examples = list.any(behaviors, fn(b) { case b.response.example { None -> False
    _ -> True } })

  let mut_issues = case has_examples {
    True -> mut_issues
    False -> [NoExamples, ..mut_issues]
  }

  // Check for explanations
  let has_explanations =
    list.any(behaviors, fn(b) {
      b.response.checks
      |> dict.values
      |> list.any(fn(c) { !string.is_empty(c.why) })
    })

  let mut_issues = case has_explanations {
    True -> mut_issues
    False -> [MissingExplanations, ..mut_issues]
  }

  // Check for untested rules
  let has_untested_rules = !list.is_empty(rules)

  let mut_issues = case has_untested_rules {
    False -> mut_issues
    True -> [UntestedRules, ..mut_issues]
  }

  mut_issues
}

/// Generate suggestions for improvement
fn generate_suggestions(
  issues: List(QualityIssue),
  _behaviors: List(Behavior),
  _rules: List(Rule),
) -> List(String) {
  []
  |> add_suggestion_if(
    list.contains(issues, MissingErrorTests),
    "Add test cases for error status codes (400, 401, 403, 404, 409, 500)",
  )
  |> add_suggestion_if(
    list.contains(issues, MissingAuthenticationTest),
    "Add test cases for authentication (missing auth, invalid token)",
  )
  |> add_suggestion_if(
    list.contains(issues, MissingEdgeCases),
    "Add edge case tests (empty values, max length, invalid input)",
  )
  |> add_suggestion_if(
    list.contains(issues, VagueRules),
    "Replace vague rules like 'valid data' with specific validation rules",
  )
  |> add_suggestion_if(
    list.contains(issues, NoExamples),
    "Add response examples to each behavior for documentation",
  )
  |> add_suggestion_if(
    list.contains(issues, MissingExplanations),
    "Add 'why' explanations to validation rules to clarify intent",
  )
}

/// Helper to add suggestion conditionally
fn add_suggestion_if(
  suggestions: List(String),
  condition: Bool,
  suggestion: String,
) -> List(String) {
  case condition {
    True -> [suggestion, ..suggestions]
    False -> suggestions
  }
}

/// Format quality report for display
pub fn format_report(report: QualityReport) -> String {
  let score_section =
    "Quality Score: " <> int.to_string(report.overall_score) <> "/100\n" <> "  Coverage: " <> int.to_string(
      report.coverage_score,
    ) <> "/100\n" <> "  Clarity: " <> int.to_string(
      report.clarity_score,
    ) <> "/100\n" <> "  Testability: " <> int.to_string(
      report.testability_score,
    ) <> "/100\n" <> "  AI Readiness: " <> int.to_string(
      report.ai_readiness_score,
    ) <> "/100"

  let issues_section = case list.is_empty(report.issues) {
    True -> "No quality issues found!"
    False ->
      "Quality Issues:\n" <> string.join(
        report.issues |> list.map(format_issue),
        "\n",
      )
  }

  let suggestions_section = case list.is_empty(report.suggestions) {
    True -> ""
    False ->
      "\n\nSuggestions for Improvement:\n" <> string.join(
        report.suggestions
        |> list.index_map(fn(s, i) { int.to_string(i + 1) <> ". " <> s }),
        "\n",
      )
  }

  score_section <> "\n\n" <> issues_section <> suggestions_section
}

/// Format a quality issue
fn format_issue(issue: QualityIssue) -> String {
  case issue {
    MissingErrorTests -> "  • Missing error status code tests (4xx, 5xx)"
    MissingAuthenticationTest -> "  • Missing authentication tests"
    MissingEdgeCases -> "  • Missing edge case tests (empty, invalid, etc)"
    VagueRules -> "  • Vague validation rules ('valid data', 'correct format')"
    NoExamples -> "  • No response examples provided"
    MissingExplanations -> "  • Missing 'why' explanations in checks"
    UntestedRules -> "  • Global rules not tested in behaviors"
    MissingAIHints -> "  • No AI implementation hints provided"
  }
}
