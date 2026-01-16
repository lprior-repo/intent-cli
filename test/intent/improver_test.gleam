//// Comprehensive tests for the improver module
//// Tests interactive specification refinement:
//// - Improvement suggestion generation from quality issues
//// - Improvement suggestion generation from lint warnings
//// - Suggestion formatting and display
//// - Impact scoring and prioritization
//// - Different proposed change types

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/improver
import intent/quality_analyzer
import intent/spec_linter
import intent/types

// ============================================================================
// Test Fixtures
// ============================================================================

fn make_minimal_spec() -> types.Spec {
  let config =
    types.Config(
      base_url: "http://test.com",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  // Create a minimal spec with one behavior to avoid quality suggestions
  // The behavior has an error status (404) so coverage suggestions are satisfied
  let check = types.Check(rule: "present", why: "Field must exist")
  let checks = dict.from_list([#("field", check)])
  let example = json.object([#("error", json.string("Not found"))])
  let behavior = make_behavior("test-error", 404, "Test error", example, checks)

  let feature =
    types.Feature(name: "Test", description: "Test", behaviors: [behavior])

  types.Spec(
    name: "Test Spec",
    description: "Test description",
    audience: "Developers",
    version: "1.0.0",
    success_criteria: ["Test passes"],
    config: config,
    features: [feature],
    rules: [],
    anti_patterns: [],
    ai_hints: types.AIHints(
      implementation: types.ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: types.SecurityHints(
        password_hashing: "bcrypt",
        jwt_algorithm: "HS256",
        jwt_expiry: "1h",
        rate_limiting: "100/hour",
      ),
      pitfalls: [],
    ),
  )
}

fn make_behavior(
  name: String,
  status: Int,
  intent: String,
  example: json.Json,
  checks: dict.Dict(String, types.Check),
) -> types.Behavior {
  let request =
    types.Request(
      method: types.Get,
      path: "/test",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let response =
    types.Response(
      status: status,
      example: example,
      checks: checks,
      headers: dict.new(),
    )

  types.Behavior(
    name: name,
    intent: intent,
    notes: "",
    requires: [],
    tags: [],
    request: request,
    response: response,
    captures: dict.new(),
  )
}

fn make_quality_report(
  issues: List(quality_analyzer.QualityIssue),
) -> quality_analyzer.QualityReport {
  quality_analyzer.QualityReport(
    coverage_score: 70,
    clarity_score: 75,
    testability_score: 80,
    ai_readiness_score: 65,
    overall_score: 72,
    issues: issues,
    suggestions: [],
  )
}

// ============================================================================
// Coverage Suggestions Tests
// ============================================================================

pub fn suggest_improvements_adds_error_test_when_missing_test() {
  let spec = make_minimal_spec()

  // Add only success behaviors (no error tests)
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior("test-success", 200, "Success", json.null(), dict.new()),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should suggest adding error test
  let has_error_test_suggestion =
    list.any(suggestions, fn(s) { string.contains(s.title, "error case") })

  has_error_test_suggestion
  |> should.equal(True)
}

pub fn suggest_improvements_no_error_test_when_present_test() {
  let spec = make_minimal_spec()

  // Add both success and error behaviors
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior("test-success", 200, "Success", json.null(), dict.new()),
      make_behavior("test-error", 404, "Not found", json.null(), dict.new()),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should NOT suggest adding error test
  let has_error_test_suggestion =
    list.any(suggestions, fn(s) { string.contains(s.title, "error case") })

  has_error_test_suggestion
  |> should.equal(False)
}

// ============================================================================
// Clarity Suggestions Tests
// ============================================================================

pub fn suggest_improvements_adds_intent_when_missing_test() {
  let spec = make_minimal_spec()

  // Add behavior with empty intent
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior("test-success", 200, "", json.null(), dict.new()),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should suggest adding intent
  let has_intent_suggestion =
    list.any(suggestions, fn(s) { string.contains(s.title, "intent") })

  has_intent_suggestion
  |> should.equal(True)
}

pub fn suggest_improvements_no_intent_when_present_test() {
  let spec = make_minimal_spec()

  // Add behavior with intent
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior(
        "test-success",
        200,
        "Verify success",
        json.null(),
        dict.new(),
      ),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should NOT suggest adding intent
  let has_intent_suggestion =
    list.any(suggestions, fn(s) { string.contains(s.title, "intent") })

  has_intent_suggestion
  |> should.equal(False)
}

// ============================================================================
// Testability Suggestions Tests
// ============================================================================

pub fn suggest_improvements_adds_example_when_missing_test() {
  let spec = make_minimal_spec()

  // Add behavior with null example
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior("test-success", 200, "Success", json.null(), dict.new()),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should suggest adding example
  let has_example_suggestion =
    list.any(suggestions, fn(s) { string.contains(s.title, "example") })

  has_example_suggestion
  |> should.equal(True)
}

pub fn suggest_improvements_no_example_when_present_test() {
  let spec = make_minimal_spec()

  // Add behavior with example
  let example = json.object([#("status", json.string("ok"))])
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior("test-success", 200, "Success", example, dict.new()),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should NOT suggest adding example
  let has_example_suggestion =
    list.any(suggestions, fn(s) { string.contains(s.title, "example") })

  has_example_suggestion
  |> should.equal(False)
}

// ============================================================================
// AI Readiness Suggestions Tests
// ============================================================================

pub fn suggest_improvements_adds_why_when_missing_test() {
  let spec = make_minimal_spec()

  // Add behavior with check missing "why"
  let check = types.Check(rule: "present", why: "")
  let checks = dict.from_list([#("field", check)])
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior("test-success", 200, "Success", json.null(), checks),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should suggest adding explanation
  let has_why_suggestion =
    list.any(suggestions, fn(s) {
      string.contains(s.title, "validation")
      || string.contains(s.title, "explanation")
    })

  has_why_suggestion
  |> should.equal(True)
}

pub fn suggest_improvements_no_why_when_present_test() {
  let spec = make_minimal_spec()

  // Add behavior with check having "why"
  let check = types.Check(rule: "present", why: "Ensures field exists")
  let checks = dict.from_list([#("field", check)])
  let feature =
    types.Feature(name: "Test Feature", description: "Test", behaviors: [
      make_behavior("test-success", 200, "Success", json.null(), checks),
    ])

  let spec_with_feature = types.Spec(..spec, features: [feature])

  let quality_report = make_quality_report([])
  let lint_result = spec_linter.LintValid

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec_with_feature,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should NOT suggest adding explanation
  let has_why_suggestion =
    list.any(suggestions, fn(s) {
      string.contains(s.title, "validation")
      && string.contains(s.title, "explanation")
    })

  has_why_suggestion
  |> should.equal(False)
}

// ============================================================================
// Lint Warning Suggestions Tests
// ============================================================================

pub fn suggest_improvements_from_vague_rule_test() {
  let spec = make_minimal_spec()

  let quality_report = make_quality_report([])
  let lint_result =
    spec_linter.LintWarnings([
      spec_linter.VagueRule("test-behavior", "email", "valid email"),
    ])

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should have suggestion from vague rule
  list.length(suggestions)
  |> should.equal(1)

  let first = list.first(suggestions)
  case first {
    Ok(suggestion) -> {
      string.contains(suggestion.title, "validation rule")
      |> should.equal(True)
      suggestion.impact_score
      |> should.equal(22)
    }
    Error(_) -> should.fail()
  }
}

pub fn suggest_improvements_from_missing_example_test() {
  let spec = make_minimal_spec()

  let quality_report = make_quality_report([])
  let lint_result =
    spec_linter.LintWarnings([spec_linter.MissingExample("test-behavior")])

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should have suggestion from missing example
  list.length(suggestions)
  |> should.equal(1)

  let first = list.first(suggestions)
  case first {
    Ok(suggestion) -> {
      string.contains(suggestion.title, "example")
      |> should.equal(True)
      suggestion.impact_score
      |> should.equal(20)
    }
    Error(_) -> should.fail()
  }
}

pub fn suggest_improvements_from_naming_convention_test() {
  let spec = make_minimal_spec()

  let quality_report = make_quality_report([])
  let lint_result =
    spec_linter.LintWarnings([
      spec_linter.NamingConvention("test_behavior", "Use kebab-case"),
    ])

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should have suggestion from naming convention
  list.length(suggestions)
  |> should.equal(1)

  let first = list.first(suggestions)
  case first {
    Ok(suggestion) -> {
      string.contains(suggestion.title, "naming")
      |> should.equal(True)
      suggestion.impact_score
      |> should.equal(10)
    }
    Error(_) -> should.fail()
  }
}

pub fn suggest_improvements_from_unused_anti_pattern_test() {
  let spec = make_minimal_spec()

  let quality_report = make_quality_report([])
  let lint_result =
    spec_linter.LintWarnings([spec_linter.UnusedAntiPattern("unused-pattern")])

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should have suggestion from unused anti-pattern
  list.length(suggestions)
  |> should.equal(1)

  let first = list.first(suggestions)
  case first {
    Ok(suggestion) -> {
      string.contains(suggestion.title, "unused")
      |> should.equal(True)
      suggestion.impact_score
      |> should.equal(5)
    }
    Error(_) -> should.fail()
  }
}

pub fn suggest_improvements_from_anti_pattern_detected_test() {
  let spec = make_minimal_spec()

  let quality_report = make_quality_report([])
  let lint_result =
    spec_linter.LintWarnings([
      spec_linter.AntiPatternDetected(
        "test-behavior",
        "bad-pattern",
        "Contains problematic field",
      ),
    ])

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should have suggestion from anti-pattern detected
  list.length(suggestions)
  |> should.equal(1)

  let first = list.first(suggestions)
  case first {
    Ok(suggestion) -> {
      string.contains(suggestion.title, "anti-pattern")
      |> should.equal(True)
      suggestion.impact_score
      |> should.equal(30)
    }
    Error(_) -> should.fail()
  }
}

pub fn suggest_improvements_from_duplicate_behavior_test() {
  let spec = make_minimal_spec()

  let quality_report = make_quality_report([])
  let lint_result =
    spec_linter.LintWarnings([
      spec_linter.DuplicateBehavior("test-1", "test-2", "80% similar"),
    ])

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should have suggestion from duplicate behavior
  list.length(suggestions)
  |> should.equal(1)

  let first = list.first(suggestions)
  case first {
    Ok(suggestion) -> {
      string.contains(suggestion.title, "duplicate")
      |> should.equal(True)
      suggestion.impact_score
      |> should.equal(25)
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Impact Scoring and Prioritization Tests
// ============================================================================

pub fn suggest_improvements_sorts_by_impact_score_test() {
  let spec = make_minimal_spec()

  let quality_report = make_quality_report([])
  let lint_result =
    spec_linter.LintWarnings([
      spec_linter.NamingConvention("test_1", "Use kebab-case"),
      // impact 10
      spec_linter.AntiPatternDetected("test_2", "pattern", "details"),
      // impact 30
      spec_linter.MissingExample("test_3"),
      // impact 20
    ])

  let context =
    improver.ImprovementContext(
      quality_report: quality_report,
      lint_result: lint_result,
      spec: spec,
    )

  let suggestions = improver.suggest_improvements(context)

  // Should be sorted by impact (highest first): 30, 20, 10
  list.length(suggestions)
  |> should.equal(3)

  let scores = list.map(suggestions, fn(s) { s.impact_score })
  scores
  |> should.equal([30, 20, 10])
}

// ============================================================================
// Format Improvements Tests
// ============================================================================

pub fn format_improvements_empty_list_test() {
  let formatted = improver.format_improvements([])

  string.contains(formatted, "No improvements")
  |> should.equal(True)
}

pub fn format_improvements_includes_count_test() {
  let suggestion =
    improver.ImprovementSuggestion(
      title: "Test",
      description: "Test desc",
      reasoning: "Test reason",
      impact_score: 50,
      proposed_change: improver.AddMissingTest("test", "Test"),
    )

  let formatted = improver.format_improvements([suggestion])

  string.contains(formatted, "1 improvement")
  |> should.equal(True)
}

pub fn format_improvements_includes_title_test() {
  let suggestion =
    improver.ImprovementSuggestion(
      title: "Add error tests",
      description: "Test desc",
      reasoning: "Test reason",
      impact_score: 50,
      proposed_change: improver.AddMissingTest("test", "Test"),
    )

  let formatted = improver.format_improvements([suggestion])

  string.contains(formatted, "Add error tests")
  |> should.equal(True)
}

pub fn format_improvements_includes_impact_score_test() {
  let suggestion =
    improver.ImprovementSuggestion(
      title: "Test",
      description: "Test desc",
      reasoning: "Test reason",
      impact_score: 75,
      proposed_change: improver.AddMissingTest("test", "Test"),
    )

  let formatted = improver.format_improvements([suggestion])

  string.contains(formatted, "75")
  |> should.equal(True)
}

pub fn format_improvements_includes_description_test() {
  let suggestion =
    improver.ImprovementSuggestion(
      title: "Test",
      description: "Missing error handling",
      reasoning: "Test reason",
      impact_score: 50,
      proposed_change: improver.AddMissingTest("test", "Test"),
    )

  let formatted = improver.format_improvements([suggestion])

  string.contains(formatted, "Missing error handling")
  |> should.equal(True)
}

pub fn format_improvements_includes_reasoning_test() {
  let suggestion =
    improver.ImprovementSuggestion(
      title: "Test",
      description: "Test desc",
      reasoning: "Improves testability",
      impact_score: 50,
      proposed_change: improver.AddMissingTest("test", "Test"),
    )

  let formatted = improver.format_improvements([suggestion])

  string.contains(formatted, "Improves testability")
  |> should.equal(True)
}

pub fn format_improvements_multiple_suggestions_test() {
  let suggestion1 =
    improver.ImprovementSuggestion(
      title: "Test 1",
      description: "Desc 1",
      reasoning: "Reason 1",
      impact_score: 30,
      proposed_change: improver.AddMissingTest("test", "Test"),
    )

  let suggestion2 =
    improver.ImprovementSuggestion(
      title: "Test 2",
      description: "Desc 2",
      reasoning: "Reason 2",
      impact_score: 20,
      proposed_change: improver.AddResponseExample("test"),
    )

  let formatted = improver.format_improvements([suggestion1, suggestion2])

  string.contains(formatted, "2 improvement")
  |> should.equal(True)
  string.contains(formatted, "Test 1")
  |> should.equal(True)
  string.contains(formatted, "Test 2")
  |> should.equal(True)
}

// ============================================================================
// Apply Improvements Tests
// ============================================================================

pub fn apply_improvements_returns_spec_unchanged_test() {
  let spec = make_minimal_spec()
  let suggestions = []

  let result = improver.apply_improvements(spec, suggestions)

  // For now, should return spec unchanged (stub implementation)
  result.name
  |> should.equal(spec.name)
  result.version
  |> should.equal(spec.version)
}
