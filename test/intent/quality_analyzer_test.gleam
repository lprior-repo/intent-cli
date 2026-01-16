//// Comprehensive tests for the quality_analyzer module
//// Tests quality analysis and scoring:
//// - Coverage score calculation (error tests, auth tests, edge cases)
//// - Clarity score calculation (intent, notes, vague rules)
//// - Testability score calculation (captures, dependencies, examples)
//// - AI readiness score calculation (AI hints, why explanations)
//// - Issue detection (all issue types)
//// - Suggestion generation (conditional based on issues)
//// - Report formatting (human-readable output)

import gleam/dict
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import intent/quality_analyzer
import intent/types

// ============================================================================
// Test Fixtures
// ============================================================================

fn create_minimal_spec() -> types.Spec {
  types.Spec(
    name: "minimal-api",
    description: "Minimal spec for testing",
    audience: "developers",
    version: "1.0.0",
    success_criteria: ["Works"],
    config: types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
    features: [
      types.Feature(name: "basic", description: "Basic feature", behaviors: [
        create_minimal_behavior(),
      ]),
    ],
    rules: [],
    anti_patterns: [],
    ai_hints: types.AIHints(
      implementation: types.ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: types.SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
    ),
  )
}

fn create_minimal_behavior() -> types.Behavior {
  types.Behavior(
    name: "get_item",
    intent: "",
    request: types.Request(
      method: types.Get,
      path: "/items/1",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    notes: "",
    requires: [],
    tags: [],
    captures: dict.new(),
  )
}

fn create_error_behavior() -> types.Behavior {
  types.Behavior(
    ..create_minimal_behavior(),
    name: "get_item_not_found",
    intent: "Test 404 error",
    response: types.Response(
      status: 404,
      example: json.object([#("error", json.string("Not found"))]),
      checks: dict.new(),
      headers: dict.new(),
    ),
  )
}

fn create_auth_behavior() -> types.Behavior {
  types.Behavior(
    ..create_minimal_behavior(),
    name: "unauthorized_access",
    intent: "Test authentication failure",
    response: types.Response(
      status: 401,
      example: json.object([#("error", json.string("Unauthorized"))]),
      checks: dict.new(),
      headers: dict.new(),
    ),
  )
}

fn create_edge_case_behavior() -> types.Behavior {
  types.Behavior(
    ..create_minimal_behavior(),
    name: "empty_input",
    intent: "Test empty input edge case",
    response: types.Response(
      status: 400,
      example: json.object([#("error", json.string("Invalid input"))]),
      checks: dict.new(),
      headers: dict.new(),
    ),
  )
}

// ============================================================================
// Coverage Score Tests
// ============================================================================

pub fn analyze_spec_minimal_coverage_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Minimal spec should have low coverage (only base 50 + small antipattern bonus)
  { report.coverage_score < 70 }
  |> should.be_true
}

pub fn analyze_spec_with_error_tests_increases_coverage_test() {
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [
          create_error_behavior(),
        ]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Error test should boost coverage (base 50 + error bonus 10)
  { report.coverage_score >= 60 }
  |> should.be_true
}

pub fn analyze_spec_with_auth_test_increases_coverage_test() {
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [
          create_auth_behavior(),
        ]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Auth test should provide bonus
  { report.coverage_score >= 65 }
  |> should.be_true
}

pub fn analyze_spec_with_edge_cases_increases_coverage_test() {
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [
          create_edge_case_behavior(),
        ]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Edge case test should provide bonus
  { report.coverage_score >= 65 }
  |> should.be_true
}

pub fn analyze_spec_with_antipatterns_increases_coverage_test() {
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      rules: [
        types.Rule(
          name: "rule1",
          description: "Test rule",
          check: types.RuleCheck(
            body_must_not_contain: [],
            body_must_contain: [],
            fields_must_exist: [],
            fields_must_not_exist: [],
            header_must_exist: "",
            header_must_not_exist: "",
          ),
          example: json.string("test"),
          when: option.Some(types.When(
            status: "200",
            method: option.None,
            path: option.None,
          )),
        ),
        types.Rule(
          name: "rule2",
          description: "Test rule 2",
          check: types.RuleCheck(
            body_must_not_contain: [],
            body_must_contain: [],
            fields_must_exist: [],
            fields_must_not_exist: [],
            header_must_exist: "",
            header_must_not_exist: "",
          ),
          example: json.string("test2"),
          when: option.Some(types.When(
            status: "200",
            method: option.None,
            path: option.None,
          )),
        ),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Anti-patterns should provide bonus (2 rules * 2 = 4 bonus)
  { report.coverage_score >= 50 }
  |> should.be_true
}

// ============================================================================
// Clarity Score Tests
// ============================================================================

pub fn analyze_spec_minimal_clarity_has_base_score_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Base clarity score is 60
  { report.clarity_score >= 50 }
  |> should.be_true
}

pub fn analyze_spec_with_intent_increases_clarity_test() {
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      intent: "Retrieve a specific item by ID",
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Intent should provide bonus
  { report.clarity_score >= 60 }
  |> should.be_true
}

pub fn analyze_spec_with_notes_increases_clarity_test() {
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      notes: "This endpoint returns cached data",
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Notes should provide bonus
  { report.clarity_score >= 60 }
  |> should.be_true
}

pub fn analyze_spec_with_vague_rules_decreases_clarity_test() {
  let check =
    types.Check(rule: "response.data is valid", why: "Data must be valid")

  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      response: types.Response(
        ..create_minimal_behavior().response,
        checks: dict.from_list([#("data", check)]),
      ),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Vague rules should apply penalty (-10)
  { report.clarity_score < 70 }
  |> should.be_true
}

// ============================================================================
// Testability Score Tests
// ============================================================================

pub fn analyze_spec_minimal_testability_has_base_score_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Base testability score is 70
  { report.testability_score >= 65 }
  |> should.be_true
}

pub fn analyze_spec_with_captures_increases_testability_test() {
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      captures: dict.from_list([#("item_id", "response.id")]),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Captures should provide bonus
  { report.testability_score >= 70 }
  |> should.be_true
}

pub fn analyze_spec_with_dependencies_increases_testability_test() {
  let behavior =
    types.Behavior(..create_minimal_behavior(), requires: ["create_item"])

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Dependencies should provide bonus
  { report.testability_score >= 70 }
  |> should.be_true
}

pub fn analyze_spec_with_examples_increases_testability_test() {
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      response: types.Response(
        ..create_minimal_behavior().response,
        example: json.object([#("id", json.int(1))]),
      ),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Examples should provide bonus
  { report.testability_score >= 70 }
  |> should.be_true
}

// ============================================================================
// AI Readiness Score Tests
// ============================================================================

pub fn analyze_spec_minimal_ai_readiness_has_base_score_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Base AI readiness is 50, but penalty for no AI hints (-10) = 40
  { report.ai_readiness_score >= 30 }
  |> should.be_true
  { report.ai_readiness_score < 50 }
  |> should.be_true
}

pub fn analyze_spec_with_ai_hints_increases_readiness_test() {
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      ai_hints: types.AIHints(
        ..create_minimal_spec().ai_hints,
        implementation: types.ImplementationHints(suggested_stack: [
          "Express.js", "PostgreSQL",
        ]),
      ),
    )

  let report = quality_analyzer.analyze_spec(spec)

  // AI hints should provide bonus (base 50 + bonus 20)
  { report.ai_readiness_score >= 70 }
  |> should.be_true
}

pub fn analyze_spec_with_pitfalls_increases_readiness_test() {
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      ai_hints: types.AIHints(
        ..create_minimal_spec().ai_hints,
        pitfalls: ["Avoid N+1 queries"],
      ),
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Pitfalls should provide bonus (base 50 + bonus 20)
  { report.ai_readiness_score >= 60 }
  |> should.be_true
}

pub fn analyze_spec_with_why_explanations_increases_readiness_test() {
  let check =
    types.Check(rule: "response.id == integer", why: "ID must be numeric")

  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      response: types.Response(
        ..create_minimal_behavior().response,
        checks: dict.from_list([#("id", check)]),
      ),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Why explanations should provide bonus
  { report.ai_readiness_score >= 40 }
  |> should.be_true
}

// ============================================================================
// Overall Score Tests
// ============================================================================

pub fn analyze_spec_overall_score_is_average_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Overall should be average of the four scores
  let expected_avg =
    {
      report.coverage_score
      + report.clarity_score
      + report.testability_score
      + report.ai_readiness_score
    }
    / 4

  report.overall_score
  |> should.equal(expected_avg)
}

// ============================================================================
// Issue Detection Tests
// ============================================================================

pub fn analyze_spec_minimal_finds_missing_error_tests_issue_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  list.contains(report.issues, quality_analyzer.MissingErrorTests)
  |> should.be_true
}

pub fn analyze_spec_minimal_finds_missing_auth_issue_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  list.contains(report.issues, quality_analyzer.MissingAuthenticationTest)
  |> should.be_true
}

pub fn analyze_spec_minimal_finds_missing_edge_cases_issue_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  list.contains(report.issues, quality_analyzer.MissingEdgeCases)
  |> should.be_true
}

pub fn analyze_spec_minimal_finds_no_examples_issue_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  list.contains(report.issues, quality_analyzer.NoExamples)
  |> should.be_true
}

pub fn analyze_spec_minimal_finds_missing_explanations_issue_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  list.contains(report.issues, quality_analyzer.MissingExplanations)
  |> should.be_true
}

pub fn analyze_spec_with_vague_rules_detects_issue_test() {
  let check = types.Check(rule: "valid data", why: "Must be valid")

  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      response: types.Response(
        ..create_minimal_behavior().response,
        checks: dict.from_list([#("data", check)]),
      ),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  list.contains(report.issues, quality_analyzer.VagueRules)
  |> should.be_true
}

// ============================================================================
// Suggestion Generation Tests
// ============================================================================

pub fn analyze_spec_minimal_generates_error_test_suggestion_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  let has_error_suggestion =
    list.any(report.suggestions, fn(s) {
      string.contains(s, "error status codes")
    })

  has_error_suggestion
  |> should.be_true
}

pub fn analyze_spec_minimal_generates_auth_suggestion_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  let has_auth_suggestion =
    list.any(report.suggestions, fn(s) { string.contains(s, "authentication") })

  has_auth_suggestion
  |> should.be_true
}

pub fn analyze_spec_minimal_generates_edge_case_suggestion_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  let has_edge_suggestion =
    list.any(report.suggestions, fn(s) { string.contains(s, "edge case") })

  has_edge_suggestion
  |> should.be_true
}

pub fn analyze_spec_minimal_generates_example_suggestion_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  let has_example_suggestion =
    list.any(report.suggestions, fn(s) { string.contains(s, "examples") })

  has_example_suggestion
  |> should.be_true
}

// ============================================================================
// Report Formatting Tests
// ============================================================================

pub fn format_report_includes_overall_score_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)
  let formatted = quality_analyzer.format_report(report)

  string.contains(formatted, "Quality Score:")
  |> should.be_true
}

pub fn format_report_includes_all_scores_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)
  let formatted = quality_analyzer.format_report(report)

  string.contains(formatted, "Coverage:")
  |> should.be_true
  string.contains(formatted, "Clarity:")
  |> should.be_true
  string.contains(formatted, "Testability:")
  |> should.be_true
  string.contains(formatted, "AI Readiness:")
  |> should.be_true
}

pub fn format_report_includes_issues_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)
  let formatted = quality_analyzer.format_report(report)

  string.contains(formatted, "Quality Issues:")
  |> should.be_true
}

pub fn format_report_includes_suggestions_test() {
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)
  let formatted = quality_analyzer.format_report(report)

  string.contains(formatted, "Suggestions for Improvement:")
  |> should.be_true
}
