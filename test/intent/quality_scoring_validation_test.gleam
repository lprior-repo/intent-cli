/// Validate exact scoring calculations for all 5 quality dimensions
/// Tests that each dimension calculates scores according to documented formulas:
/// 1. Coverage Score: base(50) + error_bonus + auth_bonus + edge_bonus + antipattern_bonus
/// 2. Clarity Score: base(60) + intent_bonus + notes_bonus + vague_penalty
/// 3. Testability Score: base(70) + capture_bonus + deps_bonus + example_bonus
/// 4. AI Readiness Score: base(50) + hints_bonus + why_bonus + example_bonus
/// 5. Overall Score: average of the four dimension scores
import gleam/dict
import gleam/json
import gleam/option
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

// ============================================================================
// 1. Coverage Score Exact Validation
// ============================================================================

pub fn coverage_score_base_only_test() {
  // Base score: 50
  // No bonuses
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 (base)
  report.coverage_score
  |> should.equal(50)
}

pub fn coverage_score_with_one_error_test_test() {
  // Base: 50
  // Error bonus: min(50, 1 * 10) = 10
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      name: "not_found",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 404,
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

  // Expected: 50 + 10 = 60
  report.coverage_score
  |> should.equal(60)
}

pub fn coverage_score_with_two_error_tests_test() {
  // Base: 50
  // Error bonus: min(50, 2 * 10) = 20
  let behavior1 =
    types.Behavior(
      ..create_minimal_behavior(),
      name: "not_found",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 404,
      ),
    )

  let behavior2 =
    types.Behavior(
      ..create_minimal_behavior(),
      name: "server_error",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 500,
      ),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [
          behavior1,
          behavior2,
        ]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 + 20 = 70
  report.coverage_score
  |> should.equal(70)
}

pub fn coverage_score_with_auth_test_test() {
  // Base: 50
  // Auth bonus: 10 (contains "auth" in name)
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      name: "unauthorized_access",
      intent: "Test authentication",
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 + 10 = 60
  report.coverage_score
  |> should.equal(60)
}

pub fn coverage_score_with_edge_case_test_test() {
  // Base: 50
  // Edge bonus: 10 (contains "empty" in name)
  let behavior =
    types.Behavior(..create_minimal_behavior(), name: "empty_input")

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 + 10 = 60
  report.coverage_score
  |> should.equal(60)
}

pub fn coverage_score_with_antipattern_rules_test() {
  // Base: 50
  // Antipattern bonus: min(5, 2 * 2) = 4
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

  // Expected: 50 + 4 = 54
  report.coverage_score
  |> should.equal(54)
}

pub fn coverage_score_all_bonuses_combined_test() {
  // Base: 50
  // Error bonus: min(50, 1 * 10) = 10
  // Auth bonus: 10
  // Edge bonus: 10
  // Antipattern bonus: min(5, 1 * 2) = 2
  let behavior1 =
    types.Behavior(
      ..create_minimal_behavior(),
      name: "unauthorized_auth_test",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 401,
      ),
    )

  let behavior2 =
    types.Behavior(..create_minimal_behavior(), name: "empty_input_test")

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [
          behavior1,
          behavior2,
        ]),
      ],
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
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 + 10 + 10 + 10 + 2 = 82
  report.coverage_score
  |> should.equal(82)
}

pub fn coverage_score_caps_at_100_test() {
  // Create 6 error behaviors: 6 * 10 = 60 (capped at 50)
  // Base: 50
  // Error bonus: min(50, 6 * 10) = 50
  // Auth bonus: 10
  // Edge bonus: 10
  // Antipattern bonus: min(5, 3 * 2) = 5
  // Total: 50 + 50 + 10 + 10 + 5 = 125, capped at 100
  let error_behaviors = [
    types.Behavior(
      ..create_minimal_behavior(),
      name: "auth_error_1",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 401,
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "error_2",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 404,
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "error_3",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 500,
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "error_4",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 403,
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "error_5",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 409,
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "empty_error_6",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 400,
      ),
    ),
  ]

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(
          name: "test",
          description: "Test",
          behaviors: error_behaviors,
        ),
      ],
      rules: [
        types.Rule(
          name: "rule1",
          description: "Test",
          check: types.RuleCheck(
            body_must_not_contain: [],
            body_must_contain: [],
            fields_must_exist: [],
            fields_must_not_exist: [],
            header_must_exist: "",
            header_must_not_exist: "",
          ),
          example: json.string("test"),
          when: option.None,
        ),
        types.Rule(
          name: "rule2",
          description: "Test",
          check: types.RuleCheck(
            body_must_not_contain: [],
            body_must_contain: [],
            fields_must_exist: [],
            fields_must_not_exist: [],
            header_must_exist: "",
            header_must_not_exist: "",
          ),
          example: json.string("test"),
          when: option.None,
        ),
        types.Rule(
          name: "rule3",
          description: "Test",
          check: types.RuleCheck(
            body_must_not_contain: [],
            body_must_contain: [],
            fields_must_exist: [],
            fields_must_not_exist: [],
            header_must_exist: "",
            header_must_not_exist: "",
          ),
          example: json.string("test"),
          when: option.None,
        ),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 100 (capped)
  report.coverage_score
  |> should.equal(100)
}

// ============================================================================
// 2. Clarity Score Exact Validation
// ============================================================================

pub fn clarity_score_base_only_test() {
  // Base: 60
  // No bonuses or penalties
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 60 (base)
  report.clarity_score
  |> should.equal(60)
}

pub fn clarity_score_with_intent_test() {
  // Base: 60
  // Intent bonus: min(10, (1/1)*100/10) = min(10, 10) = 10
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      intent: "Retrieve a specific item",
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 60 + 10 = 70
  report.clarity_score
  |> should.equal(70)
}

pub fn clarity_score_with_notes_test() {
  // Base: 60
  // Notes bonus: (1/1) * 10 = 10
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      notes: "This endpoint uses caching",
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 60 + 10 = 70
  report.clarity_score
  |> should.equal(70)
}

pub fn clarity_score_with_vague_rule_penalty_test() {
  // Base: 60
  // Vague penalty: -10
  let check = types.Check(rule: "response.data is valid", why: "Must be valid")

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

  // Expected: 60 - 10 = 50
  report.clarity_score
  |> should.equal(50)
}

pub fn clarity_score_with_intent_and_notes_test() {
  // Base: 60
  // Intent bonus: 10
  // Notes bonus: 10
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      intent: "Retrieve item",
      notes: "Uses cache",
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 60 + 10 + 10 = 80
  report.clarity_score
  |> should.equal(80)
}

pub fn clarity_score_all_bonuses_minus_penalty_test() {
  // Base: 60
  // Intent bonus: 10
  // Notes bonus: 10
  // Vague penalty: -10
  let check = types.Check(rule: "data is valid", why: "Must be valid")

  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      intent: "Get data",
      notes: "Important note",
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

  // Expected: 60 + 10 + 10 - 10 = 70
  report.clarity_score
  |> should.equal(70)
}

// ============================================================================
// 3. Testability Score Exact Validation
// ============================================================================

pub fn testability_score_base_only_test() {
  // Base: 70
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 70 (base)
  report.testability_score
  |> should.equal(70)
}

pub fn testability_score_with_one_capture_test() {
  // Base: 70
  // Capture bonus: min(10, 1 * 5) = 5
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

  // Expected: 70 + 5 = 75
  report.testability_score
  |> should.equal(75)
}

pub fn testability_score_with_two_captures_test() {
  // Base: 70
  // Capture bonus: counts behaviors with captures, not total captures
  // 1 behavior with captures: min(10, 1 * 5) = 5
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      captures: dict.from_list([
        #("item_id", "response.id"),
        #("name", "response.name"),
      ]),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 70 + 5 = 75
  report.testability_score
  |> should.equal(75)
}

pub fn testability_score_with_dependency_test() {
  // Base: 70
  // Deps bonus: min(10, 1 * 5) = 5
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

  // Expected: 70 + 5 = 75
  report.testability_score
  |> should.equal(75)
}

pub fn testability_score_with_example_test() {
  // Base: 70
  // Example bonus: min(5, with_examples / max(1, length(behaviors) / 2))
  // with_examples = 1, length(behaviors) = 1
  // min(5, 1 / max(1, 1/2)) = min(5, 1 / max(1, 0)) = min(5, 1 / 1) = min(5, 1) = 1
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

  // Expected: 70 + 1 = 71
  report.testability_score
  |> should.equal(71)
}

pub fn testability_score_all_bonuses_combined_test() {
  // Base: 70
  // Capture bonus: 1 behavior with captures: min(10, 1 * 5) = 5
  // Deps bonus: 1 behavior with dependencies: min(10, 1 * 5) = 5
  // Example bonus: 1 behavior with example: min(5, 1 / max(1, 1/2)) = 1
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      captures: dict.from_list([
        #("item_id", "response.id"),
        #("name", "response.name"),
      ]),
      requires: ["create_item"],
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

  // Expected: 70 + 5 + 5 + 1 = 81
  report.testability_score
  |> should.equal(81)
}

// ============================================================================
// 4. AI Readiness Score Exact Validation
// ============================================================================

pub fn ai_readiness_score_base_with_penalty_test() {
  // Base: 50
  // No AI hints: -10
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 - 10 = 40
  report.ai_readiness_score
  |> should.equal(40)
}

pub fn ai_readiness_score_with_implementation_hints_test() {
  // Base: 50
  // AI hints bonus: 20 (has suggested_stack)
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      ai_hints: types.AIHints(
        ..create_minimal_spec().ai_hints,
        implementation: types.ImplementationHints(suggested_stack: [
          "Express.js",
        ]),
      ),
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 + 20 = 70
  report.ai_readiness_score
  |> should.equal(70)
}

pub fn ai_readiness_score_with_pitfalls_test() {
  // Base: 50
  // AI hints bonus: 20 (has pitfalls)
  let spec =
    types.Spec(
      ..create_minimal_spec(),
      ai_hints: types.AIHints(
        ..create_minimal_spec().ai_hints,
        pitfalls: ["Avoid N+1 queries"],
      ),
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: 50 + 20 = 70
  report.ai_readiness_score
  |> should.equal(70)
}

pub fn ai_readiness_score_with_why_explanation_test() {
  // Base: 50
  // No hints: -10
  // Why bonus: (1 / 1) * 30 = 30
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

  // Expected: 50 - 10 + 30 = 70
  report.ai_readiness_score
  |> should.equal(70)
}

pub fn ai_readiness_score_with_example_test() {
  // Base: 50
  // No hints: -10
  // Example bonus: min(10, 1 * 5) = 5
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

  // Expected: 50 - 10 + 5 = 45
  report.ai_readiness_score
  |> should.equal(45)
}

pub fn ai_readiness_score_all_bonuses_combined_test() {
  // Base: 50
  // AI hints bonus: 20
  // Why bonus: (1 / 1) * 30 = 30
  // Example bonus: min(10, 1 * 5) = 5
  let check = types.Check(rule: "response.id == integer", why: "ID is required")

  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      response: types.Response(
        ..create_minimal_behavior().response,
        checks: dict.from_list([#("id", check)]),
        example: json.object([#("id", json.int(1))]),
      ),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
      ai_hints: types.AIHints(
        ..create_minimal_spec().ai_hints,
        implementation: types.ImplementationHints(suggested_stack: ["Node.js"]),
      ),
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Expected: min(100, 50 + 20 + 30 + 5) = min(100, 105) = 100
  report.ai_readiness_score
  |> should.equal(100)
}

// ============================================================================
// 5. Overall Score Exact Validation
// ============================================================================

pub fn overall_score_is_exact_average_of_four_dimensions_test() {
  // Create spec with known scores
  let spec = create_minimal_spec()
  let report = quality_analyzer.analyze_spec(spec)

  // Calculate expected average
  let expected =
    {
      report.coverage_score
      + report.clarity_score
      + report.testability_score
      + report.ai_readiness_score
    }
    / 4

  // Verify overall score equals average
  report.overall_score
  |> should.equal(expected)
}

pub fn overall_score_with_varied_dimensions_test() {
  // Create spec with varied dimension scores
  // Coverage: 60 (base 50 + edge 10)
  // Clarity: 70 (base 60 + intent 10)
  // Testability: 75 (base 70 + capture 5)
  // AI Readiness: 70 (base 50 + hints 20)
  // Average: (60 + 70 + 75 + 70) / 4 = 275 / 4 = 68
  let behavior =
    types.Behavior(
      ..create_minimal_behavior(),
      name: "empty_test",
      intent: "Test empty input",
      captures: dict.from_list([#("id", "response.id")]),
    )

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: [behavior]),
      ],
      ai_hints: types.AIHints(
        ..create_minimal_spec().ai_hints,
        implementation: types.ImplementationHints(suggested_stack: ["Node"]),
      ),
    )

  let report = quality_analyzer.analyze_spec(spec)

  // Verify individual scores
  report.coverage_score
  |> should.equal(60)

  report.clarity_score
  |> should.equal(70)

  report.testability_score
  |> should.equal(75)

  report.ai_readiness_score
  |> should.equal(70)

  // Verify overall is exact average
  report.overall_score
  |> should.equal(68)
}

pub fn overall_score_with_maximum_scores_test() {
  // Create spec that achieves maximum in all dimensions
  let behaviors = [
    types.Behavior(
      ..create_minimal_behavior(),
      name: "auth_error_1",
      intent: "Test auth",
      notes: "Auth note",
      captures: dict.from_list([
        #("id", "response.id"),
        #("token", "response.token"),
      ]),
      requires: ["setup"],
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 401,
        example: json.object([#("error", json.string("Unauthorized"))]),
        checks: dict.from_list([
          #(
            "error",
            types.Check(rule: "response.error == string", why: "Error required"),
          ),
        ]),
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "empty_edge_2",
      intent: "Test empty",
      notes: "Edge note",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 400,
        example: json.object([#("error", json.string("Bad request"))]),
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "error_3",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 404,
      ),
    ),
    types.Behavior(
      ..create_minimal_behavior(),
      name: "error_4",
      response: types.Response(
        ..create_minimal_behavior().response,
        status: 500,
      ),
    ),
  ]

  let spec =
    types.Spec(
      ..create_minimal_spec(),
      features: [
        types.Feature(name: "test", description: "Test", behaviors: behaviors),
      ],
      rules: [
        types.Rule(
          name: "rule1",
          description: "Test",
          check: types.RuleCheck(
            body_must_not_contain: [],
            body_must_contain: [],
            fields_must_exist: [],
            fields_must_not_exist: [],
            header_must_exist: "",
            header_must_not_exist: "",
          ),
          example: json.string("test"),
          when: option.None,
        ),
      ],
      ai_hints: types.AIHints(
        ..create_minimal_spec().ai_hints,
        implementation: types.ImplementationHints(suggested_stack: ["Node.js"]),
      ),
    )

  let report = quality_analyzer.analyze_spec(spec)

  // All dimensions should be high
  { report.coverage_score >= 90 }
  |> should.be_true

  { report.clarity_score >= 70 }
  |> should.be_true

  { report.testability_score >= 80 }
  |> should.be_true

  { report.ai_readiness_score >= 90 }
  |> should.be_true

  // Overall should be average
  let expected =
    {
      report.coverage_score
      + report.clarity_score
      + report.testability_score
      + report.ai_readiness_score
    }
    / 4

  report.overall_score
  |> should.equal(expected)
}
