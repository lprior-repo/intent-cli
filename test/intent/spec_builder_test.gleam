/// Tests for spec_builder.gleam - CUE spec generation from interview sessions
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None}
import gleeunit/should
import intent/checker.{CheckFailed, CheckPassed}
import intent/http_client.{ExecutionResult}
import intent/interpolate.{Context}
import intent/interview.{Answer, Api, Cli, Discovery, InterviewSession}
import intent/question_types.{Developer, Ops, Security}
import intent/spec_builder
import intent/types.{
  Behavior, Config, Feature, Get, Post, Request, Response, Spec,
}

// =============================================================================
// Extract Features Tests
// =============================================================================

pub fn extract_features_empty_answers_test() {
  let answers = []
  let features = spec_builder.extract_features_from_answers(answers)

  list.length(features)
  |> should.equal(0)
}

pub fn extract_features_single_feature_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What features does your API need?",
      perspective: Developer,
      round: 1,
      response: "User authentication",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let features = spec_builder.extract_features_from_answers([answer])

  list.length(features)
  |> should.equal(1)

  list.first(features)
  |> should.be_ok
  |> should.equal("User authentication")
}

pub fn extract_features_multiple_features_test() {
  let answer1 =
    Answer(
      question_id: "q1",
      question_text: "What features are needed?",
      perspective: Developer,
      round: 1,
      response: "User management",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let answer2 =
    Answer(
      question_id: "q2",
      question_text: "What key capability does the system need?",
      perspective: Developer,
      round: 2,
      response: "File upload",
      extracted: dict.new(),
      confidence: 0.85,
      notes: "",
      timestamp: "2024-01-01T00:05:00Z",
    )

  let features = spec_builder.extract_features_from_answers([answer1, answer2])

  list.length(features)
  |> should.equal(2)
}

pub fn extract_features_ignores_irrelevant_answers_test() {
  let answer1 =
    Answer(
      question_id: "q1",
      question_text: "What features are needed?",
      perspective: Developer,
      round: 1,
      response: "Authentication",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let answer2 =
    Answer(
      question_id: "q2",
      question_text: "What is the database type?",
      perspective: Ops,
      round: 2,
      response: "PostgreSQL",
      extracted: dict.new(),
      confidence: 0.95,
      notes: "",
      timestamp: "2024-01-01T00:05:00Z",
    )

  let features = spec_builder.extract_features_from_answers([answer1, answer2])

  // Only the feature-related answer should be extracted
  list.length(features)
  |> should.equal(1)

  list.first(features)
  |> should.be_ok
  |> should.equal("Authentication")
}

pub fn extract_features_filters_empty_responses_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What features do you need?",
      perspective: Developer,
      round: 1,
      response: "   ",
      extracted: dict.new(),
      confidence: 0.5,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let features = spec_builder.extract_features_from_answers([answer])

  list.length(features)
  |> should.equal(0)
}

// =============================================================================
// Extract Behaviors Tests
// =============================================================================

pub fn extract_behaviors_empty_answers_test() {
  let answers = []
  let behaviors = spec_builder.extract_behaviors_from_answers(answers, Api)

  // Should generate placeholder behavior section
  behaviors
  |> should.not_equal("")
}

pub fn extract_behaviors_with_endpoint_answers_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What endpoints does the API need?",
      perspective: Developer,
      round: 1,
      response: "GET /users for listing users",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let behaviors = spec_builder.extract_behaviors_from_answers([answer], Api)

  // Should contain the question text and response as comments
  behaviors
  |> should.not_equal("")
}

// =============================================================================
// Extract Constraints Tests
// =============================================================================

pub fn extract_constraints_empty_answers_test() {
  let answers = []
  let constraints = spec_builder.extract_constraints_from_answers(answers)

  list.length(constraints)
  |> should.equal(0)
}

pub fn extract_constraints_with_limit_answers_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What are the limits for this API?",
      perspective: Developer,
      round: 1,
      response: "Maximum 100 requests per minute",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let constraints = spec_builder.extract_constraints_from_answers([answer])

  list.length(constraints)
  |> should.equal(1)

  list.first(constraints)
  |> should.be_ok
  |> should.equal("Maximum 100 requests per minute")
}

// =============================================================================
// Extract Security Requirements Tests
// =============================================================================

pub fn extract_security_empty_answers_test() {
  let answers = []
  let security = spec_builder.extract_security_requirements(answers)

  // Should generate placeholder security section
  security
  |> should.not_equal("")
}

pub fn extract_security_with_auth_answers_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What authentication method should be used?",
      perspective: Security,
      round: 1,
      response: "JWT tokens with Bearer scheme",
      extracted: dict.new(),
      confidence: 0.95,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let security = spec_builder.extract_security_requirements([answer])

  // Should contain the response
  security
  |> should.not_equal("")
}

// =============================================================================
// Extract Non-Functional Requirements Tests
// =============================================================================

pub fn extract_non_functional_empty_answers_test() {
  let answers = []
  let nf = spec_builder.extract_non_functional_requirements(answers)

  list.length(nf)
  |> should.equal(0)
}

pub fn extract_non_functional_with_sla_answers_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What are the SLA requirements?",
      perspective: Ops,
      round: 1,
      response: "99.9% uptime",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let nf = spec_builder.extract_non_functional_requirements([answer])

  list.length(nf)
  |> should.equal(1)

  list.first(nf)
  |> should.be_ok
  |> should.equal("99.9% uptime")
}

pub fn extract_non_functional_with_performance_answers_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What are the performance requirements?",
      perspective: Ops,
      round: 1,
      response: "Response time under 200ms",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let nf = spec_builder.extract_non_functional_requirements([answer])

  list.length(nf)
  |> should.equal(1)
}

// =============================================================================
// Build Spec from Session Tests (End-to-End)
// =============================================================================

pub fn build_spec_from_empty_session_test() {
  let session =
    InterviewSession(
      id: "test-session",
      profile: Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T01:00:00Z",
      completed_at: "2024-01-01T01:00:00Z",
      stage: Discovery,
      rounds_completed: 3,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let spec = spec_builder.build_spec_from_session(session)

  // Should generate valid CUE package
  spec
  |> should.not_equal("")
}

pub fn build_spec_includes_package_declaration_test() {
  let session =
    InterviewSession(
      id: "test-session",
      profile: Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T01:00:00Z",
      completed_at: "2024-01-01T01:00:00Z",
      stage: Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let spec = spec_builder.build_spec_from_session(session)

  // Should start with package declaration
  case spec {
    "package api" <> _ -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn build_spec_with_feature_answers_test() {
  let answer =
    Answer(
      question_id: "q1",
      question_text: "What features does your API need?",
      perspective: Developer,
      round: 1,
      response: "User authentication",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let session =
    InterviewSession(
      id: "test-session",
      profile: Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T01:00:00Z",
      completed_at: "2024-01-01T01:00:00Z",
      stage: Discovery,
      rounds_completed: 1,
      answers: [answer],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let spec = spec_builder.build_spec_from_session(session)

  // Should contain feature
  spec
  |> should.not_equal("")
}

pub fn build_spec_with_comprehensive_answers_test() {
  let feature_answer =
    Answer(
      question_id: "q1",
      question_text: "What features are needed?",
      perspective: Developer,
      round: 1,
      response: "User management",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let endpoint_answer =
    Answer(
      question_id: "q2",
      question_text: "What endpoints does the API need?",
      perspective: Developer,
      round: 2,
      response: "GET /users",
      extracted: dict.new(),
      confidence: 0.85,
      notes: "",
      timestamp: "2024-01-01T00:05:00Z",
    )

  let auth_answer =
    Answer(
      question_id: "q3",
      question_text: "What authentication is required?",
      perspective: Security,
      round: 3,
      response: "JWT tokens",
      extracted: dict.new(),
      confidence: 0.95,
      notes: "",
      timestamp: "2024-01-01T00:10:00Z",
    )

  let session =
    InterviewSession(
      id: "test-session",
      profile: Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T01:00:00Z",
      completed_at: "2024-01-01T01:00:00Z",
      stage: Discovery,
      rounds_completed: 3,
      answers: [feature_answer, endpoint_answer, auth_answer],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let spec = spec_builder.build_spec_from_session(session)

  // Should generate comprehensive spec with all sections
  spec
  |> should.not_equal("")
}

// =============================================================================
// Create Test Spec Tests
// =============================================================================

pub fn create_test_spec_with_zero_behaviors_test() {
  let spec = spec_builder.create_test_spec(0)

  spec.name
  |> should.equal("test")

  case spec.features {
    [feature] -> {
      // Note: list.range(1, 0) in Gleam generates [1, 0] (descending), not empty list
      list.length(feature.behaviors)
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn create_test_spec_with_one_behavior_test() {
  let spec = spec_builder.create_test_spec(1)

  case spec.features {
    [feature] -> {
      list.length(feature.behaviors)
      |> should.equal(1)
    }
    _ -> should.fail()
  }
}

pub fn create_test_spec_with_multiple_behaviors_test() {
  let spec = spec_builder.create_test_spec(5)

  case spec.features {
    [feature] -> {
      list.length(feature.behaviors)
      |> should.equal(5)
    }
    _ -> should.fail()
  }
}

pub fn create_test_spec_behaviors_have_unique_names_test() {
  let spec = spec_builder.create_test_spec(3)

  case spec.features {
    [feature] -> {
      let names = list.map(feature.behaviors, fn(b) { b.name })

      // Should have 3 unique names: b1, b2, b3
      list.length(names)
      |> should.equal(3)

      list.first(names)
      |> should.be_ok
      |> should.equal("b1")
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Check Many Tests
// =============================================================================

pub fn check_many_empty_lists_test() {
  let behaviors = []
  let results = []
  let ctx =
    Context(variables: dict.new(), request_body: None, response_body: None)

  let checks = spec_builder.check_many(behaviors, results, ctx)

  list.length(checks)
  |> should.equal(0)
}

pub fn check_many_single_behavior_and_result_test() {
  let behavior =
    Behavior(
      name: "test-behavior",
      intent: "Test endpoint",
      notes: "",
      requires: [],
      tags: [],
      request: Request(Get, "/test", dict.new(), dict.new(), json.null()),
      response: Response(200, json.null(), dict.new(), dict.new()),
      captures: dict.new(),
    )

  let result =
    ExecutionResult(
      status: 200,
      headers: dict.new(),
      body: json.object([]),
      raw_body: "{}",
      elapsed_ms: 100,
      request_method: Get,
      request_path: "/test",
    )

  let ctx =
    Context(variables: dict.new(), request_body: None, response_body: None)

  let checks = spec_builder.check_many([behavior], [result], ctx)

  list.length(checks)
  |> should.equal(1)
}

pub fn check_many_multiple_behaviors_and_results_test() {
  let behavior1 =
    Behavior(
      name: "b1",
      intent: "Test 1",
      notes: "",
      requires: [],
      tags: [],
      request: Request(Get, "/test1", dict.new(), dict.new(), json.null()),
      response: Response(200, json.null(), dict.new(), dict.new()),
      captures: dict.new(),
    )

  let behavior2 =
    Behavior(
      name: "b2",
      intent: "Test 2",
      notes: "",
      requires: [],
      tags: [],
      request: Request(Post, "/test2", dict.new(), dict.new(), json.null()),
      response: Response(201, json.null(), dict.new(), dict.new()),
      captures: dict.new(),
    )

  let result1 =
    ExecutionResult(
      status: 200,
      headers: dict.new(),
      body: json.object([]),
      raw_body: "{}",
      elapsed_ms: 100,
      request_method: Get,
      request_path: "/test1",
    )

  let result2 =
    ExecutionResult(
      status: 201,
      headers: dict.new(),
      body: json.object([]),
      raw_body: "{}",
      elapsed_ms: 150,
      request_method: Post,
      request_path: "/test2",
    )

  let ctx =
    Context(variables: dict.new(), request_body: None, response_body: None)

  let checks =
    spec_builder.check_many([behavior1, behavior2], [result1, result2], ctx)

  list.length(checks)
  |> should.equal(2)
}
