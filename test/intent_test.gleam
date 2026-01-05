import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/interpolate
import intent/interview
import intent/interview_questions
import intent/resolver
import intent/types

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Resolver Tests
// ============================================================================

fn make_behavior(name: String, requires: List(String)) -> types.Behavior {
  types.Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: types.Request(
      method: types.Get,
      path: "/" <> name,
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
    captures: dict.new(),
  )
}

fn make_feature(name: String, behaviors: List(types.Behavior)) -> types.Feature {
  types.Feature(name: name, description: "Test feature", behaviors: behaviors)
}

fn make_spec(features: List(types.Feature)) -> types.Spec {
  types.Spec(
    name: "Test Spec",
    description: "Test spec",
    audience: "",
    version: "1.0.0",
    success_criteria: [],
    config: types.Config(
      base_url: "http://localhost",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
    features: features,
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

pub fn resolver_simple_no_deps_test() {
  // Three behaviors with no dependencies - all should be executable
  let b1 = make_behavior("first", [])
  let b2 = make_behavior("second", [])
  let b3 = make_behavior("third", [])

  let spec = make_spec([make_feature("Feature A", [b1, b2, b3])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_linear_dependency_chain_test() {
  // b3 -> b2 -> b1 (b3 depends on b2, b2 depends on b1)
  let b1 = make_behavior("first", [])
  let b2 = make_behavior("second", ["first"])
  let b3 = make_behavior("third", ["second"])

  let spec = make_spec([make_feature("Feature A", [b1, b2, b3])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)

      // Verify order: first, second, third
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      names
      |> should.equal(["first", "second", "third"])
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_multiple_deps_on_one_test() {
  // b2 and b3 both depend on b1
  let b1 = make_behavior("base", [])
  let b2 = make_behavior("child-a", ["base"])
  let b3 = make_behavior("child-b", ["base"])

  let spec = make_spec([make_feature("Feature A", [b1, b2, b3])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)

      // First should be base
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let assert [first, ..] = names
      first
      |> should.equal("base")
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_missing_dependency_test() {
  // b1 depends on "nonexistent" which doesn't exist
  let b1 = make_behavior("first", ["nonexistent"])

  let spec = make_spec([make_feature("Feature A", [b1])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(_) -> should.fail()
    Error(resolver.MissingDependency(behavior, missing)) -> {
      behavior
      |> should.equal("first")
      missing
      |> should.equal("nonexistent")
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_cyclic_dependency_test() {
  // b1 -> b2 -> b1 (cycle)
  let b1 = make_behavior("first", ["second"])
  let b2 = make_behavior("second", ["first"])

  let spec = make_spec([make_feature("Feature A", [b1, b2])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(_) -> should.fail()
    Error(resolver.CyclicDependency(_)) -> should.be_ok(Ok(Nil))
    Error(_) -> should.fail()
  }
}

pub fn resolver_duplicate_name_test() {
  // Two behaviors with the same name
  let b1 = make_behavior("same-name", [])
  let b2 = make_behavior("same-name", [])

  let spec = make_spec([make_feature("Feature A", [b1, b2])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(_) -> should.fail()
    Error(resolver.DuplicateBehaviorName(name)) -> {
      name
      |> should.equal("same-name")
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_cross_feature_deps_test() {
  // b2 in Feature B depends on b1 in Feature A
  let b1 = make_behavior("base", [])
  let b2 = make_behavior("dependent", ["base"])

  let spec =
    make_spec([
      make_feature("Feature A", [b1]),
      make_feature("Feature B", [b2]),
    ])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(2)

      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      names
      |> should.equal(["base", "dependent"])
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Interpolation Tests
// ============================================================================

pub fn interpolate_simple_variable_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("user_id", json_string("12345"))

  let result = interpolate.interpolate_string(ctx, "/users/${user_id}")

  result
  |> should.be_ok

  case result {
    Ok(s) ->
      s
      |> should.equal("/users/12345")
    Error(_) -> should.fail()
  }
}

pub fn interpolate_multiple_variables_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("org", json_string("acme"))
    |> interpolate.set_variable("team", json_string("dev"))

  let result = interpolate.interpolate_string(ctx, "/orgs/${org}/teams/${team}")

  case result {
    Ok(s) ->
      s
      |> should.equal("/orgs/acme/teams/dev")
    Error(_) -> should.fail()
  }
}

pub fn interpolate_missing_variable_test() {
  let ctx = interpolate.new_context()

  let result = interpolate.interpolate_string(ctx, "/users/${unknown}")

  result
  |> should.be_error
}

pub fn interpolate_no_variables_test() {
  let ctx = interpolate.new_context()

  let result = interpolate.interpolate_string(ctx, "/users/static")

  case result {
    Ok(s) ->
      s
      |> should.equal("/users/static")
    Error(_) -> should.fail()
  }
}

// Helper to create a JSON string value
fn json_string(s: String) -> json.Json {
  json.string(s)
}

// ============================================================================
// Interview Engine Tests
// ============================================================================

pub fn interview_get_questions_api_round_1_test() {
  let questions = interview.get_questions_for_round(interview.Api, 1)
  let has_questions = list.length(questions) > 0
  has_questions |> should.be_true()
}

pub fn interview_get_questions_cli_round_1_test() {
  let questions = interview.get_questions_for_round(interview.Cli, 1)
  let has_questions = list.length(questions) > 0
  has_questions |> should.be_true()
}

pub fn interview_create_session_test() {
  let session =
    interview.create_session("test-session-1", interview.Api, "2024-01-01T00:00:00Z")

  session.id |> should.equal("test-session-1")
  session.profile |> should.equal(interview.Api)
  session.rounds_completed |> should.equal(0)
  session.stage |> should.equal(interview.Discovery)
  session.answers |> list.length() |> should.equal(0)
}

pub fn interview_extract_auth_method_jwt_test() {
  let extracted =
    interview.extract_from_answer("q1", "We use JWT tokens for authentication", [
      "auth_method",
    ])
  let auth_method = dict.get(extracted, "auth_method")
  auth_method |> should.equal(Ok("jwt"))
}

pub fn interview_extract_auth_method_oauth_test() {
  let extracted =
    interview.extract_from_answer("q1", "OAuth 2.0 is our auth standard", [
      "auth_method",
    ])
  let auth_method = dict.get(extracted, "auth_method")
  auth_method |> should.equal(Ok("oauth"))
}

pub fn interview_extract_entities_test() {
  let extracted =
    interview.extract_from_answer("q1", "Users, Orders, Products, Payments", [
      "entities",
    ])
  let entities = dict.get(extracted, "entities")
  entities
  |> should.equal(Ok("Users, Orders, Products, Payments"))
}

pub fn interview_extract_audience_mobile_test() {
  let extracted =
    interview.extract_from_answer("q1", "Mainly mobile app users", [
      "audience",
    ])
  let audience = dict.get(extracted, "audience")
  audience |> should.equal(Ok("mobile"))
}

pub fn interview_detect_gaps_empty_answers_test() {
  let answers = []
  let gaps = interview.detect_gaps(interview.Api, answers)
  let has_gaps = list.length(gaps) > 0
  has_gaps |> should.be_true()
}

pub fn interview_detect_gaps_with_answers_test() {
  let answers = [
    interview.Answer(
      question_id: "q1",
      question_text: "What auth?",
      perspective: interview_questions.Security,
      round: 1,
      response: "JWT",
      extracted: dict.from_list([#("auth_method", "jwt")]),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
    interview.Answer(
      question_id: "q2",
      question_text: "What entities?",
      perspective: interview_questions.Developer,
      round: 1,
      response: "Users, Tokens",
      extracted: dict.from_list([#("entities", "Users, Tokens"), #("base_url", "http://localhost:8080")]),
      confidence: 0.85,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
    interview.Answer(
      question_id: "q3",
      question_text: "Happy path?",
      perspective: interview_questions.User,
      round: 1,
      response: "Login and get token",
      extracted: dict.from_list([#("happy_path", "Login and get token")]),
      confidence: 0.8,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
    interview.Answer(
      question_id: "q4",
      question_text: "Errors?",
      perspective: interview_questions.User,
      round: 2,
      response: "Wrong password, user not found",
      extracted: dict.from_list([#("error_cases", "Wrong password")]),
      confidence: 0.75,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
    interview.Answer(
      question_id: "q5",
      question_text: "Format?",
      perspective: interview_questions.Developer,
      round: 1,
      response: "JSON response format",
      extracted: dict.from_list([#("response_format", "json")]),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
  ]

  let gaps = interview.detect_gaps(interview.Api, answers)
  gaps |> list.length() |> should.equal(0)
}

pub fn interview_detect_conflicts_cap_theorem_test() {
  let answers = [
    interview.Answer(
      question_id: "q1",
      question_text: "Performance?",
      perspective: interview_questions.Ops,
      round: 3,
      response: "We need fast latency, under 50ms",
      extracted: dict.from_list([]),
      confidence: 0.8,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
    interview.Answer(
      question_id: "q2",
      question_text: "Consistency?",
      perspective: interview_questions.Developer,
      round: 3,
      response: "All data must be strongly consistent",
      extracted: dict.from_list([]),
      confidence: 0.85,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
  ]

  let conflicts = interview.detect_conflicts(answers)
  conflicts |> list.any(fn(c) { c.id == "conflict-cap" })
  |> should.be_true()
}

pub fn interview_calculate_confidence_high_test() {
  let extracted = dict.from_list([
    #("auth_method", "jwt"),
    #("audience", "mobile"),
  ])
  let confidence =
    interview.calculate_confidence(
      "q1",
      "This is a very detailed response about JWT authentication and mobile users with specific requirements",
      extracted,
    )
  let is_high = confidence >. 0.8
  is_high |> should.be_true()
}

pub fn interview_add_answer_test() {
  let session = interview.create_session("test-1", interview.Api, "2024-01-01T00:00:00Z")

  let answer = interview.Answer(
    question_id: "q1",
    question_text: "Test",
    perspective: interview_questions.User,
    round: 1,
    response: "Test response",
    extracted: dict.from_list([]),
    confidence: 0.8,
    notes: "",
    timestamp: "2024-01-01T00:01:00Z",
  )

  let updated = interview.add_answer(session, answer)
  updated.answers |> list.length() |> should.equal(1)
  updated.updated_at |> should.equal("2024-01-01T00:01:00Z")
}

pub fn interview_complete_round_test() {
  let session = interview.create_session("test-1", interview.Api, "2024-01-01T00:00:00Z")
  let after_round_1 = interview.complete_round(session)

  after_round_1.rounds_completed |> should.equal(1)
  after_round_1.stage |> should.equal(interview.Refinement)
}

pub fn interview_format_question_critical_test() {
  let question = interview_questions.Question(
    id: "q1",
    round: 1,
    perspective: interview_questions.User,
    category: interview_questions.HappyPath,
    priority: interview_questions.Critical,
    question: "What should this do?",
    context: "Start simple",
    example: "Example here",
    expected_type: "text",
    extract_into: [],
    depends_on: [],
    blocks: [],
  )

  let formatted = interview.format_question(question)
  formatted |> string.contains("[CRITICAL]") |> should.be_true()
  formatted |> string.contains("What should this do?") |> should.be_true()
}
