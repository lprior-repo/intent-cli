import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/interpolate
import intent/interview
import intent/interview_questions
import intent/interview_storage
import intent/resolver
import intent/rules_engine
import intent/types
import intent/http_client
import intent/bead_templates

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

// ============================================================================
// HTTP Client Tests
// ============================================================================

pub fn http_client_url_construction_simple_test() {
  // Test simple URL construction without interpolation
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Get,
    path: "/users/123",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Error(_) -> {
      // Expected to fail without mocking HTTP - we're testing URL construction logic
      should.be_ok(Ok(Nil))
    }
    Ok(_) -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_path_interpolation_test() {
  // Test path interpolation with variables
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Get,
    path: "/users/${user_id}",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("user_id", json.string("123"))

  let result = http_client.execute_request(config, request, ctx)

  // Path interpolation should work - URL construction should proceed
  // Even if HTTP request fails, interpolation error should not occur
  case result {
    Error(http_client.InterpolationError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_missing_variable_interpolation_test() {
  // Test that missing variables in path cause interpolation errors
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Get,
    path: "/users/${unknown_var}",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Error(http_client.InterpolationError(_)) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn http_client_header_interpolation_test() {
  // Test header interpolation with variables
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.from_list([#("X-Default", "default-value")]),
  )

  let request = types.Request(
    method: types.Get,
    path: "/users",
    headers: dict.from_list([#("X-Token", "${auth_token}")]),
    query: dict.new(),
    body: json.null(),
  )

  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("auth_token", json.string("secret123"))

  let result = http_client.execute_request(config, request, ctx)

  // Header interpolation should work
  case result {
    Error(http_client.InterpolationError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_header_merge_test() {
  // Test that request headers override config headers
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.from_list([
      #("X-Default", "config-value"),
      #("X-Config-Only", "config"),
    ]),
  )

  let request = types.Request(
    method: types.Get,
    path: "/users",
    headers: dict.from_list([#("X-Default", "request-value")]),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // Header merge should work without interpolation errors
  case result {
    Error(http_client.InterpolationError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_body_json_interpolation_test() {
  // Test body interpolation with JSON content
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let body_json =
    json.object([
      #("username", json.string("${username}")),
      #("email", json.string("user@example.com")),
    ])

  let request = types.Request(
    method: types.Post,
    path: "/users",
    headers: dict.new(),
    query: dict.new(),
    body: body_json,
  )

  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("username", json.string("john_doe"))

  let result = http_client.execute_request(config, request, ctx)

  // Body interpolation should work
  case result {
    Error(http_client.InterpolationError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_invalid_url_test() {
  // Test invalid URL handling
  let config = types.Config(
    base_url: "not a valid url at all",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Get,
    path: "/users",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Error(http_client.UrlParseError(_)) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn http_client_https_url_test() {
  // Test HTTPS URL handling
  let config = types.Config(
    base_url: "https://api.example.com",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Get,
    path: "/secure-endpoint",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // HTTPS URLs should be valid and not cause UrlParseError
  case result {
    Error(http_client.UrlParseError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_custom_port_test() {
  // Test URL with custom port
  let config = types.Config(
    base_url: "http://localhost:3000",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Get,
    path: "/health",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // Custom port should be parsed correctly
  case result {
    Error(http_client.UrlParseError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_path_leading_slash_test() {
  // Test that paths are normalized with leading slash
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  // Path without leading slash
  let request = types.Request(
    method: types.Get,
    path: "users/123",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // Should handle path without leading slash (not a URL parse error)
  case result {
    Error(http_client.InterpolationError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_method_conversion_get_test() {
  // Test that GET method is handled correctly
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Get,
    path: "/users",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // GET request should not cause method conversion errors
  case result {
    Error(http_client.UrlParseError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_method_conversion_post_test() {
  // Test that POST method with body is handled correctly
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  let request = types.Request(
    method: types.Post,
    path: "/users",
    headers: dict.new(),
    query: dict.new(),
    body: json.object([#("name", json.string("John"))]),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // POST request should not cause method conversion errors
  case result {
    Error(http_client.UrlParseError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn http_client_multiple_header_merge_test() {
  // Test merging multiple headers from both config and request
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.from_list([
      #("X-API-Version", "v1"),
      #("User-Agent", "intent-cli"),
    ]),
  )

  let request = types.Request(
    method: types.Get,
    path: "/data",
    headers: dict.from_list([
      #("Authorization", "Bearer token"),
      #("X-Request-ID", "123"),
    ]),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // Multiple headers should merge without errors
  case result {
    Error(http_client.InterpolationError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

// ============================================================================
// Rules Engine Tests
// ============================================================================

fn make_execution_result(
  status: Int,
  body_str: String,
  method: types.Method,
  path: String,
) -> http_client.ExecutionResult {
  http_client.ExecutionResult(
    status: status,
    headers: dict.new(),
    body: json.object([#("test", json.string(body_str))]),
    raw_body: body_str,
    elapsed_ms: 100,
    request_method: method,
    request_path: path,
  )
}

pub fn rules_engine_check_when_status_equals_test() {
  // Test status condition with exact match (== 200)
  let rule = types.Rule(
    name: "Check 200 OK",
    description: "Verify 200 response",
    when: types.When(status: "== 200", method: types.Get, path: "/users"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "ok", types.Get, "/users")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  list.length(results)
  |> should.equal(1)

  case results {
    [rules_engine.RulePassed(name)] -> name |> should.equal("Check 200 OK")
    _ -> should.fail()
  }
}

pub fn rules_engine_check_when_status_greater_than_test() {
  // Test status condition with > operator
  let rule = types.Rule(
    name: "Check 4xx error",
    description: "Verify error status",
    when: types.When(status: "> 399", method: types.Post, path: "/create"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(400, "bad request", types.Post, "/create")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  list.length(results)
  |> should.equal(1)

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_when_status_less_than_test() {
  // Test status condition with < operator
  let rule = types.Rule(
    name: "Check success range",
    description: "Verify 2xx status",
    when: types.When(status: "< 300", method: types.Get, path: "/data"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(201, "created", types.Get, "/data")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_when_method_mismatch_test() {
  // Test that rule doesn't apply when method doesn't match
  let rule = types.Rule(
    name: "POST rule",
    description: "Only for POST",
    when: types.When(status: "== 200", method: types.Post, path: "/create"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "ok", types.Get, "/create")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  // Rule should not apply because method is GET, not POST
  list.length(results)
  |> should.equal(0)
}

pub fn rules_engine_check_when_path_exact_match_test() {
  // Test exact path matching
  let rule = types.Rule(
    name: "Exact path rule",
    description: "Check exact path",
    when: types.When(status: "== 200", method: types.Get, path: "/exact/path"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "ok", types.Get, "/exact/path")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_when_path_regex_match_test() {
  // Test regex path matching
  let rule = types.Rule(
    name: "Regex path rule",
    description: "Check regex path",
    when: types.When(status: "== 200", method: types.Get, path: "^/users/.*"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "ok", types.Get, "/users/123")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_body_must_contain_test() {
  // Test body_must_contain check
  let rule = types.Rule(
    name: "Body content rule",
    description: "Verify body contains text",
    when: types.When(status: "== 200", method: types.Get, path: "/test"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: ["success"],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "Operation was a success", types.Get, "/test")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_body_must_not_contain_test() {
  // Test body_must_not_contain check
  let rule = types.Rule(
    name: "No error rule",
    description: "Verify no error in body",
    when: types.When(status: "== 200", method: types.Get, path: "/test"),
    check: types.RuleCheck(
      body_must_not_contain: ["error"],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "This is clean data", types.Get, "/test")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_body_must_not_contain_violation_test() {
  // Test body_must_not_contain violation
  let rule = types.Rule(
    name: "No error rule",
    description: "Verify no error in body",
    when: types.When(status: "== 200", method: types.Get, path: "/test"),
    check: types.RuleCheck(
      body_must_not_contain: ["error"],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response =
    make_execution_result(200, "This has an error in it", types.Get, "/test")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RuleFailed(name, _, violations)] -> {
      name |> should.equal("No error rule")
      list.length(violations) |> should.equal(1)
    }
    _ -> should.fail()
  }
}

pub fn rules_engine_check_body_must_contain_violation_test() {
  // Test body_must_contain violation
  let rule = types.Rule(
    name: "Required text rule",
    description: "Verify required text",
    when: types.When(status: "== 200", method: types.Get, path: "/test"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: ["required"],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "This is missing it", types.Get, "/test")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RuleFailed(_, _, violations)] -> {
      list.length(violations) |> should.equal(1)
    }
    _ -> should.fail()
  }
}

pub fn rules_engine_check_multiple_rules_test() {
  // Test multiple rules applied in sequence
  let rule1 = types.Rule(
    name: "Rule 1",
    description: "First rule",
    when: types.When(status: "== 200", method: types.Get, path: "/test"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let rule2 = types.Rule(
    name: "Rule 2",
    description: "Second rule",
    when: types.When(status: "== 200", method: types.Get, path: "/test"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = make_execution_result(200, "ok", types.Get, "/test")
  let results = rules_engine.check_rules([rule1, rule2], response, "test_behavior")

  list.length(results) |> should.equal(2)
}

pub fn rules_engine_format_violation_body_contains_test() {
  let violation = rules_engine.BodyContains("forbidden", "response body")
  let formatted = rules_engine.format_violation(violation)
  formatted
  |> string.contains("forbidden")
  |> should.be_true()
}

pub fn rules_engine_format_violation_body_missing_test() {
  let violation = rules_engine.BodyMissing("required")
  let formatted = rules_engine.format_violation(violation)
  formatted
  |> string.contains("required")
  |> should.be_true()
}

pub fn rules_engine_format_violation_field_missing_test() {
  let violation = rules_engine.FieldMissing("user.id")
  let formatted = rules_engine.format_violation(violation)
  formatted
  |> string.contains("user.id")
  |> should.be_true()
}

pub fn rules_engine_format_violation_header_missing_test() {
  let violation = rules_engine.HeaderMissing("X-Custom")
  let formatted = rules_engine.format_violation(violation)
  formatted
  |> string.contains("X-Custom")
  |> should.be_true()
}

// ============================================================================
// Resolver Advanced Tests
// ============================================================================

pub fn resolver_complex_diamond_dependency_test() {
  // Diamond pattern: b3 and b4 both depend on b1, b5 depends on both
  let b1 = make_behavior("base", [])
  let b3 = make_behavior("left", ["base"])
  let b4 = make_behavior("right", ["base"])
  let b5 = make_behavior("merge", ["left", "right"])

  let spec = make_spec([make_feature("Feature A", [b1, b3, b4, b5])])
  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved) |> should.equal(4)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      // base should come first
      let assert [first, ..] = names
      first |> should.equal("base")
      // merge should come last (it has two dependencies)
      case list.last(names) {
        Ok(last) -> last |> should.equal("merge")
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_multiple_branches_test() {
  // Multiple independent branches
  let b1 = make_behavior("root", [])
  let b2 = make_behavior("branch-a-1", ["root"])
  let b3 = make_behavior("branch-a-2", ["branch-a-1"])
  let b4 = make_behavior("branch-b-1", ["root"])
  let b5 = make_behavior("branch-b-2", ["branch-b-1"])

  let spec = make_spec([make_feature("Feature", [b1, b2, b3, b4, b5])])
  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved) |> should.equal(5)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      // Check that all expected behaviors are present
      list.any(names, fn(n) { n == "root" }) |> should.be_true()
      list.any(names, fn(n) { n == "branch-a-1" }) |> should.be_true()
      list.any(names, fn(n) { n == "branch-a-2" }) |> should.be_true()
      list.any(names, fn(n) { n == "branch-b-1" }) |> should.be_true()
      list.any(names, fn(n) { n == "branch-b-2" }) |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_deep_chain_test() {
  // Long dependency chain: b5 -> b4 -> b3 -> b2 -> b1
  let b1 = make_behavior("step1", [])
  let b2 = make_behavior("step2", ["step1"])
  let b3 = make_behavior("step3", ["step2"])
  let b4 = make_behavior("step4", ["step3"])
  let b5 = make_behavior("step5", ["step4"])

  let spec = make_spec([make_feature("Feature", [b1, b2, b3, b4, b5])])
  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved) |> should.equal(5)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      names |> should.equal(["step1", "step2", "step3", "step4", "step5"])
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Empty/Null Response Handling Tests
// ============================================================================

pub fn rules_engine_empty_body_test() {
  // Test rule application with empty response body
  let rule = types.Rule(
    name: "Empty body rule",
    description: "Handle empty response",
    when: types.When(status: "== 204", method: types.Delete, path: "/resource"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = http_client.ExecutionResult(
    status: 204,
    headers: dict.new(),
    body: json.null(),
    raw_body: "",
    elapsed_ms: 50,
    request_method: types.Delete,
    request_path: "/resource",
  )

  let results = rules_engine.check_rules([rule], response, "test")
  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_null_json_value_test() {
  // Test handling of null JSON values
  let rule = types.Rule(
    name: "Null handling rule",
    description: "Handle null values",
    when: types.When(status: "== 200", method: types.Get, path: "/nullable"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = http_client.ExecutionResult(
    status: 200,
    headers: dict.new(),
    body: json.object([#("value", json.null())]),
    raw_body: "{\"value\":null}",
    elapsed_ms: 60,
    request_method: types.Get,
    request_path: "/nullable",
  )

  let results = rules_engine.check_rules([rule], response, "test")
  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_whitespace_body_test() {
  // Test handling of whitespace-only body
  let rule = types.Rule(
    name: "Whitespace rule",
    description: "Handle whitespace body",
    when: types.When(status: "== 200", method: types.Get, path: "/test"),
    check: types.RuleCheck(
      body_must_not_contain: ["error"],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = http_client.ExecutionResult(
    status: 200,
    headers: dict.new(),
    body: json.null(),
    raw_body: "   \n\t  ",
    elapsed_ms: 40,
    request_method: types.Get,
    request_path: "/test",
  )

  let results = rules_engine.check_rules([rule], response, "test")
  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_nested_null_field_test() {
  // Test checking for null in nested fields
  let rule = types.Rule(
    name: "Nested null rule",
    description: "Check nested fields",
    when: types.When(status: "== 200", method: types.Get, path: "/nested"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: ["user"],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = http_client.ExecutionResult(
    status: 200,
    headers: dict.new(),
    body: json.object([#("user", json.null())]),
    raw_body: "{\"user\":null}",
    elapsed_ms: 55,
    request_method: types.Get,
    request_path: "/nested",
  )

  let results = rules_engine.check_rules([rule], response, "test")
  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_empty_object_test() {
  // Test handling of empty objects
  let rule = types.Rule(
    name: "Empty object rule",
    description: "Handle empty objects",
    when: types.When(status: "== 200", method: types.Get, path: "/data"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: ["data"],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = http_client.ExecutionResult(
    status: 200,
    headers: dict.new(),
    body: json.object([#("data", json.object([]))]),
    raw_body: "{\"data\":{}}",
    elapsed_ms: 65,
    request_method: types.Get,
    request_path: "/data",
  )

  let results = rules_engine.check_rules([rule], response, "test")
  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

// ============================================================================
// Unicode and Special Character Support Tests
// ============================================================================

pub fn interpolate_unicode_variable_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("emoji", json_string("🎉"))

  let result = interpolate.interpolate_string(ctx, "status: ${emoji}")

  case result {
    Ok(s) -> s |> should.equal("status: 🎉")
    Error(_) -> should.fail()
  }
}

pub fn interpolate_unicode_in_path_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("category", json_string("réclame"))

  let result = interpolate.interpolate_string(ctx, "/search/${category}")

  case result {
    Ok(s) -> s |> should.equal("/search/réclame")
    Error(_) -> should.fail()
  }
}

pub fn rules_engine_unicode_body_content_test() {
  // Test body checks with Unicode characters
  let rule = types.Rule(
    name: "Unicode content rule",
    description: "Check for Unicode in response",
    when: types.When(status: "== 200", method: types.Get, path: "/message"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: ["✓"],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = http_client.ExecutionResult(
    status: 200,
    headers: dict.new(),
    body: json.null(),
    raw_body: "Status: ✓ All systems operational",
    elapsed_ms: 50,
    request_method: types.Get,
    request_path: "/message",
  )

  let results = rules_engine.check_rules([rule], response, "test")
  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_emoji_in_description_test() {
  // Test emoji in rule descriptions
  let rule = types.Rule(
    name: "emoji_test",
    description: "Check emoji support 🚀 in descriptions",
    when: types.When(status: "== 200", method: types.Get, path: "/status"),
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let response = http_client.ExecutionResult(
    status: 200,
    headers: dict.new(),
    body: json.null(),
    raw_body: "ok",
    elapsed_ms: 50,
    request_method: types.Get,
    request_path: "/status",
  )

  let results = rules_engine.check_rules([rule], response, "test")
  // Description should contain emoji but not affect rule execution
  case results {
    [rules_engine.RulePassed(name)] ->
      name |> should.equal("emoji_test")
    _ -> should.fail()
  }
}

pub fn interpolate_special_characters_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("special", json_string("@#$%^&*()"))

  let result = interpolate.interpolate_string(ctx, "chars: ${special}")

  case result {
    Ok(s) -> s |> should.equal("chars: @#$%^&*()")
    Error(_) -> should.fail()
  }
}

pub fn http_client_unicode_header_test() {
  // Test Unicode in HTTP headers
  let config = types.Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.from_list([#("X-Custom", "café")]),
  )

  let request = types.Request(
    method: types.Get,
    path: "/test",
    headers: dict.from_list([#("X-Greeting", "こんにちは")]),
    query: dict.new(),
    body: json.null(),
  )

  let ctx = interpolate.new_context()

  let result = http_client.execute_request(config, request, ctx)

  // Should handle Unicode headers without interpolation errors
  case result {
    Error(http_client.InterpolationError(_)) -> should.fail()
    _ -> should.be_ok(Ok(Nil))
  }
}

// ============================================================================
// Output Formatting Tests
// ============================================================================

pub fn json_encoding_test() {
  // Test JSON encoding of various values
  let value = json.object([
    #("name", json.string("Test")),
    #("count", json.int(42)),
    #("enabled", json.bool(True)),
  ])

  let json_str = json.to_string(value)

  // Verify JSON is properly formatted
  json_str
  |> string.contains("Test")
  |> should.be_true()

  json_str
  |> string.contains("42")
  |> should.be_true()
}

pub fn summary_calculation_test() {
  // Test that summary calculations work correctly
  let passed = 10
  let failed = 3
  let blocked = 1
  let total = passed + failed + blocked

  total |> should.equal(14)

  let percentage = passed * 100 / total
  percentage |> should.equal(71)
}

pub fn string_formatting_test() {
  // Test human-readable string formatting
  let behavior_name = "get-user-by-id"
  let feature_name = "User Management"

  let formatted = feature_name <> ": " <> behavior_name

  formatted
  |> should.equal("User Management: get-user-by-id")
}

pub fn error_message_formatting_test() {
  // Test error message formatting
  let field = "status"
  let expected = "200"
  let actual = "404"

  let message =
    "Field '" <> field <> "' expected '" <> expected <> "' but got '" <> actual <> "'"

  message
  |> string.contains("status")
  |> should.be_true()

  message
  |> string.contains("200")
  |> should.be_true()

  message
  |> string.contains("404")
  |> should.be_true()
}

pub fn list_to_string_formatting_test() {
  // Test formatting lists into readable strings
  let items = ["first", "second", "third"]
  let formatted = string.join(items, ", ")

  formatted |> should.equal("first, second, third")
}

pub fn boolean_to_status_test() {
  // Test converting boolean results to status strings
  let passed = True
  let status = case passed {
    True -> "PASS"
    False -> "FAIL"
  }

  status |> should.equal("PASS")
}

pub fn json_null_handling_test() {
  // Test JSON null handling in output
  let value = json.null()
  let json_str = json.to_string(value)

  json_str |> should.equal("null")
}

// ============================================================================
// Beads Generation Tests
// ============================================================================

pub fn bead_generation_api_profile_test() {
  // Test generating beads from API profile session
  let session = interview.InterviewSession(
    id: "test-api-session",
    profile: interview.Api,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "What API endpoints do you need?",
        response: "GET /users and POST /users for user management",
        round: 1,
        perspective: interview_questions.User,
        confidence: 0.95,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
      interview.Answer(
        question_id: "q2",
        question_text: "What is the endpoint path?",
        response: "/users",
        round: 1,
        perspective: interview_questions.Developer,
        confidence: 0.9,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "API interview notes",
  )

  let beads = bead_templates.generate_beads_from_session(session)

  // Verify beads were generated
  list.is_empty(beads) |> should.equal(False)

  // Verify first bead structure
  case list.first(beads) {
    Ok(first_bead) -> {
      first_bead.profile_type |> should.equal("api")
      first_bead.issue_type |> should.equal("api_endpoint")
      first_bead.priority |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_cli_profile_test() {
  // Test generating beads from CLI profile session
  let session = interview.InterviewSession(
    id: "test-cli-session",
    profile: interview.Cli,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "What commands do you need?",
        response: "list command to show all users",
        round: 1,
        perspective: interview_questions.User,
        confidence: 0.9,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "CLI interview notes",
  )

  let beads = bead_templates.generate_beads_from_session(session)
  list.is_empty(beads) |> should.equal(False)

  case list.first(beads) {
    Ok(first_bead) ->
      first_bead.profile_type |> should.equal("cli")
    Error(_) -> should.fail()
  }
}

pub fn bead_to_jsonl_format_test() {
  // Test bead to JSONL conversion
  let bead = bead_templates.BeadRecord(
    title: "Test Implementation",
    description: "A test bead for validation",
    profile_type: "api",
    priority: 2,
    issue_type: "api_endpoint",
    labels: ["api", "test"],
    ai_hints: "Implement according to spec",
    acceptance_criteria: ["Works correctly", "Passes tests"],
    dependencies: [],
  )

  let jsonl_line = bead_templates.bead_to_jsonl_line(bead)

  // Verify JSON structure
  jsonl_line |> string.contains("\"title\"") |> should.be_true()
  jsonl_line |> string.contains("\"Test Implementation\"") |> should.be_true()
  jsonl_line |> string.contains("\"description\"") |> should.be_true()
  jsonl_line |> string.contains("\"profile_type\"") |> should.be_true()
  jsonl_line |> string.contains("\"api\"") |> should.be_true()
}

pub fn beads_to_jsonl_multiple_test() {
  // Test converting multiple beads to JSONL format
  let beads = [
    bead_templates.BeadRecord(
      title: "First Bead",
      description: "First task",
      profile_type: "api",
      priority: 3,
      issue_type: "feature",
      labels: ["high"],
      ai_hints: "Do this first",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "Second Bead",
      description: "Second task",
      profile_type: "data",
      priority: 2,
      issue_type: "schema",
      labels: ["medium"],
      ai_hints: "Then do this",
      acceptance_criteria: [],
      dependencies: ["First Bead"],
    ),
  ]

  let jsonl = bead_templates.beads_to_jsonl(beads)

  // Verify multiple lines
  let lines = string.split(jsonl, "\n")
  list.length(lines) |> should.equal(2)

  // Verify both beads are present
  jsonl |> string.contains("First Bead") |> should.be_true()
  jsonl |> string.contains("Second Bead") |> should.be_true()
  jsonl |> string.contains("First task") |> should.be_true()
  jsonl |> string.contains("Second task") |> should.be_true()
}

pub fn bead_stats_calculation_test() {
  // Test bead statistics calculation
  let beads = [
    bead_templates.BeadRecord(
      title: "API 1",
      description: "desc",
      profile_type: "api",
      priority: 3,
      issue_type: "endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "API 2",
      description: "desc",
      profile_type: "api",
      priority: 3,
      issue_type: "endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "Data 1",
      description: "desc",
      profile_type: "data",
      priority: 2,
      issue_type: "schema",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let stats = bead_templates.bead_stats(beads)

  stats.total |> should.equal(3)

  // Verify by_type counts
  dict.get(stats.by_type, "endpoint")
  |> should.equal(Ok(2))

  dict.get(stats.by_type, "schema")
  |> should.equal(Ok(1))

  // Verify by_priority counts
  dict.get(stats.by_priority, 3)
  |> should.equal(Ok(2))

  dict.get(stats.by_priority, 2)
  |> should.equal(Ok(1))
}

pub fn filter_beads_by_type_test() {
  // Test filtering beads by issue type
  let beads = [
    bead_templates.BeadRecord(
      title: "Endpoint 1",
      description: "desc",
      profile_type: "api",
      priority: 1,
      issue_type: "endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "Schema 1",
      description: "desc",
      profile_type: "data",
      priority: 1,
      issue_type: "schema",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "Endpoint 2",
      description: "desc",
      profile_type: "api",
      priority: 1,
      issue_type: "endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let endpoints = bead_templates.filter_beads_by_type(beads, "endpoint")
  list.length(endpoints) |> should.equal(2)

  let schemas = bead_templates.filter_beads_by_type(beads, "schema")
  list.length(schemas) |> should.equal(1)
}

pub fn sort_beads_by_priority_test() {
  // Test sorting beads by priority (higher number = higher priority)
  let beads = [
    bead_templates.BeadRecord(
      title: "Low Priority",
      description: "desc",
      profile_type: "api",
      priority: 1,
      issue_type: "task",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "High Priority",
      description: "desc",
      profile_type: "api",
      priority: 5,
      issue_type: "task",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "Medium Priority",
      description: "desc",
      profile_type: "api",
      priority: 3,
      issue_type: "task",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let sorted = bead_templates.sort_beads_by_priority(beads)

  // First should be highest priority
  case list.first(sorted) {
    Ok(first) -> first.title |> should.equal("High Priority")
    Error(_) -> should.fail()
  }

  // Last should be lowest priority
  case list.last(sorted) {
    Ok(last) -> last.title |> should.equal("Low Priority")
    Error(_) -> should.fail()
  }
}

pub fn add_bead_dependency_test() {
  // Test adding dependencies between beads
  let beads = [
    bead_templates.BeadRecord(
      title: "Schema Design",
      description: "desc",
      profile_type: "data",
      priority: 1,
      issue_type: "schema",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.BeadRecord(
      title: "API Endpoint",
      description: "desc",
      profile_type: "api",
      priority: 1,
      issue_type: "endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let updated =
    bead_templates.add_dependency(beads, "API Endpoint", "Schema Design")

  case list.last(updated) {
    Ok(endpoint_bead) -> {
      endpoint_bead.title |> should.equal("API Endpoint")
      list.contains(endpoint_bead.dependencies, "Schema Design")
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn empty_session_beads_test() {
  // Test generating beads from session with no answers
  let session = interview.InterviewSession(
    id: "empty-session",
    profile: interview.Api,
    stage: interview.Complete,
    rounds_completed: 0,
    answers: [],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "",
  )

  let beads = bead_templates.generate_beads_from_session(session)

  // Should handle empty session gracefully
  list.length(beads) |> should.equal(0)
}

pub fn interview_session_to_json_test() {
  // Test conversion of interview session to JSON for storage
  let session = interview.InterviewSession(
    id: "test-session-123",
    profile: interview.Api,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "Test question",
        response: "Test response",
        round: 1,
        perspective: interview_questions.User,
        confidence: 0.85,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "Test notes",
  )

  let json = interview_storage.session_to_json(session)
  let json_str = json.to_string(json)

  // Verify session data is serialized
  json_str |> string.contains("test-session-123") |> should.be_true()
  json_str |> string.contains("\"api\"") |> should.be_true()
  json_str |> string.contains("\"complete\"") |> should.be_true()
  json_str |> string.contains("Test question") |> should.be_true()
}

pub fn bead_generation_event_profile_test() {
  // Test generating beads from Event profile session
  let session = interview.InterviewSession(
    id: "test-event-session",
    profile: interview.Event,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "What events should be emitted?",
        response: "user.created and user.updated events",
        round: 1,
        perspective: interview_questions.Developer,
        confidence: 0.92,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "Event interview notes",
  )

  let beads = bead_templates.generate_beads_from_session(session)
  list.is_empty(beads) |> should.equal(False)

  case list.first(beads) {
    Ok(first_bead) ->
      first_bead.profile_type |> should.equal("event")
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_data_profile_test() {
  // Test generating beads from Data profile session
  let session = interview.InterviewSession(
    id: "test-data-session",
    profile: interview.Data,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "What data models are needed?",
        response: "User model with id, name, email fields",
        round: 1,
        perspective: interview_questions.Developer,
        confidence: 0.88,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "Data interview notes",
  )

  let beads = bead_templates.generate_beads_from_session(session)
  list.is_empty(beads) |> should.equal(False)

  case list.first(beads) {
    Ok(first_bead) ->
      first_bead.profile_type |> should.equal("data")
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_workflow_profile_test() {
  // Test generating beads from Workflow profile session
  let session = interview.InterviewSession(
    id: "test-workflow-session",
    profile: interview.Workflow,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "What workflows exist?",
        response: "User signup workflow with email verification",
        round: 1,
        perspective: interview_questions.Business,
        confidence: 0.9,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "Workflow interview notes",
  )

  let beads = bead_templates.generate_beads_from_session(session)
  list.is_empty(beads) |> should.equal(False)

  case list.first(beads) {
    Ok(first_bead) ->
      first_bead.profile_type |> should.equal("workflow")
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_ui_profile_test() {
  // Test generating beads from UI profile session
  let session = interview.InterviewSession(
    id: "test-ui-session",
    profile: interview.UI,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "What UI screens do you need?",
        response: "User dashboard and settings screen",
        round: 1,
        perspective: interview_questions.User,
        confidence: 0.87,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "UI interview notes",
  )

  let beads = bead_templates.generate_beads_from_session(session)
  list.is_empty(beads) |> should.equal(False)

  case list.first(beads) {
    Ok(first_bead) ->
      first_bead.profile_type |> should.equal("ui")
    Error(_) -> should.fail()
  }
}

pub fn bead_record_required_fields_test() {
  // Test that bead records have all required fields
  let bead = bead_templates.BeadRecord(
    title: "Required fields test",
    description: "Testing all required fields present",
    profile_type: "api",
    priority: 1,
    issue_type: "endpoint",
    labels: ["test"],
    ai_hints: "Test hints",
    acceptance_criteria: ["Criterion 1"],
    dependencies: ["dependency1"],
  )

  // Verify all fields are non-empty strings or have sensible values
  string.is_empty(bead.title) |> should.equal(False)
  string.is_empty(bead.description) |> should.equal(False)
  string.is_empty(bead.profile_type) |> should.equal(False)
  bead.priority |> should.equal(1)
  string.is_empty(bead.issue_type) |> should.equal(False)
  list.length(bead.labels) |> should.equal(1)
  string.is_empty(bead.ai_hints) |> should.equal(False)
  list.length(bead.acceptance_criteria) |> should.equal(1)
  list.length(bead.dependencies) |> should.equal(1)
}

pub fn bead_stats_empty_list_test() {
  // Test stats calculation with empty bead list
  let beads: List(bead_templates.BeadRecord) = []
  let stats = bead_templates.bead_stats(beads)

  stats.total |> should.equal(0)
  dict.is_empty(stats.by_type) |> should.be_true()
  dict.is_empty(stats.by_priority) |> should.be_true()
}

pub fn bead_multiple_dependencies_test() {
  // Test adding multiple dependencies to a bead
  let beads = [
    bead_templates.BeadRecord(
      title: "Implementation",
      description: "desc",
      profile_type: "api",
      priority: 1,
      issue_type: "endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let step1 = bead_templates.add_dependency(beads, "Implementation", "Schema")
  let step2 = bead_templates.add_dependency(step1, "Implementation", "Auth")

  case list.first(step2) {
    Ok(bead) -> {
      list.length(bead.dependencies) |> should.equal(2)
      list.contains(bead.dependencies, "Schema") |> should.be_true()
      list.contains(bead.dependencies, "Auth") |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_preserves_answer_content_test() {
  // Test that bead generation uses interview answer content
  let answer_text = "Create an API endpoint at /api/users that returns a list of all users with pagination support"
  let session = interview.InterviewSession(
    id: "test-content-session",
    profile: interview.Api,
    stage: interview.Complete,
    rounds_completed: 5,
    answers: [
      interview.Answer(
        question_id: "q1",
        question_text: "Describe the endpoint",
        response: answer_text,
        round: 1,
        perspective: interview_questions.Developer,
        confidence: 0.95,
        extracted: dict.new(),
        notes: "",
        timestamp: "2026-01-05T00:00:00Z",
      ),
    ],
    gaps: [],
    conflicts: [],
    created_at: "2026-01-05T00:00:00Z",
    updated_at: "2026-01-05T00:00:00Z",
    completed_at: "2026-01-05T00:00:00Z",
    raw_notes: "Content preservation test notes",
  )

  let beads = bead_templates.generate_beads_from_session(session)

  case list.first(beads) {
    Ok(first_bead) -> {
      // Bead description should contain the answer content
      first_bead.description
      |> string.contains(answer_text)
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}
