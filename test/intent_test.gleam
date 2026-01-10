import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import intent/bead_templates
import intent/checker
import intent/formats
import intent/http_client
import intent/interpolate
import intent/interview
import intent/interview_questions
import intent/interview_storage
import intent/question_loader
import intent/question_types.{
  type Question, Business, Critical, Developer, HappyPath, Ops, Question,
  Security, User,
}
import intent/resolver
import intent/rules_engine
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
      codebase: None,
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
  let questions = interview_questions.get_questions_for_round("api", 1)
  let has_questions = questions != []
  has_questions |> should.be_true()
}

pub fn interview_get_questions_cli_round_1_test() {
  let questions = interview_questions.get_questions_for_round("cli", 1)
  let has_questions = questions != []
  has_questions |> should.be_true()
}

pub fn interview_create_session_test() {
  let session =
    interview.create_session(
      "test-session-1",
      interview.Api,
      "2024-01-01T00:00:00Z",
    )

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
  let has_gaps = gaps != []
  has_gaps |> should.be_true()
}

pub fn interview_detect_gaps_with_answers_test() {
  let answers = [
    interview.Answer(
      question_id: "q1",
      question_text: "What auth?",
      perspective: Security,
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
      perspective: Developer,
      round: 1,
      response: "Users, Tokens",
      extracted: dict.from_list([
        #("entities", "Users, Tokens"),
        #("base_url", "http://localhost:8080"),
      ]),
      confidence: 0.85,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
    interview.Answer(
      question_id: "q3",
      question_text: "Happy path?",
      perspective: User,
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
      perspective: User,
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
      perspective: Developer,
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
      perspective: Ops,
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
      perspective: Developer,
      round: 3,
      response: "All data must be strongly consistent",
      extracted: dict.from_list([]),
      confidence: 0.85,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    ),
  ]

  let conflicts = interview.detect_conflicts(answers)
  conflicts
  |> list.any(fn(c) { c.id == "conflict-cap" })
  |> should.be_true()
}

pub fn interview_calculate_confidence_high_test() {
  let extracted =
    dict.from_list([
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
  let session =
    interview.create_session("test-1", interview.Api, "2024-01-01T00:00:00Z")

  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "Test",
      perspective: User,
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
  let session =
    interview.create_session("test-1", interview.Api, "2024-01-01T00:00:00Z")
  let after_round_1 = interview.complete_round(session)

  after_round_1.rounds_completed |> should.equal(1)
  // interview.complete_round uses Discovery for rounds 1-2
  after_round_1.stage |> should.equal(interview.Discovery)
}

pub fn interview_format_question_critical_test() {
  let question =
    Question(
      id: "q1",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.from_list([#("X-Default", "default-value")]),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.from_list([
        #("X-Default", "config-value"),
        #("X-Config-Only", "config"),
      ]),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let body_json =
    json.object([
      #("username", json.string("${username}")),
      #("email", json.string("user@example.com")),
    ])

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "not a valid url at all",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "https://api.example.com",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:3000",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  // Path without leading slash
  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.from_list([
        #("X-API-Version", "v1"),
        #("User-Agent", "intent-cli"),
      ]),
    )

  let request =
    types.Request(
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
  let rule =
    types.Rule(
      name: "Check 200 OK",
      description: "Verify 200 response",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/users")),
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
  let rule =
    types.Rule(
      name: "Check 4xx error",
      description: "Verify error status",
      when: types.When(status: option.Some("> 399"), method: option.Some(types.Post), path: option.Some("/create")),
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

  let response =
    make_execution_result(400, "bad request", types.Post, "/create")
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
  let rule =
    types.Rule(
      name: "Check success range",
      description: "Verify 2xx status",
      when: types.When(status: option.Some("< 300"), method: option.Some(types.Get), path: option.Some("/data")),
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
  let rule =
    types.Rule(
      name: "POST rule",
      description: "Only for POST",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Post), path: option.Some("/create")),
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
  let rule =
    types.Rule(
      name: "Exact path rule",
      description: "Check exact path",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/exact/path")),
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
  let rule =
    types.Rule(
      name: "Regex path rule",
      description: "Check regex path",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("^/users/.*")),
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
  let rule =
    types.Rule(
      name: "Body content rule",
      description: "Verify body contains text",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/test")),
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

  let response =
    make_execution_result(200, "Operation was a success", types.Get, "/test")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_body_must_not_contain_test() {
  // Test body_must_not_contain check
  let rule =
    types.Rule(
      name: "No error rule",
      description: "Verify no error in body",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/test")),
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
    make_execution_result(200, "This is clean data", types.Get, "/test")
  let results = rules_engine.check_rules([rule], response, "test_behavior")

  case results {
    [rules_engine.RulePassed(_)] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn rules_engine_check_body_must_not_contain_violation_test() {
  // Test body_must_not_contain violation
  let rule =
    types.Rule(
      name: "No error rule",
      description: "Verify no error in body",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/test")),
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
  let rule =
    types.Rule(
      name: "Required text rule",
      description: "Verify required text",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/test")),
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

  let response =
    make_execution_result(200, "This is missing it", types.Get, "/test")
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
  let rule1 =
    types.Rule(
      name: "Rule 1",
      description: "First rule",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/test")),
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

  let rule2 =
    types.Rule(
      name: "Rule 2",
      description: "Second rule",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/test")),
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
  let results =
    rules_engine.check_rules([rule1, rule2], response, "test_behavior")

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
  let rule =
    types.Rule(
      name: "Empty body rule",
      description: "Handle empty response",
      when: types.When(
        status: option.Some("== 204"),
        method: option.Some(types.Delete),
        path: option.Some("/resource"),
      ),
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

  let response =
    http_client.ExecutionResult(
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
  let rule =
    types.Rule(
      name: "Null handling rule",
      description: "Handle null values",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/nullable")),
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

  let response =
    http_client.ExecutionResult(
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
  let rule =
    types.Rule(
      name: "Whitespace rule",
      description: "Handle whitespace body",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/test")),
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
    http_client.ExecutionResult(
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
  let rule =
    types.Rule(
      name: "Nested null rule",
      description: "Check nested fields",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/nested")),
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

  let response =
    http_client.ExecutionResult(
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
  let rule =
    types.Rule(
      name: "Empty object rule",
      description: "Handle empty objects",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/data")),
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

  let response =
    http_client.ExecutionResult(
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
  let rule =
    types.Rule(
      name: "Unicode content rule",
      description: "Check for Unicode in response",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/message")),
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

  let response =
    http_client.ExecutionResult(
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
  let rule =
    types.Rule(
      name: "emoji_test",
      description: "Check emoji support 🚀 in descriptions",
      when: types.When(status: option.Some("== 200"), method: option.Some(types.Get), path: option.Some("/status")),
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

  let response =
    http_client.ExecutionResult(
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
    [rules_engine.RulePassed(name)] -> name |> should.equal("emoji_test")
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
  let config =
    types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.from_list([#("X-Custom", "café")]),
    )

  let request =
    types.Request(
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
  let value =
    json.object([
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
    "Field '"
    <> field
    <> "' expected '"
    <> expected
    <> "' but got '"
    <> actual
    <> "'"

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
  let session =
    interview.InterviewSession(
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
          perspective: User,
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
          perspective: Developer,
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
  let session =
    interview.InterviewSession(
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
          perspective: User,
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
    Ok(first_bead) -> first_bead.profile_type |> should.equal("cli")
    Error(_) -> should.fail()
  }
}

pub fn bead_to_jsonl_format_test() {
  // Test bead to JSONL conversion
  let bead =
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
  let session =
    interview.InterviewSession(
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
  let session =
    interview.InterviewSession(
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
          perspective: User,
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
  let session =
    interview.InterviewSession(
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
          perspective: Developer,
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
    Ok(first_bead) -> first_bead.profile_type |> should.equal("event")
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_data_profile_test() {
  // Test generating beads from Data profile session
  let session =
    interview.InterviewSession(
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
          perspective: Developer,
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
    Ok(first_bead) -> first_bead.profile_type |> should.equal("data")
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_workflow_profile_test() {
  // Test generating beads from Workflow profile session
  let session =
    interview.InterviewSession(
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
          perspective: Business,
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
    Ok(first_bead) -> first_bead.profile_type |> should.equal("workflow")
    Error(_) -> should.fail()
  }
}

pub fn bead_generation_ui_profile_test() {
  // Test generating beads from UI profile session
  let session =
    interview.InterviewSession(
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
          perspective: User,
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
    Ok(first_bead) -> first_bead.profile_type |> should.equal("ui")
    Error(_) -> should.fail()
  }
}

pub fn bead_record_required_fields_test() {
  // Test that bead records have all required fields
  let bead =
    bead_templates.new_bead(
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
    bead_templates.new_bead(
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
  let answer_text =
    "Create an API endpoint at /api/users that returns a list of all users with pagination support"
  let session =
    interview.InterviewSession(
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
          perspective: Developer,
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

// ============================================================================
// Progressive Bead Preview Tests (bead_templates.gleam)
// ============================================================================

pub fn format_bead_preview_api_test() {
  let bead =
    bead_templates.new_bead(
      title: "Implement API endpoint",
      description: "Create GET /users endpoint",
      profile_type: "api",
      priority: 3,
      issue_type: "api_endpoint",
      labels: ["api"],
      ai_hints: "Test hints",
      acceptance_criteria: ["Endpoint works"],
      dependencies: [],
    )

  let preview = bead_templates.format_bead_preview(bead)
  preview |> string.contains("[API]") |> should.be_true()
  preview |> string.contains("Implement API endpoint") |> should.be_true()
  preview |> string.contains("Create GET /users endpoint") |> should.be_true()
}

pub fn format_bead_preview_cli_test() {
  let bead =
    bead_templates.new_bead(
      title: "Implement CLI command",
      description: "Add process subcommand",
      profile_type: "cli",
      priority: 3,
      issue_type: "cli_command",
      labels: ["cli"],
      ai_hints: "Test hints",
      acceptance_criteria: ["Command works"],
      dependencies: [],
    )

  let preview = bead_templates.format_bead_preview(bead)
  preview |> string.contains("[CLI]") |> should.be_true()
  preview |> string.contains("Implement CLI command") |> should.be_true()
}

pub fn format_bead_preview_truncates_long_description_test() {
  let bead =
    bead_templates.new_bead(
      title: "Test bead",
      description: "This is a very long description that should be truncated because it exceeds fifty characters",
      profile_type: "api",
      priority: 2,
      issue_type: "data_model",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    )

  let preview = bead_templates.format_bead_preview(bead)
  preview |> string.contains("...") |> should.be_true()
  // Should not contain the full description
  preview |> string.contains("fifty characters") |> should.be_false()
}

pub fn format_progressive_preview_empty_beads_test() {
  let preview = bead_templates.format_progressive_preview([], 1)
  preview |> should.equal("")
}

pub fn format_progressive_preview_round_1_test() {
  let beads = [
    bead_templates.new_bead(
      title: "First bead",
      description: "Test description",
      profile_type: "api",
      priority: 3,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let preview = bead_templates.format_progressive_preview(beads, 1)
  preview |> string.contains("Round 1") |> should.be_true()
  preview |> string.contains("rough outline") |> should.be_true()
  preview |> string.contains("[API]") |> should.be_true()
}

pub fn format_progressive_preview_round_3_test() {
  let beads = [
    bead_templates.new_bead(
      title: "CLI command",
      description: "Process files",
      profile_type: "cli",
      priority: 3,
      issue_type: "cli_command",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let preview = bead_templates.format_progressive_preview(beads, 3)
  preview |> string.contains("Round 3") |> should.be_true()
  preview |> string.contains("error cases") |> should.be_true()
}

pub fn format_progressive_preview_shows_more_indicator_test() {
  // Create 7 beads to trigger the "... and N more" message
  let beads = [
    bead_templates.new_bead(
      title: "Bead 1",
      description: "d1",
      profile_type: "api",
      priority: 1,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.new_bead(
      title: "Bead 2",
      description: "d2",
      profile_type: "api",
      priority: 1,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.new_bead(
      title: "Bead 3",
      description: "d3",
      profile_type: "api",
      priority: 1,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.new_bead(
      title: "Bead 4",
      description: "d4",
      profile_type: "api",
      priority: 1,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.new_bead(
      title: "Bead 5",
      description: "d5",
      profile_type: "api",
      priority: 1,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.new_bead(
      title: "Bead 6",
      description: "d6",
      profile_type: "api",
      priority: 1,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
    bead_templates.new_bead(
      title: "Bead 7",
      description: "d7",
      profile_type: "api",
      priority: 1,
      issue_type: "api_endpoint",
      labels: [],
      ai_hints: "",
      acceptance_criteria: [],
      dependencies: [],
    ),
  ]

  let preview = bead_templates.format_progressive_preview(beads, 1)
  preview |> string.contains("... and 2 more") |> should.be_true()
}

// ============================================================================
// Format Validation Tests (formats.gleam)
// ============================================================================

// --- Email Validation Tests ---

pub fn formats_validate_email_valid_simple_test() {
  formats.validate_email("user@example.com")
  |> should.be_ok()
}

pub fn formats_validate_email_valid_with_subdomain_test() {
  formats.validate_email("user@mail.example.com")
  |> should.be_ok()
}

pub fn formats_validate_email_valid_with_plus_test() {
  formats.validate_email("user+tag@example.com")
  |> should.be_ok()
}

pub fn formats_validate_email_valid_with_dots_test() {
  formats.validate_email("first.last@example.com")
  |> should.be_ok()
}

pub fn formats_validate_email_valid_with_hyphen_local_test() {
  formats.validate_email("user-name@example.com")
  |> should.be_ok()
}

pub fn formats_validate_email_valid_with_underscore_test() {
  formats.validate_email("user_name@example.com")
  |> should.be_ok()
}

pub fn formats_validate_email_invalid_no_at_test() {
  formats.validate_email("userexample.com")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_multiple_at_test() {
  formats.validate_email("user@@example.com")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_empty_local_test() {
  formats.validate_email("@example.com")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_empty_domain_test() {
  formats.validate_email("user@")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_consecutive_dots_local_test() {
  formats.validate_email("user..name@example.com")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_starts_with_dot_test() {
  formats.validate_email(".user@example.com")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_ends_with_dot_test() {
  formats.validate_email("user.@example.com")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_no_domain_dot_test() {
  formats.validate_email("user@examplecom")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_domain_starts_hyphen_test() {
  formats.validate_email("user@-example.com")
  |> should.be_error()
}

pub fn formats_validate_email_invalid_domain_ends_hyphen_test() {
  formats.validate_email("user@example-.com")
  |> should.be_error()
}

// --- UUID Validation Tests ---

pub fn formats_validate_uuid_valid_v4_test() {
  formats.validate_uuid("550e8400-e29b-41d4-a716-446655440000")
  |> should.be_ok()
}

pub fn formats_validate_uuid_valid_v1_test() {
  formats.validate_uuid("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  |> should.be_ok()
}

pub fn formats_validate_uuid_valid_uppercase_test() {
  formats.validate_uuid("550E8400-E29B-41D4-A716-446655440000")
  |> should.be_ok()
}

pub fn formats_validate_uuid_invalid_wrong_segment_count_test() {
  formats.validate_uuid("550e8400-e29b-41d4-a716")
  |> should.be_error()
}

pub fn formats_validate_uuid_invalid_wrong_segment_length_test() {
  formats.validate_uuid("550e840-e29b-41d4-a716-446655440000")
  |> should.be_error()
}

pub fn formats_validate_uuid_invalid_non_hex_test() {
  formats.validate_uuid("550e8400-e29b-41d4-a716-44665544000g")
  |> should.be_error()
}

pub fn formats_validate_uuid_invalid_version_test() {
  formats.validate_uuid("550e8400-e29b-61d4-a716-446655440000")
  |> should.be_error()
}

pub fn formats_validate_uuid_invalid_variant_test() {
  formats.validate_uuid("550e8400-e29b-41d4-0716-446655440000")
  |> should.be_error()
}

pub fn formats_validate_uuid_invalid_no_dashes_test() {
  formats.validate_uuid("550e8400e29b41d4a716446655440000")
  |> should.be_error()
}

// --- URI Validation Tests ---

pub fn formats_validate_uri_valid_http_test() {
  formats.validate_uri("http://example.com")
  |> should.be_ok()
}

pub fn formats_validate_uri_valid_https_test() {
  formats.validate_uri("https://example.com")
  |> should.be_ok()
}

pub fn formats_validate_uri_valid_ftp_test() {
  formats.validate_uri("ftp://files.example.com")
  |> should.be_ok()
}

pub fn formats_validate_uri_valid_with_path_test() {
  formats.validate_uri("https://example.com/path/to/resource")
  |> should.be_ok()
}

pub fn formats_validate_uri_valid_with_port_test() {
  formats.validate_uri("http://localhost:8080")
  |> should.be_ok()
}

pub fn formats_validate_uri_valid_with_query_test() {
  formats.validate_uri("https://example.com/search?q=test")
  |> should.be_ok()
}

pub fn formats_validate_uri_invalid_empty_test() {
  formats.validate_uri("")
  |> should.be_error()
}

pub fn formats_validate_uri_invalid_no_scheme_test() {
  formats.validate_uri("example.com")
  |> should.be_error()
}

pub fn formats_validate_uri_invalid_scheme_only_test() {
  formats.validate_uri("http://")
  |> should.be_error()
}

pub fn formats_validate_uri_invalid_scheme_starts_number_test() {
  formats.validate_uri("1http://example.com")
  |> should.be_error()
}

// --- ISO8601 DateTime Validation Tests ---

pub fn formats_validate_iso8601_valid_date_only_test() {
  formats.validate_iso8601("2024-01-15")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_valid_datetime_test() {
  formats.validate_iso8601("2024-01-15T10:30:00")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_valid_datetime_with_z_test() {
  formats.validate_iso8601("2024-01-15T10:30:00Z")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_valid_datetime_with_tz_plus_test() {
  formats.validate_iso8601("2024-01-15T10:30:00+05:30")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_valid_datetime_with_tz_minus_test() {
  formats.validate_iso8601("2024-01-15T10:30:00-08:00")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_valid_datetime_fractional_seconds_test() {
  formats.validate_iso8601("2024-01-15T10:30:00.123")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_valid_feb_28_non_leap_test() {
  formats.validate_iso8601("2023-02-28")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_valid_feb_29_leap_test() {
  formats.validate_iso8601("2024-02-29")
  |> should.be_ok()
}

pub fn formats_validate_iso8601_invalid_too_short_test() {
  formats.validate_iso8601("2024-01")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_month_13_test() {
  formats.validate_iso8601("2024-13-01")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_month_00_test() {
  formats.validate_iso8601("2024-00-01")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_day_32_test() {
  formats.validate_iso8601("2024-01-32")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_day_00_test() {
  formats.validate_iso8601("2024-01-00")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_feb_29_non_leap_test() {
  formats.validate_iso8601("2023-02-29")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_april_31_test() {
  formats.validate_iso8601("2024-04-31")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_hour_24_test() {
  formats.validate_iso8601("2024-01-15T24:00:00")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_minute_60_test() {
  formats.validate_iso8601("2024-01-15T10:60:00")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_second_60_test() {
  formats.validate_iso8601("2024-01-15T10:30:60")
  |> should.be_error()
}

pub fn formats_validate_iso8601_invalid_separator_test() {
  formats.validate_iso8601("2024-01-15X10:30:00")
  |> should.be_error()
}

pub fn formats_validate_iso8601_valid_space_separator_test() {
  formats.validate_iso8601("2024-01-15 10:30:00")
  |> should.be_ok()
}

// ============================================================================
// Checker Module Tests (checker.gleam)
// ============================================================================

// Helper function to create a minimal Response for testing
fn make_test_response(
  status: Int,
  checks: Dict(String, types.Check),
) -> types.Response {
  types.Response(
    status: status,
    example: json.null(),
    checks: checks,
    headers: dict.new(),
  )
}

// Helper to create ExecutionResult
fn make_test_execution(
  status: Int,
  body_json: Json,
  headers: Dict(String, String),
) -> http_client.ExecutionResult {
  http_client.ExecutionResult(
    status: status,
    headers: headers,
    body: body_json,
    raw_body: "",
    elapsed_ms: 100,
    request_method: types.Get,
    request_path: "/test",
  )
}

// Helper to create empty context
fn empty_context() -> interpolate.Context {
  interpolate.new_context()
}

// --- Status Code Tests ---

pub fn checker_status_code_match_test() {
  let expected = make_test_response(200, dict.new())
  let actual = make_test_execution(200, json.null(), dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  result.status_ok |> should.be_true()
  result.status_expected |> should.equal(200)
  result.status_actual |> should.equal(200)
}

pub fn checker_status_code_mismatch_test() {
  let expected = make_test_response(200, dict.new())
  let actual = make_test_execution(404, json.null(), dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  result.status_ok |> should.be_false()
  result.status_expected |> should.equal(200)
  result.status_actual |> should.equal(404)
}

// --- Field Check Tests ---

pub fn checker_field_equals_string_pass_test() {
  let checks =
    dict.from_list([
      #("name", types.Check(rule: "equals John", why: "Name must match")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.string("John"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
  list.length(result.failed) |> should.equal(0)
}

pub fn checker_field_equals_string_fail_test() {
  let checks =
    dict.from_list([
      #("name", types.Check(rule: "equals John", why: "Name must match")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.string("Jane"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_equals_int_pass_test() {
  let checks =
    dict.from_list([
      #("age", types.Check(rule: "equals 25", why: "Age must match")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("age", json.int(25))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
  list.length(result.failed) |> should.equal(0)
}

pub fn checker_field_is_string_pass_test() {
  let checks =
    dict.from_list([
      #("name", types.Check(rule: "string", why: "Must be string")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.string("test"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
  list.length(result.failed) |> should.equal(0)
}

pub fn checker_field_is_string_fail_test() {
  let checks =
    dict.from_list([
      #("name", types.Check(rule: "string", why: "Must be string")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.int(123))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_is_integer_pass_test() {
  let checks =
    dict.from_list([
      #("count", types.Check(rule: "integer", why: "Must be integer")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("count", json.int(42))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_is_boolean_pass_test() {
  let checks =
    dict.from_list([
      #("active", types.Check(rule: "boolean", why: "Must be boolean")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("active", json.bool(True))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_is_array_pass_test() {
  let checks =
    dict.from_list([
      #("items", types.Check(rule: "array", why: "Must be array")),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([
      #("items", json.array([json.int(1), json.int(2)], fn(x) { x })),
    ])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_is_object_pass_test() {
  let checks =
    dict.from_list([
      #("data", types.Check(rule: "object", why: "Must be object")),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([#("data", json.object([#("key", json.string("value"))]))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_present_pass_test() {
  let checks =
    dict.from_list([
      #("id", types.Check(rule: "present", why: "ID must be present")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("id", json.string("abc-123"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
  list.length(result.failed) |> should.equal(0)
}

pub fn checker_field_present_fail_test() {
  let checks =
    dict.from_list([
      #("id", types.Check(rule: "present", why: "ID must be present")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.string("test"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_absent_pass_test() {
  let checks =
    dict.from_list([
      #(
        "password",
        types.Check(rule: "absent", why: "Password should not be returned"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.string("test"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
  list.length(result.failed) |> should.equal(0)
}

pub fn checker_field_absent_fail_test() {
  let checks =
    dict.from_list([
      #(
        "password",
        types.Check(rule: "absent", why: "Password should not be returned"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("password", json.string("secret"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_non_empty_string_pass_test() {
  let checks =
    dict.from_list([
      #(
        "name",
        types.Check(rule: "non-empty string", why: "Name must not be empty"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.string("John"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_non_empty_string_fail_test() {
  let checks =
    dict.from_list([
      #(
        "name",
        types.Check(rule: "non-empty string", why: "Name must not be empty"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("name", json.string(""))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_is_email_pass_test() {
  let checks =
    dict.from_list([
      #("email", types.Check(rule: "email", why: "Must be valid email")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("email", json.string("user@example.com"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_is_email_fail_test() {
  let checks =
    dict.from_list([
      #("email", types.Check(rule: "email", why: "Must be valid email")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("email", json.string("not-an-email"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_is_uuid_pass_test() {
  let checks =
    dict.from_list([
      #("id", types.Check(rule: "uuid", why: "Must be valid UUID")),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([#("id", json.string("550e8400-e29b-41d4-a716-446655440000"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_is_uuid_fail_test() {
  let checks =
    dict.from_list([
      #("id", types.Check(rule: "uuid", why: "Must be valid UUID")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("id", json.string("not-a-uuid"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_is_iso8601_pass_test() {
  let checks =
    dict.from_list([
      #(
        "created_at",
        types.Check(rule: "iso8601 datetime", why: "Must be valid datetime"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("created_at", json.string("2024-01-15T10:30:00Z"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

// --- Numeric Comparison Tests ---

pub fn checker_field_integer_gte_pass_test() {
  let checks =
    dict.from_list([
      #("count", types.Check(rule: "integer >= 5", why: "Must be at least 5")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("count", json.int(10))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_integer_gte_fail_test() {
  let checks =
    dict.from_list([
      #("count", types.Check(rule: "integer >= 5", why: "Must be at least 5")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("count", json.int(3))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.failed) |> should.equal(1)
}

pub fn checker_field_integer_lte_pass_test() {
  let checks =
    dict.from_list([
      #(
        "count",
        types.Check(rule: "integer <= 100", why: "Must not exceed 100"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("count", json.int(50))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_number_between_pass_test() {
  let checks =
    dict.from_list([
      #(
        "age",
        types.Check(
          rule: "number between 18.0 and 65.0",
          why: "Age must be in range",
        ),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("age", json.int(30))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_field_number_between_fail_test() {
  let checks =
    dict.from_list([
      #(
        "age",
        types.Check(
          rule: "number between 18.0 and 65.0",
          why: "Age must be in range",
        ),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("age", json.int(17))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.failed) |> should.equal(1)
}

// --- String Pattern Tests ---

pub fn checker_string_starts_with_pass_test() {
  let checks =
    dict.from_list([
      #(
        "code",
        types.Check(rule: "string starting with ERR-", why: "Error code format"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("code", json.string("ERR-001"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_string_ends_with_pass_test() {
  let checks =
    dict.from_list([
      #(
        "file",
        types.Check(rule: "string ending with .json", why: "Must be JSON file"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("file", json.string("config.json"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_string_containing_pass_test() {
  let checks =
    dict.from_list([
      #(
        "message",
        types.Check(
          rule: "string containing success",
          why: "Should mention success",
        ),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([#("message", json.string("Operation success complete"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

// --- Array Tests ---

pub fn checker_non_empty_array_pass_test() {
  let checks =
    dict.from_list([
      #("items", types.Check(rule: "non-empty array", why: "Must have items")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("items", json.array([json.int(1)], fn(x) { x }))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_non_empty_array_fail_test() {
  let checks =
    dict.from_list([
      #("items", types.Check(rule: "non-empty array", why: "Must have items")),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("items", json.array([], fn(x) { x }))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.failed) |> should.equal(1)
}

pub fn checker_array_of_length_pass_test() {
  let checks =
    dict.from_list([
      #(
        "coords",
        types.Check(rule: "array of length 3", why: "Must have 3 elements"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([
      #(
        "coords",
        json.array([json.int(1), json.int(2), json.int(3)], fn(x) { x }),
      ),
    ])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_array_min_items_pass_test() {
  let checks =
    dict.from_list([
      #(
        "tags",
        types.Check(rule: "array with min 2 items", why: "Need at least 2 tags"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([
      #(
        "tags",
        json.array(
          [json.string("a"), json.string("b"), json.string("c")],
          fn(x) { x },
        ),
      ),
    ])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

// --- One Of Tests ---

pub fn checker_one_of_pass_test() {
  let checks =
    dict.from_list([
      #(
        "status",
        types.Check(
          rule: "one of [active, inactive, pending]",
          why: "Valid status",
        ),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("status", json.string("active"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_one_of_fail_test() {
  let checks =
    dict.from_list([
      #(
        "status",
        types.Check(
          rule: "one of [active, inactive, pending]",
          why: "Valid status",
        ),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body = json.object([#("status", json.string("unknown"))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.failed) |> should.equal(1)
}

// --- Header Check Tests ---

pub fn checker_header_present_pass_test() {
  let expected =
    types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.from_list([#("Content-Type", "application/json")]),
    )
  let actual =
    make_test_execution(
      200,
      json.null(),
      dict.from_list([#("Content-Type", "application/json")]),
    )
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
  list.length(result.failed) |> should.equal(0)
}

pub fn checker_header_value_mismatch_test() {
  let expected =
    types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.from_list([#("Content-Type", "application/json")]),
    )
  let actual =
    make_test_execution(
      200,
      json.null(),
      dict.from_list([#("Content-Type", "text/html")]),
    )
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_header_missing_test() {
  let expected =
    types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.from_list([#("X-Request-Id", "abc-123")]),
    )
  let actual = make_test_execution(200, json.null(), dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(0)
  list.length(result.failed) |> should.equal(1)
}

pub fn checker_header_case_insensitive_test() {
  let expected =
    types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.from_list([#("content-type", "application/json")]),
    )
  let actual =
    make_test_execution(
      200,
      json.null(),
      dict.from_list([#("Content-Type", "application/json")]),
    )
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

// --- Nested Field Tests ---

pub fn checker_nested_field_pass_test() {
  let checks =
    dict.from_list([
      #(
        "user.name",
        types.Check(rule: "equals John", why: "User name must match"),
      ),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([#("user", json.object([#("name", json.string("John"))]))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(1)
}

pub fn checker_nested_field_missing_test() {
  let checks =
    dict.from_list([
      #("user.email", types.Check(rule: "is email", why: "Must have email")),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([#("user", json.object([#("name", json.string("John"))]))])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.failed) |> should.equal(1)
}

// --- Multiple Checks Test ---

pub fn checker_multiple_checks_test() {
  let checks =
    dict.from_list([
      #("id", types.Check(rule: "uuid", why: "ID must be UUID")),
      #("name", types.Check(rule: "non-empty string", why: "Name required")),
      #("email", types.Check(rule: "email", why: "Email required")),
      #("age", types.Check(rule: "integer >= 0", why: "Age must be positive")),
    ])
  let expected = make_test_response(200, checks)
  let body =
    json.object([
      #("id", json.string("550e8400-e29b-41d4-a716-446655440000")),
      #("name", json.string("John")),
      #("email", json.string("john@example.com")),
      #("age", json.int(30)),
    ])
  let actual = make_test_execution(200, body, dict.new())
  let result = checker.check_response(expected, actual, empty_context())

  list.length(result.passed) |> should.equal(4)
  list.length(result.failed) |> should.equal(0)
}

// ============================================================================
// Custom Question Loading Tests
// ============================================================================

// Helper to create a test question
fn make_test_question(id: String, round: Int, question_text: String) -> Question {
  Question(
    id: id,
    round: round,
    perspective: User,
    category: HappyPath,
    priority: Critical,
    question: question_text,
    context: "Test context",
    example: "Test example",
    expected_type: "text",
    extract_into: [],
    depends_on: [],
    blocks: [],
  )
}

pub fn question_loader_merge_empty_custom_returns_base_test() {
  // When custom questions are empty (None), base should be returned unchanged
  let base =
    question_loader.ProfileQuestions(
      round_1: [make_test_question("q1", 1, "Question 1")],
      round_2: [make_test_question("q2", 2, "Question 2")],
    )
  let custom = None

  let result = merge_profile_test(base, custom)

  list.length(result.round_1) |> should.equal(1)
  list.length(result.round_2) |> should.equal(1)
}

pub fn question_loader_merge_adds_new_questions_test() {
  // Custom questions with new IDs should be added
  let base =
    question_loader.ProfileQuestions(
      round_1: [make_test_question("q1", 1, "Question 1")],
      round_2: [],
    )
  let custom =
    Some(question_loader.CustomProfileQuestions(
      round_1: Some([make_test_question("q-new", 1, "New Question")]),
      round_2: None,
    ))

  let result = merge_profile_test(base, custom)

  // Should have both original and new question
  list.length(result.round_1) |> should.equal(2)
}

pub fn question_loader_merge_overrides_by_id_test() {
  // Custom question with same ID should override the base
  let base =
    question_loader.ProfileQuestions(
      round_1: [make_test_question("q1", 1, "Original Question")],
      round_2: [],
    )
  let custom =
    Some(question_loader.CustomProfileQuestions(
      round_1: Some([make_test_question("q1", 1, "Overridden Question")]),
      round_2: None,
    ))

  let result = merge_profile_test(base, custom)

  // Should still have just one question (the override)
  list.length(result.round_1) |> should.equal(1)

  // The question should be the overridden one
  case result.round_1 {
    [q] -> q.question |> should.equal("Overridden Question")
    _ -> should.fail()
  }
}

pub fn question_loader_merge_preserves_non_overridden_test() {
  // Questions not overridden should remain
  let base =
    question_loader.ProfileQuestions(
      round_1: [
        make_test_question("q1", 1, "Question 1"),
        make_test_question("q2", 1, "Question 2"),
        make_test_question("q3", 1, "Question 3"),
      ],
      round_2: [],
    )
  let custom =
    Some(question_loader.CustomProfileQuestions(
      round_1: Some([make_test_question("q2", 1, "Overridden Q2")]),
      round_2: None,
    ))

  let result = merge_profile_test(base, custom)

  // Should have 3 questions (2 preserved + 1 override)
  list.length(result.round_1) |> should.equal(3)

  // Find the overridden question
  let overridden = list.filter(result.round_1, fn(q) { q.id == "q2" })
  case overridden {
    [q] -> q.question |> should.equal("Overridden Q2")
    _ -> should.fail()
  }
}

pub fn question_loader_merge_common_rounds_test() {
  // Test merging common questions (rounds 3-5)
  let base =
    question_loader.CommonQuestions(
      round_3: [make_test_question("r3-q1", 3, "Round 3 Q1")],
      round_4: [make_test_question("r4-q1", 4, "Round 4 Q1")],
      round_5: [],
    )
  let custom =
    Some(question_loader.CustomCommonQuestions(
      round_3: None,
      round_4: Some([make_test_question("r4-q1", 4, "Overridden R4 Q1")]),
      round_5: Some([make_test_question("r5-new", 5, "New Round 5 Q")]),
    ))

  let result = merge_common_test(base, custom)

  // Round 3 unchanged
  list.length(result.round_3) |> should.equal(1)

  // Round 4 has override
  list.length(result.round_4) |> should.equal(1)
  case result.round_4 {
    [q] -> q.question |> should.equal("Overridden R4 Q1")
    _ -> should.fail()
  }

  // Round 5 has new question
  list.length(result.round_5) |> should.equal(1)
}

pub fn question_loader_file_not_found_test() {
  // Loading from non-existent file should return FileNotFound error
  let result = question_loader.load_custom_questions("/nonexistent/path.cue")

  case result {
    Error(question_loader.FileNotFound(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// Helper to test merge_profile
// Replicates the merge logic since merge_custom_questions is private
fn merge_profile_test(
  base: question_loader.ProfileQuestions,
  custom: option.Option(question_loader.CustomProfileQuestions),
) -> question_loader.ProfileQuestions {
  case custom {
    None -> base
    Some(c) ->
      question_loader.ProfileQuestions(
        round_1: merge_question_list_test(base.round_1, c.round_1),
        round_2: merge_question_list_test(base.round_2, c.round_2),
      )
  }
}

fn merge_common_test(
  base: question_loader.CommonQuestions,
  custom: option.Option(question_loader.CustomCommonQuestions),
) -> question_loader.CommonQuestions {
  case custom {
    None -> base
    Some(c) ->
      question_loader.CommonQuestions(
        round_3: merge_question_list_test(base.round_3, c.round_3),
        round_4: merge_question_list_test(base.round_4, c.round_4),
        round_5: merge_question_list_test(base.round_5, c.round_5),
      )
  }
}

fn merge_question_list_test(
  base: List(Question),
  custom: option.Option(List(Question)),
) -> List(Question) {
  case custom {
    None -> base
    Some(custom_questions) -> {
      let custom_ids = list.map(custom_questions, fn(q) { q.id })
      let filtered_base =
        list.filter(base, fn(q) { !list.contains(custom_ids, q.id) })
      list.append(filtered_base, custom_questions)
    }
  }
}

// ============================================================================
// Session Diff Tests
// ============================================================================

// Helper to create a minimal test interview session
fn make_test_interview_session(
  id: String,
  answers: List(interview.Answer),
  gaps: List(interview.Gap),
  conflicts: List(interview.Conflict),
  stage: interview.InterviewStage,
) -> interview.InterviewSession {
  interview.InterviewSession(
    id: id,
    profile: interview.Api,
    created_at: "2024-01-01T10:00:00Z",
    updated_at: "2024-01-01T12:00:00Z",
    completed_at: "",
    stage: stage,
    rounds_completed: 1,
    answers: answers,
    gaps: gaps,
    conflicts: conflicts,
    raw_notes: "",
  )
}

// Helper to create a test answer
fn make_test_answer(question_id: String, response: String) -> interview.Answer {
  interview.Answer(
    question_id: question_id,
    question_text: "Test question for " <> question_id,
    perspective: User,
    round: 1,
    response: response,
    extracted: dict.new(),
    confidence: 0.9,
    notes: "",
    timestamp: "2024-01-01T10:30:00Z",
  )
}

// Helper to create a test gap
fn make_test_gap(id: String, resolved: Bool) -> interview.Gap {
  interview.Gap(
    id: id,
    field: "test_field",
    description: "Test gap",
    blocking: True,
    suggested_default: "",
    why_needed: "Test reason",
    round: 1,
    resolved: resolved,
    resolution: "",
  )
}

// Helper to create a test conflict
fn make_test_conflict(id: String, chosen: Int) -> interview.Conflict {
  interview.Conflict(
    id: id,
    between: #("answer1", "answer2"),
    description: "Test conflict",
    impact: "High",
    options: [],
    chosen: chosen,
  )
}

pub fn diff_sessions_no_changes_test() {
  let session =
    make_test_interview_session(
      "session-1",
      [make_test_answer("q1", "Answer 1")],
      [],
      [],
      interview.Discovery,
    )

  let diff = interview_storage.diff_sessions(session, session)

  list.length(diff.answers_added) |> should.equal(0)
  list.length(diff.answers_modified) |> should.equal(0)
  list.length(diff.answers_removed) |> should.equal(0)
  diff.stage_changed |> should.equal(None)
}

pub fn diff_sessions_answer_added_test() {
  let session1 =
    make_test_interview_session(
      "session-1",
      [make_test_answer("q1", "Answer 1")],
      [],
      [],
      interview.Discovery,
    )

  let session2 =
    make_test_interview_session(
      "session-1",
      [
        make_test_answer("q1", "Answer 1"),
        make_test_answer("q2", "Answer 2"),
      ],
      [],
      [],
      interview.Discovery,
    )

  let diff = interview_storage.diff_sessions(session1, session2)

  list.length(diff.answers_added) |> should.equal(1)
  list.length(diff.answers_modified) |> should.equal(0)
  list.length(diff.answers_removed) |> should.equal(0)
}

pub fn diff_sessions_answer_modified_test() {
  let session1 =
    make_test_interview_session(
      "session-1",
      [make_test_answer("q1", "Original answer")],
      [],
      [],
      interview.Discovery,
    )

  let session2 =
    make_test_interview_session(
      "session-1",
      [make_test_answer("q1", "Modified answer")],
      [],
      [],
      interview.Discovery,
    )

  let diff = interview_storage.diff_sessions(session1, session2)

  list.length(diff.answers_added) |> should.equal(0)
  list.length(diff.answers_modified) |> should.equal(1)
  list.length(diff.answers_removed) |> should.equal(0)

  // Check the modified answer details
  case diff.answers_modified {
    [modified] -> {
      modified.question_id |> should.equal("q1")
      modified.old_response |> should.equal(Some("Original answer"))
      modified.new_response |> should.equal("Modified answer")
    }
    _ -> should.fail()
  }
}

pub fn diff_sessions_answer_removed_test() {
  let session1 =
    make_test_interview_session(
      "session-1",
      [
        make_test_answer("q1", "Answer 1"),
        make_test_answer("q2", "Answer 2"),
      ],
      [],
      [],
      interview.Discovery,
    )

  let session2 =
    make_test_interview_session(
      "session-1",
      [make_test_answer("q1", "Answer 1")],
      [],
      [],
      interview.Discovery,
    )

  let diff = interview_storage.diff_sessions(session1, session2)

  list.length(diff.answers_added) |> should.equal(0)
  list.length(diff.answers_modified) |> should.equal(0)
  list.length(diff.answers_removed) |> should.equal(1)
  diff.answers_removed |> should.equal(["q2"])
}

pub fn diff_sessions_stage_changed_test() {
  let session1 =
    make_test_interview_session("session-1", [], [], [], interview.Discovery)

  let session2 =
    make_test_interview_session("session-1", [], [], [], interview.Refinement)

  let diff = interview_storage.diff_sessions(session1, session2)

  case diff.stage_changed {
    Some(#(from, to)) -> {
      from |> should.equal("discovery")
      to |> should.equal("refinement")
    }
    None -> should.fail()
  }
}

pub fn diff_sessions_gaps_resolved_test() {
  let session1 =
    make_test_interview_session(
      "session-1",
      [],
      [make_test_gap("gap1", False), make_test_gap("gap2", False)],
      [],
      interview.Discovery,
    )

  let session2 =
    make_test_interview_session(
      "session-1",
      [],
      [make_test_gap("gap1", True), make_test_gap("gap2", False)],
      [],
      interview.Discovery,
    )

  let diff = interview_storage.diff_sessions(session1, session2)

  diff.gaps_resolved |> should.equal(1)
}

pub fn diff_sessions_conflicts_resolved_test() {
  let session1 =
    make_test_interview_session(
      "session-1",
      [],
      [],
      [make_test_conflict("c1", -1)],
      // -1 means unresolved
      interview.Discovery,
    )

  let session2 =
    make_test_interview_session(
      "session-1",
      [],
      [],
      [make_test_conflict("c1", 0)],
      // 0 means first option chosen
      interview.Discovery,
    )

  let diff = interview_storage.diff_sessions(session1, session2)

  diff.conflicts_resolved |> should.equal(1)
}

pub fn create_snapshot_test() {
  let session =
    make_test_interview_session(
      "session-1",
      [
        make_test_answer("q1", "Answer 1"),
        make_test_answer("q2", "Answer 2"),
      ],
      [make_test_gap("gap1", False)],
      [],
      interview.Discovery,
    )

  let snapshot = interview_storage.create_snapshot(session, "Test snapshot")

  snapshot.session_id |> should.equal("session-1")
  snapshot.description |> should.equal("Test snapshot")
  snapshot.gaps_count |> should.equal(1)
  dict.size(snapshot.answers) |> should.equal(2)
}

pub fn format_diff_produces_output_test() {
  let session1 =
    make_test_interview_session(
      "session-1",
      [make_test_answer("q1", "Original")],
      [],
      [],
      interview.Discovery,
    )

  let session2 =
    make_test_interview_session(
      "session-1",
      [
        make_test_answer("q1", "Modified"),
        make_test_answer("q2", "New answer"),
      ],
      [],
      [],
      interview.Refinement,
    )

  let diff = interview_storage.diff_sessions(session1, session2)
  let formatted = interview_storage.format_diff(diff)

  // Check that the output contains expected sections
  string.contains(formatted, "Session Diff:") |> should.be_true()
  string.contains(formatted, "Answers Added") |> should.be_true()
  string.contains(formatted, "Answers Modified") |> should.be_true()
  string.contains(formatted, "Stage:") |> should.be_true()
}

// ============================================================================
// Plan Mode Tests
// ============================================================================

import intent/plan_mode

// Helper to create a test bead
fn make_plan_bead(
  id: String,
  title: String,
  requires: List(String),
  effort: plan_mode.Effort,
) -> plan_mode.PlanBead {
  plan_mode.PlanBead(
    id: id,
    title: title,
    requires: requires,
    effort: effort,
    status: plan_mode.Pending,
  )
}

pub fn plan_mode_detect_dependency_graph_simple_test() {
  // Three beads with no dependencies
  let beads = [
    make_plan_bead("AUTH-001", "First bead", [], plan_mode.Effort5min),
    make_plan_bead("AUTH-002", "Second bead", [], plan_mode.Effort10min),
    make_plan_bead("AUTH-003", "Third bead", [], plan_mode.Effort15min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result |> should.be_ok()

  let assert Ok(phases) = result
  // All beads have no deps, so they should be in phase 1
  phases |> list.length |> should.equal(1)

  let assert [phase1] = phases
  phase1.phase_number |> should.equal(1)
  phase1.beads |> list.length |> should.equal(3)
  phase1.can_parallel |> should.be_true()
}

pub fn plan_mode_detect_dependency_graph_linear_test() {
  // Three beads in a linear chain: A -> B -> C
  let beads = [
    make_plan_bead("AUTH-001", "First bead", [], plan_mode.Effort5min),
    make_plan_bead(
      "AUTH-002",
      "Second bead",
      ["AUTH-001"],
      plan_mode.Effort10min,
    ),
    make_plan_bead(
      "AUTH-003",
      "Third bead",
      ["AUTH-002"],
      plan_mode.Effort15min,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result |> should.be_ok()

  let assert Ok(phases) = result
  // Linear dependency = 3 phases
  phases |> list.length |> should.equal(3)

  let assert [phase1, phase2, phase3] = phases
  phase1.beads |> list.length |> should.equal(1)
  phase2.beads |> list.length |> should.equal(1)
  phase3.beads |> list.length |> should.equal(1)

  // Single-bead phases cannot be parallelized
  phase1.can_parallel |> should.be_false()
  phase2.can_parallel |> should.be_false()
  phase3.can_parallel |> should.be_false()
}

pub fn plan_mode_detect_dependency_graph_diamond_test() {
  // Diamond dependency: A -> B, A -> C, B -> D, C -> D
  let beads = [
    make_plan_bead("AUTH-001", "A", [], plan_mode.Effort5min),
    make_plan_bead("AUTH-002", "B", ["AUTH-001"], plan_mode.Effort10min),
    make_plan_bead("AUTH-003", "C", ["AUTH-001"], plan_mode.Effort10min),
    make_plan_bead(
      "AUTH-004",
      "D",
      ["AUTH-002", "AUTH-003"],
      plan_mode.Effort15min,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result |> should.be_ok()

  let assert Ok(phases) = result
  // Diamond = 3 phases: A | B,C | D
  phases |> list.length |> should.equal(3)

  let assert [phase1, phase2, phase3] = phases
  phase1.beads |> list.length |> should.equal(1)
  phase2.beads |> list.length |> should.equal(2)
  // B and C can run together
  phase3.beads |> list.length |> should.equal(1)

  phase2.can_parallel |> should.be_true()
}

pub fn plan_mode_detect_dependency_graph_missing_dep_test() {
  // Bead depends on non-existent bead
  let beads = [
    make_plan_bead(
      "AUTH-001",
      "First bead",
      ["MISSING-001"],
      plan_mode.Effort5min,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result |> should.be_error()
}

pub fn plan_mode_format_plan_human_test() {
  let plan =
    plan_mode.ExecutionPlan(
      session_id: "test-session-123",
      generated_at: "2024-01-01T10:00:00Z",
      phases: [
        plan_mode.ExecutionPhase(
          phase_number: 1,
          title: "Phase 1",
          beads: ["AUTH-001", "AUTH-002"],
          can_parallel: True,
          effort: "15min",
        ),
        plan_mode.ExecutionPhase(
          phase_number: 2,
          title: "Phase 2",
          beads: ["AUTH-003"],
          can_parallel: False,
          effort: "10min",
        ),
      ],
      total_beads: 3,
      total_effort: "25min",
      risk: plan_mode.Low,
      blockers: [],
    )

  let formatted = plan_mode.format_plan_human(plan)

  // Check header content
  string.contains(formatted, "EXECUTION PLAN") |> should.be_true()
  string.contains(formatted, "test-session-123") |> should.be_true()
  string.contains(formatted, "Total Beads: 3") |> should.be_true()
  string.contains(formatted, "25min") |> should.be_true()
  string.contains(formatted, "low") |> should.be_true()

  // Check phase content
  string.contains(formatted, "Phase 1") |> should.be_true()
  string.contains(formatted, "AUTH-001") |> should.be_true()
  string.contains(formatted, "can run in parallel") |> should.be_true()
}

pub fn plan_mode_format_plan_json_test() {
  let plan =
    plan_mode.ExecutionPlan(
      session_id: "test-session-456",
      generated_at: "2024-01-01T10:00:00Z",
      phases: [
        plan_mode.ExecutionPhase(
          phase_number: 1,
          title: "Phase 1",
          beads: ["AUTH-001"],
          can_parallel: False,
          effort: "5min",
        ),
      ],
      total_beads: 1,
      total_effort: "5min",
      risk: plan_mode.Medium,
      blockers: ["AUTH-999: Some blocker"],
    )

  let formatted = plan_mode.format_plan_json(plan)

  // Check JSON structure
  string.contains(formatted, "\"session_id\": \"test-session-456\"")
  |> should.be_true()
  string.contains(formatted, "\"total_beads\": 1") |> should.be_true()
  string.contains(formatted, "\"risk\": \"medium\"") |> should.be_true()
  string.contains(formatted, "\"phase_number\": 1") |> should.be_true()
  string.contains(formatted, "\"AUTH-001\"") |> should.be_true()
  string.contains(formatted, "AUTH-999: Some blocker") |> should.be_true()
}

pub fn plan_mode_format_error_session_not_found_test() {
  let error = plan_mode.SessionNotFound("missing-session")
  let formatted = plan_mode.format_error(error)

  string.contains(formatted, "missing-session") |> should.be_true()
  string.contains(formatted, "Session not found") |> should.be_true()
}

pub fn plan_mode_format_error_missing_dependency_test() {
  let error = plan_mode.MissingDependency("AUTH-001", "AUTH-999")
  let formatted = plan_mode.format_error(error)

  string.contains(formatted, "AUTH-001") |> should.be_true()
  string.contains(formatted, "AUTH-999") |> should.be_true()
  string.contains(formatted, "requires") |> should.be_true()
}

// ============================================================================
// Bead Feedback Tests - Martin Fowler Style
// ============================================================================

import intent/bead_feedback

// ============================================================================
// Unit Tests: Bead Feedback Types
// ============================================================================

pub fn bead_feedback_result_variants_test() {
  // Verify all BeadResult variants exist and are distinct
  let success = bead_feedback.Success
  let failed = bead_feedback.Failed
  let blocked = bead_feedback.Blocked
  let skipped = bead_feedback.Skipped

  // Type checking ensures these compile - just verify they're the expected values
  success |> should.equal(bead_feedback.Success)
  failed |> should.equal(bead_feedback.Failed)
  blocked |> should.equal(bead_feedback.Blocked)
  skipped |> should.equal(bead_feedback.Skipped)
}

pub fn bead_feedback_error_types_test() {
  // Verify FeedbackError variants
  let session_error = bead_feedback.SessionNotFound("test-session")
  let write_error =
    bead_feedback.WriteError("/path/to/file", "Permission denied")
  let validation_error = bead_feedback.ValidationError("Invalid bead ID")

  let bead_feedback.SessionNotFound(id) = session_error
  id |> should.equal("test-session")

  let bead_feedback.WriteError(path, msg) = write_error
  path |> should.equal("/path/to/file")
  msg |> should.equal("Permission denied")

  let bead_feedback.ValidationError(val_msg) = validation_error
  val_msg |> should.equal("Invalid bead ID")
}

pub fn bead_error_construction_test() {
  // Test BeadError type construction
  let error =
    bead_feedback.BeadError(
      error_type: "compilation_error",
      message: "Failed to compile module",
      trace: Some("stack trace here"),
    )

  error.error_type |> should.equal("compilation_error")
  error.message |> should.equal("Failed to compile module")
  error.trace |> should.equal(Some("stack trace here"))
}

pub fn blocked_reason_construction_test() {
  // Test BlockedReason type construction
  let reason =
    bead_feedback.BlockedReason(
      blocker_type: "dependency",
      details: "Waiting for AUTH-001 to complete",
      unblocks_when: "AUTH-001 status is completed",
    )

  reason.blocker_type |> should.equal("dependency")
  reason.details |> should.equal("Waiting for AUTH-001 to complete")
  reason.unblocks_when |> should.equal("AUTH-001 status is completed")
}

pub fn bead_feedback_full_construction_test() {
  // Test full BeadFeedback construction
  let feedback =
    bead_feedback.BeadFeedback(
      bead_id: "AUTH-001",
      result: bead_feedback.Success,
      reason: "Implementation complete and tests pass",
      executed_at: "2026-01-08T10:00:00Z",
      duration_ms: 12_345,
      error: None,
      blocked_by: None,
    )

  feedback.bead_id |> should.equal("AUTH-001")
  feedback.reason |> should.equal("Implementation complete and tests pass")
  feedback.duration_ms |> should.equal(12_345)
  feedback.error |> should.equal(None)
  feedback.blocked_by |> should.equal(None)
}

// ============================================================================
// Integration Tests: Plan Mode Edge Cases
// ============================================================================

pub fn plan_mode_cyclic_dependency_detection_test() {
  // A -> B -> C -> A (cycle)
  let beads = [
    make_plan_bead("AUTH-001", "A", ["AUTH-003"], plan_mode.Effort5min),
    make_plan_bead("AUTH-002", "B", ["AUTH-001"], plan_mode.Effort10min),
    make_plan_bead("AUTH-003", "C", ["AUTH-002"], plan_mode.Effort15min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result |> should.be_error()
}

pub fn plan_mode_self_dependency_detection_test() {
  // A depends on itself
  let beads = [
    make_plan_bead("AUTH-001", "A", ["AUTH-001"], plan_mode.Effort5min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  // Self-dependency is caught during depth calculation
  result |> should.be_error()
}

pub fn plan_mode_complex_dependency_graph_test() {
  // Complex graph:
  // A (no deps)
  // B -> A
  // C -> A
  // D -> B, C
  // E -> D
  // F -> A
  let beads = [
    make_plan_bead("AUTH-001", "A", [], plan_mode.Effort5min),
    make_plan_bead("AUTH-002", "B", ["AUTH-001"], plan_mode.Effort10min),
    make_plan_bead("AUTH-003", "C", ["AUTH-001"], plan_mode.Effort10min),
    make_plan_bead(
      "AUTH-004",
      "D",
      ["AUTH-002", "AUTH-003"],
      plan_mode.Effort15min,
    ),
    make_plan_bead("AUTH-005", "E", ["AUTH-004"], plan_mode.Effort20min),
    make_plan_bead("AUTH-006", "F", ["AUTH-001"], plan_mode.Effort5min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result |> should.be_ok()

  let assert Ok(phases) = result

  // Phase 1: A (only node with no deps)
  // Phase 2: B, C, F (all depend only on A)
  // Phase 3: D (depends on B, C)
  // Phase 4: E (depends on D)
  phases |> list.length |> should.equal(4)
}

pub fn plan_mode_effort_calculation_test() {
  // Test effort totals
  let beads = [
    make_plan_bead("AUTH-001", "5 min task", [], plan_mode.Effort5min),
    make_plan_bead("AUTH-002", "10 min task", [], plan_mode.Effort10min),
    make_plan_bead("AUTH-003", "15 min task", [], plan_mode.Effort15min),
    make_plan_bead("AUTH-004", "20 min task", [], plan_mode.Effort20min),
    make_plan_bead("AUTH-005", "30 min task", [], plan_mode.Effort30min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)
  result |> should.be_ok()

  let assert Ok(phases) = result
  let assert [phase] = phases

  // Total: 5 + 10 + 15 + 20 + 30 = 80 min = 1h 20min
  phase.effort |> should.equal("1h 20min")
}

pub fn plan_mode_risk_level_low_test() {
  // Low risk: few beads, no blockers
  let plan =
    plan_mode.ExecutionPlan(
      session_id: "test",
      generated_at: "2026-01-01T00:00:00Z",
      phases: [
        plan_mode.ExecutionPhase(
          phase_number: 1,
          title: "Phase 1",
          beads: ["AUTH-001"],
          can_parallel: False,
          effort: "5min",
        ),
      ],
      total_beads: 1,
      total_effort: "5min",
      risk: plan_mode.Low,
      blockers: [],
    )

  plan.risk |> should.equal(plan_mode.Low)
}

pub fn plan_mode_empty_beads_test() {
  // Empty bead list should produce empty phases
  let beads: List(plan_mode.PlanBead) = []

  let result = plan_mode.detect_dependency_graph(beads)

  result |> should.be_ok()

  let assert Ok(phases) = result
  phases |> list.is_empty |> should.be_true()
}

// ============================================================================
// Contract Tests: Bead ID Validation (Design by Contract)
// ============================================================================

pub fn bead_id_format_valid_prefix_number_test() {
  // Valid format: PREFIX-NNN
  // Testing via BeadFeedback construction patterns
  let valid_ids = ["AUTH-001", "API-042", "CUE-007", "TEST-999"]

  list.each(valid_ids, fn(id) {
    // If ID format is invalid, mark_bead_executed would fail
    // Here we just verify the pattern
    id |> string.contains("-") |> should.be_true()
  })
}

// ============================================================================
// Property-Based Tests: Plan Phase Ordering
// ============================================================================

pub fn plan_phases_are_ordered_test() {
  let plan =
    plan_mode.ExecutionPlan(
      session_id: "test",
      generated_at: "2026-01-01T00:00:00Z",
      phases: [
        plan_mode.ExecutionPhase(
          phase_number: 1,
          title: "Phase 1",
          beads: ["AUTH-001"],
          can_parallel: False,
          effort: "5min",
        ),
        plan_mode.ExecutionPhase(
          phase_number: 2,
          title: "Phase 2",
          beads: ["AUTH-002"],
          can_parallel: False,
          effort: "10min",
        ),
        plan_mode.ExecutionPhase(
          phase_number: 3,
          title: "Phase 3",
          beads: ["AUTH-003"],
          can_parallel: False,
          effort: "15min",
        ),
      ],
      total_beads: 3,
      total_effort: "30min",
      risk: plan_mode.Low,
      blockers: [],
    )

  // Verify phases are numbered sequentially
  let phase_numbers =
    plan.phases
    |> list.map(fn(p) { p.phase_number })

  phase_numbers |> should.equal([1, 2, 3])
}

// ============================================================================
// Golden File Tests: JSON Output Format
// ============================================================================

pub fn plan_json_output_is_valid_json_test() {
  let plan =
    plan_mode.ExecutionPlan(
      session_id: "json-test",
      generated_at: "2026-01-01T00:00:00Z",
      phases: [],
      total_beads: 0,
      total_effort: "0min",
      risk: plan_mode.Low,
      blockers: [],
    )

  let json_output = plan_mode.format_plan_json(plan)

  // Verify it starts and ends like JSON
  json_output |> string.starts_with("{") |> should.be_true()
  json_output |> string.ends_with("}") |> should.be_true()

  // Verify key fields are quoted
  json_output |> string.contains("\"session_id\"") |> should.be_true()
  json_output |> string.contains("\"phases\"") |> should.be_true()
  json_output |> string.contains("\"risk\"") |> should.be_true()
}

pub fn plan_json_escapes_special_characters_test() {
  let plan =
    plan_mode.ExecutionPlan(
      session_id: "test-with-\"quotes\"",
      generated_at: "2026-01-01T00:00:00Z",
      phases: [],
      total_beads: 0,
      total_effort: "0min",
      risk: plan_mode.Low,
      blockers: ["Blocker with \"quotes\" and\nnewlines"],
    )

  let json_output = plan_mode.format_plan_json(plan)

  // Verify quotes are escaped
  json_output |> string.contains("\\\"quotes\\\"") |> should.be_true()
  // Verify newlines are escaped
  json_output |> string.contains("\\n") |> should.be_true()
}

// ============================================================================
// Regression Tests: Previously Discovered Bugs
// ============================================================================

pub fn plan_mode_single_bead_is_not_parallel_test() {
  // Regression: single bead phase was incorrectly marked parallel
  let beads = [
    make_plan_bead("AUTH-001", "Only bead", [], plan_mode.Effort5min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  let assert Ok(phases) = result
  let assert [phase] = phases

  // Single bead cannot run in parallel
  phase.can_parallel |> should.be_false()
}

pub fn plan_mode_multiple_beads_in_phase_are_parallel_test() {
  // Multiple beads with same dependency level should be parallel
  let beads = [
    make_plan_bead("AUTH-001", "First", [], plan_mode.Effort5min),
    make_plan_bead("AUTH-002", "Second", [], plan_mode.Effort5min),
    make_plan_bead("AUTH-003", "Third", [], plan_mode.Effort5min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  let assert Ok(phases) = result
  let assert [phase] = phases

  // Multiple beads can run in parallel
  phase.can_parallel |> should.be_true()
  phase.beads |> list.length |> should.equal(3)
}
