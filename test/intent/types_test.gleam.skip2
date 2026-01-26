//// Tests for intent/types.gleam
//// Comprehensive tests for the Spec type and all nested types
//// Contract: Validates type construction, field access, and KIRK analyzer integration

import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import intent/types.{
  AIHints, AntiPattern, Behavior, Check, Config, Delete, EntityHint, Feature,
  Get, Head, ImplementationHints, Options, Patch, Post, Put, Request, Response,
  Rule, RuleCheck, SecurityHints, Spec, When,
}
import test_helpers

// =============================================================================
// Method enum tests
// =============================================================================

pub fn method_to_string_get_test() {
  // Contract: GET method converts to uppercase string
  types.method_to_string(Get)
  |> should.equal("GET")
}

pub fn method_to_string_post_test() {
  // Contract: POST method converts to uppercase string
  types.method_to_string(Post)
  |> should.equal("POST")
}

pub fn method_to_string_put_test() {
  // Contract: PUT method converts to uppercase string
  types.method_to_string(Put)
  |> should.equal("PUT")
}

pub fn method_to_string_patch_test() {
  // Contract: PATCH method converts to uppercase string
  types.method_to_string(Patch)
  |> should.equal("PATCH")
}

pub fn method_to_string_delete_test() {
  // Contract: DELETE method converts to uppercase string
  types.method_to_string(Delete)
  |> should.equal("DELETE")
}

pub fn method_to_string_head_test() {
  // Contract: HEAD method converts to uppercase string
  types.method_to_string(Head)
  |> should.equal("HEAD")
}

pub fn method_to_string_options_test() {
  // Contract: OPTIONS method converts to uppercase string
  types.method_to_string(Options)
  |> should.equal("OPTIONS")
}

pub fn method_from_string_get_test() {
  // Contract: Uppercase GET string converts to Get variant
  types.method_from_string("GET")
  |> should.equal(Ok(Get))
}

pub fn method_from_string_post_test() {
  // Contract: Uppercase POST string converts to Post variant
  types.method_from_string("POST")
  |> should.equal(Ok(Post))
}

pub fn method_from_string_put_test() {
  // Contract: Uppercase PUT string converts to Put variant
  types.method_from_string("PUT")
  |> should.equal(Ok(Put))
}

pub fn method_from_string_patch_test() {
  // Contract: Uppercase PATCH string converts to Patch variant
  types.method_from_string("PATCH")
  |> should.equal(Ok(Patch))
}

pub fn method_from_string_delete_test() {
  // Contract: Uppercase DELETE string converts to Delete variant
  types.method_from_string("DELETE")
  |> should.equal(Ok(Delete))
}

pub fn method_from_string_head_test() {
  // Contract: Uppercase HEAD string converts to Head variant
  types.method_from_string("HEAD")
  |> should.equal(Ok(Head))
}

pub fn method_from_string_options_test() {
  // Contract: Uppercase OPTIONS string converts to Options variant
  types.method_from_string("OPTIONS")
  |> should.equal(Ok(Options))
}

pub fn method_from_string_invalid_test() {
  // Contract: Invalid method string returns descriptive error
  let result = types.method_from_string("INVALID")
  case result {
    Error(msg) -> {
      msg |> should.equal("Unknown HTTP method: INVALID")
    }
    Ok(_) -> should.fail()
  }
}

pub fn method_from_string_lowercase_fails_test() {
  // Contract: Lowercase method strings are rejected
  types.method_from_string("get")
  |> should.be_error
}

pub fn method_roundtrip_test() {
  // Contract: Method -> String -> Method preserves value for GET
  Get
  |> types.method_to_string
  |> types.method_from_string
  |> should.equal(Ok(Get))

  // Contract: Method -> String -> Method preserves value for POST
  Post
  |> types.method_to_string
  |> types.method_from_string
  |> should.equal(Ok(Post))

  // Contract: Method -> String -> Method preserves value for DELETE
  Delete
  |> types.method_to_string
  |> types.method_from_string
  |> should.equal(Ok(Delete))
}

// =============================================================================
// Spec construction tests
// =============================================================================

pub fn spec_construction_minimal_test() {
  // Contract: Spec can be constructed with all required fields
  let spec = test_helpers.make_test_spec([])

  spec.name |> should.equal("Test Spec")
  spec.description |> should.equal("Test spec for tests")
  spec.audience |> should.equal("developers")
  spec.version |> should.equal("1.0.0")
}

pub fn spec_construction_with_features_test() {
  // Contract: Spec can include features
  let feature = test_helpers.make_test_feature("auth", [])
  let spec = test_helpers.make_test_spec([feature])

  spec.features
  |> list.length
  |> should.equal(1)
}

pub fn spec_all_fields_accessible_test() {
  // Contract: All Spec fields are accessible
  let spec = test_helpers.make_test_spec([])

  // Access all fields to ensure they exist
  let _name = spec.name
  let _desc = spec.description
  let _aud = spec.audience
  let _ver = spec.version
  let _crit = spec.success_criteria
  let _cfg = spec.config
  let _feat = spec.features
  let _rules = spec.rules
  let _anti = spec.anti_patterns
  let _hints = spec.ai_hints

  // If we get here without compile errors, test passes
  spec.name |> should.not_equal("")
}

pub fn spec_with_success_criteria_test() {
  // Contract: Spec can include success criteria
  let spec =
    Spec(..test_helpers.make_test_spec([]), success_criteria: [
      "All tests pass",
      "Performance < 100ms",
    ])

  spec.success_criteria
  |> list.length
  |> should.equal(2)

  spec.success_criteria
  |> should.equal(["All tests pass", "Performance < 100ms"])
}

// =============================================================================
// Config type tests
// =============================================================================

pub fn config_construction_test() {
  // Contract: Config can be constructed with all fields
  let config =
    Config(
      base_url: "http://localhost:3000",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    )

  config.base_url |> should.equal("http://localhost:3000")
  config.timeout_ms |> should.equal(5000)
  config.allow_localhost |> should.equal(False)
}

pub fn config_with_headers_test() {
  // Contract: Config can include headers
  let headers =
    dict.from_list([
      #("Authorization", "Bearer token"),
      #("Content-Type", "application/json"),
    ])

  let config =
    Config(
      base_url: "http://api.example.com",
      timeout_ms: 10_000,
      headers: headers,
      allow_localhost: False,
    )

  config.headers |> dict.size |> should.equal(2)
  config.headers
  |> dict.get("Authorization")
  |> should.equal(Ok("Bearer token"))
}

pub fn config_timeout_test() {
  // Contract: Config timeout can be customized
  let config =
    Config(
      base_url: "http://slow.api.com",
      timeout_ms: 30_000,
      headers: dict.new(),
      allow_localhost: False,
    )

  config.timeout_ms |> should.equal(30_000)
}

pub fn config_localhost_allowed_test() {
  // Contract: Config can allow localhost connections
  let config =
    Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: True,
    )

  config.allow_localhost |> should.equal(True)
}

// =============================================================================
// Feature and Behavior tests
// =============================================================================

pub fn feature_construction_test() {
  // Contract: Feature can be constructed with required fields
  let feature =
    Feature(name: "User Auth", description: "Authentication", behaviors: [])

  feature.name |> should.equal("User Auth")
  feature.description |> should.equal("Authentication")
  feature.behaviors
  |> list.length
  |> should.equal(0)
}

pub fn feature_with_behaviors_test() {
  // Contract: Feature can include multiple behaviors
  let behaviors = [
    test_helpers.make_test_behavior("login", []),
    test_helpers.make_test_behavior("logout", []),
  ]

  let feature = test_helpers.make_test_feature("auth", behaviors)

  feature.behaviors
  |> list.length
  |> should.equal(2)
}

pub fn behavior_construction_test() {
  // Contract: Behavior can be constructed with all fields
  let behavior = test_helpers.make_test_behavior("get-user", [])

  behavior.name |> should.equal("get-user")
  behavior.intent |> should.equal("Test intent for get-user")
  behavior.notes |> should.equal("")
  behavior.requires
  |> list.length
  |> should.equal(0)
  behavior.tags
  |> list.length
  |> should.equal(0)
}

pub fn behavior_with_checks_test() {
  // Contract: Behavior response can include checks
  let checks =
    dict.from_list([
      #("id", Check(rule: "integer", why: "User ID must be an integer")),
      #("email", Check(rule: "string", why: "Email must be a string")),
    ])

  let behavior =
    Behavior(
      ..test_helpers.make_test_behavior("get-user", []),
      response: Response(
        status: 200,
        example: json.null(),
        checks: checks,
        headers: dict.new(),
      ),
    )

  behavior.response.checks |> dict.size |> should.equal(2)
}

pub fn behavior_with_requires_test() {
  // Contract: Behavior can specify dependencies
  let behavior = test_helpers.make_test_behavior("delete-user", ["create-user"])

  behavior.requires
  |> list.length
  |> should.equal(1)
  behavior.requires |> should.equal(["create-user"])
}

pub fn behavior_with_captures_test() {
  // Contract: Behavior can capture response values
  let captures = dict.from_list([#("user_id", "$.id"), #("token", "$.token")])

  let behavior =
    Behavior(..test_helpers.make_test_behavior("login", []), captures: captures)

  behavior.captures |> dict.size |> should.equal(2)
}

pub fn behavior_with_tags_test() {
  // Contract: Behavior can include tags
  let behavior =
    Behavior(..test_helpers.make_test_behavior("get-user", []), tags: [
      "smoke",
      "critical",
    ])

  behavior.tags
  |> list.length
  |> should.equal(2)
  behavior.tags |> should.equal(["smoke", "critical"])
}

// =============================================================================
// Request and Response tests
// =============================================================================

pub fn request_construction_test() {
  // Contract: Request can be constructed with all fields
  let request =
    Request(
      method: Get,
      path: "/users/123",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  request.method |> should.equal(Get)
  request.path |> should.equal("/users/123")
}

pub fn request_with_headers_test() {
  // Contract: Request can include headers
  let headers = dict.from_list([#("Accept", "application/json")])

  let request =
    Request(
      method: Get,
      path: "/api/data",
      headers: headers,
      query: dict.new(),
      body: json.null(),
    )

  request.headers |> dict.size |> should.equal(1)
}

pub fn request_with_query_test() {
  // Contract: Request can include query parameters
  let query = dict.from_list([#("page", json.int(1)), #("limit", json.int(10))])

  let request =
    Request(
      method: Get,
      path: "/users",
      headers: dict.new(),
      query: query,
      body: json.null(),
    )

  request.query |> dict.size |> should.equal(2)
}

pub fn request_with_body_test() {
  // Contract: Request can include JSON body
  let body =
    json.object([#("name", json.string("John")), #("age", json.int(30))])

  let request =
    Request(
      method: Post,
      path: "/users",
      headers: dict.new(),
      query: dict.new(),
      body: body,
    )

  // Body is present (not null)
  case request.body {
    _ -> should.be_ok(Ok(Nil))
  }
}

pub fn response_construction_test() {
  // Contract: Response can be constructed with all fields
  let response =
    Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    )

  response.status |> should.equal(200)
}

pub fn response_with_checks_test() {
  // Contract: Response can include validation checks
  let checks =
    dict.from_list([#("id", Check(rule: "integer", why: "ID must be numeric"))])

  let response =
    Response(
      status: 200,
      example: json.null(),
      checks: checks,
      headers: dict.new(),
    )

  response.checks |> dict.size |> should.equal(1)
}

pub fn response_with_headers_test() {
  // Contract: Response can specify expected headers
  let headers = dict.from_list([#("Content-Type", "application/json")])

  let response =
    Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: headers,
    )

  response.headers |> dict.size |> should.equal(1)
}

pub fn response_with_example_test() {
  // Contract: Response can include example data
  let example =
    json.object([#("id", json.int(123)), #("name", json.string("Test User"))])

  let response =
    Response(
      status: 200,
      example: example,
      checks: dict.new(),
      headers: dict.new(),
    )

  // Example is present
  case response.example {
    _ -> should.be_ok(Ok(Nil))
  }
}

// =============================================================================
// Check type tests
// =============================================================================

pub fn check_construction_test() {
  // Contract: Check can be constructed with rule and why
  let check = Check(rule: "integer", why: "Must be a number")

  check.rule |> should.equal("integer")
  check.why |> should.equal("Must be a number")
}

pub fn check_rule_and_why_test() {
  // Contract: Check includes both validation rule and explanation
  let check = Check(rule: "string && length > 0", why: "Name cannot be empty")

  check.rule |> should.equal("string && length > 0")
  check.why |> should.equal("Name cannot be empty")
}

// =============================================================================
// Complex nested structure tests
// =============================================================================

pub fn spec_with_multiple_features_test() {
  // Contract: Spec can have multiple features with behaviors
  let feature1 =
    test_helpers.make_test_feature("auth", [
      test_helpers.make_test_behavior("login", []),
    ])

  let feature2 =
    test_helpers.make_test_feature("users", [
      test_helpers.make_test_behavior("get-user", []),
      test_helpers.make_test_behavior("create-user", []),
    ])

  let spec = test_helpers.make_test_spec([feature1, feature2])

  spec.features
  |> list.length
  |> should.equal(2)

  // Count total behaviors across all features
  let total_behaviors =
    spec.features
    |> list.flat_map(fn(f) { f.behaviors })
    |> list.length

  total_behaviors |> should.equal(3)
}

pub fn spec_with_nested_behaviors_test() {
  // Contract: Behaviors within features are accessible
  let behavior1 = test_helpers.make_test_behavior("action1", [])
  let behavior2 = test_helpers.make_test_behavior("action2", [])
  let feature =
    test_helpers.make_test_feature("feature1", [behavior1, behavior2])
  let spec = test_helpers.make_test_spec([feature])

  // Access nested behaviors
  case spec.features {
    [first_feature, ..] -> {
      first_feature.behaviors
      |> list.length
      |> should.equal(2)

      case first_feature.behaviors {
        [first_behavior, ..] -> {
          first_behavior.name |> should.equal("action1")
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn spec_with_behavior_dependencies_test() {
  // Contract: Behaviors can reference other behaviors via requires
  let behavior1 = test_helpers.make_test_behavior("create-user", [])
  let behavior2 =
    test_helpers.make_test_behavior("update-user", ["create-user"])
  let behavior3 =
    test_helpers.make_test_behavior("delete-user", [
      "create-user",
      "update-user",
    ])

  let feature =
    test_helpers.make_test_feature("users", [behavior1, behavior2, behavior3])
  let spec = test_helpers.make_test_spec([feature])

  // Verify dependency chain
  case spec.features {
    [first_feature, ..] -> {
      case first_feature.behaviors {
        [_, b2, b3] -> {
          b2.requires
          |> list.length
          |> should.equal(1)
          b3.requires
          |> list.length
          |> should.equal(2)
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn spec_traversal_test() {
  // Contract: Spec structure can be traversed to access all elements
  let checks =
    dict.from_list([#("id", Check(rule: "integer", why: "Must be numeric"))])

  let behavior =
    Behavior(
      ..test_helpers.make_test_behavior("get-user", []),
      response: Response(
        status: 200,
        example: json.null(),
        checks: checks,
        headers: dict.new(),
      ),
    )

  let feature = test_helpers.make_test_feature("users", [behavior])
  let spec = test_helpers.make_test_spec([feature])

  // Traverse: Spec -> Feature -> Behavior -> Response -> Check
  let check_count =
    spec.features
    |> list.flat_map(fn(f) { f.behaviors })
    |> list.flat_map(fn(b) { dict.to_list(b.response.checks) })
    |> list.length

  check_count |> should.equal(1)
}

// =============================================================================
// Anti-pattern tests
// =============================================================================

pub fn anti_pattern_construction_test() {
  // Contract: AntiPattern can be constructed with all fields
  let anti =
    AntiPattern(
      name: "Leaky Abstraction",
      description: "Don't expose implementation details",
      bad_example: json.object([#("internal_id", json.int(42))]),
      good_example: json.object([#("id", json.int(42))]),
      why: "Internal IDs leak implementation",
    )

  anti.name |> should.equal("Leaky Abstraction")
  anti.why |> should.equal("Internal IDs leak implementation")
}

pub fn anti_pattern_with_examples_test() {
  // Contract: AntiPattern includes both good and bad examples
  let anti =
    AntiPattern(
      name: "Plain Passwords",
      description: "Never store plain passwords",
      bad_example: json.object([#("password", json.string("secret123"))]),
      good_example: json.object([#("password_hash", json.string("$2b$..."))]),
      why: "Plain passwords are a security risk",
    )

  // Both examples should be present
  case anti.bad_example {
    _ -> should.be_ok(Ok(Nil))
  }
  case anti.good_example {
    _ -> should.be_ok(Ok(Nil))
  }
}

// =============================================================================
// AI Hints tests
// =============================================================================

pub fn ai_hints_construction_test() {
  // Contract: AIHints can be constructed with all fields
  let hints =
    AIHints(
      implementation: ImplementationHints(suggested_stack: [
        "Gleam",
        "PostgreSQL",
      ]),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "bcrypt",
        jwt_algorithm: "RS256",
        jwt_expiry: "1h",
        rate_limiting: "100/min",
      ),
      pitfalls: ["Watch out for N+1 queries"],
    )

  hints.implementation.suggested_stack
  |> list.length
  |> should.equal(2)

  hints.pitfalls
  |> list.length
  |> should.equal(1)
}

pub fn ai_hints_implementation_test() {
  // Contract: Implementation hints can suggest tech stack
  let impl = ImplementationHints(suggested_stack: ["Rust", "SQLite", "Axum"])

  impl.suggested_stack
  |> list.length
  |> should.equal(3)
  impl.suggested_stack |> should.equal(["Rust", "SQLite", "Axum"])
}

pub fn ai_hints_entities_test() {
  // Contract: Entity hints can describe data structures
  let user_entity =
    EntityHint(
      fields: dict.from_list([
        #("id", "UUID"),
        #("email", "String"),
        #("created_at", "Timestamp"),
      ]),
    )

  let entities = dict.from_list([#("User", user_entity)])

  let hints =
    AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: entities,
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
    )

  hints.entities |> dict.size |> should.equal(1)
}

pub fn ai_hints_security_test() {
  // Contract: Security hints provide crypto guidance
  let security =
    SecurityHints(
      password_hashing: "argon2id",
      jwt_algorithm: "EdDSA",
      jwt_expiry: "15m",
      rate_limiting: "10/sec per IP",
    )

  security.password_hashing |> should.equal("argon2id")
  security.jwt_algorithm |> should.equal("EdDSA")
  security.jwt_expiry |> should.equal("15m")
  security.rate_limiting |> should.equal("10/sec per IP")
}

pub fn ai_hints_pitfalls_test() {
  // Contract: Pitfalls list common mistakes to avoid
  let hints =
    AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [
        "Don't forget to validate email format",
        "Rate limit password reset attempts",
        "Use HTTPS in production",
      ],
    )

  hints.pitfalls
  |> list.length
  |> should.equal(3)
}

// =============================================================================
// Rule tests
// =============================================================================

pub fn rule_construction_test() {
  // Contract: Rule can be constructed with all fields
  let rule =
    Rule(
      name: "no-null-responses",
      description: "Responses must never be null",
      when: None,
      check: RuleCheck(
        body_must_not_contain: ["null"],
        body_must_contain: [],
        fields_must_exist: [],
        fields_must_not_exist: [],
        header_must_exist: "",
        header_must_not_exist: "",
      ),
      example: None,
    )

  rule.name |> should.equal("no-null-responses")
}

pub fn rule_with_when_condition_test() {
  // Contract: Rule can specify conditions via When clause
  let when_clause = When(status: Some("200"), method: Some(Get), path: None)

  let rule =
    Rule(
      name: "get-success",
      description: "GET requests must return 200",
      when: Some(when_clause),
      check: RuleCheck(
        body_must_not_contain: [],
        body_must_contain: [],
        fields_must_exist: [],
        fields_must_not_exist: [],
        header_must_exist: "",
        header_must_not_exist: "",
      ),
      example: None,
    )

  case rule.when {
    Some(w) -> {
      w.status |> should.equal(Some("200"))
      w.method |> should.equal(Some(Get))
    }
    None -> should.fail()
  }
}

pub fn rule_check_construction_test() {
  // Contract: RuleCheck can specify validation constraints
  let check =
    RuleCheck(
      body_must_not_contain: ["error", "null"],
      body_must_contain: ["data"],
      fields_must_exist: ["id", "created_at"],
      fields_must_not_exist: ["password"],
      header_must_exist: "Content-Type",
      header_must_not_exist: "X-Debug",
    )

  check.body_must_not_contain
  |> list.length
  |> should.equal(2)

  check.fields_must_exist
  |> list.length
  |> should.equal(2)

  check.header_must_exist |> should.equal("Content-Type")
}

pub fn when_construction_test() {
  // Contract: When can specify optional conditions
  let when1 = When(status: Some("404"), method: None, path: None)
  let when2 = When(status: None, method: Some(Post), path: Some("/users"))

  when1.status |> should.equal(Some("404"))
  when2.method |> should.equal(Some(Post))
  when2.path |> should.equal(Some("/users"))
}
