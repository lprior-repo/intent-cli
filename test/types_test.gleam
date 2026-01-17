//// Comprehensive tests for intent/types.gleam
//// Tests cover all type constructors, method conversions, and pattern matching
////
//// Design by Contract:
//// - Preconditions: Valid type constructors, valid method strings
//// - Postconditions: All methods convert correctly, types construct properly
//// - Invariants: Round-trip conversions preserve values, exhaustive pattern matching

import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleeunit
import gleeunit/should
import intent/types.{
  type AIHints, type AntiPattern, type Behavior, type Check, type Config,
  type EntityHint, type Feature, type ImplementationHints, type Method,
  type Request, type Response, type Rule, type RuleCheck, type SecurityHints,
  type Spec, type When, AIHints, AntiPattern, Behavior, Check, Config, Delete,
  EntityHint, Feature, Get, Head, ImplementationHints, Options, Patch, Post, Put,
  Request, Response, Rule, RuleCheck, SecurityHints, Spec, When,
  method_from_string, method_to_string,
}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Method Conversion Tests
// ============================================================================

pub fn method_to_string_get_test() {
  Get
  |> method_to_string
  |> should.equal("GET")
}

pub fn method_to_string_post_test() {
  Post
  |> method_to_string
  |> should.equal("POST")
}

pub fn method_to_string_put_test() {
  Put
  |> method_to_string
  |> should.equal("PUT")
}

pub fn method_to_string_patch_test() {
  Patch
  |> method_to_string
  |> should.equal("PATCH")
}

pub fn method_to_string_delete_test() {
  Delete
  |> method_to_string
  |> should.equal("DELETE")
}

pub fn method_to_string_head_test() {
  Head
  |> method_to_string
  |> should.equal("HEAD")
}

pub fn method_to_string_options_test() {
  Options
  |> method_to_string
  |> should.equal("OPTIONS")
}

pub fn method_from_string_get_test() {
  "GET"
  |> method_from_string
  |> should.be_ok
  |> should.equal(Get)
}

pub fn method_from_string_post_test() {
  "POST"
  |> method_from_string
  |> should.be_ok
  |> should.equal(Post)
}

pub fn method_from_string_put_test() {
  "PUT"
  |> method_from_string
  |> should.be_ok
  |> should.equal(Put)
}

pub fn method_from_string_patch_test() {
  "PATCH"
  |> method_from_string
  |> should.be_ok
  |> should.equal(Patch)
}

pub fn method_from_string_delete_test() {
  "DELETE"
  |> method_from_string
  |> should.be_ok
  |> should.equal(Delete)
}

pub fn method_from_string_head_test() {
  "HEAD"
  |> method_from_string
  |> should.be_ok
  |> should.equal(Head)
}

pub fn method_from_string_options_test() {
  "OPTIONS"
  |> method_from_string
  |> should.be_ok
  |> should.equal(Options)
}

pub fn method_from_string_invalid_test() {
  "INVALID"
  |> method_from_string
  |> should.be_error
  |> should.equal("Unknown HTTP method: INVALID")
}

pub fn method_from_string_lowercase_test() {
  "get"
  |> method_from_string
  |> should.be_error
  |> should.equal("Unknown HTTP method: get")
}

pub fn method_from_string_mixed_case_test() {
  "Get"
  |> method_from_string
  |> should.be_error
  |> should.equal("Unknown HTTP method: Get")
}

pub fn method_from_string_empty_test() {
  ""
  |> method_from_string
  |> should.be_error
  |> should.equal("Unknown HTTP method: ")
}

pub fn method_from_string_whitespace_test() {
  " GET "
  |> method_from_string
  |> should.be_error
  |> should.equal("Unknown HTTP method:  GET ")
}

// ============================================================================
// Method Round-Trip Conversion Tests
// ============================================================================

pub fn method_roundtrip_get_test() {
  Get
  |> method_to_string
  |> method_from_string
  |> should.be_ok
  |> should.equal(Get)
}

pub fn method_roundtrip_post_test() {
  Post
  |> method_to_string
  |> method_from_string
  |> should.be_ok
  |> should.equal(Post)
}

pub fn method_roundtrip_put_test() {
  Put
  |> method_to_string
  |> method_from_string
  |> should.be_ok
  |> should.equal(Put)
}

pub fn method_roundtrip_patch_test() {
  Patch
  |> method_to_string
  |> method_from_string
  |> should.be_ok
  |> should.equal(Patch)
}

pub fn method_roundtrip_delete_test() {
  Delete
  |> method_to_string
  |> method_from_string
  |> should.be_ok
  |> should.equal(Delete)
}

pub fn method_roundtrip_head_test() {
  Head
  |> method_to_string
  |> method_from_string
  |> should.be_ok
  |> should.equal(Head)
}

pub fn method_roundtrip_options_test() {
  Options
  |> method_to_string
  |> method_from_string
  |> should.be_ok
  |> should.equal(Options)
}

// ============================================================================
// Method Pattern Matching Tests
// ============================================================================

pub fn method_pattern_match_all_test() {
  // Exhaustive pattern matching test
  let methods = [Get, Post, Put, Patch, Delete, Head, Options]

  methods
  |> list.map(fn(method) {
    case method {
      Get -> "get"
      Post -> "post"
      Put -> "put"
      Patch -> "patch"
      Delete -> "delete"
      Head -> "head"
      Options -> "options"
    }
  })
  |> should.equal(["get", "post", "put", "patch", "delete", "head", "options"])
}

pub fn method_pattern_match_safe_vs_unsafe_test() {
  // Test pattern matching for safe vs unsafe methods
  let is_safe = fn(method: Method) -> Bool {
    case method {
      Get | Head | Options -> True
      Post | Put | Patch | Delete -> False
    }
  }

  is_safe(Get) |> should.be_true
  is_safe(Head) |> should.be_true
  is_safe(Options) |> should.be_true
  is_safe(Post) |> should.be_false
  is_safe(Put) |> should.be_false
  is_safe(Patch) |> should.be_false
  is_safe(Delete) |> should.be_false
}

pub fn method_pattern_match_idempotent_test() {
  // Test pattern matching for idempotent methods
  let is_idempotent = fn(method: Method) -> Bool {
    case method {
      Get | Put | Delete | Head | Options -> True
      Post | Patch -> False
    }
  }

  is_idempotent(Get) |> should.be_true
  is_idempotent(Put) |> should.be_true
  is_idempotent(Delete) |> should.be_true
  is_idempotent(Head) |> should.be_true
  is_idempotent(Options) |> should.be_true
  is_idempotent(Post) |> should.be_false
  is_idempotent(Patch) |> should.be_false
}

// ============================================================================
// Type Constructor Tests - Config
// ============================================================================

pub fn config_minimal_test() {
  let config = Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
  )

  config.base_url |> should.equal("http://localhost:8080")
  config.timeout_ms |> should.equal(5000)
  config.headers |> dict.size |> should.equal(0)
}

pub fn config_with_headers_test() {
  let headers = dict.from_list([
    #("Content-Type", "application/json"),
    #("Authorization", "Bearer token123"),
  ])

  let config = Config(
    base_url: "https://api.example.com",
    timeout_ms: 10000,
    headers: headers,
  )

  config.base_url |> should.equal("https://api.example.com")
  config.timeout_ms |> should.equal(10000)
  config.headers |> dict.size |> should.equal(2)
  config.headers |> dict.get("Content-Type") |> should.be_ok
    |> should.equal("application/json")
}

pub fn config_zero_timeout_test() {
  let config = Config(
    base_url: "http://localhost",
    timeout_ms: 0,
    headers: dict.new(),
  )

  config.timeout_ms |> should.equal(0)
}

pub fn config_negative_timeout_test() {
  // Type system allows negative timeout, runtime would handle
  let config = Config(
    base_url: "http://localhost",
    timeout_ms: -1,
    headers: dict.new(),
  )

  config.timeout_ms |> should.equal(-1)
}

// ============================================================================
// Type Constructor Tests - Request
// ============================================================================

pub fn request_minimal_get_test() {
  let request = Request(
    method: Get,
    path: "/users",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  request.method |> should.equal(Get)
  request.path |> should.equal("/users")
  request.headers |> dict.size |> should.equal(0)
  request.query |> dict.size |> should.equal(0)
}

pub fn request_post_with_body_test() {
  let body = json.object([
    #("name", json.string("Alice")),
    #("age", json.int(30)),
  ])

  let request = Request(
    method: Post,
    path: "/users",
    headers: dict.from_list([#("Content-Type", "application/json")]),
    query: dict.new(),
    body: body,
  )

  request.method |> should.equal(Post)
  // Dict order is not guaranteed, check both possible orderings
  let json_str = request.body |> json.to_string
  case json_str {
    "{\"age\":30,\"name\":\"Alice\"}" -> Nil
    "{\"name\":\"Alice\",\"age\":30}" -> Nil
    _ -> should.fail()
  }
}

pub fn request_with_query_params_test() {
  let query = dict.from_list([
    #("limit", json.int(10)),
    #("offset", json.int(20)),
    #("sort", json.string("name")),
  ])

  let request = Request(
    method: Get,
    path: "/users",
    headers: dict.new(),
    query: query,
    body: json.null(),
  )

  request.query |> dict.size |> should.equal(3)
  request.query |> dict.get("limit") |> should.be_ok
}

pub fn request_all_methods_test() {
  let methods = [Get, Post, Put, Patch, Delete, Head, Options]

  methods
  |> list.map(fn(method) {
    Request(
      method: method,
      path: "/test",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )
  })
  |> list.length
  |> should.equal(7)
}

pub fn request_empty_path_test() {
  let request = Request(
    method: Get,
    path: "",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  request.path |> should.equal("")
}

pub fn request_root_path_test() {
  let request = Request(
    method: Get,
    path: "/",
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )

  request.path |> should.equal("/")
}

// ============================================================================
// Type Constructor Tests - Response
// ============================================================================

pub fn response_minimal_test() {
  let response = Response(
    status: 200,
    example: json.null(),
    checks: dict.new(),
    headers: dict.new(),
  )

  response.status |> should.equal(200)
  response.checks |> dict.size |> should.equal(0)
  response.headers |> dict.size |> should.equal(0)
}

pub fn response_with_example_test() {
  let example = json.object([
    #("id", json.int(1)),
    #("name", json.string("Test User")),
  ])

  let response = Response(
    status: 200,
    example: example,
    checks: dict.new(),
    headers: dict.new(),
  )

  response.example |> json.to_string
    |> should.equal("{\"id\":1,\"name\":\"Test User\"}")
}

pub fn response_with_checks_test() {
  let checks = dict.from_list([
    #("id_exists", Check(rule: "id exists", why: "ID is required")),
    #("name_string", Check(rule: "name is string", why: "Name must be string")),
  ])

  let response = Response(
    status: 200,
    example: json.null(),
    checks: checks,
    headers: dict.new(),
  )

  response.checks |> dict.size |> should.equal(2)
}

pub fn response_4xx_status_test() {
  let response = Response(
    status: 404,
    example: json.object([#("error", json.string("Not found"))]),
    checks: dict.new(),
    headers: dict.new(),
  )

  response.status |> should.equal(404)
}

pub fn response_5xx_status_test() {
  let response = Response(
    status: 500,
    example: json.object([#("error", json.string("Internal error"))]),
    checks: dict.new(),
    headers: dict.new(),
  )

  response.status |> should.equal(500)
}

pub fn response_with_headers_test() {
  let headers = dict.from_list([
    #("Content-Type", "application/json"),
    #("Cache-Control", "no-cache"),
  ])

  let response = Response(
    status: 200,
    example: json.null(),
    checks: dict.new(),
    headers: headers,
  )

  response.headers |> dict.size |> should.equal(2)
  response.headers |> dict.get("Content-Type") |> should.be_ok
    |> should.equal("application/json")
}

// ============================================================================
// Type Constructor Tests - Check
// ============================================================================

pub fn check_simple_test() {
  let check = Check(rule: "status == 200", why: "Request should succeed")

  check.rule |> should.equal("status == 200")
  check.why |> should.equal("Request should succeed")
}

pub fn check_empty_strings_test() {
  let check = Check(rule: "", why: "")

  check.rule |> should.equal("")
  check.why |> should.equal("")
}

pub fn check_complex_rule_test() {
  let check = Check(
    rule: "response.data.users.length > 0 && response.data.users[0].id exists",
    why: "Must return at least one user with an ID",
  )

  check.rule
    |> should.equal("response.data.users.length > 0 && response.data.users[0].id exists")
}

// ============================================================================
// Type Constructor Tests - Behavior
// ============================================================================

pub fn behavior_minimal_test() {
  let behavior = Behavior(
    name: "get_user",
    intent: "Retrieve user by ID",
    notes: "",
    requires: [],
    tags: [],
    request: Request(
      method: Get,
      path: "/users/1",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )

  behavior.name |> should.equal("get_user")
  behavior.intent |> should.equal("Retrieve user by ID")
  behavior.requires |> list.length |> should.equal(0)
}

pub fn behavior_with_dependencies_test() {
  let behavior = Behavior(
    name: "delete_user",
    intent: "Delete a user",
    notes: "Requires authentication",
    requires: ["create_user", "login"],
    tags: ["destructive", "auth"],
    request: Request(
      method: Delete,
      path: "/users/1",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: 204,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )

  behavior.requires |> list.length |> should.equal(2)
  behavior.requires |> should.equal(["create_user", "login"])
  behavior.tags |> should.equal(["destructive", "auth"])
}

pub fn behavior_with_captures_test() {
  let captures = dict.from_list([
    #("user_id", "response.id"),
    #("auth_token", "response.token"),
  ])

  let behavior = Behavior(
    name: "login",
    intent: "User login",
    notes: "",
    requires: [],
    tags: [],
    request: Request(
      method: Post,
      path: "/login",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: captures,
  )

  behavior.captures |> dict.size |> should.equal(2)
  behavior.captures |> dict.get("user_id") |> should.be_ok
    |> should.equal("response.id")
}

pub fn behavior_with_notes_test() {
  let behavior = Behavior(
    name: "test",
    intent: "Test behavior",
    notes: "This is a long note explaining\nthe behavior across multiple lines",
    requires: [],
    tags: [],
    request: Request(
      method: Get,
      path: "/test",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )

  behavior.notes
    |> should.equal("This is a long note explaining\nthe behavior across multiple lines")
}

// ============================================================================
// Type Constructor Tests - Feature
// ============================================================================

pub fn feature_minimal_test() {
  let behaviors = [
    Behavior(
      name: "test1",
      intent: "Test 1",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/test1",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    ),
  ]

  let feature = Feature(
    name: "Test Feature",
    description: "A test feature",
    behaviors: behaviors,
  )

  feature.name |> should.equal("Test Feature")
  feature.description |> should.equal("A test feature")
  feature.behaviors |> list.length |> should.equal(1)
}

pub fn feature_multiple_behaviors_test() {
  let behaviors =
    list.range(1, 5)
    |> list.map(fn(i) {
      Behavior(
        name: "behavior_" <> int.to_string(i),
        intent: "Test behavior " <> int.to_string(i),
        notes: "",
        requires: [],
        tags: [],
        request: Request(
          method: Get,
          path: "/test",
          headers: dict.new(),
          query: dict.new(),
          body: json.null(),
        ),
        response: Response(
          status: 200,
          example: json.null(),
          checks: dict.new(),
          headers: dict.new(),
        ),
        captures: dict.new(),
      )
    })

  let feature = Feature(
    name: "Multi-behavior Feature",
    description: "Feature with multiple behaviors",
    behaviors: behaviors,
  )

  feature.behaviors |> list.length |> should.equal(5)
}

pub fn feature_empty_description_test() {
  let feature = Feature(
    name: "Test",
    description: "",
    behaviors: [],
  )

  feature.description |> should.equal("")
}

// ============================================================================
// Type Constructor Tests - Rule
// ============================================================================

pub fn rule_minimal_test() {
  let rule = Rule(
    name: "no_internal_errors",
    description: "Responses must not expose internal errors",
    when: When(status: "5xx", method: Get, path: "*"),
    check: RuleCheck(
      body_must_not_contain: ["stack trace", "internal error"],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  rule.name |> should.equal("no_internal_errors")
  rule.when.status |> should.equal("5xx")
  rule.check.body_must_not_contain |> list.length |> should.equal(2)
}

pub fn rule_complex_check_test() {
  let rule = Rule(
    name: "auth_response",
    description: "Auth responses must have specific structure",
    when: When(status: "200", method: Post, path: "/auth/*"),
    check: RuleCheck(
      body_must_not_contain: ["password", "secret"],
      body_must_contain: ["token", "expires_at"],
      fields_must_exist: ["user_id", "token"],
      fields_must_not_exist: ["password_hash", "internal_id"],
      header_must_exist: "Set-Cookie",
      header_must_not_exist: "X-Debug-Info",
    ),
    example: json.object([
      #("user_id", json.int(1)),
      #("token", json.string("abc123")),
      #("expires_at", json.string("2024-12-31T23:59:59Z")),
    ]),
  )

  rule.check.body_must_contain |> list.length |> should.equal(2)
  rule.check.fields_must_exist |> list.length |> should.equal(2)
  rule.check.header_must_exist |> should.equal("Set-Cookie")
}

// ============================================================================
// Type Constructor Tests - When
// ============================================================================

pub fn when_wildcard_status_test() {
  let when = When(status: "2xx", method: Get, path: "/users/*")

  when.status |> should.equal("2xx")
  when.method |> should.equal(Get)
  when.path |> should.equal("/users/*")
}

pub fn when_specific_status_test() {
  let when = When(status: "404", method: Get, path: "/missing")

  when.status |> should.equal("404")
}

pub fn when_all_methods_test() {
  let methods = [Get, Post, Put, Patch, Delete, Head, Options]

  methods
  |> list.map(fn(method) {
    When(status: "200", method: method, path: "/test")
  })
  |> list.length
  |> should.equal(7)
}

// ============================================================================
// Type Constructor Tests - RuleCheck
// ============================================================================

pub fn rule_check_empty_test() {
  let check = RuleCheck(
    body_must_not_contain: [],
    body_must_contain: [],
    fields_must_exist: [],
    fields_must_not_exist: [],
    header_must_exist: "",
    header_must_not_exist: "",
  )

  check.body_must_not_contain |> list.length |> should.equal(0)
  check.body_must_contain |> list.length |> should.equal(0)
  check.fields_must_exist |> list.length |> should.equal(0)
  check.fields_must_not_exist |> list.length |> should.equal(0)
}

pub fn rule_check_body_constraints_test() {
  let check = RuleCheck(
    body_must_not_contain: ["error", "exception", "stack"],
    body_must_contain: ["success", "data"],
    fields_must_exist: [],
    fields_must_not_exist: [],
    header_must_exist: "",
    header_must_not_exist: "",
  )

  check.body_must_not_contain |> should.equal(["error", "exception", "stack"])
  check.body_must_contain |> should.equal(["success", "data"])
}

pub fn rule_check_field_constraints_test() {
  let check = RuleCheck(
    body_must_not_contain: [],
    body_must_contain: [],
    fields_must_exist: ["id", "created_at", "updated_at"],
    fields_must_not_exist: ["deleted_at", "internal_notes"],
    header_must_exist: "",
    header_must_not_exist: "",
  )

  check.fields_must_exist |> list.length |> should.equal(3)
  check.fields_must_not_exist |> list.length |> should.equal(2)
}

pub fn rule_check_header_constraints_test() {
  let check = RuleCheck(
    body_must_not_contain: [],
    body_must_contain: [],
    fields_must_exist: [],
    fields_must_not_exist: [],
    header_must_exist: "Content-Type",
    header_must_not_exist: "X-Powered-By",
  )

  check.header_must_exist |> should.equal("Content-Type")
  check.header_must_not_exist |> should.equal("X-Powered-By")
}

// ============================================================================
// Type Constructor Tests - AntiPattern
// ============================================================================

pub fn anti_pattern_simple_test() {
  let bad = json.object([#("password", json.string("secret123"))])
  let good = json.object([#("token", json.string("abc123"))])

  let pattern = AntiPattern(
    name: "password_in_response",
    description: "Passwords should never be in responses",
    bad_example: bad,
    good_example: good,
    why: "Exposing passwords is a security risk",
  )

  pattern.name |> should.equal("password_in_response")
  pattern.why |> should.equal("Exposing passwords is a security risk")
}

pub fn anti_pattern_complex_examples_test() {
  let bad = json.object([
    #("user", json.object([
      #("id", json.int(1)),
      #("password_hash", json.string("$2b$...")),
      #("salt", json.string("random_salt")),
    ])),
  ])

  let good = json.object([
    #("user", json.object([
      #("id", json.int(1)),
      #("email", json.string("user@example.com")),
    ])),
  ])

  let pattern = AntiPattern(
    name: "password_leak",
    description: "Never expose password implementation details",
    bad_example: bad,
    good_example: good,
    why: "Implementation details help attackers",
  )

  pattern.bad_example |> json.to_string
    |> should.equal("{\"user\":{\"id\":1,\"password_hash\":\"$2b$...\",\"salt\":\"random_salt\"}}")
}

// ============================================================================
// Type Constructor Tests - SecurityHints
// ============================================================================

pub fn security_hints_minimal_test() {
  let hints = SecurityHints(
    password_hashing: "",
    jwt_algorithm: "",
    jwt_expiry: "",
    rate_limiting: "",
  )

  hints.password_hashing |> should.equal("")
  hints.jwt_algorithm |> should.equal("")
}

pub fn security_hints_complete_test() {
  let hints = SecurityHints(
    password_hashing: "bcrypt with cost 12",
    jwt_algorithm: "HS256",
    jwt_expiry: "15 minutes",
    rate_limiting: "100 requests per minute",
  )

  hints.password_hashing |> should.equal("bcrypt with cost 12")
  hints.jwt_algorithm |> should.equal("HS256")
  hints.jwt_expiry |> should.equal("15 minutes")
  hints.rate_limiting |> should.equal("100 requests per minute")
}

// ============================================================================
// Type Constructor Tests - EntityHint
// ============================================================================

pub fn entity_hint_minimal_test() {
  let hint = EntityHint(fields: dict.new())

  hint.fields |> dict.size |> should.equal(0)
}

pub fn entity_hint_with_fields_test() {
  let fields = dict.from_list([
    #("id", "UUID v4"),
    #("email", "valid email format"),
    #("created_at", "ISO 8601 timestamp"),
  ])

  let hint = EntityHint(fields: fields)

  hint.fields |> dict.size |> should.equal(3)
  hint.fields |> dict.get("email") |> should.be_ok
    |> should.equal("valid email format")
}

// ============================================================================
// Type Constructor Tests - ImplementationHints
// ============================================================================

pub fn implementation_hints_empty_test() {
  let hints = ImplementationHints(suggested_stack: [])

  hints.suggested_stack |> list.length |> should.equal(0)
}

pub fn implementation_hints_with_stack_test() {
  let hints = ImplementationHints(
    suggested_stack: ["Node.js", "Express", "PostgreSQL", "Redis"],
  )

  hints.suggested_stack |> list.length |> should.equal(4)
  hints.suggested_stack |> should.equal(["Node.js", "Express", "PostgreSQL", "Redis"])
}

// ============================================================================
// Type Constructor Tests - AIHints
// ============================================================================

pub fn ai_hints_minimal_test() {
  let hints = AIHints(
    implementation: ImplementationHints(suggested_stack: []),
    entities: dict.new(),
    security: SecurityHints(
      password_hashing: "",
      jwt_algorithm: "",
      jwt_expiry: "",
      rate_limiting: "",
    ),
    pitfalls: [],
  )

  hints.pitfalls |> list.length |> should.equal(0)
  hints.entities |> dict.size |> should.equal(0)
}

pub fn ai_hints_complete_test() {
  let entities = dict.from_list([
    #("User", EntityHint(fields: dict.from_list([
      #("id", "UUID"),
      #("email", "string"),
    ]))),
    #("Post", EntityHint(fields: dict.from_list([
      #("id", "UUID"),
      #("title", "string"),
    ]))),
  ])

  let hints = AIHints(
    implementation: ImplementationHints(
      suggested_stack: ["Python", "FastAPI", "PostgreSQL"],
    ),
    entities: entities,
    security: SecurityHints(
      password_hashing: "argon2",
      jwt_algorithm: "RS256",
      jwt_expiry: "30 minutes",
      rate_limiting: "1000/hour",
    ),
    pitfalls: ["Avoid N+1 queries", "Use database transactions"],
  )

  hints.entities |> dict.size |> should.equal(2)
  hints.pitfalls |> list.length |> should.equal(2)
  hints.implementation.suggested_stack |> should.equal(["Python", "FastAPI", "PostgreSQL"])
}

// ============================================================================
// Type Constructor Tests - Spec
// ============================================================================

pub fn spec_minimal_test() {
  let spec = Spec(
    name: "Test API",
    description: "A test API specification",
    audience: "developers",
    version: "1.0.0",
    success_criteria: [],
    config: Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
    features: [],
    rules: [],
    anti_patterns: [],
    ai_hints: AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
    ),
  )

  spec.name |> should.equal("Test API")
  spec.version |> should.equal("1.0.0")
  spec.features |> list.length |> should.equal(0)
}

pub fn spec_complete_test() {
  let feature = Feature(
    name: "User Management",
    description: "User CRUD operations",
    behaviors: [],
  )

  let rule = Rule(
    name: "auth_required",
    description: "All endpoints require auth",
    when: When(status: "*", method: Get, path: "/api/*"),
    check: RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "Authorization",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )

  let anti_pattern = AntiPattern(
    name: "plain_passwords",
    description: "Never use plain text passwords",
    bad_example: json.object([#("password", json.string("plain"))]),
    good_example: json.object([#("password_hash", json.string("hashed"))]),
    why: "Security risk",
  )

  let spec = Spec(
    name: "User API",
    description: "User management API",
    audience: "web developers",
    version: "2.1.0",
    success_criteria: ["All users can be created", "Users can be retrieved"],
    config: Config(
      base_url: "https://api.example.com",
      timeout_ms: 10000,
      headers: dict.from_list([#("X-API-Key", "test123")]),
    ),
    features: [feature],
    rules: [rule],
    anti_patterns: [anti_pattern],
    ai_hints: AIHints(
      implementation: ImplementationHints(suggested_stack: ["Go", "PostgreSQL"]),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "bcrypt",
        jwt_algorithm: "HS256",
        jwt_expiry: "1 hour",
        rate_limiting: "100/min",
      ),
      pitfalls: ["Validate all inputs"],
    ),
  )

  spec.name |> should.equal("User API")
  spec.version |> should.equal("2.1.0")
  spec.features |> list.length |> should.equal(1)
  spec.rules |> list.length |> should.equal(1)
  spec.anti_patterns |> list.length |> should.equal(1)
  spec.success_criteria |> list.length |> should.equal(2)
}

// ============================================================================
// Integration Tests - Complex Type Compositions
// ============================================================================

pub fn full_behavior_construction_test() {
  // Test building a complete behavior with all features
  let behavior = Behavior(
    name: "create_user",
    intent: "Create a new user account",
    notes: "Requires admin privileges",
    requires: ["authenticate_admin"],
    tags: ["auth", "user", "write"],
    request: Request(
      method: Post,
      path: "/api/users",
      headers: dict.from_list([
        #("Content-Type", "application/json"),
        #("Authorization", "Bearer {{admin_token}}"),
      ]),
      query: dict.new(),
      body: json.object([
        #("email", json.string("test@example.com")),
        #("name", json.string("Test User")),
      ]),
    ),
    response: Response(
      status: 201,
      example: json.object([
        #("id", json.int(123)),
        #("email", json.string("test@example.com")),
        #("created_at", json.string("2024-01-01T00:00:00Z")),
      ]),
      checks: dict.from_list([
        #("id_exists", Check(rule: "id exists", why: "User must have ID")),
        #("email_matches", Check(
          rule: "email == request.body.email",
          why: "Response email must match request",
        )),
      ]),
      headers: dict.from_list([
        #("Location", "/api/users/123"),
        #("Content-Type", "application/json"),
      ]),
    ),
    captures: dict.from_list([#("user_id", "response.id")]),
  )

  behavior.name |> should.equal("create_user")
  behavior.requires |> list.length |> should.equal(1)
  behavior.tags |> list.length |> should.equal(3)
  behavior.request.method |> should.equal(Post)
  behavior.response.status |> should.equal(201)
  behavior.response.checks |> dict.size |> should.equal(2)
  behavior.captures |> dict.size |> should.equal(1)
}
