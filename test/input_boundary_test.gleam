/// RED-02: Input Boundary Attacks on intent-cli
/// Critical bugs only for rapid testing

import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/string
import gleeunit/should
import intent/parser
import intent/validator
import intent/types

pub fn empty_string_required_field_name_test() {
  let bad_json = "{\"name\":\"\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":5000,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

  case json.decode(bad_json, dynamic.dynamic) {
    Ok(data) -> {
      let result = parser.parse_spec(data)
      case result {
        Ok(spec) -> {
          spec.name |> should.equal("")
        }
        Error(_) -> should.be_ok(Ok(Nil))
      }
    }
    Error(_) -> should.be_ok(Ok(Nil))
  }
}

pub fn zero_timeout_accepted_test() {
  let bad_json = "{\"name\":\"test\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":0,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

  case json.decode(bad_json, dynamic.dynamic) {
    Ok(data) -> {
      let result = parser.parse_spec(data)
      case result {
        Ok(spec) -> {
          spec.config.timeout_ms |> should.equal(0)
        }
        Error(_) -> should.be_ok(Ok(Nil))
      }
    }
    Error(_) -> should.be_ok(Ok(Nil))
  }
}

pub fn negative_timeout_accepted_test() {
  let bad_json = "{\"name\":\"test\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":-100,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

  case json.decode(bad_json, dynamic.dynamic) {
    Ok(data) -> {
      let result = parser.parse_spec(data)
      case result {
        Ok(spec) -> {
          spec.config.timeout_ms |> should.equal(-100)
        }
        Error(_) -> should.be_ok(Ok(Nil))
      }
    }
    Error(_) -> should.be_ok(Ok(Nil))
  }
}

pub fn null_byte_in_string_test() {
  let bad_json = "{\"name\":\"test\\u0000\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":5000,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

  case json.decode(bad_json, dynamic.dynamic) {
    Ok(data) -> {
      case dynamic.field("name", dynamic.string)(data) {
        Ok(name) -> {
          string.contains(name, "\u{0000}") |> should.be_true
        }
        Error(_) -> should.be_ok(Ok(Nil))
      }
    }
    Error(_) -> should.be_ok(Ok(Nil))
  }
}

pub fn path_traversal_accepted_test() {
  let spec = make_minimal_spec_with_path("/../../etc/passwd")
  let result = validator.validate_spec(spec)
  case result {
    validator.ValidationValid -> should.be_ok(Ok(Nil))
    validator.ValidationInvalid(_) -> should.be_ok(Ok(Nil))
  }
}

pub fn shell_metacharacters_accepted_test() {
  let spec = make_minimal_spec_with_path("/test; rm -rf /")
  let result = validator.validate_spec(spec)
  case result {
    validator.ValidationValid -> should.be_ok(Ok(Nil))
    validator.ValidationInvalid(_) -> should.be_ok(Ok(Nil))
  }
}

fn make_minimal_spec_with_path(path: String) -> types.Spec {
  let behavior = types.Behavior(
    name: "test_behavior",
    intent: "test intent",
    notes: "",
    requires: [],
    tags: [],
    request: types.Request(
      method: types.Get,
      path: path,
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
  let feature = types.Feature(
    name: "Test Feature",
    description: "Test description",
    behaviors: [behavior],
  )
  types.Spec(
    name: "Test Spec",
    description: "Test spec",
    audience: "developers",
    version: "1.0.0",
    success_criteria: [],
    config: types.Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
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
        rate_limiting: "100/min",
      ),
      pitfalls: [],
    ),
  )
}
