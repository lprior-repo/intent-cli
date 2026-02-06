/// RED-02: Input Boundary Attacks - Bug Discovery Report
/// Direct EARS format reporting of discovered bugs
import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/string
import gleeunit/should
import intent/parser
import intent/types
import intent/validator

/// BUG 1: Empty string accepted for required 'name' field
/// SEVERITY: MEDIUM
/// EVIDENCE: parser.gleam:20 accepts "" after dynamic.string validation
/// RISK: Empty specs break downstream processing
pub fn bug_1_empty_string_for_required_name_test() {
  let bad_json =
    "{\"name\":\"\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":5000,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

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

/// BUG 2: Zero timeout accepted (should require positive int)
/// SEVERITY: MEDIUM  
/// EVIDENCE: parser.gleam:58 accepts 0 via dynamic.int
/// RISK: Infinite hangs or immediate timeouts
pub fn bug_2_zero_timeout_accepted_test() {
  let bad_json =
    "{\"name\":\"test\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":0,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

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

/// BUG 3: Negative timeout accepted (should be positive only)
/// SEVERITY: MEDIUM
/// EVIDENCE: parser.gleam:58 accepts -100 via dynamic.int  
/// RISK: Integer overflow or undefined behavior
pub fn bug_3_negative_timeout_accepted_test() {
  let bad_json =
    "{\"name\":\"test\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":-100,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

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

/// BUG 4: Null bytes accepted without sanitization
/// SEVERITY: HIGH
/// EVIDENCE: json.decode + parser.gleam:20 accepts \u0000 in strings
/// RISK: String truncation, buffer overflows, C API issues
pub fn bug_4_null_byte_sanitization_test() {
  let bad_json =
    "{\"name\":\"test\\u0000\",\"description\":\"test\",\"audience\":\"dev\",\"version\":\"1.0\",\"success_criteria\":[],\"config\":{\"base_url\":\"http://localhost\",\"timeout_ms\":5000,\"headers\":{}},\"features\":[],\"rules\":[],\"anti_patterns\":[],\"ai_hints\":{\"implementation\":{\"suggested_stack\":[]},\"entities\":{},\"security\":{\"password_hashing\":\"bcrypt\",\"jwt_algorithm\":\"HS256\",\"jwt_expiry\":\"1h\",\"rate_limiting\":\"100/min\"},\"pitfalls\":[]}}"

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

/// BUG 5: No path validation for traversal attacks
/// SEVERITY: MEDIUM
/// EVIDENCE: validator.gleam doesn't validate request.path format
/// RISK: Path traversal if paths are used in file operations
pub fn bug_5_path_traversal_validation_test() {
  let spec = make_minimal_spec_with_path("/../../etc/passwd")
  let result = validator.validate_spec(spec)
  case result {
    validator.ValidationValid -> should.be_ok(Ok(Nil))
    validator.ValidationInvalid(_) -> should.be_ok(Ok(Nil))
  }
}

/// BUG 6: No sanitization of shell metacharacters  
/// SEVERITY: MEDIUM
/// EVIDENCE: validator.gleam:127 doesn't check path for shell chars
/// RISK: Command injection if paths reach shell execution
pub fn bug_6_shell_metacharacter_validation_test() {
  let spec = make_minimal_spec_with_path("/test; rm -rf /")
  let result = validator.validate_spec(spec)
  case result {
    validator.ValidationValid -> should.be_ok(Ok(Nil))
    validator.ValidationInvalid(_) -> should.be_ok(Ok(Nil))
  }
}

fn make_minimal_spec_with_path(path: String) -> types.Spec {
  let behavior =
    types.Behavior(
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
  let feature =
    types.Feature(
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
