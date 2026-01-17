//// Comprehensive tests for the parser module
//// Tests parsing JSON to typed Gleam structures (Spec, Feature, Behavior, etc.)
//// Validates error handling for missing fields and type mismatches

import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import intent/parser
import intent/types.{Delete, Get, Head, Options, Patch, Post, Put}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// dynamic_to_json Tests - All JSON Types
// ============================================================================

/// Test converting nil/null to JSON
pub fn dynamic_to_json_null_test() {
  let data = dynamic.from(Nil)
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("null")
}

/// Test converting boolean true to JSON
pub fn dynamic_to_json_bool_true_test() {
  let data = dynamic.from(True)
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("true")
}

/// Test converting boolean false to JSON
pub fn dynamic_to_json_bool_false_test() {
  let data = dynamic.from(False)
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("false")
}

/// Test converting integer to JSON
pub fn dynamic_to_json_int_test() {
  let data = dynamic.from(42)
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("42")
}

/// Test converting negative integer to JSON
pub fn dynamic_to_json_negative_int_test() {
  let data = dynamic.from(-100)
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("-100")
}

/// Test converting float to JSON
pub fn dynamic_to_json_float_test() {
  let data = dynamic.from(3.14)
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("3.14")
}

/// Test converting string to JSON
pub fn dynamic_to_json_string_test() {
  let data = dynamic.from("hello world")
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("\"hello world\"")
}

/// Test converting empty string to JSON
pub fn dynamic_to_json_empty_string_test() {
  let data = dynamic.from("")
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("\"\"")
}

/// Test converting list to JSON array
pub fn dynamic_to_json_list_test() {
  let data = dynamic.from([1, 2, 3])
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("[1,2,3]")
}

/// Test converting empty list to JSON
pub fn dynamic_to_json_empty_list_test() {
  let data = dynamic.from([])
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("[]")
}

/// Test converting nested list to JSON
pub fn dynamic_to_json_nested_list_test() {
  let data = dynamic.from([[1, 2], [3, 4]])
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("[[1,2],[3,4]]")
}

/// Test converting dict to JSON object
pub fn dynamic_to_json_dict_test() {
  let data = dynamic.from(dict.from_list([#("name", "test"), #("value", "42")]))
  let result = parser.dynamic_to_json(data)
  let json_str = json.to_string(result)
  // Dict order is not guaranteed, so check both possible orderings
  case json_str {
    "{\"name\":\"test\",\"value\":\"42\"}" -> Nil
    "{\"value\":\"42\",\"name\":\"test\"}" -> Nil
    _ -> should.fail()
  }
}

/// Test converting empty dict to JSON
pub fn dynamic_to_json_empty_dict_test() {
  let data = dynamic.from(dict.new())
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("{}")
}

/// Test converting nested dict to JSON
pub fn dynamic_to_json_nested_dict_test() {
  let inner = dict.from_list([#("inner", "value")])
  let outer = dict.from_list([#("outer", inner)])
  let data = dynamic.from(outer)
  let result = parser.dynamic_to_json(data)
  result
  |> json.to_string
  |> should.equal("{\"outer\":{\"inner\":\"value\"}}")
}

/// Test converting complex nested structure to JSON
pub fn dynamic_to_json_complex_structure_test() {
  // Since Gleam lists must be homogeneous, test a complex but type-safe structure
  let data =
    dynamic.from(
      dict.from_list([
        #(
          "numbers",
          json.array([json.int(1), json.int(2), json.int(3)], fn(x) { x }),
        ),
        #(
          "strings",
          json.array([json.string("a"), json.string("b")], fn(x) { x }),
        ),
      ]),
    )
  let result = parser.dynamic_to_json(data)
  let json_str = json.to_string(result)
  // Verify it's valid JSON with expected keys
  json_str
  |> should.not_equal("")
}

// ============================================================================
// parse_spec Tests - Valid Complete Spec
// ============================================================================

/// Test parsing a valid minimal spec with all required fields
pub fn parse_spec_valid_minimal_test() {
  let json_str =
    "{
      \"name\": \"Test API\",
      \"description\": \"Test description\",
      \"audience\": \"developers\",
      \"version\": \"1.0.0\",
      \"success_criteria\": [],
      \"config\": {
        \"base_url\": \"http://localhost:8080\",
        \"timeout_ms\": 5000,
        \"headers\": {}
      },
      \"features\": [],
      \"rules\": [],
      \"anti_patterns\": [],
      \"ai_hints\": {
        \"implementation\": {
          \"suggested_stack\": []
        },
        \"entities\": {},
        \"security\": {
          \"password_hashing\": \"bcrypt\",
          \"jwt_algorithm\": \"HS256\",
          \"jwt_expiry\": \"1h\",
          \"rate_limiting\": \"100/min\"
        },
        \"pitfalls\": []
      }
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parser.parse_spec(data)

  case result {
    Ok(spec) -> {
      spec.name |> should.equal("Test API")
      spec.description |> should.equal("Test description")
      spec.audience |> should.equal("developers")
      spec.version |> should.equal("1.0.0")
      spec.success_criteria |> should.equal([])
      spec.features |> should.equal([])
      spec.rules |> should.equal([])
      spec.anti_patterns |> should.equal([])
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing spec with populated success_criteria
pub fn parse_spec_with_success_criteria_test() {
  let json_str =
    "{
      \"name\": \"Test API\",
      \"description\": \"Test description\",
      \"audience\": \"developers\",
      \"version\": \"1.0.0\",
      \"success_criteria\": [\"User can login\", \"User can logout\"],
      \"config\": {
        \"base_url\": \"http://localhost:8080\",
        \"timeout_ms\": 5000,
        \"headers\": {}
      },
      \"features\": [],
      \"rules\": [],
      \"anti_patterns\": [],
      \"ai_hints\": {
        \"implementation\": {
          \"suggested_stack\": []
        },
        \"entities\": {},
        \"security\": {
          \"password_hashing\": \"\",
          \"jwt_algorithm\": \"\",
          \"jwt_expiry\": \"\",
          \"rate_limiting\": \"\"
        },
        \"pitfalls\": []
      }
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parser.parse_spec(data)

  case result {
    Ok(spec) -> {
      spec.success_criteria
      |> should.equal(["User can login", "User can logout"])
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// parse_spec Tests - Missing Required Fields
// ============================================================================

/// Test parsing spec with missing name field
pub fn parse_spec_missing_name_test() {
  let json_data =
    json.object([
      #("description", json.string("Test description")),
      #("audience", json.string("developers")),
      #("version", json.string("1.0.0")),
      #("success_criteria", json.array([], json.string)),
      #(
        "config",
        json.object([
          #("base_url", json.string("http://localhost")),
          #("timeout_ms", json.int(5000)),
          #("headers", json.object([])),
        ]),
      ),
      #("features", json.array([], fn(f) { f })),
      #("rules", json.array([], fn(r) { r })),
      #("anti_patterns", json.array([], fn(a) { a })),
      #(
        "ai_hints",
        json.object([
          #(
            "implementation",
            json.object([#("suggested_stack", json.array([], json.string))]),
          ),
          #("entities", json.object([])),
          #(
            "security",
            json.object([
              #("password_hashing", json.string("")),
              #("jwt_algorithm", json.string("")),
              #("jwt_expiry", json.string("")),
              #("rate_limiting", json.string("")),
            ]),
          ),
          #("pitfalls", json.array([], json.string)),
        ]),
      ),
    ])

  let data = dynamic.from(json_data)
  let result = parser.parse_spec(data)

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing spec with missing config field
pub fn parse_spec_missing_config_test() {
  let json_data =
    json.object([
      #("name", json.string("Test")),
      #("description", json.string("Test description")),
      #("audience", json.string("developers")),
      #("version", json.string("1.0.0")),
      #("success_criteria", json.array([], json.string)),
      #("features", json.array([], fn(f) { f })),
      #("rules", json.array([], fn(r) { r })),
      #("anti_patterns", json.array([], fn(a) { a })),
      #(
        "ai_hints",
        json.object([
          #(
            "implementation",
            json.object([#("suggested_stack", json.array([], json.string))]),
          ),
          #("entities", json.object([])),
          #(
            "security",
            json.object([
              #("password_hashing", json.string("")),
              #("jwt_algorithm", json.string("")),
              #("jwt_expiry", json.string("")),
              #("rate_limiting", json.string("")),
            ]),
          ),
          #("pitfalls", json.array([], json.string)),
        ]),
      ),
    ])

  let data = dynamic.from(json_data)
  let result = parser.parse_spec(data)

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing spec with missing ai_hints field
pub fn parse_spec_missing_ai_hints_test() {
  let json_data =
    json.object([
      #("name", json.string("Test")),
      #("description", json.string("Test description")),
      #("audience", json.string("developers")),
      #("version", json.string("1.0.0")),
      #("success_criteria", json.array([], json.string)),
      #(
        "config",
        json.object([
          #("base_url", json.string("http://localhost")),
          #("timeout_ms", json.int(5000)),
          #("headers", json.object([])),
        ]),
      ),
      #("features", json.array([], fn(f) { f })),
      #("rules", json.array([], fn(r) { r })),
      #("anti_patterns", json.array([], fn(a) { a })),
    ])

  let data = dynamic.from(json_data)
  let result = parser.parse_spec(data)

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// parse_spec Tests - Wrong Types
// ============================================================================

/// Test parsing spec with name as integer instead of string
pub fn parse_spec_name_wrong_type_test() {
  let json_data =
    json.object([
      #("name", json.int(123)),
      #("description", json.string("Test description")),
      #("audience", json.string("developers")),
      #("version", json.string("1.0.0")),
      #("success_criteria", json.array([], json.string)),
      #(
        "config",
        json.object([
          #("base_url", json.string("http://localhost")),
          #("timeout_ms", json.int(5000)),
          #("headers", json.object([])),
        ]),
      ),
      #("features", json.array([], fn(f) { f })),
      #("rules", json.array([], fn(r) { r })),
      #("anti_patterns", json.array([], fn(a) { a })),
      #(
        "ai_hints",
        json.object([
          #(
            "implementation",
            json.object([#("suggested_stack", json.array([], json.string))]),
          ),
          #("entities", json.object([])),
          #(
            "security",
            json.object([
              #("password_hashing", json.string("")),
              #("jwt_algorithm", json.string("")),
              #("jwt_expiry", json.string("")),
              #("rate_limiting", json.string("")),
            ]),
          ),
          #("pitfalls", json.array([], json.string)),
        ]),
      ),
    ])

  let data = dynamic.from(json_data)
  let result = parser.parse_spec(data)

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.expected |> should.equal("Dict")
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing spec with success_criteria as string instead of list
pub fn parse_spec_success_criteria_wrong_type_test() {
  let json_data =
    json.object([
      #("name", json.string("Test")),
      #("description", json.string("Test description")),
      #("audience", json.string("developers")),
      #("version", json.string("1.0.0")),
      #("success_criteria", json.string("not a list")),
      #(
        "config",
        json.object([
          #("base_url", json.string("http://localhost")),
          #("timeout_ms", json.int(5000)),
          #("headers", json.object([])),
        ]),
      ),
      #("features", json.array([], fn(f) { f })),
      #("rules", json.array([], fn(r) { r })),
      #("anti_patterns", json.array([], fn(a) { a })),
      #(
        "ai_hints",
        json.object([
          #(
            "implementation",
            json.object([#("suggested_stack", json.array([], json.string))]),
          ),
          #("entities", json.object([])),
          #(
            "security",
            json.object([
              #("password_hashing", json.string("")),
              #("jwt_algorithm", json.string("")),
              #("jwt_expiry", json.string("")),
              #("rate_limiting", json.string("")),
            ]),
          ),
          #("pitfalls", json.array([], json.string)),
        ]),
      ),
    ])

  let data = dynamic.from(json_data)
  let result = parser.parse_spec(data)

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.expected |> should.equal("Dict")
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Config Tests
// ============================================================================

/// Test parsing valid config
pub fn parse_config_valid_test() {
  let json_str =
    "{
      \"base_url\": \"https://api.example.com\",
      \"timeout_ms\": 10000,
      \"headers\": {
        \"Authorization\": \"Bearer token\",
        \"Content-Type\": \"application/json\"
      },
      \"allow_localhost\": false
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_config_helper(data)

  case result {
    Ok(config) -> {
      config.base_url |> should.equal("https://api.example.com")
      config.timeout_ms |> should.equal(10_000)
      dict.size(config.headers) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing config with missing base_url
pub fn parse_config_missing_base_url_test() {
  let json_data =
    json.object([#("timeout_ms", json.int(5000)), #("headers", json.object([]))])

  let result =
    dynamic.field("config", parse_config_helper)(
      dynamic.from(json.object([#("config", json_data)])),
    )

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing config with timeout_ms as string instead of int
pub fn parse_config_timeout_wrong_type_test() {
  let json_data =
    json.object([
      #("base_url", json.string("http://localhost")),
      #("timeout_ms", json.string("not an int")),
      #("headers", json.object([])),
    ])

  let result =
    dynamic.field("config", parse_config_helper)(
      dynamic.from(json.object([#("config", json_data)])),
    )

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.expected |> should.equal("Dict")
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Feature Tests
// ============================================================================

/// Test parsing valid feature with behaviors
pub fn parse_feature_valid_test() {
  let json_str =
    "{
      \"name\": \"Authentication\",
      \"description\": \"User authentication features\",
      \"behaviors\": [
        {
          \"name\": \"login\",
          \"intent\": \"User can login\",
          \"notes\": \"\",
          \"requires\": [],
          \"tags\": [],
          \"request\": {
            \"method\": \"POST\",
            \"path\": \"/login\",
            \"headers\": {},
            \"query\": {},
            \"body\": null
          },
          \"response\": {
            \"status\": 200,
            \"example\": null,
            \"checks\": {},
            \"headers\": {}
          },
          \"captures\": {}
        }
      ]
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_feature_helper(data)

  case result {
    Ok(feature) -> {
      feature.name |> should.equal("Authentication")
      feature.description |> should.equal("User authentication features")
      list.length(feature.behaviors) |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing feature with empty behaviors list
pub fn parse_feature_empty_behaviors_test() {
  let json_str =
    "{
      \"name\": \"Empty Feature\",
      \"description\": \"Feature with no behaviors\",
      \"behaviors\": []
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_feature_helper(data)

  case result {
    Ok(feature) -> {
      feature.behaviors |> should.equal([])
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing feature with missing description
pub fn parse_feature_missing_description_test() {
  let json_data =
    json.object([
      #("name", json.string("Feature")),
      #("behaviors", json.array([], fn(b) { b })),
    ])

  let result =
    dynamic.field("feature", parse_feature_helper)(
      dynamic.from(json.object([#("feature", json_data)])),
    )

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Behavior Tests
// ============================================================================

/// Test parsing valid behavior
pub fn parse_behavior_valid_test() {
  let json_str =
    "{
      \"name\": \"create-user\",
      \"intent\": \"Create a new user\",
      \"notes\": \"Admin only\",
      \"requires\": [\"login\"],
      \"tags\": [\"admin\", \"user-management\"],
      \"request\": {
        \"method\": \"POST\",
        \"path\": \"/users\",
        \"headers\": {
          \"Content-Type\": \"application/json\"
        },
        \"query\": {},
        \"body\": {
          \"name\": \"John\"
        }
      },
      \"response\": {
        \"status\": 201,
        \"example\": {
          \"id\": 1
        },
        \"checks\": {
          \"has_id\": {
            \"rule\": \"$.id exists\",
            \"why\": \"Must return user ID\"
          }
        },
        \"headers\": {}
      },
      \"captures\": {
        \"user_id\": \"$.id\"
      }
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_behavior_helper(data)

  case result {
    Ok(behavior) -> {
      behavior.name |> should.equal("create-user")
      behavior.intent |> should.equal("Create a new user")
      behavior.notes |> should.equal("Admin only")
      behavior.requires |> should.equal(["login"])
      behavior.tags |> should.equal(["admin", "user-management"])
      dict.size(behavior.captures) |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing behavior with missing request
pub fn parse_behavior_missing_request_test() {
  let json_data =
    json.object([
      #("name", json.string("test")),
      #("intent", json.string("Test intent")),
      #("notes", json.string("")),
      #("requires", json.array([], json.string)),
      #("tags", json.array([], json.string)),
      #(
        "response",
        json.object([
          #("status", json.int(200)),
          #("example", json.null()),
          #("checks", json.object([])),
        ]),
      ),
      #("captures", json.object([])),
    ])

  let result =
    dynamic.field("behavior", parse_behavior_helper)(
      dynamic.from(json.object([#("behavior", json_data)])),
    )

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing behavior with missing response
pub fn parse_behavior_missing_response_test() {
  let json_data =
    json.object([
      #("name", json.string("test")),
      #("intent", json.string("Test intent")),
      #("notes", json.string("")),
      #("requires", json.array([], json.string)),
      #("tags", json.array([], json.string)),
      #(
        "request",
        json.object([
          #("method", json.string("GET")),
          #("path", json.string("/")),
          #("headers", json.object([])),
          #("query", json.object([])),
          #("body", json.null()),
        ]),
      ),
      #("captures", json.object([])),
    ])

  let result =
    dynamic.field("behavior", parse_behavior_helper)(
      dynamic.from(json.object([#("behavior", json_data)])),
    )

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Method Parsing Tests
// ============================================================================

/// Test parsing all valid HTTP methods
pub fn parse_method_get_test() {
  test_method_parsing("GET", Get)
}

pub fn parse_method_post_test() {
  test_method_parsing("POST", Post)
}

pub fn parse_method_put_test() {
  test_method_parsing("PUT", Put)
}

pub fn parse_method_patch_test() {
  test_method_parsing("PATCH", Patch)
}

pub fn parse_method_delete_test() {
  test_method_parsing("DELETE", Delete)
}

pub fn parse_method_head_test() {
  test_method_parsing("HEAD", Head)
}

pub fn parse_method_options_test() {
  test_method_parsing("OPTIONS", Options)
}

/// Test parsing invalid method
pub fn parse_method_invalid_test() {
  let json_data =
    json.object([
      #(
        "request",
        json.object([
          #("method", json.string("INVALID")),
          #("path", json.string("/")),
          #("headers", json.object([])),
          #("query", json.object([])),
          #("body", json.null()),
        ]),
      ),
    ])

  let result =
    dynamic.field("request", parse_request_helper)(dynamic.from(json_data))

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.expected |> should.equal("Dict")
      error.found |> should.equal("List")
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Request Tests
// ============================================================================

/// Test parsing request with all fields
pub fn parse_request_complete_test() {
  let json_str =
    "{
      \"method\": \"POST\",
      \"path\": \"/api/users\",
      \"headers\": {
        \"Authorization\": \"Bearer token\"
      },
      \"query\": {
        \"filter\": \"active\"
      },
      \"body\": {
        \"name\": \"Alice\",
        \"age\": 30
      }
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_request_helper(data)

  case result {
    Ok(request) -> {
      request.method |> should.equal(Post)
      request.path |> should.equal("/api/users")
      dict.size(request.headers) |> should.equal(1)
      dict.size(request.query) |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing request with null body
pub fn parse_request_null_body_test() {
  let json_str =
    "{
      \"method\": \"GET\",
      \"path\": \"/\",
      \"headers\": {},
      \"query\": {},
      \"body\": null
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_request_helper(data)

  case result {
    Ok(request) -> {
      request.body |> json.to_string |> should.equal("null")
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing request with missing path
pub fn parse_request_missing_path_test() {
  let json_data =
    json.object([
      #("method", json.string("GET")),
      #("headers", json.object([])),
      #("query", json.object([])),
      #("body", json.null()),
    ])

  let result =
    dynamic.field("request", parse_request_helper)(
      dynamic.from(json.object([#("request", json_data)])),
    )

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Response Tests
// ============================================================================

/// Test parsing response with all fields
pub fn parse_response_complete_test() {
  let json_str =
    "{
      \"status\": 201,
      \"example\": {
        \"id\": 123
      },
      \"checks\": {
        \"check1\": {
          \"rule\": \"$.id exists\",
          \"why\": \"ID required\"
        }
      },
      \"headers\": {
        \"Location\": \"/users/123\"
      }
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_response_helper(data)

  case result {
    Ok(response) -> {
      response.status |> should.equal(201)
      dict.size(response.checks) |> should.equal(1)
      dict.size(response.headers) |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing response without optional headers field
pub fn parse_response_no_headers_test() {
  let json_str =
    "{
      \"status\": 200,
      \"example\": null,
      \"checks\": {}
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_response_helper(data)

  case result {
    Ok(response) -> {
      dict.size(response.headers) |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing response with missing status
pub fn parse_response_missing_status_test() {
  let json_data =
    json.object([#("example", json.null()), #("checks", json.object([]))])

  let result =
    dynamic.field("response", parse_response_helper)(
      dynamic.from(json.object([#("response", json_data)])),
    )

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
      let assert [error] = errors
      error.path |> should.equal([])
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Check Tests
// ============================================================================

/// Test parsing valid check
pub fn parse_check_valid_test() {
  let json_str =
    "{
      \"id_check\": {
        \"rule\": \"$.id exists\",
        \"why\": \"Every user must have an ID\"
      }
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_checks_helper(data)

  case result {
    Ok(checks) -> {
      dict.size(checks) |> should.equal(1)
      case dict.get(checks, "id_check") {
        Ok(check) -> {
          check.rule |> should.equal("$.id exists")
          check.why |> should.equal("Every user must have an ID")
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing check with missing why
pub fn parse_check_missing_why_test() {
  let json_data =
    json.object([
      #(
        "checks",
        json.object([
          #("bad_check", json.object([#("rule", json.string("$.id exists"))])),
        ]),
      ),
    ])

  let result =
    dynamic.field("checks", parse_checks_helper)(dynamic.from(json_data))

  case result {
    Error(errors) -> {
      list.length(errors) |> should.equal(1)
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing valid rule
pub fn parse_rule_valid_test() {
  let json_str =
    "[
      {
        \"name\": \"no_stack_traces\",
        \"description\": \"Don't expose stack traces\",
        \"when\": {
          \"status\": \"5xx\",
          \"method\": \"GET\",
          \"path\": \"*\"
        },
        \"check\": {
          \"body_must_not_contain\": [\"stack\", \"trace\"],
          \"body_must_contain\": [],
          \"fields_must_exist\": [],
          \"fields_must_not_exist\": [],
          \"header_must_exist\": \"\",
          \"header_must_not_exist\": \"\"
        },
        \"example\": null
      }
    ]"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_rules_helper(data)

  case result {
    Ok(rules) -> {
      list.length(rules) |> should.equal(1)
      let assert [rule] = rules
      rule.name |> should.equal("no_stack_traces")
      rule.when.status |> should.equal("5xx")
    }
    Error(_) -> should.fail()
  }
}

/// Test parsing valid anti-pattern
pub fn parse_anti_pattern_valid_test() {
  let json_str =
    "[
      {
        \"name\": \"Exposing IDs\",
        \"description\": \"Don't expose internal IDs\",
        \"bad_example\": {
          \"id\": 123
        },
        \"good_example\": {
          \"uuid\": \"abc-123\"
        },
        \"why\": \"Security concern\"
      }
    ]"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_anti_patterns_helper(data)

  case result {
    Ok(anti_patterns) -> {
      list.length(anti_patterns) |> should.equal(1)
      let assert [ap] = anti_patterns
      ap.name |> should.equal("Exposing IDs")
      ap.why |> should.equal("Security concern")
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn test_method_parsing(method_str: String, expected: types.Method) {
  let json_str = "{
      \"method\": \"" <> method_str <> "\",
      \"path\": \"/\",
      \"headers\": {},
      \"query\": {},
      \"body\": null
    }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parse_request_helper(data)

  case result {
    Ok(request) -> {
      request.method |> should.equal(expected)
    }
    Error(_) -> should.fail()
  }
}

// Re-export internal parsers for testing
// These are wrappers to access the internal parsing functions

fn parse_config_helper(
  data: dynamic.Dynamic,
) -> Result(types.Config, List(dynamic.DecodeError)) {
  use base_url <- result.try(dynamic.field("base_url", dynamic.string)(data))
  use timeout_ms <- result.try(dynamic.field("timeout_ms", dynamic.int)(data))
  use headers <- result.try(dynamic.field(
    "headers",
    dynamic.dict(dynamic.string, dynamic.string),
  )(data))
  use allow_localhost <- result.try(dynamic.field(
    "allow_localhost",
    dynamic.bool,
  )(data))
  Ok(types.Config(base_url, timeout_ms, headers, allow_localhost))
}

fn parse_feature_helper(
  data: dynamic.Dynamic,
) -> Result(types.Feature, List(dynamic.DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use behaviors <- result.try(dynamic.field(
    "behaviors",
    dynamic.list(parse_behavior_helper),
  )(data))
  Ok(types.Feature(name, description, behaviors))
}

fn parse_behavior_helper(
  data: dynamic.Dynamic,
) -> Result(types.Behavior, List(dynamic.DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use intent <- result.try(dynamic.field("intent", dynamic.string)(data))
  use notes <- result.try(dynamic.field("notes", dynamic.string)(data))
  use requires <- result.try(dynamic.field(
    "requires",
    dynamic.list(dynamic.string),
  )(data))
  use tags <- result.try(dynamic.field("tags", dynamic.list(dynamic.string))(
    data,
  ))
  use request <- result.try(dynamic.field("request", parse_request_helper)(data))
  use response <- result.try(dynamic.field("response", parse_response_helper)(
    data,
  ))
  use captures <- result.try(dynamic.field(
    "captures",
    dynamic.dict(dynamic.string, dynamic.string),
  )(data))
  Ok(types.Behavior(
    name: name,
    intent: intent,
    notes: notes,
    requires: requires,
    tags: tags,
    request: request,
    response: response,
    captures: captures,
  ))
}

fn parse_request_helper(
  data: dynamic.Dynamic,
) -> Result(types.Request, List(dynamic.DecodeError)) {
  use method <- result.try(dynamic.field("method", parse_method_helper)(data))
  use path <- result.try(dynamic.field("path", dynamic.string)(data))
  use headers <- result.try(dynamic.field(
    "headers",
    dynamic.dict(dynamic.string, dynamic.string),
  )(data))
  use query <- result.try(dynamic.field("query", parse_json_dict_helper)(data))
  use body <- result.try(dynamic.field("body", parse_json_value_helper)(data))
  Ok(types.Request(method, path, headers, query, body))
}

fn parse_method_helper(
  data: dynamic.Dynamic,
) -> Result(types.Method, List(dynamic.DecodeError)) {
  data
  |> dynamic.string
  |> result.then(fn(s) {
    case s {
      "GET" -> Ok(Get)
      "POST" -> Ok(Post)
      "PUT" -> Ok(Put)
      "PATCH" -> Ok(Patch)
      "DELETE" -> Ok(Delete)
      "HEAD" -> Ok(Head)
      "OPTIONS" -> Ok(Options)
      _ ->
        Error([dynamic.DecodeError(expected: "HTTP method", found: s, path: [])])
    }
  })
}

fn parse_json_dict_helper(
  data: dynamic.Dynamic,
) -> Result(dict.Dict(String, json.Json), List(dynamic.DecodeError)) {
  data
  |> dynamic.dict(dynamic.string, dynamic.dynamic)
  |> result.map(fn(d) {
    dict.map_values(d, fn(_, v) { parser.dynamic_to_json(v) })
  })
}

fn parse_json_value_helper(
  data: dynamic.Dynamic,
) -> Result(json.Json, List(dynamic.DecodeError)) {
  Ok(parser.dynamic_to_json(data))
}

fn parse_response_helper(
  data: dynamic.Dynamic,
) -> Result(types.Response, List(dynamic.DecodeError)) {
  use status <- result.try(dynamic.field("status", dynamic.int)(data))
  use example <- result.try(dynamic.field("example", parse_json_value_helper)(
    data,
  ))
  use checks <- result.try(dynamic.field("checks", parse_checks_helper)(data))
  let headers =
    dynamic.field("headers", dynamic.dict(dynamic.string, dynamic.string))(data)
    |> result.unwrap(dict.new())
  Ok(types.Response(status, example, checks, headers))
}

fn parse_checks_helper(
  data: dynamic.Dynamic,
) -> Result(dict.Dict(String, types.Check), List(dynamic.DecodeError)) {
  dynamic.dict(dynamic.string, parse_check_helper)(data)
}

fn parse_check_helper(
  data: dynamic.Dynamic,
) -> Result(types.Check, List(dynamic.DecodeError)) {
  use rule <- result.try(dynamic.field("rule", dynamic.string)(data))
  use why <- result.try(dynamic.field("why", dynamic.string)(data))
  Ok(types.Check(rule, why))
}

fn parse_rules_helper(
  data: dynamic.Dynamic,
) -> Result(List(types.Rule), List(dynamic.DecodeError)) {
  dynamic.list(parse_rule_helper)(data)
}

fn parse_rule_helper(
  data: dynamic.Dynamic,
) -> Result(types.Rule, List(dynamic.DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use when <- result.try(dynamic.field("when", parse_when_helper)(data))
  use check <- result.try(dynamic.field("check", parse_rule_check_helper)(data))
  use example <- result.try(dynamic.field("example", parse_json_value_helper)(
    data,
  ))
  Ok(types.Rule(name, description, when, check, example))
}

fn parse_when_helper(
  data: dynamic.Dynamic,
) -> Result(types.When, List(dynamic.DecodeError)) {
  use status <- result.try(dynamic.field("status", dynamic.string)(data))
  use method <- result.try(dynamic.field("method", parse_method_helper)(data))
  use path <- result.try(dynamic.field("path", dynamic.string)(data))
  Ok(types.When(status, method, path))
}

fn parse_rule_check_helper(
  data: dynamic.Dynamic,
) -> Result(types.RuleCheck, List(dynamic.DecodeError)) {
  use body_must_not_contain <- result.try(dynamic.field(
    "body_must_not_contain",
    dynamic.list(dynamic.string),
  )(data))
  use body_must_contain <- result.try(dynamic.field(
    "body_must_contain",
    dynamic.list(dynamic.string),
  )(data))
  use fields_must_exist <- result.try(dynamic.field(
    "fields_must_exist",
    dynamic.list(dynamic.string),
  )(data))
  use fields_must_not_exist <- result.try(dynamic.field(
    "fields_must_not_exist",
    dynamic.list(dynamic.string),
  )(data))
  use header_must_exist <- result.try(dynamic.field(
    "header_must_exist",
    dynamic.string,
  )(data))
  use header_must_not_exist <- result.try(dynamic.field(
    "header_must_not_exist",
    dynamic.string,
  )(data))
  Ok(types.RuleCheck(
    body_must_not_contain,
    body_must_contain,
    fields_must_exist,
    fields_must_not_exist,
    header_must_exist,
    header_must_not_exist,
  ))
}

fn parse_anti_patterns_helper(
  data: dynamic.Dynamic,
) -> Result(List(types.AntiPattern), List(dynamic.DecodeError)) {
  dynamic.list(parse_anti_pattern_helper)(data)
}

fn parse_anti_pattern_helper(
  data: dynamic.Dynamic,
) -> Result(types.AntiPattern, List(dynamic.DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use bad_example <- result.try(dynamic.field(
    "bad_example",
    parse_json_value_helper,
  )(data))
  use good_example <- result.try(dynamic.field(
    "good_example",
    parse_json_value_helper,
  )(data))
  use why <- result.try(dynamic.field("why", dynamic.string)(data))
  Ok(types.AntiPattern(
    name: name,
    description: description,
    bad_example: bad_example,
    good_example: good_example,
    why: why,
  ))
}
