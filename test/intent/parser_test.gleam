//// Comprehensive tests for the parser module
//// Tests JSON parsing, dynamic_to_json utility, and all parse functions

import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/parser
import intent/types.{Post}

// ============================================================================
// dynamic_to_json Tests
// ============================================================================

pub fn dynamic_to_json_bool_true_test() {
  let dyn = dynamic.from(True)
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("true")
}

pub fn dynamic_to_json_bool_false_test() {
  let dyn = dynamic.from(False)
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("false")
}

pub fn dynamic_to_json_int_test() {
  let dyn = dynamic.from(42)
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("42")
}

pub fn dynamic_to_json_int_negative_test() {
  let dyn = dynamic.from(-123)
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("-123")
}

pub fn dynamic_to_json_float_test() {
  let dyn = dynamic.from(3.14)
  let json_str = parser.dynamic_to_json(dyn) |> json.to_string
  string.contains(json_str, "3.14")
  |> should.be_true
}

pub fn dynamic_to_json_string_test() {
  let dyn = dynamic.from("hello")
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("\"hello\"")
}

pub fn dynamic_to_json_string_empty_test() {
  let dyn = dynamic.from("")
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("\"\"")
}

pub fn dynamic_to_json_list_of_ints_test() {
  let dyn = dynamic.from([1, 2, 3])
  let json_result = parser.dynamic_to_json(dyn)
  let json_str = json.to_string(json_result)
  string.contains(json_str, "1")
  |> should.be_true
}

pub fn dynamic_to_json_list_of_strings_test() {
  let dyn = dynamic.from(["a", "b", "c"])
  let json_result = parser.dynamic_to_json(dyn)
  let json_str = json.to_string(json_result)
  string.contains(json_str, "\"a\"")
  |> should.be_true
}

pub fn dynamic_to_json_empty_list_test() {
  let dyn = dynamic.from([])
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("[]")
}

pub fn dynamic_to_json_nested_list_test() {
  let dyn = dynamic.from([[1, 2], [3, 4]])
  let json_result = parser.dynamic_to_json(dyn)
  let json_str = json.to_string(json_result)
  string.contains(json_str, "[1,2]")
  |> should.be_true
}

pub fn dynamic_to_json_dict_test() {
  let dict_val = dict.from_list([#("key", "value")])
  let dyn = dynamic.from(dict_val)
  let json_result = parser.dynamic_to_json(dyn)
  let json_str = json.to_string(json_result)
  string.contains(json_str, "\"key\"")
  |> should.be_true
}

pub fn dynamic_to_json_dict_multiple_entries_test() {
  let dict_val = dict.from_list([#("name", "Alice"), #("age", "30")])
  let dyn = dynamic.from(dict_val)
  let json_result = parser.dynamic_to_json(dyn)
  let json_str = json.to_string(json_result)
  {string.contains(json_str, "\"name\"")
  && string.contains(json_str, "\"Alice\"")}
  |> should.be_true
}

pub fn dynamic_to_json_empty_dict_test() {
  let dict_val = dict.new()
  let dyn = dynamic.from(dict_val)
  parser.dynamic_to_json(dyn)
  |> json.to_string
  |> should.equal("{}")
}

pub fn dynamic_to_json_nested_dict_test() {
  let inner_dict = dict.from_list([#("inner_key", "inner_value")])
  let outer_dict = dict.from_list([#("outer_key", inner_dict)])
  let dyn = dynamic.from(outer_dict)
  let json_result = parser.dynamic_to_json(dyn)
  let json_str = json.to_string(json_result)
  {string.contains(json_str, "\"outer_key\"")
  && string.contains(json_str, "\"inner_key\"")}
  |> should.be_true
}

// ============================================================================
// parse_method Tests
// ============================================================================

pub fn parse_method_get_test() {
  parser.parse_spec(dynamic.from(minimal_spec_data("GET")))
  |> should.be_ok
}

pub fn parse_method_post_test() {
  parser.parse_spec(dynamic.from(minimal_spec_data("POST")))
  |> should.be_ok
}

pub fn parse_method_put_test() {
  parser.parse_spec(dynamic.from(minimal_spec_data("PUT")))
  |> should.be_ok
}

pub fn parse_method_patch_test() {
  parser.parse_spec(dynamic.from(minimal_spec_data("PATCH")))
  |> should.be_ok
}

pub fn parse_method_delete_test() {
  parser.parse_spec(dynamic.from(minimal_spec_data("DELETE")))
  |> should.be_ok
}

pub fn parse_method_head_test() {
  parser.parse_spec(dynamic.from(minimal_spec_data("HEAD")))
  |> should.be_ok
}

pub fn parse_method_options_test() {
  parser.parse_spec(dynamic.from(minimal_spec_data("OPTIONS")))
  |> should.be_ok
}

// ============================================================================
// parse_spec Tests
// ============================================================================

pub fn parse_spec_minimal_test() {
  let spec_data = minimal_spec_data("GET")
  parser.parse_spec(dynamic.from(spec_data))
  |> should.be_ok
}

pub fn parse_spec_name_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.name |> should.equal("Test Spec")
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_description_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.description |> should.equal("Test Description")
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_audience_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.audience |> should.equal("Test Audience")
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_version_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.version |> should.equal("1.0.0")
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_success_criteria_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) ->
      spec.success_criteria |> should.equal(["Criterion 1", "Criterion 2"])
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_config_base_url_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.config.base_url |> should.equal("http://localhost:8080")
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_config_timeout_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.config.timeout_ms |> should.equal(5000)
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_config_headers_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) ->
      spec.config.headers
      |> dict.get("Authorization")
      |> should.equal(Ok("Bearer token123"))
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_features_length_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.features |> list.length |> should.equal(1)
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_feature_name_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> {
      case spec.features {
        [feature, ..] -> feature.name |> should.equal("User Management")
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_behavior_method_post_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("POST"))) {
    Ok(spec) -> {
      case spec.features {
        [feature, ..] -> {
          case feature.behaviors {
            [behavior, ..] -> behavior.request.method |> should.equal(Post)
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_behavior_path_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> {
      case spec.features {
        [feature, ..] -> {
          case feature.behaviors {
            [behavior, ..] -> behavior.request.path |> should.equal("/api/users")
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_request_headers_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> {
      case spec.features {
        [feature, ..] -> {
          case feature.behaviors {
            [behavior, ..] ->
              behavior.request.headers
              |> dict.get("Content-Type")
              |> should.equal(Ok("application/json"))
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_response_status_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> {
      case spec.features {
        [feature, ..] -> {
          case feature.behaviors {
            [behavior, ..] -> behavior.response.status |> should.equal(200)
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_rules_length_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.rules |> list.length |> should.equal(1)
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_rule_name_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> {
      case spec.rules {
        [rule, ..] ->
          rule.name |> should.equal("Always Include Auth Header")
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_anti_patterns_length_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> spec.anti_patterns |> list.length |> should.equal(1)
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_anti_pattern_name_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) -> {
      case spec.anti_patterns {
        [pattern, ..] ->
          pattern.name |> should.equal("Missing Error Handling")
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_spec_ai_hints_stack_length_test() {
  case parser.parse_spec(dynamic.from(minimal_spec_data("GET"))) {
    Ok(spec) ->
      spec.ai_hints.implementation.suggested_stack
      |> list.length
      |> should.equal(2)
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Error Cases
// ============================================================================

pub fn parse_spec_missing_name_test() {
  let spec_data = dict.from_list([
    #("description", dynamic.from("Test")),
    #("audience", dynamic.from("Test")),
    #("version", dynamic.from("1.0.0")),
    #("success_criteria", dynamic.from([])),
    #("config", dynamic.from(dict.new())),
    #("features", dynamic.from([])),
    #("rules", dynamic.from([])),
    #("anti_patterns", dynamic.from([])),
    #("ai_hints", dynamic.from(dict.new())),
  ])
  parser.parse_spec(dynamic.from(spec_data))
  |> should.be_error
}

pub fn parse_spec_missing_description_test() {
  let spec_data = dict.from_list([
    #("name", dynamic.from("Test")),
    #("audience", dynamic.from("Test")),
    #("version", dynamic.from("1.0.0")),
    #("success_criteria", dynamic.from([])),
    #("config", dynamic.from(dict.new())),
    #("features", dynamic.from([])),
    #("rules", dynamic.from([])),
    #("anti_patterns", dynamic.from([])),
    #("ai_hints", dynamic.from(dict.new())),
  ])
  parser.parse_spec(dynamic.from(spec_data))
  |> should.be_error
}

pub fn parse_spec_wrong_type_timeout_test() {
  let spec_data = dict.from_list([
    #("name", dynamic.from("Test")),
    #("description", dynamic.from("Test")),
    #("audience", dynamic.from("Test")),
    #("version", dynamic.from("1.0.0")),
    #("success_criteria", dynamic.from([])),
    #("config", dynamic.from(dict.from_list([
      #("base_url", dynamic.from("http://localhost")),
      #("timeout_ms", dynamic.from("5000")),
      #("headers", dynamic.from(dict.new())),
    ]))),
    #("features", dynamic.from([])),
    #("rules", dynamic.from([])),
    #("anti_patterns", dynamic.from([])),
    #("ai_hints", dynamic.from(dict.new())),
  ])
  parser.parse_spec(dynamic.from(spec_data))
  |> should.be_error
}

// ============================================================================
// Helper Functions
// ============================================================================

fn minimal_spec_data(method: String) {
  dict.from_list([
    #("name", dynamic.from("Test Spec")),
    #("description", dynamic.from("Test Description")),
    #("audience", dynamic.from("Test Audience")),
    #("version", dynamic.from("1.0.0")),
    #("success_criteria", dynamic.from(["Criterion 1", "Criterion 2"])),
    #("config", dynamic.from(dict.from_list([
      #("base_url", dynamic.from("http://localhost:8080")),
      #("timeout_ms", dynamic.from(5000)),
      #("headers", dynamic.from(dict.from_list([
        #("Authorization", dynamic.from("Bearer token123")),
      ]))),
    ]))),
    #("features", dynamic.from([dict.from_list([
      #("name", dynamic.from("User Management")),
      #("description", dynamic.from("User management features")),
      #("behaviors", dynamic.from([dict.from_list([
        #("name", dynamic.from("Get User")),
        #("intent", dynamic.from("Retrieve user details")),
        #("notes", dynamic.from("")),
        #("requires", dynamic.from([])),
        #("tags", dynamic.from(["read"])),
        #("request", dynamic.from(dict.from_list([
          #("method", dynamic.from(method)),
          #("path", dynamic.from("/api/users")),
          #("headers", dynamic.from(dict.from_list([
            #("Content-Type", dynamic.from("application/json")),
          ]))),
          #("query", dynamic.from(dict.from_list([]))),
          #("body", dynamic.from(json.null())),
        ]))),
        #("response", dynamic.from(dict.from_list([
          #("status", dynamic.from(200)),
          #("example", dynamic.from(json.null())),
          #("checks", dynamic.from(dict.from_list([]))),
          #("headers", dynamic.from(dict.from_list([]))),
        ]))),
        #("captures", dynamic.from(dict.from_list([]))),
      ])])),
    ])])),
    #("rules", dynamic.from([dict.from_list([
      #("name", dynamic.from("Always Include Auth Header")),
      #("description", dynamic.from("All requests must include Authorization")),
      #("when", dynamic.from(dict.from_list([
        #("status", dynamic.from("2xx")),
        #("method", dynamic.from("GET")),
        #("path", dynamic.from("/api/*")),
      ]))),
      #("check", dynamic.from(dict.from_list([
        #("body_must_not_contain", dynamic.from([])),
        #("body_must_contain", dynamic.from([])),
        #("fields_must_exist", dynamic.from([])),
        #("fields_must_not_exist", dynamic.from([])),
        #("header_must_exist", dynamic.from("Authorization")),
        #("header_must_not_exist", dynamic.from("")),
      ]))),
      #("example", dynamic.from(json.null())),
    ])])),
    #("anti_patterns", dynamic.from([dict.from_list([
      #("name", dynamic.from("Missing Error Handling")),
      #("description", dynamic.from("Endpoints should handle errors gracefully")),
      #("bad_example", dynamic.from(json.null())),
      #("good_example", dynamic.from(json.null())),
      #("why", dynamic.from("Error handling is critical")),
    ])])),
    #("ai_hints", dynamic.from(dict.from_list([
      #("implementation", dynamic.from(dict.from_list([
        #("suggested_stack", dynamic.from(["Node.js", "Express"])),
      ]))),
      #("entities", dynamic.from(dict.from_list([]))),
      #("security", dynamic.from(dict.from_list([
        #("password_hashing", dynamic.from("bcrypt")),
        #("jwt_algorithm", dynamic.from("HS256")),
        #("jwt_expiry", dynamic.from("24h")),
        #("rate_limiting", dynamic.from("100 per minute")),
      ]))),
      #("pitfalls", dynamic.from([])),
    ]))),
  ])
}
