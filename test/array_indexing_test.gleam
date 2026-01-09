//// Comprehensive tests for array indexing in JSON path navigation
//// Tests parsing, navigation, and error handling for array access patterns:
//// - items[0] (positive index)
//// - items[-1] (negative index / from end)
//// - items[*] (wildcard / all elements)
//// - Nested paths with arrays
//// - Error cases and boundary conditions

import gleam/json
import gleam/list
import gleam/function
import gleam/string
import gleeunit/should
import intent/array_indexing.{
  All, Index, LastN, NoArray, get_all_array_elements,
  navigate_path, parse_path_component, split_path, validate_path,
}

// ============================================================================
// parse_path_component Tests
// ============================================================================

pub fn parse_simple_field_test() {
  // Simple field without array index
  parse_path_component("name")
  |> should.equal(Ok(#("name", NoArray)))
}

pub fn parse_field_with_underscore_test() {
  parse_path_component("user_name")
  |> should.equal(Ok(#("user_name", NoArray)))
}

pub fn parse_field_with_dash_test() {
  parse_path_component("user-name")
  |> should.equal(Ok(#("user-name", NoArray)))
}

pub fn parse_positive_index_zero_test() {
  // First element: items[0]
  parse_path_component("items[0]")
  |> should.equal(Ok(#("items", Index(0))))
}

pub fn parse_positive_index_nonzero_test() {
  // Specific element: items[5]
  parse_path_component("items[5]")
  |> should.equal(Ok(#("items", Index(5))))
}

pub fn parse_positive_index_large_test() {
  // Large index: items[999]
  parse_path_component("items[999]")
  |> should.equal(Ok(#("items", Index(999))))
}

pub fn parse_negative_index_last_test() {
  // Last element: items[-1]
  parse_path_component("items[-1]")
  |> should.equal(Ok(#("items", LastN(1))))
}

pub fn parse_negative_index_second_last_test() {
  // Second to last: items[-2]
  parse_path_component("items[-2]")
  |> should.equal(Ok(#("items", LastN(2))))
}

pub fn parse_wildcard_test() {
  // All elements: items[*]
  parse_path_component("items[*]")
  |> should.equal(Ok(#("items", All)))
}

pub fn parse_empty_field_name_with_index_test() {
  // Edge case: just [0] without field name
  case parse_path_component("[0]") {
    Ok(#("", Index(0))) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

// ============================================================================
// parse_path_component Error Cases
// ============================================================================

pub fn parse_missing_closing_bracket_test() {
  case parse_path_component("items[0") {
    Error(msg) -> {
      msg
      |> string.contains("Missing closing ]")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn parse_missing_opening_bracket_test() {
  // This should parse as a regular field (no array syntax detected)
  parse_path_component("items0]")
  |> should.equal(Ok(#("items0]", NoArray)))
}

pub fn parse_non_numeric_index_test() {
  case parse_path_component("items[abc]") {
    Error(msg) -> {
      msg
      |> string.contains("must be a number")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn parse_float_index_test() {
  case parse_path_component("items[1.5]") {
    Error(msg) -> {
      msg
      |> string.contains("must be a number")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn parse_empty_index_test() {
  case parse_path_component("items[]") {
    Error(msg) -> {
      msg
      |> string.contains("must be a number")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn parse_multiple_brackets_test() {
  // items[0][1] - only first index should be processed, rest is invalid
  case parse_path_component("items[0][1]") {
    Error(msg) -> {
      msg
      |> string.contains("only one ]")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn parse_space_in_index_test() {
  case parse_path_component("items[ 0 ]") {
    Error(msg) -> {
      msg
      |> string.contains("must be a number")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// split_path Tests
// ============================================================================

pub fn split_simple_path_test() {
  split_path("user.name")
  |> should.equal(["user", "name"])
}

pub fn split_nested_path_test() {
  split_path("user.profile.settings.theme")
  |> should.equal(["user", "profile", "settings", "theme"])
}

pub fn split_path_with_array_test() {
  split_path("users[0].name")
  |> should.equal(["users[0]", "name"])
}

pub fn split_path_multiple_arrays_test() {
  split_path("orgs[0].teams[1].members[2]")
  |> should.equal(["orgs[0]", "teams[1]", "members[2]"])
}

pub fn split_single_field_test() {
  split_path("name")
  |> should.equal(["name"])
}

pub fn split_empty_path_test() {
  split_path("")
  |> should.equal([])
}

pub fn split_path_with_spaces_test() {
  // Spaces should be trimmed
  split_path("user . name")
  |> should.equal(["user", "name"])
}

pub fn split_path_leading_dot_test() {
  // Leading dot creates empty segment which is filtered
  split_path(".user.name")
  |> should.equal(["user", "name"])
}

pub fn split_path_trailing_dot_test() {
  split_path("user.name.")
  |> should.equal(["user", "name"])
}

// ============================================================================
// validate_path Tests
// ============================================================================

pub fn validate_simple_path_test() {
  validate_path("user.name")
  |> should.be_ok
}

pub fn validate_path_with_arrays_test() {
  validate_path("users[0].emails[1].address")
  |> should.be_ok
}

pub fn validate_path_with_negative_index_test() {
  validate_path("items[-1].value")
  |> should.be_ok
}

pub fn validate_path_with_wildcard_test() {
  validate_path("users[*].name")
  |> should.be_ok
}

pub fn validate_empty_path_error_test() {
  case validate_path("") {
    Error(msg) -> {
      msg
      |> string.contains("cannot be empty")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_invalid_index_in_path_test() {
  case validate_path("users[abc].name") {
    Error(msg) -> {
      msg
      |> string.contains("must be a number")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_unclosed_bracket_in_path_test() {
  case validate_path("users[0.name") {
    Error(msg) -> {
      msg
      |> string.contains("Missing closing ]")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// navigate_path Tests - Happy Path
// ============================================================================

pub fn navigate_simple_field_test() {
  let json_data = json.object([#("name", json.string("Alice"))])

  navigate_path(json_data, ["name"])
  |> should.equal(Ok(json.string("Alice")))
}

pub fn navigate_nested_field_test() {
  let json_data =
    json.object([
      #("user", json.object([#("profile", json.object([#("name", json.string("Bob"))]))])),
    ])

  navigate_path(json_data, ["user", "profile", "name"])
  |> should.equal(Ok(json.string("Bob")))
}

pub fn navigate_array_first_element_test() {
  let json_data =
    json.object([#("items", json.array([json.int(10), json.int(20), json.int(30)], function.identity))])

  navigate_path(json_data, ["items[0]"])
  |> should.equal(Ok(json.int(10)))
}

pub fn navigate_array_middle_element_test() {
  let json_data =
    json.object([#("items", json.array([json.int(10), json.int(20), json.int(30)], function.identity))])

  navigate_path(json_data, ["items[1]"])
  |> should.equal(Ok(json.int(20)))
}

pub fn navigate_array_last_by_index_test() {
  let json_data =
    json.object([#("items", json.array([json.int(10), json.int(20), json.int(30)], function.identity))])

  navigate_path(json_data, ["items[2]"])
  |> should.equal(Ok(json.int(30)))
}

pub fn navigate_array_last_negative_test() {
  let json_data =
    json.object([#("items", json.array([json.int(10), json.int(20), json.int(30)], function.identity))])

  navigate_path(json_data, ["items[-1]"])
  |> should.equal(Ok(json.int(30)))
}

pub fn navigate_array_second_last_test() {
  let json_data =
    json.object([#("items", json.array([json.int(10), json.int(20), json.int(30)], function.identity))])

  navigate_path(json_data, ["items[-2]"])
  |> should.equal(Ok(json.int(20)))
}

pub fn navigate_nested_array_element_test() {
  let json_data =
    json.object([
      #(
        "users",
        json.array([
          json.object([#("name", json.string("Alice"))]),
          json.object([#("name", json.string("Bob"))]),
        ], function.identity),
      ),
    ])

  navigate_path(json_data, ["users[1]", "name"])
  |> should.equal(Ok(json.string("Bob")))
}

pub fn navigate_empty_path_test() {
  let json_data = json.object([#("name", json.string("test"))])

  // Empty path returns the whole JSON
  navigate_path(json_data, [])
  |> should.equal(Ok(json_data))
}

pub fn navigate_array_of_objects_test() {
  let json_data =
    json.object([
      #(
        "products",
        json.array([
          json.object([#("id", json.int(1)), #("price", json.float(9.99))]),
          json.object([#("id", json.int(2)), #("price", json.float(19.99))]),
        ], function.identity),
      ),
    ])

  navigate_path(json_data, ["products[0]", "price"])
  |> should.equal(Ok(json.float(9.99)))
}

// ============================================================================
// navigate_path Tests - Error Cases
// ============================================================================

pub fn navigate_field_not_found_test() {
  let json_data = json.object([#("name", json.string("Alice"))])

  case navigate_path(json_data, ["nonexistent"]) {
    Error(msg) -> {
      msg
      |> string.contains("not found")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_array_out_of_bounds_positive_test() {
  let json_data =
    json.object([#("items", json.array([json.int(1), json.int(2), json.int(3)], function.identity))])

  case navigate_path(json_data, ["items[10]"]) {
    Error(msg) -> {
      msg
      |> string.contains("out of bounds")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_array_out_of_bounds_negative_test() {
  let json_data =
    json.object([#("items", json.array([json.int(1), json.int(2), json.int(3)], function.identity))])

  case navigate_path(json_data, ["items[-10]"]) {
    Error(msg) -> {
      msg
      |> string.contains("out of bounds")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_index_on_non_array_test() {
  let json_data = json.object([#("name", json.string("Alice"))])

  case navigate_path(json_data, ["name[0]"]) {
    Error(msg) -> {
      msg
      |> string.contains("non-array")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_field_on_non_object_test() {
  let json_data = json.string("just a string")

  case navigate_path(json_data, ["field"]) {
    Error(msg) -> {
      msg
      |> string.contains("non-object")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_wildcard_error_test() {
  let json_data =
    json.object([#("items", json.array([json.int(1), json.int(2)], function.identity))])

  case navigate_path(json_data, ["items[*]"]) {
    Error(msg) -> {
      msg
      |> string.contains("wildcard")
      |> should.be_true
      msg
      |> string.contains("special handling")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_deeply_nested_not_found_test() {
  let json_data =
    json.object([#("a", json.object([#("b", json.object([#("c", json.int(1))]))]))])

  case navigate_path(json_data, ["a", "b", "x", "y"]) {
    Error(msg) -> {
      msg
      |> string.contains("not found")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_array_empty_test() {
  let json_data = json.object([#("items", json.array([], function.identity))])

  case navigate_path(json_data, ["items[0]"]) {
    Error(msg) -> {
      msg
      |> string.contains("out of bounds")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn navigate_negative_index_empty_array_test() {
  let json_data = json.object([#("items", json.array([], function.identity))])

  case navigate_path(json_data, ["items[-1]"]) {
    Error(msg) -> {
      msg
      |> string.contains("out of bounds")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// get_all_array_elements Tests
// ============================================================================

pub fn get_all_elements_simple_test() {
  let json_array = json.array([json.int(1), json.int(2), json.int(3)], function.identity)

  case get_all_array_elements(json_array) {
    Ok(elements) -> {
      list.length(elements)
      |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

pub fn get_all_elements_objects_test() {
  let json_array =
    json.array([
      json.object([#("id", json.int(1))]),
      json.object([#("id", json.int(2))]),
    ], function.identity)

  case get_all_array_elements(json_array) {
    Ok(elements) -> {
      list.length(elements)
      |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

pub fn get_all_elements_empty_array_test() {
  let json_array = json.array([], function.identity)

  case get_all_array_elements(json_array) {
    Ok(elements) -> {
      list.length(elements)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

pub fn get_all_elements_non_array_error_test() {
  let json_obj = json.object([#("name", json.string("test"))])

  case get_all_array_elements(json_obj) {
    Error(msg) -> {
      msg
      |> string.contains("non-array")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn get_all_elements_string_error_test() {
  let json_str = json.string("not an array")

  case get_all_array_elements(json_str) {
    Error(msg) -> {
      msg
      |> string.contains("non-array")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn get_all_elements_mixed_types_test() {
  let json_array =
    json.array([
      json.string("text"),
      json.int(42),
      json.bool(True),
      json.null(),
    ], function.identity)

  case get_all_array_elements(json_array) {
    Ok(elements) -> {
      list.length(elements)
      |> should.equal(4)
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Complex/Integration Tests
// ============================================================================

pub fn complex_nested_array_path_test() {
  // Deep nesting: users[0].emails[1].verified
  let json_data =
    json.object([
      #(
        "users",
        json.array([
          json.object([
            #("name", json.string("Alice")),
            #(
              "emails",
              json.array([
                json.object([#("addr", json.string("a@x.com")), #("verified", json.bool(True))]),
                json.object([#("addr", json.string("b@x.com")), #("verified", json.bool(False))]),
              ], function.identity),
            ),
          ]),
        ], function.identity),
      ),
    ])

  navigate_path(json_data, ["users[0]", "emails[1]", "verified"])
  |> should.equal(Ok(json.bool(False)))
}

pub fn navigate_array_of_arrays_test() {
  // Matrix-like structure: matrix[1][0]
  // Note: This requires two separate array access components
  let json_data =
    json.object([
      #(
        "matrix",
        json.array([
          json.array([json.int(1), json.int(2)], function.identity),
          json.array([json.int(3), json.int(4)], function.identity),
        ], function.identity),
      ),
    ])

  // First get matrix[1]
  case navigate_path(json_data, ["matrix[1]"]) {
    Ok(row) -> {
      // Then navigate into the array result
      // This shows a limitation - we can't do matrix[1][0] in one path
      case get_all_array_elements(row) {
        Ok(elements) -> {
          list.length(elements)
          |> should.equal(2)
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn navigate_null_value_test() {
  let json_data = json.object([#("value", json.null())])

  navigate_path(json_data, ["value"])
  |> should.equal(Ok(json.null()))
}

pub fn navigate_boolean_value_test() {
  let json_data = json.object([#("active", json.bool(True))])

  navigate_path(json_data, ["active"])
  |> should.equal(Ok(json.bool(True)))
}

pub fn navigate_float_value_test() {
  let json_data = json.object([#("price", json.float(19.99))])

  navigate_path(json_data, ["price"])
  |> should.equal(Ok(json.float(19.99)))
}

pub fn navigate_large_array_last_element_test() {
  // Array with 100 elements, access last one
  let elements = list.range(1, 100) |> list.map(json.int)
  let json_data = json.object([#("numbers", json.array(elements, function.identity))])

  navigate_path(json_data, ["numbers[-1]"])
  |> should.equal(Ok(json.int(100)))
}

pub fn navigate_large_array_specific_index_test() {
  let elements = list.range(0, 99) |> list.map(json.int)
  let json_data = json.object([#("numbers", json.array(elements, function.identity))])

  navigate_path(json_data, ["numbers[50]"])
  |> should.equal(Ok(json.int(50)))
}

pub fn validate_path_complex_valid_test() {
  // Validate a complex but valid path
  validate_path("data.users[0].profile.settings[-1].enabled")
  |> should.be_ok
}
