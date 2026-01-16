//// Comprehensive tests for the array_indexing module
//// Tests parsing and accessing array elements using [index] notation
//// Covers positive indices, negative indices, wildcards, and error cases

import gleam/json
import gleam/list
import gleeunit
import gleeunit/should
import intent/array_indexing.{
  All, Index, LastN, NoArray, get_all_array_elements,
  navigate_path, parse_path_component, split_path, validate_path,
}

pub fn main() {
  gleeunit.main()
}

// Helper function to create JSON arrays with the required `of` parameter
fn json_array(items: List(json.Json)) -> json.Json {
  json.array(items, of: fn(x) { x })
}

// ============================================================================
// parse_path_component Tests - Valid Parsing
// ============================================================================

/// Test parsing a simple field name without array notation
pub fn parse_path_component_simple_field_test() {
  let result = parse_path_component("username")
  result
  |> should.equal(Ok(#("username", NoArray)))
}

/// Test parsing field with positive array index [0]
pub fn parse_path_component_zero_index_test() {
  let result = parse_path_component("items[0]")
  result
  |> should.equal(Ok(#("items", Index(0))))
}

/// Test parsing field with positive array index [1]
pub fn parse_path_component_positive_index_test() {
  let result = parse_path_component("items[1]")
  result
  |> should.equal(Ok(#("items", Index(1))))
}

/// Test parsing field with large positive index
pub fn parse_path_component_large_index_test() {
  let result = parse_path_component("items[999]")
  result
  |> should.equal(Ok(#("items", Index(999))))
}

/// Test parsing field with negative index [-1] (last element)
pub fn parse_path_component_negative_one_test() {
  let result = parse_path_component("items[-1]")
  result
  |> should.equal(Ok(#("items", LastN(1))))
}

/// Test parsing field with negative index [-2]
pub fn parse_path_component_negative_two_test() {
  let result = parse_path_component("items[-2]")
  result
  |> should.equal(Ok(#("items", LastN(2))))
}

/// Test parsing field with large negative index
pub fn parse_path_component_large_negative_index_test() {
  let result = parse_path_component("items[-10]")
  result
  |> should.equal(Ok(#("items", LastN(10))))
}

/// Test parsing field with wildcard [*] (all elements)
pub fn parse_path_component_wildcard_test() {
  let result = parse_path_component("items[*]")
  result
  |> should.equal(Ok(#("items", All)))
}

/// Test parsing field with underscore in name
pub fn parse_path_component_with_underscore_test() {
  let result = parse_path_component("user_data[0]")
  result
  |> should.equal(Ok(#("user_data", Index(0))))
}

/// Test parsing field with hyphen in name
pub fn parse_path_component_with_hyphen_test() {
  let result = parse_path_component("user-data[1]")
  result
  |> should.equal(Ok(#("user-data", Index(1))))
}

// ============================================================================
// parse_path_component Tests - Error Cases
// ============================================================================

/// Test parsing field with missing closing bracket
pub fn parse_path_component_missing_closing_bracket_test() {
  let result = parse_path_component("items[0")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing closing ] in array index: items[0")
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing field with extra text after bracket
pub fn parse_path_component_extra_text_after_bracket_test() {
  let result = parse_path_component("items[0]extra")
  case result {
    Error(msg) -> {
      msg
      |> should.equal(
        "Invalid array syntax: only one ] expected: items[0]extra",
      )
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing field with non-numeric index
pub fn parse_path_component_non_numeric_index_test() {
  let result = parse_path_component("items[abc]")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index must be a number: abc")
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing field with empty brackets
pub fn parse_path_component_empty_brackets_test() {
  let result = parse_path_component("items[]")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index must be a number: ")
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing field with negative sign but non-numeric
pub fn parse_path_component_negative_non_numeric_test() {
  let result = parse_path_component("items[-abc]")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index must be a number: abc")
    }
    Ok(_) -> should.fail()
  }
}

/// Test parsing field with double negative
pub fn parse_path_component_double_negative_test() {
  let result = parse_path_component("items[--1]")
  case result {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

/// Test parsing field with floating point index
pub fn parse_path_component_float_index_test() {
  let result = parse_path_component("items[1.5]")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index must be a number: 1.5")
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// navigate_path Tests - Simple Arrays
// ============================================================================

/// Test navigating to first element of array
pub fn navigate_path_first_element_test() {
  let json =
    json.object([
      #(
        "items",
        json_array([json.string("a"), json.string("b"), json.string("c")]),
      ),
    ])
  let result = navigate_path(json, ["items[0]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"a\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating to second element of array
pub fn navigate_path_second_element_test() {
  let json =
    json.object([
      #(
        "items",
        json_array([json.string("a"), json.string("b"), json.string("c")]),
      ),
    ])
  let result = navigate_path(json, ["items[1]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"b\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating to last element using negative index
pub fn navigate_path_last_element_test() {
  let json =
    json.object([
      #(
        "items",
        json_array([json.string("a"), json.string("b"), json.string("c")]),
      ),
    ])
  let result = navigate_path(json, ["items[-1]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"c\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating to second-to-last element using negative index
pub fn navigate_path_second_to_last_element_test() {
  let json =
    json.object([
      #(
        "items",
        json_array([json.string("a"), json.string("b"), json.string("c")]),
      ),
    ])
  let result = navigate_path(json, ["items[-2]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"b\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating to third-to-last element using negative index
pub fn navigate_path_third_to_last_element_test() {
  let json =
    json.object([
      #(
        "items",
        json_array([json.string("a"), json.string("b"), json.string("c")]),
      ),
    ])
  let result = navigate_path(json, ["items[-3]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"a\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating array of numbers
pub fn navigate_path_number_array_test() {
  let json =
    json.object([
      #("numbers", json_array([json.int(1), json.int(2), json.int(3)])),
    ])
  let result = navigate_path(json, ["numbers[1]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("2")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating array of objects
pub fn navigate_path_object_array_test() {
  let json =
    json.object([
      #(
        "users",
        json_array([
          json.object([#("name", json.string("Alice"))]),
          json.object([#("name", json.string("Bob"))]),
        ]),
      ),
    ])
  let result = navigate_path(json, ["users[0]", "name"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"Alice\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating array of nested objects
pub fn navigate_path_nested_object_array_test() {
  let json =
    json.object([
      #(
        "users",
        json_array([
          json.object([
            #(
              "profile",
              json.object([#("email", json.string("alice@example.com"))]),
            ),
          ]),
          json.object([
            #(
              "profile",
              json.object([#("email", json.string("bob@example.com"))]),
            ),
          ]),
        ]),
      ),
    ])
  let result = navigate_path(json, ["users[1]", "profile", "email"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"bob@example.com\"")
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// navigate_path Tests - Error Cases
// ============================================================================

/// Test navigating with out of bounds positive index
pub fn navigate_path_out_of_bounds_positive_test() {
  let json =
    json.object([#("items", json_array([json.string("a"), json.string("b")]))])
  let result = navigate_path(json, ["items[5]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index 5 out of bounds (length: 2)")
    }
    Ok(_) -> should.fail()
  }
}

/// Test navigating with out of bounds negative index
pub fn navigate_path_out_of_bounds_negative_test() {
  let json =
    json.object([#("items", json_array([json.string("a"), json.string("b")]))])
  let result = navigate_path(json, ["items[-5]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index -5 out of bounds (length: 2)")
    }
    Ok(_) -> should.fail()
  }
}

/// Test navigating non-array with index notation
pub fn navigate_path_non_array_with_index_test() {
  let json = json.object([#("item", json.string("not an array"))])
  let result = navigate_path(json, ["item[0]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Cannot index non-array JSON with [0]")
    }
    Ok(_) -> should.fail()
  }
}

/// Test navigating non-array with negative index
pub fn navigate_path_non_array_negative_index_test() {
  let json = json.object([#("item", json.int(42))])
  let result = navigate_path(json, ["item[-1]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Cannot index non-array JSON with negative index")
    }
    Ok(_) -> should.fail()
  }
}

/// Test navigating to non-existent field
pub fn navigate_path_missing_field_test() {
  let json = json.object([#("items", json_array([json.string("a")]))])
  let result = navigate_path(json, ["missing[0]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Field 'missing' not found")
    }
    Ok(_) -> should.fail()
  }
}

/// Test navigating with wildcard (should error)
pub fn navigate_path_wildcard_test() {
  let json =
    json.object([#("items", json_array([json.string("a"), json.string("b")]))])
  let result = navigate_path(json, ["items[*]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array wildcard [*] requires special handling in rules")
    }
    Ok(_) -> should.fail()
  }
}

/// Test navigating empty array with index
pub fn navigate_path_empty_array_test() {
  let json = json.object([#("items", json_array([]))])
  let result = navigate_path(json, ["items[0]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index 0 out of bounds (length: 0)")
    }
    Ok(_) -> should.fail()
  }
}

/// Test navigating empty array with negative index
pub fn navigate_path_empty_array_negative_test() {
  let json = json.object([#("items", json_array([]))])
  let result = navigate_path(json, ["items[-1]"])
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index -1 out of bounds (length: 0)")
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// navigate_path Tests - Edge Cases
// ============================================================================

/// Test navigating single element array with index 0
pub fn navigate_path_single_element_array_test() {
  let json = json.object([#("items", json_array([json.string("only")]))])
  let result = navigate_path(json, ["items[0]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"only\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating single element array with negative index
pub fn navigate_path_single_element_array_negative_test() {
  let json = json.object([#("items", json_array([json.string("only")]))])
  let result = navigate_path(json, ["items[-1]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"only\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating array with null element
pub fn navigate_path_null_element_test() {
  let json =
    json.object([#("items", json_array([json.null(), json.string("b")]))])
  let result = navigate_path(json, ["items[0]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("null")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating array with boolean elements
pub fn navigate_path_boolean_array_test() {
  let json =
    json.object([#("flags", json_array([json.bool(True), json.bool(False)]))])
  let result = navigate_path(json, ["flags[0]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("true")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating deeply nested array path
pub fn navigate_path_deeply_nested_test() {
  let json =
    json.object([
      #(
        "level1",
        json.object([
          #(
            "level2",
            json_array([
              json.object([#("level3", json_array([json.string("found")]))]),
            ]),
          ),
        ]),
      ),
    ])
  let result = navigate_path(json, ["level1", "level2[0]", "level3[0]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"found\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating array with mixed types
pub fn navigate_path_mixed_type_array_test() {
  let json =
    json.object([
      #("mixed", json_array([json.string("a"), json.int(42), json.bool(True)])),
    ])
  let result = navigate_path(json, ["mixed[1]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("42")
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// get_all_array_elements Tests
// ============================================================================

/// Test getting all elements from simple array
pub fn get_all_array_elements_simple_test() {
  let json = json_array([json.string("a"), json.string("b"), json.string("c")])
  let result = get_all_array_elements(json)
  case result {
    Ok(elements) -> {
      list.length(elements)
      |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

/// Test getting all elements from empty array
pub fn get_all_array_elements_empty_test() {
  let json = json_array([])
  let result = get_all_array_elements(json)
  case result {
    Ok(elements) -> {
      list.length(elements)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

/// Test getting all elements from single-element array
pub fn get_all_array_elements_single_test() {
  let json = json_array([json.string("only")])
  let result = get_all_array_elements(json)
  case result {
    Ok(elements) -> {
      list.length(elements)
      |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

/// Test getting all elements from non-array (should error)
pub fn get_all_array_elements_non_array_test() {
  let json = json.string("not an array")
  let result = get_all_array_elements(json)
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Cannot get array elements from non-array JSON")
    }
    Ok(_) -> should.fail()
  }
}

/// Test getting all elements from object (should error)
pub fn get_all_array_elements_object_test() {
  let json = json.object([#("key", json.string("value"))])
  let result = get_all_array_elements(json)
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Cannot get array elements from non-array JSON")
    }
    Ok(_) -> should.fail()
  }
}

/// Test getting all elements from number (should error)
pub fn get_all_array_elements_number_test() {
  let json = json.int(42)
  let result = get_all_array_elements(json)
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Cannot get array elements from non-array JSON")
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// split_path Tests
// ============================================================================

/// Test splitting simple path
pub fn split_path_simple_test() {
  let result = split_path("user.name")
  result
  |> should.equal(["user", "name"])
}

/// Test splitting path with array notation
pub fn split_path_with_array_test() {
  let result = split_path("users[0].name")
  result
  |> should.equal(["users[0]", "name"])
}

/// Test splitting deeply nested path
pub fn split_path_nested_test() {
  let result = split_path("user.profile.emails[0].address")
  result
  |> should.equal(["user", "profile", "emails[0]", "address"])
}

/// Test splitting single component path
pub fn split_path_single_component_test() {
  let result = split_path("user")
  result
  |> should.equal(["user"])
}

/// Test splitting path with trailing dot
pub fn split_path_trailing_dot_test() {
  let result = split_path("user.name.")
  result
  |> should.equal(["user", "name"])
}

/// Test splitting path with leading dot
pub fn split_path_leading_dot_test() {
  let result = split_path(".user.name")
  result
  |> should.equal(["user", "name"])
}

/// Test splitting path with multiple consecutive dots
pub fn split_path_consecutive_dots_test() {
  let result = split_path("user..name")
  result
  |> should.equal(["user", "name"])
}

/// Test splitting path with spaces (trimmed)
pub fn split_path_with_spaces_test() {
  let result = split_path("user . name")
  result
  |> should.equal(["user", "name"])
}

/// Test splitting path with negative index
pub fn split_path_negative_index_test() {
  let result = split_path("users[-1].name")
  result
  |> should.equal(["users[-1]", "name"])
}

/// Test splitting path with wildcard
pub fn split_path_wildcard_test() {
  let result = split_path("users[*].name")
  result
  |> should.equal(["users[*]", "name"])
}

// ============================================================================
// validate_path Tests
// ============================================================================

/// Test validating simple valid path
pub fn validate_path_simple_valid_test() {
  let result = validate_path("user.name")
  result
  |> should.equal(Ok(Nil))
}

/// Test validating path with array index
pub fn validate_path_with_index_test() {
  let result = validate_path("users[0].name")
  result
  |> should.equal(Ok(Nil))
}

/// Test validating path with negative index
pub fn validate_path_with_negative_index_test() {
  let result = validate_path("users[-1].name")
  result
  |> should.equal(Ok(Nil))
}

/// Test validating path with wildcard
pub fn validate_path_with_wildcard_test() {
  let result = validate_path("users[*].name")
  result
  |> should.equal(Ok(Nil))
}

/// Test validating empty path (should error)
pub fn validate_path_empty_test() {
  let result = validate_path("")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Path cannot be empty")
    }
    Ok(_) -> should.fail()
  }
}

/// Test validating path with invalid array syntax
pub fn validate_path_invalid_array_syntax_test() {
  let result = validate_path("users[0.name")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing closing ] in array index: users[0")
    }
    Ok(_) -> should.fail()
  }
}

/// Test validating path with non-numeric index
pub fn validate_path_non_numeric_index_test() {
  let result = validate_path("users[abc].name")
  case result {
    Error(msg) -> {
      msg
      |> should.equal("Array index must be a number: abc")
    }
    Ok(_) -> should.fail()
  }
}

/// Test validating complex nested path
pub fn validate_path_complex_nested_test() {
  let result = validate_path("data.users[0].profile.emails[-1].address")
  result
  |> should.equal(Ok(Nil))
}

/// Test validating path with multiple array indices
pub fn validate_path_multiple_indices_test() {
  let result = validate_path("matrix[0][1][2]")
  case result {
    Error(_) -> Nil
    // Multiple brackets in one component should error
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// ArraySpec Type Tests
// ============================================================================

/// Test ArraySpec NoArray equality
pub fn array_spec_no_array_equality_test() {
  NoArray
  |> should.equal(NoArray)
}

/// Test ArraySpec Index equality
pub fn array_spec_index_equality_test() {
  Index(5)
  |> should.equal(Index(5))
}

/// Test ArraySpec LastN equality
pub fn array_spec_lastn_equality_test() {
  LastN(2)
  |> should.equal(LastN(2))
}

/// Test ArraySpec All equality
pub fn array_spec_all_equality_test() {
  All
  |> should.equal(All)
}

// ============================================================================
// Integration Tests - Complex Scenarios
// ============================================================================

/// Test navigating complex real-world API response structure
pub fn navigate_path_complex_api_response_test() {
  let json =
    json.object([
      #(
        "data",
        json.object([
          #(
            "users",
            json_array([
              json.object([
                #("id", json.int(1)),
                #("name", json.string("Alice")),
                #(
                  "emails",
                  json_array([
                    json.object([
                      #("address", json.string("alice@primary.com")),
                      #("verified", json.bool(True)),
                    ]),
                    json.object([
                      #("address", json.string("alice@secondary.com")),
                      #("verified", json.bool(False)),
                    ]),
                  ]),
                ),
              ]),
              json.object([
                #("id", json.int(2)),
                #("name", json.string("Bob")),
                #(
                  "emails",
                  json_array([
                    json.object([
                      #("address", json.string("bob@example.com")),
                      #("verified", json.bool(True)),
                    ]),
                  ]),
                ),
              ]),
            ]),
          ),
        ]),
      ),
    ])

  // Test accessing first user's second email address
  let result = navigate_path(json, ["data", "users[0]", "emails[1]", "address"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"alice@secondary.com\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating with combination of positive and negative indices
pub fn navigate_path_mixed_indices_test() {
  let json =
    json.object([
      #(
        "data",
        json_array([
          json.object([
            #(
              "items",
              json_array([json.string("a"), json.string("b"), json.string("c")]),
            ),
          ]),
        ]),
      ),
    ])

  // Access first object in data array, then last item in items array
  let result = navigate_path(json, ["data[0]", "items[-1]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("\"c\"")
    }
    Error(_) -> should.fail()
  }
}

/// Test navigating array with very large index (boundary test)
pub fn navigate_path_large_array_test() {
  // Create array with 100 elements
  let items =
    list.range(0, 99)
    |> list.map(fn(i) { json.int(i) })

  let json = json.object([#("numbers", json_array(items))])

  let result = navigate_path(json, ["numbers[99]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("99")
    }
    Error(_) -> should.fail()
  }
}

/// Test accessing last element of large array with negative index
pub fn navigate_path_large_array_negative_test() {
  // Create array with 100 elements
  let items =
    list.range(0, 99)
    |> list.map(fn(i) { json.int(i) })

  let json = json.object([#("numbers", json_array(items))])

  let result = navigate_path(json, ["numbers[-1]"])
  case result {
    Ok(value) -> {
      json.to_string(value)
      |> should.equal("99")
    }
    Error(_) -> should.fail()
  }
}
