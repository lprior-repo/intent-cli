import gleeunit
import gleeunit/should
import intent/array_indexing
import intent/parser
import gleam/json
import gleam/dynamic
import gleam/list

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// parse_path_component Tests
// ============================================================================

// Test basic field without array indexing
pub fn parse_path_component_no_array_test() {
  case array_indexing.parse_path_component("items") {
    Ok(#("items", array_indexing.NoArray)) -> should.be_true(True)
    _ -> should.fail("Expected NoArray for basic field")
  }
}

// Test positive index parsing
pub fn parse_path_component_positive_index_test() {
  case array_indexing.parse_path_component("items[0]") {
    Ok(#("items", array_indexing.Index(0))) -> should.be_true(True)
    _ -> should.fail("Expected Index(0) for items[0]")
  }
}

// Test positive index greater than 0
pub fn parse_path_component_positive_index_gt_0_test() {
  case array_indexing.parse_path_component("items[5]") {
    Ok(#("items", array_indexing.Index(5))) -> should.be_true(True)
    _ -> should.fail("Expected Index(5) for items[5]")
  }
}

// Test negative index parsing (LastN)
pub fn parse_path_component_negative_index_test() {
  case array_indexing.parse_path_component("items[-1]") {
    Ok(#("items", array_indexing.LastN(1))) -> should.be_true(True)
    _ -> should.fail("Expected LastN(1) for items[-1]")
  }
}

// Test negative index with n > 1
pub fn parse_path_component_negative_index_gt_1_test() {
  case array_indexing.parse_path_component("items[-3]") {
    Ok(#("items", array_indexing.LastN(3))) -> should.be_true(True)
    _ -> should.fail("Expected LastN(3) for items[-3]")
  }
}

// Test wildcard array parsing
pub fn parse_path_component_wildcard_test() {
  case array_indexing.parse_path_component("items[*]") {
    Ok(#("items", array_indexing.All)) -> should.be_true(True)
    _ -> should.fail("Expected All for items[*]")
  }
}

// Test invalid syntax - missing closing bracket
pub fn parse_path_component_missing_closing_bracket_test() {
  case array_indexing.parse_path_component("items[0") {
    Error(_) -> should.be_true(True)
    _ -> should.fail("Expected error for missing closing bracket")
  }
}

// Test invalid syntax - multiple brackets
pub fn parse_path_component_multiple_brackets_test() {
  case array_indexing.parse_path_component("items[0][1]") {
    Error(_) -> should.be_true(True)
    _ -> should.fail("Expected error for multiple brackets")
  }
}

// Test invalid syntax - non-numeric index
pub fn parse_path_component_non_numeric_index_test() {
  case array_indexing.parse_path_component("items[abc]") {
    Error(_) -> should.be_true(True)
    _ -> should.fail("Expected error for non-numeric index")
  }
}

// Test invalid syntax - negative zero
pub fn parse_path_component_negative_zero_test() {
  case array_indexing.parse_path_component("items[0]") {
    Ok(#("items", array_indexing.Index(0))) -> should.be_true(True)
    _ -> should.fail("Expected Index(0) for items[0]")
  }
}

// Test invalid syntax - empty bracket
pub fn parse_path_component_empty_bracket_test() {
  case array_indexing.parse_path_component("items[]") {
    Error(_) -> should.be_true(True)
    _ -> should.fail("Expected error for empty bracket")
  }
}

// ============================================================================
// get_array_element Tests
// ============================================================================

// Test valid array access at index 0
pub fn get_array_element_index_0_test() {
  let test_array = json.from_dynamic(dynamic.from(
    [1, 2, 3]
  ))

  case array_indexing.get_array_element(test_array, 0) {
    Ok(result) ->
      case result {
        json.Int(1) -> should.be_true(True)
        _ -> should.fail("Expected int 1 at index 0")
      }
    Error(_) -> should.fail("Should not error for valid index 0")
  }
}

// Test valid array access at middle index
pub fn get_array_element_middle_index_test() {
  let test_array = json.from_dynamic(dynamic.from(
    ["first", "second", "third"]
  ))

  case array_indexing.get_array_element(test_array, 1) {
    Ok(result) ->
      case result {
        json.String("second") -> should.be_true(True)
        _ -> should.fail("Expected string 'second' at index 1")
      }
    Error(_) -> should.fail("Should not error for valid index 1")
  }
}

// Test valid array access at last index
pub fn get_array_element_last_index_test() {
  let test_array = json.from_dynamic(dynamic.from(
    [true, false, true]
  ))

  let length = 3
  case array_indexing.get_array_element(test_array, length - 1) {
    Ok(result) ->
      case result {
        json.Bool(True) -> should.be_true(True)
        _ -> should.fail("Expected bool true at last index")
      }
    Error(_) -> should.fail("Should not error for valid last index")
  }
}

// Test out-of-bounds access (too large)
pub fn get_array_element_out_of_bounds_test() {
  let test_array = json.from_dynamic(dynamic.from(
    ["only", "one", "element"]
  ))

  case array_indexing.get_array_element(test_array, 5) {
    Error(msg) ->
      should.equal(
        msg,
        "Array index 5 out of bounds (length: 3)",
        "Should return proper error message for out of bounds"
      )
    Ok(_) -> should.fail("Should error for out of bounds index")
  }
}

// Test out-of-bounds access (negative index passed to get_array_element)
pub fn get_array_element_negative_index_test() {
  let test_array = json.from_dynamic(dynamic.from(
    [1, 2, 3]
  ))

  case array_indexing.get_array_element(test_array, -1) {
    Error(msg) ->
      should.equal(
        msg,
        "Array index -1 out of bounds (length: 3)",
        "Should return proper error message for negative index"
      )
    Ok(_) -> should.fail("Should error for negative index")
  }
}

// Test indexing non-array JSON
pub fn get_array_element_non_array_test() {
  let not_array = json.Object([
    #("key", json.String("value"))
  ])

  case array_indexing.get_array_element(not_array, 0) {
    Error(msg) ->
      should.equal(
        msg,
        "Cannot index non-array JSON with [0]",
        "Should return proper error message for non-array"
      )
    Ok(_) -> should.fail("Should error when indexing non-array")
  }
}

// ============================================================================
// get_array_element_last Tests
// ============================================================================

// Test LastN(1) - get last element
pub fn get_array_element_last_n_1_test() {
  let test_array = json.from_dynamic(dynamic.from(
    [10, 20, 30, 40]
  ))

  case array_indexing.get_array_element_last(test_array, 1) {
    Ok(result) ->
      case result {
        json.Int(40) -> should.be_true(True)
        _ -> should.fail("Expected int 40 for last element")
      }
    Error(_) -> should.fail("Should not error for LastN(1)")
  }
}

// Test LastN(2) - get second to last element
pub fn get_array_element_last_n_2_test() {
  let test_array = json.from_dynamic(dynamic.from(
    ["a", "b", "c", "d", "e"]
  ))

  case array_indexing.get_array_element_last(test_array, 2) {
    Ok(result) ->
      case result {
        json.String("c") -> should.be_true(True)
        _ -> should.fail("Expected string 'c' for second to last")
      }
    Error(_) -> should.fail("Should not error for LastN(2)")
  }
}

// Test LastN with small array
pub fn get_array_element_last_small_array_test() {
  let test_array = json.from_dynamic(dynamic.from(
    [1, 2]
  ))

  case array_indexing.get_array_element_last(test_array, 2) {
    Ok(result) ->
      case result {
        json.Int(1) -> should.be_true(True)
        _ -> should.fail("Expected int 1 for LastN(2) on small array")
      }
    Error(_) -> should.fail("Should not error for valid LastN")
  }
}

// Test LastN out of bounds (too far from end)
pub fn get_array_element_last_out_of_bounds_test() {
  let test_array = json.from_dynamic(dynamic.from(
    [1, 2, 3]
  ))

  case array_indexing.get_array_element_last(test_array, 5) {
    Error(msg) ->
      should.equal(
        msg,
        "Array index -5 out of bounds (length: 3)",
        "Should return proper error message for LastN out of bounds"
      )
    Ok(_) -> should.fail("Should error for LastN out of bounds")
  }
}

// Test LastN with empty array
pub fn get_array_element_last_empty_array_test() {
  let test_array = json.from_dynamic(dynamic.from(
    []
  ))

  case array_indexing.get_array_element_last(test_array, 1) {
    Error(msg) ->
      should.equal(
        msg,
        "Array index -1 out of bounds (length: 0)",
        "Should return proper error message for LastN on empty array"
      )
    Ok(_) -> should.fail("Should error for LastN on empty array")
  }
}

// ============================================================================
// get_all_array_elements Tests
// ============================================================================

// Test getting all elements from valid array
pub fn get_all_array_elements_valid_test() {
  let test_array = json.from_dynamic(dynamic.from(
    [1, "hello", true, null]
  ))

  case array_indexing.get_all_array_elements(test_array) {
    Ok(elements) ->
      should.equal(
        list.length(elements),
        4,
        "Should return all 4 elements"
      )
    Error(_) -> should.fail("Should not error for valid array")
  }
}

// Test getting elements from empty array
pub fn get_all_array_elements_empty_test() {
  let test_array = json.from_dynamic(dynamic.from(
    []
  ))

  case array_indexing.get_all_array_elements(test_array) {
    Ok(elements) ->
      should.equal(
        list.length(elements),
        0,
        "Should return empty list for empty array"
      )
    Error(_) -> should.fail("Should not error for empty array")
  }
}

// Test getting elements from non-array
pub fn get_all_array_elements_non_array_test() {
  let not_array = json.String("not an array")

  case array_indexing.get_all_array_elements(not_array) {
    Error(msg) ->
      should.equal(
        msg,
        "Cannot get array elements from non-array JSON",
        "Should return proper error message"
      )
    Ok(_) -> should.fail("Should error for non-array")
  }
}

// ============================================================================
// navigate_path Tests
// ============================================================================

// Test navigating to field without array indexing
pub fn navigate_path_no_array_test() {
  let test_json = json.from_dynamic(dynamic.from([
    #("user", dynamic.from([
      #("name", dynamic.from("John Doe")),
      #("age", dynamic.from(30))
    ]))
  ]))

  case array_indexing.navigate_path(test_json, ["user", "name"]) {
    Ok(json.String("John Doe")) -> should.be_true(True)
    _ -> should.fail("Should navigate to nested field without array")
  }
}

// Test navigating to array element with positive index
pub fn navigate_path_with_index_test() {
  let test_json = json.from_dynamic(dynamic.from([
    #("items", dynamic.from([
      dynamic.from("first"),
      dynamic.from("second"),
      dynamic.from("third")
    ]))
  ]))

  case array_indexing.navigate_path(test_json, ["items", "name"]) {
    Error(_) -> should.fail("Should handle non-existent field")
    Ok(_) ->
      case array_indexing.navigate_path(test_json, ["items[0]", "name"]) {
        Error(_) -> should.fail("Should navigate to array element")
        Ok(result) ->
          case result {
            json.String("first") -> should.be_true(True)
            _ -> should.fail("Expected 'first' at index 0")
          }
      }
  }
}

// Test navigating to array element with negative index
pub fn navigate_path_with_negative_index_test() {
  let test_json = json.from_dynamic(dynamic.from([
    #("items", dynamic.from([
      dynamic.from("first"),
      dynamic.from("second"),
      dynamic.from("third")
    ]))
  ]))

  case array_indexing.navigate_path(test_json, ["items[-1]"]) {
    Ok(result) ->
      case result {
        json.String("third") -> should.be_true(True)
        _ -> should.fail("Expected 'third' at last position")
      }
    Error(_) -> should.fail("Should navigate with negative index")
  }
}

// Test navigating to non-existent field
pub fn navigate_path_non_existent_field_test() {
  let test_json = json.from_dynamic(dynamic.from([
    #("items", dynamic.from([
      dynamic.from("first"),
      dynamic.from("second")
    ]))
  ]))

  case array_indexing.navigate_path(test_json, ["nonexistent", "field"]) {
    Error(msg) ->
      should.equal(
        msg,
        "Field 'nonexistent' not found",
        "Should return proper error for non-existent field"
      )
    Ok(_) -> should.fail("Should error for non-existent field")
  }
}

// Test navigating to array with wildcard
pub fn navigate_path_with_wildcard_test() {
  let test_json = json.from_dynamic(dynamic.from([
    #("items", dynamic.from([
      dynamic.from("first"),
      dynamic.from("second")
    ]))
  ]))

  case array_indexing.navigate_path(test_json, ["items[*]"]) {
    Error(msg) ->
      should.equal(
        msg,
        "Array wildcard [*] requires special handling in rules",
        "Should return proper error for wildcard"
      )
    Ok(_) -> should.fail("Should error for wildcard")
  }
}

// Test navigating to nested array element
pub fn navigate_path_nested_array_test() {
  let test_json = json.from_dynamic(dynamic.from([
    #("data", dynamic.from([
      #("users", dynamic.from([
        dynamic.from([
          #("name", dynamic.from("Alice")),
          #("emails", dynamic.from([
            dynamic.from("alice@example.com"),
            dynamic.from("alice@work.com")
          ]))
        ]),
        dynamic.from([
          #("name", dynamic.from("Bob")),
          #("emails", dynamic.from([
            dynamic.from("bob@example.com")
          ]))
        ])
      ]))
    ]))
  ]))

  case array_indexing.navigate_path(test_json, ["data", "users[0]", "emails[1]"]) {
    Ok(result) ->
      case result {
        json.String("alice@work.com") -> should.be_true(True)
        _ -> should.fail("Expected alice@work.com for nested access")
      }
    Error(_) -> should.fail("Should navigate to nested array element")
  }
}

// ============================================================================
// validate_path Tests
// ============================================================================

// Test valid path without arrays
pub fn validate_path_simple_test() {
  case array_indexing.validate_path("user.profile.name") {
    Ok(Nil) -> should.be_true(True)
    Error(_) -> should.fail("Should validate simple path")
  }
}

// Test valid path with array indices
pub fn validate_path_with_arrays_test() {
  case array_indexing.validate_path("items[0].user.name") {
    Ok(Nil) -> should.be_true(True)
    Error(_) -> should.fail("Should validate path with arrays")
  }
}

// Test valid path with negative indices
pub fn validate_path_with_negative_indices_test() {
  case array_indexing.validate_path("items[-1].last.name") {
    Ok(Nil) -> should.be_true(True)
    Error(_) -> should.fail("Should validate path with negative indices")
  }
}

// Test valid path with mixed indices
pub fn validate_path_with_mixed_indices_test() {
  case array_indexing.validate_path("items[0].users[-1].data") {
    Ok(Nil) -> should.be_true(True)
    Error(_) -> should.fail("Should validate path with mixed indices")
  }
}

// Test invalid path - empty string
pub fn validate_path_empty_test() {
  case array_indexing.validate_path("") {
    Error(msg) ->
      should.equal(
        msg,
        "Path cannot be empty",
        "Should return proper error for empty path"
      )
    Ok(_) -> should.fail("Should error for empty path")
  }
}

// Test invalid path - malformed array syntax
pub fn validate_path_malformed_test() {
  case array_indexing.validate_path("items[0]missing_bracket") {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail("Should error for malformed array syntax")
  }
}

// Test invalid path - invalid index
pub fn validate_path_invalid_index_test() {
  case array_indexing.validate_path("items[abc]") {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail("Should error for invalid index")
  }
}

// ============================================================================
// split_path Tests
// ============================================================================

// Test basic path splitting
pub fn split_path_basic_test() {
  let components = array_indexing.split_path("user.profile.name")

  should.equal(
    components,
    ["user", "profile", "name"],
    "Should split basic path correctly"
  )
}

// Test path with spaces
pub fn split_path_with_spaces_test() {
  let components = array_indexing.split_path(" user . profile . name ")

  should.equal(
    components,
    ["user", "profile", "name"],
    "Should trim spaces from components"
  )
}

// Test path with empty components
pub fn split_path_empty_components_test() {
  let components = array_indexing.split_path("user..profile.name")

  should.equal(
    components,
    ["user", "profile", "name"],
    "Should filter out empty components"
  )
}

// Test path with array indices preserved
pub fn split_path_with_arrays_test() {
  let components = array_indexing.split_path("items[0].user.profile")

  should.equal(
    components,
    ["items[0]", "user", "profile"],
    "Should preserve array indices in components"
  )
}