/// Comprehensive tests for O(n) JSON field navigation
/// Tests the get_field_value function in checker/json.gleam
import gleam/json
import gleam/option.{None, Some}
import gleeunit/should
import intent/checker/json as field_json

// =============================================================================
// Empty Path Tests
// =============================================================================

pub fn empty_path_returns_original_value_test() {
  let body = json.object([#("name", json.string("Alice"))])

  let result = field_json.get_field_value(body, "")

  result |> should.equal(Some(body))
}

pub fn empty_path_with_complex_object_test() {
  let body =
    json.object([
      #("users", json.array([json.string("a"), json.string("b")], fn(x) { x })),
      #("count", json.int(2)),
    ])

  let result = field_json.get_field_value(body, "")

  result |> should.equal(Some(body))
}

// =============================================================================
// Single Key Access Tests
// =============================================================================

pub fn single_key_string_value_test() {
  let body = json.object([#("name", json.string("Alice"))])

  let result = field_json.get_field_value(body, "name")

  result |> should.equal(Some(json.string("Alice")))
}

pub fn single_key_int_value_test() {
  let body = json.object([#("age", json.int(30))])

  let result = field_json.get_field_value(body, "age")

  result |> should.equal(Some(json.int(30)))
}

pub fn single_key_bool_value_test() {
  let body = json.object([#("active", json.bool(True))])

  let result = field_json.get_field_value(body, "active")

  result |> should.equal(Some(json.bool(True)))
}

pub fn single_key_null_value_test() {
  let body = json.object([#("deleted_at", json.null())])

  let result = field_json.get_field_value(body, "deleted_at")

  result |> should.equal(Some(json.null()))
}

pub fn single_key_array_value_test() {
  let arr = json.array([json.int(1), json.int(2), json.int(3)], fn(x) { x })
  let body = json.object([#("items", arr)])

  let result = field_json.get_field_value(body, "items")

  result |> should.equal(Some(arr))
}

pub fn single_key_object_value_test() {
  let nested = json.object([#("inner", json.string("value"))])
  let body = json.object([#("data", nested)])

  let result = field_json.get_field_value(body, "data")

  result |> should.equal(Some(nested))
}

pub fn single_key_missing_returns_none_test() {
  let body = json.object([#("name", json.string("Alice"))])

  let result = field_json.get_field_value(body, "missing")

  result |> should.equal(None)
}

// =============================================================================
// Nested Path Tests (2 levels)
// =============================================================================

pub fn two_level_path_test() {
  let body =
    json.object([#("user", json.object([#("name", json.string("Alice"))]))])

  let result = field_json.get_field_value(body, "user.name")

  result |> should.equal(Some(json.string("Alice")))
}

pub fn two_level_path_missing_first_key_test() {
  let body =
    json.object([#("user", json.object([#("name", json.string("Alice"))]))])

  let result = field_json.get_field_value(body, "missing.name")

  result |> should.equal(None)
}

pub fn two_level_path_missing_second_key_test() {
  let body =
    json.object([#("user", json.object([#("name", json.string("Alice"))]))])

  let result = field_json.get_field_value(body, "user.missing")

  result |> should.equal(None)
}

// =============================================================================
// Deeply Nested Path Tests (3+ levels)
// =============================================================================

pub fn three_level_path_test() {
  let body =
    json.object([
      #(
        "data",
        json.object([
          #("user", json.object([#("email", json.string("alice@example.com"))])),
        ]),
      ),
    ])

  let result = field_json.get_field_value(body, "data.user.email")

  result |> should.equal(Some(json.string("alice@example.com")))
}

pub fn four_level_path_test() {
  let body =
    json.object([
      #(
        "response",
        json.object([
          #(
            "data",
            json.object([
              #(
                "user",
                json.object([
                  #(
                    "profile",
                    json.object([
                      #("avatar", json.string("https://example.com/avatar.png")),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ])

  let result = field_json.get_field_value(body, "response.data.user.profile")

  case result {
    Some(value) -> {
      // Verify we got the profile object
      let expected =
        json.object([#("avatar", json.string("https://example.com/avatar.png"))])
      value |> should.equal(expected)
    }
    None -> should.fail()
  }
}

pub fn five_level_path_test() {
  let body =
    json.object([
      #(
        "a",
        json.object([
          #(
            "b",
            json.object([
              #(
                "c",
                json.object([
                  #("d", json.object([#("e", json.string("deep value"))])),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ])

  let result = field_json.get_field_value(body, "a.b.c.d.e")

  result |> should.equal(Some(json.string("deep value")))
}

pub fn deep_path_missing_intermediate_test() {
  let body =
    json.object([
      #("a", json.object([#("b", json.object([#("c", json.string("value"))]))])),
    ])

  // "x" doesn't exist at level 2
  let result = field_json.get_field_value(body, "a.x.c")

  result |> should.equal(None)
}

// =============================================================================
// Array Index Tests
// =============================================================================

pub fn array_first_element_test() {
  let body =
    json.object([
      #(
        "items",
        json.array(
          [json.string("first"), json.string("second"), json.string("third")],
          fn(x) { x },
        ),
      ),
    ])

  let result = field_json.get_field_value(body, "items.0")

  result |> should.equal(Some(json.string("first")))
}

pub fn array_second_element_test() {
  let body =
    json.object([
      #(
        "items",
        json.array(
          [json.string("first"), json.string("second"), json.string("third")],
          fn(x) { x },
        ),
      ),
    ])

  let result = field_json.get_field_value(body, "items.1")

  result |> should.equal(Some(json.string("second")))
}

pub fn array_out_of_bounds_test() {
  let body =
    json.object([#("items", json.array([json.string("first")], fn(x) { x }))])

  let result = field_json.get_field_value(body, "items.5")

  result |> should.equal(None)
}

pub fn array_then_object_field_test() {
  let body =
    json.object([
      #(
        "users",
        json.array(
          [
            json.object([#("name", json.string("Alice"))]),
            json.object([#("name", json.string("Bob"))]),
          ],
          fn(x) { x },
        ),
      ),
    ])

  let result = field_json.get_field_value(body, "users.0.name")

  result |> should.equal(Some(json.string("Alice")))
}

pub fn array_nested_deep_test() {
  let body =
    json.object([
      #(
        "data",
        json.object([
          #(
            "results",
            json.array(
              [
                json.object([
                  #(
                    "items",
                    json.array([json.object([#("id", json.int(42))])], fn(x) {
                      x
                    }),
                  ),
                ]),
              ],
              fn(x) { x },
            ),
          ),
        ]),
      ),
    ])

  let result = field_json.get_field_value(body, "data.results.0.items.0.id")

  result |> should.equal(Some(json.int(42)))
}

// =============================================================================
// Numeric Keys on Objects (edge case)
// =============================================================================

pub fn object_with_numeric_string_key_test() {
  // Objects can have "0" as a string key
  let body =
    json.object([#("0", json.string("zero")), #("1", json.string("one"))])

  let result = field_json.get_field_value(body, "0")

  result |> should.equal(Some(json.string("zero")))
}

pub fn nested_object_with_numeric_key_test() {
  let body =
    json.object([#("data", json.object([#("0", json.string("first item"))]))])

  let result = field_json.get_field_value(body, "data.0")

  result |> should.equal(Some(json.string("first item")))
}

// =============================================================================
// Type Mismatch Tests
// =============================================================================

pub fn navigate_into_string_fails_test() {
  let body = json.object([#("name", json.string("Alice"))])

  // Can't navigate into a string
  let result = field_json.get_field_value(body, "name.foo")

  result |> should.equal(None)
}

pub fn navigate_into_int_fails_test() {
  let body = json.object([#("count", json.int(42))])

  let result = field_json.get_field_value(body, "count.value")

  result |> should.equal(None)
}

pub fn navigate_into_bool_fails_test() {
  let body = json.object([#("active", json.bool(True))])

  let result = field_json.get_field_value(body, "active.status")

  result |> should.equal(None)
}

pub fn navigate_into_null_fails_test() {
  let body = json.object([#("data", json.null())])

  let result = field_json.get_field_value(body, "data.value")

  result |> should.equal(None)
}

// =============================================================================
// Float Value Tests
// =============================================================================

pub fn float_value_test() {
  let body = json.object([#("price", json.float(19.99))])

  let result = field_json.get_field_value(body, "price")

  result |> should.equal(Some(json.float(19.99)))
}

pub fn nested_float_value_test() {
  let body =
    json.object([#("product", json.object([#("price", json.float(29.99))]))])

  let result = field_json.get_field_value(body, "product.price")

  result |> should.equal(Some(json.float(29.99)))
}

// =============================================================================
// Special String Values Tests
// =============================================================================

pub fn string_with_dots_test() {
  // The value contains dots, but key navigation should still work
  let body = json.object([#("email", json.string("alice.bob@example.com"))])

  let result = field_json.get_field_value(body, "email")

  result |> should.equal(Some(json.string("alice.bob@example.com")))
}

pub fn string_with_special_chars_test() {
  let body = json.object([#("message", json.string("Hello\nWorld\t!"))])

  let result = field_json.get_field_value(body, "message")

  result |> should.equal(Some(json.string("Hello\nWorld\t!")))
}

pub fn empty_string_value_test() {
  let body = json.object([#("empty", json.string(""))])

  let result = field_json.get_field_value(body, "empty")

  result |> should.equal(Some(json.string("")))
}

// =============================================================================
// Complex Nested Array Tests
// =============================================================================

pub fn array_of_arrays_test() {
  let body =
    json.object([
      #(
        "matrix",
        json.array(
          [
            json.array([json.int(1), json.int(2)], fn(x) { x }),
            json.array([json.int(3), json.int(4)], fn(x) { x }),
          ],
          fn(x) { x },
        ),
      ),
    ])

  let result = field_json.get_field_value(body, "matrix.0.1")

  result |> should.equal(Some(json.int(2)))
}

pub fn deeply_nested_array_test() {
  let body =
    json.object([
      #(
        "level1",
        json.array(
          [
            json.object([
              #(
                "level2",
                json.array([json.object([#("value", json.int(999))])], fn(x) {
                  x
                }),
              ),
            ]),
          ],
          fn(x) { x },
        ),
      ),
    ])

  let result = field_json.get_field_value(body, "level1.0.level2.0.value")

  result |> should.equal(Some(json.int(999)))
}

// =============================================================================
// Empty Container Tests
// =============================================================================

pub fn empty_object_value_test() {
  let body = json.object([#("data", json.object([]))])

  let result = field_json.get_field_value(body, "data")

  result |> should.equal(Some(json.object([])))
}

pub fn empty_array_value_test() {
  let body = json.object([#("items", json.array([], fn(x) { x }))])

  let result = field_json.get_field_value(body, "items")

  result |> should.equal(Some(json.array([], fn(x) { x })))
}

pub fn access_into_empty_array_fails_test() {
  let body = json.object([#("items", json.array([], fn(x) { x }))])

  let result = field_json.get_field_value(body, "items.0")

  result |> should.equal(None)
}

pub fn access_into_empty_object_fails_test() {
  let body = json.object([#("data", json.object([]))])

  let result = field_json.get_field_value(body, "data.foo")

  result |> should.equal(None)
}

// =============================================================================
// Root-level Non-Object Tests
// =============================================================================

pub fn root_array_index_access_test() {
  let body =
    json.array([json.string("first"), json.string("second")], fn(x) { x })

  let result = field_json.get_field_value(body, "0")

  result |> should.equal(Some(json.string("first")))
}

pub fn root_array_nested_access_test() {
  let body =
    json.array(
      [
        json.object([#("name", json.string("Alice"))]),
        json.object([#("name", json.string("Bob"))]),
      ],
      fn(x) { x },
    )

  let result = field_json.get_field_value(body, "1.name")

  result |> should.equal(Some(json.string("Bob")))
}

// =============================================================================
// Stress Test - Very Deep Nesting
// =============================================================================

pub fn ten_level_deep_path_test() {
  // Build a 10-level deep structure: a.b.c.d.e.f.g.h.i.j
  let body =
    json.object([
      #(
        "a",
        json.object([
          #(
            "b",
            json.object([
              #(
                "c",
                json.object([
                  #(
                    "d",
                    json.object([
                      #(
                        "e",
                        json.object([
                          #(
                            "f",
                            json.object([
                              #(
                                "g",
                                json.object([
                                  #(
                                    "h",
                                    json.object([
                                      #(
                                        "i",
                                        json.object([
                                          #("j", json.string("deep!")),
                                        ]),
                                      ),
                                    ]),
                                  ),
                                ]),
                              ),
                            ]),
                          ),
                        ]),
                      ),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ])

  let result = field_json.get_field_value(body, "a.b.c.d.e.f.g.h.i.j")

  result |> should.equal(Some(json.string("deep!")))
}
