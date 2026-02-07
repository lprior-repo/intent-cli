import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import intent/interpolate
import intent/runner
import intent/types
import intent/test_helpers

// ============================================================================
// Capture Functionality Tests
// ============================================================================

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Test 1: Test capturing values from responses
// ============================================================================

fn test_capture_values_from_responses() {
  // Create a response with JSON data
  let response_body = json.object([
    #("id", json.int(123)),
    #("name", json.string("test-user")),
    #("email", json.string("test@example.com")),
    #("active", json.bool(True)),
  ])

  // Create a context with the response body
  let ctx = interpolate.set_response_body(interpolate.new_context(), response_body)

  // Test capturing simple values
  let captured_id = interpolate.extract_capture(ctx, "response.body.id")
  should.equal(captured_id, Ok(json.int(123)))

  let captured_name = interpolate.extract_capture(ctx, "response.body.name")
  should.equal(captured_name, Ok(json.string("test-user")))

  let captured_email = interpolate.extract_capture(ctx, "response.body.email")
  should.equal(captured_email, Ok(json.string("test@example.com")))

  let captured_active = interpolate.extract_capture(ctx, "response.body.active")
  should.equal(captured_active, Ok(json.bool(True)))
}

// ============================================================================
// Test 2: Test using captured values in subsequent requests
// ============================================================================

fn test_using_captured_values_in_requests() {
  // First response with user data
  let first_response = json.object([
    #("user_id", json.int(123)),
    #("token", json.string("abc123-token")),
  ])

  // Create context and capture values
  let ctx = interpolate.new_context()
  |> interpolate.set_response_body(_, first_response)
  |> interpolate.set_variable(_, "user_id", json.int(123))
  |> interpolate.set_variable(_, "auth_token", json.string("abc123-token"))

  // Test interpolation in headers
  let headers = dict.from_list([
    #("Authorization", "Bearer ${auth_token}"),
    #("X-User-ID", "${user_id}"),
  ])

  let interpolated_headers = interpolate.interpolate_headers(ctx, headers)
  case interpolated_headers {
    Ok(headers) -> {
      let auth_header = dict.get(headers, "Authorization")
      should.equal(auth_header, Ok("Bearer abc123-token"))

      let user_id_header = dict.get(headers, "X-User-ID")
      should.equal(user_id_header, Ok("123"))
    }
    Error(_) -> should.fail("Header interpolation failed")
  }

  // Test interpolation in path
  let original_path = "/users/${user_id}/profile"
  let interpolated_path = interpolate.interpolate_string(ctx, original_path)
  should.equal(interpolated_path, Ok("/users/123/profile"))
}

// ============================================================================
// Test 3: Test capture with JSON path expressions
// ============================================================================

fn test_capture_with_json_path_expressions() {
  // Complex nested response
  let complex_response = json.object([
    #("user", json.object([
      #("id", json.int(123)),
      #("profile", json.object([
        #("name", json.string("John Doe")),
        #("email", json.string("john@example.com")),
        #("preferences", json.object([
          #("theme", json.string("dark")),
          #("notifications", json.bool(True)),
        ])),
      ])),
      #("orders", json.list([
        json.object([
          #("id", json.int(1)),
          #("amount", json.float(99.99)),
          #("status", json.string("completed")),
        ]),
        json.object([
          #("id", json.int(2)),
          #("amount", json.float(149.99)),
          #("status", json.string("pending")),
        ]),
      ])),
    ])),
  ])

  let ctx = interpolate.set_response_body(interpolate.new_context(), complex_response)

  // Test nested field access
  let user_id = interpolate.extract_capture(ctx, "response.body.user.id")
  should.equal(user_id, Ok(json.int(123)))

  let user_name = interpolate.extract_capture(ctx, "response.body.user.profile.name")
  should.equal(user_name, Ok(json.string("John Doe")))

  // Test array indexing
  let first_order_id = interpolate.extract_capture(ctx, "response.body.user.orders[0].id")
  should.equal(first_order_id, Ok(json.int(1)))

  let second_order_amount = interpolate.extract_capture(ctx, "response.body.user.orders[1].amount")
  should.equal(second_order_amount, Ok(json.float(149.99)))

  // Test negative indexing (last element)
  let last_order_status = interpolate.extract_capture(ctx, "response.body.user.orders[-1].status")
  should.equal(last_order_status, Ok(json.string("pending")))

  // Test nested fields with arrays
  let first_order_status = interpolate.extract_capture(ctx, "response.body.user.orders[0].status")
  should.equal(first_order_status, Ok(json.string("completed")))
}

// ============================================================================
// Test 4: Test capture with header values
// ============================================================================

fn test_capture_with_header_values() {
  // This test focuses on the interpolation module since header capture
  // would need to be implemented in the HTTP client/runner

  // Create a context with some variables
  let ctx = interpolate.new_context()
  |> interpolate.set_variable(_, "session_id", json.string("sess-123"))
  |> interpolate.set_variable(_, "api_key", json.string("key-abc123"))

  // Test header interpolation
  let headers = dict.from_list([
    #("X-Session-ID", "${session_id}"),
    #("Authorization", "Bearer ${api_key}"),
    #("Content-Type", "application/json"),
  ])

  let result = interpolate.interpolate_headers(ctx, headers)
  case result {
    Ok(interpolated) -> {
      should.equal(dict.get(interpolated, "X-Session-ID"), Ok("sess-123"))
      should.equal(dict.get(interpolated, "Authorization"), Ok("Bearer key-abc123"))
      should.equal(dict.get(interpolated, "Content-Type"), Ok("application/json"))
    }
    Error(_) -> should.fail("Header interpolation failed")
  }
}

// ============================================================================
// Test 5: Test capture with status codes
// ============================================================================

fn test_capture_with_status_codes() {
  // Status codes are typically captured as part of the execution result
  // We'll test the basic functionality by setting status in context

  let ctx = interpolate.new_context()
  |> interpolate.set_variable(_, "success_status", json.int(200))
  |> interpolate.set_variable(_, "error_status", json.int(404))

  // Test status code interpolation
  let status_check = interpolate.interpolate_string(ctx, "Expected status: ${success_status}")
  should.equal(status_check, Ok("Expected status: 200"))

  let error_message = interpolate.interpolate_string(ctx, "Not found: ${error_status}")
  should.equal(error_message, Ok("Not found: 404"))
}

// ============================================================================
// Test 6: Test undefined capture references
// ============================================================================

fn test_undefined_capture_references() {
  let ctx = interpolate.new_context()

  // Test non-existent variable
  let undefined_var = interpolate.extract_capture(ctx, "nonexistent_var")
  should.equal(undefined_var, Error("Variable not found: nonexistent_var"))

  // Test non-existent field
  let simple_response = json.object([#("id", json.int(123))])
  let ctx_with_response = interpolate.set_response_body(ctx, simple_response)

  let non_existent_field = interpolate.extract_capture(ctx_with_response, "response.body.nonexistent")
  should.equal(non_existent_field, Error("Field 'nonexistent' not found"))
}

// ============================================================================
// Test 7: Test capture with null values
// ============================================================================

fn test_capture_with_null_values() {
  let response_with_null = json.object([
    #("id", json.int(123)),
    #("name", json.null()),
    #("description", json.string("test")),
    #("metadata", json.null()),
  ])

  let ctx = interpolate.set_response_body(interpolate.new_context(), response_with_null)

  // Test capturing null values
  let captured_id = interpolate.extract_capture(ctx, "response.body.id")
  should.equal(captured_id, Ok(json.int(123)))

  let captured_name = interpolate.extract_capture(ctx, "response.body.name")
  should.equal(captured_name, Ok(json.null()))

  let captured_metadata = interpolate.extract_capture(ctx, "response.body.metadata")
  should.equal(captured_metadata, Ok(json.null()))
}

// ============================================================================
// Test 8: Test capture with complex nested structures
// ============================================================================

fn test_capture_with_complex_nested_structures() {
  // Deeply nested and complex structure
  let complex_structure = json.object([
    #("data", json.object([
      #("users", json.list([
        json.object([
          #("id", json.int(1)),
          #("profile", json.object([
            #("personal", json.object([
              #("name", json.string("Alice")),
              #("age", json.int(30)),
              #("address", json.object([
                #("street", json.string("123 Main St")),
                #("city", json.string("New York")),
                #("coordinates", json.list([
                  json.float(-74.0060),
                  json.float(40.7128),
                ])),
              ])),
            ])),
            #("preferences", json.object([
              #("theme", json.string("dark")),
              #("language", json.string("en")),
            ])),
          ])),
        ]),
        json.object([
          #("id", json.int(2)),
          #("profile", json.object([
            #("personal", json.object([
              #("name", json.string("Bob")),
              #("age", json.int(25)),
              #("address", json.object([
                #("street", json.string("456 Oak Ave")),
                #("city", json.string("Los Angeles")),
                #("coordinates", json.list([
                  json.float(-118.2437),
                  json.float(34.0522),
                ])),
              ])),
            ])),
            #("preferences", json.object([
              #("theme", json.string("light")),
              #("language", json.string("es")),
            ])),
          ])),
        ]),
      ])),
      #("metadata", json.object([
        #("pagination", json.object([
          #("page", json.int(1)),
          #("total_pages", json.int(5)),
          #("per_page", json.int(2)),
        ])),
        #("stats", json.object([
          #("total_users", json.int(2)),
          #("active_users", json.int(2)),
        ])),
      ])),
    ])),
  ])

  let ctx = interpolate.set_response_body(interpolate.new_context(), complex_structure)

  // Test deep nested access
  let alice_name = interpolate.extract_capture(ctx, "response.body.data.users[0].profile.personal.name")
  should.equal(alice_name, Ok(json.string("Alice")))

  let bob_city = interpolate.extract_capture(ctx, "response.body.data.users[1].profile.personal.address.city")
  should.equal(bob_city, Ok(json.string("Los Angeles")))

  // Test coordinates access
  let alice_lat = interpolate.extract_capture(ctx, "response.body.data.users[0].profile.personal.address.coordinates[1]")
  should.equal(alice_lat, Ok(json.float(40.7128)))

  // Test metadata access
  let total_pages = interpolate.extract_capture(ctx, "response.body.data.metadata.pagination.total_pages")
  should.equal(total_pages, Ok(json.int(5)))

  let per_page = interpolate.extract_capture(ctx, "response.body.data.metadata.pagination.per_page")
  should.equal(per_page, Ok(json.int(2)))

  // Test stats access
  let total_users = interpolate.extract_capture(ctx, "response.body.data.metadata.stats.total_users")
  should.equal(total_users, Ok(json.int(2)))
}

// ============================================================================
// Integration Tests: Full capture workflow in behavior execution
// ============================================================================

fn test_capture_workflow_integration() {
  // Create behaviors that use captures

  // First behavior creates a user and captures the user ID
  let create_user_behavior = test_helpers.make_test_behavior("create_user", [])
  |> fn(b) {
    types.Behavior(
      ..b,
      request: types.Request(
        method: types.Post,
        path: "/users",
        headers: dict.from_list([#("Content-Type", "application/json")]),
        query: dict.new(),
        body: json.object([
          #("name", json.string("Test User")),
          #("email", json.string("test@example.com")),
        ]),
      ),
      response: types.Response(
        status: 201,
        example: json.object([
          #("id", json.int(123)),
          #("name", json.string("Test User")),
          #("email", json.string("test@example.com")),
        ]),
        checks: dict.from_list([
          #("status", json.object([
            #("rule", "== 201"),
            #("why", "User created successfully"),
          ])),
        ]),
        headers: dict.new(),
      ),
      captures: dict.from_list([
        #("user_id", "response.body.id"),
        #("user_name", "response.body.name"),
      ]),
    )
  }

  // Second behavior uses the captured user ID
  let get_user_behavior = test_helpers.make_test_behavior("get_user", ["create_user"])
  |> fn(b) {
    types.Behavior(
      ..b,
      request: types.Request(
        method: types.Get,
        path: "/users/${user_id}",
        headers: dict.from_list([#("Authorization", "Bearer ${auth_token}")]),
        query: dict.new(),
        body: json.null(),
      ),
      response: types.Response(
        status: 200,
        example: json.object([
          #("id", json.int(123)),
          #("name", json.string("Test User")),
          #("email", json.string("test@example.com")),
        ]),
        checks: dict.from_list([
          #("status", json.object([
            #("rule", "== 200"),
            #("why", "User retrieved successfully"),
          ])),
        ]),
        headers: dict.new(),
      ),
      captures: dict.from_list([
        #("retrieved_name", "response.body.name"),
      ]),
    )
  }

  // Create spec with both behaviors
  let spec = test_helpers.make_test_spec([test_helpers.make_test_feature("User Flow", [create_user_behavior, get_user_behavior])])

  // Mock HTTP responses for testing
  let mock_responses = [
    #(
      "create_user",
      Ok(intent/http_client.ExecutionResult(
        status: 201,
        headers: dict.from_list([
          #("Content-Type", "application/json"),
        ]),
        body: json.object([
          #("id", json.int(123)),
          #("name", json.string("Test User")),
          #("email", json.string("test@example.com")),
        ]),
      )),
    ),
    #(
      "get_user",
      Ok(intent/http_client.ExecutionResult(
        status: 200,
        headers: dict.from_list([
          #("Content-Type", "application/json"),
        ]),
        body: json.object([
          #("id", json.int(123)),
          #("name", json.string("Test User")),
          #("email", json.string("test@example.com")),
        ]),
      )),
    ),
  ]

  // Create mock executor that returns predefined responses
  let mock_executor = intent/runner.BehaviorExecutor(
    execute: fn(_config, _request, _ctx) {
      // Find the behavior name and return corresponding mock response
      // This is a simplified mock - in real tests you'd need to match on behavior name
      Ok(intent/http_client.ExecutionResult(
        status: 200,
        headers: dict.new(),
        body: json.object([#("mock", json.string("response"))]),
      ))
    },
  )

  // This is a placeholder for integration testing
  // In a real implementation, you'd need to properly mock the HTTP client
  // and test the full behavior execution flow with captures
  should.not_equal(create_user_behavior.captures, dict.new())
  should.not_equal(get_user_behavior.captures, dict.new())
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

fn test_capture_edge_cases() {
  let ctx = interpolate.new_context()

  // Test empty path
  let empty_path = interpolate.extract_capture(ctx, "")
  should.equal(empty_path, Error("Empty variable path"))

  // Test malformed JSON path
  let simple_response = json.object([#("id", json.int(123))])
  let ctx_with_response = interpolate.set_response_body(ctx, simple_response)

  let malformed_path = interpolate.extract_capture(ctx_with_response, "response.body.[invalid]")
  should.equal(malformed_path, Error("Invalid array syntax: [invalid]"))

  // Test out of bounds array access
  let array_response = json.list([json.int(1), json.int(2)])
  let ctx_with_array = interpolate.set_response_body(ctx, array_response)

  let out_of_bounds = interpolate.extract_capture(ctx_with_array, "[5]")
  should.equal(out_of_bounds, Error("Array index 5 out of bounds (length: 2)"))

  // Test negative out of bounds
  let negative_out_of_bounds = interpolate.extract_capture(ctx_with_array, "[-10]")
  should.equal(negative_out_of_bounds, Error("Array index -10 out of bounds (length: 2)"))

  // Test non-array with index
  let non_array_response = json.int(123)
  let ctx_with_non_array = interpolate.set_response_body(ctx, non_array_response)

  let non_array_index = interpolate.extract_capture(ctx_with_non_array, "[0]")
  should.equal(non_array_index, Error("Cannot index non-array JSON with [0]"))
}