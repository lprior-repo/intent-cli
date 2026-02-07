import gleam/dict
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleeunit/should
import intent/http_client
import intent/interpolate
import intent/types

/// Test HTTP methods: GET requests
pub fn test_get_request() {
  // Test with a real external API
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/get",
      headers: dict.from_list([#("user-agent", "intent-cli-test")]),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      // Verify response
      execution_result.status |> should.equal(200)
      string.contains(execution_result.raw_body, "\"url\":") |> should.be_true()
      execution_result.request_method |> should.equal(types.Get)
      execution_result.elapsed_ms |> should.be_greater_than(0)
    }
    Error(_) -> should.fail("GET request should succeed")
  }
}

/// Test HTTP methods: POST with JSON body
pub fn test_post_request() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request_body =
    json.object([
      #("name", json.string("test-user")),
      #("email", json.string("test@example.com")),
      #("age", json.int(25)),
    ])

  let request =
    types.Request(
      method: types.Post,
      path: "/post",
      headers: dict.new(),
      query: dict.new(),
      body: request_body,
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(200)
      string.contains(execution_result.raw_body, "\"json\":") |> should.be_true()
      string.contains(execution_result.raw_body, "test-user") |> should.be_true()
      execution_result.request_method |> should.equal(types.Post)
    }
    Error(_) -> should.fail("POST request should succeed")
  }
}

/// Test HTTP methods: PUT requests
pub fn test_put_request() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request_body =
    json.object([
      #("id", json.int(1)),
      #("updated", json.bool(True)),
      #("data", json.string("put-test")),
    ])

  let request =
    types.Request(
      method: types.Put,
      path: "/put",
      headers: dict.new(),
      query: dict.new(),
      body: request_body,
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(200)
      string.contains(execution_result.raw_body, "\"json\":") |> should.be_true()
      string.contains(execution_result.raw_body, "put-test") |> should.be_true()
      execution_result.request_method |> should.equal(types.Put)
    }
    Error(_) -> should.fail("PUT request should succeed")
  }
}

/// Test HTTP methods: DELETE requests
pub fn test_delete_request() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Delete,
      path: "/delete",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(200)
      string.contains(execution_result.raw_body, "\"deleted\":") |> should.be_true()
      execution_result.request_method |> should.equal(types.Delete)
    }
    Error(_) -> should.fail("DELETE request should succeed")
  }
}

/// Test HTTP methods: PATCH requests
pub fn test_patch_request() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request_body =
    json.object([
      #("patch_field", json.string("patched")),
    ])

  let request =
    types.Request(
      method: types.Patch,
      path: "/patch",
      headers: dict.new(),
      query: dict.new(),
      body: request_body,
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(200)
      string.contains(execution_result.raw_body, "\"json\":") |> should.be_true()
      execution_result.request_method |> should.equal(types.Patch)
    }
    Error(_) -> should.fail("PATCH request should succeed")
  }
}

/// Test headers handling
pub fn test_headers_handling() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.from_list([#("authorization", "Bearer token123")]),
    )

  let request_headers = dict.from_list([
    #("content-type", "application/json"),
    #("x-custom-header", "test-value"),
  ])

  let request =
    types.Request(
      method: types.Get,
      path: "/headers",
      headers: request_headers,
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(200)
      // Check that both config and request headers are present
      let raw_headers = execution_result.raw_body
      string.contains(raw_headers, "Bearer token123") |> should.be_true()
      string.contains(raw_headers, "x-custom-header") |> should.be_true()
      string.contains(raw_headers, "application/json") |> should.be_true()
    }
    Error(_) -> should.fail("Headers handling should work")
  }
}

/// Test timeout behavior with slow endpoint
pub fn test_timeout_behavior() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 100, // Very short timeout
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/delay/2", // This endpoint waits 2 seconds
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  // Should fail due to timeout
  case result {
    Error(http_client.RequestError(msg)) ->
      string.contains(msg, "timeout") |> should.be_true()
    _ -> should.fail("Should timeout with a short timeout")
  }
}

/// Test error handling: Invalid URLs
pub fn test_invalid_url_error() {
  let config =
    types.Config(
      base_url: "invalid-url",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/test",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Error(http_client.UrlParseError(_)) -> Nil // Test passes
    _ -> should.fail("Should fail with invalid URL")
  }
}

/// Test error handling: Non-200 responses
pub fn test_non_200_response() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/status/404",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  // The client doesn't filter non-200 responses, they should be returned as successful
  case result {
    Ok(execution_result) ->
      execution_result.status |> should.equal(404)
    Error(_) -> should.fail("Non-200 responses should be returned")
  }
}

/// Test interpolation in path
pub fn test_path_interpolation() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/users/{{user_id}}",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.from_list([#("user_id", json.string("123"))]),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) ->
      execution_result.status |> should.equal(200)
      string.contains(execution_result.raw_body, "\"url\":") |> should.be_true()
    Error(_) -> should.fail("Path interpolation should work")
  }
}

/// Test interpolation in headers
pub fn test_headers_interpolation() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.from_list([#("api-key", "{{api_key}}")]),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/headers",
      headers: dict.from_list([#("x-auth-token", "{{auth_token}}")]),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.from_list([
        #("api_key", json.string("secret123")),
        #("auth_token", json.string("token456")),
      ]),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) ->
      execution_result.status |> should.equal(200)
      let raw_headers = execution_result.raw_body
      string.contains(raw_headers, "secret123") |> should.be_true()
      string.contains(raw_headers, "token456") |> should.be_true()
    Error(_) -> should.fail("Headers interpolation should work")
  }
}

/// Test interpolation in body
pub fn test_body_interpolation() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request_body =
    json.object([
      #("user_id", json.string("{{user_id}}")),
      #("message", json.string("Hello {{name}}!")),
      #("timestamp", json.string("{{timestamp}}")),
    ])

  let request =
    types.Request(
      method: types.Post,
      path: "/post",
      headers: dict.new(),
      query: dict.new(),
      body: request_body,
    )

  let ctx =
    interpolate.Context(
      variables: dict.from_list([
        #("user_id", json.string("789")),
        #("name", json.string("World")),
        #("timestamp", json.string("2023-01-01T00:00:00Z")),
      ]),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(200)
      let raw_body = execution_result.raw_body
      string.contains(raw_body, "\"user_id\": \"789\"") |> should.be_true()
      string.contains(raw_body, "\"message\": \"Hello World!\"") |> should.be_true()
    }
    Error(_) -> should.fail("Body interpolation should work")
  }
}

/// Test empty response body
pub fn test_empty_response_body() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/status/204", // No Content
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(204)
      execution_result.body |> should.equal(json.null())
      let empty_check = string.is_empty(execution_result.raw_body)
      empty_check |> should.be_true()
    }
    Error(_) -> should.fail("Empty response body should work")
  }
}

/// Test JSON parsing error in response
pub fn test_json_parsing_error() {
  // This test requires a mock server that returns invalid JSON
  // For now, we'll test the path interpolation and response structure

  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/html", // Returns HTML, not JSON
      headers: dict.from_list([#("accept", "text/html")]),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  // This will fail with ResponseParseError since it tries to parse HTML as JSON
  case result {
    Error(http_client.ResponseParseError(_)) -> Nil // Test passes
    _ -> should.fail("Should fail when response is not valid JSON")
  }
}

/// Test query parameters
pub fn test_query_parameters() {
  let config =
    types.Config(
      base_url: "https://httpbin.org",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let query = dict.from_list([
    #("param1", "value1"),
    #("param2", "value2"),
    #("number", "123"),
  ])

  let request =
    types.Request(
      method: types.Get,
      path: "/get",
      headers: dict.new(),
      query: query,
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  case result {
    Ok(execution_result) -> {
      execution_result.status |> should.equal(200)
      raw_body = execution_result.raw_body
      string.contains(raw_body, "\"param1\": \"value1\"") |> should.be_true()
      string.contains(raw_body, "\"param2\": \"value2\"") |> should.be_true()
      string.contains(raw_body, "\"number\": 123") |> should.be_true()
    }
    Error(_) -> should.fail("Query parameters should work")
  }
}

/// Test connection refused error (using non-existent host)
pub fn test_connection_refused_error() {
  let config =
    types.Config(
      base_url: "http://localhost:9999", // Unlikely to be listening
      timeout_ms: 1000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/test",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = http_client.execute_request(config, request, ctx)

  // Should fail with connection refused error
  case result {
    Error(http_client.RequestError(msg)) ->
      string.contains(string.lowercase(msg), "connection") |> should.be_true()
    _ -> should.fail("Should fail with connection refused")
  }
}

/// Test DNS resolution error (using invalid hostname)
pub fn test_dns_resolution_error() {
  let config =
    types.Config(
      base_url: "http://nonexistent-domain-12345.com",
      timeout_ms: 5000,
      headers: dict.new(),
    )

  let request =
    types.Request(
      method: types.Get,
      path: "/test",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    )

  let ctx =
    interpolate.Context(
      variables: dict.new(),
      request_body: option.None,
      response_body: option.None,
    )

  let result = https_client.execute_request(config, request, ctx)

  // Should fail with DNS resolution error
  case result {
    Error(http_client.RequestError(msg)) ->
      string.contains(string.lowercase(msg), "dns") |> should.be_true() ||
      string.contains(string.lowercase(msg), "nxdomain") |> should.be_true()
    _ -> should.fail("Should fail with DNS resolution error")
  }
}