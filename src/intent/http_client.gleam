/// HTTP client for executing behavior requests

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/http
import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/httpc
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleam/uri
import intent/interpolate.{type Context}
import intent/parser
import intent/types.{type Config, type Request}

/// Result of executing a request
pub type ExecutionResult {
  ExecutionResult(
    status: Int,
    headers: Dict(String, String),
    body: Json,
    raw_body: String,
    elapsed_ms: Int,
    request_method: types.Method,
    request_path: String,
  )
}

/// Error types for HTTP execution
pub type ExecutionError {
  UrlParseError(message: String)
  InterpolationError(message: String)
  RequestError(message: String)
  ResponseParseError(message: String)
}

/// Execute a behavior request against the target
pub fn execute_request(
  config: Config,
  req: Request,
  ctx: Context,
) -> Result(ExecutionResult, ExecutionError) {
  // Build the full URL
  let base_url = config.base_url
  use path <- result.try(interpolate_path(req.path, ctx))

  let full_url = base_url <> path

  // Parse the URL
  use parsed_uri <- result.try(
    uri.parse(full_url)
    |> result.map_error(fn(_) { UrlParseError("Invalid URL: " <> full_url) }),
  )

  // Build headers - merge config headers with request headers
  use request_headers <- result.try(interpolate_headers(req.headers, ctx))
  let merged_headers = merge_headers(config.headers, request_headers)

  // Interpolate the request body if present
  use interpolated_body <- result.try(interpolate_body(req.body, ctx))

  // Build the HTTP request
  let method = convert_method(req.method)

  // Create request with body if present
  use http_req <- result.try(build_http_request(
    method,
    parsed_uri,
    merged_headers,
    interpolated_body,
  ))

  // Execute the request (pass method and path for rule checking)
  execute_with_timing(http_req, req.method, path)
}

fn interpolate_path(path: String, ctx: Context) -> Result(String, ExecutionError) {
  interpolate.interpolate_string(ctx, path)
  |> result.map_error(InterpolationError)
}

fn interpolate_headers(
  headers: Dict(String, String),
  ctx: Context,
) -> Result(Dict(String, String), ExecutionError) {
  interpolate.interpolate_headers(ctx, headers)
  |> result.map_error(InterpolationError)
}

fn interpolate_body(
  body: Json,
  ctx: Context,
) -> Result(Json, ExecutionError) {
  // Convert to string, interpolate, then parse back to JSON
  let body_str = json.to_string(body)
  case interpolate.interpolate_string(ctx, body_str) {
    Ok(interpolated_str) ->
      case json.decode(interpolated_str, dynamic.dynamic) {
        Ok(data) -> Ok(parser.dynamic_to_json(data))
        Error(_) -> Error(InterpolationError("Failed to parse interpolated body as JSON"))
      }
    Error(e) -> Error(InterpolationError(e))
  }
}

fn merge_headers(
  config_headers: Dict(String, String),
  request_headers: Dict(String, String),
) -> Dict(String, String) {
  // Request headers override config headers
  dict.merge(config_headers, request_headers)
}

fn convert_method(method: types.Method) -> http.Method {
  case method {
    types.Get -> http.Get
    types.Post -> http.Post
    types.Put -> http.Put
    types.Patch -> http.Patch
    types.Delete -> http.Delete
    types.Head -> http.Head
    types.Options -> http.Options
  }
}

fn build_http_request(
  method: http.Method,
  parsed_uri: uri.Uri,
  headers: Dict(String, String),
  body: Json,
) -> Result(HttpRequest(String), ExecutionError) {
  // Get host from URI - path is now a String, not Option(String) in older API
  let host = option.unwrap(parsed_uri.host, "localhost")
  let path = ensure_leading_slash(parsed_uri.path)
  let port = parsed_uri.port
  let scheme = case parsed_uri.scheme {
    Some("https") -> http.Https
    _ -> http.Http
  }

  // Start building request
  let req =
    request.new()
    |> request.set_method(method)
    |> request.set_host(host)
    |> request.set_path(path)
    |> request.set_scheme(scheme)

  // Add port if specified
  let req = case port {
    Some(p) -> request.set_port(req, p)
    None -> req
  }

  // Add headers
  let req =
    dict.fold(headers, req, fn(acc, key, value) {
      request.set_header(acc, string.lowercase(key), value)
    })

  // Add body and content-type
  let body_str = json.to_string(body)
  let req =
    req
    |> request.set_body(body_str)
    |> request.set_header("content-type", "application/json")

  Ok(req)
}

fn ensure_leading_slash(path: String) -> String {
  case string.starts_with(path, "/") {
    True -> path
    False -> "/" <> path
  }
}

fn execute_with_timing(
  req: HttpRequest(String),
  method: types.Method,
  path: String,
) -> Result(ExecutionResult, ExecutionError) {
  let start = erlang_now_ms()

  case httpc.send(req) {
    Ok(resp) -> {
      let elapsed = erlang_now_ms() - start
      Ok(parse_response(resp, elapsed, method, path))
    }
    Error(e) -> Error(RequestError(format_httpc_error(e)))
  }
}

fn parse_response(
  resp: HttpResponse(String),
  elapsed_ms: Int,
  method: types.Method,
  path: String,
) -> ExecutionResult {
  let headers =
    resp.headers
    |> list.map(fn(pair) { #(pair.0, pair.1) })
    |> dict.from_list

  let body = case string.is_empty(resp.body) {
    True -> json.null()
    False ->
      case json.decode(resp.body, dynamic.dynamic) {
        Ok(data) -> parser.dynamic_to_json(data)
        Error(_) -> json.null()
      }
  }

  ExecutionResult(
    status: resp.status,
    headers: headers,
    body: body,
    raw_body: resp.body,
    elapsed_ms: elapsed_ms,
    request_method: method,
    request_path: path,
  )
}

fn format_httpc_error(error: dynamic.Dynamic) -> String {
  let error_str = string.inspect(error) |> string.lowercase

  // Helper function to check multiple patterns
  let check_patterns = fn(patterns: List(String)) -> Bool {
    list.any(patterns, fn(p) { string.contains(error_str, p) })
  }

  // Determine the error message
  let message =
    case check_patterns(["timeout"]) {
      True -> {
        "Connection timeout: The request took too long to complete.\n"
        <> "  • Check if the target API is responding slowly\n"
        <> "  • Try increasing the timeout_ms in your config\n"
        <> "  • Verify the base_url is correct and accessible"
      }
      False ->
        case check_patterns(["econnrefused", "connection_refused"]) {
          True -> {
            "Connection refused: Cannot connect to the target server.\n"
            <> "  • Check if the base_url is correct\n"
            <> "  • Verify the server is running and listening on the specified port\n"
            <> "  • Ensure your network firewall allows connections to this server"
          }
          False ->
            case check_patterns(["nxdomain", "enotfound"]) {
              True -> {
                "DNS resolution failed: Cannot find the hostname.\n"
                <> "  • Check if the base_url hostname is spelled correctly\n"
                <> "  • Verify your network connection\n"
                <> "  • Try pinging the hostname to test DNS resolution"
              }
              False ->
                case check_patterns(["ssl", "certificate"]) {
                  True -> {
                    "SSL/TLS certificate error: Cannot verify the server's certificate.\n"
                    <> "  • The server may have an invalid or expired certificate\n"
                    <> "  • Check if your system's certificate store is up to date\n"
                    <> "  • For development, ensure you're using the correct base_url scheme (http vs https)"
                  }
                  False ->
                    case check_patterns(["eacces"]) {
                      True -> {
                        "Permission denied: No access to the specified resource.\n"
                        <> "  • Check if you have permission to access the target URL\n"
                        <> "  • Verify the base_url and path are correct"
                      }
                      False ->
                        case check_patterns(["ehostunreach", "enetunreach"]) {
                          True -> {
                            "Network unreachable: Cannot reach the target host.\n"
                            <> "  • Check your network connection\n"
                            <> "  • Verify the host is accessible from your location\n"
                            <> "  • Check for firewall or VPN restrictions"
                          }
                          False -> {
                            "HTTP request failed: " <> string.inspect(error) <> "\n"
                            <> "  • Check the base_url and ensure the target server is reachable\n"
                            <> "  • Verify the request path and headers are correct\n"
                            <> "  • Try running with a simpler request to isolate the issue"
                          }
                        }
                    }
                }
            }
        }
    }

  message
}

@external(erlang, "intent_ffi", "now_ms")
fn erlang_now_ms() -> Int
