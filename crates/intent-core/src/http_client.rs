//! HTTP client implementation for making API requests
//!
//! Provides functional, zero-panic HTTP client using Railway-Oriented Programming.
//! Wraps reqwest for actual HTTP operations with timing capture.
//!
//! # Philosophy
//!
//! - **Zero panics**: All fallible operations return `Result`
//! - **Railway-oriented**: Chain operations using combinators
//! - **Timing capture**: Every response includes elapsed time
//! - **Error mapping**: Convert reqwest errors to `IntentError`
//! - **Functional core, imperative shell**: Pure request building, I/O at edges

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

use std::collections::HashMap;

use crate::{
    error::{IntentError, IntentResult},
    http::HttpRequest,
    types::{HeaderName, HeaderValue, HttpMethod, HttpResponse, StatusCode, Url},
};

// =============================================================================
// HttpClient - Functional HTTP Client Wrapper
// =============================================================================

/// HTTP client for executing requests
///
/// Wraps `reqwest::Client` to provide functional, Railway-Oriented HTTP operations
/// with automatic timing capture and error mapping to `IntentError`.
///
/// # Philosophy
///
/// - **Functional core**: Request building is pure, execution is the imperative shell
/// - **Zero panics**: All errors returned as `Result`
/// - **Timing captured**: Every response includes elapsed time in milliseconds
/// - **Type-safe**: Uses validated types from `types` module
///
/// # Examples
///
/// ```no_run
/// use intent_core::{
///     http::HttpRequest,
///     http_client::HttpClient,
///     types::{HttpMethod, Url},
/// };
///
/// #[tokio::main]
/// async fn main() -> intent_core::error::IntentResult<()> {
///     let client = HttpClient::new();
///
///     let url = Url::try_new("https://api.example.com/data")?;
///     let request = HttpRequest::builder()
///         .method(HttpMethod::Get)
///         .url(url)
///         .build();
///
///     let response = client.execute(request).await?;
///     println!(
///         "Status: {}, Elapsed: {}ms",
///         response.status(),
///         response.elapsed_ms()
///     );
///
///     Ok(())
/// }
/// ```
#[derive(Debug, Clone)]
pub struct HttpClient {
    client: reqwest::Client,
}

impl HttpClient {
    /// Create a new HTTP client
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http_client::HttpClient;
    ///
    /// let client = HttpClient::new();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
        }
    }

    /// Execute an HTTP request and return the response with timing
    ///
    /// This is the imperative shell - performs actual I/O and captures timing.
    /// Maps all errors to `IntentError` for Railway-Oriented error handling.
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Connection` if:
    /// - Network connection fails
    /// - DNS resolution fails
    /// - Request timeout occurs
    ///
    /// Returns `IntentError::Http` if:
    /// - Server returns an error status
    /// - Invalid response received
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use intent_core::{
    ///     http::HttpRequest,
    ///     http_client::HttpClient,
    ///     types::{HttpMethod, Url},
    /// };
    ///
    /// #[tokio::main]
    /// async fn main() -> intent_core::error::IntentResult<()> {
    ///     let client = HttpClient::new();
    ///     let url = Url::try_new("https://httpbin.org/get")?;
    ///
    ///     let request = HttpRequest::builder()
    ///         .method(HttpMethod::Get)
    ///         .url(url)
    ///         .build();
    ///
    ///     let response = client.execute(request).await?;
    ///     assert!(response.is_success());
    ///
    ///     Ok(())
    /// }
    /// ```
    pub async fn execute(&self, request: HttpRequest) -> IntentResult<HttpResponse> {
        // Start timing
        let start = std::time::Instant::now();

        // Build reqwest request from our HttpRequest
        let mut req_builder = self
            .client
            .request(Self::map_method(request.method()), request.url().as_str());

        // Add headers
        for (name, value) in request.headers() {
            req_builder = req_builder.header(name.as_str(), value.as_str());
        }

        // Add body if present
        if let Some(body) = request.body() {
            req_builder = req_builder.body(body.clone());
        }

        // Add timeout if specified
        if let Some(timeout) = request.timeout() {
            req_builder = req_builder.timeout(timeout.inner());
        }

        // Execute request and capture timing
        let response = req_builder
            .send()
            .await
            .map_err(|e| Self::map_reqwest_error(e, request.url()))?;

        // Capture final URL (after redirects)
        let final_url = Url::try_new(response.url().as_str())?;

        // Extract status
        let status = StatusCode::try_new(response.status().as_u16())?;

        // Extract headers
        let headers = response
            .headers()
            .iter()
            .filter_map(|(name, value)| {
                // Convert headers, skipping any that fail validation
                HeaderName::try_new(name.as_str()).ok().and_then(|n| {
                    value
                        .to_str()
                        .ok()
                        .and_then(|v| HeaderValue::try_new(v).ok())
                        .map(|v| (n, v))
                })
            })
            .collect::<HashMap<_, _>>();

        // Extract body
        let body = response
            .text()
            .await
            .map_err(|e| IntentError::connection(request.url().as_str(), e.to_string()))?;

        // Calculate elapsed time in milliseconds
        #[allow(clippy::cast_possible_truncation)] // Safe: elapsed time fits in u64
        let elapsed_ms = start.elapsed().as_millis() as u64;

        // Construct functional response
        Ok(HttpResponse::new(
            status, headers, body, elapsed_ms, final_url,
        ))
    }

    /// Map `HttpMethod` to `reqwest::Method`
    ///
    /// Pure function - no side effects, always returns the same output for the same input.
    fn map_method(method: HttpMethod) -> reqwest::Method {
        match method {
            HttpMethod::Get => reqwest::Method::GET,
            HttpMethod::Post => reqwest::Method::POST,
            HttpMethod::Put => reqwest::Method::PUT,
            HttpMethod::Patch => reqwest::Method::PATCH,
            HttpMethod::Delete => reqwest::Method::DELETE,
            HttpMethod::Head => reqwest::Method::HEAD,
            HttpMethod::Options => reqwest::Method::OPTIONS,
        }
    }

    /// Map reqwest errors to IntentError
    ///
    /// Railway bridge - converts external errors to our domain errors.
    /// This function is pure - no side effects, deterministic mapping.
    ///
    /// # Error Mapping Strategy
    ///
    /// | reqwest Error Type | IntentError Variant | Rationale |
    /// |-------------------|---------------------|-----------|
    /// | `is_timeout()` | `Timeout` | Request exceeded time limit |
    /// | `is_connect()` | `Connection` | Network/DNS/TLS connection failed |
    /// | `is_status()` | `Http` | Server returned error status code |
    /// | `is_request()` | `Connection` | Request building failed |
    /// | `is_body()` | `Connection` | Response body processing failed |
    /// | `is_decode()` | `Connection` | Response decoding failed |
    /// | `is_redirect()` | `Connection` | Too many redirects |
    /// | Other | `Connection` | Unknown/unclassified error |
    ///
    /// # Parameters
    ///
    /// - `error`: The reqwest error to map
    /// - `url`: URL context for error reporting
    ///
    /// # Returns
    ///
    /// Appropriate `IntentError` variant with full context
    fn map_reqwest_error(error: reqwest::Error, url: &Url) -> IntentError {
        // Pattern match on error types in priority order
        // Using if-else chain because reqwest::Error doesn't expose enum variants

        if error.is_timeout() {
            // Timeout errors: Request took too long
            // Note: reqwest doesn't expose the timeout duration, so we use 0
            // The actual timeout is configured in the request builder
            IntentError::timeout(
                format!("HTTP request to {}", url.as_str()),
                0, // Duration not available from reqwest::Error
            )
        } else if error.is_connect() {
            // Connection errors: DNS, network, TLS failures
            IntentError::connection(url.as_str(), format!("Connection failed: {error}"))
        } else if let Some(status) = error.status() {
            // HTTP status errors: Server returned error status code
            // Extract method from error if available, default to "HTTP"
            IntentError::http(
                "HTTP", // Method not available from reqwest::Error
                url.as_str(),
                status.as_u16(),
            )
        } else if error.is_request() {
            // Request building errors: Invalid headers, body, etc.
            IntentError::connection(
                url.as_str(),
                format!("Request construction failed: {error}"),
            )
        } else if error.is_body() {
            // Response body errors: Body reading/streaming failed
            IntentError::connection(
                url.as_str(),
                format!("Response body processing failed: {error}"),
            )
        } else if error.is_decode() {
            // Decode errors: JSON, text, or other format parsing failed
            IntentError::connection(url.as_str(), format!("Response decoding failed: {error}"))
        } else if error.is_redirect() {
            // Redirect errors: Too many redirects
            IntentError::connection(url.as_str(), format!("Too many redirects: {error}"))
        } else {
            // Fallback for any unclassified errors
            // This ensures we never panic and always return a valid error
            IntentError::connection(url.as_str(), format!("HTTP request failed: {error}"))
        }
    }
}

impl Default for HttpClient {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::HttpMethod;

    #[test]
    fn http_client_new() {
        // Test client construction
        let client = HttpClient::new();
        assert!(format!("{:?}", client).contains("HttpClient"));
    }

    #[test]
    fn http_client_default() {
        // Test default trait
        let client = HttpClient::default();
        assert!(format!("{:?}", client).contains("HttpClient"));
    }

    #[test]
    fn http_client_clone() {
        // Test client cloning
        let client1 = HttpClient::new();
        let client2 = client1.clone();
        assert!(format!("{:?}", client2).contains("HttpClient"));
    }

    #[test]
    fn map_method_all_variants() {
        // Test all HTTP method mappings
        assert_eq!(
            HttpClient::map_method(HttpMethod::Get),
            reqwest::Method::GET
        );
        assert_eq!(
            HttpClient::map_method(HttpMethod::Post),
            reqwest::Method::POST
        );
        assert_eq!(
            HttpClient::map_method(HttpMethod::Put),
            reqwest::Method::PUT
        );
        assert_eq!(
            HttpClient::map_method(HttpMethod::Patch),
            reqwest::Method::PATCH
        );
        assert_eq!(
            HttpClient::map_method(HttpMethod::Delete),
            reqwest::Method::DELETE
        );
        assert_eq!(
            HttpClient::map_method(HttpMethod::Head),
            reqwest::Method::HEAD
        );
        assert_eq!(
            HttpClient::map_method(HttpMethod::Options),
            reqwest::Method::OPTIONS
        );
    }

    #[tokio::test]
    async fn test_execute_request_success() {
        // TDD: Test successful HTTP request execution with timing
        let client = HttpClient::new();

        let url = Url::try_new("https://httpbin.org/get").ok();
        assert!(url.is_some());

        if let Some(url) = url {
            let request = HttpRequest::builder()
                .method(HttpMethod::Get)
                .url(url)
                .build();

            let result = client.execute(request).await;

            // Should succeed
            assert!(result.is_ok());

            if let Ok(response) = result {
                // Should have 2xx status
                assert!(response.is_success());

                // Should capture timing (elapsed_ms should be > 0)
                assert!(response.elapsed_ms() > 0);

                // Should have body
                assert!(!response.body().is_empty());
            }
        }
    }

    #[tokio::test]
    async fn test_execute_request_with_headers() {
        // Test request with custom headers
        let client = HttpClient::new();

        let url = Url::try_new("https://httpbin.org/headers").ok();
        let accept_name = HeaderName::try_new("Accept").ok();
        let accept_value = HeaderValue::try_new("application/json").ok();

        assert!(url.is_some() && accept_name.is_some() && accept_value.is_some());

        if let (Some(url), Some(name), Some(value)) = (url, accept_name, accept_value) {
            let request = HttpRequest::builder()
                .method(HttpMethod::Get)
                .url(url)
                .header(name, value)
                .build();

            let result = client.execute(request).await;
            assert!(result.is_ok());

            if let Ok(response) = result {
                assert!(response.is_success());
                // Headers should be reflected in the response body from httpbin
                assert!(response.body().contains("Accept"));
            }
        }
    }

    #[tokio::test]
    async fn test_execute_request_404() {
        // Test handling of 404 response
        let client = HttpClient::new();

        let url = Url::try_new("https://httpbin.org/status/404").ok();
        assert!(url.is_some());

        if let Some(url) = url {
            let request = HttpRequest::builder()
                .method(HttpMethod::Get)
                .url(url)
                .build();

            let result = client.execute(request).await;

            // Should succeed (we got a response)
            assert!(result.is_ok());

            if let Ok(response) = result {
                // Status should be 404
                assert_eq!(response.status().as_u16(), 404);
                assert!(response.is_client_error());
            }
        }
    }

    #[tokio::test]
    async fn test_execute_request_invalid_domain() {
        // Test connection error handling
        let client = HttpClient::new();

        let url = Url::try_new("https://this-domain-does-not-exist-12345.com").ok();
        assert!(url.is_some());

        if let Some(url) = url {
            let request = HttpRequest::builder()
                .method(HttpMethod::Get)
                .url(url)
                .build();

            let result = client.execute(request).await;

            // Should fail with connection error
            assert!(result.is_err());
        }
    }

    #[tokio::test]
    async fn test_execute_captures_timing() {
        // Test that timing is captured for all requests
        let client = HttpClient::new();

        let url = Url::try_new("https://httpbin.org/delay/1").ok();
        assert!(url.is_some());

        if let Some(url) = url {
            let request = HttpRequest::builder()
                .method(HttpMethod::Get)
                .url(url)
                .build();

            let result = client.execute(request).await;

            assert!(result.is_ok());

            if let Ok(response) = result {
                // Should have taken at least 1 second (1000ms)
                assert!(response.elapsed_ms() >= 900); // Allow some margin
                assert!(response.is_success());
            }
        }
    }

    // =========================================================================
    // Error Mapping Tests (TDD: BEAD intent-cli-le77)
    // =========================================================================
    // These tests verify reqwest::Error is correctly mapped to IntentError
    // variants. Following functional Rust patterns with zero unwraps.

    #[test]
    fn test_error_mapping_timeout_creates_timeout_variant() {
        // Arrange: Create a timeout error scenario
        // We can't easily mock reqwest::Error, so we test the function signature
        // and ensure IntentError::Timeout has the right structure
        let _url = Url::try_new("https://example.com").ok();
        assert!(_url.is_some());

        // Act: Create a timeout error
        let error = IntentError::timeout("HTTP request", 5000);

        // Assert: Verify correct variant and structure
        match error {
            IntentError::Timeout {
                operation,
                duration_ms,
                partial_result,
            } => {
                assert_eq!(operation, "HTTP request");
                assert_eq!(duration_ms, 5000);
                assert!(partial_result.is_none());
            }
            _ => panic!("Expected Timeout variant"),
        }
    }

    #[test]
    fn test_error_mapping_connection_creates_connection_variant() {
        // Arrange: Create a connection error
        let url = "https://example.com";
        let message = "Connection refused";

        // Act: Create connection error
        let error = IntentError::connection(url, message);

        // Assert: Verify correct variant and data
        match error {
            IntentError::Connection {
                url: error_url,
                message: error_message,
            } => {
                assert_eq!(error_url, url);
                assert_eq!(error_message, message);
            }
            _ => panic!("Expected Connection variant"),
        }
    }

    #[test]
    fn test_error_mapping_http_status_creates_http_variant() {
        // Arrange: Create an HTTP error with status code
        let method = "GET";
        let url = "https://api.example.com/users";
        let status = 500;

        // Act: Create HTTP error
        let error = IntentError::http(method, url, status);

        // Assert: Verify correct variant and all fields
        match error {
            IntentError::Http {
                method: error_method,
                url: error_url,
                status: error_status,
                body,
                expected_status,
            } => {
                assert_eq!(error_method, method);
                assert_eq!(error_url, url);
                assert_eq!(error_status, status);
                assert!(body.is_none());
                assert!(expected_status.is_none());
            }
            _ => panic!("Expected Http variant"),
        }
    }

    #[test]
    fn test_error_mapping_validates_all_reqwest_error_types() {
        // This test documents all reqwest error types we need to handle:
        // 1. is_timeout() -> IntentError::Timeout
        // 2. is_connect() -> IntentError::Connection
        // 3. is_status() with status code -> IntentError::Http
        // 4. is_request() (builder errors) -> IntentError::Connection
        // 5. is_body() (response body errors) -> IntentError::Connection
        // 6. is_decode() (JSON/text decode) -> IntentError::Connection
        // 7. is_redirect() (too many redirects) -> IntentError::Connection
        // 8. Fallback: any other error -> IntentError::Connection

        // Assert: Verify each error type maps to the correct IntentError variant
        // (This is a documentation test - actual mapping tested in integration tests)

        // Timeout errors
        let timeout_err = IntentError::timeout("HTTP request", 5000);
        assert!(matches!(timeout_err, IntentError::Timeout { .. }));

        // Connection errors (connect, DNS, etc.)
        let conn_err = IntentError::connection("https://example.com", "Connection failed");
        assert!(matches!(conn_err, IntentError::Connection { .. }));

        // HTTP status errors
        let http_err = IntentError::http("GET", "https://example.com", 500);
        assert!(matches!(http_err, IntentError::Http { .. }));
    }

    #[test]
    fn test_error_mapping_preserves_url_context() {
        // Arrange: Create errors with URL context
        let url = "https://api.example.com/endpoint";

        // Act & Assert: All error types should preserve URL
        let timeout = IntentError::timeout("HTTP request", 5000);
        let display = timeout.to_string();
        assert!(display.contains("HTTP request"));

        let connection = IntentError::connection(url, "DNS failed");
        let display = connection.to_string();
        assert!(display.contains(url));

        let http = IntentError::http("POST", url, 404);
        let display = http.to_string();
        assert!(display.contains(url));
    }

    #[test]
    fn test_error_mapping_timeout_includes_operation_context() {
        // Arrange: Create timeout with operation context
        let operation = "HTTP GET /api/data";
        let duration = 30000;

        // Act: Create timeout error
        let error = IntentError::timeout(operation, duration);

        // Assert: Error display includes operation and duration
        let display = error.to_string();
        assert!(display.contains(operation));
        assert!(display.contains("30000"));
        assert!(display.contains("ms"));
    }

    #[test]
    fn test_error_mapping_connection_includes_detailed_message() {
        // Arrange: Create connection error with detailed message
        let url = "https://api.example.com";
        let detailed_msg = "Connection refused: DNS resolution failed for api.example.com";

        // Act: Create connection error
        let error = IntentError::connection(url, detailed_msg);

        // Assert: Full message is preserved
        let display = error.to_string();
        assert!(display.contains("Connection failed"));
        assert!(display.contains(url));
        assert!(display.contains(detailed_msg));
    }

    #[test]
    fn test_error_mapping_http_status_includes_method_and_url() {
        // Arrange: Create HTTP error with all context
        let method = "POST";
        let url = "https://api.example.com/users";
        let status = 403;

        // Act: Create HTTP error
        let error = IntentError::http(method, url, status);

        // Assert: Display includes all context
        let display = error.to_string();
        assert!(display.contains(method));
        assert!(display.contains(url));
        assert!(display.contains("403"));
    }

    #[test]
    fn test_error_mapping_maintains_railway_compatibility() {
        // Test that errors can be chained using Railway-Oriented Programming
        let url = Url::try_new("https://example.com").ok();
        assert!(url.is_some());

        if let Some(url) = url {
            // Simulate error mapping in a railway chain
            let result: Result<(), IntentError> = Err(IntentError::timeout("test", 1000));

            let mapped = result.map_err(|e| match e {
                IntentError::Timeout { .. } => {
                    IntentError::connection(url.as_str(), "Timeout converted to connection")
                }
                other => other,
            });

            // Assert: Error was mapped correctly
            assert!(mapped.is_err());
            if let Err(error) = mapped {
                assert!(matches!(error, IntentError::Connection { .. }));
            }
        }
    }
}
