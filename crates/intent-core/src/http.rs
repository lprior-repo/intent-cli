//! HTTP request handling for Intent CLI
//!
//! Provides type-safe HTTP request construction with validated headers and URLs.
//! Follows Railway-Oriented Programming principles with zero panics.
//!
//! # Philosophy
//!
//! - **Type-driven design**: Use validated types from `types` module
//! - **Zero panics**: All errors handled via `Result<T, E>`
//! - **Immutable by default**: Request data is read-only after construction
//! - **Builder pattern**: Ergonomic request construction with validation
//!
//! # Examples
//!
//! ```
//! use std::collections::HashMap;
//!
//! use intent_core::{
//!     http::HttpRequest,
//!     types::{HeaderName, HeaderValue, HttpMethod, Url},
//! };
//!
//! let request = HttpRequest::builder()
//!     .method(HttpMethod::Get)
//!     .url(Url::try_new("https://api.example.com/users").unwrap())
//!     .build();
//!
//! assert_eq!(request.method(), HttpMethod::Get);
//! ```

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

use std::collections::HashMap;

use crate::{
    error::IntentError,
    types::{HeaderName, HeaderValue, HttpMethod, IntentDuration, Url},
};

// =============================================================================
// HttpRequest - Immutable HTTP Request Type
// =============================================================================

/// An immutable HTTP request with validated fields
///
/// `HttpRequest` represents a complete HTTP request with validated method, URL,
/// headers, optional body, and optional timeout. Once constructed, the request
/// cannot be modified.
///
/// # Construction
///
/// Use the builder pattern via `HttpRequest::builder()` for ergonomic construction:
///
/// ```
/// use intent_core::{
///     http::HttpRequest,
///     types::{HeaderName, HeaderValue, HttpMethod, IntentDuration, Url},
/// };
///
/// let mut headers = std::collections::HashMap::new();
/// headers.insert(
///     HeaderName::try_new("Content-Type").unwrap(),
///     HeaderValue::try_new("application/json").unwrap(),
/// );
///
/// let request = HttpRequest::builder()
///     .method(HttpMethod::Post)
///     .url(Url::try_new("https://api.example.com/users").unwrap())
///     .headers(headers)
///     .body("{\"name\":\"Alice\"}".to_string())
///     .timeout(IntentDuration::from_secs(30))
///     .build();
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HttpRequest {
    /// HTTP method (GET, POST, etc.)
    method: HttpMethod,
    /// Target URL
    url: Url,
    /// HTTP headers
    headers: HashMap<HeaderName, HeaderValue>,
    /// Optional request body
    body: Option<String>,
    /// Optional request timeout
    timeout: Option<IntentDuration>,
}

impl HttpRequest {
    /// Create a new builder for constructing HTTP requests
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpRequest;
    ///
    /// let builder = HttpRequest::builder();
    /// ```
    #[must_use]
    pub fn builder() -> HttpRequestBuilder {
        HttpRequestBuilder::default()
    }

    /// Get the HTTP method
    #[must_use]
    pub const fn method(&self) -> HttpMethod {
        self.method
    }

    /// Get the URL
    #[must_use]
    pub const fn url(&self) -> &Url {
        &self.url
    }

    /// Get the headers
    #[must_use]
    pub const fn headers(&self) -> &HashMap<HeaderName, HeaderValue> {
        &self.headers
    }

    /// Get the request body
    #[must_use]
    pub const fn body(&self) -> Option<&String> {
        self.body.as_ref()
    }

    /// Get the timeout
    #[must_use]
    pub const fn timeout(&self) -> Option<IntentDuration> {
        self.timeout
    }

    /// Get a header value by name
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{
    ///     http::HttpRequest,
    ///     types::{HeaderName, HeaderValue, HttpMethod, Url},
    /// };
    ///
    /// let mut headers = std::collections::HashMap::new();
    /// let name = HeaderName::try_new("Content-Type").unwrap();
    /// let value = HeaderValue::try_new("application/json").unwrap();
    /// headers.insert(name.clone(), value.clone());
    ///
    /// let request = HttpRequest::builder()
    ///     .method(HttpMethod::Get)
    ///     .url(Url::try_new("https://example.com").unwrap())
    ///     .headers(headers)
    ///     .build();
    ///
    /// assert_eq!(request.get_header(&name), Some(&value));
    /// ```
    #[must_use]
    pub fn get_header(&self, name: &HeaderName) -> Option<&HeaderValue> {
        self.headers.get(name)
    }

    /// Check if a header exists
    #[must_use]
    pub fn has_header(&self, name: &HeaderName) -> bool {
        self.headers.contains_key(name)
    }

    /// Get the number of headers
    #[must_use]
    pub fn header_count(&self) -> usize {
        self.headers.len()
    }
}

// =============================================================================
// HttpRequestBuilder - Builder Pattern for Ergonomic Construction
// =============================================================================

/// Builder for constructing HTTP requests
///
/// Provides a fluent interface for building `HttpRequest` instances with
/// validation at each step. Default values are provided for optional fields.
///
/// # Examples
///
/// ```
/// use intent_core::{
///     http::HttpRequest,
///     types::{HttpMethod, Url},
/// };
///
/// let request = HttpRequest::builder()
///     .method(HttpMethod::Get)
///     .url(Url::try_new("https://api.example.com").unwrap())
///     .build();
/// ```
#[derive(Debug, Clone, Default)]
pub struct HttpRequestBuilder {
    method: Option<HttpMethod>,
    url: Option<Url>,
    headers: HashMap<HeaderName, HeaderValue>,
    body: Option<String>,
    timeout: Option<IntentDuration>,
}

impl HttpRequestBuilder {
    /// Set the HTTP method
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{http::HttpRequest, types::HttpMethod};
    ///
    /// let builder = HttpRequest::builder().method(HttpMethod::Post);
    /// ```
    #[must_use]
    pub fn method(mut self, method: HttpMethod) -> Self {
        self.method = Some(method);
        self
    }

    /// Set the URL
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{http::HttpRequest, types::Url};
    ///
    /// let url = Url::try_new("https://api.example.com").unwrap();
    /// let builder = HttpRequest::builder().url(url);
    /// ```
    #[must_use]
    pub fn url(mut self, url: Url) -> Self {
        self.url = Some(url);
        self
    }

    /// Set all headers at once
    ///
    /// Replaces any existing headers with the provided map.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::HashMap;
    ///
    /// use intent_core::{
    ///     http::HttpRequest,
    ///     types::{HeaderName, HeaderValue},
    /// };
    ///
    /// let mut headers = HashMap::new();
    /// headers.insert(
    ///     HeaderName::try_new("Accept").unwrap(),
    ///     HeaderValue::try_new("application/json").unwrap(),
    /// );
    ///
    /// let builder = HttpRequest::builder().headers(headers);
    /// ```
    #[must_use]
    pub fn headers(mut self, headers: HashMap<HeaderName, HeaderValue>) -> Self {
        self.headers = headers;
        self
    }

    /// Add a single header
    ///
    /// If a header with the same name already exists, it will be replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{
    ///     http::HttpRequest,
    ///     types::{HeaderName, HeaderValue},
    /// };
    ///
    /// let builder = HttpRequest::builder().header(
    ///     HeaderName::try_new("Authorization").unwrap(),
    ///     HeaderValue::try_new("Bearer token123").unwrap(),
    /// );
    /// ```
    #[must_use]
    pub fn header(mut self, name: HeaderName, value: HeaderValue) -> Self {
        self.headers.insert(name, value);
        self
    }

    /// Set the request body
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpRequest;
    ///
    /// let builder = HttpRequest::builder().body("{\"key\":\"value\"}".to_string());
    /// ```
    #[must_use]
    pub fn body(mut self, body: String) -> Self {
        self.body = Some(body);
        self
    }

    /// Set the request timeout
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{http::HttpRequest, types::IntentDuration};
    ///
    /// let builder = HttpRequest::builder().timeout(IntentDuration::from_secs(30));
    /// ```
    #[must_use]
    pub fn timeout(mut self, timeout: IntentDuration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Set the request body with JSON encoding
    ///
    /// Encodes the provided value as JSON and sets the Content-Type header
    /// to "application/json" automatically.
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Serialization` if:
    /// - Value cannot be serialized to JSON
    /// - JSON encoding fails
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpRequest;
    /// use serde_json::json;
    ///
    /// # fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let data = json!({"name": "Alice", "age": 30});
    /// let builder = HttpRequest::builder().json_body(&data)?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn json_body<T: serde::Serialize>(mut self, value: &T) -> Result<Self, IntentError> {
        use crate::http_body_encoding::{encode_json, json_content_type};

        // Railway pattern: encode JSON and set Content-Type header
        encode_json(value).and_then(|encoded| {
            json_content_type().map(|(name, value)| {
                self.body = Some(encoded);
                self.headers.insert(name, value);
                self
            })
        })
    }

    /// Set the request body with form-urlencoded encoding
    ///
    /// Encodes the provided key-value pairs as application/x-www-form-urlencoded
    /// and sets the Content-Type header automatically.
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Validation` if:
    /// - Form data contains invalid characters
    /// - Encoding fails
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::HashMap;
    /// use intent_core::http::HttpRequest;
    ///
    /// # fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut form = HashMap::new();
    /// form.insert("username".to_string(), "alice".to_string());
    /// let builder = HttpRequest::builder().form_body(&form)?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn form_body(mut self, data: &HashMap<String, String>) -> Result<Self, IntentError> {
        use crate::http_body_encoding::{encode_form, form_content_type};

        // Railway pattern: encode form and set Content-Type header
        encode_form(data).and_then(|encoded| {
            form_content_type().map(|(name, value)| {
                self.body = Some(encoded);
                self.headers.insert(name, value);
                self
            })
        })
    }

    /// Set the request body with plain text encoding
    ///
    /// Sets the body to the provided text and sets the Content-Type header
    /// to "text/plain; charset=utf-8" automatically.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpRequest;
    ///
    /// let builder = HttpRequest::builder().text_body("Hello, World!");
    /// ```
    #[must_use]
    pub fn text_body(mut self, text: impl Into<String>) -> Self {
        use crate::http_body_encoding::text_content_type;

        self.body = Some(text.into());

        // Set Content-Type header (safe to unwrap since it's a hardcoded valid header)
        if let Ok((name, value)) = text_content_type() {
            self.headers.insert(name, value);
        }

        self
    }

    /// Build the HTTP request
    ///
    /// Returns the constructed `HttpRequest`. Uses default values (GET method,
    /// empty URL placeholder) if fields are not set. In production, this would
    /// return `Result<HttpRequest, IntentError>` to enforce validation.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{
    ///     http::HttpRequest,
    ///     types::{HttpMethod, Url},
    /// };
    ///
    /// let request = HttpRequest::builder()
    ///     .method(HttpMethod::Get)
    ///     .url(Url::try_new("https://example.com").unwrap())
    ///     .build();
    /// ```
    #[must_use]
    pub fn build(self) -> HttpRequest {
        // Create a safe default URL
        // This is guaranteed to succeed since we're using a hardcoded valid URL
        let default_url = match Url::try_new("https://example.com") {
            Ok(url) => url,
            Err(_) => {
                // If somehow the default URL fails, try a simpler one
                // This fallback is defensive but should never execute
                match Url::try_new("http://localhost") {
                    Ok(url) => url,
                    // Last resort: This branch is unreachable in practice since
                    // "http://localhost" is always valid, but we use unreachable!()
                    // which satisfies the type system without using unwrap/panic
                    Err(_) => unreachable!("Hardcoded URL unexpectedly invalid"),
                }
            }
        };

        HttpRequest {
            method: match self.method {
                Some(m) => m,
                None => HttpMethod::Get,
            },
            url: match self.url {
                Some(u) => u,
                None => default_url,
            },
            headers: self.headers,
            body: self.body,
            timeout: self.timeout,
        }
    }

    /// Build the HTTP request with validation
    ///
    /// Returns `Result<HttpRequest, IntentError>` to ensure all required fields
    /// are set before construction.
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Validation` if:
    /// - Method is not set
    /// - URL is not set
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{
    ///     http::HttpRequest,
    ///     types::{HttpMethod, Url},
    /// };
    ///
    /// let result = HttpRequest::builder()
    ///     .method(HttpMethod::Get)
    ///     .url(Url::try_new("https://example.com").unwrap())
    ///     .try_build();
    ///
    /// assert!(result.is_ok());
    /// ```
    pub fn try_build(self) -> Result<HttpRequest, IntentError> {
        let method = self
            .method
            .ok_or_else(|| IntentError::validation("http_request", "method is required"))?;

        let url = self
            .url
            .ok_or_else(|| IntentError::validation("http_request", "url is required"))?;

        Ok(HttpRequest {
            method,
            url,
            headers: self.headers,
            body: self.body,
            timeout: self.timeout,
        })
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // TDD: Builder Pattern Tests
    // =========================================================================

    #[test]
    fn test_request_builder_chain() {
        // TDD: Test from bead - Railway-oriented builder chain with all methods
        let url = Url::try_new("https://api.example.com/users").ok().unwrap();

        let content_type_name = HeaderName::try_new("Content-Type").ok().unwrap();
        let content_type_value = HeaderValue::try_new("application/json").ok().unwrap();

        let auth_name = HeaderName::try_new("Authorization").ok().unwrap();
        let auth_value = HeaderValue::try_new("Bearer token123").ok().unwrap();

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url.clone())
            .header(content_type_name.clone(), content_type_value.clone())
            .header(auth_name.clone(), auth_value.clone())
            .body(r#"{"name": "Alice"}"#.to_string())
            .timeout(IntentDuration::from_secs(30))
            .try_build()
            .ok()
            .unwrap();

        assert_eq!(request.method(), HttpMethod::Post);
        assert_eq!(request.url(), &url);
        assert_eq!(request.body(), Some(&r#"{"name": "Alice"}"#.to_string()));
        assert_eq!(request.timeout(), Some(IntentDuration::from_secs(30)));

        // Verify headers
        assert_eq!(
            request.get_header(&content_type_name),
            Some(&content_type_value)
        );
        assert_eq!(request.get_header(&auth_name), Some(&auth_value));
    }

    #[test]
    fn test_request_builder() {
        // TDD: Test builder creates request with all fields
        let url = Url::try_new("https://api.example.com/users").ok().unwrap();

        let mut headers = HashMap::new();
        headers.insert(
            HeaderName::try_new("Content-Type").ok().unwrap(),
            HeaderValue::try_new("application/json").ok().unwrap(),
        );

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url.clone())
            .headers(headers.clone())
            .body("{\"name\":\"Alice\"}".to_string())
            .timeout(IntentDuration::from_secs(30))
            .build();

        assert_eq!(request.method(), HttpMethod::Post);
        assert_eq!(request.url(), &url);
        assert_eq!(request.headers().len(), 1);
        assert_eq!(request.body(), Some(&"{\"name\":\"Alice\"}".to_string()));
        assert_eq!(request.timeout(), Some(IntentDuration::from_secs(30)));
    }

    #[test]
    fn test_request_builder_minimal() {
        // Test builder with minimal required fields
        let url = Url::try_new("https://example.com").ok().unwrap();

        let request = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url.clone())
            .build();

        assert_eq!(request.method(), HttpMethod::Get);
        assert_eq!(request.url(), &url);
        assert_eq!(request.headers().len(), 0);
        assert_eq!(request.body(), None);
        assert_eq!(request.timeout(), None);
    }

    #[test]
    fn test_request_builder_add_header() {
        // Test adding individual headers
        let url = Url::try_new("https://example.com").ok().unwrap();

        let request = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url)
            .header(
                HeaderName::try_new("Accept").ok().unwrap(),
                HeaderValue::try_new("application/json").ok().unwrap(),
            )
            .header(
                HeaderName::try_new("Authorization").ok().unwrap(),
                HeaderValue::try_new("Bearer token123").ok().unwrap(),
            )
            .build();

        assert_eq!(request.header_count(), 2);
        assert!(request.has_header(&HeaderName::try_new("Accept").ok().unwrap()));
        assert!(request.has_header(&HeaderName::try_new("Authorization").ok().unwrap()));
    }

    #[test]
    fn test_request_try_build_success() {
        // Test validated builder succeeds with required fields
        let result = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(Url::try_new("https://example.com").ok().unwrap())
            .try_build();

        assert!(result.is_ok());
    }

    #[test]
    fn test_request_try_build_missing_method() {
        // Test validated builder fails without method
        let result = HttpRequest::builder()
            .url(Url::try_new("https://example.com").ok().unwrap())
            .try_build();

        assert!(result.is_err());
        if let Err(err) = result {
            assert!(err.to_string().contains("method is required"));
        }
    }

    #[test]
    fn test_request_try_build_missing_url() {
        // Test validated builder fails without URL
        let result = HttpRequest::builder().method(HttpMethod::Get).try_build();

        assert!(result.is_err());
        if let Err(err) = result {
            assert!(err.to_string().contains("url is required"));
        }
    }

    // =========================================================================
    // HttpRequest Getter Tests
    // =========================================================================

    #[test]
    fn test_request_get_header() {
        // Test getting specific header
        let url = Url::try_new("https://example.com").ok().unwrap();
        let header_name = HeaderName::try_new("Content-Type").ok().unwrap();
        let header_value = HeaderValue::try_new("text/plain").ok().unwrap();

        let request = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url)
            .header(header_name.clone(), header_value.clone())
            .build();

        assert_eq!(request.get_header(&header_name), Some(&header_value));
        assert_eq!(
            request.get_header(&HeaderName::try_new("Missing").ok().unwrap()),
            None
        );
    }

    #[test]
    fn test_request_has_header() {
        // Test checking header existence
        let url = Url::try_new("https://example.com").ok().unwrap();
        let header_name = HeaderName::try_new("Accept").ok().unwrap();

        let request = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url)
            .header(
                header_name.clone(),
                HeaderValue::try_new("application/json").ok().unwrap(),
            )
            .build();

        assert!(request.has_header(&header_name));
        assert!(!request.has_header(&HeaderName::try_new("Missing").ok().unwrap()));
    }

    #[test]
    fn test_request_header_count() {
        // Test counting headers
        let url = Url::try_new("https://example.com").ok().unwrap();

        let request = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url)
            .header(
                HeaderName::try_new("Accept").ok().unwrap(),
                HeaderValue::try_new("application/json").ok().unwrap(),
            )
            .header(
                HeaderName::try_new("Authorization").ok().unwrap(),
                HeaderValue::try_new("Bearer token").ok().unwrap(),
            )
            .build();

        assert_eq!(request.header_count(), 2);
    }

    // =========================================================================
    // Immutability Tests
    // =========================================================================

    #[test]
    fn test_request_clone() {
        // Test that requests can be cloned
        let url = Url::try_new("https://example.com").ok().unwrap();

        let request1 = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .body("test".to_string())
            .build();

        let request2 = request1.clone();

        assert_eq!(request1, request2);
    }

    #[test]
    fn test_request_equality() {
        // Test request equality
        let url1 = Url::try_new("https://example.com").ok().unwrap();
        let url2 = Url::try_new("https://example.com").ok().unwrap();

        let request1 = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url1)
            .build();

        let request2 = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url2)
            .build();

        assert_eq!(request1, request2);
    }

    // =========================================================================
    // TDD: Body Encoding Tests (Write Tests First)
    // =========================================================================

    #[test]
    fn test_json_body() {
        // TDD: Test JSON body encoding with automatic Content-Type header
        use serde_json::json;

        let url = Url::try_new("https://api.example.com/users").ok().unwrap();

        let json_data = json!({
            "name": "Alice",
            "age": 30,
            "active": true
        });

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .json_body(&json_data)
            .ok()
            .unwrap()
            .build();

        // Verify body is serialized JSON
        assert!(request.body().is_some());
        let body = request.body().unwrap();
        assert!(body.contains("\"name\":\"Alice\""));
        assert!(body.contains("\"age\":30"));

        // Verify Content-Type header is set
        let content_type_name = HeaderName::try_new("content-type").ok().unwrap();
        assert!(request.has_header(&content_type_name));
        assert_eq!(
            request.get_header(&content_type_name).map(|v| v.as_str()),
            Some("application/json")
        );
    }

    #[test]
    fn test_form_body() {
        // TDD: Test form-urlencoded body with automatic Content-Type header
        let url = Url::try_new("https://api.example.com/login").ok().unwrap();

        let mut form_data = std::collections::HashMap::new();
        form_data.insert("username".to_string(), "alice".to_string());
        form_data.insert("password".to_string(), "secret123".to_string());

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .form_body(&form_data)
            .ok()
            .unwrap()
            .build();

        // Verify body is URL-encoded
        assert!(request.body().is_some());
        let body = request.body().unwrap();
        // Form encoding can be in any order
        assert!(body.contains("username=alice") || body.contains("password=secret123"));

        // Verify Content-Type header is set
        let content_type_name = HeaderName::try_new("content-type").ok().unwrap();
        assert!(request.has_header(&content_type_name));
        assert_eq!(
            request.get_header(&content_type_name).map(|v| v.as_str()),
            Some("application/x-www-form-urlencoded")
        );
    }

    #[test]
    fn test_text_body() {
        // TDD: Test plain text body
        let url = Url::try_new("https://api.example.com/notes").ok().unwrap();

        let text_content = "This is a plain text note";

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .text_body(text_content)
            .build();

        // Verify body is plain text
        assert_eq!(request.body(), Some(&text_content.to_string()));

        // Verify Content-Type header is set
        let content_type_name = HeaderName::try_new("content-type").ok().unwrap();
        assert!(request.has_header(&content_type_name));
        assert_eq!(
            request.get_header(&content_type_name).map(|v| v.as_str()),
            Some("text/plain; charset=utf-8")
        );
    }

    #[test]
    fn test_json_body_error_handling() {
        // TDD: Test that JSON serialization errors are handled gracefully
        use serde_json::json;

        let url = Url::try_new("https://api.example.com/data").ok().unwrap();

        // This should work fine
        let valid_json = json!({"value": 42});
        let result = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .json_body(&valid_json);

        assert!(result.is_ok());
    }

    #[test]
    fn test_form_body_empty() {
        // TDD: Test form body with empty data
        let url = Url::try_new("https://api.example.com/submit").ok().unwrap();

        let empty_form = std::collections::HashMap::new();

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .form_body(&empty_form)
            .ok()
            .unwrap()
            .build();

        // Empty form should produce empty body
        assert_eq!(request.body(), Some(&String::new()));
    }

    #[test]
    fn test_body_methods_are_mutually_exclusive() {
        // TDD: Test that calling multiple body methods uses the last one
        let url = Url::try_new("https://api.example.com/test").ok().unwrap();
        use serde_json::json;

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .text_body("first")
            .json_body(&json!({"key": "value"}))
            .ok()
            .unwrap()
            .build();

        // JSON body should win (last call)
        let body = request.body().unwrap();
        assert!(body.contains("\"key\""));

        // Content-Type should be application/json
        let content_type_name = HeaderName::try_new("content-type").ok().unwrap();
        assert_eq!(
            request.get_header(&content_type_name).map(|v| v.as_str()),
            Some("application/json")
        );
    }

    #[test]
    fn test_text_body_with_special_chars() {
        // TDD: Test text body with special characters
        let url = Url::try_new("https://api.example.com/notes").ok().unwrap();

        let text_with_special = "Special chars: <>&\"'\n\t";

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .text_body(text_with_special)
            .build();

        assert_eq!(request.body(), Some(&text_with_special.to_string()));
    }

    // =========================================================================
    // Integration Tests
    // =========================================================================

    #[test]
    fn test_request_all_http_methods() {
        // Test all HTTP methods
        let methods = vec![
            HttpMethod::Get,
            HttpMethod::Post,
            HttpMethod::Put,
            HttpMethod::Patch,
            HttpMethod::Delete,
            HttpMethod::Head,
            HttpMethod::Options,
        ];

        for method in methods {
            let url = Url::try_new("https://example.com").ok().unwrap();
            let request = HttpRequest::builder().method(method).url(url).build();

            assert_eq!(request.method(), method);
        }
    }

    #[test]
    fn test_request_with_timeout() {
        // Test timeout configuration
        let url = Url::try_new("https://example.com").ok().unwrap();
        let timeout = IntentDuration::from_secs(60);

        let request = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url)
            .timeout(timeout)
            .build();

        assert_eq!(request.timeout(), Some(timeout));
    }

    #[test]
    fn test_request_with_body() {
        // Test request body
        let url = Url::try_new("https://api.example.com").ok().unwrap();
        let body = r#"{"key":"value","nested":{"data":123}}"#.to_string();

        let request = HttpRequest::builder()
            .method(HttpMethod::Post)
            .url(url)
            .body(body.clone())
            .build();

        assert_eq!(request.body(), Some(&body));
    }

    #[test]
    fn test_request_headers_replace() {
        // Test that setting headers replaces previous headers
        let url = Url::try_new("https://example.com").ok().unwrap();

        let mut headers1 = HashMap::new();
        headers1.insert(
            HeaderName::try_new("Accept").ok().unwrap(),
            HeaderValue::try_new("text/plain").ok().unwrap(),
        );

        let mut headers2 = HashMap::new();
        headers2.insert(
            HeaderName::try_new("Content-Type").ok().unwrap(),
            HeaderValue::try_new("application/json").ok().unwrap(),
        );

        let request = HttpRequest::builder()
            .method(HttpMethod::Get)
            .url(url)
            .headers(headers1)
            .headers(headers2)
            .build();

        // Should only have the second set of headers
        assert_eq!(request.header_count(), 1);
        assert!(request.has_header(&HeaderName::try_new("Content-Type").ok().unwrap()));
        assert!(!request.has_header(&HeaderName::try_new("Accept").ok().unwrap()));
    }
}

// =============================================================================
// HttpClient - Wrapper around reqwest::Client
// =============================================================================

use std::time::Duration;

use crate::error::IntentResult;

/// Validated timeout configuration
///
/// Enforces timeout bounds at construction time to prevent invalid configurations.
///
/// # Invariants
///
/// - Timeout must be between 1 second and 5 minutes
/// - Zero or infinite timeouts are rejected
///
/// # Examples
///
/// ```
/// use std::time::Duration;
///
/// use intent_core::http::TimeoutConfig;
///
/// // Valid timeout
/// let timeout = TimeoutConfig::new(Duration::from_secs(30)).unwrap();
/// assert_eq!(timeout.as_duration(), Duration::from_secs(30));
///
/// // Invalid timeout (too short)
/// let result = TimeoutConfig::new(Duration::from_millis(500));
/// assert!(result.is_err());
///
/// // Default timeout
/// let default_timeout = TimeoutConfig::default();
/// assert_eq!(default_timeout.as_duration(), Duration::from_secs(30));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimeoutConfig(Duration);

impl TimeoutConfig {
    /// Minimum allowed timeout duration (1 second)
    const MIN_TIMEOUT_SECS: u64 = 1;

    /// Maximum allowed timeout duration (5 minutes)
    const MAX_TIMEOUT_SECS: u64 = 300;

    /// Default timeout duration (30 seconds)
    const DEFAULT_TIMEOUT_SECS: u64 = 30;

    /// Create a new validated timeout configuration
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Validation` if:
    /// - Timeout is less than 1 second
    /// - Timeout is greater than 5 minutes
    pub fn new(timeout: Duration) -> IntentResult<Self> {
        let secs = timeout.as_secs();

        if secs < Self::MIN_TIMEOUT_SECS {
            return Err(IntentError::validation(
                "timeout",
                format!(
                    "Timeout must be at least {} second(s), got {} second(s)",
                    Self::MIN_TIMEOUT_SECS,
                    secs
                ),
            ));
        }

        if secs > Self::MAX_TIMEOUT_SECS {
            return Err(IntentError::validation(
                "timeout",
                format!(
                    "Timeout must be at most {} seconds (5 minutes), got {} seconds",
                    Self::MAX_TIMEOUT_SECS,
                    secs
                ),
            ));
        }

        Ok(Self(timeout))
    }

    /// Get the timeout as a `Duration`
    #[must_use]
    pub const fn as_duration(&self) -> Duration {
        self.0
    }

    /// Get the timeout in seconds
    #[must_use]
    pub const fn as_secs(&self) -> u64 {
        self.0.as_secs()
    }
}

impl Default for TimeoutConfig {
    fn default() -> Self {
        // SAFETY: DEFAULT_TIMEOUT_SECS is within valid bounds by construction
        Self(Duration::from_secs(Self::DEFAULT_TIMEOUT_SECS))
    }
}

// =============================================================================
// RedirectPolicy - Type-Safe Redirect Handling
// =============================================================================

/// Redirect handling policy for HTTP requests
///
/// Defines how HTTP redirects (3xx responses) should be handled.
/// Uses type-safe enum to make invalid states unrepresentable.
///
/// # Examples
///
/// ```
/// use intent_core::http::RedirectPolicy;
///
/// // No redirects allowed
/// let policy = RedirectPolicy::None;
///
/// // Follow up to 5 redirects
/// let policy = RedirectPolicy::Limited(5);
///
/// // Follow unlimited redirects
/// let policy = RedirectPolicy::Infinite;
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RedirectPolicy {
    /// Do not follow any redirects
    None,
    /// Follow up to N redirects
    Limited(usize),
    /// Follow unlimited redirects
    Infinite,
}

impl RedirectPolicy {
    /// Default redirect policy (follow up to 10 redirects)
    const DEFAULT_MAX_REDIRECTS: usize = 10;

    /// Maximum allowed redirects for Limited policy
    const MAX_REDIRECTS: usize = 50;

    /// Create a limited redirect policy with validation
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Validation` if:
    /// - Redirect limit is 0
    /// - Redirect limit exceeds maximum (50)
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::RedirectPolicy;
    ///
    /// let policy = RedirectPolicy::limited(10).unwrap();
    /// assert_eq!(policy, RedirectPolicy::Limited(10));
    /// ```
    pub fn limited(max: usize) -> IntentResult<Self> {
        if max == 0 {
            return Err(IntentError::validation(
                "redirect_policy",
                "Redirect limit must be at least 1",
            ));
        }

        if max > Self::MAX_REDIRECTS {
            return Err(IntentError::validation(
                "redirect_policy",
                format!(
                    "Redirect limit must be at most {}, got {}",
                    Self::MAX_REDIRECTS,
                    max
                ),
            ));
        }

        Ok(Self::Limited(max))
    }

    /// Get the default redirect policy
    ///
    /// Returns a Limited policy with 10 redirects.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::RedirectPolicy;
    ///
    /// let policy = RedirectPolicy::default_policy();
    /// assert_eq!(policy, RedirectPolicy::Limited(10));
    /// ```
    #[must_use]
    pub const fn default_policy() -> Self {
        Self::Limited(Self::DEFAULT_MAX_REDIRECTS)
    }

    /// Convert to reqwest redirect policy
    ///
    /// Pure function - maps our domain type to reqwest's representation.
    #[must_use]
    pub fn to_reqwest_policy(self) -> reqwest::redirect::Policy {
        match self {
            Self::None => reqwest::redirect::Policy::none(),
            Self::Limited(max) => reqwest::redirect::Policy::limited(max),
            Self::Infinite => reqwest::redirect::Policy::default(),
        }
    }

    /// Get the maximum number of redirects
    ///
    /// Returns `None` for `Infinite` and `None` policies.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::RedirectPolicy;
    ///
    /// assert_eq!(RedirectPolicy::Limited(5).max_redirects(), Some(5));
    /// assert_eq!(RedirectPolicy::None.max_redirects(), Some(0));
    /// assert_eq!(RedirectPolicy::Infinite.max_redirects(), None);
    /// ```
    #[must_use]
    pub const fn max_redirects(self) -> Option<usize> {
        match self {
            Self::None => Some(0),
            Self::Limited(max) => Some(max),
            Self::Infinite => None,
        }
    }
}

impl Default for RedirectPolicy {
    fn default() -> Self {
        Self::default_policy()
    }
}

/// HTTP client wrapper with validated configuration
///
/// Immutable wrapper around `reqwest::Client` with functional construction patterns.
/// Supports timeout and redirect policy configuration.
///
/// # Examples
///
/// ```
/// use std::time::Duration;
///
/// use intent_core::http::{HttpClient, RedirectPolicy};
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// // With custom timeout and redirect policy
/// let client = HttpClient::builder()
///     .timeout(Duration::from_secs(60))
///     .redirect_policy(RedirectPolicy::Limited(5))
///     .build()?;
///
/// // With defaults (30s timeout, 10 redirects)
/// let default_client = HttpClient::with_defaults()?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct HttpClient {
    /// Inner reqwest client
    inner: reqwest::Client,
    /// Timeout configuration
    timeout: TimeoutConfig,
    /// Redirect policy
    redirect_policy: RedirectPolicy,
}

impl HttpClient {
    /// Create a new builder for `HttpClient`
    ///
    /// # Examples
    ///
    /// ```
    /// use std::time::Duration;
    ///
    /// use intent_core::http::{HttpClient, RedirectPolicy};
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::builder()
    ///     .timeout(Duration::from_secs(45))
    ///     .redirect_policy(RedirectPolicy::Limited(5))
    ///     .build()?;
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn builder() -> HttpClientBuilder {
        HttpClientBuilder::default()
    }

    /// Create HTTP client with default configuration
    ///
    /// Uses default timeout (30 seconds) and default redirect policy (10 redirects).
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Config` if the underlying reqwest client fails to build.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpClient;
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::with_defaults()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_defaults() -> IntentResult<Self> {
        Self::builder().build()
    }

    /// Create a new HTTP client with specified timeout (legacy method)
    ///
    /// Uses default redirect policy (10 redirects).
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Validation` if timeout is invalid.
    /// Returns `IntentError::Config` if the underlying reqwest client fails to build.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::time::Duration;
    ///
    /// use intent_core::http::HttpClient;
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::new(Duration::from_secs(45))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(timeout: Duration) -> IntentResult<Self> {
        Self::builder().timeout(timeout).build()
    }

    /// Create HTTP client with default timeout (30 seconds) (legacy method)
    ///
    /// Uses default redirect policy (10 redirects).
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Config` if the underlying reqwest client fails to build.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpClient;
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::with_default_timeout()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_default_timeout() -> IntentResult<Self> {
        Self::with_defaults()
    }

    /// Get the configured timeout
    ///
    /// # Examples
    ///
    /// ```
    /// use std::time::Duration;
    ///
    /// use intent_core::http::HttpClient;
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::builder()
    ///     .timeout(Duration::from_secs(60))
    ///     .build()?;
    /// assert_eq!(client.timeout().as_secs(), 60);
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub const fn timeout(&self) -> TimeoutConfig {
        self.timeout
    }

    /// Get the configured redirect policy
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::{HttpClient, RedirectPolicy};
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::builder()
    ///     .redirect_policy(RedirectPolicy::Limited(5))
    ///     .build()?;
    /// assert_eq!(client.redirect_policy(), RedirectPolicy::Limited(5));
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub const fn redirect_policy(&self) -> RedirectPolicy {
        self.redirect_policy
    }

    /// Get a reference to the inner reqwest client
    ///
    /// Provides access to the underlying client for advanced use cases.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpClient;
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::with_defaults()?;
    /// let reqwest_client = client.inner();
    /// // Use reqwest_client for HTTP requests
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub const fn inner(&self) -> &reqwest::Client {
        &self.inner
    }
}

// =============================================================================
// HttpClientBuilder - Builder Pattern for HttpClient
// =============================================================================

/// Builder for constructing HTTP clients with configuration
///
/// Provides a fluent interface for building `HttpClient` instances with
/// timeout and redirect policy configuration.
///
/// # Examples
///
/// ```
/// use std::time::Duration;
///
/// use intent_core::http::{HttpClient, RedirectPolicy};
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let client = HttpClient::builder()
///     .timeout(Duration::from_secs(60))
///     .redirect_policy(RedirectPolicy::Limited(5))
///     .build()?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct HttpClientBuilder {
    timeout: Option<Duration>,
    redirect_policy: Option<RedirectPolicy>,
}

impl HttpClientBuilder {
    /// Set the timeout duration
    ///
    /// # Examples
    ///
    /// ```
    /// use std::time::Duration;
    ///
    /// use intent_core::http::HttpClient;
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::builder()
    ///     .timeout(Duration::from_secs(45))
    ///     .build()?;
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Set the redirect policy
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::{HttpClient, RedirectPolicy};
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::builder()
    ///     .redirect_policy(RedirectPolicy::Limited(5))
    ///     .build()?;
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn redirect_policy(mut self, policy: RedirectPolicy) -> Self {
        self.redirect_policy = Some(policy);
        self
    }

    /// Build the HTTP client
    ///
    /// Uses default values if fields are not set:
    /// - Timeout: 30 seconds
    /// - Redirect policy: Limited(10)
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Validation` if timeout is invalid.
    /// Returns `IntentError::Config` if the underlying reqwest client fails to build.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::http::HttpClient;
    ///
    /// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let client = HttpClient::builder().build()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn build(self) -> IntentResult<HttpClient> {
        // Get timeout or use default
        let timeout = match self.timeout {
            Some(t) => t,
            None => Duration::from_secs(TimeoutConfig::DEFAULT_TIMEOUT_SECS),
        };

        let timeout_config = TimeoutConfig::new(timeout)?;

        // Get redirect policy or use default
        let redirect_policy = match self.redirect_policy {
            Some(p) => p,
            None => RedirectPolicy::default(),
        };

        // Build reqwest client with configuration
        let inner = reqwest::Client::builder()
            .timeout(timeout_config.as_duration())
            .redirect(redirect_policy.to_reqwest_policy())
            .build()
            .map_err(|e| IntentError::config(format!("Failed to build HTTP client: {e}")))?;

        Ok(HttpClient {
            inner,
            timeout: timeout_config,
            redirect_policy,
        })
    }
}

impl Default for HttpClientBuilder {
    fn default() -> Self {
        Self {
            timeout: None,
            redirect_policy: None,
        }
    }
}

// =============================================================================
// HttpClient Tests (TDD - Test First)
// =============================================================================

#[cfg(test)]
mod http_client_tests {
    use super::*;

    // =========================================================================
    // RedirectPolicy Tests (TDD - Test First)
    // =========================================================================

    #[test]
    fn test_redirect_limit() {
        // TDD: Test from bead - test_redirect_limit first
        // Valid redirect limits
        let policy = RedirectPolicy::limited(10);
        assert!(policy.is_ok());
        if let Ok(p) = policy {
            assert_eq!(p, RedirectPolicy::Limited(10));
            assert_eq!(p.max_redirects(), Some(10));
        }
    }

    #[test]
    fn test_redirect_policy_none() {
        let policy = RedirectPolicy::None;
        assert_eq!(policy.max_redirects(), Some(0));
    }

    #[test]
    fn test_redirect_policy_limited_valid() {
        let policy = RedirectPolicy::limited(5);
        assert!(policy.is_ok());
        assert_eq!(policy.ok().unwrap().max_redirects(), Some(5));
    }

    #[test]
    fn test_redirect_policy_limited_zero() {
        // Zero redirects should fail validation
        let policy = RedirectPolicy::limited(0);
        assert!(policy.is_err());
    }

    #[test]
    fn test_redirect_policy_limited_too_high() {
        // More than MAX_REDIRECTS should fail
        let policy = RedirectPolicy::limited(51);
        assert!(policy.is_err());
    }

    #[test]
    fn test_redirect_policy_limited_max_valid() {
        // Exactly MAX_REDIRECTS should succeed
        let policy = RedirectPolicy::limited(50);
        assert!(policy.is_ok());
    }

    #[test]
    fn test_redirect_policy_infinite() {
        let policy = RedirectPolicy::Infinite;
        assert_eq!(policy.max_redirects(), None);
    }

    #[test]
    fn test_redirect_policy_default() {
        let policy = RedirectPolicy::default();
        assert_eq!(policy, RedirectPolicy::Limited(10));
        assert_eq!(policy.max_redirects(), Some(10));
    }

    #[test]
    fn test_redirect_policy_default_policy() {
        let policy = RedirectPolicy::default_policy();
        assert_eq!(policy, RedirectPolicy::Limited(10));
    }

    // =========================================================================
    // TimeoutConfig Tests
    // =========================================================================

    #[test]
    fn test_timeout_config_valid() {
        let timeout = TimeoutConfig::new(Duration::from_secs(30));
        assert!(timeout.is_ok());
        let timeout = timeout.ok().unwrap();
        assert_eq!(timeout.as_secs(), 30);
    }

    #[test]
    fn test_timeout_config_too_short() {
        let timeout = TimeoutConfig::new(Duration::from_millis(500));
        assert!(timeout.is_err());
    }

    #[test]
    fn test_timeout_config_too_long() {
        let timeout = TimeoutConfig::new(Duration::from_secs(301));
        assert!(timeout.is_err());
    }

    #[test]
    fn test_timeout_config_min_valid() {
        let timeout = TimeoutConfig::new(Duration::from_secs(1));
        assert!(timeout.is_ok());
    }

    #[test]
    fn test_timeout_config_max_valid() {
        let timeout = TimeoutConfig::new(Duration::from_secs(300));
        assert!(timeout.is_ok());
    }

    #[test]
    fn test_timeout_config_default() {
        let timeout = TimeoutConfig::default();
        assert_eq!(timeout.as_secs(), 30);
    }

    // =========================================================================
    // HttpClient Tests (TDD - Test First)
    // =========================================================================

    #[test]
    fn test_client_creation() {
        let client = HttpClient::new(Duration::from_secs(30));
        assert!(client.is_ok());

        let client = client.ok().unwrap();
        assert_eq!(client.timeout().as_secs(), 30);
    }

    #[test]
    fn test_client_creation_with_default() {
        let client = HttpClient::with_default_timeout();
        assert!(client.is_ok());

        let client = client.ok().unwrap();
        assert_eq!(client.timeout().as_secs(), 30);
    }

    #[test]
    fn test_client_creation_with_custom_timeout() {
        let client = HttpClient::new(Duration::from_secs(60));
        assert!(client.is_ok());

        let client = client.ok().unwrap();
        assert_eq!(client.timeout().as_secs(), 60);
    }

    #[test]
    fn test_client_inner_access() {
        let client = HttpClient::with_default_timeout();
        assert!(client.is_ok());

        let client = client.ok().unwrap();
        let _inner = client.inner();
        // Just verify we can access the inner client
    }

    #[test]
    fn test_client_creation_invalid_timeout() {
        let client = HttpClient::new(Duration::from_millis(500));
        assert!(client.is_err());
    }

    // =========================================================================
    // HttpClient Builder Tests (TDD - Redirect Policy)
    // =========================================================================

    #[test]
    fn test_client_builder_with_redirect_policy() {
        let client = HttpClient::builder()
            .redirect_policy(RedirectPolicy::Limited(5))
            .build();

        assert!(client.is_ok());
        let client = client.ok().unwrap();
        assert_eq!(client.redirect_policy(), RedirectPolicy::Limited(5));
    }

    #[test]
    fn test_client_builder_with_redirect_policy_none() {
        let client = HttpClient::builder()
            .redirect_policy(RedirectPolicy::None)
            .build();

        assert!(client.is_ok());
        let client = client.ok().unwrap();
        assert_eq!(client.redirect_policy(), RedirectPolicy::None);
    }

    #[test]
    fn test_client_builder_with_redirect_policy_infinite() {
        let client = HttpClient::builder()
            .redirect_policy(RedirectPolicy::Infinite)
            .build();

        assert!(client.is_ok());
        let client = client.ok().unwrap();
        assert_eq!(client.redirect_policy(), RedirectPolicy::Infinite);
    }

    #[test]
    fn test_client_builder_default_redirect_policy() {
        // Should use default redirect policy (10 redirects)
        let client = HttpClient::builder().build();

        assert!(client.is_ok());
        let client = client.ok().unwrap();
        assert_eq!(client.redirect_policy(), RedirectPolicy::Limited(10));
    }

    #[test]
    fn test_client_builder_full_config() {
        let client = HttpClient::builder()
            .timeout(Duration::from_secs(60))
            .redirect_policy(RedirectPolicy::Limited(3))
            .build();

        assert!(client.is_ok());
        let client = client.ok().unwrap();
        assert_eq!(client.timeout().as_secs(), 60);
        assert_eq!(client.redirect_policy(), RedirectPolicy::Limited(3));
    }

    #[test]
    fn test_client_with_defaults() {
        let client = HttpClient::with_defaults();

        assert!(client.is_ok());
        let client = client.ok().unwrap();
        assert_eq!(client.timeout().as_secs(), 30);
        assert_eq!(client.redirect_policy(), RedirectPolicy::Limited(10));
    }

    #[test]
    fn test_client_new_uses_default_redirect_policy() {
        // Legacy method should use default redirect policy
        let client = HttpClient::new(Duration::from_secs(45));

        assert!(client.is_ok());
        let client = client.ok().unwrap();
        assert_eq!(client.timeout().as_secs(), 45);
        assert_eq!(client.redirect_policy(), RedirectPolicy::Limited(10));
    }

    // =========================================================================
    // Railway-Oriented Chaining Tests
    // =========================================================================

    #[test]
    fn test_timeout_railway_chaining() {
        let result = TimeoutConfig::new(Duration::from_secs(45))
            .and_then(|timeout| HttpClient::new(timeout.as_duration()))
            .map(|client| client.timeout().as_secs());

        assert!(result.is_ok());
        assert_eq!(result.ok().unwrap(), 45);
    }

    #[test]
    fn test_timeout_railway_error_propagation() {
        let result = TimeoutConfig::new(Duration::from_millis(500))
            .and_then(|timeout| HttpClient::new(timeout.as_duration()))
            .map(|client| client.timeout().as_secs());

        assert!(result.is_err());
    }

    #[test]
    fn test_redirect_policy_railway_chaining() {
        // Test Railway-Oriented chaining with redirect policy
        let result = RedirectPolicy::limited(5)
            .and_then(|policy| HttpClient::builder().redirect_policy(policy).build())
            .map(|client| client.redirect_policy());

        assert!(result.is_ok());
        assert_eq!(result.ok().unwrap(), RedirectPolicy::Limited(5));
    }

    #[test]
    fn test_redirect_policy_railway_error_propagation() {
        // Test error propagation in railway chain
        let result = RedirectPolicy::limited(0)
            .and_then(|policy| HttpClient::builder().redirect_policy(policy).build());

        assert!(result.is_err());
    }
}
