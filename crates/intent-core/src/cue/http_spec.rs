//! HTTP request specification for CUE-based API testing
//!
//! This module provides the `HttpRequestSpec` struct for representing HTTP requests
//! parsed from CUE files. It follows strict functional programming principles with
//! zero unwraps and comprehensive validation.

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

use std::{collections::HashMap, fmt};

use serde::{Deserialize, Serialize};

use super::CueError;

/// A parsed CUE specification representing an HTTP request test case
///
/// HttpRequestSpec represents the parsed output from CUE definitions, containing
/// all necessary information to execute an HTTP request and validate its response.
///
/// # Philosophy
///
/// - **Immutable**: Once constructed, the spec cannot be modified
/// - **Validated**: All fields are validated at construction time
/// - **Type-safe**: Invalid states are unrepresentable
/// - **Serializable**: Supports JSON deserialization via serde
///
/// # Examples
///
/// ```
/// use std::collections::HashMap;
///
/// use intent_core::cue::HttpRequestSpec;
///
/// let mut headers = HashMap::new();
/// headers.insert("Content-Type".to_string(), "application/json".to_string());
///
/// let spec = HttpRequestSpec::try_new(
///     "test-api",
///     "GET",
///     "https://api.example.com/users",
///     headers,
///     None,
///     Some("200".to_string()),
/// )
/// .expect("Valid spec");
///
/// assert_eq!(spec.name(), "test-api");
/// assert_eq!(spec.method(), "GET");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HttpRequestSpec {
    /// Test case name/identifier
    name: String,
    /// HTTP method (GET, POST, PUT, DELETE, etc.)
    method: String,
    /// Target URL for the request
    url: String,
    /// HTTP headers as key-value pairs
    #[serde(default)]
    headers: HashMap<String, String>,
    /// Optional request body
    #[serde(default)]
    body: Option<String>,
    /// Optional expected response (status code, body pattern, etc.)
    #[serde(default)]
    expected: Option<String>,
}

impl HttpRequestSpec {
    /// Valid HTTP methods (subset of RFC 7231)
    const VALID_METHODS: &'static [&'static str] =
        &["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS"];

    /// Create a new HttpRequestSpec with validation
    ///
    /// # Arguments
    ///
    /// * `name` - Test case identifier
    /// * `method` - HTTP method (must be valid)
    /// * `url` - Target URL (must be valid)
    /// * `headers` - HTTP headers
    /// * `body` - Optional request body
    /// * `expected` - Optional expected response
    ///
    /// # Errors
    ///
    /// Returns `CueError` if:
    /// - Name is empty
    /// - Method is not a valid HTTP method
    /// - URL is empty or invalid
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::HashMap;
    ///
    /// use intent_core::cue::HttpRequestSpec;
    ///
    /// let spec = HttpRequestSpec::try_new(
    ///     "test",
    ///     "GET",
    ///     "https://example.com",
    ///     HashMap::new(),
    ///     None,
    ///     None,
    /// );
    /// assert!(spec.is_ok());
    /// ```
    pub fn try_new(
        name: impl Into<String>,
        method: impl Into<String>,
        url: impl Into<String>,
        headers: HashMap<String, String>,
        body: Option<String>,
        expected: Option<String>,
    ) -> Result<Self, CueError> {
        let name = name.into();
        let method = method.into();
        let url = url.into();

        // Railway-oriented validation pipeline
        Self::validate_name(&name)
            .and_then(|_| Self::validate_method(&method))
            .and_then(|_| Self::validate_url(&url))
            .map(|_| Self {
                name,
                method: method.to_uppercase(),
                url,
                headers,
                body,
                expected,
            })
    }

    /// Validate that name is not empty
    fn validate_name(name: &str) -> Result<(), CueError> {
        if name.is_empty() {
            Err(CueError::invalid_name("spec name cannot be empty"))
        } else {
            Ok(())
        }
    }

    /// Validate that method is a valid HTTP method
    fn validate_method(method: &str) -> Result<(), CueError> {
        let method_upper = method.to_uppercase();
        if Self::VALID_METHODS.contains(&method_upper.as_str()) {
            Ok(())
        } else {
            Err(CueError::invalid_method(format!(
                "method '{}' is not valid. Valid methods: {}",
                method,
                Self::VALID_METHODS.join(", ")
            )))
        }
    }

    /// Validate that URL is not empty and has proper scheme
    ///
    /// Note: This performs basic validation. Full URL parsing could be added
    /// using the `url` crate for more robust validation.
    fn validate_url(url: &str) -> Result<(), CueError> {
        if url.is_empty() {
            Err(CueError::invalid_url("URL cannot be empty"))
        } else if !url.starts_with("http://") && !url.starts_with("https://") {
            Err(CueError::invalid_url(
                "URL must start with http:// or https://",
            ))
        } else {
            Ok(())
        }
    }

    /// Get the spec name
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the HTTP method
    #[must_use]
    pub fn method(&self) -> &str {
        &self.method
    }

    /// Get the URL
    #[must_use]
    pub fn url(&self) -> &str {
        &self.url
    }

    /// Get the headers
    #[must_use]
    pub fn headers(&self) -> &HashMap<String, String> {
        &self.headers
    }

    /// Get the request body
    #[must_use]
    pub const fn body(&self) -> Option<&String> {
        self.body.as_ref()
    }

    /// Get the expected response
    #[must_use]
    pub const fn expected(&self) -> Option<&String> {
        self.expected.as_ref()
    }

    /// Get a header value by key
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::HashMap;
    ///
    /// use intent_core::cue::HttpRequestSpec;
    ///
    /// let mut headers = HashMap::new();
    /// headers.insert("Content-Type".to_string(), "application/json".to_string());
    ///
    /// let spec = HttpRequestSpec::try_new("test", "GET", "https://example.com", headers, None, None)
    ///     .expect("Valid spec");
    ///
    /// assert_eq!(
    ///     spec.get_header("Content-Type"),
    ///     Some(&"application/json".to_string())
    /// );
    /// assert_eq!(spec.get_header("Accept"), None);
    /// ```
    #[must_use]
    pub fn get_header(&self, key: &str) -> Option<&String> {
        self.headers.get(key)
    }

    /// Check if a header exists
    #[must_use]
    pub fn has_header(&self, key: &str) -> bool {
        self.headers.contains_key(key)
    }

    /// Get the number of headers
    #[must_use]
    pub fn header_count(&self) -> usize {
        self.headers.len()
    }
}

impl fmt::Display for HttpRequestSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.name, self.method, self.url)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cue_spec_fields() {
        // TDD: Test all required fields exist and are accessible
        let mut headers = HashMap::new();
        headers.insert("Content-Type".to_string(), "application/json".to_string());
        headers.insert("Authorization".to_string(), "Bearer token123".to_string());

        let spec = HttpRequestSpec::try_new(
            "api-test",
            "POST",
            "https://api.example.com/users",
            headers.clone(),
            Some("{\"name\": \"John\"}".to_string()),
            Some("201".to_string()),
        )
        .expect("Valid spec should be created");

        // Test all field getters
        assert_eq!(spec.name(), "api-test");
        assert_eq!(spec.method(), "POST"); // Should be uppercased
        assert_eq!(spec.url(), "https://api.example.com/users");
        assert_eq!(spec.headers().len(), 2);
        assert_eq!(
            spec.get_header("Content-Type"),
            Some(&"application/json".to_string())
        );
        assert_eq!(spec.body(), Some(&"{\"name\": \"John\"}".to_string()));
        assert_eq!(spec.expected(), Some(&"201".to_string()));
    }

    #[test]
    fn test_cue_spec_optional_fields() {
        // Test that body and expected are truly optional
        let spec = HttpRequestSpec::try_new(
            "simple-get",
            "GET",
            "https://example.com",
            HashMap::new(),
            None,
            None,
        )
        .expect("Valid spec with no body/expected");

        assert_eq!(spec.body(), None);
        assert_eq!(spec.expected(), None);
    }

    #[test]
    fn test_cue_spec_method_validation() {
        // Valid methods should work (case-insensitive)
        for method in &[
            "GET", "get", "Post", "PUT", "delete", "PATCH", "HEAD", "OPTIONS",
        ] {
            let result = HttpRequestSpec::try_new(
                "test",
                *method,
                "https://example.com",
                HashMap::new(),
                None,
                None,
            );
            assert!(result.is_ok(), "Method '{}' should be valid", method);
        }

        // Invalid method should fail
        let result = HttpRequestSpec::try_new(
            "test",
            "INVALID",
            "https://example.com",
            HashMap::new(),
            None,
            None,
        );
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::InvalidMethod(_))));
    }

    #[test]
    fn test_cue_spec_url_validation() {
        // Valid URLs
        for url in &[
            "https://example.com",
            "http://localhost:8080",
            "https://api.example.com/v1/users",
        ] {
            let result = HttpRequestSpec::try_new("test", "GET", *url, HashMap::new(), None, None);
            assert!(result.is_ok(), "URL '{}' should be valid", url);
        }

        // Invalid URLs
        for url in &["", "ftp://example.com", "not-a-url"] {
            let result = HttpRequestSpec::try_new("test", "GET", *url, HashMap::new(), None, None);
            assert!(result.is_err(), "URL '{}' should be invalid", url);
        }
    }

    #[test]
    fn test_cue_spec_name_validation() {
        // Empty name should fail
        let result =
            HttpRequestSpec::try_new("", "GET", "https://example.com", HashMap::new(), None, None);
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::InvalidName(_))));
    }

    #[test]
    fn test_cue_spec_serde() {
        // Test JSON deserialization
        let json = r#"{
            "name": "test-api",
            "method": "POST",
            "url": "https://api.example.com/users",
            "headers": {
                "Content-Type": "application/json"
            },
            "body": "{\"name\": \"Alice\"}",
            "expected": "201"
        }"#;

        let spec: HttpRequestSpec =
            serde_json::from_str(json).expect("Valid JSON should deserialize");
        assert_eq!(spec.name(), "test-api");
        assert_eq!(spec.method(), "POST");
        assert_eq!(spec.url(), "https://api.example.com/users");

        // Test with missing optional fields
        let json_minimal = r#"{
            "name": "get-users",
            "method": "GET",
            "url": "https://api.example.com/users"
        }"#;

        let spec: HttpRequestSpec = serde_json::from_str(json_minimal)
            .expect("Valid JSON with defaults should deserialize");
        assert_eq!(spec.body(), None);
        assert_eq!(spec.expected(), None);
        assert_eq!(spec.headers().len(), 0);
    }

    #[test]
    fn test_cue_spec_header_methods() {
        let mut headers = HashMap::new();
        headers.insert("X-API-Key".to_string(), "secret123".to_string());

        let spec =
            HttpRequestSpec::try_new("test", "GET", "https://example.com", headers, None, None)
                .expect("Valid spec");

        assert!(spec.has_header("X-API-Key"));
        assert!(!spec.has_header("Missing-Header"));
        assert_eq!(spec.header_count(), 1);
    }

    #[test]
    fn test_cue_spec_display() {
        let spec = HttpRequestSpec::try_new(
            "my-test",
            "GET",
            "https://example.com/api",
            HashMap::new(),
            None,
            None,
        )
        .expect("Valid spec");

        let display = format!("{}", spec);
        assert!(display.contains("my-test"));
        assert!(display.contains("GET"));
        assert!(display.contains("https://example.com/api"));
    }
}
