//! HTTP request body encoding
//!
//! Provides functional, Railway-Oriented body encoding for HTTP requests.
//! Supports JSON, form-urlencoded, and plain text encodings.
//!
//! # Philosophy
//!
//! - **Type-safe**: Use validated types for encoding
//! - **Zero panics**: All encoding operations return `Result`
//! - **Railway-oriented**: Chain encoding operations with combinators
//! - **Automatic headers**: Content-Type set automatically based on encoding
//!
//! # Examples
//!
//! ```
//! use intent_core::http::HttpRequest;
//! use intent_core::types::HttpMethod;
//! use serde_json::json;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let json_data = json!({"name": "Alice", "age": 30});
//!
//! let request = HttpRequest::builder()
//!     .method(HttpMethod::Post)
//!     .json_body(&json_data)?
//!     .build();
//! # Ok(())
//! # }
//! ```

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

use std::collections::HashMap;

use serde::Serialize;

use crate::{
    error::IntentError,
    types::{HeaderName, HeaderValue},
};

// =============================================================================
// Body Encoding Functions (Pure, Railway-Oriented)
// =============================================================================

/// Encode a value as JSON
///
/// Railway-oriented function that serializes a value to JSON string.
/// Returns error if serialization fails.
///
/// # Errors
///
/// Returns `IntentError::Serialization` if:
/// - Value cannot be serialized to JSON
/// - Value contains invalid data (NaN, Infinity in numbers)
///
/// # Examples
///
/// ```
/// use serde_json::json;
/// use intent_core::http_body_encoding::encode_json;
///
/// let data = json!({"name": "Alice"});
/// let result = encode_json(&data);
/// assert!(result.is_ok());
/// ```
pub fn encode_json<T: Serialize>(value: &T) -> Result<String, IntentError> {
    serde_json::to_string(value)
        .map_err(|e| IntentError::validation("json_encoding", format!("Failed to encode JSON: {e}")))
}

/// Encode form data as application/x-www-form-urlencoded
///
/// Railway-oriented function that URL-encodes key-value pairs.
/// Keys and values are percent-encoded according to RFC 3986.
///
/// # Errors
///
/// Returns `IntentError::Validation` if:
/// - Keys or values contain invalid characters
///
/// # Examples
///
/// ```
/// use std::collections::HashMap;
///
/// use intent_core::http_body_encoding::encode_form;
///
/// let mut form = HashMap::new();
/// form.insert("username".to_string(), "alice".to_string());
/// let result = encode_form(&form);
/// assert!(result.is_ok());
/// ```
pub fn encode_form(data: &HashMap<String, String>) -> Result<String, IntentError> {
    // Railway pattern: transform map to vector of encoded pairs, then join
    data.iter()
        .map(|(key, value)| {
            // Percent-encode key and value
            let encoded_key = percent_encode(key);
            let encoded_value = percent_encode(value);
            Ok(format!("{encoded_key}={encoded_value}"))
        })
        .collect::<Result<Vec<_>, IntentError>>()
        .map(|pairs| pairs.join("&"))
}

/// Percent-encode a string for URL encoding
///
/// Pure function that encodes special characters according to RFC 3986.
/// Alphanumeric characters, hyphens, underscores, periods, and tildes are not encoded.
fn percent_encode(input: &str) -> String {
    input
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | '.' | '~') {
                c.to_string()
            } else {
                // Percent-encode character
                c.to_string()
                    .bytes()
                    .map(|b| format!("%{b:02X}"))
                    .collect::<String>()
            }
        })
        .collect()
}

/// Create Content-Type header for JSON
///
/// Pure function that creates a validated Content-Type header.
///
/// # Errors
///
/// Returns `IntentError::Validation` if header construction fails.
pub fn json_content_type() -> Result<(HeaderName, HeaderValue), IntentError> {
    HeaderName::try_new("content-type")
        .and_then(|name| HeaderValue::try_new("application/json").map(|value| (name, value)))
}

/// Create Content-Type header for form-urlencoded
///
/// Pure function that creates a validated Content-Type header.
///
/// # Errors
///
/// Returns `IntentError::Validation` if header construction fails.
pub fn form_content_type() -> Result<(HeaderName, HeaderValue), IntentError> {
    HeaderName::try_new("content-type").and_then(|name| {
        HeaderValue::try_new("application/x-www-form-urlencoded").map(|value| (name, value))
    })
}

/// Create Content-Type header for plain text
///
/// Pure function that creates a validated Content-Type header.
///
/// # Errors
///
/// Returns `IntentError::Validation` if header construction fails.
pub fn text_content_type() -> Result<(HeaderName, HeaderValue), IntentError> {
    HeaderName::try_new("content-type").and_then(|name| {
        HeaderValue::try_new("text/plain; charset=utf-8").map(|value| (name, value))
    })
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    // =========================================================================
    // JSON Encoding Tests
    // =========================================================================

    #[test]
    fn test_encode_json_simple() {
        let data = json!({"name": "Alice", "age": 30});
        let result = encode_json(&data);
        assert!(result.is_ok());

        let encoded = result.ok().unwrap();
        assert!(encoded.contains("\"name\":\"Alice\""));
        assert!(encoded.contains("\"age\":30"));
    }

    #[test]
    fn test_encode_json_nested() {
        let data = json!({
            "user": {
                "name": "Bob",
                "profile": {
                    "email": "bob@example.com"
                }
            }
        });
        let result = encode_json(&data);
        assert!(result.is_ok());

        let encoded = result.ok().unwrap();
        assert!(encoded.contains("\"name\":\"Bob\""));
        assert!(encoded.contains("\"email\":\"bob@example.com\""));
    }

    #[test]
    fn test_encode_json_array() {
        let data = json!([1, 2, 3, 4, 5]);
        let result = encode_json(&data);
        assert!(result.is_ok());

        let encoded = result.ok().unwrap();
        assert_eq!(encoded, "[1,2,3,4,5]");
    }

    #[test]
    fn test_encode_json_empty_object() {
        let data = json!({});
        let result = encode_json(&data);
        assert!(result.is_ok());
        assert_eq!(result.ok().unwrap(), "{}");
    }

    // =========================================================================
    // Form Encoding Tests
    // =========================================================================

    #[test]
    fn test_encode_form_simple() {
        let mut form = HashMap::new();
        form.insert("username".to_string(), "alice".to_string());
        form.insert("password".to_string(), "secret".to_string());

        let result = encode_form(&form);
        assert!(result.is_ok());

        let encoded = result.ok().unwrap();
        // Order is not guaranteed, check both fields exist
        assert!(encoded.contains("username=alice"));
        assert!(encoded.contains("password=secret"));
        assert!(encoded.contains('&'));
    }

    #[test]
    fn test_encode_form_empty() {
        let empty_form = HashMap::new();
        let result = encode_form(&empty_form);
        assert!(result.is_ok());
        assert_eq!(result.ok().unwrap(), "");
    }

    #[test]
    fn test_encode_form_single_field() {
        let mut form = HashMap::new();
        form.insert("token".to_string(), "abc123".to_string());

        let result = encode_form(&form);
        assert!(result.is_ok());
        assert_eq!(result.ok().unwrap(), "token=abc123");
    }

    #[test]
    fn test_encode_form_special_chars() {
        let mut form = HashMap::new();
        form.insert("email".to_string(), "user@example.com".to_string());
        form.insert("query".to_string(), "hello world".to_string());

        let result = encode_form(&form);
        assert!(result.is_ok());

        let encoded = result.ok().unwrap();
        // @ and space should be percent-encoded
        assert!(encoded.contains("email=user%40example.com"));
        assert!(encoded.contains("query=hello%20world"));
    }

    // =========================================================================
    // Percent Encoding Tests
    // =========================================================================

    #[test]
    fn test_percent_encode_alphanumeric() {
        assert_eq!(percent_encode("abc123"), "abc123");
    }

    #[test]
    fn test_percent_encode_safe_chars() {
        assert_eq!(
            percent_encode("hello-world_test.foo~bar"),
            "hello-world_test.foo~bar"
        );
    }

    #[test]
    fn test_percent_encode_space() {
        assert_eq!(percent_encode("hello world"), "hello%20world");
    }

    #[test]
    fn test_percent_encode_special() {
        assert_eq!(percent_encode("user@example.com"), "user%40example.com");
    }

    #[test]
    fn test_percent_encode_symbols() {
        assert_eq!(percent_encode("a+b=c"), "a%2Bb%3Dc");
    }

    // =========================================================================
    // Content-Type Header Tests
    // =========================================================================

    #[test]
    fn test_json_content_type() {
        let result = json_content_type();
        assert!(result.is_ok());

        if let Ok((name, value)) = result {
            assert_eq!(name.as_str(), "content-type");
            assert_eq!(value.as_str(), "application/json");
        }
    }

    #[test]
    fn test_form_content_type() {
        let result = form_content_type();
        assert!(result.is_ok());

        if let Ok((name, value)) = result {
            assert_eq!(name.as_str(), "content-type");
            assert_eq!(value.as_str(), "application/x-www-form-urlencoded");
        }
    }

    #[test]
    fn test_text_content_type() {
        let result = text_content_type();
        assert!(result.is_ok());

        if let Ok((name, value)) = result {
            assert_eq!(name.as_str(), "content-type");
            assert_eq!(value.as_str(), "text/plain; charset=utf-8");
        }
    }
}
