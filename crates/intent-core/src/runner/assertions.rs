#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

//! Assertion functions for HTTP response validation
//!
//! This module provides pure, functional assertion functions for validating
//! HTTP responses against expected values using Railway-Oriented Programming.

use crate::types::{HttpResponse, StatusCode};

use super::{Assertion, AssertionKind};

/// Assert that an HTTP response has the expected status code
///
/// Pure function that compares actual vs expected status codes.
#[must_use]
pub fn assert_status(response: &HttpResponse, expected: StatusCode) -> Assertion {
    let actual = response.status();
    let passed = actual == expected;

    Assertion::new(
        AssertionKind::StatusEquals,
        expected.to_string(),
        actual.to_string(),
        passed,
    )
}

// JSON Path Assertions
// =============================================================================

/// Assert that a JSON path in the response body matches the expected value
///
/// This function extracts a value from JSON using JSONPath syntax and compares
/// it with an expected value. Follows Railway-Oriented Programming patterns
/// with zero panics.
///
/// # JSONPath Syntax Supported
///
/// - `$.field` - Top-level field
/// - `$.nested.field` - Nested fields
/// - `$.array[0]` - Array indexing
/// - `$.data.items[0].name` - Combined navigation
///
/// # Philosophy
///
/// - **Pure**: Deterministic parsing and comparison
/// - **Railway-Oriented**: Returns Result for error handling
/// - **Zero panics**: All errors handled via Result<T, IntentError>
/// - **Functional**: Uses combinator chains (map, and_then)
///
/// # Examples
///
/// ```
/// use intent_core::runner::assertions::assert_json_path;
///
/// let json = r#"{"user": {"name": "Alice"}}"#;
/// let result = assert_json_path(json, "$.user.name", "Alice");
/// assert!(result.is_ok());
/// let assertion = result.unwrap();
/// assert!(assertion.passed());
/// ```
///
/// # Arguments
///
/// * `response` - JSON string response body
/// * `path` - JSONPath expression (must start with `$`)
/// * `expected` - Expected value as string
///
/// # Returns
///
/// - `Ok(Assertion)` - Assertion result with pass/fail and actual value
/// - `Err(IntentError::JsonParse)` - If JSON parsing fails
/// - `Err(IntentError::Validation)` - If path is invalid or doesn't exist
///
/// # Errors
///
/// Returns `IntentError::JsonParse` if the response is not valid JSON.
/// Returns `IntentError::Validation` if the path is invalid or doesn't exist.
pub fn assert_json_path(
    response: &str,
    path: &str,
    expected: &str,
) -> Result<Assertion, crate::error::IntentError> {
    // Railway-oriented pipeline:
    // 1. Parse JSON
    // 2. Validate and extract JSONPath
    // 3. Navigate to value
    // 4. Convert to string and compare
    // 5. Create assertion result

    parse_json(response)
        .and_then(|json| extract_json_value(&json, path))
        .map(|actual_value| {
            let actual_str = json_value_to_string(&actual_value);
            let passed = actual_str == expected;
            Assertion::new(
                AssertionKind::BodyJsonPath,
                expected.to_string(),
                actual_str,
                passed,
            )
        })
}

/// Parse JSON string into serde_json::Value
///
/// Pure function that transforms a JSON string into a structured value.
/// Uses Railway-Oriented Programming by returning Result.
fn parse_json(json_str: &str) -> Result<serde_json::Value, crate::error::IntentError> {
    serde_json::from_str(json_str).map_err(crate::error::IntentError::from)
}

/// Extract value from JSON using JSONPath syntax
///
/// Supports:
/// - $.field - Top-level field
/// - $.nested.field - Nested fields
/// - $.array[0] - Array indexing
/// - $.data.items[0].name - Combined navigation
fn extract_json_value(
    json: &serde_json::Value,
    path: &str,
) -> Result<serde_json::Value, crate::error::IntentError> {
    // Validate path starts with $
    if !path.starts_with('$') {
        return Err(crate::error::IntentError::validation(
            "JSONPath",
            format!("Path must start with '$', got: {path}"),
        ));
    }

    // Remove leading $ and split into segments
    let path_without_root = path.strip_prefix("$.").or_else(|| path.strip_prefix('$'))
        .ok_or_else(|| crate::error::IntentError::validation("JSONPath", format!("Invalid path format: {path}")))?;

    // Handle root-only path ($)
    if path_without_root.is_empty() {
        return Ok(json.clone());
    }

    // Navigate through path segments
    navigate_json_path(json, path_without_root)
}

/// Navigate through JSON path segments
///
/// Recursively processes path segments, handling both object fields and array indices.
fn navigate_json_path(
    value: &serde_json::Value,
    remaining_path: &str,
) -> Result<serde_json::Value, crate::error::IntentError> {
    if remaining_path.is_empty() {
        return Ok(value.clone());
    }

    // Parse next segment
    let (segment, rest) = parse_next_segment(remaining_path)?;

    // Handle array index notation: field[index]
    if let Some((field_name, index)) = parse_array_notation(&segment)? {
        // Navigate to field first
        let field_value = value
            .get(field_name)
            .ok_or_else(|| {
                crate::error::IntentError::validation(
                    "JSONPath",
                    format!("Field '{field_name}' not found"),
                )
            })?;

        // Then index into array
        let indexed_value = field_value
            .get(index)
            .ok_or_else(|| {
                crate::error::IntentError::validation(
                    "JSONPath",
                    format!("Array index {index} out of bounds"),
                )
            })?;

        // Continue with remaining path
        navigate_json_path(indexed_value, rest)
    } else if segment.contains('[') {
        // Pure array index: [0]
        let index = parse_index(&segment)?;
        let indexed_value = value
            .get(index)
            .ok_or_else(|| {
                crate::error::IntentError::validation(
                    "JSONPath",
                    format!("Array index {index} out of bounds"),
                )
            })?;
        navigate_json_path(indexed_value, rest)
    } else {
        // Simple field access
        let next_value = value
            .get(&segment)
            .ok_or_else(|| {
                crate::error::IntentError::validation(
                    "JSONPath",
                    format!("Field '{segment}' not found"),
                )
            })?;
        navigate_json_path(next_value, rest)
    }
}

/// Parse the next segment from a path
///
/// Splits on '.' to get the next field/index and the remaining path.
fn parse_next_segment(path: &str) -> Result<(String, &str), crate::error::IntentError> {
    // Find the next dot, accounting for array brackets
    let mut bracket_depth = 0;
    let split_pos = path.chars().position(|c| {
        match c {
            '[' => {
                bracket_depth += 1;
                false
            }
            ']' => {
                bracket_depth -= 1;
                false
            }
            '.' if bracket_depth == 0 => true,
            _ => false,
        }
    });

    match split_pos {
        Some(pos) => {
            let segment = path[..pos].to_string();
            let rest = &path[pos + 1..];
            Ok((segment, rest))
        }
        None => Ok((path.to_string(), "")),
    }
}

/// Parse array notation like "field[0]" into (field, index)
///
/// Returns None if not array notation, Some((field, index)) if valid.
fn parse_array_notation(segment: &str) -> Result<Option<(&str, usize)>, crate::error::IntentError> {
    if !segment.contains('[') {
        return Ok(None);
    }

    let parts: Vec<&str> = segment.split('[').collect();
    if parts.len() != 2 {
        return Err(crate::error::IntentError::validation(
            "JSONPath",
            format!("Invalid array notation: {segment}"),
        ));
    }

    let field_name = parts[0];
    let index_str = parts[1].trim_end_matches(']');
    let index = index_str.parse::<usize>().map_err(|_| {
        crate::error::IntentError::validation(
            "JSONPath",
            format!("Invalid array index: {index_str}"),
        )
    })?;

    Ok(Some((field_name, index)))
}

/// Parse pure array index like "[0]"
fn parse_index(segment: &str) -> Result<usize, crate::error::IntentError> {
    let index_str = segment.trim_start_matches('[').trim_end_matches(']');
    index_str.parse::<usize>().map_err(|_| {
        crate::error::IntentError::validation(
            "JSONPath",
            format!("Invalid array index: {segment}"),
        )
    })
}

/// Convert JSON value to string representation
///
/// Pure function that handles all JSON types:
/// - String: returns the string content (without quotes)
/// - Number: formats as string
/// - Boolean: "true" or "false"
/// - Null: "null"
/// - Object/Array: JSON representation
fn json_value_to_string(value: &serde_json::Value) -> String {
    match value {
        serde_json::Value::String(s) => s.clone(),
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::Bool(b) => b.to_string(),
        serde_json::Value::Null => "null".to_string(),
        serde_json::Value::Array(_) | serde_json::Value::Object(_) => value.to_string(),
    }
}

// =============================================================================
// JSON Path Tests (TDD)
// =============================================================================

#[cfg(test)]
mod json_path_tests {
    use super::*;
#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::types::Url;

    use super::*;

    const TEST_URL: &str = "https://example.com";

    fn create_response_with_status(status: StatusCode) -> HttpResponse {
        let url = Url::try_new(TEST_URL)
            .or_else(|_| Url::try_new("http://localhost"))
            .unwrap_or_else(|_| {
                // Hardcoded URLs should always be valid in tests
                // If this fails, the test setup is broken
                panic!("Test URL construction failed - this should never happen")
            });

        HttpResponse::new(status, HashMap::new(), String::new(), 0, url)
    }

    #[test]
    fn test_assert_status_matching_200() {
        let response = create_response_with_status(StatusCode::ok());
        let assertion = assert_status(&response, StatusCode::ok());
        assert!(assertion.passed());
        assert_eq!(assertion.expected(), "200");
        assert_eq!(assertion.actual(), "200");
    }

    #[test]
    fn test_assert_status_non_matching() {
        let response = create_response_with_status(StatusCode::not_found());
        let assertion = assert_status(&response, StatusCode::ok());
        assert!(!assertion.passed());
        assert_eq!(assertion.expected(), "200");
        assert_eq!(assertion.actual(), "404");
    }

    #[test]
    fn test_assert_status_2xx_range() {
        for (status, code_str) in &[
            (StatusCode::ok(), "200"),
            (StatusCode::created(), "201"),
            (StatusCode::no_content(), "204"),
        ] {
            let response = create_response_with_status(*status);
            let assertion = assert_status(&response, *status);
            assert!(assertion.passed());
            assert_eq!(assertion.expected(), code_str);
        }
    }
}

// =============================================================================
// Header Assertions
// =============================================================================

/// Assert that a specific header exists in the HTTP response
///
/// Pure function that checks for header existence without validating value.
/// Header name comparison is case-insensitive per HTTP specification.
#[must_use]
pub fn assert_header_exists(response: &HttpResponse, header_name: &str) -> Assertion {
    use crate::types::HeaderName;
    
    let header_exists = HeaderName::try_new(header_name)
        .ok()
        .and_then(|name| response.header(&name))
        .is_some();

    Assertion::new(
        AssertionKind::HeaderExists,
        header_name.to_string(),
        if header_exists {
            "exists".to_string()
        } else {
            "missing".to_string()
        },
        header_exists,
    )
}

/// Assert that a specific header exists and has the expected value
///
/// Validates both header existence and value equality.
/// Both comparisons are case-insensitive per HTTP spec.
///
/// # Errors
///
/// Returns `IntentError::Validation` if header name is invalid.
pub fn assert_header_equals(
    response: &HttpResponse,
    header_name: &str,
    expected_value: &str,
) -> Result<Assertion, IntentError> {
    use crate::{error::IntentError, types::HeaderName};
    
    HeaderName::try_new(header_name).map(|name| {
        let actual_value = response.header(&name);

        let (passed, actual_str) = actual_value
            .map(|value| {
                let matches = value.as_str().eq_ignore_ascii_case(expected_value);
                (matches, value.as_str().to_string())
            })
            .unwrap_or_else(|| (false, "missing".to_string()));

        Assertion::new(
            AssertionKind::HeaderEquals,
            expected_value.to_string(),
            actual_str,
            passed,
        )
    })
}

    // =========================================================================
    // TDD Tests: assert_body_contains (BEAD intent-cli-hz5t)
    // =========================================================================

    /// Create a test HTTP response with the given body
    ///
    /// Helper function to create responses for testing.
    /// Uses default values for status, headers, timing, and URL.
    #[allow(clippy::unwrap_used)] // Test helper only - acceptable in tests
    fn create_response_with_body(body: String) -> HttpResponse {
        HttpResponse::new(
            StatusCode::ok(),
            HashMap::new(),
            body,
            0,
            Url::try_new("https://test.example.com").unwrap(),
        )
    }

    #[test]
    fn test_assert_body_contains_pass_simple() {
        // Arrange: Create response with known body content
        let response = create_response_with_body(String::from("Hello, World!"));

        // Act: Assert body contains substring
        let assertion = assert_body_contains(&response, "World");

        // Assert: Should pass
        assert!(assertion.passed());
        assert!(!assertion.failed());
        assert_eq!(assertion.kind(), &AssertionKind::BodyContains);
        assert_eq!(assertion.expected(), "World");
        assert_eq!(assertion.actual(), "Hello, World!");
    }

    #[test]
    fn test_assert_body_contains_pass_json() {
        // Arrange: Create response with JSON body
        let body = r#"{"status":"success","data":{"id":123,"name":"test"}}"#;
        let response = create_response_with_body(String::from(body));

        // Act: Assert body contains JSON fragment
        let assertion = assert_body_contains(&response, "\"status\":\"success\"");

        // Assert: Should pass
        assert!(assertion.passed());
        assert_eq!(assertion.expected(), "\"status\":\"success\"");
        assert_eq!(assertion.actual(), body);
    }

    #[test]
    fn test_assert_body_contains_fail_not_found() {
        // Arrange: Create response with known body
        let response = create_response_with_body(String::from("Hello, World!"));

        // Act: Assert body contains substring that doesn't exist
        let assertion = assert_body_contains(&response, "Goodbye");

        // Assert: Should fail
        assert!(assertion.failed());
        assert!(!assertion.passed());
        assert_eq!(assertion.kind(), &AssertionKind::BodyContains);
        assert_eq!(assertion.expected(), "Goodbye");
        assert_eq!(assertion.actual(), "Hello, World!");
    }

    #[test]
    fn test_assert_body_contains_empty_needle() {
        // Arrange: Create response with body
        let response = create_response_with_body(String::from("Hello, World!"));

        // Act: Assert body contains empty string
        let assertion = assert_body_contains(&response, "");

        // Assert: Empty string is always found (Rust str::contains behavior)
        assert!(assertion.passed());
        assert_eq!(assertion.expected(), "");
    }

    #[test]
    fn test_assert_body_contains_empty_body() {
        // Arrange: Create response with empty body
        let response = create_response_with_body(String::new());

        // Act: Assert empty body contains substring
        let assertion = assert_body_contains(&response, "test");

        // Assert: Should fail
        assert!(assertion.failed());
        assert_eq!(assertion.expected(), "test");
        assert_eq!(assertion.actual(), "");
    }

    #[test]
    fn test_assert_body_contains_case_sensitive() {
        // Arrange: Create response with mixed case body
        let response = create_response_with_body(String::from("Hello, World!"));

        // Act: Search for lowercase "world"
        let assertion = assert_body_contains(&response, "world");

        // Assert: Should fail (case-sensitive)
        assert!(assertion.failed());
        assert_eq!(assertion.expected(), "world");

        // Act: Search for correct case "World"
        let assertion_pass = assert_body_contains(&response, "World");

        // Assert: Should pass
        assert!(assertion_pass.passed());
    }

    #[test]
    fn test_assert_body_contains_truncates_long_body() {
        // Arrange: Create response with very long body (> 200 chars)
        let long_body = "a".repeat(500);
        let response = create_response_with_body(long_body);

        // Act: Assert contains
        let assertion = assert_body_contains(&response, "aaa");

        // Assert: Should pass and truncate actual value
        assert!(assertion.passed());
        assert_eq!(assertion.expected(), "aaa");
        // Actual should be truncated to 200 chars + "..."
        assert_eq!(assertion.actual().len(), 203); // 200 + "..."
        assert!(assertion.actual().ends_with("..."));
        assert_eq!(&assertion.actual()[..200], &"a".repeat(200));
    }

    #[test]
    fn test_assert_body_contains_exactly_200_chars() {
        // Arrange: Create response with exactly 200 chars (edge case)
        let body = "a".repeat(200);
        let response = create_response_with_body(body.clone());

        // Act: Assert contains
        let assertion = assert_body_contains(&response, "aaa");

        // Assert: Should not truncate (exactly at limit)
        assert!(assertion.passed());
        assert_eq!(assertion.actual(), body);
        assert!(!assertion.actual().ends_with("..."));
    }

    #[test]
    fn test_assert_body_contains_201_chars() {
        // Arrange: Create response with 201 chars (just over limit)
        let body = "a".repeat(201);
        let response = create_response_with_body(body);

        // Act: Assert contains
        let assertion = assert_body_contains(&response, "aaa");

        // Assert: Should truncate
        assert!(assertion.passed());
        assert_eq!(assertion.actual().len(), 203); // 200 + "..."
        assert!(assertion.actual().ends_with("..."));
    }


    #[test]
    fn test_assert_json_path_simple_field_match() {
        // Arrange: Valid JSON with top-level field
        let json = r#"{"status": "success"}"#;
        let path = "$.status";
        let expected = "success";

        // Act: Assert the path
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.kind(), &AssertionKind::BodyJsonPath);
            assert_eq!(assertion.expected(), expected);
            assert_eq!(assertion.actual(), "success");
        }
    }

    #[test]
    fn test_assert_json_path_simple_field_mismatch() {
        // Arrange: Valid JSON with non-matching value
        let json = r#"{"status": "error"}"#;
        let path = "$.status";
        let expected = "success";

        // Act: Assert the path
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass but fail the assertion
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.failed());
            assert_eq!(assertion.kind(), &AssertionKind::BodyJsonPath);
            assert_eq!(assertion.expected(), expected);
            assert_eq!(assertion.actual(), "error");
        }
    }

    #[test]
    fn test_assert_json_path_nested_field_match() {
        // Arrange: Valid JSON with nested fields
        let json = r#"{"user": {"name": "Alice", "age": 30}}"#;
        let path = "$.user.name";
        let expected = "Alice";

        // Act: Assert the nested path
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.actual(), "Alice");
        }
    }

    #[test]
    fn test_assert_json_path_array_index() {
        // Arrange: Valid JSON with array
        let json = r#"{"items": ["apple", "banana", "cherry"]}"#;
        let path = "$.items[1]";
        let expected = "banana";

        // Act: Assert the array element
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.actual(), "banana");
        }
    }

    #[test]
    fn test_assert_json_path_complex_nested_array() {
        // Arrange: Valid JSON with nested objects and arrays
        let json = r#"{"data": {"items": [{"name": "first"}, {"name": "second"}]}}"#;
        let path = "$.data.items[0].name";
        let expected = "first";

        // Act: Assert the complex path
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.actual(), "first");
        }
    }

    #[test]
    fn test_assert_json_path_invalid_json() {
        // Arrange: Invalid JSON string
        let json = r#"{"invalid": json}"#;
        let path = "$.field";
        let expected = "value";

        // Act: Attempt to assert on invalid JSON
        let result = assert_json_path(json, path, expected);

        // Assert: Should return JsonParse error
        assert!(result.is_err());
        if let Err(error) = result {
            assert!(matches!(error, crate::error::IntentError::JsonParse { .. }));
        }
    }

    #[test]
    fn test_assert_json_path_missing_field() {
        // Arrange: Valid JSON but path doesn't exist
        let json = r#"{"status": "success"}"#;
        let path = "$.nonexistent";
        let expected = "value";

        // Act: Attempt to assert on missing path
        let result = assert_json_path(json, path, expected);

        // Assert: Should return Validation error
        assert!(result.is_err());
        if let Err(error) = result {
            assert!(matches!(error, crate::error::IntentError::Validation { .. }));
        }
    }

    #[test]
    fn test_assert_json_path_invalid_path_format() {
        // Arrange: Valid JSON but invalid path syntax
        let json = r#"{"status": "success"}"#;
        let path = "invalid.path.without.dollar";
        let expected = "value";

        // Act: Attempt to assert with invalid path
        let result = assert_json_path(json, path, expected);

        // Assert: Should return Validation error
        assert!(result.is_err());
        if let Err(error) = result {
            assert!(matches!(error, crate::error::IntentError::Validation { .. }));
        }
    }

    #[test]
    fn test_assert_json_path_array_out_of_bounds() {
        // Arrange: Valid JSON with array but invalid index
        let json = r#"{"items": ["one", "two"]}"#;
        let path = "$.items[5]";
        let expected = "value";

        // Act: Attempt to access out of bounds index
        let result = assert_json_path(json, path, expected);

        // Assert: Should return Validation error
        assert!(result.is_err());
        if let Err(error) = result {
            assert!(matches!(error, crate::error::IntentError::Validation { .. }));
        }
    }

    #[test]
    fn test_assert_json_path_number_value() {
        // Arrange: Valid JSON with number value
        let json = r#"{"count": 42}"#;
        let path = "$.count";
        let expected = "42";

        // Act: Assert the number as string
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.actual(), "42");
        }
    }

    #[test]
    fn test_assert_json_path_boolean_value() {
        // Arrange: Valid JSON with boolean value
        let json = r#"{"active": true}"#;
        let path = "$.active";
        let expected = "true";

        // Act: Assert the boolean as string
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.actual(), "true");
        }
    }

    #[test]
    fn test_assert_json_path_null_value() {
        // Arrange: Valid JSON with null value
        let json = r#"{"value": null}"#;
        let path = "$.value";
        let expected = "null";

        // Act: Assert the null value
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.actual(), "null");
        }
    }

    #[test]
    fn test_assert_json_path_deep_nesting() {
        // Arrange: Valid JSON with deep nesting
        let json = r#"{"a": {"b": {"c": {"d": {"e": "deep"}}}}}"#;
        let path = "$.a.b.c.d.e";
        let expected = "deep";

        // Act: Assert the deeply nested path
        let result = assert_json_path(json, path, expected);

        // Assert: Should pass with matching value
        assert!(result.is_ok());
        if let Ok(assertion) = result {
            assert!(assertion.passed());
            assert_eq!(assertion.actual(), "deep");
        }
    }
}
