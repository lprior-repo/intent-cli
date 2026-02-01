#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

//! Assertion kind enumeration for test assertions
//!
//! This module defines the types of assertions that can be made during
//! HTTP test execution, following functional programming principles.

/// Types of assertions that can be made during test execution
///
/// Each variant represents a different kind of validation that can be
/// performed on an HTTP response. All variants are pure and contain no
/// mutable state.
///
/// # Examples
///
/// ```
/// use intent_core::runner::AssertionKind;
///
/// let status_check = AssertionKind::StatusEquals;
/// let header_check = AssertionKind::HeaderExists;
/// assert_ne!(status_check, header_check);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssertionKind {
    /// Assert that the HTTP status code equals a specific value
    ///
    /// Example: Assert status equals 200 OK
    StatusEquals,

    /// Assert that a specific header exists in the response
    ///
    /// Example: Assert that "Content-Type" header is present
    HeaderExists,

    /// Assert that a header has a specific value
    ///
    /// Example: Assert that "Content-Type" equals "application/json"
    HeaderEquals,

    /// Assert that the response body contains a specific substring
    ///
    /// Example: Assert body contains "success"
    BodyContains,

    /// Assert a value at a specific JSONPath in the response body
    ///
    /// Example: Assert $.user.id equals 123
    BodyJsonPath,
}

impl AssertionKind {
    /// Returns a human-readable description of the assertion kind
    ///
    /// This is a pure function that always returns the same output for
    /// the same input, with no side effects.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::AssertionKind;
    ///
    /// assert_eq!(
    ///     AssertionKind::StatusEquals.description(),
    ///     "Status code equality check"
    /// );
    /// ```
    #[must_use]
    pub const fn description(&self) -> &'static str {
        match self {
            Self::StatusEquals => "Status code equality check",
            Self::HeaderExists => "Header existence check",
            Self::HeaderEquals => "Header value equality check",
            Self::BodyContains => "Body substring search",
            Self::BodyJsonPath => "JSONPath value extraction and comparison",
        }
    }

    /// Returns whether this assertion kind operates on the response body
    ///
    /// Pure function for determining if body parsing is required.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::AssertionKind;
    ///
    /// assert!(AssertionKind::BodyContains.requires_body());
    /// assert!(!AssertionKind::StatusEquals.requires_body());
    /// ```
    #[must_use]
    pub const fn requires_body(&self) -> bool {
        matches!(self, Self::BodyContains | Self::BodyJsonPath)
    }

    /// Returns whether this assertion kind operates on headers
    ///
    /// Pure function for determining if header parsing is required.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::AssertionKind;
    ///
    /// assert!(AssertionKind::HeaderExists.requires_headers());
    /// assert!(!AssertionKind::StatusEquals.requires_headers());
    /// ```
    #[must_use]
    pub const fn requires_headers(&self) -> bool {
        matches!(self, Self::HeaderExists | Self::HeaderEquals)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// TDD Test: Verify all assertion kind variants can be constructed
    ///
    /// This is the primary test specified in the bead requirements.
    #[test]
    fn test_assertion_kinds() {
        // Test that all required variants exist and can be constructed
        let _status_equals = AssertionKind::StatusEquals;
        let _header_exists = AssertionKind::HeaderExists;
        let _header_equals = AssertionKind::HeaderEquals;
        let _body_contains = AssertionKind::BodyContains;
        let _body_json_path = AssertionKind::BodyJsonPath;

        // If compilation succeeds, all variants are properly defined
    }

    #[test]
    fn test_assertion_kind_equality() {
        // Reflexivity: x == x
        assert_eq!(AssertionKind::StatusEquals, AssertionKind::StatusEquals);
        assert_eq!(AssertionKind::HeaderExists, AssertionKind::HeaderExists);
        assert_eq!(AssertionKind::HeaderEquals, AssertionKind::HeaderEquals);
        assert_eq!(AssertionKind::BodyContains, AssertionKind::BodyContains);
        assert_eq!(AssertionKind::BodyJsonPath, AssertionKind::BodyJsonPath);

        // Different variants are not equal
        assert_ne!(AssertionKind::StatusEquals, AssertionKind::HeaderExists);
        assert_ne!(AssertionKind::HeaderEquals, AssertionKind::BodyContains);
        assert_ne!(AssertionKind::BodyJsonPath, AssertionKind::StatusEquals);
    }

    #[test]
    fn test_assertion_kind_clone() {
        // Test that Clone produces equal values
        let kinds = [
            AssertionKind::StatusEquals,
            AssertionKind::HeaderExists,
            AssertionKind::HeaderEquals,
            AssertionKind::BodyContains,
            AssertionKind::BodyJsonPath,
        ];

        for kind in &kinds {
            let cloned = kind.clone();
            assert_eq!(kind, &cloned);
        }
    }

    #[test]
    fn test_assertion_kind_debug() {
        // Test Debug implementation produces expected output
        let test_cases = [
            (AssertionKind::StatusEquals, "StatusEquals"),
            (AssertionKind::HeaderExists, "HeaderExists"),
            (AssertionKind::HeaderEquals, "HeaderEquals"),
            (AssertionKind::BodyContains, "BodyContains"),
            (AssertionKind::BodyJsonPath, "BodyJsonPath"),
        ];

        for (kind, expected_substr) in &test_cases {
            let debug_str = format!("{kind:?}");
            assert!(
                debug_str.contains(expected_substr),
                "Debug output '{debug_str}' should contain '{expected_substr}'"
            );
        }
    }

    #[test]
    fn test_description_is_pure() {
        // Test that description() returns consistent values
        let kind = AssertionKind::StatusEquals;
        let desc1 = kind.description();
        let desc2 = kind.description();
        assert_eq!(desc1, desc2, "description() must be pure");

        // Test all descriptions are non-empty
        let kinds = [
            AssertionKind::StatusEquals,
            AssertionKind::HeaderExists,
            AssertionKind::HeaderEquals,
            AssertionKind::BodyContains,
            AssertionKind::BodyJsonPath,
        ];

        for kind in &kinds {
            assert!(
                !kind.description().is_empty(),
                "{kind:?} must have a non-empty description"
            );
        }
    }

    #[test]
    fn test_requires_body() {
        // Test body requirement classification
        assert!(
            !AssertionKind::StatusEquals.requires_body(),
            "StatusEquals should not require body"
        );
        assert!(
            !AssertionKind::HeaderExists.requires_body(),
            "HeaderExists should not require body"
        );
        assert!(
            !AssertionKind::HeaderEquals.requires_body(),
            "HeaderEquals should not require body"
        );
        assert!(
            AssertionKind::BodyContains.requires_body(),
            "BodyContains should require body"
        );
        assert!(
            AssertionKind::BodyJsonPath.requires_body(),
            "BodyJsonPath should require body"
        );
    }

    #[test]
    fn test_requires_headers() {
        // Test header requirement classification
        assert!(
            !AssertionKind::StatusEquals.requires_headers(),
            "StatusEquals should not require headers"
        );
        assert!(
            AssertionKind::HeaderExists.requires_headers(),
            "HeaderExists should require headers"
        );
        assert!(
            AssertionKind::HeaderEquals.requires_headers(),
            "HeaderEquals should require headers"
        );
        assert!(
            !AssertionKind::BodyContains.requires_headers(),
            "BodyContains should not require headers"
        );
        assert!(
            !AssertionKind::BodyJsonPath.requires_headers(),
            "BodyJsonPath should not require headers"
        );
    }

    #[test]
    fn test_hash_consistency() {
        use std::collections::HashSet;

        // Test that Hash implementation is consistent with Eq
        let mut set = HashSet::new();
        set.insert(AssertionKind::StatusEquals);
        set.insert(AssertionKind::StatusEquals); // Duplicate

        assert_eq!(set.len(), 1, "Duplicate values should not increase set size");

        // Test all variants are unique
        let mut unique_set = HashSet::new();
        unique_set.insert(AssertionKind::StatusEquals);
        unique_set.insert(AssertionKind::HeaderExists);
        unique_set.insert(AssertionKind::HeaderEquals);
        unique_set.insert(AssertionKind::BodyContains);
        unique_set.insert(AssertionKind::BodyJsonPath);

        assert_eq!(
            unique_set.len(),
            5,
            "All five variants should be unique in a HashSet"
        );
    }

    #[test]
    fn test_exhaustive_match() {
        // Test that pattern matching is exhaustive
        let test_match = |kind: AssertionKind| match kind {
            AssertionKind::StatusEquals => "status",
            AssertionKind::HeaderExists => "header_exists",
            AssertionKind::HeaderEquals => "header_equals",
            AssertionKind::BodyContains => "body_contains",
            AssertionKind::BodyJsonPath => "body_json",
        };

        // If this compiles, all variants are covered
        let _ = test_match(AssertionKind::StatusEquals);
    }
}
