#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

//! Assertion type for representing test assertion results

use super::assertion_kind::AssertionKind;

/// Represents a single assertion result in a test execution
///
/// This type captures the complete state of an assertion, including what was
/// expected, what was actually observed, and whether the assertion passed.
///
/// # Examples
///
/// ```
/// use intent_core::runner::{Assertion, AssertionKind};
///
/// let assertion = Assertion::new(
///     AssertionKind::StatusEquals,
///     "200".to_string(),
///     "200".to_string(),
///     true,
/// );
/// assert!(assertion.passed());
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assertion {
    /// The kind of assertion being made
    kind: AssertionKind,
    /// The expected value
    expected: String,
    /// The actual value observed
    actual: String,
    /// Whether the assertion passed
    passed: bool,
}

impl Assertion {
    /// Creates a new assertion result
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::{Assertion, AssertionKind};
    ///
    /// let assertion = Assertion::new(
    ///     AssertionKind::StatusEquals,
    ///     "200".to_string(),
    ///     "404".to_string(),
    ///     false,
    /// );
    /// assert!(!assertion.passed());
    /// ```
    #[must_use]
    pub const fn new(kind: AssertionKind, expected: String, actual: String, passed: bool) -> Self {
        Self {
            kind,
            expected,
            actual,
            passed,
        }
    }

    /// Returns the kind of this assertion
    #[must_use]
    pub const fn kind(&self) -> &AssertionKind {
        &self.kind
    }

    /// Returns the expected value
    #[must_use]
    pub fn expected(&self) -> &str {
        &self.expected
    }

    /// Returns the actual value observed
    #[must_use]
    pub fn actual(&self) -> &str {
        &self.actual
    }

    /// Returns whether this assertion passed
    #[must_use]
    pub const fn passed(&self) -> bool {
        self.passed
    }

    /// Returns whether this assertion failed
    #[must_use]
    pub const fn failed(&self) -> bool {
        !self.passed
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assertion_types() {
        // Test different assertion kinds
        let status_assertion = Assertion::new(
            AssertionKind::StatusEquals,
            "200".to_string(),
            "200".to_string(),
            true,
        );
        assert_eq!(status_assertion.kind(), &AssertionKind::StatusEquals);
        assert_eq!(status_assertion.expected(), "200");
        assert_eq!(status_assertion.actual(), "200");
        assert!(status_assertion.passed());
        assert!(!status_assertion.failed());

        let header_assertion = Assertion::new(
            AssertionKind::HeaderEquals,
            "application/json".to_string(),
            "text/html".to_string(),
            false,
        );
        assert_eq!(header_assertion.kind(), &AssertionKind::HeaderEquals);
        assert_eq!(header_assertion.expected(), "application/json");
        assert_eq!(header_assertion.actual(), "text/html");
        assert!(!header_assertion.passed());
        assert!(header_assertion.failed());
    }

    #[test]
    fn test_assertion_passed_state() {
        let passed = Assertion::new(
            AssertionKind::StatusEquals,
            "200".to_string(),
            "200".to_string(),
            true,
        );
        assert!(passed.passed());
        assert!(!passed.failed());

        let failed = Assertion::new(
            AssertionKind::StatusEquals,
            "200".to_string(),
            "404".to_string(),
            false,
        );
        assert!(!failed.passed());
        assert!(failed.failed());
    }

    #[test]
    fn test_assertion_accessors() {
        let assertion = Assertion::new(
            AssertionKind::BodyContains,
            "success".to_string(),
            "error".to_string(),
            false,
        );

        // Test all accessor methods
        assert_eq!(assertion.kind(), &AssertionKind::BodyContains);
        assert_eq!(assertion.expected(), "success");
        assert_eq!(assertion.actual(), "error");
        assert!(!assertion.passed());
        assert!(assertion.failed());
    }

    #[test]
    fn test_assertion_equality() {
        let assertion1 = Assertion::new(
            AssertionKind::StatusEquals,
            "200".to_string(),
            "200".to_string(),
            true,
        );
        let assertion2 = Assertion::new(
            AssertionKind::StatusEquals,
            "200".to_string(),
            "200".to_string(),
            true,
        );
        let assertion3 = Assertion::new(
            AssertionKind::StatusEquals,
            "200".to_string(),
            "404".to_string(),
            false,
        );

        assert_eq!(assertion1, assertion2);
        assert_ne!(assertion1, assertion3);
    }

    #[test]
    fn test_assertion_clone() {
        let original = Assertion::new(
            AssertionKind::BodyJsonPath,
            "100ms".to_string(),
            "150ms".to_string(),
            false,
        );
        let cloned = original.clone();
        assert_eq!(original, cloned);
    }

    #[test]
    fn test_assertion_debug() {
        let assertion = Assertion::new(
            AssertionKind::HeaderExists,
            "Content-Type".to_string(),
            "Content-Type".to_string(),
            true,
        );
        let debug_str = format!("{:?}", assertion);
        assert!(debug_str.contains("Assertion"));
        assert!(debug_str.contains("HeaderExists"));
    }

    #[test]
    fn test_all_assertion_kinds() {
        // Test that we can create assertions with all kinds
        let kinds = vec![
            AssertionKind::StatusEquals,
            AssertionKind::HeaderExists,
            AssertionKind::HeaderEquals,
            AssertionKind::BodyContains,
            AssertionKind::BodyJsonPath,
        ];

        for kind in kinds {
            let assertion = Assertion::new(
                kind.clone(),
                "expected".to_string(),
                "actual".to_string(),
                true,
            );
            assert_eq!(assertion.kind(), &kind);
        }
    }
}
