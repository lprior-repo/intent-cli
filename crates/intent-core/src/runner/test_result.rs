#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

//! Test result types for representing test execution outcomes.
//!
//! This module provides the `TestResult` enum which captures the three
//! possible outcomes of a test execution: Pass, Fail, or Skip.
//!
//! # Examples
//!
//! ```
//! use intent_core::runner::TestResult;
//!
//! let pass = TestResult::Pass;
//! let fail = TestResult::Fail {
//!     reason: "Expected 5, got 3".to_string(),
//! };
//! let skip = TestResult::Skip {
//!     reason: "Test disabled in CI".to_string(),
//! };
//! ```

use std::fmt;

/// Represents the outcome of a test execution.
///
/// This enum uses functional patterns to ensure type safety and
/// exhaustive pattern matching for all test outcomes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TestResult {
    /// Test passed successfully.
    Pass,

    /// Test failed with a reason.
    Fail {
        /// Human-readable explanation of why the test failed.
        reason: String,
    },

    /// Test was skipped with a reason.
    Skip {
        /// Human-readable explanation of why the test was skipped.
        reason: String,
    },
}

impl TestResult {
    /// Creates a new passing test result.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let result = TestResult::pass();
    /// assert!(result.is_pass());
    /// ```
    #[must_use]
    pub const fn pass() -> Self {
        Self::Pass
    }

    /// Creates a new failing test result with a reason.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let result = TestResult::fail("assertion failed");
    /// assert!(result.is_fail());
    /// ```
    #[must_use]
    pub fn fail(reason: impl Into<String>) -> Self {
        Self::Fail {
            reason: reason.into(),
        }
    }

    /// Creates a new skipped test result with a reason.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let result = TestResult::skip("disabled");
    /// assert!(result.is_skip());
    /// ```
    #[must_use]
    pub fn skip(reason: impl Into<String>) -> Self {
        Self::Skip {
            reason: reason.into(),
        }
    }

    /// Returns `true` if the test result is `Pass`.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// assert!(TestResult::Pass.is_pass());
    /// assert!(!TestResult::fail("error").is_pass());
    /// ```
    #[must_use]
    pub const fn is_pass(&self) -> bool {
        matches!(self, Self::Pass)
    }

    /// Returns `true` if the test result is `Fail`.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// assert!(TestResult::fail("error").is_fail());
    /// assert!(!TestResult::Pass.is_fail());
    /// ```
    #[must_use]
    pub const fn is_fail(&self) -> bool {
        matches!(self, Self::Fail { .. })
    }

    /// Returns `true` if the test result is `Skip`.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// assert!(TestResult::skip("disabled").is_skip());
    /// assert!(!TestResult::Pass.is_skip());
    /// ```
    #[must_use]
    pub const fn is_skip(&self) -> bool {
        matches!(self, Self::Skip { .. })
    }

    /// Returns the reason if the test failed, otherwise `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let fail = TestResult::fail("assertion failed");
    /// assert_eq!(fail.fail_reason(), Some("assertion failed"));
    ///
    /// let pass = TestResult::Pass;
    /// assert_eq!(pass.fail_reason(), None);
    /// ```
    #[must_use]
    pub fn fail_reason(&self) -> Option<&str> {
        match self {
            Self::Fail { reason } => Some(reason.as_str()),
            _ => None,
        }
    }

    /// Returns the reason if the test was skipped, otherwise `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let skip = TestResult::skip("disabled in CI");
    /// assert_eq!(skip.skip_reason(), Some("disabled in CI"));
    ///
    /// let pass = TestResult::Pass;
    /// assert_eq!(pass.skip_reason(), None);
    /// ```
    #[must_use]
    pub fn skip_reason(&self) -> Option<&str> {
        match self {
            Self::Skip { reason } => Some(reason.as_str()),
            _ => None,
        }
    }

    /// Maps a `TestResult::Pass` to another value, leaving `Fail` and `Skip` unchanged.
    ///
    /// This is useful for composing test results with other operations.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let result = TestResult::Pass.map_pass(|| TestResult::skip("converted"));
    /// assert!(result.is_skip());
    ///
    /// let fail = TestResult::fail("error").map_pass(|| TestResult::Pass);
    /// assert!(fail.is_fail());
    /// ```
    #[must_use]
    pub fn map_pass<F>(self, f: F) -> Self
    where
        F: FnOnce() -> Self,
    {
        match self {
            Self::Pass => f(),
            other => other,
        }
    }

    /// Combines two test results using logical AND semantics.
    ///
    /// Returns:
    /// - `Pass` only if both results are `Pass`
    /// - `Fail` if either result is `Fail` (prioritizes first failure)
    /// - `Skip` if either result is `Skip` and neither is `Fail`
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let pass1 = TestResult::Pass;
    /// let pass2 = TestResult::Pass;
    /// assert!(pass1.and(pass2).is_pass());
    ///
    /// let fail = TestResult::fail("error");
    /// assert!(pass1.and(fail).is_fail());
    /// ```
    #[must_use]
    pub fn and(self, other: Self) -> Self {
        match (self, other) {
            (Self::Pass, Self::Pass) => Self::Pass,
            (Self::Fail { reason }, _) | (_, Self::Fail { reason }) => Self::Fail { reason },
            (Self::Skip { reason }, _) | (_, Self::Skip { reason }) => Self::Skip { reason },
        }
    }

    /// Combines two test results using logical OR semantics.
    ///
    /// Returns:
    /// - `Pass` if either result is `Pass`
    /// - `Fail` only if both results are `Fail`
    /// - `Skip` if either result is `Skip` and neither is `Pass`
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestResult;
    ///
    /// let pass = TestResult::Pass;
    /// let fail = TestResult::fail("error");
    /// assert!(pass.or(fail).is_pass());
    ///
    /// let fail1 = TestResult::fail("error1");
    /// let fail2 = TestResult::fail("error2");
    /// assert!(fail1.or(fail2).is_fail());
    /// ```
    #[must_use]
    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Self::Pass, _) | (_, Self::Pass) => Self::Pass,
            (Self::Fail { reason }, Self::Fail { .. }) => Self::Fail { reason },
            (Self::Skip { reason }, _) | (_, Self::Skip { reason }) => Self::Skip { reason },
        }
    }
}

impl fmt::Display for TestResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pass => write!(f, "PASS"),
            Self::Fail { reason } => write!(f, "FAIL: {reason}"),
            Self::Skip { reason } => write!(f, "SKIP: {reason}"),
        }
    }
}

// =============================================================================
// TESTS (TDD - Tests First!)
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -------------------------------------------------------------------------
    // Test Result Variants (Primary TDD Test)
    // -------------------------------------------------------------------------

    #[test]
    fn test_result_variants() {
        // Pass variant
        let pass = TestResult::Pass;
        assert!(pass.is_pass());
        assert!(!pass.is_fail());
        assert!(!pass.is_skip());
        assert_eq!(pass.fail_reason(), None);
        assert_eq!(pass.skip_reason(), None);

        // Fail variant
        let fail = TestResult::Fail {
            reason: "assertion failed".to_string(),
        };
        assert!(!fail.is_pass());
        assert!(fail.is_fail());
        assert!(!fail.is_skip());
        assert_eq!(fail.fail_reason(), Some("assertion failed"));
        assert_eq!(fail.skip_reason(), None);

        // Skip variant
        let skip = TestResult::Skip {
            reason: "disabled".to_string(),
        };
        assert!(!skip.is_pass());
        assert!(!skip.is_fail());
        assert!(skip.is_skip());
        assert_eq!(skip.fail_reason(), None);
        assert_eq!(skip.skip_reason(), Some("disabled"));
    }

    // -------------------------------------------------------------------------
    // Constructor Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_pass_constructor() {
        let result = TestResult::pass();
        assert!(result.is_pass());
        assert_eq!(result, TestResult::Pass);
    }

    #[test]
    fn test_fail_constructor() {
        let result = TestResult::fail("test failed");
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some("test failed"));
    }

    #[test]
    fn test_skip_constructor() {
        let result = TestResult::skip("not implemented");
        assert!(result.is_skip());
        assert_eq!(result.skip_reason(), Some("not implemented"));
    }

    #[test]
    fn test_fail_constructor_with_string() {
        let reason = String::from("dynamic error");
        let result = TestResult::fail(reason);
        assert_eq!(result.fail_reason(), Some("dynamic error"));
    }

    #[test]
    fn test_skip_constructor_with_string() {
        let reason = String::from("dynamic skip");
        let result = TestResult::skip(reason);
        assert_eq!(result.skip_reason(), Some("dynamic skip"));
    }

    // -------------------------------------------------------------------------
    // Equality and Clone Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_equality() {
        assert_eq!(TestResult::Pass, TestResult::Pass);
        assert_eq!(
            TestResult::fail("error"),
            TestResult::Fail {
                reason: "error".to_string()
            }
        );
        assert_eq!(
            TestResult::skip("disabled"),
            TestResult::Skip {
                reason: "disabled".to_string()
            }
        );
    }

    #[test]
    fn test_inequality() {
        assert_ne!(TestResult::Pass, TestResult::fail("error"));
        assert_ne!(TestResult::Pass, TestResult::skip("disabled"));
        assert_ne!(TestResult::fail("error1"), TestResult::fail("error2"));
    }

    #[test]
    fn test_clone() {
        let original = TestResult::fail("error");
        let cloned = original.clone();
        assert_eq!(original, cloned);
    }

    // -------------------------------------------------------------------------
    // Display Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_display_pass() {
        let result = TestResult::Pass;
        assert_eq!(format!("{result}"), "PASS");
    }

    #[test]
    fn test_display_fail() {
        let result = TestResult::fail("assertion failed");
        assert_eq!(format!("{result}"), "FAIL: assertion failed");
    }

    #[test]
    fn test_display_skip() {
        let result = TestResult::skip("not implemented");
        assert_eq!(format!("{result}"), "SKIP: not implemented");
    }

    // -------------------------------------------------------------------------
    // Combinator Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_map_pass_on_pass() {
        let result = TestResult::Pass.map_pass(|| TestResult::skip("converted"));
        assert!(result.is_skip());
        assert_eq!(result.skip_reason(), Some("converted"));
    }

    #[test]
    fn test_map_pass_on_fail() {
        let result = TestResult::fail("error").map_pass(|| TestResult::Pass);
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some("error"));
    }

    #[test]
    fn test_map_pass_on_skip() {
        let result = TestResult::skip("disabled").map_pass(|| TestResult::Pass);
        assert!(result.is_skip());
        assert_eq!(result.skip_reason(), Some("disabled"));
    }

    #[test]
    fn test_and_both_pass() {
        let result = TestResult::Pass.and(TestResult::Pass);
        assert!(result.is_pass());
    }

    #[test]
    fn test_and_first_fail() {
        let result = TestResult::fail("error1").and(TestResult::Pass);
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some("error1"));
    }

    #[test]
    fn test_and_second_fail() {
        let result = TestResult::Pass.and(TestResult::fail("error2"));
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some("error2"));
    }

    #[test]
    fn test_and_both_fail() {
        let result = TestResult::fail("error1").and(TestResult::fail("error2"));
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some("error1")); // First failure wins
    }

    #[test]
    fn test_and_with_skip() {
        let result = TestResult::Pass.and(TestResult::skip("disabled"));
        assert!(result.is_skip());
        assert_eq!(result.skip_reason(), Some("disabled"));
    }

    #[test]
    fn test_and_fail_takes_precedence_over_skip() {
        let result = TestResult::fail("error").and(TestResult::skip("disabled"));
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some("error"));
    }

    #[test]
    fn test_or_both_pass() {
        let result = TestResult::Pass.or(TestResult::Pass);
        assert!(result.is_pass());
    }

    #[test]
    fn test_or_first_pass() {
        let result = TestResult::Pass.or(TestResult::fail("error"));
        assert!(result.is_pass());
    }

    #[test]
    fn test_or_second_pass() {
        let result = TestResult::fail("error").or(TestResult::Pass);
        assert!(result.is_pass());
    }

    #[test]
    fn test_or_both_fail() {
        let result = TestResult::fail("error1").or(TestResult::fail("error2"));
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some("error1")); // First failure preserved
    }

    #[test]
    fn test_or_with_skip() {
        let result = TestResult::fail("error").or(TestResult::skip("disabled"));
        assert!(result.is_skip());
        assert_eq!(result.skip_reason(), Some("disabled"));
    }

    #[test]
    fn test_or_pass_takes_precedence_over_skip() {
        let result = TestResult::Pass.or(TestResult::skip("disabled"));
        assert!(result.is_pass());
    }

    // -------------------------------------------------------------------------
    // Edge Case Tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_empty_reason_fail() {
        let result = TestResult::fail("");
        assert!(result.is_fail());
        assert_eq!(result.fail_reason(), Some(""));
    }

    #[test]
    fn test_empty_reason_skip() {
        let result = TestResult::skip("");
        assert!(result.is_skip());
        assert_eq!(result.skip_reason(), Some(""));
    }

    #[test]
    fn test_unicode_in_reason() {
        let result = TestResult::fail("Failed: 测试失败 🚫");
        assert_eq!(result.fail_reason(), Some("Failed: 测试失败 🚫"));
    }

    #[test]
    fn test_multiline_reason() {
        let reason = "Line 1\nLine 2\nLine 3";
        let result = TestResult::fail(reason);
        assert_eq!(result.fail_reason(), Some(reason));
    }
}
