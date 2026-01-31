#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

//! Test runner for executing HTTP test specifications
//!
//! Provides the `TestRunner` struct which executes HTTP test specifications
//! (CueSpec) and returns test results using functional patterns.

use crate::{
    cue::CueSpec,
    error::IntentResult,
    http_client::HttpClient,
};

use super::test_result::TestResult;

// =============================================================================
// TestRunner - Main Test Execution Engine
// =============================================================================

/// Test runner for executing HTTP test specifications
///
/// The `TestRunner` holds an HTTP client and optional configuration,
/// providing methods to execute test specifications and return results.
///
/// # Philosophy
///
/// - **Functional core, imperative shell**: Pure test logic, I/O at edges
/// - **Zero panics**: All errors returned as `Result`
/// - **Immutable state**: Runner is reusable without mutation
/// - **Railway-oriented**: Chain operations using combinators
///
/// # Examples
///
/// ```
/// use intent_core::{
///     http_client::HttpClient,
///     runner::TestRunner,
/// };
///
/// let client = HttpClient::new();
/// let runner = TestRunner::new(client);
/// ```
#[derive(Debug, Clone)]
pub struct TestRunner {
    /// HTTP client for making requests
    http_client: HttpClient,
}

impl TestRunner {
    /// Creates a new test runner with the given HTTP client
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{
    ///     http_client::HttpClient,
    ///     runner::TestRunner,
    /// };
    ///
    /// let client = HttpClient::new();
    /// let runner = TestRunner::new(client);
    /// ```
    #[must_use]
    pub const fn new(http_client: HttpClient) -> Self {
        Self { http_client }
    }

    /// Returns a reference to the HTTP client
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{
    ///     http_client::HttpClient,
    ///     runner::TestRunner,
    /// };
    ///
    /// let client = HttpClient::new();
    /// let runner = TestRunner::new(client.clone());
    /// assert_eq!(format!("{:?}", runner.http_client()), format!("{:?}", &client));
    /// ```
    #[must_use]
    pub const fn http_client(&self) -> &HttpClient {
        &self.http_client
    }

    /// Executes a test specification and returns the result
    ///
    /// This is a placeholder implementation that will be completed in a future bead.
    /// Currently returns `TestResult::Skip` for all tests.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::{
    ///     cue::CueSpec,
    ///     http_client::HttpClient,
    ///     runner::TestRunner,
    /// };
    ///
    /// let client = HttpClient::new();
    /// let runner = TestRunner::new(client);
    ///
    /// // Placeholder - will be implemented in future bead
    /// // let spec = CueSpec { ... };
    /// // let result = runner.run_test(spec).await?;
    /// ```
    ///
    /// # Errors
    ///
    /// Will return errors for:
    /// - Network failures
    /// - Invalid test specifications
    /// - Assertion failures
    ///
    /// # Note
    ///
    /// Full implementation will be added in bead `intent-cli-68w3`
    pub async fn run_test(&self, _spec: &CueSpec) -> IntentResult<TestResult> {
        // Placeholder - will be implemented in intent-cli-68w3
        Ok(TestResult::skip("Not yet implemented - see bead intent-cli-68w3"))
    }
}

impl Default for TestRunner {
    /// Creates a test runner with a default HTTP client
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::runner::TestRunner;
    ///
    /// let runner = TestRunner::default();
    /// ```
    fn default() -> Self {
        Self::new(HttpClient::new())
    }
}

// =============================================================================
// TESTS (TDD - Tests First!)
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -------------------------------------------------------------------------
    // Test Runner Creation (Primary TDD Test)
    // -------------------------------------------------------------------------

    #[test]
    fn test_runner_creation() {
        // Test that we can create a TestRunner with an HTTP client
        let client = HttpClient::new();
        let runner = TestRunner::new(client.clone());

        // Verify the runner holds the client
        assert_eq!(
            format!("{:?}", runner.http_client()),
            format!("{:?}", &client)
        );
    }

    #[test]
    fn test_runner_default_creation() {
        // Test that we can create a default TestRunner
        let runner = TestRunner::default();

        // Should have a valid HTTP client
        let _ = runner.http_client();
    }

    #[test]
    fn test_runner_clone() {
        // Test that TestRunner is cloneable
        let client = HttpClient::new();
        let runner = TestRunner::new(client);
        let cloned = runner.clone();

        // Both should have equivalent clients
        assert_eq!(
            format!("{:?}", runner.http_client()),
            format!("{:?}", cloned.http_client())
        );
    }

    #[test]
    fn test_runner_debug() {
        // Test Debug implementation
        let runner = TestRunner::default();
        let debug_str = format!("{:?}", runner);
        assert!(debug_str.contains("TestRunner"));
        assert!(debug_str.contains("http_client"));
    }

    #[test]
    fn test_runner_http_client_accessor() {
        // Test that we can access the HTTP client
        let client = HttpClient::new();
        let runner = TestRunner::new(client.clone());

        let retrieved_client = runner.http_client();
        assert_eq!(
            format!("{:?}", retrieved_client),
            format!("{:?}", &client)
        );
    }

    // -------------------------------------------------------------------------
    // Run Test Placeholder Tests
    // -------------------------------------------------------------------------

    #[tokio::test]
    async fn test_run_test_placeholder() {
        // Test the placeholder implementation
        let runner = TestRunner::default();

        // Create a minimal CueSpec for testing
        let spec = CueSpec {
            id: "test-1".to_string(),
            title: "Test".to_string(),
            bead_type: "task".to_string(),
            priority: 2,
            effort_estimate: None,
            labels: vec![],
            extra: serde_json::Value::Object(serde_json::Map::new()),
        };

        // Should return Skip for now
        let result = runner.run_test(&spec).await;
        assert!(result.is_ok());

        let test_result = result.unwrap_or_else(|_| TestResult::fail("unexpected error"));
        assert!(test_result.is_skip());
    }

    #[test]
    fn test_runner_is_send_sync() {
        // Ensure TestRunner can be used across threads
        fn assert_send<T: Send>() {}
        fn assert_sync<T: Sync>() {}

        assert_send::<TestRunner>();
        assert_sync::<TestRunner>();
    }
}
