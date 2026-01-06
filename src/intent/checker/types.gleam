/// Shared types for the checker module

/// Result of checking a single field
pub type CheckResult {
  CheckPassed(field: String, rule: String)
  CheckFailed(
    field: String,
    rule: String,
    expected: String,
    actual: String,
    explanation: String,
  )
}

/// Result of checking all fields in a response
pub type ResponseCheckResult {
  ResponseCheckResult(
    passed: List(CheckResult),
    failed: List(CheckResult),
    status_ok: Bool,
    status_expected: Int,
    status_actual: Int,
  )
}
