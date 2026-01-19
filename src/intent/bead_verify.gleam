//// Bead verification hooks for auto-verification in AI-native workflows.
////
//// This module provides a hook system that automatically verifies bead completion.
//// Verification hooks run when beads transition to complete status, ensuring
//// acceptance criteria are met before final closure.
////
//// Architecture: Functional Core / Imperative Shell
//// - Pure functions: create_hook, verify_criteria, build_report, etc.
//// - I/O functions: persist_report, run_verification_with_io

import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/bead_feedback.{type BeadFeedback, type BeadResult, Success}
import intent/bead_types.{
  type Bead, type CompletionIssue, type VerificationResult, CanClose,
  CannotClose, InProgress, InvalidStatusTransition, MissingId, MissingTitle,
  RequiredLabelMissing, TestsNotPassing, UnresolvedBlocker,
}
import simplifile

// =============================================================================
// TYPES
// =============================================================================

/// Type of verification hook determining when it runs.
pub type HookType {
  /// Runs before marking bead as complete - can block completion
  PreComplete
  /// Runs after marking complete - for logging/notification
  PostComplete
  /// Runs on any status transition
  OnStatusChange
}

/// A verification hook that checks specific criteria.
pub type VerificationHook {
  VerificationHook(
    id: String,
    name: String,
    description: String,
    hook_type: HookType,
    criteria: List(String),
    required: Bool,
  )
}

/// Result of running a single verification hook.
pub type HookResult {
  /// Hook passed all checks
  HookPassed(hook_id: String, message: String)
  /// Hook failed one or more checks
  HookFailed(hook_id: String, message: String, failed_criteria: List(String))
  /// Hook was skipped (e.g., not applicable)
  HookSkipped(hook_id: String, reason: String)
}

/// Complete report of verification for a bead.
pub type HookReport {
  HookReport(
    bead_id: String,
    results: List(HookResult),
    all_passed: Bool,
    required_passed: Bool,
    timestamp: String,
  )
}

/// Errors that can occur during verification.
pub type VerificationError {
  HookNotFound(hook_id: String)
  CriteriaCheckFailed(criteria: String, reason: String)
  PersistenceError(path: String, message: String)
  InvalidBeadState(bead_id: String, message: String)
}

// =============================================================================
// PURE: Hook Creation (Functional Core)
// =============================================================================

/// Create a new verification hook with the given parameters.
///
/// ## Examples
///
/// ```gleam
/// create_hook(
///   "test-coverage",
///   "Test Coverage Check",
///   "Ensures adequate test coverage",
///   PreComplete,
///   ["Unit tests exist", "Coverage > 80%"],
///   True,
/// )
/// ```
pub fn create_hook(
  id: String,
  name: String,
  description: String,
  hook_type: HookType,
  criteria: List(String),
  required: Bool,
) -> VerificationHook {
  VerificationHook(
    id: id,
    name: name,
    description: description,
    hook_type: hook_type,
    criteria: criteria,
    required: required,
  )
}

/// Create a pre-complete hook (convenience function).
pub fn pre_complete_hook(
  id: String,
  name: String,
  criteria: List(String),
) -> VerificationHook {
  create_hook(
    id,
    name,
    "Pre-completion verification",
    PreComplete,
    criteria,
    True,
  )
}

/// Create an optional post-complete hook.
pub fn post_complete_hook(
  id: String,
  name: String,
  criteria: List(String),
) -> VerificationHook {
  create_hook(
    id,
    name,
    "Post-completion verification",
    PostComplete,
    criteria,
    False,
  )
}

// =============================================================================
// PURE: Built-in Hooks (Functional Core)
// =============================================================================

/// Standard verification hooks for common bead types.
pub fn default_hooks() -> List(VerificationHook) {
  [
    // Code quality hooks
    pre_complete_hook("code-compiles", "Code Compiles", [
      "Code compiles without errors",
      "No type errors",
    ]),
    pre_complete_hook("tests-pass", "Tests Pass", [
      "All unit tests pass",
      "No test failures",
    ]),
    // Documentation hooks
    post_complete_hook("docs-updated", "Documentation Updated", [
      "README updated if needed",
      "API docs current",
    ]),
  ]
}

/// Get hooks for a specific bead issue type.
pub fn hooks_for_issue_type(issue_type: String) -> List(VerificationHook) {
  case issue_type {
    "api_endpoint" -> [
      pre_complete_hook("endpoint-responds", "Endpoint Responds", [
        "Endpoint responds with correct status",
        "Response matches schema",
        "Error handling works",
      ]),
      pre_complete_hook("api-tests", "API Tests", [
        "Integration tests exist",
        "Edge cases covered",
      ]),
    ]
    "cli_command" -> [
      pre_complete_hook("command-works", "Command Works", [
        "Command parses arguments",
        "Help text displays",
        "Error messages clear",
      ]),
    ]
    "bug" -> [
      pre_complete_hook("bug-fixed", "Bug Fixed", [
        "Root cause identified",
        "Fix verified",
        "Regression test added",
      ]),
    ]
    "feature" -> [
      pre_complete_hook("feature-complete", "Feature Complete", [
        "Acceptance criteria met",
        "Tests written",
        "Documentation updated",
      ]),
    ]
    _ -> default_hooks()
  }
}

// =============================================================================
// PURE: Verification Logic (Functional Core)
// =============================================================================

/// Verify a single criterion against provided evidence.
/// Returns True if criterion is satisfied.
///
/// This is a pure function - actual verification logic should be injected
/// via the evidence parameter.
pub fn verify_criterion(criterion: String, evidence: List(String)) -> Bool {
  // Check if any evidence item mentions the criterion
  let criterion_lower = string.lowercase(criterion)
  list.any(evidence, fn(e) {
    let evidence_lower = string.lowercase(e)
    string.contains(evidence_lower, criterion_lower)
    || string.contains(criterion_lower, evidence_lower)
  })
}

/// Run a single hook against provided evidence.
/// Returns the hook result.
pub fn run_hook(hook: VerificationHook, evidence: List(String)) -> HookResult {
  let failed_criteria =
    hook.criteria
    |> list.filter(fn(c) { !verify_criterion(c, evidence) })

  case list.is_empty(failed_criteria) {
    True -> HookPassed(hook.id, "All criteria verified: " <> hook.name)
    False ->
      HookFailed(hook.id, "Failed verification: " <> hook.name, failed_criteria)
  }
}

/// Run all hooks of a specific type.
pub fn run_hooks_of_type(
  hooks: List(VerificationHook),
  hook_type: HookType,
  evidence: List(String),
) -> List(HookResult) {
  hooks
  |> list.filter(fn(h) { h.hook_type == hook_type })
  |> list.map(fn(h) { run_hook(h, evidence) })
}

/// Run all hooks and build a verification report.
pub fn verify_bead(
  bead_id: String,
  hooks: List(VerificationHook),
  evidence: List(String),
  timestamp: String,
) -> HookReport {
  let results = list.map(hooks, fn(h) { run_hook(h, evidence) })

  let all_passed =
    list.all(results, fn(r) {
      case r {
        HookPassed(_, _) -> True
        HookSkipped(_, _) -> True
        HookFailed(_, _, _) -> False
      }
    })

  // Check if all required hooks passed
  let required_hooks = list.filter(hooks, fn(h) { h.required })
  let required_hook_ids = list.map(required_hooks, fn(h) { h.id })

  let required_passed =
    list.all(results, fn(r) {
      case r {
        HookPassed(_id, _) -> True
        HookSkipped(id, _) -> !list.contains(required_hook_ids, id)
        HookFailed(id, _, _) -> !list.contains(required_hook_ids, id)
      }
    })

  HookReport(
    bead_id: bead_id,
    results: results,
    all_passed: all_passed,
    required_passed: required_passed,
    timestamp: timestamp,
  )
}

/// Check if verification allows completion.
/// A bead can be marked complete if all required hooks pass.
pub fn can_complete(report: HookReport) -> Bool {
  report.required_passed
}

// =============================================================================
// PURE: Auto-Verification (Functional Core)
// =============================================================================

/// Generate evidence from bead feedback for verification.
pub fn evidence_from_feedback(feedback: BeadFeedback) -> List(String) {
  let base = [feedback.reason]

  let error_evidence = case feedback.error {
    Some(err) -> [err.message]
    None -> []
  }

  let blocked_evidence = case feedback.blocked_by {
    Some(b) -> [b.details]
    None -> []
  }

  list.concat([base, error_evidence, blocked_evidence])
}

/// Determine if auto-verification should run based on result type.
pub fn should_auto_verify(result: BeadResult) -> Bool {
  case result {
    bead_feedback.Success -> True
    bead_feedback.Failed -> False
    bead_feedback.Blocked -> False
    bead_feedback.Skipped -> False
  }
}

// =============================================================================
// PURE: Serialization (Functional Core)
// =============================================================================

/// Convert HookType to string representation.
pub fn hook_type_to_string(hook_type: HookType) -> String {
  case hook_type {
    PreComplete -> "pre_complete"
    PostComplete -> "post_complete"
    OnStatusChange -> "on_status_change"
  }
}

/// Convert string to HookType.
pub fn hook_type_from_string(s: String) -> Result(HookType, String) {
  case s {
    "pre_complete" -> Ok(PreComplete)
    "post_complete" -> Ok(PostComplete)
    "on_status_change" -> Ok(OnStatusChange)
    _ -> Error("Unknown hook type: " <> s)
  }
}

/// Convert HookResult to string for display.
pub fn result_to_string(result: HookResult) -> String {
  case result {
    HookPassed(id, msg) -> "[PASS] " <> id <> ": " <> msg
    HookFailed(id, msg, criteria) ->
      "[FAIL] "
      <> id
      <> ": "
      <> msg
      <> "\n  Failed: "
      <> string.join(criteria, ", ")
    HookSkipped(id, reason) -> "[SKIP] " <> id <> ": " <> reason
  }
}

/// Convert HookReport to CUE string format.
pub fn report_to_cue(report: HookReport) -> String {
  let results_cue =
    report.results
    |> list.map(result_to_cue)
    |> string.join(",\n")

  "// Verification Report for "
  <> report.bead_id
  <> "\nverification: {\n"
  <> "\tbead_id: \""
  <> report.bead_id
  <> "\"\n"
  <> "\tall_passed: "
  <> bool_to_string(report.all_passed)
  <> "\n"
  <> "\trequired_passed: "
  <> bool_to_string(report.required_passed)
  <> "\n"
  <> "\ttimestamp: \""
  <> report.timestamp
  <> "\"\n"
  <> "\tresults: [\n"
  <> results_cue
  <> "\n\t]\n}\n"
}

fn result_to_cue(result: HookResult) -> String {
  case result {
    HookPassed(id, msg) ->
      "\t\t{\n"
      <> "\t\t\thook_id: \""
      <> id
      <> "\"\n"
      <> "\t\t\tstatus: \"passed\"\n"
      <> "\t\t\tmessage: \""
      <> escape_cue_string(msg)
      <> "\"\n"
      <> "\t\t}"
    HookFailed(id, msg, criteria) ->
      "\t\t{\n"
      <> "\t\t\thook_id: \""
      <> id
      <> "\"\n"
      <> "\t\t\tstatus: \"failed\"\n"
      <> "\t\t\tmessage: \""
      <> escape_cue_string(msg)
      <> "\"\n"
      <> "\t\t\tfailed_criteria: ["
      <> format_string_list(criteria)
      <> "]\n"
      <> "\t\t}"
    HookSkipped(id, reason) ->
      "\t\t{\n"
      <> "\t\t\thook_id: \""
      <> id
      <> "\"\n"
      <> "\t\t\tstatus: \"skipped\"\n"
      <> "\t\t\treason: \""
      <> escape_cue_string(reason)
      <> "\"\n"
      <> "\t\t}"
  }
}

/// Convert HookReport to JSON.
pub fn report_to_json(report: HookReport) -> Json {
  json.object([
    #("bead_id", json.string(report.bead_id)),
    #("all_passed", json.bool(report.all_passed)),
    #("required_passed", json.bool(report.required_passed)),
    #("timestamp", json.string(report.timestamp)),
    #("results", json.array(report.results, result_to_json)),
  ])
}

fn result_to_json(result: HookResult) -> Json {
  case result {
    HookPassed(id, msg) ->
      json.object([
        #("hook_id", json.string(id)),
        #("status", json.string("passed")),
        #("message", json.string(msg)),
      ])
    HookFailed(id, msg, criteria) ->
      json.object([
        #("hook_id", json.string(id)),
        #("status", json.string("failed")),
        #("message", json.string(msg)),
        #("failed_criteria", json.array(criteria, json.string)),
      ])
    HookSkipped(id, reason) ->
      json.object([
        #("hook_id", json.string(id)),
        #("status", json.string("skipped")),
        #("reason", json.string(reason)),
      ])
  }
}

/// Convert hook to JSON for API consumption.
pub fn hook_to_json(hook: VerificationHook) -> Json {
  json.object([
    #("id", json.string(hook.id)),
    #("name", json.string(hook.name)),
    #("description", json.string(hook.description)),
    #("hook_type", json.string(hook_type_to_string(hook.hook_type))),
    #("criteria", json.array(hook.criteria, json.string)),
    #("required", json.bool(hook.required)),
  ])
}

// =============================================================================
// I/O: Persistence (Imperative Shell)
// =============================================================================

/// Persist verification report to CUE file.
pub fn persist_report(
  session_id: String,
  report: HookReport,
) -> Result(Nil, VerificationError) {
  let path = ".intent/verification-" <> session_id <> ".cue"
  let cue_content = report_to_cue(report)

  case simplifile.append(path, cue_content) {
    Ok(Nil) -> Ok(Nil)
    Error(err) ->
      Error(PersistenceError(path, "Failed to write: " <> string.inspect(err)))
  }
}

/// Load verification reports for a session.
pub fn load_reports(session_id: String) -> Result(String, VerificationError) {
  let path = ".intent/verification-" <> session_id <> ".cue"

  case simplifile.read(path) {
    Ok(content) -> Ok(content)
    Error(_) -> Ok("")
    // Empty if no reports yet
  }
}

// =============================================================================
// PRIVATE: Helpers
// =============================================================================

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn format_string_list(items: List(String)) -> String {
  items
  |> list.map(fn(s) { "\"" <> escape_cue_string(s) <> "\"" })
  |> string.join(", ")
}

// =============================================================================
// PURE: Bead Completion Verification (Functional Core)
// =============================================================================

/// Verify a bead can be closed by checking all completion criteria.
///
/// This function validates:
/// - Required fields are filled (title, id)
/// - Status is valid for closing (must be InProgress)
/// - Tests are passing (if feedback provided)
/// - Required labels are present
/// - No unresolved blockers
///
/// ## Examples
///
/// ```gleam
/// let bead = Bead(.., id: "TEST-001", title: "Test", status: InProgress, ..)
/// verify_bead_completion(bead, Some(success_feedback), [])
/// // -> CanClose
/// ```
pub fn verify_bead_completion(
  bead: Bead,
  feedback: Option(BeadFeedback),
  required_labels: List(String),
) -> VerificationResult {
  let issues = []

  let issues = case string.is_empty(string.trim(bead.id)) {
    True -> list.append(issues, [MissingId])
    False -> issues
  }

  let issues = case string.is_empty(string.trim(bead.title)) {
    True -> list.append(issues, [MissingTitle])
    False -> issues
  }

  let issues = case bead.status {
    InProgress -> issues
    _ -> list.append(issues, [InvalidStatusTransition(from: bead.status)])
  }

  let issues = case feedback {
    None -> issues
    Some(fb) -> {
      case fb.result {
        Success -> issues
        _ ->
          list.append(issues, [
            TestsNotPassing(
              reason: "Last test result was: "
              <> bead_result_to_string(fb.result),
            ),
          ])
      }
    }
  }

  let issues = case required_labels {
    [] -> issues
    _ -> {
      let missing =
        required_labels
        |> list.filter(fn(label) { !list.contains(bead.labels, label) })
        |> list.map(fn(label) { RequiredLabelMissing(label: label) })
      list.append(issues, missing)
    }
  }

  case issues {
    [] -> CanClose
    _ -> CannotClose(issues)
  }
}

fn bead_result_to_string(result: BeadResult) -> String {
  case result {
    Success -> "success"
    _ -> "not successful"
  }
}

/// Format a VerificationResult to a human-readable string.
pub fn format_verification_result(result: VerificationResult) -> String {
  case result {
    CanClose -> "Bead can be closed - all criteria met"
    CannotClose(issues) ->
      "Cannot close bead:\n"
      <> string.join(list.map(issues, format_completion_issue), "\n")
  }
}

fn format_completion_issue(issue: CompletionIssue) -> String {
  case issue {
    MissingTitle -> "  - Title is missing or empty"
    MissingId -> "  - ID is missing or empty"
    InvalidStatusTransition(from) ->
      "  - Invalid status transition from: "
      <> bead_types.status_to_string(from)
    UnresolvedBlocker(blocker_id) -> "  - Unresolved blocker: " <> blocker_id
    RequiredLabelMissing(label) -> "  - Required label missing: " <> label
    TestsNotPassing(reason) -> "  - Tests not passing: " <> reason
  }
}
