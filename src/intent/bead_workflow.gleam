//// Bead workflow management and closure verification.
////
//// This module orchestrates the complete workflow for verifying and closing beads.
//// It integrates feedback collection, evidence gathering, and verification hooks
//// to provide a complete closure workflow.
////
//// Architecture: Functional Core / Imperative Shell
//// - Pure functions: empty_evidence, add_feedback, verify_bead_for_close
//// - I/O functions: None (all are pure in this module)

import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/bead_feedback.{type BeadFeedback, type BeadResult, Success}
import intent/bead_types.{type BeadStatus, Closed, InProgress, Open}
import intent/bead_verify.{
  type HookReport, type VerificationHook, can_complete, hooks_for_issue_type,
  run_hooks_of_type, verify_bead, PreComplete,
}

// =============================================================================
// TYPES
// =============================================================================

/// Evidence collected for verifying bead completion.
pub type Evidence {
  Evidence(
    issue_type: String,
    feedback: List(BeadFeedback),
    custom_evidence: List(String),
  )
}

/// Result of attempting to close a bead.
pub type CloseResult {
  /// Bead was successfully closed with verification report
  BeadClosed(bead_id: String, report: HookReport)
  /// Bead could not be closed due to invalid state
  InvalidState(bead_id: String, reason: String)
}

// =============================================================================
// PURE: Evidence Management (Functional Core)
// =============================================================================

/// Create empty evidence container for a bead.
pub fn empty_evidence(issue_type: String) -> Evidence {
  Evidence(issue_type: issue_type, feedback: [], custom_evidence: [])
}

/// Add feedback to evidence collection.
pub fn add_feedback(evidence: Evidence, feedback: BeadFeedback) -> Evidence {
  Evidence(
    ..evidence,
    feedback: list.append(evidence.feedback, [feedback]),
  )
}

/// Add custom evidence items to the evidence collection.
pub fn add_custom_evidence(
  evidence: Evidence,
  items: List(String),
) -> Evidence {
  Evidence(
    ..evidence,
    custom_evidence: list.append(evidence.custom_evidence, items),
  )
}

/// Build a flat list of all evidence items for verification.
pub fn build_evidence_list(evidence: Evidence) -> List(String) {
  let feedback_evidence =
    evidence.feedback
    |> list.map(fn(fb) { fb.reason })

  list.append(feedback_evidence, evidence.custom_evidence)
}

// =============================================================================
// PURE: Bead Closure Verification (Functional Core)
// =============================================================================

/// Verify a bead can be closed and generate closure report.
///
/// This function:
/// 1. Validates the bead is in InProgress status (required for closure)
/// 2. Runs pre-completion verification hooks
/// 3. Returns either a successful closure with report or error reason
pub fn verify_bead_for_close(
  bead_id: String,
  status: BeadStatus,
  evidence: Evidence,
) -> CloseResult {
  // Check status - must be InProgress to close
  case status {
    Closed -> InvalidState(bead_id, "Bead is already closed")
    Open -> {
      InvalidState(
        bead_id,
        "Bead must be InProgress to close (currently Open)",
      )
    }
    InProgress -> {
      // Get verification hooks for this issue type
      let hooks = hooks_for_issue_type(evidence.issue_type)

      // Collect all evidence
      let all_evidence = build_evidence_list(evidence)

      // Run pre-complete hooks
      let report = verify_bead(bead_id, hooks, all_evidence, "")

      // Check if all required hooks passed
      case can_complete(report) {
        True -> BeadClosed(bead_id, report)
        False ->
          InvalidState(
            bead_id,
            "Required verification hooks failed - cannot close",
          )
      }
    }
  }
}

/// Verify bead with explicit feedback list.
pub fn verify_with_feedback(
  bead_id: String,
  status: BeadStatus,
  feedback: List(BeadFeedback),
  issue_type: String,
) -> CloseResult {
  let evidence =
    empty_evidence(issue_type)
    |> fn(e) { list.fold(feedback, e, add_feedback) }

  verify_bead_for_close(bead_id, status, evidence)
}

// =============================================================================
// PURE: Result Formatting (Functional Core)
// =============================================================================

/// Format a close result as a human-readable string.
pub fn format_close_result(result: CloseResult) -> String {
  case result {
    BeadClosed(bead_id, report) ->
      "✓ Bead "
      <> bead_id
      <> " closed successfully\n"
      <> "  All required verifications passed\n"
      <> "  Total checks: "
      <> string.inspect(list.length(report.results))
      <> "\n"
      <> "  Required passed: "
      <> bool_to_string(report.required_passed)

    InvalidState(bead_id, reason) ->
      "✗ Cannot close bead "
      <> bead_id
      <> ": "
      <> reason
  }
}

/// Convert close result to JSON for API consumption.
pub fn close_result_to_json(result: CloseResult) -> Option(Json) {
  case result {
    BeadClosed(bead_id, report) -> {
      let report_json = bead_verify.report_to_json(report)

      Some(
        json.object([
          #("status", json.string("closed")),
          #("bead_id", json.string(bead_id)),
          #("report", report_json),
        ]),
      )
    }

    InvalidState(bead_id, reason) ->
      Some(
        json.object([
          #("status", json.string("error")),
          #("bead_id", json.string(bead_id)),
          #("reason", json.string(reason)),
        ]),
      )
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
