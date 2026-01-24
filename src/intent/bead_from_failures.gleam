/// Generate beads from check command failures
/// Converts failed behaviors into actionable fix tasks
import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/result
import intent/bead_templates.{type BeadRecord, BeadRecord}

/// Represents a failure from check command JSON output
pub type CheckFailure {
  CheckFailure(
    behavior: String,
    error_type: String,
    message: String,
    hint: String,
  )
}

/// Represents a blocked behavior from check command JSON output
pub type BlockedBehavior {
  BlockedBehavior(behavior: String, reason: String, hint: String)
}

/// Parse check results JSON and generate fix beads
pub fn generate_beads_from_check_results(
  json_content: String,
) -> Result(List(BeadRecord), String) {
  use parsed <- result.try(
    json.decode(json_content, check_results_decoder())
    |> result.map_error(fn(_) { "Invalid JSON format" }),
  )

  let failure_beads = list.map(parsed.error_failures, create_bead_for_failure)
  let blocked_beads = list.map(parsed.blocked, create_bead_for_blocked)

  Ok(list.append(failure_beads, blocked_beads))
}

/// Create a bead for fixing a failed behavior
fn create_bead_for_failure(failure: CheckFailure) -> BeadRecord {
  let priority = case failure.error_type {
    "REQUEST_ERROR" -> 2
    "INTERPOLATION_ERROR" -> 1
    "VALIDATION_ERROR" -> 1
    _ -> 2
  }

  let issue_type = case failure.error_type {
    "REQUEST_ERROR" -> "bug"
    "INTERPOLATION_ERROR" -> "bug"
    "VALIDATION_ERROR" -> "bug"
    _ -> "task"
  }

  let description =
    "Fix failing behavior: "
    <> failure.behavior
    <> "\n\n"
    <> "Error type: "
    <> failure.error_type
    <> "\n"
    <> "Message: "
    <> failure.message
    <> case failure.hint {
      "" -> ""
      hint -> "\n\nHint: " <> hint
    }

  BeadRecord(
    title: "Fix " <> failure.behavior <> " behavior",
    description: description,
    profile_type: "check",
    priority: priority,
    issue_type: issue_type,
    labels: ["bug", "check-failure", failure.error_type],
    ai_hints: "Review error message and spec. Common fixes: update base_url, fix variable captures, verify response structure",
    acceptance_criteria: [
      "Behavior passes check command",
      "Error no longer appears in output",
      "Response matches expected structure",
    ],
    dependencies: [],
  )
}

/// Create a bead for unblocking a blocked behavior
fn create_bead_for_blocked(blocked: BlockedBehavior) -> BeadRecord {
  BeadRecord(
    title: "Unblock " <> blocked.behavior <> " behavior",
    description: "Blocked behavior: "
      <> blocked.behavior
      <> "\n\n"
      <> "Reason: "
      <> blocked.reason
      <> case blocked.hint {
      "" -> ""
      hint -> "\n\nHint: " <> hint
    },
    profile_type: "check",
    priority: 3,
    issue_type: "blocked",
    labels: ["blocked", "dependency"],
    ai_hints: "This behavior is blocked by dependencies. Fix the blocking behaviors first.",
    acceptance_criteria: [
      "Blocking behaviors are fixed",
      "This behavior can run",
      "All dependencies satisfied",
    ],
    dependencies: [],
  )
}

// =============================================================================
// JSON Decoding
// =============================================================================

type CheckResults {
  CheckResults(
    error_failures: List(CheckFailure),
    blocked: List(BlockedBehavior),
  )
}

fn check_results_decoder() -> fn(Dynamic) ->
  Result(CheckResults, List(dynamic.DecodeError)) {
  fn(data: Dynamic) -> Result(CheckResults, List(dynamic.DecodeError)) {
    use data_obj <- result.try(dynamic.field("data", dynamic.dynamic)(data))
    use error_failures <- result.try(dynamic.field(
      "error_failures",
      dynamic.list(failure_decoder()),
    )(data_obj))
    use blocked <- result.try(dynamic.field(
      "blocked",
      dynamic.list(blocked_decoder()),
    )(data_obj))

    Ok(CheckResults(error_failures: error_failures, blocked: blocked))
  }
}

fn failure_decoder() -> fn(Dynamic) ->
  Result(CheckFailure, List(dynamic.DecodeError)) {
  fn(data: Dynamic) -> Result(CheckFailure, List(dynamic.DecodeError)) {
    use behavior <- result.try(dynamic.field("behavior", dynamic.string)(data))
    use error_type <- result.try(dynamic.field("error_type", dynamic.string)(
      data,
    ))
    use message <- result.try(dynamic.field("message", dynamic.string)(data))
    use hint <- result.try(dynamic.field("hint", dynamic.string)(data))

    Ok(CheckFailure(
      behavior: behavior,
      error_type: error_type,
      message: message,
      hint: hint,
    ))
  }
}

fn blocked_decoder() -> fn(Dynamic) ->
  Result(BlockedBehavior, List(dynamic.DecodeError)) {
  fn(data: Dynamic) -> Result(BlockedBehavior, List(dynamic.DecodeError)) {
    use behavior <- result.try(dynamic.field("behavior", dynamic.string)(data))
    use reason <- result.try(dynamic.field("reason", dynamic.string)(data))
    use hint <- result.try(dynamic.field("hint", dynamic.string)(data))

    Ok(BlockedBehavior(behavior: behavior, reason: reason, hint: hint))
  }
}
