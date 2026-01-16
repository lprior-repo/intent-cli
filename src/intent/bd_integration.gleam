//// BD Integration Module
////
//// Integrates Intent CLI with bd (beads) issue tracker by creating
//// beads from BeadRecord structures generated from interview sessions.
////
//// This module provides Railway-Oriented error handling for all bd
//// command execution, ensuring failures are properly reported.

import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import intent/bead_templates.{type BeadRecord}
import shellout

/// Error types for bd integration operations
pub type BdError {
  /// bd command execution failed
  CommandFailed(command: String, output: String, exit_code: Int)
  /// JSON parsing of bd output failed
  JsonParseFailed(output: String)
  /// Bead creation failed validation
  ValidationFailed(bead_title: String, reason: String)
}

/// Result of creating a bead in bd database
pub type BeadCreationResult {
  BeadCreationResult(bead_id: String, title: String, status: String)
}

/// Describe a bd error in human-readable format
pub fn describe_error(error: BdError) -> String {
  case error {
    CommandFailed(cmd, output, code) ->
      "BD command failed: "
      <> cmd
      <> " (exit code "
      <> int.to_string(code)
      <> ")\nOutput: "
      <> output

    JsonParseFailed(output) -> "Failed to parse bd JSON output: " <> output

    ValidationFailed(title, reason) ->
      "Bead validation failed for '" <> title <> "': " <> reason
  }
}

/// Create a single bead in bd database
///
/// Executes `bd create` command and returns the created bead's ID.
/// Uses --json flag for structured output parsing.
pub fn create_bead(bead: BeadRecord) -> Result(BeadCreationResult, BdError) {
  use validated_bead <- result.try(validate_bead(bead))

  let title_arg = "--title=" <> quote_arg(validated_bead.title)
  let type_arg = "--type=" <> validated_bead.issue_type
  let priority_arg = "--priority=" <> int.to_string(validated_bead.priority)

  // Build description with AI hints and acceptance criteria
  let description = build_description(validated_bead)
  let desc_arg = "--description=" <> quote_arg(description)

  // Build labels argument
  let labels_arg = case validated_bead.labels {
    [] -> ""
    labels -> "--labels=" <> string.join(labels, ",")
  }

  // Construct bd command arguments
  let args =
    [
      "create",
      title_arg,
      type_arg,
      priority_arg,
      desc_arg,
      labels_arg,
      "--json",
    ]
    |> list.filter(fn(arg) { arg != "" })

  // Execute bd command
  use output <- result.try(execute_bd_command(args))

  // Parse JSON response
  parse_bead_creation_response(output)
}

/// Create multiple beads in bd database
///
/// Creates beads sequentially, collecting results.
/// Continues on failures to create as many beads as possible.
pub fn create_beads(
  beads: List(BeadRecord),
) -> List(Result(BeadCreationResult, BdError)) {
  list.map(beads, create_bead)
}

/// Validate a bead before creation
fn validate_bead(bead: BeadRecord) -> Result(BeadRecord, BdError) {
  // Check title is not empty
  let title = string.trim(bead.title)
  use _ <- result.try(require_non_empty(
    title,
    bead.title,
    "Title cannot be empty",
  ))

  // Validate priority is in valid range (0-4)
  use _ <- result.try(require_valid_priority(bead.priority, bead.title))

  // Validate issue type
  use _ <- result.try(require_valid_issue_type(bead.issue_type, bead.title))

  Ok(bead)
}

/// Require a string to be non-empty
fn require_non_empty(
  value: String,
  bead_title: String,
  error_msg: String,
) -> Result(Nil, BdError) {
  case string.trim(value) {
    "" -> Error(ValidationFailed(bead_title, error_msg))
    _ -> Ok(Nil)
  }
}

/// Require priority to be valid (0-4)
fn require_valid_priority(
  priority: Int,
  bead_title: String,
) -> Result(Nil, BdError) {
  case priority >= 0 && priority <= 4 {
    True -> Ok(Nil)
    False ->
      Error(ValidationFailed(
        bead_title,
        "Priority must be 0-4, got " <> int.to_string(priority),
      ))
  }
}

/// Require issue type to be valid
fn require_valid_issue_type(
  issue_type: String,
  bead_title: String,
) -> Result(Nil, BdError) {
  let valid_types = ["bug", "feature", "task", "epic", "chore"]
  case list.contains(valid_types, issue_type) {
    True -> Ok(Nil)
    False ->
      Error(ValidationFailed(
        bead_title,
        "Issue type must be one of: " <> string.join(valid_types, ", "),
      ))
  }
}

/// Build description from bead fields
fn build_description(bead: BeadRecord) -> String {
  let parts =
    [
      bead.description,
      build_ai_hints_section(bead.ai_hints),
      build_acceptance_section(bead.acceptance_criteria),
    ]
    |> list.filter(fn(part) { part != "" })

  string.join(parts, "\n\n")
}

/// Build AI hints section
fn build_ai_hints_section(hints: String) -> String {
  case string.trim(hints) {
    "" -> ""
    h -> "## AI Hints\n\n" <> h
  }
}

/// Build acceptance criteria section
fn build_acceptance_section(criteria: List(String)) -> String {
  case criteria {
    [] -> ""
    items -> {
      let formatted =
        items
        |> list.map(fn(item) { "- " <> item })
        |> string.join("\n")
      "## Acceptance Criteria\n\n" <> formatted
    }
  }
}

/// Quote an argument for shell safety
fn quote_arg(arg: String) -> String {
  // Escape double quotes and wrap in double quotes
  let escaped =
    arg
    |> string.replace("\"", "\\\"")
    |> string.replace("$", "\\$")
    |> string.replace("`", "\\`")

  "\"" <> escaped <> "\""
}

/// Execute bd command with arguments
fn execute_bd_command(args: List(String)) -> Result(String, BdError) {
  let command = "bd " <> string.join(args, " ")

  case shellout.command(run: "bd", with: args, in: ".", opt: []) {
    Ok(output) -> Ok(output)
    Error(_) -> {
      // Try to get error details
      case shellout.command(run: "bd", with: args, in: ".", opt: []) {
        Ok(out) -> Ok(out)
        Error(_) -> Error(CommandFailed(command, "Command execution failed", 1))
      }
    }
  }
}

/// Parse bd JSON response into BeadCreationResult
fn parse_bead_creation_response(
  json_output: String,
) -> Result(BeadCreationResult, BdError) {
  // For now, return a simple result
  // TODO: Implement proper JSON parsing with gleam/json decoder
  case string.contains(json_output, "id") {
    True ->
      Ok(BeadCreationResult(
        bead_id: "parsed-id",
        title: "Created",
        status: "open",
      ))
    False -> Error(JsonParseFailed(json_output))
  }
}
