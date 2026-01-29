/// Bead Commands Module
///
/// Handles all bead-related commands for managing work items, feedback, and prompts.
/// Beads are atomic 5-30min work units generated from interview sessions or specs.
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/bead_feedback
import intent/bead_from_failures
import intent/bead_templates
import intent/ffi
import intent/interview_storage
import intent/json_output
import intent/list_limits
import intent/prompt_generator
import simplifile

// ============================================================================
// Exit Codes
// ============================================================================

const exit_pass = 0

const exit_fail = 1

const exit_invalid = 3

const exit_error = 4

// Local constants to avoid circular dependency
const sessions_jsonl = ".intent/sessions.jsonl"

// ============================================================================
// External Function Declarations
// ============================================================================

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

// ============================================================================
// Private Utility Functions
// ============================================================================

/// Escape special characters in strings for CUE output
fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

/// Get current timestamp in ISO 8601 format
fn current_iso8601_timestamp() -> String {
  ffi.current_iso8601_timestamp()
}

/// Generate a UUID v4 string
/// Convert bead feedback error to string message
fn bead_feedback_error_to_string(err: bead_feedback.FeedbackError) -> String {
  case err {
    bead_feedback.SessionNotFound(id) -> "Session not found: " <> id
    bead_feedback.WriteError(path, msg) ->
      "Write error to " <> path <> ": " <> msg
    bead_feedback.ValidationError(msg) -> "Validation error: " <> msg
  }
}

/// Generate regeneration entries for failed/blocked beads
fn generate_regeneration_entries(
  failed_beads: List(bead_feedback.BeadFeedback),
  strategy: String,
) -> String {
  let timestamp = current_iso8601_timestamp()

  let entries =
    failed_beads
    |> list.map(fn(fb) {
      let root_cause = case fb.error {
        Some(err) -> err.message
        None -> fb.reason
      }

      "  {\n"
      <> "    bead_id: \""
      <> fb.bead_id
      <> "\"\n"
      <> "    strategy: \""
      <> strategy
      <> "\"\n"
      <> "    root_cause: \""
      <> escape_cue_string(root_cause)
      <> "\"\n"
      <> "    regenerated_at: \""
      <> timestamp
      <> "\"\n"
      <> "  }"
    })
    |> string.join(",\n")

  entries
}

/// Append regeneration metadata to session file
fn append_regeneration_to_session(
  session_path: String,
  entries: String,
) -> Result(Nil, String) {
  let regen_cue =
    "\n// Regeneration Metadata\nregenerations: [\n" <> entries <> "\n]\n"

  case simplifile.append(session_path, regen_cue) {
    Ok(Nil) -> Ok(Nil)
    Error(err) -> Error("Failed to append: " <> string.inspect(err))
  }
}

// ============================================================================
// Public Command Functions
// ============================================================================

/// Generate work items (beads) from an interview session
pub fn beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let max_items =
      flag.get_int(input.flags, "max-items")
      |> result.unwrap(list_limits.default_max_items)

    case input.args {
      [session_id, ..] -> {
        // Load session from JSONL
        case
          interview_storage.get_session_from_jsonl(sessions_jsonl, session_id)
        {
          Error(_) -> {
            let is_spec_file = string.ends_with(session_id, ".cue")
            let error_msg =
              "Session not found: "
              <> session_id
              <> case is_spec_file {
                True ->
                  "\nNote: The beads command expects a session ID, not a spec file."
                False -> ""
              }
            let response =
              json_output.failure(
                "beads_failed",
                "beads",
                json.null(),
                [json_output.error("session_not_found", error_msg)],
                Some(session_id),
                [
                  json_output.next_action(
                    "intent sessions",
                    "List available session IDs",
                  ),
                  json_output.next_action(
                    "interview",
                    "Start a new interview session",
                  ),
                ],
                case is_spec_file {
                  True -> exit_error
                  False -> exit_invalid
                },
              )
            json_output.output(response)
            halt(case is_spec_file {
              True -> exit_error
              False -> exit_invalid
            })
          }
          Ok(session) -> {
            // Generate beads from session
            let all_beads = bead_templates.generate_beads_from_session(session)
            let total_count = list.length(all_beads)

            // Apply max-items limit for output (AI guardrail)
            let beads = list_limits.apply_limit(all_beads, max_items)
            let bead_count = list.length(beads)
            let _was_limited = total_count > bead_count

            // Export to .beads/issues.jsonl (all beads, not limited)
            let jsonl_output = bead_templates.beads_to_jsonl(all_beads)

            case
              simplifile.append(".beads/issues.jsonl", jsonl_output <> "\n")
            {
              Ok(Nil) -> {
                // Output consistent JSON response with next_actions
                let beads_json =
                  bead_templates.beads_to_action_json(beads, session_id)
                let next_actions = [
                  json_output.next_action(
                    "intent plan " <> session_id,
                    "Create execution plan from beads",
                  ),
                  json_output.next_action(
                    "bd list --status open",
                    "Show open work items",
                  ),
                  json_output.next_action(
                    "intent sessions",
                    "List available sessions",
                  ),
                ]
                let response =
                  json_output.success(
                    "beads_generated",
                    "beads",
                    beads_json,
                    Some(session_id),
                    next_actions,
                  )
                json_output.output(response)
                halt(exit_pass)
              }
              Error(err) -> {
                let response =
                  json_output.failure(
                    "beads_export_failed",
                    "beads",
                    json.object([#("error", json.string(string.inspect(err)))]),
                    [
                      json_output.error(
                        "file_error",
                        "Failed to write beads file",
                      ),
                    ],
                    Some(session_id),
                    [
                      json_output.next_action(
                        "intent beads " <> session_id,
                        "Retry beads generation",
                      ),
                    ],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
            }
          }
        }
      }
      [] -> {
        // No session ID provided - return error with next_actions
        let response =
          json_output.failure(
            "beads_failed",
            "beads",
            json.null(),
            [json_output.error("usage_error", "session ID required")],
            None,
            [
              json_output.next_action(
                "intent beads <session_id>",
                "Generate beads from a session",
              ),
              json_output.next_action(
                "intent sessions",
                "List available session IDs",
              ),
              json_output.next_action(
                "interview",
                "Start a new interview session",
              ),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description("Generate work items (beads) from an interview session")
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output JSON for machine consumption"),
  )
  |> glint.flag(
    "max-items",
    flag.int()
      |> flag.default(list_limits.default_max_items)
      |> flag.description(
        "Maximum number of beads to return (default: 100, AI guardrail)",
      ),
  )
}

/// Mark bead execution status (success/failed/blocked)
pub fn bead_status_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // Check for unexpected arguments (common mistake: passing spec file)
    case input.args {
      [arg, ..] -> {
        let response =
          json_output.failure(
            "bead_status_failed",
            "bead_status",
            json.object([
              #("unexpected_argument", json.string(arg)),
            ]),
            [
              json_output.error(
                "usage_error",
                "bead-status updates individual bead execution status, not specs",
              ),
            ],
            None,
            [
              json_output.next_action(
                "intent beads <session-id> --json=true",
                "Generate beads from session",
              ),
              json_output.next_action(
                "bd list --status=open",
                "View bead statuses",
              ),
              json_output.next_action(
                "intent bead-status --bead-id <id> --status success",
                "Mark bead complete using flags",
              ),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
      [] -> {
        let bead_id =
          flag.get_string(input.flags, "bead-id")
          |> result.unwrap("")

        let status =
          flag.get_string(input.flags, "status")
          |> result.unwrap("")

        let reason =
          flag.get_string(input.flags, "reason")
          |> result.unwrap("")

        let session_id =
          flag.get_string(input.flags, "session")
          |> result.unwrap("")

        case string.is_empty(bead_id) {
          True -> {
            let response =
              json_output.failure(
                "bead_status_failed",
                "bead_status",
                json.object([
                  #("provided_bead_id", json.string(bead_id)),
                  #("provided_status", json.string(status)),
                ]),
                [
                  json_output.error(
                    "missing_bead_id",
                    "Required flag --bead-id not provided",
                  ),
                ],
                None,
                [
                  json_output.next_action(
                    "intent bead-status --bead-id <id> --status success|failed|blocked [--reason 'text'] [--session <id>]",
                    "Mark bead status with required flags",
                  ),
                ],
                exit_error,
              )
            json_output.output(response)
            halt(exit_error)
          }
          False -> {
            case status {
              "success" -> {
                case
                  bead_feedback.mark_bead_executed(
                    session_id,
                    bead_id,
                    bead_feedback.Success,
                    reason,
                    0,
                  )
                {
                  Ok(Nil) -> {
                    let response =
                      json_output.success(
                        "bead_status_updated",
                        "bead_status",
                        json.object([
                          #("bead_id", json.string(bead_id)),
                          #("status", json.string("success")),
                          #(
                            "message",
                            json.string(
                              "Bead " <> bead_id <> " marked as success",
                            ),
                          ),
                        ]),
                        Some(session_id),
                        [
                          json_output.next_action(
                            "bd list --status=open",
                            "View remaining open beads",
                          ),
                          json_output.next_action(
                            "intent beads " <> session_id,
                            "View all beads for session",
                          ),
                        ],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    let response =
                      json_output.failure(
                        "bead_status_failed",
                        "bead_status",
                        json.object([
                          #("bead_id", json.string(bead_id)),
                          #("status", json.string("success")),
                          #(
                            "error",
                            json.string(bead_feedback_error_to_string(err)),
                          ),
                        ]),
                        [
                          json_output.error(
                            "update_failed",
                            "Failed to mark bead as success: "
                              <> bead_feedback_error_to_string(err),
                          ),
                        ],
                        Some(session_id),
                        [
                          json_output.next_action(
                            "bd list --session=" <> session_id,
                            "View bead status for session",
                          ),
                        ],
                        exit_error,
                      )
                    json_output.output(response)
                    halt(exit_error)
                  }
                }
              }
              "failed" -> {
                case
                  bead_feedback.mark_bead_failed(
                    session_id,
                    bead_id,
                    reason,
                    "execution_error",
                    "Bead execution failed",
                    option.None,
                    0,
                  )
                {
                  Ok(Nil) -> {
                    let response =
                      json_output.success(
                        "bead_status_updated",
                        "bead_status",
                        json.object([
                          #("bead_id", json.string(bead_id)),
                          #("status", json.string("failed")),
                          #("reason", json.string(reason)),
                          #(
                            "message",
                            json.string(
                              "Bead " <> bead_id <> " marked as failed",
                            ),
                          ),
                        ]),
                        Some(session_id),
                        [
                          json_output.next_action(
                            "intent feedback --results <check-output.json>",
                            "Generate fix beads from failures",
                          ),
                          json_output.next_action(
                            "bd list --status=failed",
                            "View all failed beads",
                          ),
                        ],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    let response =
                      json_output.failure(
                        "bead_status_failed",
                        "bead_status",
                        json.object([
                          #("bead_id", json.string(bead_id)),
                          #("status", json.string("failed")),
                          #("reason", json.string(reason)),
                          #(
                            "error",
                            json.string(bead_feedback_error_to_string(err)),
                          ),
                        ]),
                        [
                          json_output.error(
                            "update_failed",
                            "Failed to mark bead as failed: "
                              <> bead_feedback_error_to_string(err),
                          ),
                        ],
                        Some(session_id),
                        [
                          json_output.next_action(
                            "bd list --session=" <> session_id,
                            "View bead status for session",
                          ),
                        ],
                        exit_error,
                      )
                    json_output.output(response)
                    halt(exit_error)
                  }
                }
              }
              "blocked" -> {
                case string.is_empty(reason) {
                  True -> {
                    let response =
                      json_output.failure(
                        "bead_status_failed",
                        "bead_status",
                        json.object([
                          #("bead_id", json.string(bead_id)),
                          #("status", json.string("blocked")),
                          #("reason", json.string(reason)),
                        ]),
                        [
                          json_output.error(
                            "missing_reason",
                            "The --status blocked requires --reason",
                          ),
                        ],
                        Some(session_id),
                        [
                          json_output.next_action(
                            "intent bead-status --bead-id "
                              <> bead_id
                              <> " --status blocked --reason 'explain why blocked'",
                            "Provide reason for blocked status",
                          ),
                        ],
                        exit_error,
                      )
                    json_output.output(response)
                    halt(exit_error)
                  }
                  False -> {
                    case
                      bead_feedback.mark_bead_blocked(
                        session_id,
                        bead_id,
                        reason,
                        "user_action",
                        "User blocked this bead",
                        "Manual resume required",
                        0,
                      )
                    {
                      Ok(Nil) -> {
                        let response =
                          json_output.success(
                            "bead_status_updated",
                            "bead_status",
                            json.object([
                              #("bead_id", json.string(bead_id)),
                              #("status", json.string("blocked")),
                              #("reason", json.string(reason)),
                              #(
                                "message",
                                json.string(
                                  "Bead "
                                  <> bead_id
                                  <> " marked as blocked: "
                                  <> reason,
                                ),
                              ),
                            ]),
                            Some(session_id),
                            [
                              json_output.next_action(
                                "bd list --status=blocked",
                                "View all blocked beads",
                              ),
                            ],
                          )
                        json_output.output(response)
                        halt(exit_pass)
                      }
                      Error(err) -> {
                        let response =
                          json_output.failure(
                            "bead_status_failed",
                            "bead_status",
                            json.object([
                              #("bead_id", json.string(bead_id)),
                              #("status", json.string("blocked")),
                              #("reason", json.string(reason)),
                              #(
                                "error",
                                json.string(bead_feedback_error_to_string(err)),
                              ),
                            ]),
                            [
                              json_output.error(
                                "update_failed",
                                "Failed to mark bead as blocked: "
                                  <> bead_feedback_error_to_string(err),
                              ),
                            ],
                            Some(session_id),
                            [
                              json_output.next_action(
                                "bd list --session=" <> session_id,
                                "View bead status for session",
                              ),
                            ],
                            exit_error,
                          )
                        json_output.output(response)
                        halt(exit_error)
                      }
                    }
                  }
                }
              }
              _ -> {
                let response =
                  json_output.failure(
                    "bead_status_failed",
                    "bead_status",
                    json.object([
                      #("bead_id", json.string(bead_id)),
                      #("invalid_status", json.string(status)),
                    ]),
                    [
                      json_output.error(
                        "invalid_status",
                        "Invalid status '"
                          <> status
                          <> "'. Valid statuses: success, failed, blocked",
                      ),
                    ],
                    Some(session_id),
                    [
                      json_output.next_action(
                        "intent bead-status --bead-id "
                          <> bead_id
                          <> " --status success|failed|blocked",
                        "Use a valid status value",
                      ),
                    ],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
            }
          }
        }
      }
    }
  })
  |> glint.description("Mark bead execution status (success/failed/blocked)")
  |> glint.flag(
    "bead-id",
    flag.string() |> flag.default("") |> flag.description("Bead ID (required)"),
  )
  |> glint.flag(
    "status",
    flag.string()
      |> flag.default("")
      |> flag.description("Status: success, failed, or blocked (required)"),
  )
  |> glint.flag(
    "reason",
    flag.string()
      |> flag.default("")
      |> flag.description("Reason for status (required for blocked)"),
  )
  |> glint.flag(
    "session",
    flag.string() |> flag.default("") |> flag.description("Session ID"),
  )
}

/// Regenerate failed/blocked beads with adjusted approach
pub fn beads_regenerate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let strategy =
      flag.get_string(input.flags, "strategy")
      |> result.unwrap("hybrid")

    case input.args {
      [session_id, ..] -> {
        let session_path = ".intent/session-" <> session_id <> ".cue"

        // Check session exists
        case simplifile.verify_is_file(session_path) {
          Error(_) -> {
            let response =
              json_output.failure(
                "beads_regenerate_failed",
                "beads_regenerate",
                json.object([
                  #("session_id", json.string(session_id)),
                  #("expected_path", json.string(session_path)),
                ]),
                [
                  json_output.error(
                    "session_not_found",
                    "Session not found: " <> session_id,
                  ),
                ],
                Some(session_id),
                [
                  json_output.next_action(
                    "intent sessions",
                    "List available session IDs",
                  ),
                ],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
          Ok(_) -> {
            // Load feedback
            case bead_feedback.load_feedback_for_session(session_id) {
              Error(err) -> {
                let response =
                  json_output.failure(
                    "beads_regenerate_failed",
                    "beads_regenerate",
                    json.object([
                      #("session_id", json.string(session_id)),
                      #(
                        "error",
                        json.string(bead_feedback_error_to_string(err)),
                      ),
                    ]),
                    [
                      json_output.error(
                        "feedback_load_error",
                        "Failed to load feedback: "
                          <> bead_feedback_error_to_string(err),
                      ),
                    ],
                    Some(session_id),
                    [
                      json_output.next_action(
                        "intent bead-status --bead-id <id> --status failed",
                        "Mark a bead as failed first",
                      ),
                    ],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
              Ok(feedback) -> {
                // Filter failed/blocked beads
                let needs_regen =
                  feedback
                  |> list.filter(fn(fb) {
                    case fb.result {
                      bead_feedback.Failed -> True
                      bead_feedback.Blocked -> True
                      _ -> False
                    }
                  })

                case list.is_empty(needs_regen) {
                  True -> {
                    let response =
                      json_output.success(
                        "beads_regenerate_skipped",
                        "beads_regenerate",
                        json.object([
                          #("session_id", json.string(session_id)),
                          #("strategy", json.string(strategy)),
                          #(
                            "message",
                            json.string(
                              "No beads need regeneration - all passed or skipped",
                            ),
                          ),
                          #("feedback_count", json.int(list.length(feedback))),
                        ]),
                        Some(session_id),
                        [
                          json_output.next_action(
                            "intent beads " <> session_id,
                            "Generate new beads from session",
                          ),
                          json_output.next_action(
                            "intent plan " <> session_id,
                            "Create execution plan from session",
                          ),
                        ],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  False -> {
                    // Generate regeneration entries
                    let regen_entries =
                      generate_regeneration_entries(needs_regen, strategy)

                    // Append regeneration metadata to session
                    case
                      append_regeneration_to_session(
                        session_path,
                        regen_entries,
                      )
                    {
                      Ok(Nil) -> {
                        // Build beads data for output
                        let beads_json =
                          json.array(needs_regen, fn(fb) {
                            json.object([
                              #("bead_id", json.string(fb.bead_id)),
                              #(
                                "status",
                                json.string(case fb.result {
                                  bead_feedback.Failed -> "failed"
                                  bead_feedback.Blocked -> "blocked"
                                  _ -> "unknown"
                                }),
                              ),
                              #("reason", json.string(fb.reason)),
                            ])
                          })

                        let response =
                          json_output.success(
                            "beads_regenerated",
                            "beads_regenerate",
                            json.object([
                              #("session_id", json.string(session_id)),
                              #("strategy", json.string(strategy)),
                              #(
                                "regenerated_count",
                                json.int(list.length(needs_regen)),
                              ),
                              #("session_path", json.string(session_path)),
                              #("beads", beads_json),
                            ]),
                            Some(session_id),
                            [
                              json_output.next_action(
                                "intent plan " <> session_id,
                                "See updated plan with regenerated beads",
                              ),
                              json_output.next_action(
                                "bd list --session-id " <> session_id,
                                "Show work items for this session",
                              ),
                            ],
                          )
                        json_output.output(response)
                        halt(exit_pass)
                      }
                      Error(err) -> {
                        let response =
                          json_output.failure(
                            "beads_regenerate_failed",
                            "beads_regenerate",
                            json.object([
                              #("session_id", json.string(session_id)),
                              #("error", json.string(err)),
                            ]),
                            [
                              json_output.error(
                                "session_update_error",
                                "Failed to update session: " <> err,
                              ),
                            ],
                            Some(session_id),
                            [
                              json_output.next_action(
                                "intent beads-regenerate " <> session_id,
                                "Retry bead regeneration",
                              ),
                            ],
                            exit_error,
                          )
                        json_output.output(response)
                        halt(exit_error)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      [] -> {
        let response =
          json_output.failure(
            "beads_regenerate_failed",
            "beads_regenerate",
            json.object([
              #(
                "usage",
                json.string(
                  "intent beads-regenerate <session_id> [--strategy hybrid|inversion|premortem]",
                ),
              ),
              #(
                "description",
                json.string(
                  "Regenerate failed/blocked beads with adjusted approach",
                ),
              ),
              #(
                "strategies",
                json.object([
                  #("hybrid", json.string("Use all analysis methods (default)")),
                  #("inversion", json.string("Focus on failure mode analysis")),
                  #("premortem", json.string("Focus on what could go wrong")),
                ]),
              ),
            ]),
            [
              json_output.error(
                "usage_error",
                "Session ID is required. Usage: intent beads-regenerate <session_id> [--strategy hybrid|inversion|premortem]",
              ),
            ],
            None,
            [
              json_output.next_action(
                "intent sessions",
                "List available session IDs",
              ),
            ],
            exit_invalid,
          )
        json_output.output(response)
        halt(exit_invalid)
      }
    }
  })
  |> glint.description("Regenerate failed/blocked beads with adjusted approach")
  |> glint.flag(
    "strategy",
    flag.string()
      |> flag.default("hybrid")
      |> flag.description(
        "Regeneration strategy: hybrid, inversion, or premortem",
      ),
  )
}

/// Generate fix beads from check command failures
pub fn feedback_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let results_path =
      flag.get_string(input.flags, "results")
      |> result.map_error(fn(_) { "Missing required --results flag" })

    case results_path {
      Error(_err) -> {
        // Return JSON usage info with exit code 0 for testability
        let error =
          json_output.error(
            "missing_arguments",
            "The --results flag is required for feedback command",
          )

        let response =
          json_output.failure(
            "feedback_usage",
            "feedback",
            json.object([
              #(
                "usage",
                json.string(
                  "intent feedback --results <check-output.json> [--json]",
                ),
              ),
              #(
                "description",
                json.string("Generate fix beads from check command failures"),
              ),
            ]),
            [error],
            None,
            [
              json_output.next_action(
                "intent check <spec> --target=<url> --json > results.json",
                "Generate check results first",
              ),
            ],
            exit_pass,
          )

        json_output.output(response)
        halt(exit_pass)
      }
      Ok(path) -> {
        // Read the check results JSON file
        case simplifile.read(path) {
          Error(_) -> {
            io.println_error("Error: Cannot read file: " <> path)
            io.println_error("")
            io.println_error("Make sure the file exists and is readable.")
            io.println_error(
              "Run 'intent check <spec> --target=<url> --json > results.json' first.",
            )
            halt(exit_invalid)
          }
          Ok(json_content) -> {
            // Generate beads from failures
            case
              bead_from_failures.generate_beads_from_check_results(json_content)
            {
              Error(err) -> {
                io.println_error("Error parsing check results: " <> err)
                io.println_error("")
                io.println_error(
                  "Ensure the file contains valid JSON output from 'intent check --json'",
                )
                halt(exit_invalid)
              }
              Ok(beads) -> {
                case list.is_empty(beads) {
                  True -> {
                    {
                      let data =
                        json.object([
                          #("beads", json.array([], fn(_) { json.null() })),
                          #("count", json.int(0)),
                          #(
                            "message",
                            json.string("No failures - all behaviors passed"),
                          ),
                        ])
                      let response =
                        json_output.success(
                          "feedback_result",
                          "feedback",
                          data,
                          option.None,
                          [],
                        )
                      json_output.output(response)
                    }
                    halt(exit_pass)
                  }
                  False -> {
                    let bead_count = list.length(beads)
                    {
                      let beads_json =
                        json.array(beads, fn(bead) {
                          json.object([
                            #("title", json.string(bead.title)),
                            #("description", json.string(bead.description)),
                            #("priority", json.int(bead.priority)),
                            #("issue_type", json.string(bead.issue_type)),
                            #("labels", json.array(bead.labels, json.string)),
                            #("ai_hints", json.string(bead.ai_hints)),
                            #(
                              "acceptance_criteria",
                              json.array(bead.acceptance_criteria, json.string),
                            ),
                          ])
                        })

                      let data =
                        json.object([
                          #("beads", beads_json),
                          #("count", json.int(bead_count)),
                          #(
                            "message",
                            json.string(
                              "Generated "
                              <> string.inspect(bead_count)
                              <> " fix beads",
                            ),
                          ),
                        ])
                      let response =
                        json_output.success(
                          "feedback_result",
                          "feedback",
                          data,
                          option.None,
                          [],
                        )
                      json_output.output(response)
                    }
                    halt(exit_pass)
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  |> glint.description("Generate fix beads from check command failures")
  |> glint.flag(
    "results",
    flag.string()
      |> flag.description("Path to JSON file from 'intent check --json' output"),
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output JSON for machine consumption"),
  )
}

/// Generate AI implementation prompts from session beads
pub fn prompt_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let max_items =
      flag.get_int(input.flags, "max-items")
      |> result.unwrap(list_limits.default_max_items)

    case input.args {
      [session_id, ..] -> {
        // Load session from JSONL
        case
          interview_storage.get_session_from_jsonl(sessions_jsonl, session_id)
        {
          Error(err) -> {
            io.println_error("Error: " <> err)
            io.println_error("")
            io.println_error(
              "Hint: Run 'intent sessions' to see available session IDs.",
            )
            halt(exit_error)
          }
          Ok(session) -> {
            // Generate beads from session
            let all_beads = bead_templates.generate_beads_from_session(session)
            let total_count = list.length(all_beads)

            case list.is_empty(all_beads) {
              True -> {
                io.println_error("")
                io.println_error("No beads found in session: " <> session_id)
                io.println_error("")
                io.println_error(
                  "This session may not have enough information to generate work items.",
                )
                halt(exit_fail)
              }
              False -> {
                // Apply max-items limit for output
                let beads = list_limits.apply_limit(all_beads, max_items)
                let bead_count = list.length(beads)

                // Generate implementation prompts for each bead
                let prompts =
                  list.map(beads, fn(bead) {
                    prompt_generator.generate_gleam_prompt(bead, "intent-cli")
                  })

                {
                  // JSON output for AI consumption
                  let prompts_json =
                    json.array(prompts, prompt_generator.prompt_to_json)

                  let data =
                    json.object([
                      #("prompts", prompts_json),
                      #("count", json.int(bead_count)),
                      #("total_beads", json.int(total_count)),
                      #("session_id", json.string(session_id)),
                    ])

                  let response =
                    json_output.success(
                      "prompt_result",
                      "prompt",
                      data,
                      option.None,
                      [],
                    )
                  json_output.output(response)
                }
                halt(exit_pass)
              }
            }
          }
        }
      }
      [] -> {
        io.println_error(
          "Usage: intent prompt <session-id> [--json] [--max-items N]",
        )
        io.println_error("")
        io.println_error("Example: intent prompt interview-abc123def456")
        io.println_error("")
        io.println_error("Run 'intent sessions' to see available session IDs.")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Generate AI implementation prompts from session beads")
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output JSON for machine consumption"),
  )
  |> glint.flag(
    "max-items",
    flag.int()
      |> flag.default(list_limits.default_max_items)
      |> flag.description(
        "Maximum number of prompts to generate (default: 100, AI guardrail)",
      ),
  )
}
