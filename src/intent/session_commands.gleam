/// Session Commands Module
///
/// Commands for managing interview sessions and history snapshots.
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/interview
import intent/interview_commands
import intent/interview_storage
import intent/json_output
import intent/list_limits
import intent/output_mode
import simplifile

/// Exit codes
const exit_pass = 0

const exit_invalid = 3

const exit_error = 4

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

/// The `history` command - view session snapshot history
pub fn history_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let history_path = "history_jsonl"
    let _mode = output_mode.Interactive

    let profile_filter =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("")

    let max_items =
      flag.get_int(input.flags, "max-items")
      |> result.unwrap(list_limits.default_max_items)

    case input.args {
      [session_id, ..] -> {
        case interview_storage.list_session_history(history_path, session_id) {
          Error(err) -> {
            // Handle missing history file gracefully with JSON
            case string.contains(err, "Enoent") {
              True -> {
                let response =
                  json_output.failure(
                    "history_not_found",
                    "history",
                    json.object([#("session_id", json.string(session_id))]),
                    [
                      json_output.error(
                        "not_found",
                        "No history snapshots exist yet",
                      ),
                    ],
                    Some(session_id),
                    [
                      json_output.next_action(
                        "interview --snapshot",
                        "Create a new interview with snapshot history",
                      ),
                      json_output.next_action(
                        "intent sessions",
                        "List available sessions",
                      ),
                    ],
                    exit_pass,
                  )
                json_output.output(response)
                halt(exit_pass)
              }
              False -> {
                let response =
                  json_output.failure(
                    "history_error",
                    "history",
                    json.object([#("error", json.string(err))]),
                    [json_output.error("file_error", err)],
                    Some(session_id),
                    [
                      json_output.next_action(
                        "interview --snapshot",
                        "Create a new interview with snapshot history",
                      ),
                    ],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
            }
          }
          Ok([]) -> {
            let response =
              json_output.success(
                "history_empty",
                "history",
                json.object([
                  #("session_id", json.string(session_id)),
                  #("snapshots", json.array([], fn(_) { json.null() })),
                ]),
                Some(session_id),
                [
                  json_output.next_action(
                    "interview --snapshot",
                    "Create a new interview with snapshot history",
                  ),
                  json_output.next_action(
                    "intent sessions",
                    "List available sessions",
                  ),
                ],
              )
            json_output.output(response)
            halt(exit_pass)
          }
          Ok(all_snapshots) -> {
            // Apply max-items limit (AI guardrail)
            let total_count = list.length(all_snapshots)
            let snapshots = list_limits.apply_limit(all_snapshots, max_items)
            let shown_count = list.length(snapshots)
            let was_limited = total_count > shown_count

            let data =
              json.object([
                #("session_id", json.string(session_id)),
                #(
                  "snapshots",
                  json.array(snapshots, interview_storage.snapshot_to_json),
                ),
                #("total", json.int(total_count)),
                #("shown", json.int(shown_count)),
                #("truncated", json.bool(was_limited)),
              ])
            let next_actions = [
              json_output.next_action(
                "interview --resume=" <> session_id <> " --snapshot",
                "Resume session with snapshot enabled",
              ),
              json_output.next_action("intent sessions", "List all sessions"),
            ]
            let response =
              json_output.success(
                "history_result",
                "history",
                data,
                Some(session_id),
                next_actions,
              )
            json_output.output(response)
            halt(exit_pass)
          }
        }
      }
      [] -> {
        // No session ID - return JSON with all history entries
        case interview_storage.list_all_history(history_path) {
          Error(_) -> {
            // File does not exist yet - return empty JSON
            let data =
              json.object([
                #("snapshots", json.array([], fn(_) { json.null() })),
                #("total", json.int(0)),
                #("shown", json.int(0)),
                #("truncated", json.bool(False)),
              ])
            let response =
              json_output.success("history_result", "history", data, None, [
                json_output.next_action(
                  "intent interview --profile api",
                  "Start a new interview to create history",
                ),
              ])
            json_output.output(response)
            halt(exit_pass)
          }
          Ok(all_snapshots) -> {
            // Filter by profile if specified
            let filtered = case profile_filter {
              "" -> all_snapshots
              p -> {
                // Load sessions to get profile mapping
                case
                  interview_storage.list_sessions_from_jsonl(
                    ".intent/sessions.jsonl",
                  )
                {
                  Error(_) -> all_snapshots
                  Ok(sessions) -> {
                    // Build session_id -> profile mapping
                    let session_profiles =
                      sessions
                      |> list.fold(dict.new(), fn(acc, session) {
                        dict.insert(
                          acc,
                          session.id,
                          interview_commands.profile_to_string(session.profile),
                        )
                      })

                    // Filter snapshots by session profile
                    all_snapshots
                    |> list.filter(fn(snapshot) {
                      case dict.get(session_profiles, snapshot.session_id) {
                        Ok(session_profile) ->
                          session_profile == string.lowercase(p)
                        Error(_) -> False
                      }
                    })
                  }
                }
              }
            }

            let total_count = list.length(filtered)
            let snapshots = list_limits.apply_limit(filtered, max_items)
            let shown_count = list.length(snapshots)
            let was_limited = total_count > shown_count

            let data =
              json.object([
                #(
                  "snapshots",
                  json.array(snapshots, interview_storage.snapshot_to_json),
                ),
                #("total", json.int(total_count)),
                #("shown", json.int(shown_count)),
                #("truncated", json.bool(was_limited)),
              ])
            let next_actions = case list.is_empty(filtered) {
              True -> [
                json_output.next_action(
                  "intent interview --profile api",
                  "Start a new interview to create history",
                ),
              ]
              False -> [
                json_output.next_action(
                  "intent history <session-id>",
                  "View history for a specific session",
                ),
                json_output.next_action(
                  "intent sessions",
                  "List all interview sessions",
                ),
              ]
            }
            let response =
              json_output.success(
                "history_result",
                "history",
                data,
                None,
                next_actions,
              )
            json_output.output(response)
            halt(exit_pass)
          }
        }
      }
    }
  })
  |> glint.description("View snapshot history for an interview session")
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("")
      |> flag.description("Filter by profile (api, cli, event, etc.)"),
  )
  |> glint.flag(
    "max-items",
    flag.int()
      |> flag.default(list_limits.default_max_items)
      |> flag.description(
        "Maximum number of history snapshots to return (default: 100, AI guardrail)",
      ),
  )
}

pub fn sessions_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let jsonl_path = ".intent/sessions.jsonl"

    let delete_result = flag.get_string(input.flags, "delete")

    // Handle delete mode - only if --delete flag was explicitly provided
    case delete_result {
      // Flag was provided but has empty value - error
      Ok("") -> {
        let response =
          json_output.failure(
            "sessions_failed",
            "sessions",
            json.object([
              #("usage", json.string("intent sessions --delete=<session-id>")),
            ]),
            [
              json_output.error(
                "usage_error",
                "Session ID required for --delete flag",
              ),
            ],
            None,
            [
              json_output.next_action(
                "intent sessions",
                "List sessions to find session IDs",
              ),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
      // Flag was provided with a value - process delete
      Ok(session_id) -> {
        case simplifile.read(jsonl_path) {
          Error(_) -> {
            let response =
              json_output.failure(
                "session_not_found",
                "sessions",
                json.object([]),
                [json_output.error("NOT_FOUND", "No sessions file found")],
                None,
                [],
                exit_error,
              )
            json_output.output(response)
            halt(exit_error)
          }
          Ok(content) -> {
            let sessions = interview_storage.parse_sessions_content(content)
            case interview_storage.find_session_by_id(sessions, session_id) {
              Error(_) -> {
                let response =
                  json_output.failure(
                    "session_not_found",
                    "sessions",
                    json.object([]),
                    [
                      json_output.error(
                        "NOT_FOUND",
                        "Session not found: " <> session_id,
                      ),
                    ],
                    None,
                    [],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
              Ok(_) -> {
                let new_content =
                  interview_storage.remove_session_from_content(
                    content,
                    session_id,
                  )
                let _ = simplifile.write(jsonl_path, new_content)
                // Clean up related files
                let _ =
                  simplifile.delete(".intent/spec-" <> session_id <> ".cue")
                let response =
                  json_output.success(
                    "session_deleted",
                    "sessions",
                    json.object([
                      #("deleted_id", json.string(session_id)),
                    ]),
                    None,
                    [
                      json_output.next_action(
                        "intent sessions",
                        "List remaining sessions",
                      ),
                    ],
                  )
                json_output.output(response)
                halt(exit_pass)
              }
            }
          }
        }
      }
      // Flag was not provided - continue to list sessions
      Error(_) -> Nil
    }

    let profile_filter =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("")

    let incomplete_only =
      flag.get_bool(input.flags, "incomplete")
      |> result.unwrap(False)

    let max_items =
      flag.get_int(input.flags, "max-items")
      |> result.unwrap(list_limits.default_max_items)

    case interview_storage.list_sessions_from_jsonl(jsonl_path) {
      Error(err) -> {
        // Check error type and handle appropriately
        case string.contains(err, "Symbolic links are not allowed") {
          True -> {
            // Security error - symlink detected
            let response =
              json_output.failure(
                "sessions_failed",
                "sessions",
                json.object([]),
                [json_output.error("SECURITY_ERROR", err)],
                None,
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
          False ->
            case string.contains(err, "PERMISSION_DENIED") {
              True -> {
                // Permission denied - return error with actionable message
                let response =
                  json_output.failure(
                    "sessions_failed",
                    "sessions",
                    json.object([#("path", json.string(jsonl_path))]),
                    [
                      json_output.error(
                        "PERMISSION_DENIED",
                        "Cannot read sessions file: " <> err,
                      ),
                    ],
                    None,
                    [
                      json_output.next_action(
                        "chmod +r " <> jsonl_path,
                        "Fix file permissions to make it readable",
                      ),
                      json_output.next_action(
                        "ls -la " <> jsonl_path,
                        "Check current file permissions",
                      ),
                    ],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
              False ->
                case string.contains(err, "FILE_NOT_FOUND") {
                  True -> {
                    // File doesn't exist yet - treat as empty
                    let response =
                      json_output.success(
                        "sessions_empty",
                        "sessions",
                        json.object([
                          #("sessions", json.array([], fn(_) { json.null() })),
                          #("total", json.int(0)),
                        ]),
                        None,
                        [
                          json_output.next_action(
                            "interview --profile api",
                            "Start a new interview",
                          ),
                        ],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  False -> {
                    // Other I/O error - return error
                    let response =
                      json_output.failure(
                        "sessions_failed",
                        "sessions",
                        json.object([#("path", json.string(jsonl_path))]),
                        [json_output.error("IO_ERROR", err)],
                        None,
                        [],
                        exit_error,
                      )
                    json_output.output(response)
                    halt(exit_error)
                  }
                }
            }
        }
      }
      Ok([]) -> {
        let response =
          json_output.success(
            "sessions_empty",
            "sessions",
            json.object([
              #("sessions", json.array([], fn(_) { json.null() })),
              #("total", json.int(0)),
            ]),
            None,
            [
              json_output.next_action(
                "interview --profile api",
                "Start a new interview",
              ),
            ],
          )
        json_output.output(response)
        halt(exit_pass)
      }
      Ok(sessions) -> {
        // Filter by profile if specified
        let filtered = case profile_filter {
          "" -> sessions
          p ->
            list.filter(sessions, fn(s) {
              interview_commands.profile_to_string(s.profile)
              == string.lowercase(p)
            })
        }

        // Filter by incomplete if specified
        let filtered = case incomplete_only {
          True ->
            list.filter(filtered, fn(s) {
              case s.stage {
                interview.Complete -> False
                _ -> True
              }
            })
          False -> filtered
        }

        // Apply max-items limit (AI guardrail)
        let total_count = list.length(filtered)
        let limited = list_limits.apply_limit(filtered, max_items)
        let shown_count = list.length(limited)
        let was_limited = total_count > shown_count

        let data =
          json.object([
            #(
              "sessions",
              json.array(limited, interview_storage.session_to_json),
            ),
            #("total", json.int(total_count)),
            #("shown", json.int(shown_count)),
            #("truncated", json.bool(was_limited)),
          ])
        let next_actions = [
          json_output.next_action(
            "interview --resume <id>",
            "Resume an incomplete session",
          ),
          json_output.next_action(
            "intent beads <session_id>",
            "Generate work items from session",
          ),
        ]
        let response =
          json_output.success(
            "sessions_result",
            "sessions",
            data,
            None,
            next_actions,
          )
        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("List all interview sessions")
  |> glint.flag(
    "delete",
    flag.string()
      |> flag.description("Delete a session by ID"),
  )
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("")
      |> flag.description("Filter by profile (api, cli, event, etc.)"),
  )
  |> glint.flag(
    "incomplete",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Show only incomplete sessions"),
  )
  |> glint.flag(
    "max-items",
    flag.int()
      |> flag.default(list_limits.default_max_items)
      |> flag.description(
        "Maximum number of sessions to return (default: 100, AI guardrail)",
      ),
  )
}
