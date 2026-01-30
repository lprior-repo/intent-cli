/// Export Command Module
/// Exports interview sessions to CUE spec files
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import glint
import glint/flag
import intent/interview_storage
import intent/json_output
import intent/spec_builder
import simplifile

// Exit codes
const exit_pass = 0

const exit_invalid = 3

const exit_error = 4

// Path constants (duplicated to avoid circular dependency)
const sessions_jsonl = ".intent/sessions.jsonl"

/// Generate spec file path for completed interview
fn spec_file_path(session_id: String) -> String {
  ".intent/spec-" <> session_id <> ".cue"
}

// Local halt FFI
@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

/// The `export` command - export interview session to CUE spec
pub fn export_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_file =
      flag.get_string(input.flags, "output")
      |> result.unwrap("")

    case input.args {
      [session_id, ..] -> {
        // Load session from JSONL
        case
          interview_storage.get_session_from_jsonl(sessions_jsonl, session_id)
        {
          Ok(session) -> {
            // Convert session to CUE spec
            let cue_content = spec_builder.build_spec_from_session(session)

            // Determine output file path
            let output_path = case output_file {
              "" -> spec_file_path(session_id)
              path -> path
            }

            // Write CUE file
            case simplifile.write(output_path, cue_content) {
              Ok(_) -> {
                let next_actions = [
                  json_output.next_action(
                    "intent validate " <> output_path,
                    "Validate generated spec",
                  ),
                  json_output.next_action(
                    "intent quality " <> output_path,
                    "Analyze spec quality",
                  ),
                  json_output.next_action(
                    "intent lint " <> output_path,
                    "Check for anti-patterns",
                  ),
                ]
                let response =
                  json_output.success(
                    "export_result",
                    "export",
                    json.object([
                      #("session_id", json.string(session_id)),
                      #("output_file", json.string(output_path)),
                      #(
                        "message",
                        json.string(
                          "Interview session exported to CUE spec successfully",
                        ),
                      ),
                    ]),
                    Some(output_path),
                    next_actions,
                  )
                json_output.output(response)
                halt(exit_pass)
              }
              Error(err) -> {
                let error_msg = "Failed to write file: " <> case err {
                  simplifile.Enoent -> "File or directory not found"
                  simplifile.Eacces -> "Permission denied"
                  simplifile.Enospc -> "No space left on device"
                  simplifile.Eio -> "I/O error"
                  _ -> "Unknown error"
                }
                let response =
                  json_output.failure(
                    "export_failed",
                    "export",
                    json.null(),
                    [json_output.error("write_error", error_msg)],
                    Some(output_path),
                    [],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
            }
          }
          Error(err) -> {
            let response =
              json_output.failure(
                "export_failed",
                "export",
                json.null(),
                [json_output.error("session_not_found", err)],
                None,
                [
                  json_output.next_action(
                    "intent sessions",
                    "List all available sessions",
                  ),
                  json_output.next_action(
                    "intent interview --profile=api",
                    "Start new interview",
                  ),
                ],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        let response =
          json_output.failure(
            "export_failed",
            "export",
            json.null(),
            [
              json_output.error(
                "usage_error",
                "session ID required. Usage: intent export <session-id> [--output=<file.cue>]",
              ),
            ],
            None,
            [
              json_output.next_action(
                "intent sessions",
                "List all available sessions",
              ),
              json_output.next_action(
                "intent export <session-id> --output=spec.cue",
                "Export session to specific file",
              ),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description("Export interview session to CUE spec file")
  |> glint.flag(
    "output",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Output file path (default: .intent/spec-<session-id>.cue)",
      ),
  )
}
