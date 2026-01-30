/// Intent CLI - Human-writes, AI-verifies, AI-implements
/// Contract-driven API testing and AI-powered planning framework
import argv
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import glint
import intent/ai_errors
import intent/analysis_commands
import intent/bead_commands
import intent/export_command as session_export
import intent/interview_commands
import intent/json_output
import intent/kirk_beads_commands
import intent/kirk_commands
import intent/loader
import intent/plan_commands
import intent/ready_commands
import intent/session_commands
import intent/shape_commands
import intent/spec_commands
import intent/types
import intent/utility_commands
import intent/validate_commands
import intent/vision_commands

/// Exit codes
const exit_pass = 0

const exit_invalid_input = 3

// ============================================================================
// Path Constants
// ============================================================================

/// Standard intent directory for all intent-managed files
pub const intent_dir = ".intent"

/// Path to sessions JSONL file
pub const sessions_jsonl = ".intent/sessions.jsonl"

/// Path to history JSONL file
pub const history_jsonl = ".intent/history.jsonl"

/// Path to custom questions CUE file
pub const custom_questions_path = ".intent/custom-questions.cue"

/// Generate session file path
pub fn session_file_path(session_id: String) -> String {
  ".intent/session-" <> session_id <> ".cue"
}

/// Generate spec file path for completed interview
pub fn spec_file_path(session_id: String) -> String {
  ".intent/spec-" <> session_id <> ".cue"
}

/// Generate feedback file path
pub fn feedback_file_path(session_id: String) -> String {
  ".intent/feedback-" <> session_id <> ".cue"
}

/// Generate claims file path
pub fn claims_file_path(session_id: String) -> String {
  ".intent/claims-" <> session_id <> ".cue"
}

/// Generate verification file path
pub fn verification_file_path(session_id: String) -> String {
  ".intent/verification-" <> session_id <> ".cue"
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Load spec with appropriate loader based on JSON mode.
/// Uses quiet loader (no spinner) for JSON mode to avoid ANSI escape codes.
pub fn load_spec_for_mode(
  path: String,
  json_mode: Bool,
) -> Result(types.Spec, loader.LoadError) {
  case json_mode {
    True -> loader.load_spec_quiet(path)
    False -> loader.load_spec(path)
  }
}

// ============================================================================
// Flag Normalization
// ============================================================================

/// Normalize flag syntax to support both --flag=value and --flag value
/// Glint only supports --flag=value, so we pre-process args to convert
/// --flag value into --flag=value before passing to glint
///
/// Also handles short flags like -o by converting them to --output
pub fn normalize_flag_syntax(args: List(String)) -> List(String) {
  do_normalize(args)
}

fn do_normalize(args: List(String)) -> List(String) {
  case args {
    // Empty list
    [] -> []

    // Single argument - could be a bare --help flag or -o=file
    [arg] -> {
      case arg {
        "--help" -> ["--help=true"]
        "-h" -> ["--help=true"]
        "-o" -> ["--output="]
        // Short flag without value
        _ -> {
          // Handle -o=value format
          case string.starts_with(arg, "-o=") {
            True -> ["--output=" <> string.slice(arg, 3, string.length(arg))]
            _ -> [arg]
          }
        }
      }
    }

    // Two or more arguments
    [first, second, ..rest] -> {
      case first {
        "--help" -> ["--help=true", ..do_normalize([second, ..rest])]
        "-h" -> ["--help=true", ..do_normalize([second, ..rest])]
        "-o" -> ["--output=" <> second, ..do_normalize(rest)]
        _ -> {
          // Handle -o=value format
          case string.starts_with(first, "-o=") {
            True -> [
              "--output=" <> string.slice(first, 3, string.length(first)),
              ..do_normalize([second, ..rest])
            ]
            _ -> {
              case string.starts_with(first, "--") {
                True -> {
                  // First is a flag
                  case string.contains(first, "=") {
                    True -> {
                      // Flag already has equals (--flag=value)
                      [first, ..do_normalize([second, ..rest])]
                    }
                    False -> {
                      // Flag doesn't have equals, check if second is a value or flag
                      case
                        string.starts_with(second, "--")
                        || string.starts_with(second, "-")
                      {
                        True -> {
                          // Second is also a flag (first is boolean) - keep both unchanged
                          [first, ..do_normalize([second, ..rest])]
                        }
                        False -> {
                          // Second is a value, merge with first
                          [first <> "=" <> second, ..do_normalize(rest)]
                        }
                      }
                    }
                  }
                }
                False -> {
                  // First is not a flag (positional argument)
                  [first, ..do_normalize([second, ..rest])]
                }
              }
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// Shared Utilities
// ============================================================================

pub fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

@external(erlang, "intent_ffi", "halt")
pub fn halt(code: Int) -> Nil

// ============================================================================
// Main Entry Point
// ============================================================================

pub fn main() {
  let raw_args = argv.load().arguments

  // Handle --help flag before glint processing for clean exit code 0
  case raw_args {
    ["--version"] | ["-V"] -> {
      io.println("intent 0.1.1")
      exit_pass
    }
    ["--help", ..] | ["-h", ..] -> {
      // Show help JSON
      let app = build_app()
      let _ = glint.run(app, ["help"])
      exit_pass
    }
    [] -> {
      // No args: show help
      run_command_with_json_errors(["help"])
    }
    _ -> {
      // Check if --help or -h appears anywhere in args (e.g., "show --help")
      // Redirect to our JSON help command
      case list.any(raw_args, fn(arg) { arg == "--help" || arg == "-h" }) {
        True -> {
          // Extract the command name for context-sensitive help
          let cmd =
            raw_args
            |> list.filter(fn(arg) { !string.starts_with(arg, "-") })
            |> list.first
          case cmd {
            Ok(command) -> {
              // Run help for specific command
              run_command_with_json_errors(["help", command])
            }
            Error(_) -> {
              // Generic help
              run_command_with_json_errors(["help"])
            }
          }
        }
        False -> {
          let normalized_args = normalize_flag_syntax(raw_args)
          run_command_with_json_errors(normalized_args)
        }
      }
    }
  }
}

/// Run a command and handle errors with JSON output
fn run_command_with_json_errors(args: List(String)) -> Int {
  let app = build_app()

  case glint.execute(app, args) {
    Ok(glint.Out(_)) -> exit_pass
    Ok(glint.Help(help_text)) -> {
      // For glint's built-in help, output as JSON
      output_glint_help_as_json(help_text, args)
      exit_pass
    }
    Error(error_msg) -> {
      // Output error as structured JSON
      let cmd = list.first(args) |> option.from_result
      output_command_error(error_msg, cmd)
      // Halt with proper exit code since we're returning from main
      halt(exit_invalid_input)
      exit_invalid_input
    }
  }
}

/// Output glint's help text wrapped in JSON for AI consumption
fn output_glint_help_as_json(help_text: String, args: List(String)) -> Nil {
  let cmd = case args {
    [first, ..] -> first
    [] -> "intent"
  }

  let response =
    json_output.success(
      "help_fallback",
      cmd,
      json.object([
        #("help_text", json.string(help_text)),
        #("note", json.string("Use 'intent help' for structured JSON help")),
      ]),
      None,
      [json_output.next_action("intent help", "Get structured JSON help")],
    )
  json_output.output(response)
}

/// Output command error as structured JSON
fn output_command_error(
  error_msg: String,
  command: option.Option(String),
) -> Nil {
  let cmd = case command {
    Some(c) -> c
    None -> "unknown"
  }

  // Check if the error is about an unknown flag
  let is_flag_error =
    string.contains(error_msg, "flag")
    || string.contains(error_msg, "Flag")
    || string.contains(error_msg, "unexpected")

  // List of available commands for error context
  let available = [
    "validate", "show", "lint", "improve", "diff", "quality", "coverage", "gaps",
    "invert", "effects", "ears", "doctor", "interview", "sessions", "history",
    "export", "beads", "plan", "prompt", "feedback", "help", "vision", "spec",
    "shape", "ready", "ai",
  ]

  let #(error_type, error_message, suggestion) = case is_flag_error {
    True -> #(
      "unknown_flag",
      error_msg,
      "Check available flags with: intent " <> cmd <> " --help",
    )
    False -> {
      let error = ai_errors.command_not_found(cmd, available)
      #(
        ai_errors.error_type_to_string(error.error_type),
        error.message,
        error.suggestion,
      )
    }
  }

  let response =
    json_output.failure(
      "command_error",
      cmd,
      json.null(),
      [
        json_output.detailed_error(
          error_type,
          error_message,
          "",
          suggestion,
          "intent help",
        ),
      ],
      None,
      [json_output.next_action("intent help", "See all available commands")],
      exit_invalid_input,
    )
  json_output.output(response)
}

fn build_app() {
  glint.new()
  |> glint.with_name("intent")
  |> glint.with_pretty_help(glint.default_pretty_help())
  // Validate/Show/Export/Lint/Check
  |> glint.add(at: ["validate"], do: validate_commands.validate_command())
  |> glint.add(at: ["show"], do: validate_commands.show_command())
  |> glint.add(at: ["export"], do: session_export.export_command())
  |> glint.add(at: ["lint"], do: validate_commands.lint_command())
  |> glint.add(at: ["check"], do: validate_commands.check_command())
  // Analysis
  |> glint.add(at: ["analyze"], do: analysis_commands.analyze_command())
  |> glint.add(at: ["improve"], do: analysis_commands.improve_command())
  |> glint.add(at: ["doctor"], do: analysis_commands.doctor_command())
  // Interview
  |> glint.add(at: ["interview"], do: interview_commands.interview_command())
  // Beads
  |> glint.add(at: ["beads"], do: bead_commands.beads_command())
  |> glint.add(at: ["bead-status"], do: bead_commands.bead_status_command())
  |> glint.add(
    at: ["beads-regenerate"],
    do: bead_commands.beads_regenerate_command(),
  )
  |> glint.add(at: ["feedback"], do: bead_commands.feedback_command())
  |> glint.add(at: ["prompt"], do: bead_commands.prompt_command())
  // Session/History
  |> glint.add(at: ["history"], do: session_commands.history_command())
  |> glint.add(at: ["sessions"], do: session_commands.sessions_command())
  // Utility
  |> glint.add(at: ["diff"], do: utility_commands.diff_command())
  |> glint.add(at: ["help"], do: utility_commands.help_command())
  |> glint.add(at: ["ai", "schema"], do: utility_commands.ai_schema_command())
  |> glint.add(
    at: ["ai", "aggregate"],
    do: utility_commands.ai_aggregate_command(),
  )
  // KIRK commands
  |> glint.add(at: ["quality"], do: kirk_commands.kirk_quality_command())
  |> glint.add(at: ["invert"], do: kirk_commands.kirk_invert_command())
  |> glint.add(at: ["coverage"], do: kirk_commands.kirk_coverage_command())
  |> glint.add(at: ["gaps"], do: kirk_commands.kirk_gaps_command())
  |> glint.add(at: ["ears"], do: kirk_commands.kirk_ears_command())
  |> glint.add(at: ["parse"], do: kirk_commands.parse_command())
  |> glint.add(at: ["effects"], do: kirk_commands.kirk_effects_command())
  // KIRK-to-beads commands
  |> glint.add(at: ["kirk-beads"], do: kirk_beads_commands.kirk_beads_command())
  |> glint.add(at: ["bead-show"], do: kirk_beads_commands.bead_show_command())
  |> glint.add(
    at: ["bead-verify"],
    do: kirk_beads_commands.bead_verify_command(),
  )
  // Plan commands
  |> glint.add(at: ["plan"], do: plan_commands.plan_command())
  |> glint.add(at: ["plan-approve"], do: plan_commands.plan_approve_command())
  // Vision phase commands
  |> glint.add(at: ["vision"], do: vision_commands.vision_group_command())
  |> glint.add(
    at: ["vision", "start"],
    do: vision_commands.vision_start_command(),
  )
  |> glint.add(
    at: ["vision", "check"],
    do: vision_commands.vision_check_command(),
  )
  |> glint.add(
    at: ["vision", "critique"],
    do: vision_commands.vision_critique_command(),
  )
  |> glint.add(
    at: ["vision", "respond"],
    do: vision_commands.vision_respond_command(),
  )
  |> glint.add(
    at: ["vision", "agree"],
    do: vision_commands.vision_agree_command(),
  )
  // Spec phase commands
  |> glint.add(at: ["spec"], do: spec_commands.spec_group_command())
  |> glint.add(at: ["spec", "start"], do: spec_commands.spec_start_command())
  |> glint.add(at: ["spec", "check"], do: spec_commands.spec_check_command())
  |> glint.add(
    at: ["spec", "critique"],
    do: spec_commands.spec_critique_command(),
  )
  |> glint.add(
    at: ["spec", "respond"],
    do: spec_commands.spec_respond_command(),
  )
  |> glint.add(at: ["spec", "agree"], do: spec_commands.spec_agree_command())
  // Shape phase commands
  |> glint.add(at: ["shape"], do: shape_commands.shape_group_command())
  |> glint.add(at: ["shape", "start"], do: shape_commands.shape_start_command())
  |> glint.add(at: ["shape", "check"], do: shape_commands.shape_check_command())
  |> glint.add(
    at: ["shape", "critique"],
    do: shape_commands.shape_critique_command(),
  )
  |> glint.add(
    at: ["shape", "respond"],
    do: shape_commands.shape_respond_command(),
  )
  |> glint.add(at: ["shape", "agree"], do: shape_commands.shape_agree_command())
  // Ready phase commands
  |> glint.add(at: ["ready"], do: ready_commands.ready_group_command())
  |> glint.add(at: ["ready", "start"], do: ready_commands.ready_start_command())
  |> glint.add(at: ["ready", "check"], do: ready_commands.ready_check_command())
  |> glint.add(
    at: ["ready", "critique"],
    do: ready_commands.ready_critique_command(),
  )
  |> glint.add(
    at: ["ready", "respond"],
    do: ready_commands.ready_respond_command(),
  )
  |> glint.add(at: ["ready", "agree"], do: ready_commands.ready_agree_command())
}
