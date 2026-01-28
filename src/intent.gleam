/// Intent CLI - Human-writes, AI-verifies, AI-implements
/// Contract-driven API testing tool
import argv
import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/ai_errors
import intent/ai_schema
import intent/bead_feedback
import intent/bead_from_failures
import intent/bead_templates
import intent/diff
import intent/doctor
import intent/ffi
import intent/file_watcher
import intent/improver
import intent/interview
import intent/interview_questions
import intent/interview_storage
import intent/json_output
import intent/kirk/coverage_analyzer
import intent/kirk/ears_parser
import intent/kirk/effects_analyzer
import intent/kirk/gap_detector
import intent/kirk/inversion_checker
import intent/list_limits
import intent/loader
import intent/output_mode
import intent/parser
import intent/plan_mode
import intent/prompt_generator
import intent/quality_analyzer
import intent/question_types.{type Question}
import intent/ready_commands
import intent/smart_start
import intent/spec_aggregator
import intent/spec_builder
import intent/spec_commands
import intent/spec_linter
import intent/types
import intent/vision_commands
import intent/watch_output
import simplifile

/// Exit codes
const exit_pass = 0

const exit_fail = 1

const exit_invalid = 3

const exit_error = 4

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
fn load_spec_for_mode(
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

pub fn main() {
  let raw_args = argv.load().arguments

  // Handle --help flag before glint processing for clean exit code 0
  case raw_args {
    ["--help", ..] | ["-h", ..] -> {
      // Show help and exit cleanly
      let app = build_app()
      let _ = glint.run(app, ["--help=true"])
      exit_pass
    }
    [] -> {
      // No args: smart start - detect and resume sessions or start new interview
      let sessions_path = ".intent/sessions.jsonl"
      let action =
        smart_start.determine_start_action(
          sessions_path,
          interview_storage.simplifile_reader(),
        )

      case action {
        smart_start.Resume(session_id) -> {
          // Auto-resume the session by running interview command with --resume flag
          let app = build_app()
          let _ = glint.run(app, ["interview", "--resume=" <> session_id])
          exit_pass
        }
        smart_start.StartNew(profile) -> {
          // Start new interview with default profile
          let profile_str = interview.profile_to_string(profile)
          let app = build_app()
          let _ = glint.run(app, ["interview", "--profile=" <> profile_str])
          exit_pass
        }
      }
    }
    _ -> {
      // Check if --help or -h appears anywhere in args (e.g., "show --help")
      // This ensures all commands show help cleanly with exit code 0
      case list.any(raw_args, fn(arg) { arg == "--help" || arg == "-h" }) {
        True -> {
          let normalized_args = normalize_flag_syntax(raw_args)
          let app = build_app()
          let _ = glint.run(app, normalized_args)
          exit_pass
        }
        False -> {
          let normalized_args = normalize_flag_syntax(raw_args)
          let app = build_app()
          let _ = glint.run(app, normalized_args)
          exit_pass
        }
      }
    }
  }
}

fn build_app() {
  glint.new()
  |> glint.with_name("intent")
  |> glint.with_pretty_help(glint.default_pretty_help())
  |> glint.add(at: ["validate"], do: validate_command())
  |> glint.add(at: ["show"], do: show_command())
  |> glint.add(at: ["export"], do: export_command())
  |> glint.add(at: ["lint"], do: lint_command())
  |> glint.add(at: ["check"], do: check_command())
  |> glint.add(at: ["analyze"], do: analyze_command())
  |> glint.add(at: ["improve"], do: improve_command())
  |> glint.add(at: ["doctor"], do: doctor_command())
  |> glint.add(at: ["interview"], do: interview_command())
  |> glint.add(at: ["beads"], do: beads_command())
  |> glint.add(at: ["bead-status"], do: bead_status_command())
  |> glint.add(at: ["history"], do: history_command())
  |> glint.add(at: ["diff"], do: diff_command())
  |> glint.add(at: ["sessions"], do: sessions_command())
  |> glint.add(at: ["help"], do: help_command())
  // KIRK commands
  |> glint.add(at: ["quality"], do: kirk_quality_command())
  |> glint.add(at: ["invert"], do: kirk_invert_command())
  |> glint.add(at: ["coverage"], do: kirk_coverage_command())
  |> glint.add(at: ["gaps"], do: kirk_gaps_command())
  // DISABLED: compact_format module not available
  // |> glint.add(at: ["compact"], do: kirk_compact_command())
  // |> glint.add(at: ["prototext"], do: kirk_prototext_command())
  |> glint.add(at: ["ears"], do: kirk_ears_command())
  |> glint.add(at: ["parse"], do: parse_command())
  |> glint.add(at: ["effects"], do: kirk_effects_command())
  // Plan commands
  |> glint.add(at: ["plan"], do: plan_command())
  |> glint.add(at: ["plan-approve"], do: plan_approve_command())
  |> glint.add(at: ["beads-regenerate"], do: beads_regenerate_command())
  |> glint.add(at: ["feedback"], do: feedback_command())
  |> glint.add(at: ["prompt"], do: prompt_command())
  // AI commands
  |> glint.add(at: ["ai", "schema"], do: ai_schema_command())
  |> glint.add(at: ["ai", "aggregate"], do: ai_aggregate_command())
  // Vision phase commands
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
  |> glint.add(at: ["shape", "start"], do: shape_start_command())
  |> glint.add(at: ["shape", "check"], do: shape_check_command())
  |> glint.add(at: ["shape", "critique"], do: shape_critique_command())
  |> glint.add(at: ["shape", "respond"], do: shape_respond_command())
  |> glint.add(at: ["shape", "agree"], do: shape_agree_command())
  // Ready phase commands
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

/// The `validate` command - validate CUE spec syntax AND structure
fn validate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let watch_mode = flag.get_bool(input.flags, "watch") |> result.unwrap(False)

    case input.args {
      [spec_path, ..] -> {
        case watch_mode {
          True -> {
            // Watch mode: continuous validation
            file_watcher.watch(spec_path, 1000, fn() {
              watch_output.display_result(
                spec_path,
                loader.load_spec_quiet(spec_path),
              )
            })
          }
          False -> {
            // Normal mode: single validation
            case loader.load_spec_quiet(spec_path) {
              Ok(_) -> {
                let next_actions = [
                  json_output.next_action(
                    "intent lint " <> spec_path,
                    "Check for quality issues",
                  ),
                  json_output.next_action(
                    "intent check " <> spec_path <> " --target=URL",
                    "Test against API",
                  ),
                ]
                let response =
                  json_output.success(
                    "validate_result",
                    "validate",
                    json.object([#("valid", json.bool(True))]),
                    Some(spec_path),
                    next_actions,
                  )
                json_output.output(response)
                halt(exit_pass)
              }
              Error(e) -> {
                let error_msg = loader.format_error(e)
                let response =
                  json_output.failure(
                    "validate_failed",
                    "validate",
                    json.null(),
                    [json_output.error("validation_error", error_msg)],
                    Some(spec_path),
                    [],
                    exit_invalid,
                  )
                json_output.output(response)
                halt(exit_invalid)
              }
            }
          }
        }
      }
      [] -> {
        let response =
          json_output.failure(
            "validate_failed",
            "validate",
            json.null(),
            [json_output.error("usage_error", "spec file path required")],
            None,
            [],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description("Validate a CUE spec file (syntax and structure)")
  |> glint.flag(
    "watch",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Watch file for changes and re-validate automatically",
      ),
  )
}

/// The `show` command - pretty print a parsed spec
fn show_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.export_spec_json(spec_path, loader.default_cue_exporter) {
          Ok(json_str) -> {
            // Parse the spec JSON string to embed as data
            case json.decode(json_str, dynamic.dynamic) {
              Ok(spec_json) -> {
                let next_actions = [
                  json_output.next_action(
                    "intent check " <> spec_path <> " --target=URL",
                    "Test spec against API",
                  ),
                  json_output.next_action(
                    "intent quality " <> spec_path <> " --json",
                    "Analyze spec quality",
                  ),
                ]
                let response =
                  json_output.success(
                    "show_result",
                    "show",
                    parser.dynamic_to_json(spec_json),
                    Some(spec_path),
                    next_actions,
                  )
                json_output.output(response)
                halt(exit_pass)
              }
              Error(_) -> {
                // Fallback: shouldn't happen since export_spec_json produces valid JSON
                io.println_error("Error: failed to parse exported spec JSON")
                halt(exit_error)
              }
            }
          }
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "show_failed",
                "show",
                json.null(),
                [json_output.error("load_error", error_msg)],
                Some(spec_path),
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        {
          let response =
            json_output.failure(
              "show_failed",
              "show",
              json.null(),
              [json_output.error("usage_error", "spec file path required")],
              None,
              [],
              exit_error,
            )
          json_output.output(response)
        }
        halt(exit_error)
      }
    }
  })
  |> glint.description("Pretty print a parsed spec")
}

/// The `export` command - export spec to JSON
fn export_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.export_spec_json(spec_path, loader.default_cue_exporter) {
          Ok(json_str) -> {
            // Wrap in proper JSON response with next_actions
            case json.decode(json_str, dynamic.dynamic) {
              Ok(spec_json) -> {
                let next_actions = [
                  json_output.next_action(
                    "intent check " <> spec_path <> " --target=URL",
                    "Test spec against API",
                  ),
                  json_output.next_action(
                    "intent quality " <> spec_path,
                    "Analyze spec quality",
                  ),
                  json_output.next_action(
                    "intent validate " <> spec_path,
                    "Validate spec structure",
                  ),
                ]
                let response =
                  json_output.success(
                    "export_result",
                    "export",
                    parser.dynamic_to_json(spec_json),
                    Some(spec_path),
                    next_actions,
                  )
                json_output.output(response)
                halt(exit_pass)
              }
              Error(_) -> {
                // Fallback: raw JSON output if parse fails
                let response =
                  json_output.success(
                    "export_result",
                    "export",
                    json.object([#("raw_json", json.string(json_str))]),
                    Some(spec_path),
                    [
                      json_output.next_action(
                        "intent check " <> spec_path <> " --target=URL",
                        "Test spec against API",
                      ),
                    ],
                  )
                json_output.output(response)
                halt(exit_pass)
              }
            }
          }
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "export_failed",
                "export",
                json.null(),
                [json_output.error("load_error", error_msg)],
                Some(spec_path),
                [
                  json_output.next_action(
                    "intent interview",
                    "Create a new spec",
                  ),
                  json_output.next_action(
                    "intent validate " <> spec_path,
                    "Validate spec path",
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
            [json_output.error("usage_error", "spec file path required")],
            None,
            [
              json_output.next_action(
                "intent export <spec.cue>",
                "Export spec to JSON",
              ),
              json_output.next_action("intent interview", "Create a new spec"),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description("Export spec to JSON format")
}

/// The `lint` command - check for specification anti-patterns
fn lint_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let lint_result = spec_linter.lint_spec(spec)
            case lint_result {
              spec_linter.LintValid -> {
                {
                  let next_actions = [
                    json_output.next_action(
                      "intent check " <> spec_path <> " --target=URL",
                      "Test against API",
                    ),
                    json_output.next_action(
                      "intent quality " <> spec_path,
                      "Check overall quality",
                    ),
                  ]
                  let response =
                    json_output.success(
                      "lint_result",
                      "lint",
                      json.object([
                        #("valid", json.bool(True)),
                        #("warnings", json.array([], fn(x) { x })),
                      ]),
                      Some(spec_path),
                      next_actions,
                    )
                  json_output.output(response)
                }
                halt(exit_pass)
              }
              spec_linter.LintWarnings(warnings) -> {
                {
                  let warnings_by_severity = fn(severity) {
                    warnings
                    |> list.filter(fn(w) {
                      spec_linter.warning_severity(w) == severity
                    })
                  }

                  let errors = warnings_by_severity(spec_linter.SeverityError)
                  let warns = warnings_by_severity(spec_linter.SeverityWarning)
                  let infos = warnings_by_severity(spec_linter.SeverityInfo)

                  let next_actions = [
                    json_output.next_action(
                      "intent improve " <> spec_path,
                      "Get actionable suggestions",
                    ),
                    json_output.next_action(
                      "intent doctor " <> spec_path,
                      "Prioritized improvements",
                    ),
                  ]

                  let data =
                    json.object([
                      #("valid", json.bool(False)),
                      #("total_warnings", json.int(list.length(warnings))),
                      #("errors", json.int(list.length(errors))),
                      #("warnings", json.int(list.length(warns))),
                      #("info", json.int(list.length(infos))),
                      #(
                        "findings",
                        json.array(warnings, spec_linter.warning_to_json),
                      ),
                    ])

                  let response =
                    json_output.success(
                      "lint_result",
                      "lint",
                      data,
                      Some(spec_path),
                      next_actions,
                    )
                  json_output.output(response)
                }
                // Lint warnings are informational, not errors - exit 0
                halt(exit_pass)
              }
            }
          }
          Error(e) -> {
            {
              let error_msg = loader.format_error(e)
              let response =
                json_output.failure(
                  "lint_failed",
                  "lint",
                  json.null(),
                  [json_output.error("load_error", error_msg)],
                  Some(spec_path),
                  [],
                  exit_invalid,
                )
              json_output.output(response)
            }
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        {
          let response =
            json_output.failure(
              "lint_failed",
              "lint",
              json.null(),
              [json_output.error("usage_error", "spec file path required")],
              None,
              [],
              exit_error,
            )
          json_output.output(response)
        }
        halt(exit_error)
      }
    }
  })
  |> glint.description("Check spec for anti-patterns and quality issues")
}

/// The `check` command - run spec against API (placeholder)
fn check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let target_url =
      flag.get_string(input.flags, "target")
      |> result.unwrap("")

    let spec_path = case input.args {
      [path, ..] -> Ok(path)
      [] -> Error("spec file path required")
    }

    case spec_path {
      Ok(path) -> {
        case loader.load_spec(path) {
          Ok(spec) -> {
            let next_actions = [
              json_output.next_action(
                "intent validate " <> path,
                "Validate spec structure",
              ),
              json_output.next_action(
                "intent lint " <> path,
                "Check for anti-patterns",
              ),
            ]
            let data =
              json.object([
                #("spec_name", json.string(spec.name)),
                #("target_url", case string.is_empty(target_url) {
                  True -> json.string(spec.config.base_url)
                  False -> json.string(target_url)
                }),
                #(
                  "message",
                  json.string(
                    "Check command is under development. The full API testing implementation is not yet available.",
                  ),
                ),
              ])
            let response =
              json_output.success(
                "check_result",
                "check",
                data,
                Some(path),
                next_actions,
              )
            json_output.output(response)
            halt(exit_pass)
          }
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "check_failed",
                "check",
                json.null(),
                [json_output.error("load_error", error_msg)],
                Some(path),
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
        }
      }
      Error(msg) -> {
        let response =
          json_output.failure(
            "check_failed",
            "check",
            json.null(),
            [json_output.error("usage_error", msg)],
            None,
            [],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description("Run spec against API (under development)")
  |> glint.flag(
    "target",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Target API URL (uses config.base_url if not specified)",
      ),
  )
}

/// The `analyze` command - analyze spec quality
/// The `analyze` command - Quality analysis (alias for quality, text output only)
fn analyze_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        // analyze is an alias for quality - now supports both text and JSON output
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let report = quality_analyzer.analyze_spec(spec)
            {
              let data =
                json.object([
                  #("coverage_score", json.int(report.coverage_score)),
                  #("clarity_score", json.int(report.clarity_score)),
                  #("testability_score", json.int(report.testability_score)),
                  #("ai_readiness_score", json.int(report.ai_readiness_score)),
                  #("overall_score", json.int(report.overall_score)),
                  #(
                    "issues",
                    json.array(report.issues, fn(i) {
                      json.string(quality_analyzer.format_issue(i))
                    }),
                  ),
                  #(
                    "suggestions",
                    json.array(report.suggestions, fn(s) { json.string(s) }),
                  ),
                ])
              let next_actions = [
                json_output.next_action(
                  "intent gaps " <> spec_path <> " --json",
                  "Find coverage gaps",
                ),
                json_output.next_action(
                  "intent invert " <> spec_path <> " --json",
                  "Analyze failure modes",
                ),
              ]
              let response =
                json_output.success(
                  "analyze_result",
                  "analyze",
                  data,
                  Some(spec_path),
                  next_actions,
                )
              json_output.output(response)
            }
            halt(exit_pass)
          }
          Error(e) -> {
            ai_errors.from_load_error(e, spec_path)
            |> ai_errors.format_text()
            |> io.println_error()
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        ai_errors.cli_error_with_usage(
          message: "spec file path required",
          usage: "intent analyze <spec.cue> [--json]",
        )
        |> ai_errors.format_cli_error()
        |> io.println_error()
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Analyze spec quality and provide improvement suggestions",
  )
}

/// The `improve` command - suggest improvements
fn improve_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let quality_report = quality_analyzer.analyze_spec(spec)
            let lint_result = spec_linter.lint_spec(spec)
            let context =
              improver.ImprovementContext(
                quality_report: quality_report,
                lint_result: lint_result,
                spec: spec,
              )
            let suggestions = improver.suggest_improvements(context)

            {
              let data =
                json.object([
                  #(
                    "suggestions",
                    json.array(suggestions, fn(s) {
                      json.object([
                        #("title", json.string(s.title)),
                        #("description", json.string(s.description)),
                        #("reasoning", json.string(s.reasoning)),
                        #("impact_score", json.int(s.impact_score)),
                      ])
                    }),
                  ),
                  #("suggestion_count", json.int(list.length(suggestions))),
                ])
              let next_actions = [
                json_output.next_action(
                  "intent validate " <> spec_path,
                  "Verify spec structure and syntax",
                ),
                json_output.next_action(
                  "intent doctor " <> spec_path <> " --json",
                  "Get prioritized recommendations",
                ),
              ]
              let response =
                json_output.success(
                  "improve_result",
                  "improve",
                  data,
                  Some(spec_path),
                  next_actions,
                )
              json_output.output(response)
            }
            halt(exit_pass)
          }
          Error(e) -> {
            ai_errors.from_load_error(e, spec_path)
            |> ai_errors.format_text()
            |> io.println_error()
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        ai_errors.cli_error_with_usage(
          message: "spec file path required",
          usage: "intent improve <spec.cue> [--json]",
        )
        |> ai_errors.format_cli_error()
        |> io.println_error()
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Suggest improvements based on quality analysis and linting",
  )
}

/// The `doctor` command - health report with prioritized improvements
fn doctor_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let report = doctor.run_doctor(spec)
            doctor.json_output(report, spec_path)
            halt(exit_pass)
          }
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "doctor_failed",
                "doctor",
                json.null(),
                [json_output.error("load_error", error_msg)],
                Some(spec_path),
                [],
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
            "doctor_failed",
            "doctor",
            json.null(),
            [json_output.error("usage_error", "spec file path required")],
            None,
            [],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Analyze spec health and generate prioritized improvement report",
  )
}

/// The `interview` command - guided specification discovery
fn interview_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // Check for unexpected arguments (common mistake: profile as arg instead of flag)
    case input.args {
      [arg, ..] -> {
        io.println_error("Error: Unexpected argument '" <> arg <> "'")
        io.println_error("")
        io.println_error(
          "Did you mean: intent interview --profile=" <> arg <> " ?",
        )
        io.println_error("")
        io.println_error("Valid profiles: api, cli, event, data, workflow, ui")
        halt(exit_error)
      }
      [] -> Nil
    }

    let profile_result = flag.get_string(input.flags, "profile")
    let profile_str = result.unwrap(profile_result, "api")

    let resume_id =
      flag.get_string(input.flags, "resume")
      |> result.unwrap("")

    let session_flag =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    let answer_text =
      flag.get_string(input.flags, "answer")
      |> result.unwrap("")

    let dry_run =
      flag.get_bool(input.flags, "dry-run")
      |> result.unwrap(False)

    let batch_mode =
      flag.get_bool(input.flags, "batch")
      |> result.unwrap(False)

    let input_file =
      flag.get_string(input.flags, "input")
      |> result.unwrap("")

    let export_path =
      flag.get_string(input.flags, "export")
      |> result.unwrap("")

    // Batch mode: process all answers from JSON file
    case batch_mode {
      True -> {
        // Validate input file is provided
        case string.is_empty(input_file) {
          True -> {
            io.println_error("Error: --input flag required with --batch mode")
            halt(exit_error)
          }
          False -> {
            run_interview_batch(input_file, export_path)
          }
        }
      }
      False -> {
        // Continue to CUE mode handling (AI-only mode)
        Nil
      }
    }

    // CUE mode: output CUE directives for AI agents (AI-only, always enabled)
    let has_resume = !string.is_empty(resume_id)
    let has_session = !string.is_empty(session_flag)
    let has_answer = !string.is_empty(answer_text)

    case has_resume, has_session, has_answer {
      // Resume session in CUE mode (--resume takes precedence)
      True, _, _ -> run_interview_cue_resume(resume_id, dry_run)
      // Submitting an answer to an existing session
      False, True, True ->
        run_interview_cue_answer(session_flag, answer_text, dry_run)
      // Start new session in CUE mode (requires explicit --profile)
      False, False, False -> {
        case profile_result {
          Error(_) -> {
            output_cue_error(
              "No flags provided. Use --profile to start a new interview, --resume to continue one, or --session with --answer to submit a response.\n\nExamples:\n  intent interview --profile=api\n  intent interview --resume=<session-id>\n  intent interview --session=<session-id> --answer='THE SYSTEM SHALL ...'",
            )
            halt(exit_error)
          }
          Ok(_) -> {
            let profile = parse_profile(profile_str)
            case profile {
              Ok(p) -> run_interview_cue_start(p, dry_run)
              Error(msg) -> {
                output_cue_error(msg)
                halt(exit_error)
              }
            }
          }
        }
      }
      // Invalid: answer without session
      False, False, True -> {
        output_cue_error("--answer requires --session flag")
        halt(exit_error)
      }
      // Invalid: session without answer
      False, True, False -> {
        output_cue_error(
          "--session requires --answer flag (use --resume to resume a session)",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Guided specification discovery through structured interview",
  )
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.description(
        "System profile type: api, cli, event, data, workflow, or ui",
      ),
  )
  |> glint.flag(
    "resume",
    flag.string()
      |> flag.default("")
      |> flag.description("Resume existing interview session using its ID"),
  )
  |> glint.flag(
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Session identifier for CUE mode (required with --cue flag)",
      ),
  )
  |> glint.flag(
    "answer",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Response value for current question in CUE mode (use with --cue and --session)",
      ),
  )
  |> glint.flag(
    "dry-run",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Preview interview questions without persisting to session storage",
      ),
  )
  |> glint.flag(
    "batch",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Batch mode: process all answers from JSON file non-interactively",
      ),
  )
  |> glint.flag(
    "input",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Path to JSON file with answers for batch mode (required with --batch)",
      ),
  )
  |> glint.flag(
    "export",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Path to write generated CUE spec (optional, spec returned in JSON output)",
      ),
  )
}

fn profile_to_string(profile: interview.Profile) -> String {
  case profile {
    interview.Api -> "api"
    interview.Cli -> "cli"
    interview.Event -> "event"
    interview.Data -> "data"
    interview.Workflow -> "workflow"
    interview.UI -> "ui"
  }
}

// =============================================================================
// CUE MODE INTERVIEW FUNCTIONS
// =============================================================================

/// Parse profile string to Profile type
fn parse_profile(profile_str: String) -> Result(interview.Profile, String) {
  case string.lowercase(profile_str) {
    "api" -> Ok(interview.Api)
    "cli" -> Ok(interview.Cli)
    "event" -> Ok(interview.Event)
    "data" -> Ok(interview.Data)
    "workflow" -> Ok(interview.Workflow)
    "ui" -> Ok(interview.UI)
    _ ->
      Error(
        "Unknown profile '"
        <> profile_str
        <> "'. Valid profiles: api, cli, event, data, workflow, ui",
      )
  }
}

// =============================================================================
// BATCH MODE INTERVIEW FUNCTIONS
// =============================================================================

/// Batch input from JSON file
pub type BatchInput {
  BatchInput(profile: String, answers: List(BatchAnswer))
}

/// Single answer in batch input
pub type BatchAnswer {
  BatchAnswer(question_id: String, response: String)
}

/// Parse batch input JSON from string
pub fn parse_batch_input_from_string(
  content: String,
) -> Result(BatchInput, String) {
  case json.decode(content, dynamic.dynamic) {
    Error(_) -> Error("Invalid JSON syntax")
    Ok(data) -> {
      // Decode profile
      let profile_decoder = dynamic.field("profile", dynamic.string)
      let profile_result = profile_decoder(data)

      // Decode answers array
      let answer_decoder =
        dynamic.decode2(
          BatchAnswer,
          dynamic.field("question_id", dynamic.string),
          dynamic.field("response", dynamic.string),
        )
      let answers_decoder =
        dynamic.field("answers", dynamic.list(answer_decoder))
      let answers_result = answers_decoder(data)

      case profile_result, answers_result {
        Ok(profile), Ok(answers) -> {
          // Validate profile value
          case parse_profile(profile) {
            Error(_) ->
              Error(
                "Invalid profile value: '"
                <> profile
                <> "'. Must be one of: api, cli, event, data, workflow, ui",
              )
            Ok(_) -> {
              // Validate answers not empty
              case list.is_empty(answers) {
                True -> Error("Answers array cannot be empty")
                False -> Ok(BatchInput(profile: profile, answers: answers))
              }
            }
          }
        }
        Error(_), _ -> Error("Missing required field: profile")
        _, Error(_) -> Error("Missing required field: answers")
      }
    }
  }
}

/// Parse batch input from file
fn parse_batch_input(file_path: String) -> Result(BatchInput, String) {
  case simplifile.read(file_path) {
    Error(_) -> Error("File not found: " <> file_path)
    Ok(content) -> parse_batch_input_from_string(content)
  }
}

/// Run interview in batch mode
fn run_interview_batch(input_file: String, export_path: String) -> Nil {
  // Parse input file
  let batch_result = parse_batch_input(input_file)
  case batch_result {
    Error(msg) -> {
      // Determine exit code based on error type
      case
        string.contains(msg, "File not found")
        || string.contains(msg, "Invalid JSON")
      {
        True -> {
          // Exit 3 for invalid JSON or file not found
          io.println_error("Error: " <> msg)
          halt(exit_invalid)
        }
        False -> {
          // Exit 4 for missing required fields
          io.println_error("Error: " <> msg)
          halt(exit_error)
        }
      }
    }
    Ok(batch_input) -> {
      // Parse profile
      let profile_result = parse_profile(batch_input.profile)
      case profile_result {
        Error(msg) -> {
          io.println_error("Error: " <> msg)
          halt(exit_error)
        }
        Ok(profile) -> {
          // Create session
          let session_id = "interview-" <> generate_uuid()
          let timestamp = current_timestamp()
          let session = interview.create_session(session_id, profile, timestamp)

          // Process answers
          let updated_session =
            list.fold(batch_input.answers, session, fn(sess, batch_answer) {
              // Create Answer object
              let answer =
                interview.Answer(
                  question_id: batch_answer.question_id,
                  question_text: "",
                  perspective: question_types.Developer,
                  round: 1,
                  response: batch_answer.response,
                  extracted: dict.new(),
                  confidence: 1.0,
                  notes: "",
                  timestamp: timestamp,
                )
              interview.add_answer(sess, answer)
            })

          // Save session
          let save_result =
            interview_storage.append_session_to_jsonl(
              updated_session,
              sessions_jsonl,
            )

          case save_result {
            Error(err) -> {
              io.println_error("Error saving session: " <> err)
              halt(exit_error)
            }
            Ok(_) -> {
              // Generate spec
              let spec_content =
                spec_builder.build_spec_from_session(updated_session)

              // Write spec to export file if provided
              let spec_path = case string.is_empty(export_path) {
                True -> ""
                False -> {
                  case simplifile.write(export_path, spec_content) {
                    Ok(_) -> export_path
                    Error(_) -> {
                      io.println_error("Warning: Failed to write spec to file")
                      ""
                    }
                  }
                }
              }

              // Output JSON result
              let output =
                json.object([
                  #("success", json.bool(True)),
                  #("session_id", json.string(session_id)),
                  #("profile", json.string(batch_input.profile)),
                  #(
                    "answers_processed",
                    json.int(list.length(batch_input.answers)),
                  ),
                  #("spec_generated", json.bool(True)),
                  #("spec_path", json.string(spec_path)),
                ])
              io.println(json.to_string(output))
              halt(exit_pass)
            }
          }
        }
      }
    }
  }
}

/// Output a CUE error directive
fn output_cue_error(message: String) -> Nil {
  io.println(
    "{\n\taction: \"validation_error\"\n\terror: {\n\t\tmessage: \""
    <> escape_cue_string(message)
    <> "\"\n\t\tsuggestion: \"Check your input and try again\"\n\t\tretry_allowed: true\n\t}\n}",
  )
}

/// Start a new interview session in CUE mode
fn run_interview_cue_start(profile: interview.Profile, dry_run: Bool) -> Nil {
  let session_id = case dry_run {
    True -> "dry-run-" <> generate_uuid()
    False -> "interview-" <> generate_uuid()
  }
  let timestamp = current_timestamp()
  let session = interview.create_session(session_id, profile, timestamp)

  // Save session to JSONL (skip in dry-run mode)
  let save_result = case dry_run {
    True -> Ok(Nil)
    False -> interview_storage.append_session_to_jsonl(session, sessions_jsonl)
  }

  case save_result {
    Ok(_) -> {
      // Get first question
      case interview.get_first_question_for_round(session, 1) {
        Ok(question) -> output_cue_question(session, question, 1)
        Error(_) -> {
          output_cue_error("No questions available for this profile")
          halt(exit_error)
        }
      }
    }
    Error(err) -> {
      output_cue_error("Failed to save session: " <> err)
      halt(exit_error)
    }
  }
}

/// Resume an existing interview session in CUE mode
fn run_interview_cue_resume(session_id: String, dry_run: Bool) -> Nil {
  let is_dry_run_session = string.starts_with(session_id, "dry-run-")

  case interview_storage.get_session_from_jsonl(sessions_jsonl, session_id) {
    Error(err) -> {
      case is_dry_run_session || dry_run {
        True -> {
          output_cue_error(
            "Cannot resume dry-run session (not saved): " <> session_id,
          )
          halt(exit_error)
        }
        False -> {
          output_cue_error("Session not found: " <> err)
          halt(exit_error)
        }
      }
    }
    Ok(session) -> {
      // Check if interview is complete
      case session.stage {
        interview.Complete -> {
          output_cue_complete(session)
        }
        _ -> {
          // Find next unanswered question
          let next_round = case session.rounds_completed {
            0 -> 1
            r if r < 5 -> r + 1
            _ -> 5
          }
          case get_next_unanswered_question(session, next_round) {
            Some(question) -> output_cue_question(session, question, next_round)
            None -> {
              // All questions answered, complete the interview
              output_cue_complete(session)
            }
          }
        }
      }
    }
  }
}

/// Get the next unanswered question for a session
fn get_next_unanswered_question(
  session: interview.InterviewSession,
  start_round: Int,
) -> Option(Question) {
  let profile_str = profile_to_string(session.profile)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  // Try each round starting from start_round
  find_unanswered_in_rounds(profile_str, answered_ids, start_round)
}

fn find_unanswered_in_rounds(
  profile_str: String,
  answered_ids: List(String),
  round: Int,
) -> Option(Question) {
  case round > 5 {
    True -> None
    False -> {
      let questions =
        interview_questions.get_questions_for_round(profile_str, round)
      let unanswered =
        list.filter(questions, fn(q) { !list.contains(answered_ids, q.id) })
      case unanswered {
        [first, ..] -> Some(first)
        [] -> find_unanswered_in_rounds(profile_str, answered_ids, round + 1)
      }
    }
  }
}

/// Get the next unanswered question within a specific round only
/// Returns None if all questions in the round are answered
/// Does NOT advance to subsequent rounds (unlike get_next_unanswered_question)
fn get_next_unanswered_question_in_round(
  session: interview.InterviewSession,
  round: Int,
) -> Option(Question) {
  let profile_str = profile_to_string(session.profile)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  // Only search within the specified round
  let questions =
    interview_questions.get_questions_for_round(profile_str, round)
  let unanswered =
    list.filter(questions, fn(q) { !list.contains(answered_ids, q.id) })

  case unanswered {
    [first, ..] -> Some(first)
    [] -> None
  }
}

/// Submit an answer to a session in CUE mode
fn run_interview_cue_answer(
  session_id: String,
  answer_text: String,
  dry_run: Bool,
) -> Nil {
  let is_dry_run_session = string.starts_with(session_id, "dry-run-")

  case interview_storage.get_session_from_jsonl(sessions_jsonl, session_id) {
    Error(err) -> {
      case is_dry_run_session || dry_run {
        True -> {
          output_cue_error(
            "Cannot answer dry-run session (not saved): " <> session_id,
          )
          halt(exit_error)
        }
        False -> {
          output_cue_error("Session not found: " <> err)
          halt(exit_error)
        }
      }
    }
    Ok(session) -> {
      // Validate answer (basic validation)
      case string.length(string.trim(answer_text)) < 3 {
        True -> {
          output_cue_validation_error(
            "Answer too short",
            "Please provide a more detailed response",
          )
          halt(exit_fail)
        }
        False -> {
          // Find the current question being answered
          let next_round = case session.rounds_completed {
            0 -> 1
            r if r < 5 -> r + 1
            _ -> 5
          }

          case get_next_unanswered_question(session, next_round) {
            None -> {
              // No questions left, interview is complete
              output_cue_complete(session)
            }
            Some(question) -> {
              // Create answer record
              let extracted =
                interview.extract_from_answer(
                  question.id,
                  answer_text,
                  question.extract_into,
                )
              let confidence =
                interview.calculate_confidence(
                  question.id,
                  answer_text,
                  extracted,
                )

              let answer =
                interview.Answer(
                  question_id: question.id,
                  question_text: question.question,
                  perspective: question.perspective,
                  round: next_round,
                  response: answer_text,
                  extracted: extracted,
                  confidence: confidence,
                  notes: "",
                  timestamp: current_timestamp(),
                )

              // Add answer to session
              let updated_session = interview.add_answer(session, answer)

              // Check for gaps and conflicts
              let #(sess_with_gaps, _gaps) =
                interview.check_for_gaps(updated_session, question, answer)
              let #(sess_final, _conflicts) =
                interview.check_for_conflicts(sess_with_gaps, answer)

              // Save updated session (skip in dry-run mode)
              let save_result = case is_dry_run_session || dry_run {
                True -> Ok(Nil)
                False ->
                  interview_storage.append_session_to_jsonl(
                    sess_final,
                    sessions_jsonl,
                  )
              }

              case save_result {
                Error(err) -> {
                  output_cue_error("Failed to save session: " <> err)
                  halt(exit_error)
                }
                Ok(_) -> {
                  // Check if there are more questions in the current round
                  case
                    get_next_unanswered_question_in_round(
                      sess_final,
                      next_round,
                    )
                  {
                    Some(next_q) -> {
                      // More questions in current round - continue
                      output_cue_question(sess_final, next_q, next_round)
                    }
                    None -> {
                      // Current round is complete - increment rounds_completed
                      let sess_round_completed =
                        interview.complete_round(sess_final)

                      // Save round completion (skip in dry-run mode)
                      let round_save_result = case
                        is_dry_run_session || dry_run
                      {
                        True -> Ok(Nil)
                        False ->
                          interview_storage.append_session_to_jsonl(
                            sess_round_completed,
                            sessions_jsonl,
                          )
                      }

                      case round_save_result {
                        Error(err) -> {
                          output_cue_error(
                            "Failed to save round completion: " <> err,
                          )
                          halt(exit_error)
                        }
                        Ok(_) -> {
                          // Check if there are more rounds
                          case next_round < 5 {
                            True -> {
                              case
                                get_next_unanswered_question(
                                  sess_round_completed,
                                  next_round + 1,
                                )
                              {
                                Some(next_q) ->
                                  output_cue_question(
                                    sess_round_completed,
                                    next_q,
                                    next_round + 1,
                                  )
                                None ->
                                  output_cue_complete(sess_round_completed)
                              }
                            }
                            False -> output_cue_complete(sess_round_completed)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Output a CUE question directive
fn output_cue_question(
  session: interview.InterviewSession,
  question: Question,
  round: Int,
) -> Nil {
  let profile_str = profile_to_string(session.profile)
  let total_questions = get_total_questions(profile_str)
  let answered_count = list.length(session.answers)
  let percent = case total_questions > 0 {
    True -> { answered_count * 100 } / total_questions
    False -> 0
  }

  let pattern = infer_ears_pattern(question)
  let hint = get_ears_hint(pattern)
  let examples = get_pattern_examples(pattern)

  let category = case round {
    1 -> "basic_info"
    2 -> "behaviors"
    3 -> "edge_cases"
    4 -> "security"
    _ -> "validation"
  }

  let is_dry_run = string.starts_with(session.id, "dry-run-")

  let output =
    "{\n"
    <> "\taction: \"ask_question\"\n\n"
    <> "\tquestion: {\n"
    <> "\t\ttext: \""
    <> escape_cue_string(question.question)
    <> "\"\n"
    <> "\t\tpattern: \""
    <> pattern
    <> "\"\n"
    <> "\t\texamples: ["
    <> format_cue_string_list(examples)
    <> "]\n"
    <> "\t\thint: \""
    <> escape_cue_string(hint)
    <> "\"\n"
    <> "\t}\n\n"
    <> "\tprogress: {\n"
    <> "\t\tcurrent_step: "
    <> string.inspect(answered_count + 1)
    <> "\n"
    <> "\t\ttotal_steps: "
    <> string.inspect(total_questions)
    <> "\n"
    <> "\t\tpercent_complete: "
    <> string.inspect(percent)
    <> "\n"
    <> "\t\tcategory: \""
    <> category
    <> "\"\n"
    <> "\t}\n\n"
    <> "\tsession: {\n"
    <> "\t\tid: \""
    <> session.id
    <> "\"\n"
    <> "\t\tprofile: \""
    <> profile_str
    <> "\"\n"
    <> "\t\tstarted_at: \""
    <> session.created_at
    <> "\"\n"
    <> case is_dry_run {
      True -> "\t\tdry_run: true\n"
      False -> ""
    }
    <> "\t}\n\n"
    <> "\tnext_action: {\n"
    <> "\t\tcommand: \"intent interview --cue --session="
    <> session.id
    <> " --answer='<your-answer>'\"\n"
    <> "\t\tdescription: \"Submit your answer to continue the interview\"\n"
    <> "\t\texample: \"intent interview --cue --session="
    <> session.id
    <> " --answer='THE SYSTEM SHALL validate all API inputs'\"\n"
    <> "\t}\n"
    <> "}"

  io.println(output)
  halt(exit_pass)
}

/// Output a CUE validation error
fn output_cue_validation_error(message: String, suggestion: String) -> Nil {
  io.println(
    "{\n\taction: \"validation_error\"\n\terror: {\n\t\tmessage: \""
    <> escape_cue_string(message)
    <> "\"\n\t\tsuggestion: \""
    <> escape_cue_string(suggestion)
    <> "\"\n\t\tretry_allowed: true\n\t}\n}",
  )
}

/// Output interview complete directive
fn output_cue_complete(session: interview.InterviewSession) -> Nil {
  let behaviors_count = list.length(session.answers)
  let anti_patterns_count = list.length(session.gaps)
  let is_dry_run = string.starts_with(session.id, "dry-run-")

  // Generate and save the spec (skip in dry-run mode)
  let spec_path = case is_dry_run {
    True -> ""
    False -> spec_file_path(session.id)
  }

  case is_dry_run {
    False -> {
      let spec_cue = spec_builder.build_spec_from_session(session)
      let _ = simplifile.write(spec_path, spec_cue)
      Nil
    }
    True -> Nil
  }

  let summary = case is_dry_run {
    True ->
      "DRY RUN complete. No spec generated (use without --dry-run to save)."
    False ->
      "Interview complete. Generated spec with "
      <> string.inspect(behaviors_count)
      <> " behaviors."
  }

  let output =
    "{\n"
    <> "\taction: \"interview_complete\"\n\n"
    <> "\toutput: {\n"
    <> case is_dry_run {
      False -> "\t\tspec_path: \"" <> spec_path <> "\"\n"
      True -> ""
    }
    <> "\t\tbehaviors_count: "
    <> string.inspect(behaviors_count)
    <> "\n"
    <> "\t\tanti_patterns_count: "
    <> string.inspect(anti_patterns_count)
    <> "\n"
    <> "\t\tsummary: \""
    <> escape_cue_string(summary)
    <> "\"\n"
    <> case is_dry_run {
      True -> "\t\tdry_run: true\n"
      False -> ""
    }
    <> "\t}\n\n"
    <> "\tsession: {\n"
    <> "\t\tid: \""
    <> session.id
    <> "\"\n"
    <> "\t\tprofile: \""
    <> profile_to_string(session.profile)
    <> "\"\n"
    <> "\t\tstarted_at: \""
    <> session.created_at
    <> "\"\n"
    <> "\t\tcompleted_at: \""
    <> current_timestamp()
    <> "\"\n"
    <> case is_dry_run {
      True -> "\t\tdry_run: true\n"
      False -> ""
    }
    <> "\t}\n"
    <> "}"

  io.println(output)
  halt(exit_pass)
}

/// Get total number of questions for a profile
fn get_total_questions(profile_str: String) -> Int {
  list.range(1, 5)
  |> list.map(fn(round) {
    interview_questions.get_questions_for_round(profile_str, round)
  })
  |> list.map(list.length)
  |> list.fold(0, fn(acc, n) { acc + n })
}

/// Infer EARS pattern from question context
fn infer_ears_pattern(question: Question) -> String {
  let q_lower = string.lowercase(question.question)

  case
    string.contains(q_lower, "when"),
    string.contains(q_lower, "while"),
    string.contains(q_lower, "if"),
    string.contains(q_lower, "should not"),
    string.contains(q_lower, "optional")
  {
    True, True, _, _, _ -> "complex"
    True, False, _, _, _ -> "event_driven"
    False, True, _, _, _ -> "state_driven"
    _, _, True, True, _ -> "unwanted"
    _, _, _, _, True -> "optional"
    _, _, _, _, _ -> "ubiquitous"
  }
}

/// Get EARS hint for a pattern
fn get_ears_hint(pattern: String) -> String {
  case pattern {
    "ubiquitous" -> "Use format: THE SYSTEM SHALL [behavior]"
    "event_driven" -> "Use format: WHEN [trigger] THE SYSTEM SHALL [behavior]"
    "state_driven" -> "Use format: WHILE [state] THE SYSTEM SHALL [behavior]"
    "optional" -> "Use format: WHERE [condition] THE SYSTEM SHALL [behavior]"
    "unwanted" -> "Use format: IF [condition] THE SYSTEM SHALL NOT [behavior]"
    "complex" ->
      "Use format: WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]"
    _ -> "Use EARS format: THE SYSTEM SHALL [behavior]"
  }
}

/// Get example answers for a pattern
fn get_pattern_examples(pattern: String) -> List(String) {
  case pattern {
    "ubiquitous" -> [
      "THE SYSTEM SHALL validate all API inputs",
      "THE SYSTEM SHALL log all requests",
    ]
    "event_driven" -> [
      "WHEN user submits form THE SYSTEM SHALL validate data",
      "WHEN request times out THE SYSTEM SHALL retry",
    ]
    "state_driven" -> [
      "WHILE user is authenticated THE SYSTEM SHALL allow access",
      "WHILE rate limit exceeded THE SYSTEM SHALL reject requests",
    ]
    "optional" -> [
      "WHERE user has admin role THE SYSTEM SHALL allow admin actions",
    ]
    "unwanted" -> [
      "IF token is expired THE SYSTEM SHALL NOT authorize requests",
    ]
    "complex" -> [
      "WHILE in transaction WHEN error occurs THE SYSTEM SHALL rollback",
    ]
    _ -> ["THE SYSTEM SHALL [describe behavior]"]
  }
}

/// Format a list of strings for CUE output
fn format_cue_string_list(items: List(String)) -> String {
  items
  |> list.map(fn(s) { "\"" <> escape_cue_string(s) <> "\"" })
  |> string.join(", ")
}

/// The `beads` command - generate work items from interview session
fn beads_command() -> glint.Command(Nil) {
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

/// Mark a bead with execution status (success/failed/blocked)
fn bead_status_command() -> glint.Command(Nil) {
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

// =============================================================================
// PLAN COMMANDS
// =============================================================================

/// The `plan` command - display execution plan for a session
fn plan_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let format =
      flag.get_string(input.flags, "format")
      |> result.unwrap("human")

    case input.args {
      [session_id, ..] -> {
        case compute_plan_with_session(session_id) {
          Error(err) -> {
            io.println_error(plan_mode.format_error(err))
            halt(exit_error)
          }
          Ok(plan) -> {
            let output = case format {
              "json" -> plan_mode.format_plan_json(plan)
              _ -> plan_mode.format_plan_human(plan)
            }
            io.println(output)
            halt(exit_pass)
          }
        }
      }
      [] -> {
        // No session_id provided - return JSON usage info with exit code 0
        // This makes the command more testable and AI-friendly
        let error =
          json_output.error(
            "missing_arguments",
            "Session ID is required for plan command",
          )

        let response =
          json_output.failure(
            "plan_usage",
            "plan",
            json.object([
              #(
                "usage",
                json.string("intent plan <session_id> [--format human|json]"),
              ),
              #(
                "description",
                json.string("Display execution plan from session beads"),
              ),
            ]),
            [error],
            None,
            [
              json_output.next_action(
                "intent sessions",
                "List available interview sessions",
              ),
              json_output.next_action(
                "intent beads <session_id>",
                "Generate beads from a session first",
              ),
            ],
            exit_pass,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Display execution plan from session beads")
  |> glint.flag(
    "format",
    flag.string()
      |> flag.default("human")
      |> flag.description("Output format: human or json"),
  )
}

/// Compute plan with rounds_completed from session JSONL
/// Loads session to get rounds_completed, then computes plan with RCS score
fn compute_plan_with_session(
  session_id: String,
) -> Result(plan_mode.ExecutionPlan, plan_mode.PlanError) {
  let jsonl_path = sessions_jsonl
  let session_path = session_file_path(session_id)

  // Try to load rounds_completed from session JSONL (default to 0 if not found)
  let rounds_completed =
    interview_storage.get_session_from_jsonl(jsonl_path, session_id)
    |> result.map(fn(session) { session.rounds_completed })
    |> result.unwrap(0)

  // Read the CUE content and compute plan with rounds_completed
  case simplifile.read(session_path) {
    Error(_) -> Error(plan_mode.SessionNotFound(session_id))
    Ok(content) ->
      plan_mode.compute_plan_from_content(session_id, content, rounds_completed)
  }
}

/// The `plan-approve` command - approve execution plan for CI/automation
fn plan_approve_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let auto_approve =
      flag.get_bool(input.flags, "yes")
      |> result.unwrap(False)

    let notes =
      flag.get_string(input.flags, "notes")
      |> result.unwrap("")

    case input.args {
      [session_id, ..] -> {
        // First verify the session exists and has a valid plan
        case compute_plan_with_session(session_id) {
          Error(err) -> {
            let response =
              json_output.failure(
                "plan_approve_failed",
                "plan_approve",
                json.object([#("session_id", json.string(session_id))]),
                [json_output.error("plan_error", plan_mode.format_error(err))],
                Some(session_id),
                [
                  json_output.next_action(
                    "intent plan " <> session_id,
                    "Generate plan for session",
                  ),
                  json_output.next_action(
                    "intent sessions",
                    "List available sessions",
                  ),
                ],
                exit_error,
              )
            json_output.output(response)
            halt(exit_error)
          }
          Ok(plan) -> {
            // Auto-approve or prompt
            case auto_approve {
              True -> {
                case approve_plan(session_id, "ci", notes) {
                  Ok(Nil) -> {
                    let response =
                      json_output.success(
                        "plan_approved",
                        "plan_approve",
                        json.object([
                          #("session_id", json.string(session_id)),
                          #("total_beads", json.int(plan.total_beads)),
                          #("total_effort", json.string(plan.total_effort)),
                          #(
                            "risk_level",
                            json.string(risk_level_to_string(plan.risk)),
                          ),
                          #("phases", json.int(list.length(plan.phases))),
                          #("approved_by", json.string("ci")),
                        ]),
                        Some(session_id),
                        [
                          json_output.next_action(
                            "bd list --status open",
                            "Show work items to execute",
                          ),
                          json_output.next_action(
                            "intent check <spec>",
                            "Begin execution",
                          ),
                        ],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    let response =
                      json_output.failure(
                        "plan_approve_failed",
                        "plan_approve",
                        json.object([#("session_id", json.string(session_id))]),
                        [json_output.error("approval_error", err)],
                        Some(session_id),
                        [
                          json_output.next_action(
                            "intent plan " <> session_id,
                            "Review plan again",
                          ),
                        ],
                        exit_error,
                      )
                    json_output.output(response)
                    halt(exit_error)
                  }
                }
              }
              False -> {
                // Interactive mode removed - AI-only mode requires --yes flag
                let response =
                  json_output.failure(
                    "plan_approve_failed",
                    "plan_approve",
                    json.null(),
                    [
                      json_output.error(
                        "interactive_not_available",
                        "Interactive approval not available in AI-only mode",
                      ),
                    ],
                    None,
                    [
                      json_output.next_action(
                        "intent plan-approve " <> session_id <> " --yes",
                        "Approve plan for CI execution",
                      ),
                      json_output.next_action(
                        "intent plan-approve "
                          <> session_id
                          <> " --yes --notes='...'",
                        "Approve with notes",
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
        let response =
          json_output.failure(
            "plan_approve_failed",
            "plan_approve",
            json.null(),
            [json_output.error("usage_error", "session ID required")],
            None,
            [
              json_output.next_action(
                "intent plan-approve <session_id> --yes",
                "Approve execution plan for session",
              ),
              json_output.next_action(
                "intent plan <session_id>",
                "Generate plan first",
              ),
              json_output.next_action(
                "intent sessions",
                "List available sessions",
              ),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description("Approve execution plan for session")
  |> glint.flag(
    "yes",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Auto-approve for CI (non-interactive)"),
  )
  |> glint.flag(
    "notes",
    flag.string() |> flag.default("") |> flag.description("Approval notes"),
  )
}

/// Write plan approval to session CUE file
fn approve_plan(
  session_id: String,
  approved_by: String,
  notes: String,
) -> Result(Nil, String) {
  let session_path = ".intent/session-" <> session_id <> ".cue"
  let timestamp = current_iso8601_timestamp()

  let notes_line = case string.is_empty(notes) {
    True -> ""
    False -> "\n\tnotes: \"" <> escape_cue_string(notes) <> "\""
  }

  let approval_cue =
    "\n// Plan Approval\napproval: {\n\tapproved: true\n\tapproved_at: \""
    <> timestamp
    <> "\"\n\tapproved_by: \""
    <> approved_by
    <> "\""
    <> notes_line
    <> "\n}\n"

  case simplifile.append(session_path, approval_cue) {
    Ok(Nil) -> Ok(Nil)
    Error(err) -> Error("Failed to write approval: " <> string.inspect(err))
  }
}

fn risk_level_to_string(risk: plan_mode.RiskLevel) -> String {
  case risk {
    plan_mode.Low -> "low"
    plan_mode.Medium -> "medium"
    plan_mode.High -> "high"
    plan_mode.Critical -> "critical"
  }
}

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn current_iso8601_timestamp() -> String {
  ffi.current_iso8601_timestamp()
}

// =============================================================================
// BEADS REGENERATE
// =============================================================================

/// The `beads-regenerate` command - regenerate failed/blocked beads
fn beads_regenerate_command() -> glint.Command(Nil) {
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
fn feedback_command() -> glint.Command(Nil) {
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

/// Generate AI-ready implementation prompts from session beads
fn prompt_command() -> glint.Command(Nil) {
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

/// Generate regeneration entries based on failed beads and strategy
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

/// Append regeneration metadata to session CUE file
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

// =============================================================================
// ERROR FORMATTING
// =============================================================================

fn bead_feedback_error_to_string(err: bead_feedback.FeedbackError) -> String {
  case err {
    bead_feedback.SessionNotFound(id) -> "Session not found: " <> id
    bead_feedback.WriteError(path, msg) ->
      "Write error to " <> path <> ": " <> msg
    bead_feedback.ValidationError(msg) -> "Validation error: " <> msg
  }
}

/// The `history` command - view session snapshot history
fn history_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let history_path = "history_jsonl"
    let _mode = output_mode.Interactive

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
            let total_count = list.length(all_snapshots)
            let snapshots = list_limits.apply_limit(all_snapshots, max_items)
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
            let next_actions = case list.is_empty(all_snapshots) {
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
    "max-items",
    flag.int()
      |> flag.default(list_limits.default_max_items)
      |> flag.description(
        "Maximum number of history snapshots to return (default: 100, AI guardrail)",
      ),
  )
}

/// The `diff` command - compare two specs
fn diff_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec1_path, spec2_path] -> {
        case load_spec_for_mode(spec1_path, True) {
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "diff_failed",
                "diff",
                json.null(),
                [json_output.error("load_error", error_msg)],
                Some(spec1_path),
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
          Ok(spec1) -> {
            case load_spec_for_mode(spec2_path, True) {
              Error(e) -> {
                let error_msg = loader.format_error(e)
                let response =
                  json_output.failure(
                    "diff_failed",
                    "diff",
                    json.null(),
                    [json_output.error("load_error", error_msg)],
                    Some(spec2_path),
                    [],
                    exit_invalid,
                  )
                json_output.output(response)
                halt(exit_invalid)
              }
              Ok(spec2) -> {
                let spec_diff = diff.compare_specs(spec1, spec2)
                let data = diff.diff_to_json(spec_diff)
                let next_actions = case spec_diff.has_changes {
                  True -> [
                    json_output.next_action(
                      "intent quality " <> spec2_path,
                      "Analyze quality of new spec",
                    ),
                  ]
                  False -> []
                }
                let response =
                  json_output.success(
                    "diff_result",
                    "diff",
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
      }
      _ -> {
        let error =
          json_output.error(
            "missing_arguments",
            "Two spec paths are required for diff",
          )

        let response =
          json_output.failure(
            "diff_usage",
            "diff",
            json.object([
              #(
                "usage",
                json.string("intent diff <spec1.cue> <spec2.cue>"),
              ),
              #(
                "description",
                json.string("Compare two spec versions and show differences"),
              ),
            ]),
            [error],
            None,
            [
              json_output.next_action(
                "intent diff spec1.cue spec2.cue",
                "Compare two specs",
              ),
            ],
            exit_pass,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Compare two spec versions and show differences")
}

/// The `help` command - show detailed help for a specific command
fn help_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [command_name] -> {
        io.println("For help on '" <> command_name <> "', run:")
        io.println("  intent " <> command_name <> " --help")
        halt(exit_pass)
      }
      [] -> {
        io.println("Intent CLI - AI-Only Mode")
        io.println("")
        io.println("Usage: intent <command> --help")
        io.println("")
        io.println("Available commands:")
        io.println("  validate, show, export, lint, analyze, improve, doctor,")
        io.println("  interview, beads, bead-status, history, diff, sessions,")
        io.println("  quality, invert, coverage, gaps, ears, parse, effects,")
        io.println("  plan, plan-approve, beads-regenerate, prompt, feedback")
        halt(exit_pass)
      }
      _ -> {
        io.println_error("Error: Too many arguments")
        io.println("")
        io.println("Usage: intent help <command>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Show detailed help for a specific command with usage examples and related commands",
  )
}

/// The `sessions` command - list all interview sessions
fn sessions_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let jsonl_path = sessions_jsonl

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
      Error(_) -> {
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
              profile_to_string(s.profile) == string.lowercase(p)
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

// =============================================================================
// KIRK COMMANDS
// =============================================================================

/// The `quality` command - Quality analysis (alias for analyze)
fn kirk_quality_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let report = quality_analyzer.analyze_spec(spec)
            {
              let data =
                json.object([
                  #("coverage_score", json.int(report.coverage_score)),
                  #("clarity_score", json.int(report.clarity_score)),
                  #("testability_score", json.int(report.testability_score)),
                  #("ai_readiness_score", json.int(report.ai_readiness_score)),
                  #("overall_score", json.int(report.overall_score)),
                  #(
                    "issues",
                    json.array(report.issues, fn(i) {
                      json.string(quality_analyzer.format_issue(i))
                    }),
                  ),
                  #(
                    "suggestions",
                    json.array(report.suggestions, fn(s) { json.string(s) }),
                  ),
                ])
              let next_actions = [
                json_output.next_action(
                  "intent gaps " <> spec_path <> " --json",
                  "Find coverage gaps",
                ),
                json_output.next_action(
                  "intent invert " <> spec_path <> " --json",
                  "Analyze failure modes",
                ),
              ]
              let response =
                json_output.success(
                  "quality_result",
                  "quality",
                  data,
                  Some(spec_path),
                  next_actions,
                )
              json_output.output(response)
            }
            halt(exit_pass)
          }
          Error(e) -> {
            {
              let error_msg = loader.format_error(e)
              let response =
                json_output.failure(
                  "quality_check_failed",
                  "quality",
                  json.null(),
                  [json_output.error("load_error", error_msg)],
                  Some(spec_path),
                  [],
                  exit_invalid,
                )
              json_output.output(response)
            }
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("spec file path required")
        io.println("Usage: intent quality <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description("KIRK: Analyze spec quality across multiple dimensions")
}

/// The `invert` command - KIRK inversion analysis
fn kirk_invert_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let report = inversion_checker.analyze_inversions(spec)
            {
              let data =
                json.object([
                  #("score", json.float(report.score)),
                  #(
                    "security_gaps",
                    json.array(report.security_gaps, gap_to_json),
                  ),
                  #(
                    "usability_gaps",
                    json.array(report.usability_gaps, gap_to_json),
                  ),
                  #(
                    "integration_gaps",
                    json.array(report.integration_gaps, gap_to_json),
                  ),
                  #(
                    "suggested_behaviors",
                    json.array(report.suggested_behaviors, fn(s) {
                      json.object([
                        #("name", json.string(s.name)),
                        #("intent", json.string(s.intent)),
                        #("expected_status", json.int(s.expected_status)),
                        #("category", json.string(s.category)),
                      ])
                    }),
                  ),
                ])
              let next_actions = [
                json_output.next_action(
                  "intent coverage " <> spec_path <> " --json",
                  "Check OWASP coverage",
                ),
                json_output.next_action(
                  "intent effects " <> spec_path <> " --json",
                  "Analyze second-order effects",
                ),
              ]
              let response =
                json_output.success(
                  "invert_result",
                  "invert",
                  data,
                  Some(spec_path),
                  next_actions,
                )
              json_output.output(response)
            }
            halt(exit_pass)
          }
          Error(e) -> {
            {
              let error_msg = loader.format_error(e)
              let response =
                json_output.failure(
                  "invert_check_failed",
                  "invert",
                  json.null(),
                  [json_output.error("load_error", error_msg)],
                  Some(spec_path),
                  [],
                  exit_invalid,
                )
              json_output.output(response)
            }
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("spec file path required")
        io.println("Usage: intent invert <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "KIRK: Inversion analysis - what failure cases are missing?",
  )
}

fn gap_to_json(gap: inversion_checker.InversionGap) -> json.Json {
  json.object([
    #("category", json.string(gap.category)),
    #("description", json.string(gap.description)),
    #(
      "severity",
      json.string(inversion_checker.severity_to_string(gap.severity)),
    ),
    #("what_could_fail", json.string(gap.what_could_fail)),
  ])
}

/// The `coverage` command - KIRK coverage analysis
fn kirk_coverage_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let report = coverage_analyzer.analyze_coverage(spec)
            {
              let data =
                json.object([
                  #("overall_score", json.float(report.overall_score)),
                  #(
                    "methods",
                    json.object(
                      report.methods
                      |> dict.to_list()
                      |> list.map(fn(pair) { #(pair.0, json.int(pair.1)) }),
                    ),
                  ),
                  #(
                    "status_codes",
                    json.object(
                      report.status_codes
                      |> dict.to_list()
                      |> list.map(fn(pair) { #(pair.0, json.int(pair.1)) }),
                    ),
                  ),
                  #("owasp_score", json.float(report.owasp.score)),
                  #(
                    "owasp_missing",
                    json.array(report.owasp.missing, json.string),
                  ),
                ])
              let next_actions = [
                json_output.next_action(
                  "intent gaps " <> spec_path <> " --json",
                  "Detect mental model gaps",
                ),
                json_output.next_action(
                  "intent quality " <> spec_path <> " --json",
                  "Check overall quality",
                ),
              ]
              let response =
                json_output.success(
                  "coverage_result",
                  "coverage",
                  data,
                  Some(spec_path),
                  next_actions,
                )
              json_output.output(response)
            }
            halt(exit_pass)
          }
          Error(e) -> {
            {
              let error_msg = loader.format_error(e)
              let response =
                json_output.failure(
                  "coverage_check_failed",
                  "coverage",
                  json.null(),
                  [json_output.error("load_error", error_msg)],
                  Some(spec_path),
                  [],
                  exit_invalid,
                )
              json_output.output(response)
            }
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("spec file path required")
        io.println("Usage: intent coverage <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description("KIRK: Coverage analysis including OWASP Top 10")
}

/// The `gaps` command - KIRK gap detection
fn kirk_gaps_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let report = gap_detector.detect_gaps(spec)
            {
              let data =
                json.object([
                  #("total_gaps", json.int(report.total_gaps)),
                  #(
                    "severity_breakdown",
                    json.object([
                      #(
                        "critical",
                        json.int(report.severity_breakdown.critical),
                      ),
                      #("high", json.int(report.severity_breakdown.high)),
                      #("medium", json.int(report.severity_breakdown.medium)),
                      #("low", json.int(report.severity_breakdown.low)),
                    ]),
                  ),
                  #(
                    "inversion_gaps",
                    json.array(report.inversion_gaps, detected_gap_to_json),
                  ),
                  #(
                    "second_order_gaps",
                    json.array(report.second_order_gaps, detected_gap_to_json),
                  ),
                  #(
                    "checklist_gaps",
                    json.array(report.checklist_gaps, detected_gap_to_json),
                  ),
                  #(
                    "coverage_gaps",
                    json.array(report.coverage_gaps, detected_gap_to_json),
                  ),
                  #(
                    "security_gaps",
                    json.array(report.security_gaps, detected_gap_to_json),
                  ),
                ])
              let next_actions = [
                json_output.next_action(
                  "intent doctor " <> spec_path,
                  "Get prioritized recommendations",
                ),
                json_output.next_action(
                  "intent improve " <> spec_path,
                  "Get improvement suggestions",
                ),
              ]
              let response =
                json_output.success(
                  "gaps_result",
                  "gaps",
                  data,
                  Some(spec_path),
                  next_actions,
                )
              json_output.output(response)
            }
            halt(exit_pass)
          }
          Error(e) -> {
            {
              let error_msg = loader.format_error(e)
              let response =
                json_output.failure(
                  "gaps_check_failed",
                  "gaps",
                  json.null(),
                  [json_output.error("load_error", error_msg)],
                  Some(spec_path),
                  [],
                  exit_invalid,
                )
              json_output.output(response)
            }
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("spec file path required")
        io.println("Usage: intent gaps <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description("KIRK: Detect gaps using mental models")
}

fn detected_gap_to_json(gap: gap_detector.Gap) -> json.Json {
  json.object([
    #("type", json.string(gap_detector.gap_type_to_string(gap.gap_type))),
    #("description", json.string(gap.description)),
    #("severity", json.string(gap_detector.severity_to_string(gap.severity))),
    #("suggestion", json.string(gap.suggestion)),
    #("mental_model", json.string(gap.mental_model)),
  ])
}

/// The `effects` command - KIRK second-order effects analysis
fn kirk_effects_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, True) {
          Ok(spec) -> {
            let report = effects_analyzer.analyze_effects(spec)

            {
              let json_output =
                effects_analyzer.effects_report_to_action_json(
                  report,
                  spec.name,
                )
              io.println(json.to_string(json_output))
            }

            halt(exit_pass)
          }
          Error(e) -> {
            {
              let error_msg = loader.format_error(e)
              let response =
                json_output.failure(
                  "effects_check_failed",
                  "effects",
                  json.null(),
                  [json_output.error("load_error", error_msg)],
                  Some(spec_path),
                  [],
                  exit_invalid,
                )
              json_output.output(response)
            }
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("spec file path required")
        io.println("Usage: intent effects <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "KIRK: Analyze second-order effects (consequence tracing)",
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output JSON for machine consumption"),
  )
}

// The `compact` command - KIRK compact format (CIN)
// DISABLED: compact_format module not available
// fn kirk_compact_command() -> glint.Command(Nil) {
//   ...
// }

// DISABLED: compact_format module not available
// fn kirk_prototext_command() -> glint.Command(Nil) {
//   ...
// }

/// The `ears` command - KIRK EARS requirements parser
fn kirk_ears_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let _mode = output_mode.Interactive

    let output_format =
      flag.get_string(input.flags, "output")
      |> result.unwrap("text")

    let output_file =
      flag.get_string(input.flags, "out")
      |> result.unwrap("")

    case input.args {
      [requirements_path, ..] -> {
        case simplifile.read(requirements_path) {
          Ok(content) -> {
            let result = ears_parser.parse(content)

            let output = case output_format {
              "cue" -> {
                let spec_name = case flag.get_string(input.flags, "name") {
                  Ok(n) -> n
                  Error(_) -> "GeneratedSpec"
                }
                ears_parser.to_cue(result, spec_name)
              }
              "json" -> {
                let behaviors = ears_parser.to_behaviors(result)
                let data =
                  json.object([
                    #(
                      "requirements",
                      json.array(result.requirements, fn(r) {
                        json.object([
                          #("id", json.string(r.id)),
                          #(
                            "pattern",
                            json.string(ears_parser.pattern_to_string(r.pattern)),
                          ),
                          #("system_shall", json.string(r.system_shall)),
                          #("raw_text", json.string(r.raw_text)),
                        ])
                      }),
                    ),
                    #(
                      "behaviors",
                      json.array(behaviors, fn(b) {
                        json.object([
                          #("name", json.string(b.name)),
                          #("intent", json.string(b.intent)),
                          #("method", json.string(b.method)),
                          #("path", json.string(b.path)),
                          #("status", json.int(b.status)),
                        ])
                      }),
                    ),
                    #(
                      "errors",
                      json.array(result.errors, fn(e) {
                        let #(message, suggestion) =
                          ears_parser.error_message(e)
                        let line = case e {
                          ears_parser.PatternNotMatched(line:, ..) -> line
                          ears_parser.PatternMatchFailed(line:, ..) -> line
                          ears_parser.RegexCompileFailed(line:, ..) -> line
                          ears_parser.ComponentExtractionFailed(line:, ..) ->
                            line
                        }
                        json.object([
                          #("line", json.int(line)),
                          #("message", json.string(message)),
                          #("suggestion", json.string(suggestion)),
                        ])
                      }),
                    ),
                    #("warnings", json.array(result.warnings, json.string)),
                  ])
                let next_actions = [
                  json_output.next_action(
                    "intent ears " <> requirements_path <> " --output cue",
                    "Generate CUE spec from requirements",
                  ),
                ]
                let response =
                  json_output.success(
                    "ears_result",
                    "ears",
                    data,
                    None,
                    next_actions,
                  )
                json.to_string(json_output.to_json(response))
              }
              _ -> ears_parser.format_result(result)
            }

            case output_file {
              "" -> {
                io.println(output)
                // Add next-step guidance for text/JSON output modes (not CUE since CUE writes to file)
                case output_format {
                  "cue" | "json" -> Nil
                  _ -> {
                    io.println("")
                    io.println("Next steps:")
                    io.println(
                      "  • intent ears "
                      <> requirements_path
                      <> " --output=cue --out=spec.cue - Generate CUE spec",
                    )
                    io.println(
                      "  • intent ears "
                      <> requirements_path
                      <> " --output=json - Machine-readable output",
                    )
                  }
                }
              }
              path -> {
                case simplifile.write(path, output) {
                  Ok(_) -> {
                    io.println("✓ Written to: " <> path)
                    // Add next-step guidance after writing CUE file
                    case output_format {
                      "cue" -> {
                        io.println("")
                        io.println("Next steps:")
                        io.println(
                          "  • intent validate "
                          <> path
                          <> " - Verify spec syntax",
                        )
                        io.println(
                          "  • intent lint "
                          <> path
                          <> " - Check for quality issues",
                        )
                        io.println(
                          "  • intent quality "
                          <> path
                          <> " - Analyze overall quality",
                        )
                        io.println(
                          "  • intent check "
                          <> path
                          <> " --target=URL - Test against API",
                        )
                      }
                      _ -> Nil
                    }
                  }
                  Error(_) -> io.println_error("Failed to write to: " <> path)
                }
              }
            }

            halt(exit_pass)
          }
          Error(_) -> {
            io.println_error("Failed to read: " <> requirements_path)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        // Return JSON usage info with exit code 0 for testability
        let error =
          json_output.error(
            "missing_arguments",
            "Requirements file path is required for ears command",
          )

        let response =
          json_output.failure(
            "ears_usage",
            "ears",
            json.object([
              #(
                "usage",
                json.string(
                  "intent ears <requirements.md> [--output text|cue|json] [--out <file>]",
                ),
              ),
              #(
                "description",
                json.string(
                  "EARS requirements parser - converts natural language to CUE behaviors",
                ),
              ),
              #(
                "patterns",
                json.object([
                  #("ubiquitous", json.string("THE SYSTEM SHALL [behavior]")),
                  #(
                    "event_driven",
                    json.string("WHEN [trigger] THE SYSTEM SHALL [behavior]"),
                  ),
                  #(
                    "state_driven",
                    json.string("WHILE [state] THE SYSTEM SHALL [behavior]"),
                  ),
                  #(
                    "optional",
                    json.string("WHERE [condition] THE SYSTEM SHALL [behavior]"),
                  ),
                  #(
                    "unwanted",
                    json.string("IF [condition] THEN THE SYSTEM SHALL NOT"),
                  ),
                ]),
              ),
            ]),
            [error],
            None,
            [
              json_output.next_action(
                "intent parse <requirements.md>",
                "Quick EARS validation",
              ),
            ],
            exit_pass,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("KIRK: Parse EARS requirements to Intent behaviors")
  |> glint.flag(
    "output",
    flag.string()
      |> flag.default("beads")
      |> flag.description("Output directory path for generated test files"),
  )
  |> glint.flag(
    "out",
    flag.string() |> flag.default("") |> flag.description("Output file path"),
  )
  |> glint.flag(
    "lang",
    flag.string()
      |> flag.default("gleam")
      |> flag.description(
        "Programming language for generated tests: gleam, python, typescript, or rust",
      ),
  )
}

// =============================================================================
// PARSE COMMAND
// =============================================================================

/// The `parse` command - parse EARS requirements to spec
fn parse_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // Check --output flag (normalize_flag_syntax converts -o to --output)
    let output_file =
      flag.get_string(input.flags, "output")
      |> result.unwrap("")

    case input.args {
      [requirements_path, ..] -> {
        case simplifile.read(requirements_path) {
          Ok(content) -> {
            let result = ears_parser.parse(content)
            let req_count = list.length(result.requirements)
            let err_count = list.length(result.errors)

            // Count by pattern type
            let #(ubiq, event, state, opt, unwant, complex) =
              list.fold(result.requirements, #(0, 0, 0, 0, 0, 0), fn(acc, r) {
                let #(u, e, s, o, w, c) = acc
                case r.pattern {
                  ears_parser.Ubiquitous -> #(u + 1, e, s, o, w, c)
                  ears_parser.EventDriven -> #(u, e + 1, s, o, w, c)
                  ears_parser.StateDriven -> #(u, e, s + 1, o, w, c)
                  ears_parser.Optional -> #(u, e, s, o + 1, w, c)
                  ears_parser.Unwanted -> #(u, e, s, o, w + 1, c)
                  ears_parser.Complex -> #(u, e, s, o, w, c + 1)
                }
              })

            {
              let behaviors = ears_parser.to_behaviors(result)
              let data =
                json.object([
                  #(
                    "requirements",
                    json.array(result.requirements, fn(r) {
                      json.object([
                        #("id", json.string(r.id)),
                        #(
                          "pattern",
                          json.string(ears_parser.pattern_to_string(r.pattern)),
                        ),
                        #("system_shall", json.string(r.system_shall)),
                        #("raw_text", json.string(r.raw_text)),
                      ])
                    }),
                  ),
                  #(
                    "behaviors",
                    json.array(behaviors, fn(b) {
                      json.object([
                        #("name", json.string(b.name)),
                        #("intent", json.string(b.intent)),
                        #("method", json.string(b.method)),
                        #("path", json.string(b.path)),
                        #("status", json.int(b.status)),
                      ])
                    }),
                  ),
                  #(
                    "errors",
                    json.array(result.errors, fn(e) {
                      let #(message, suggestion) = ears_parser.error_message(e)
                      let line = case e {
                        ears_parser.PatternNotMatched(line:, ..) -> line
                        ears_parser.PatternMatchFailed(line:, ..) -> line
                        ears_parser.RegexCompileFailed(line:, ..) -> line
                        ears_parser.ComponentExtractionFailed(line:, ..) -> line
                      }
                      json.object([
                        #("line", json.int(line)),
                        #("message", json.string(message)),
                        #("suggestion", json.string(suggestion)),
                      ])
                    }),
                  ),
                  #("warnings", json.array(result.warnings, json.string)),
                  #("count", json.int(req_count)),
                ])
              let next_actions = [
                json_output.next_action(
                  "intent parse " <> requirements_path <> " -o spec.cue",
                  "Generate CUE spec file",
                ),
                json_output.next_action(
                  "intent validate spec.cue",
                  "Validate generated spec",
                ),
              ]
              let response =
                json_output.success(
                  "parse_result",
                  "parse",
                  data,
                  // Set spec_path if output file was specified
                  case output_file {
                    "" -> None
                    path -> Some(path)
                  },
                  next_actions,
                )
              json_output.output(response)

              // Write to output file if specified
              case output_file {
                "" -> Nil
                path -> {
                  let spec_name = case string.split(path, "/") {
                    [] -> "GeneratedSpec"
                    parts ->
                      case list.last(parts) {
                        Ok(filename) ->
                          case string.split(filename, ".") {
                            [name, ..] -> name
                            [] -> "GeneratedSpec"
                          }
                        Error(_) -> "GeneratedSpec"
                      }
                  }
                  let cue_output = ears_parser.to_cue(result, spec_name)
                  case simplifile.write(path, cue_output) {
                    Ok(_) -> {
                      io.println("")
                      io.println("✓ Wrote spec to: " <> path)
                    }
                    Error(err) -> {
                      io.println_error("Failed to write spec: " <> path)
                      io.println_error(string.inspect(err))
                    }
                  }
                }
              }
            }
            io.println(
              "✓ Parsed " <> string.inspect(ubiq) <> " ubiquitous requirements",
            )
            io.println(
              "✓ Parsed "
              <> string.inspect(event)
              <> " event-driven requirements",
            )
            io.println(
              "✓ Parsed "
              <> string.inspect(state)
              <> " state-driven requirements",
            )
            io.println(
              "✓ Parsed " <> string.inspect(opt) <> " optional requirements",
            )
            io.println(
              "✓ Parsed " <> string.inspect(unwant) <> " unwanted requirements",
            )
            io.println(
              "✓ Parsed " <> string.inspect(complex) <> " complex requirements",
            )
            {
              io.println("")
              list.each(result.errors, fn(e) {
                let #(message, suggestion) = ears_parser.error_message(e)
                let line = case e {
                  ears_parser.PatternNotMatched(line:, ..) -> line
                  ears_parser.PatternMatchFailed(line:, ..) -> line
                  ears_parser.RegexCompileFailed(line:, ..) -> line
                  ears_parser.ComponentExtractionFailed(line:, ..) -> line
                }
                io.println("Error parsing requirements:")
                io.println("Line " <> string.inspect(line) <> ": " <> message)
                io.println("  ❌ Does not match any EARS pattern")
                io.println("  💡 Suggestion: " <> suggestion)
              })
              io.println("")
              io.println(
                "Parsed: " <> string.inspect(req_count) <> " requirements",
              )
              io.println(
                "Failed: " <> string.inspect(err_count) <> " requirements",
              )
            }

            case err_count > 0 {
              True -> halt(exit_fail)
              False -> halt(exit_pass)
            }
          }
          Error(_) -> {
            io.println_error("Failed to read: " <> requirements_path)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("requirements file path required")
        io.println(
          "Usage: intent parse <requirements.ears.md> [-o spec.cue] [--json]",
        )
        io.println("")
        io.println(
          "Parse EARS-formatted requirements and output structured CUE spec.",
        )
        io.println("")
        io.println("EARS Patterns:")
        io.println(
          "  THE SYSTEM SHALL [behavior]                    - Ubiquitous",
        )
        io.println(
          "  WHEN [trigger] THE SYSTEM SHALL [behavior]     - Event-Driven",
        )
        io.println(
          "  WHILE [state] THE SYSTEM SHALL [behavior]      - State-Driven",
        )
        io.println(
          "  WHERE [condition] THE SYSTEM SHALL [behavior]  - Optional",
        )
        io.println(
          "  IF [condition] THEN THE SYSTEM SHALL NOT       - Unwanted",
        )
        io.println("  WHILE [state] WHEN [trigger] THE SYSTEM SHALL  - Complex")
        io.println("")
        io.println("Examples:")
        io.println("  intent parse examples/requirements.ears.md")
        io.println("  intent parse requirements.md -o spec.cue")
        io.println("  intent parse requirements.md --json")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Parse EARS requirements to spec")
  |> glint.flag(
    "output",
    flag.string()
      |> flag.default("")
      |> flag.description("Output spec file path (-o is also supported)"),
  )
}

// =============================================================================
// ANSWER LOADER ERROR FORMATTING
// =============================================================================

// DISABLED: answer_loader module not available
// fn answer_loader_error_to_string(err: answer_loader.AnswerLoaderError) -> String {
//   case err {
//     answer_loader.FileNotFound(path) -> "File not found: " <> path
//     answer_loader.PermissionDenied(path) -> "Permission denied reading: " <> path
//     answer_loader.ParseError(path, msg) -> "Parse error in " <> path <> ": " <> msg
//     answer_loader.SchemaError(msg) -> "Schema validation failed: " <> msg
//     answer_loader.IoError(msg) -> "I/O error: " <> msg
//   }
// }

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

fn generate_uuid() -> String {
  ffi.generate_uuid()
}

fn current_timestamp() -> String {
  ffi.current_timestamp()
}

// =============================================================================
// AI COMMANDS
// =============================================================================

/// The `ai schema` command - Schema introspection for AI agents
fn ai_schema_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let command_flag = flag.get_string(input.flags, "command")
    let type_flag = flag.get_string(input.flags, "type")
    let all_flag = flag.get_bool(input.flags, "all") |> result.unwrap(False)
    let list_flag = flag.get_bool(input.flags, "list") |> result.unwrap(False)

    case all_flag, list_flag, command_flag, type_flag {
      // --all: Return all schemas
      True, _, _, _ -> {
        case ai_schema.get_all_schemas() {
          Ok(schemas) -> {
            let schemas_json =
              json.array(schemas, fn(schema) {
                json.object([
                  #("command", json.string(schema.command)),
                  #("schema_type", json.string(schema.schema_type)),
                  #("content", json.string(schema.content)),
                ])
              })

            let data =
              json.object([
                #("schemas", schemas_json),
                #("count", json.int(list.length(schemas))),
              ])

            let next_actions = [
              json_output.next_action(
                "intent validate <spec.cue>",
                "Validate a spec using these schemas",
              ),
              json_output.next_action(
                "intent ai schema --command=<cmd> --type=input",
                "Get specific schema for a command",
              ),
            ]

            let response =
              json_output.success(
                "schema_list_result",
                "ai schema",
                data,
                None,
                next_actions,
              )

            json_output.output(response)
            halt(exit_pass)
          }
          Error(ai_schema.SchemaDirectoryNotFound) -> {
            let error =
              json_output.error(
                "schema_directory_not_found",
                "Schema directory not found: schema/ai/",
              )

            let next_actions = [
              json_output.next_action("intent help", "Show available commands"),
              json_output.next_action(
                "intent interview --profile=api",
                "Start a new interview session",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          Error(_) -> {
            let error =
              json_output.error("unknown_error", "Failed to read schemas")

            let next_actions = [
              json_output.next_action(
                "intent ai schema --list",
                "List available commands",
              ),
              json_output.next_action(
                "intent help",
                "Show all available commands",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
        }
      }

      // --list: List available command names
      _, True, _, _ -> {
        case ai_schema.list_commands() {
          Ok(commands) -> {
            let data =
              json.object([
                #("commands", json.array(commands, json.string)),
                #("count", json.int(list.length(commands))),
              ])

            let next_actions = [
              json_output.next_action(
                "intent ai schema --all",
                "List all available schemas",
              ),
              json_output.next_action(
                "intent ai schema --command=<cmd> --type=input",
                "Get specific schema for a command",
              ),
            ]

            let response =
              json_output.success(
                "schema_commands_result",
                "ai schema",
                data,
                None,
                next_actions,
              )

            json_output.output(response)
            halt(exit_pass)
          }
          Error(_) -> {
            let error =
              json_output.error(
                "failed_to_list_commands",
                "Failed to list available commands",
              )

            let next_actions = [
              json_output.next_action("intent help", "Show available commands"),
              json_output.next_action(
                "intent ai schema --all",
                "Try listing all schemas",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
        }
      }

      // --command=X --type=Y: Get specific schema
      _, _, Ok(cmd), Ok(typ) -> {
        case ai_schema.get_schema(command: cmd, schema_type: typ) {
          Ok(content) -> {
            let data =
              json.object([
                #("command", json.string(cmd)),
                #("schema_type", json.string(typ)),
                #("content", json.string(content)),
              ])

            let next_actions = [
              json_output.next_action(
                "intent validate <spec.cue>",
                "Validate a spec using this schema",
              ),
              json_output.next_action(
                "intent help " <> cmd,
                "Get help for " <> cmd <> " command",
              ),
            ]

            let response =
              json_output.success(
                "schema_result",
                "ai schema",
                data,
                None,
                next_actions,
              )

            json_output.output(response)
            halt(exit_pass)
          }
          Error(ai_schema.SchemaNotFound(command)) -> {
            let error =
              json_output.detailed_error(
                "schema_not_found",
                "Schema not found for command: " <> command,
                "schema/ai/" <> typ <> "/" <> command <> ".cue",
                "Check available schemas with: intent ai schema --all",
                "intent ai schema --all",
              )

            let data = json.object([#("command", json.string(command))])

            let next_actions = [
              json_output.next_action(
                "intent ai schema --all",
                "List all available schemas",
              ),
              json_output.next_action(
                "intent ai schema --list",
                "List available command names",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                data,
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          Error(ai_schema.InvalidSchemaType(schema_type)) -> {
            let error =
              json_output.detailed_error(
                "invalid_schema_type",
                "Invalid schema type: "
                  <> schema_type
                  <> ". Must be 'input' or 'output'",
                "command line",
                "Use --type=input or --type=output",
                "intent ai schema --command=" <> cmd <> " --type=input",
              )

            let data = json.object([#("schema_type", json.string(schema_type))])

            let next_actions = [
              json_output.next_action(
                "intent ai schema --command=" <> cmd <> " --type=input",
                "Try with input schema type",
              ),
              json_output.next_action(
                "intent ai schema --command=" <> cmd <> " --type=output",
                "Try with output schema type",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                data,
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          Error(_) -> {
            let error =
              json_output.error("unknown_error", "Failed to read schema")

            let next_actions = [
              json_output.next_action(
                "intent ai schema --all",
                "List all available schemas",
              ),
              json_output.next_action(
                "intent ai schema --list",
                "List available commands",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
        }
      }

      // Missing required flags
      _, _, _, _ -> {
        let error =
          json_output.detailed_error(
            "missing_flags",
            "Must specify either --all, --list, or both --command and --type",
            "command line",
            "Use --all to list all schemas, --list for commands, or --command=X --type=Y for specific schema",
            "intent ai schema --all",
          )

        let next_actions = [
          json_output.next_action(
            "intent ai schema --all",
            "List all available schemas",
          ),
          json_output.next_action(
            "intent ai schema --list",
            "List available command names",
          ),
          json_output.next_action("intent help", "Show command help"),
        ]

        let response =
          json_output.failure(
            "schema_error",
            "ai schema",
            json.object([]),
            [error],
            None,
            next_actions,
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Schema introspection for AI agents - get CUE schemas for command input/output",
  )
  |> glint.flag(
    "command",
    flag.string() |> flag.description("Command name (e.g., 'quality')"),
  )
  |> glint.flag(
    "type",
    flag.string() |> flag.description("Schema type: 'input' or 'output'"),
  )
  |> glint.flag(
    "all",
    flag.bool()
      |> flag.default(False)
      |> flag.description("List all available schemas"),
  )
  |> glint.flag(
    "list",
    flag.bool()
      |> flag.default(False)
      |> flag.description("List all available command names"),
  )
}

/// The `ai aggregate` command - Aggregate analysis from multiple specs
fn ai_aggregate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [] -> {
        let error =
          json_output.error(
            "missing_spec_paths",
            "At least one spec file path is required",
          )

        let next_actions = [
          json_output.next_action(
            "intent validate <spec.cue>",
            "Validate a spec file first",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "aggregate_error",
            "ai aggregate",
            json.object([]),
            [error],
            None,
            next_actions,
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
      spec_paths -> {
        // Load all specs
        let loaded_specs =
          spec_paths
          |> list.map(fn(path) {
            case loader.load_spec_quiet(path) {
              Ok(spec) ->
                Ok(spec_aggregator.SpecWithPath(path: path, spec: spec))
              Error(e) -> Error(#(path, e))
            }
          })

        // Separate successes from failures
        let #(specs, errors) =
          loaded_specs
          |> list.fold(#([], []), fn(acc, result) {
            let #(specs_acc, errors_acc) = acc
            case result {
              Ok(spec_with_path) -> #([spec_with_path, ..specs_acc], errors_acc)
              Error(#(path, err)) -> #(specs_acc, [#(path, err), ..errors_acc])
            }
          })

        // If any specs failed to load, report errors
        case errors != [] {
          True -> {
            let error_messages =
              errors
              |> list.map(fn(error_pair) {
                let #(path, err) = error_pair
                json_output.error(
                  "spec_load_error",
                  "Failed to load " <> path <> ": " <> loader.format_error(err),
                )
              })

            let next_actions = [
              json_output.next_action(
                "intent validate <spec.cue>",
                "Validate individual spec files",
              ),
              json_output.next_action(
                "intent quality <spec.cue>",
                "Analyze spec quality",
              ),
            ]

            let response =
              json_output.failure(
                "aggregate_error",
                "ai aggregate",
                json.object([]),
                error_messages,
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          False -> {
            // All specs loaded successfully, perform aggregation
            let report = spec_aggregator.aggregate_specs(list.reverse(specs))

            let next_actions = [
              json_output.next_action(
                "intent quality <merged-spec.cue>",
                "Analyze quality of aggregated insights",
              ),
              json_output.next_action(
                "intent gaps <merged-spec.cue>",
                "Find coverage gaps",
              ),
              json_output.next_action(
                "intent beads <session-id>",
                "Generate implementation work items",
              ),
            ]

            let response =
              json_output.success(
                "ai_aggregate_result",
                "ai aggregate",
                spec_aggregator.to_json(report),
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
  |> glint.description(
    "Aggregate analysis from multiple specs to find common patterns, duplicates, and conflicts",
  )
}

// =============================================================================
// Shape Phase Commands
// =============================================================================

/// Start a new shape session
fn shape_start_command() -> glint.Command(Nil) {
  glint.command(fn(_input: glint.CommandInput) {
    let session_id = ffi.generate_uuid()
    let timestamp = ffi.current_timestamp()

    let data =
      json.object([
        #("session_id", json.string(session_id)),
        #("phase", json.string("shape")),
        #("status", json.string("in_progress")),
        #("created_at", json.string(timestamp)),
      ])

    let next_actions = [
      json_output.next_action(
        "intent shape check --session=" <> session_id,
        "Check session status",
      ),
      json_output.next_action(
        "intent shape critique --session=" <> session_id,
        "Run critique on shape session",
      ),
    ]

    let response =
      json_output.success(
        "shape_start_result",
        "shape start",
        data,
        None,
        next_actions,
      )

    json_output.output(response)
    halt(exit_pass)
  })
  |> glint.description("Start a new shape phase session")
}

/// Check shape session status
fn shape_check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_check_error",
            "shape check",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("status", json.string("in_progress")),
            #("answered_count", json.int(0)),
            #("gaps", json.array([], fn(x) { json.string(x) })),
          ])

        let next_actions = [
          json_output.next_action(
            "intent shape critique --session=" <> session_id,
            "Run critique on shape session",
          ),
        ]

        let response =
          json_output.success(
            "shape_check_result",
            "shape check",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Check shape session status")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to check"),
  )
}

/// Run shape critique
fn shape_critique_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_critique_error",
            "shape critique",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("passed", json.bool(False)),
            #("score", json.int(0)),
            #("issues", json.array([], fn(x) { json.string(x) })),
          ])

        let next_actions = [
          json_output.next_action(
            "intent shape respond --session="
              <> session_id
              <> " --question=<qid> --answer='...'",
            "Respond to critique issues",
          ),
          json_output.next_action(
            "intent shape agree --session=" <> session_id,
            "Finalize shape if critique passed",
          ),
        ]

        let response =
          json_output.success(
            "shape_critique_result",
            "shape critique",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Run Pragmatic Tech Lead critique on shape session")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to critique"),
  )
}

/// Submit answer to shape question
fn shape_respond_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")
    let question_id =
      flag.get_string(input.flags, "question") |> result.unwrap("")
    let answer = flag.get_string(input.flags, "answer") |> result.unwrap("")

    case session_id, question_id, answer {
      "", _, _ | _, "", _ | _, _, "" -> {
        let error =
          json_output.error(
            "missing_required_fields",
            "session, question, and answer are required",
          )

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_respond_error",
            "shape respond",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _, _, _ -> {
        let timestamp = ffi.current_timestamp()

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("question_id", json.string(question_id)),
            #("answered", json.bool(True)),
            #("timestamp", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent shape critique --session=" <> session_id,
            "Re-run critique to check progress",
          ),
        ]

        let response =
          json_output.success(
            "shape_respond_result",
            "shape respond",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Submit answer to a shape question")
  |> glint.flag("session", flag.string() |> flag.description("Session ID"))
  |> glint.flag(
    "question",
    flag.string() |> flag.description("Question ID to answer"),
  )
  |> glint.flag("answer", flag.string() |> flag.description("Answer text"))
}

/// Finalize shape session
fn shape_agree_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_agree_error",
            "shape agree",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let timestamp = ffi.current_timestamp()

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("status", json.string("complete")),
            #("finalized_at", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent spec start --vision-session=<vision-id> --shape-session="
              <> session_id,
            "Start Spec phase with this shape session",
          ),
          json_output.next_action(
            "intent beads <session-id>",
            "Generate implementation beads",
          ),
        ]

        let response =
          json_output.success(
            "shape_agree_result",
            "shape agree",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Finalize and complete shape session")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to finalize"),
  )
}
