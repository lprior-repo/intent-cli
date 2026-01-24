/// Intent CLI - Human-writes, AI-verifies, AI-implements
/// Contract-driven API testing tool
import argv
import gleam/dict
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/bead_feedback
import intent/bead_templates
import intent/cli_ui
import intent/doctor
import intent/help
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
import intent/output
import intent/output_mode
import intent/plan_mode
import intent/quality_analyzer
import intent/question_types.{type Question}
import intent/runner
import intent/security
import intent/spec_builder
import intent/spec_linter
import intent/stdin
import intent/types
import simplifile

/// Exit codes
const exit_pass = 0

const exit_fail = 1

const exit_blocked = 2

const exit_invalid = 3

const exit_error = 4

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
pub fn normalize_flag_syntax(args: List(String)) -> List(String) {
  do_normalize(args)
}

fn do_normalize(args: List(String)) -> List(String) {
  case args {
    // Empty list
    [] -> []

    // Single argument
    [arg] -> {
      case string.starts_with(arg, "--") {
        True -> [arg]
        // Boolean flag or flag with equals
        False -> [arg]
        // Positional argument
      }
    }

    // Two or more arguments
    [first, second, ..rest] -> {
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
              case string.starts_with(second, "--") {
                True -> {
                  // Second is also a flag (first is boolean)
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

pub fn main() {
  let raw_args = argv.load().arguments
  let normalized_args = normalize_flag_syntax(raw_args)

  let _app =
    glint.new()
    |> glint.with_name("intent")
    |> glint.with_pretty_help(glint.default_pretty_help())
    |> glint.add(at: ["check"], do: check_command())
    |> glint.add(at: ["validate"], do: validate_command())
    |> glint.add(at: ["show"], do: show_command())
    |> glint.add(at: ["export"], do: export_command())
    |> glint.add(at: ["lint"], do: lint_command())
    |> glint.add(at: ["analyze"], do: analyze_command())
    |> glint.add(at: ["improve"], do: improve_command())
    |> glint.add(at: ["doctor"], do: doctor_command())
    |> glint.add(at: ["interview"], do: interview_command())
    |> glint.add(at: ["beads"], do: beads_command())
    |> glint.add(at: ["bead-status"], do: bead_status_command())
    |> glint.add(at: ["history"], do: history_command())
    |> glint.add(at: ["diff"], do: diff_command())
    |> glint.add(at: ["sessions"], do: sessions_command())
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
    // Context scanning
    // TODO: Re-enable when context_scan_command is implemented
    // |> glint.add(at: ["context-scan"], do: context_scan_command())
    |> glint.run(normalized_args)
}

/// The `check` command - run spec against a target
fn check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let target_url =
      flag.get_string(input.flags, "target")
      |> result.unwrap("")

    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)
      || {
        flag.get_bool(input.flags, "json-out")
        |> result.unwrap(False)
      }
    let feature_filter =
      flag.get_string(input.flags, "feature")
      |> result.unwrap("")

    let only_filter =
      flag.get_string(input.flags, "only")
      |> result.unwrap("")

    let output_level = case flag.get_bool(input.flags, "verbose") {
      Ok(True) -> runner.Verbose
      _ ->
        case flag.get_bool(input.flags, "quiet") {
          Ok(True) -> runner.Quiet
          _ -> runner.Normal
        }
    }

    let allow_localhost =
      flag.get_bool(input.flags, "allow-localhost")
      |> result.unwrap(False)
      || is_localhost_allowed_by_env()

    // Parse timeout flag - None means use spec config, Some(ms) overrides
    let timeout_ms: Option(Int) = case flag.get_int(input.flags, "timeout") {
      Ok(t) if t > 0 -> Some(t)
      _ -> None
    }

    case input.args {
      [spec_path, ..] -> {
        run_check(
          spec_path,
          target_url,
          is_json,
          feature_filter,
          only_filter,
          output_level,
          allow_localhost,
          timeout_ms,
        )
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent check <spec.cue> --target <url>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.check_help()))
  |> glint.flag(
    "target",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Required. Base URL of target API (e.g., https://api.example.com)",
      ),
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Output results in JSON format for programmatic parsing",
      ),
  )
  |> glint.flag(
    "feature",
    flag.string()
      |> flag.default("")
      |> flag.description("Filter execution to specific feature by name"),
  )
  |> glint.flag(
    "since",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Compare from specific timestamp or version (e.g., '2h ago', 'v1.2.0')",
      ),
  )
  |> glint.flag(
    "verbose",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Show detailed execution logs and request/response data",
      ),
  )
  |> glint.flag(
    "quiet",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Minimal output: show only errors and final result"),
  )
  |> glint.flag(
    "allow-localhost",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Permit localhost URLs for local development (bypasses SSRF protection)",
      ),
  )
  |> glint.flag(
    "timeout",
    flag.int()
      |> flag.default(0)
      |> flag.description(
        "HTTP request timeout in milliseconds (overrides spec config, default: 30000ms)",
      ),
  )
}

fn run_check(
  spec_path: String,
  target_url: String,
  is_json: Bool,
  feature_filter: String,
  only_filter: String,
  output_level: runner.OutputLevel,
  allow_localhost: Bool,
  timeout_ms: Option(Int),
) -> Nil {
  // Determine output mode based on --json flag
  let mode = output_mode.from_json_flag(is_json)

  // Validate target URL is provided
  case string.is_empty(target_url) {
    True -> {
      cli_ui.print_error("--target URL is required", mode)
      io.println("Usage: intent check <spec.cue> --target=<url>")
      halt(exit_error)
    }
    False -> Nil
  }

  // Validate target URL for SSRF protection
  case security.validate_url(target_url, allow_localhost) {
    Error(e) -> {
      cli_ui.print_error(security.format_security_error(e), mode)
      halt(exit_error)
    }
    Ok(_) -> Nil
  }

  // Load the spec
  case loader.load_spec(spec_path) {
    Error(e) -> {
      cli_ui.print_error(loader.format_error(e), mode)
      halt(exit_invalid)
    }
    Ok(spec) -> {
      cli_ui.print_header("Checking spec: " <> spec.name, mode)

      // Build run options with timeout override
      let options =
        runner.RunOptions(
          feature_filter: case feature_filter {
            "" -> None
            f -> Some(f)
          },
          behavior_filter: case only_filter {
            "" -> None
            b -> Some(b)
          },
          output_level: output_level,
          timeout_ms: timeout_ms,
        )

      // Run the spec
      let result = runner.run_spec(spec, target_url, options, mode)

      // Output results
      case is_json {
        True -> {
          let json_result = output.spec_result_to_action_json(result, spec_path)
          io.println(json.to_string(json_result))
        }
        False -> {
          io.println(output.spec_result_to_text(result))
        }
      }

      // Exit with appropriate code
      let exit_code = case result {
        output.SpecResult(pass: True, ..) -> {
          cli_ui.print_success("All checks passed!", mode)
          exit_pass
        }
        output.SpecResult(blocked: blocked, ..) if blocked > 0 -> {
          cli_ui.print_warning("Blocked behaviors detected", mode)
          exit_blocked
        }
        _ -> {
          cli_ui.print_error("Check failed", mode)
          exit_fail
        }
      }
      halt(exit_code)
    }
  }
}

/// The `validate` command - validate CUE spec syntax AND structure
fn validate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let mode = output_mode.Interactive

    case input.args {
      [spec_path, ..] -> {
        // Use load_spec_quiet to validate both CUE syntax AND spec structure
        case loader.load_spec_quiet(spec_path) {
          Ok(_) -> {
            cli_ui.print_success("Valid spec: " <> spec_path, mode)
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error("Invalid spec: " <> loader.format_error(e), mode)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent validate <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.validate_help()))
}

/// The `show` command - pretty print a parsed spec
fn show_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    case input.args {
      [spec_path, ..] -> {
        case is_json {
          True ->
            case
              loader.export_spec_json(spec_path, loader.default_cue_exporter)
            {
              Ok(json_str) -> {
                io.println(json_str)
                halt(exit_pass)
              }
              Error(e) -> {
                io.println_error("Error: " <> loader.format_error(e))
                halt(exit_error)
              }
            }
          False -> {
            case loader.load_spec(spec_path) {
              Ok(spec) -> {
                print_spec_summary(spec)
                halt(exit_pass)
              }
              Error(e) -> {
                io.println_error("Error: " <> loader.format_error(e))
                halt(exit_error)
              }
            }
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent show <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.show_help()))
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
  )
}

fn print_spec_summary(spec: types.Spec) -> Nil {
  io.println("Spec: " <> spec.name)
  io.println("Version: " <> spec.version)
  io.println("")
  io.println("Description:")
  io.println(spec.description)
  io.println("")

  case spec.audience {
    "" -> Nil
    audience -> {
      io.println("Audience: " <> audience)
      io.println("")
    }
  }

  case spec.success_criteria {
    [] -> Nil
    criteria -> {
      io.println("Success Criteria:")
      list.each(criteria, fn(c) { io.println("  - " <> c) })
      io.println("")
    }
  }

  io.println("Features:")
  list.each(spec.features, fn(feature) {
    io.println("  " <> feature.name)
    io.println("    " <> feature.description)
    io.println(
      "    Behaviors: " <> string.inspect(list.length(feature.behaviors)),
    )
    list.each(feature.behaviors, fn(b) {
      io.println("      - " <> b.name <> ": " <> b.intent)
    })
  })

  case spec.rules {
    [] -> Nil
    rules -> {
      io.println("")
      io.println("Global Rules:")
      list.each(rules, fn(rule) {
        io.println("  - " <> rule.name <> ": " <> rule.description)
      })
    }
  }

  case spec.anti_patterns {
    [] -> Nil
    patterns -> {
      io.println("")
      io.println("Anti-Patterns:")
      list.each(patterns, fn(p) {
        io.println("  - " <> p.name <> ": " <> p.description)
      })
    }
  }

  Nil
}

/// The `export` command - export spec to JSON
fn export_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.export_spec_json(spec_path, loader.default_cue_exporter) {
          Ok(json_str) -> {
            io.println(json_str)
            halt(exit_pass)
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            halt(exit_error)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent export <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.export_help()))
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
                io.println("✓ Spec is well-formed - no linting issues found")
                halt(exit_pass)
              }
              spec_linter.LintWarnings(warnings) -> {
                io.println(spec_linter.format_warnings(warnings))
                halt(exit_fail)
              }
            }
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent lint <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.lint_help()))
}

/// The `analyze` command - analyze spec quality
fn analyze_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = quality_analyzer.analyze_spec(spec)
            io.println(quality_analyzer.format_report(report))
            halt(exit_pass)
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent analyze <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.analyze_help()))
}

/// The `improve` command - suggest improvements
fn improve_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
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
            io.println(improver.format_improvements(suggestions))
            halt(exit_pass)
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent improve <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.improve_help()))
}

/// The `doctor` command - health report with prioritized improvements
fn doctor_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = doctor.run_doctor(spec)
            case is_json {
              True -> doctor.json_output(report, spec_path)
              False -> doctor.print_report(report, output_mode.Interactive)
            }
            halt(exit_pass)
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent doctor <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.doctor_help()))
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
  )
}

/// The `interview` command - guided specification discovery
fn interview_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let profile_str =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("api")

    let resume_id =
      flag.get_string(input.flags, "resume")
      |> result.unwrap("")

    let export_to =
      flag.get_string(input.flags, "export")
      |> result.unwrap("")

    let answers_file =
      flag.get_string(input.flags, "answers")
      |> result.unwrap("")

    let strict_mode =
      flag.get_bool(input.flags, "strict")
      |> result.unwrap(False)

    let cue_mode =
      flag.get_bool(input.flags, "cue")
      |> result.unwrap(False)

    let session_flag =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    let answer_text =
      flag.get_string(input.flags, "answer")
      |> result.unwrap("")

    let dry_run =
      flag.get_bool(input.flags, "dry-run")
      |> result.unwrap(False)

    // CUE mode: output CUE directives for AI agents
    case cue_mode {
      True -> {
        // Check if this is answering a question or starting/resuming
        let has_resume = !string.is_empty(resume_id)
        let has_session = !string.is_empty(session_flag)
        let has_answer = !string.is_empty(answer_text)

        case has_resume, has_session, has_answer {
          // Resume session in CUE mode (--resume takes precedence)
          True, _, _ -> run_interview_cue_resume(resume_id, dry_run)
          // Submitting an answer to an existing session
          False, True, True ->
            run_interview_cue_answer(session_flag, answer_text, dry_run)
          // Start new session in CUE mode
          False, False, False -> {
            let profile = parse_profile(profile_str)
            case profile {
              Ok(p) -> run_interview_cue_start(p, dry_run)
              Error(msg) -> {
                output_cue_error(msg)
                halt(exit_error)
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
      }
      False -> {
        // Regular interactive mode
        case resume_id {
          "" ->
            case string.lowercase(profile_str) {
              "api" ->
                run_interview(
                  interview.Api,
                  answers_file,
                  strict_mode,
                  export_to,
                )
              "cli" ->
                run_interview(
                  interview.Cli,
                  answers_file,
                  strict_mode,
                  export_to,
                )
              "event" ->
                run_interview(
                  interview.Event,
                  answers_file,
                  strict_mode,
                  export_to,
                )
              "data" ->
                run_interview(
                  interview.Data,
                  answers_file,
                  strict_mode,
                  export_to,
                )
              "workflow" ->
                run_interview(
                  interview.Workflow,
                  answers_file,
                  strict_mode,
                  export_to,
                )
              "ui" ->
                run_interview(
                  interview.UI,
                  answers_file,
                  strict_mode,
                  export_to,
                )
              _ -> {
                io.println_error(
                  "Error: unknown profile '" <> profile_str <> "'",
                )
                io.println_error(
                  "Valid profiles: api, cli, event, data, workflow, ui",
                )
                halt(exit_error)
              }
            }
          id -> run_resume_interview(id, export_to)
        }
      }
    }
  })
  |> glint.description(help.format_for_glint(help.interview_help()))
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("api")
      |> flag.description(
        "System profile type: api, cli, event, data, workflow, or ui (default: api)",
      ),
  )
  |> glint.flag(
    "resume",
    flag.string()
      |> flag.default("")
      |> flag.description("Resume existing interview session using its ID"),
  )
  |> glint.flag(
    "answers",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Path to file with pre-filled answers for non-interactive batch mode",
      ),
  )
  |> glint.flag(
    "strict",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Strict validation: fail if required answers are missing (requires --answers file)",
      ),
  )
  |> glint.flag(
    "export",
    flag.string()
      |> flag.default("")
      |> flag.description("Output file path to save completed specification"),
  )
  |> glint.flag(
    "cue",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Machine-readable mode: output CUE directives for integration with AI agents",
      ),
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
}

fn run_interview(
  profile: interview.Profile,
  answers_file: String,
  strict_mode: Bool,
  export_to: String,
) -> Nil {
  // Initialize session
  let session_id = "interview-" <> generate_uuid()
  let timestamp = current_timestamp()

  let session = interview.create_session(session_id, profile, timestamp)

  // Load answers from file if provided
  // DISABLED: answer_loader module not available
  let _answers_dict = option.None
  let _ = answers_file
  // suppress unused warning
  let _ = strict_mode
  // suppress unused warning

  // Print welcome message
  io.println("")
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("                    INTENT INTERVIEW")
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("")
  io.println("Profile: " <> profile_to_display_string(profile))
  io.println("Session: " <> session_id)
  io.println("")
  io.println("This guided interview will help us discover and refine your")
  io.println("specification through structured questioning.")
  io.println("")
  io.println("We'll ask questions across 5 rounds × multiple perspectives:")
  io.println("  • Round 1: Core Intent (what are you building?)")
  io.println("  • Round 2: Scope & Boundaries (what's in/out?)")
  io.println("  • Round 3: Error Cases (what can go wrong?)")
  io.println("  • Round 4: Security & Compliance (how do we keep it safe?)")
  io.println("  • Round 5: Operations (how does it run in production?)")
  io.println("")
  io.println("Press Ctrl+C to save and exit at any time.")
  io.println("Session will be saved to: .interview/sessions.jsonl")
  io.println("")
  io.println("Ready? Let's begin.")
  io.println("")

  // Run the interview loop
  let final_session = interview_loop(session, 1)

  // Save session to JSONL
  let save_result =
    interview_storage.append_session_to_jsonl(
      final_session,
      ".interview/sessions.jsonl",
    )

  case save_result {
    Ok(Nil) -> {
      io.println("")
      io.println("✓ Session saved: " <> session_id)
    }
    Error(err) -> {
      io.println_error("✗ Failed to save session: " <> err)
    }
  }

  // Export to spec if requested
  case export_to {
    "" -> Nil
    path -> {
      let spec_cue = spec_builder.build_spec_from_session(final_session)
      case simplifile.write(path, spec_cue) {
        Ok(Nil) -> {
          io.println("✓ Spec exported to: " <> path)
        }
        Error(err) -> {
          io.println_error("✗ Failed to export spec: " <> string.inspect(err))
        }
      }
    }
  }

  halt(exit_pass)
}

/// Resume an existing interview session
fn run_resume_interview(session_id: String, export_to: String) -> Nil {
  let jsonl_path = ".interview/sessions.jsonl"
  let mode = output_mode.Interactive

  // Load the session from JSONL
  case interview_storage.get_session_from_jsonl(jsonl_path, session_id) {
    Error(err) -> {
      cli_ui.print_error(err, mode)
      halt(exit_error)
    }
    Ok(session) -> {
      cli_ui.print_header("Resuming Interview: " <> session.id, mode)
      cli_ui.print_info(
        "Profile: " <> profile_to_display_string(session.profile),
        mode,
      )
      io.println("")

      // Show progress
      io.println("Progress:")
      io.println(
        "  • Answers collected: "
        <> string.inspect(list.length(session.answers)),
      )
      io.println(
        "  • Gaps detected: " <> string.inspect(list.length(session.gaps)),
      )
      io.println(
        "  • Conflicts detected: "
        <> string.inspect(list.length(session.conflicts)),
      )
      io.println("")

      // Determine which round to resume from
      let next_round = case session.rounds_completed {
        0 -> 1
        r if r < 5 -> r + 1
        _ -> 5
      }

      io.println("Resuming from Round " <> string.inspect(next_round))
      io.println("")

      // Continue the interview from the next round
      let final_session = interview_loop(session, next_round)

      // Save updated session
      let save_result =
        interview_storage.append_session_to_jsonl(final_session, jsonl_path)

      case save_result {
        Ok(Nil) -> {
          io.println("")
          cli_ui.print_success("Session updated: " <> session.id, mode)
        }
        Error(err) -> {
          cli_ui.print_error("Failed to save session: " <> err, mode)
        }
      }

      // Export to spec if requested
      case export_to {
        "" -> Nil
        path -> {
          let spec_cue = spec_builder.build_spec_from_session(final_session)
          case simplifile.write(path, spec_cue) {
            Ok(Nil) -> {
              cli_ui.print_success("Spec exported to: " <> path, mode)
            }
            Error(err) -> {
              cli_ui.print_error(
                "Failed to export spec: " <> string.inspect(err),
                mode,
              )
            }
          }
        }
      }

      halt(exit_pass)
    }
  }
}

/// Main interview loop - asks questions round by round
fn interview_loop(
  session: interview.InterviewSession,
  round: Int,
) -> interview.InterviewSession {
  case round > 5 {
    True -> session
    False -> {
      io.println("")
      io.println(
        "═══════════════════════════════════════════════════════════════════",
      )
      io.println("ROUND " <> string.inspect(round) <> "/5")
      io.println(
        "═══════════════════════════════════════════════════════════════════",
      )
      io.println("")

      // Get questions for this round
      case interview.get_first_question_for_round(session, round) {
        Error(_) -> {
          io.println("(No questions for this round)")
          interview_loop(session, round + 1)
        }
        Ok(first_question) -> {
          // Ask all questions in this round
          let updated_session =
            ask_questions_in_round(session, round, first_question)

          // Check for blocking gaps before proceeding
          let blocking_gaps = interview.get_blocking_gaps(updated_session)
          case blocking_gaps {
            [] -> interview_loop(updated_session, round + 1)
            gaps -> {
              io.println("")
              io.println("⚠️ BLOCKING GAPS DETECTED:")
              list.each(gaps, fn(gap) {
                io.println("  • " <> gap.description)
                io.println("    " <> gap.why_needed)
              })
              io.println("")
              interview_loop(updated_session, round + 1)
            }
          }
        }
      }
    }
  }
}

/// Ask all unanswered questions in a round
fn ask_questions_in_round(
  session: interview.InterviewSession,
  round: Int,
  _current_question: Question,
) -> interview.InterviewSession {
  let profile_str = profile_to_string(session.profile)

  // Get all questions for this round
  let questions =
    interview_questions.get_questions_for_round(profile_str, round)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  // Filter to unanswered questions
  let unanswered =
    list.filter(questions, fn(q) { !list.contains(answered_ids, q.id) })

  // Ask each unanswered question
  list.fold(unanswered, session, fn(sess, question) {
    ask_single_question(sess, question, round)
  })
}

/// Ask a single question and collect answer
fn ask_single_question(
  session: interview.InterviewSession,
  question: Question,
  round: Int,
) -> interview.InterviewSession {
  io.println("")
  io.print("Q" <> string.inspect(question.priority) <> ": ")
  io.println(question.question)

  case string.length(question.context) > 0 {
    True -> io.println("   Context: " <> question.context)
    False -> Nil
  }

  case string.length(question.example) > 0 {
    True -> io.println("   Example: " <> question.example)
    False -> Nil
  }

  io.print("")

  // Read answer from stdin with validation
  let answer_text = case stdin.prompt_for_answer("> ") {
    Ok(text) -> text
    Error(err) -> {
      io.println_error("Error reading input: " <> err)
      io.println("")
      // Return placeholder if input fails
      "(input error - please try again)"
    }
  }

  // Extract fields from answer
  let extracted =
    interview.extract_from_answer(
      question.id,
      answer_text,
      question.extract_into,
    )

  // Calculate confidence
  let confidence =
    interview.calculate_confidence(question.id, answer_text, extracted)

  // Create answer record
  let answer =
    interview.Answer(
      question_id: question.id,
      question_text: question.question,
      perspective: question.perspective,
      round: round,
      response: answer_text,
      extracted: extracted,
      confidence: confidence,
      notes: "",
      timestamp: current_timestamp(),
    )

  // Add to session
  let updated_session = interview.add_answer(session, answer)

  // Check for gaps and conflicts
  let #(sess_with_gaps, _gaps) =
    interview.check_for_gaps(updated_session, question, answer)

  let #(sess_final, _conflicts) =
    interview.check_for_conflicts(sess_with_gaps, answer)

  sess_final
}

/// Helper: convert Profile to string for questions module
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

fn profile_to_display_string(profile: interview.Profile) -> String {
  case profile {
    interview.Api -> "API"
    interview.Cli -> "CLI"
    interview.Event -> "Event System"
    interview.Data -> "Data System"
    interview.Workflow -> "Workflow"
    interview.UI -> "User Interface"
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
    False ->
      interview_storage.append_session_to_jsonl(
        session,
        ".interview/sessions.jsonl",
      )
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

  case
    interview_storage.get_session_from_jsonl(
      ".interview/sessions.jsonl",
      session_id,
    )
  {
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

/// Submit an answer to a session in CUE mode
fn run_interview_cue_answer(
  session_id: String,
  answer_text: String,
  dry_run: Bool,
) -> Nil {
  let is_dry_run_session = string.starts_with(session_id, "dry-run-")

  case
    interview_storage.get_session_from_jsonl(
      ".interview/sessions.jsonl",
      session_id,
    )
  {
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
                    ".interview/sessions.jsonl",
                  )
              }

              case save_result {
                Error(err) -> {
                  output_cue_error("Failed to save session: " <> err)
                  halt(exit_error)
                }
                Ok(_) -> {
                  // Get next question or complete
                  case get_next_unanswered_question(sess_final, next_round) {
                    Some(next_q) ->
                      output_cue_question(sess_final, next_q, next_round)
                    None -> {
                      // Round is complete - increment rounds_completed
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
                            ".interview/sessions.jsonl",
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
    False -> ".interview/spec-" <> session.id <> ".cue"
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
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let max_items =
      flag.get_int(input.flags, "max-items")
      |> result.unwrap(list_limits.default_max_items)

    case input.args {
      [session_id, ..] -> {
        // Load session from JSONL
        case
          interview_storage.get_session_from_jsonl(
            ".interview/sessions.jsonl",
            session_id,
          )
        {
          Error(err) -> {
            io.println_error("Error: " <> err)
            halt(exit_error)
          }
          Ok(session) -> {
            // Generate beads from session
            let all_beads = bead_templates.generate_beads_from_session(session)
            let total_count = list.length(all_beads)

            // Apply max-items limit for output (AI guardrail)
            let beads = list_limits.apply_limit(all_beads, max_items)
            let bead_count = list.length(beads)
            let was_limited = total_count > bead_count

            // Export to .beads/issues.jsonl (all beads, not limited)
            let jsonl_output = bead_templates.beads_to_jsonl(all_beads)

            case
              simplifile.append(".beads/issues.jsonl", jsonl_output <> "\n")
            {
              Ok(Nil) -> {
                case is_json {
                  True -> {
                    // Output JSON for AI agents (limited)
                    let json_output =
                      bead_templates.beads_to_action_json(beads, session_id)
                    io.println(json.to_string(json_output))
                  }
                  False -> {
                    // Human-readable output
                    io.println("")
                    io.println(
                      "═══════════════════════════════════════════════════════════════════",
                    )
                    io.println("                    BEAD GENERATION")
                    io.println(
                      "═══════════════════════════════════════════════════════════════════",
                    )
                    io.println("")
                    io.println(
                      "Generated "
                      <> string.inspect(total_count)
                      <> " work items from session: "
                      <> session_id,
                    )
                    case was_limited {
                      True ->
                        io.println(
                          "(showing first "
                          <> string.inspect(bead_count)
                          <> " of "
                          <> string.inspect(total_count)
                          <> ")",
                        )
                      False -> Nil
                    }
                    io.println("")
                    io.println("✓ Beads exported to: .beads/issues.jsonl")
                    io.println("")

                    // Show stats
                    let stats = bead_templates.bead_stats(all_beads)
                    io.println("Summary:")
                    io.println("  Total beads: " <> string.inspect(stats.total))
                  }
                }

                halt(exit_pass)
              }
              Error(err) -> {
                io.println_error(
                  "✗ Failed to write beads: " <> string.inspect(err),
                )
                halt(exit_error)
              }
            }
          }
        }
      }
      [] -> {
        io.println_error(
          "Usage: intent beads <session_id> [--json] [--max-items N]",
        )
        io.println_error("")
        io.println_error("Example: intent beads interview-abc123def456")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.beads_help()))
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
        io.println_error(
          "Usage: intent bead-status --bead-id <id> --status success|failed|blocked [--reason 'text'] [--session <id>]",
        )
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
                io.println("✓ Bead " <> bead_id <> " marked as success")
                halt(exit_pass)
              }
              Error(err) -> {
                io.println_error(
                  "✗ Failed to mark bead: "
                  <> bead_feedback_error_to_string(err),
                )
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
                io.println("✓ Bead " <> bead_id <> " marked as failed")
                halt(exit_pass)
              }
              Error(err) -> {
                io.println_error(
                  "✗ Failed to mark bead: "
                  <> bead_feedback_error_to_string(err),
                )
                halt(exit_error)
              }
            }
          }
          "blocked" -> {
            case string.is_empty(reason) {
              True -> {
                io.println_error("Error: --status blocked requires --reason")
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
                    io.println(
                      "✓ Bead " <> bead_id <> " marked as blocked: " <> reason,
                    )
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    io.println_error(
                      "✗ Failed to mark bead: "
                      <> bead_feedback_error_to_string(err),
                    )
                    halt(exit_error)
                  }
                }
              }
            }
          }
          _ -> {
            io.println_error("Error: invalid status '" <> status <> "'")
            io.println_error("Valid statuses: success, failed, blocked")
            halt(exit_error)
          }
        }
      }
    }
  })
  |> glint.description(help.format_for_glint(help.bead_status_help()))
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
        io.println_error(
          "Usage: intent plan <session_id> [--format human|json]",
        )
        io.println_error("")
        io.println_error("Display execution plan from session beads.")
        io.println_error("")
        io.println_error("Examples:")
        io.println_error(
          "  intent plan abc123              # Human-readable output",
        )
        io.println_error("  intent plan abc123 --format json  # JSON output")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.plan_help()))
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
  let jsonl_path = ".interview/sessions.jsonl"
  let session_path = ".intent/session-" <> session_id <> ".cue"

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
            io.println_error(plan_mode.format_error(err))
            halt(exit_error)
          }
          Ok(plan) -> {
            // Show plan summary
            io.println("")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("                    PLAN APPROVAL")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("")
            io.println("Session: " <> plan.session_id)
            io.println("Total Beads: " <> string.inspect(plan.total_beads))
            io.println("Total Effort: " <> plan.total_effort)
            io.println("Risk Level: " <> risk_level_to_string(plan.risk))
            io.println("Phases: " <> string.inspect(list.length(plan.phases)))
            io.println("")

            case list.is_empty(plan.blockers) {
              True -> Nil
              False -> {
                io.println("⚠ BLOCKERS:")
                list.each(plan.blockers, fn(b) { io.println("  • " <> b) })
                io.println("")
              }
            }

            // Auto-approve or prompt
            case auto_approve {
              True -> {
                case approve_plan(session_id, "ci", notes) {
                  Ok(Nil) -> {
                    io.println("✓ Plan approved automatically (CI mode)")
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    io.println_error("✗ Failed to approve plan: " <> err)
                    halt(exit_error)
                  }
                }
              }
              False -> {
                io.println("Approve this plan? (yes/no)")
                case stdin.read_line() {
                  Ok(response) -> {
                    let cleaned = string.trim(string.lowercase(response))
                    case cleaned {
                      "yes" | "y" -> {
                        case approve_plan(session_id, "human", notes) {
                          Ok(Nil) -> {
                            io.println("✓ Plan approved")
                            halt(exit_pass)
                          }
                          Error(err) -> {
                            io.println_error(
                              "✗ Failed to approve plan: " <> err,
                            )
                            halt(exit_error)
                          }
                        }
                      }
                      "no" | "n" -> {
                        io.println("Plan not approved")
                        halt(exit_fail)
                      }
                      _ -> {
                        io.println_error(
                          "Invalid response. Please enter 'yes' or 'no'",
                        )
                        halt(exit_error)
                      }
                    }
                  }
                  Error(_) -> {
                    io.println_error("Failed to read input")
                    halt(exit_error)
                  }
                }
              }
            }
          }
        }
      }
      [] -> {
        io.println_error(
          "Usage: intent plan-approve <session_id> [--yes] [--notes 'text']",
        )
        io.println_error("")
        io.println_error("Approve execution plan for a session.")
        io.println_error("")
        io.println_error("Flags:")
        io.println_error(
          "  --yes      Auto-approve for CI pipelines (non-interactive)",
        )
        io.println_error("  --notes    Optional approval notes")
        io.println_error("")
        io.println_error("Examples:")
        io.println_error(
          "  intent plan-approve abc123           # Interactive approval",
        )
        io.println_error(
          "  intent plan-approve abc123 --yes     # CI auto-approval",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.plan_approve_help()))
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

@external(erlang, "intent_ffi", "current_iso8601_timestamp")
fn current_iso8601_timestamp() -> String

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
            io.println_error("Session not found: " <> session_id)
            io.println_error("Expected file: " <> session_path)
            halt(exit_error)
          }
          Ok(_) -> {
            // Load feedback
            case bead_feedback.load_feedback_for_session(session_id) {
              Error(err) -> {
                io.println_error(
                  "Failed to load feedback: "
                  <> bead_feedback_error_to_string(err),
                )
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

                io.println("")
                io.println(
                  "═══════════════════════════════════════════════════════════════════",
                )
                io.println("                    BEAD REGENERATION")
                io.println(
                  "═══════════════════════════════════════════════════════════════════",
                )
                io.println("")
                io.println("Session: " <> session_id)
                io.println("Strategy: " <> strategy)
                io.println(
                  "Feedback entries: " <> string.inspect(list.length(feedback)),
                )
                io.println(
                  "Beads needing regeneration: "
                  <> string.inspect(list.length(needs_regen)),
                )
                io.println("")

                case list.is_empty(needs_regen) {
                  True -> {
                    io.println(
                      "✓ No beads need regeneration - all passed or skipped",
                    )
                    halt(exit_pass)
                  }
                  False -> {
                    // Display beads that need regeneration
                    io.println("Beads to regenerate:")
                    list.each(needs_regen, fn(fb) {
                      let status_icon = case fb.result {
                        bead_feedback.Failed -> "✗"
                        bead_feedback.Blocked -> "⊘"
                        _ -> "?"
                      }
                      io.println(
                        "  "
                        <> status_icon
                        <> " "
                        <> fb.bead_id
                        <> ": "
                        <> fb.reason,
                      )
                    })
                    io.println("")

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
                        io.println("✓ Regeneration metadata added to session")
                        io.println("  Strategy: " <> strategy)
                        io.println(
                          "  Beads marked for regeneration: "
                          <> string.inspect(list.length(needs_regen)),
                        )
                        io.println("")
                        io.println("Next steps:")
                        io.println(
                          "  1. Review regeneration suggestions in "
                          <> session_path,
                        )
                        io.println(
                          "  2. Run 'intent plan "
                          <> session_id
                          <> "' to see updated plan",
                        )
                        io.println("  3. Execute regenerated beads")
                        halt(exit_pass)
                      }
                      Error(err) -> {
                        io.println_error("✗ Failed to update session: " <> err)
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
        io.println_error(
          "Usage: intent beads-regenerate <session_id> [--strategy hybrid|inversion|premortem]",
        )
        io.println_error("")
        io.println_error(
          "Regenerate failed/blocked beads with adjusted approach.",
        )
        io.println_error("")
        io.println_error("Strategies:")
        io.println_error("  hybrid     - Use all analysis methods (default)")
        io.println_error("  inversion  - Focus on failure mode analysis")
        io.println_error("  premortem  - Focus on what could go wrong")
        io.println_error("")
        io.println_error("Examples:")
        io.println_error("  intent beads-regenerate abc123")
        io.println_error(
          "  intent beads-regenerate abc123 --strategy inversion",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.beads_regenerate_help()))
  |> glint.flag(
    "strategy",
    flag.string()
      |> flag.default("hybrid")
      |> flag.description(
        "Regeneration strategy: hybrid, inversion, or premortem",
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
    let history_path = ".interview/history.jsonl"
    let mode = output_mode.Interactive

    let max_items =
      flag.get_int(input.flags, "max-items")
      |> result.unwrap(list_limits.default_max_items)

    case input.args {
      [session_id, ..] -> {
        case interview_storage.list_session_history(history_path, session_id) {
          Error(err) -> {
            cli_ui.print_error(err, mode)
            halt(exit_error)
          }
          Ok([]) -> {
            cli_ui.print_warning(
              "No history found for session: " <> session_id,
              mode,
            )
            io.println("")
            io.println(
              "Tip: Session history is recorded when you save snapshots",
            )
            io.println("during an interview with --snapshot flag.")
            halt(exit_pass)
          }
          Ok(all_snapshots) -> {
            // Apply max-items limit (AI guardrail)
            let total_count = list.length(all_snapshots)
            let snapshots = list_limits.apply_limit(all_snapshots, max_items)
            let shown_count = list.length(snapshots)
            let was_limited = total_count > shown_count

            cli_ui.print_header("Session History: " <> session_id, mode)
            io.println("")

            case was_limited {
              True ->
                io.println(
                  "(showing "
                  <> string.inspect(shown_count)
                  <> " of "
                  <> string.inspect(total_count)
                  <> " snapshots)",
                )
              False -> Nil
            }

            list.each(snapshots, fn(snapshot) {
              io.println("┌─ " <> snapshot.snapshot_id)
              io.println("│  Time: " <> snapshot.timestamp)
              io.println("│  Stage: " <> snapshot.stage)
              io.println("│  Description: " <> snapshot.description)
              io.println(
                "│  Answers: " <> string.inspect(dict.size(snapshot.answers)),
              )
              io.println("│  Gaps: " <> string.inspect(snapshot.gaps_count))
              io.println(
                "│  Conflicts: " <> string.inspect(snapshot.conflicts_count),
              )
              io.println("└─")
              io.println("")
            })

            halt(exit_pass)
          }
        }
      }
      [] -> {
        cli_ui.print_error("Session ID required", mode)
        io.println("")
        io.println("Usage: intent history <session-id> [--max-items N]")
        io.println("")
        io.println("Example: intent history interview-abc123")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.history_help()))
  |> glint.flag(
    "max-items",
    flag.int()
      |> flag.default(list_limits.default_max_items)
      |> flag.description(
        "Maximum number of history snapshots to return (default: 100, AI guardrail)",
      ),
  )
}

/// The `diff` command - compare two sessions
fn diff_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let jsonl_path = ".interview/sessions.jsonl"
    let mode = output_mode.Interactive

    case input.args {
      [from_id, to_id, ..] -> {
        // Load both sessions
        case interview_storage.get_session_from_jsonl(jsonl_path, from_id) {
          Error(err) -> {
            cli_ui.print_error("Failed to load 'from' session: " <> err, mode)
            halt(exit_error)
          }
          Ok(from_session) -> {
            case interview_storage.get_session_from_jsonl(jsonl_path, to_id) {
              Error(err) -> {
                cli_ui.print_error("Failed to load 'to' session: " <> err, mode)
                halt(exit_error)
              }
              Ok(to_session) -> {
                // Compute and display diff
                let diff =
                  interview_storage.diff_sessions(from_session, to_session)
                cli_ui.print_header("Session Comparison", mode)
                io.println("")
                io.println(interview_storage.format_diff(diff))

                // Summary stats
                io.println("")
                let total_changes =
                  list.length(diff.answers_added)
                  + list.length(diff.answers_modified)
                  + list.length(diff.answers_removed)

                case total_changes {
                  0 ->
                    cli_ui.print_info(
                      "No answer changes between sessions",
                      mode,
                    )
                  n ->
                    cli_ui.print_info(
                      string.inspect(n) <> " total answer changes",
                      mode,
                    )
                }

                halt(exit_pass)
              }
            }
          }
        }
      }
      [single_id] -> {
        // Compare session with its previous version (if exists)
        cli_ui.print_error("Two session IDs required for comparison", mode)
        io.println("")
        io.println("Usage: intent diff <from-session> <to-session>")
        io.println("")
        io.println("Tip: Use 'intent sessions' to list available sessions")
        io.println("     Session provided: " <> single_id)
        halt(exit_error)
      }
      [] -> {
        cli_ui.print_error("Session IDs required", mode)
        io.println("")
        io.println("Usage: intent diff <from-session> <to-session>")
        io.println("")
        io.println("Compare two interview sessions and show differences")
        io.println("in answers, gaps, conflicts, and stage.")
        io.println("")
        io.println("Example:")
        io.println("  intent diff interview-abc123 interview-def456")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.diff_help()))
}

/// The `sessions` command - list all interview sessions
fn sessions_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let jsonl_path = ".interview/sessions.jsonl"

    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let mode = output_mode.from_json_flag(is_json)

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
        cli_ui.print_warning("No interview sessions found", mode)
        io.println("")
        io.println("Start a new interview with:")
        io.println("  intent interview --profile api")
        halt(exit_pass)
      }
      Ok([]) -> {
        cli_ui.print_warning("No interview sessions found", mode)
        io.println("")
        io.println("Start a new interview with:")
        io.println("  intent interview --profile api")
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

        case is_json {
          True -> {
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
                "intent interview --resume <id>",
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
          }
          False -> {
            cli_ui.print_header("Interview Sessions", mode)
            io.println("")

            case was_limited {
              True ->
                io.println(
                  "(showing "
                  <> string.inspect(shown_count)
                  <> " of "
                  <> string.inspect(total_count)
                  <> " sessions)",
                )
              False -> Nil
            }

            list.each(limited, fn(session) {
              let status_icon = case session.stage {
                interview.Complete -> "✓"
                interview.Paused -> "⏸"
                _ -> "●"
              }

              io.println(status_icon <> " " <> session.id)
              io.println(
                "  Profile: " <> profile_to_display_string(session.profile),
              )
              io.println("  Stage: " <> stage_to_display_string(session.stage))
              io.println(
                "  Rounds: " <> string.inspect(session.rounds_completed) <> "/5",
              )
              io.println(
                "  Answers: " <> string.inspect(list.length(session.answers)),
              )
              io.println("  Created: " <> session.created_at)
              io.println("  Updated: " <> session.updated_at)
              io.println("")
            })

            io.println(
              "Total: "
              <> string.inspect(total_count)
              <> " session(s)"
              <> case was_limited {
                True -> " (limited to " <> string.inspect(shown_count) <> ")"
                False -> ""
              },
            )
          }
        }

        halt(exit_pass)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.sessions_help()))
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
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

fn stage_to_display_string(stage: interview.InterviewStage) -> String {
  case stage {
    interview.Discovery -> "Discovery"
    interview.Refinement -> "Refinement"
    interview.Validation -> "Validation"
    interview.Complete -> "Complete"
    interview.Paused -> "Paused"
  }
}

// =============================================================================
// KIRK COMMANDS
// =============================================================================

/// The `quality` command - Quality analysis (alias for analyze)
fn kirk_quality_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let mode = output_mode.from_json_flag(is_json)

    case input.args {
      [spec_path, ..] -> {
        case load_spec_for_mode(spec_path, is_json) {
          Ok(spec) -> {
            let report = quality_analyzer.analyze_spec(spec)
            case is_json {
              True -> {
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
              False -> io.println(quality_analyzer.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent quality <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.quality_help()))
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
  )
}

/// The `invert` command - KIRK inversion analysis
fn kirk_invert_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let mode = output_mode.from_json_flag(is_json)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = inversion_checker.analyze_inversions(spec)
            case is_json {
              True -> {
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
              False -> io.println(inversion_checker.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent invert <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.invert_help()))
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
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
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let mode = output_mode.from_json_flag(is_json)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = coverage_analyzer.analyze_coverage(spec)
            case is_json {
              True -> {
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
              False -> io.println(coverage_analyzer.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent coverage <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.coverage_help()))
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
  )
}

/// The `gaps` command - KIRK gap detection
fn kirk_gaps_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let mode = output_mode.from_json_flag(is_json)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = gap_detector.detect_gaps(spec)
            case is_json {
              True -> {
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
              False -> io.println(gap_detector.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent gaps <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.gaps_help()))
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
  )
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
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let mode = output_mode.from_json_flag(is_json)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = effects_analyzer.analyze_effects(spec)

            case is_json {
              True -> {
                let json_output =
                  effects_analyzer.effects_report_to_action_json(
                    report,
                    spec.name,
                  )
                io.println(json.to_string(json_output))
              }
              False -> {
                io.println(effects_analyzer.format_report(report))
              }
            }

            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent effects <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.effects_help()))
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
    let mode = output_mode.Interactive

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
              "" -> io.println(output)
              path -> {
                case simplifile.write(path, output) {
                  Ok(_) -> io.println("Written to: " <> path)
                  Error(_) ->
                    cli_ui.print_error("Failed to write to: " <> path, mode)
                }
              }
            }

            halt(exit_pass)
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read: " <> requirements_path, mode)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error("requirements file path required", mode)
        io.println(
          "Usage: intent ears <requirements.md> [--output text|cue|json] [--out <file>]",
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
        halt(exit_error)
      }
    }
  })
  |> glint.description(help.format_for_glint(help.ears_help()))
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
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let mode = output_mode.from_json_flag(is_json)

    let output_file =
      flag.get_string(input.flags, "o")
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

            case is_json {
              True -> {
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
                    None,
                    next_actions,
                  )
                json_output.output(response)
              }
              False -> {
                // Print parsing progress
                io.println("Parsing EARS requirements...")

                case ubiq > 0 {
                  True ->
                    io.println(
                      "✓ Parsed "
                      <> string.inspect(ubiq)
                      <> " ubiquitous requirements",
                    )
                  False -> Nil
                }
                case event > 0 {
                  True ->
                    io.println(
                      "✓ Parsed "
                      <> string.inspect(event)
                      <> " event-driven requirements",
                    )
                  False -> Nil
                }
                case state > 0 {
                  True ->
                    io.println(
                      "✓ Parsed "
                      <> string.inspect(state)
                      <> " state-driven requirements",
                    )
                  False -> Nil
                }
                case opt > 0 {
                  True ->
                    io.println(
                      "✓ Parsed "
                      <> string.inspect(opt)
                      <> " optional requirements",
                    )
                  False -> Nil
                }
                case unwant > 0 {
                  True ->
                    io.println(
                      "✓ Parsed "
                      <> string.inspect(unwant)
                      <> " unwanted requirements",
                    )
                  False -> Nil
                }
                case complex > 0 {
                  True ->
                    io.println(
                      "✓ Parsed "
                      <> string.inspect(complex)
                      <> " complex requirements",
                    )
                  False -> Nil
                }

                // Print errors
                case err_count > 0 {
                  True -> {
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
                      io.println(
                        "Line " <> string.inspect(line) <> ": " <> message,
                      )
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
                  False -> Nil
                }

                // Write to output file if specified
                case output_file {
                  "" -> Nil
                  path -> {
                    // Infer spec name from filename
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
                      Ok(_) -> io.println("Written to: " <> path)
                      Error(e) ->
                        cli_ui.print_error(
                          "Failed to write: " <> string.inspect(e),
                          mode,
                        )
                    }
                  }
                }
              }
            }

            case err_count > 0 {
              True -> halt(exit_fail)
              False -> halt(exit_pass)
            }
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read: " <> requirements_path, mode)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error("requirements file path required", mode)
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
  |> glint.description(help.format_for_glint(help.parse_help()))
  |> glint.flag(
    "o",
    flag.string()
      |> flag.default("")
      |> flag.description("Output spec file path"),
  )
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
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

@external(erlang, "intent_ffi", "generate_uuid")
fn generate_uuid() -> String

@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String

@external(erlang, "intent_ffi", "get_env")
fn get_env(name: String) -> Result(String, Nil)

/// Check if localhost is allowed via environment variable
fn is_localhost_allowed_by_env() -> Bool {
  case get_env("INTENT_ALLOW_LOCALHOST") {
    Ok("true") | Ok("1") | Ok("yes") -> True
    _ -> False
  }
}
