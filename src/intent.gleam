/// Intent CLI - Human-writes, AI-verifies, AI-implements
/// Contract-driven API testing tool
import argv
import gleam/dict
import gleam/float
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/answer_loader
import intent/bd_integration
import intent/bead_feedback
import intent/bead_generator
import intent/bead_templates
import intent/cli_ui
import intent/ears_interview
import intent/improver
import intent/interview
import intent/interview_questions
import intent/interview_storage
import intent/kirk/compact_format
import intent/kirk/coverage_analyzer
import intent/kirk/ears_parser
import intent/kirk/effects_analyzer
import intent/kirk/gap_detector
import intent/kirk/inversion_checker
import intent/kirk/quality_analyzer as kirk_quality
import intent/kirk_contract
import intent/loader
import intent/mental_lattice
import intent/output
import intent/plan_mode
import intent/quality_analyzer
import intent/question_types.{type Question}
import intent/runner
import intent/security
import intent/spec_builder
import intent/spec_linter
import intent/stdin
import intent/structure_planner
import intent/types
import simplifile

/// Exit codes
const exit_pass = 0

const exit_fail = 1

const exit_blocked = 2

const exit_invalid = 3

const exit_error = 4

pub fn main() {
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
  |> glint.add(at: ["interview"], do: interview_command())
  |> glint.add(at: ["beads"], do: beads_command())
  |> glint.add(at: ["execute-beads"], do: execute_beads_command())
  |> glint.add(at: ["bead-status"], do: bead_status_command())
  |> glint.add(at: ["history"], do: history_command())
  |> glint.add(at: ["diff"], do: diff_command())
  |> glint.add(at: ["sessions"], do: sessions_command())
  // KIRK commands
  |> glint.add(at: ["quality"], do: kirk_quality_command())
  |> glint.add(at: ["invert"], do: kirk_invert_command())
  |> glint.add(at: ["coverage"], do: kirk_coverage_command())
  |> glint.add(at: ["gaps"], do: kirk_gaps_command())
  |> glint.add(at: ["compact"], do: kirk_compact_command())
  |> glint.add(at: ["prototext"], do: kirk_prototext_command())
  |> glint.add(at: ["ears"], do: kirk_ears_command())
  |> glint.add(at: ["ears-interview"], do: ears_interview_command())
  |> glint.add(at: ["lattice-analyze"], do: lattice_analyze_command())
  |> glint.add(at: ["generate-contract"], do: generate_contract_command())
  |> glint.add(at: ["plan-structure"], do: plan_structure_command())
  |> glint.add(at: ["generate-beads"], do: generate_beads_command())
  |> glint.add(at: ["parse"], do: parse_command())
  |> glint.add(at: ["effects"], do: kirk_effects_command())
  // Plan commands
  |> glint.add(at: ["plan"], do: plan_command())
  |> glint.add(at: ["plan-approve"], do: plan_approve_command())
  |> glint.add(at: ["beads-regenerate"], do: beads_regenerate_command())
  // Context scanning
  // TODO: Re-enable when context_scan_command is implemented
  // |> glint.add(at: ["context-scan"], do: context_scan_command())
  |> glint.run(argv.load().arguments)
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

    case input.args {
      [spec_path, ..] -> {
        run_check(
          spec_path,
          target_url,
          is_json,
          feature_filter,
          only_filter,
          output_level,
        )
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent check <spec.cue> --target <url>")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Run spec against a target URL and verify behaviors")
  |> glint.flag(
    "target",
    flag.string()
      |> flag.default("")
      |> flag.description("Target base URL to test against"),
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output results as JSON"),
  )
  |> glint.flag(
    "feature",
    flag.string()
      |> flag.default("")
      |> flag.description("Filter to a specific feature"),
  )
  |> glint.flag(
    "only",
    flag.string()
      |> flag.default("")
      |> flag.description("Run only a specific behavior"),
  )
  |> glint.flag(
    "verbose",
    flag.bool() |> flag.default(False) |> flag.description("Verbose output"),
  )
  |> glint.flag(
    "quiet",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Quiet output (errors only)"),
  )
}

fn run_check(
  spec_path: String,
  target_url: String,
  is_json: Bool,
  feature_filter: String,
  only_filter: String,
  output_level: runner.OutputLevel,
) -> Nil {
  // Validate target URL is provided
  case string.is_empty(target_url) {
    True -> {
      cli_ui.print_error("--target URL is required")
      io.println("Usage: intent check <spec.cue> --target=<url>")
      halt(exit_error)
    }
    False -> Nil
  }

  // Validate target URL for SSRF protection
  case security.validate_url(target_url) {
    Error(e) -> {
      cli_ui.print_error(security.format_security_error(e))
      halt(exit_error)
    }
    Ok(_) -> Nil
  }

  // Load the spec
  case loader.load_spec(spec_path) {
    Error(e) -> {
      cli_ui.print_error(loader.format_error(e))
      halt(exit_invalid)
    }
    Ok(spec) -> {
      cli_ui.print_header("Checking spec: " <> spec.name)

      // Build run options
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
        )

      // Run the spec
      let result = runner.run_spec(spec, target_url, options)

      // Output results
      case is_json {
        True -> {
          let json_result = output.spec_result_to_json(result)
          io.println(json.to_string(json_result))
        }
        False -> {
          io.println(output.spec_result_to_text(result))
        }
      }

      // Exit with appropriate code
      let exit_code = case result {
        output.SpecResult(pass: True, ..) -> {
          cli_ui.print_success("All checks passed!")
          exit_pass
        }
        output.SpecResult(blocked: blocked, ..) if blocked > 0 -> {
          cli_ui.print_warning("Blocked behaviors detected")
          exit_blocked
        }
        _ -> {
          cli_ui.print_error("Check failed")
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
    case input.args {
      [spec_path, ..] -> {
        // Use load_spec_quiet to validate both CUE syntax AND spec structure
        case loader.load_spec_quiet(spec_path) {
          Ok(_) -> {
            cli_ui.print_success("Valid spec: " <> spec_path)
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error("Invalid spec: " <> loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent validate <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Validate a CUE spec file (syntax and structure)")
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
            case loader.export_spec_json(spec_path) {
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
  |> glint.description("Pretty print a parsed spec")
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
        case loader.export_spec_json(spec_path) {
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
  |> glint.description("Check spec for anti-patterns and quality issues")
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
  |> glint.description(
    "Analyze spec quality and provide improvement suggestions",
  )
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
  |> glint.description(
    "Suggest improvements based on quality analysis and linting",
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

    // CUE mode: output CUE directives for AI agents
    case cue_mode {
      True -> {
        // Check if this is answering a question or starting/resuming
        let has_session = !string.is_empty(session_flag)
        let has_answer = !string.is_empty(answer_text)

        case has_session, has_answer {
          // Submitting an answer to an existing session
          True, True -> run_interview_cue_answer(session_flag, answer_text)
          // Resume session in CUE mode
          True, False -> run_interview_cue_resume(session_flag)
          // Start new session in CUE mode
          False, False -> {
            let profile = parse_profile(profile_str)
            case profile {
              Ok(p) -> run_interview_cue_start(p)
              Error(msg) -> {
                output_cue_error(msg)
                halt(exit_error)
              }
            }
          }
          // Invalid: answer without session
          False, True -> {
            output_cue_error("--answer requires --session flag")
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
  |> glint.description(
    "Guided specification discovery through structured interview",
  )
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("api")
      |> flag.description(
        "System profile: api, cli, event, data, workflow, or ui",
      ),
  )
  |> glint.flag(
    "resume",
    flag.string()
      |> flag.default("")
      |> flag.description("Resume existing interview session by ID"),
  )
  |> glint.flag(
    "answers",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Path to CUE file with pre-filled answers for non-interactive mode",
      ),
  )
  |> glint.flag(
    "strict",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Strict mode: fail if answers file is missing required answers (requires --answers)",
      ),
  )
  |> glint.flag(
    "export",
    flag.string()
      |> flag.default("")
      |> flag.description("Export completed interview to spec file"),
  )
  |> glint.flag(
    "cue",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Output CUE directives for AI agents (non-interactive)",
      ),
  )
  |> glint.flag(
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description("Session ID for CUE mode (use with --cue)"),
  )
  |> glint.flag(
    "answer",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Submit answer to current question (use with --cue --session)",
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
  let answers_dict = case string.is_empty(answers_file) {
    True -> option.None
    False -> {
      case answer_loader.load_from_file(answers_file) {
        Ok(dict) -> {
          io.println("")
          io.println(
            "✓ Loaded "
            <> string.inspect(dict.size(dict))
            <> " pre-filled answers from: "
            <> answers_file,
          )
          option.Some(dict)
        }
        Error(err) -> {
          case strict_mode {
            True -> {
              io.println_error(
                "✗ Failed to load answers file: "
                <> answer_loader_error_to_string(err),
              )
              halt(exit_error)
              option.None
              // unreachable, but needed for type consistency
            }
            False -> {
              io.println(
                "⚠ Failed to load answers file: "
                <> answer_loader_error_to_string(err),
              )
              io.println("  Continuing in interactive mode...")
              option.None
            }
          }
        }
      }
    }
  }

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
  case answers_dict {
    option.None -> Nil
    option.Some(_) -> io.println("Mode: Non-interactive (answers from file)")
  }
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

  // Load the session from JSONL
  case interview_storage.get_session_from_jsonl(jsonl_path, session_id) {
    Error(err) -> {
      cli_ui.print_error(err)
      halt(exit_error)
    }
    Ok(session) -> {
      cli_ui.print_header("Resuming Interview: " <> session.id)
      cli_ui.print_info(
        "Profile: " <> profile_to_display_string(session.profile),
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
          cli_ui.print_success("Session updated: " <> session.id)
        }
        Error(err) -> {
          cli_ui.print_error("Failed to save session: " <> err)
        }
      }

      // Export to spec if requested
      case export_to {
        "" -> Nil
        path -> {
          let spec_cue = spec_builder.build_spec_from_session(final_session)
          case simplifile.write(path, spec_cue) {
            Ok(Nil) -> {
              cli_ui.print_success("Spec exported to: " <> path)
            }
            Error(err) -> {
              cli_ui.print_error(
                "Failed to export spec: " <> string.inspect(err),
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
fn run_interview_cue_start(profile: interview.Profile) -> Nil {
  let session_id = "interview-" <> generate_uuid()
  let timestamp = current_timestamp()
  let session = interview.create_session(session_id, profile, timestamp)

  // Save session to JSONL
  let save_result =
    interview_storage.append_session_to_jsonl(
      session,
      ".interview/sessions.jsonl",
    )

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
fn run_interview_cue_resume(session_id: String) -> Nil {
  case
    interview_storage.get_session_from_jsonl(
      ".interview/sessions.jsonl",
      session_id,
    )
  {
    Error(err) -> {
      output_cue_error("Session not found: " <> err)
      halt(exit_error)
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
fn run_interview_cue_answer(session_id: String, answer_text: String) -> Nil {
  case
    interview_storage.get_session_from_jsonl(
      ".interview/sessions.jsonl",
      session_id,
    )
  {
    Error(err) -> {
      output_cue_error("Session not found: " <> err)
      halt(exit_error)
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

              // Save updated session
              case
                interview_storage.append_session_to_jsonl(
                  sess_final,
                  ".interview/sessions.jsonl",
                )
              {
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
                      // Check if there are more rounds
                      case next_round < 5 {
                        True -> {
                          case
                            get_next_unanswered_question(
                              sess_final,
                              next_round + 1,
                            )
                          {
                            Some(next_q) ->
                              output_cue_question(
                                sess_final,
                                next_q,
                                next_round + 1,
                              )
                            None -> output_cue_complete(sess_final)
                          }
                        }
                        False -> output_cue_complete(sess_final)
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
  let spec_path = ".interview/spec-" <> session.id <> ".cue"

  // Generate and save the spec
  let spec_cue = spec_builder.build_spec_from_session(session)
  let _ = simplifile.write(spec_path, spec_cue)

  let output =
    "{\n"
    <> "\taction: \"interview_complete\"\n\n"
    <> "\toutput: {\n"
    <> "\t\tspec_path: \""
    <> spec_path
    <> "\"\n"
    <> "\t\tbehaviors_count: "
    <> string.inspect(behaviors_count)
    <> "\n"
    <> "\t\tanti_patterns_count: "
    <> string.inspect(anti_patterns_count)
    <> "\n"
    <> "\t\tsummary: \"Interview complete. Generated spec with "
    <> string.inspect(behaviors_count)
    <> " behaviors.\"\n"
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
            let beads = bead_templates.generate_beads_from_session(session)
            let bead_count = list.length(beads)

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
              <> string.inspect(bead_count)
              <> " work items from session: "
              <> session_id,
            )
            io.println("")

            // Export to .beads/issues.jsonl
            let jsonl_output = bead_templates.beads_to_jsonl(beads)

            case
              simplifile.append(".beads/issues.jsonl", jsonl_output <> "\n")
            {
              Ok(Nil) -> {
                io.println("✓ Beads exported to: .beads/issues.jsonl")
                io.println("")

                // Show stats
                let stats = bead_templates.bead_stats(beads)
                io.println("Summary:")
                io.println("  Total beads: " <> string.inspect(stats.total))

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
        io.println_error("Usage: intent beads <session_id>")
        io.println_error("")
        io.println_error("Example: intent beads interview-abc123def456")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Generate work items (beads) from an interview session")
}

/// The `execute-beads` command - create beads in bd database from interview session
fn execute_beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
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
            io.println_error("Error loading session: " <> err)
            halt(exit_error)
          }
          Ok(session) -> {
            // Generate beads from session
            let beads = bead_templates.generate_beads_from_session(session)
            let bead_count = list.length(beads)

            io.println("")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("                 EXECUTE BEADS IN BD DATABASE")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("")
            io.println(
              "Creating "
              <> string.inspect(bead_count)
              <> " beads from session: "
              <> session_id,
            )
            io.println("")

            // Create beads in bd database
            let results = bd_integration.create_beads(beads)

            // Count successes and failures
            let success_count =
              results
              |> list.filter(fn(r) { result.is_ok(r) })
              |> list.length

            let failure_count =
              results
              |> list.filter(fn(r) { result.is_error(r) })
              |> list.length

            // Display results
            io.println("Results:")
            io.println(
              "  ✓ Successfully created: " <> string.inspect(success_count),
            )

            case failure_count > 0 {
              True -> {
                io.println(
                  "  ✗ Failed to create: " <> string.inspect(failure_count),
                )
                io.println("")
                io.println("Errors:")

                // Show errors
                results
                |> list.filter(fn(r) { result.is_error(r) })
                |> list.map(fn(r) {
                  case r {
                    Error(err) ->
                      io.println("  - " <> bd_integration.describe_error(err))
                    Ok(_) -> Nil
                  }
                })

                halt(exit_error)
              }
              False -> {
                io.println("")
                io.println("All beads created successfully!")
                halt(exit_pass)
              }
            }
          }
        }
      }
      [] -> {
        io.println_error("Usage: intent execute-beads <session_id>")
        io.println_error("")
        io.println_error("Example: intent execute-beads interview-abc123def456")
        io.println_error("")
        io.println_error(
          "This command creates beads in the bd database using 'bd create'.",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description("Create beads in bd database from an interview session")
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
        case plan_mode.compute_plan(session_id) {
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
  |> glint.description("Display execution plan from session beads")
  |> glint.flag(
    "format",
    flag.string()
      |> flag.default("human")
      |> flag.description("Output format: human or json"),
  )
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
        case plan_mode.compute_plan(session_id) {
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
                        io.println_error(
                          "✗ Failed to update session: " <> err,
                        )
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

    case input.args {
      [session_id, ..] -> {
        case interview_storage.list_session_history(history_path, session_id) {
          Error(err) -> {
            cli_ui.print_error(err)
            halt(exit_error)
          }
          Ok([]) -> {
            cli_ui.print_warning("No history found for session: " <> session_id)
            io.println("")
            io.println(
              "Tip: Session history is recorded when you save snapshots",
            )
            io.println("during an interview with --snapshot flag.")
            halt(exit_pass)
          }
          Ok(snapshots) -> {
            cli_ui.print_header("Session History: " <> session_id)
            io.println("")

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
        cli_ui.print_error("Session ID required")
        io.println("")
        io.println("Usage: intent history <session-id>")
        io.println("")
        io.println("Example: intent history interview-abc123")
        halt(exit_error)
      }
    }
  })
  |> glint.description("View snapshot history for an interview session")
}

/// The `diff` command - compare two sessions
fn diff_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let jsonl_path = ".interview/sessions.jsonl"

    case input.args {
      [from_id, to_id, ..] -> {
        // Load both sessions
        case interview_storage.get_session_from_jsonl(jsonl_path, from_id) {
          Error(err) -> {
            cli_ui.print_error("Failed to load 'from' session: " <> err)
            halt(exit_error)
          }
          Ok(from_session) -> {
            case interview_storage.get_session_from_jsonl(jsonl_path, to_id) {
              Error(err) -> {
                cli_ui.print_error("Failed to load 'to' session: " <> err)
                halt(exit_error)
              }
              Ok(to_session) -> {
                // Compute and display diff
                let diff =
                  interview_storage.diff_sessions(from_session, to_session)
                cli_ui.print_header("Session Comparison")
                io.println("")
                io.println(interview_storage.format_diff(diff))

                // Summary stats
                io.println("")
                let total_changes =
                  list.length(diff.answers_added)
                  + list.length(diff.answers_modified)
                  + list.length(diff.answers_removed)

                case total_changes {
                  0 -> cli_ui.print_info("No answer changes between sessions")
                  n ->
                    cli_ui.print_info(
                      string.inspect(n) <> " total answer changes",
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
        cli_ui.print_error("Two session IDs required for comparison")
        io.println("")
        io.println("Usage: intent diff <from-session> <to-session>")
        io.println("")
        io.println("Tip: Use 'intent sessions' to list available sessions")
        io.println("     Session provided: " <> single_id)
        halt(exit_error)
      }
      [] -> {
        cli_ui.print_error("Session IDs required")
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
  |> glint.description("Compare two interview sessions and show differences")
}

/// The `sessions` command - list all interview sessions
fn sessions_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let jsonl_path = ".interview/sessions.jsonl"

    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let profile_filter =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("")

    case interview_storage.list_sessions_from_jsonl(jsonl_path) {
      Error(_) -> {
        // File doesn't exist yet - treat as empty
        cli_ui.print_warning("No interview sessions found")
        io.println("")
        io.println("Start a new interview with:")
        io.println("  intent interview --profile api")
        halt(exit_pass)
      }
      Ok([]) -> {
        cli_ui.print_warning("No interview sessions found")
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

        case is_json {
          True -> {
            let json_sessions =
              json.array(filtered, interview_storage.session_to_json)
            io.println(json.to_string(json_sessions))
          }
          False -> {
            cli_ui.print_header("Interview Sessions")
            io.println("")

            list.each(filtered, fn(session) {
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
              <> string.inspect(list.length(filtered))
              <> " session(s)",
            )
          }
        }

        halt(exit_pass)
      }
    }
  })
  |> glint.description("List all interview sessions")
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

/// The `quality` command - KIRK quality analysis
fn kirk_quality_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = kirk_quality.analyze_quality(spec)
            case is_json {
              True -> {
                let json_obj =
                  json.object([
                    #("completeness", json.float(report.completeness)),
                    #("consistency", json.float(report.consistency)),
                    #("testability", json.float(report.testability)),
                    #("clarity", json.float(report.clarity)),
                    #("security", json.float(report.security)),
                    #("overall", json.float(report.overall)),
                    #(
                      "issues",
                      json.array(report.issues, fn(i) {
                        json.object([
                          #("field", json.string(i.field)),
                          #("issue", json.string(i.issue)),
                          #(
                            "severity",
                            json.string(kirk_quality.severity_to_string(
                              i.severity,
                            )),
                          ),
                        ])
                      }),
                    ),
                  ])
                io.println(json.to_string(json_obj))
              }
              False -> io.println(kirk_quality.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent quality <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description("KIRK: Analyze spec quality across multiple dimensions")
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

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = inversion_checker.analyze_inversions(spec)
            case is_json {
              True -> {
                let json_obj =
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
                io.println(json.to_string(json_obj))
              }
              False -> io.println(inversion_checker.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent invert <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "KIRK: Inversion analysis - what failure cases are missing?",
  )
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

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = coverage_analyzer.analyze_coverage(spec)
            case is_json {
              True -> {
                let json_obj =
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
                io.println(json.to_string(json_obj))
              }
              False -> io.println(coverage_analyzer.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent coverage <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description("KIRK: Coverage analysis including OWASP Top 10")
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

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = gap_detector.detect_gaps(spec)
            case is_json {
              True -> {
                let json_obj =
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
                io.println(json.to_string(json_obj))
              }
              False -> io.println(gap_detector.format_report(report))
            }
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent gaps <spec.cue> [--json]")
        halt(exit_error)
      }
    }
  })
  |> glint.description("KIRK: Detect gaps using mental models")
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
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = effects_analyzer.analyze_effects(spec)
            io.println(effects_analyzer.format_report(report))
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent effects <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "KIRK: Analyze second-order effects (consequence tracing)",
  )
}

/// The `compact` command - KIRK compact format (CIN)
fn kirk_compact_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let show_tokens =
      flag.get_bool(input.flags, "tokens")
      |> result.unwrap(False)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let compact = compact_format.spec_to_compact(spec)
            let output = compact_format.format_compact(compact)
            io.println(output)

            case show_tokens {
              True -> {
                let #(full, compact_tokens, savings) =
                  compact_format.compare_token_usage(spec)
                io.println("")
                io.println(
                  "─────────────────────────────────────",
                )
                io.println("Token Analysis:")
                io.println(
                  "  Full JSON:    ~" <> string.inspect(full) <> " tokens",
                )
                io.println(
                  "  Compact CIN:  ~"
                  <> string.inspect(compact_tokens)
                  <> " tokens",
                )
                io.println(
                  "  Savings:      "
                  <> string.inspect(float.round(savings))
                  <> "%",
                )
              }
              False -> Nil
            }

            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent compact <spec.cue> [--tokens]")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "KIRK: Convert to Compact Intent Notation (token-efficient)",
  )
  |> glint.flag(
    "tokens",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Show token comparison"),
  )
}

/// The `prototext` command - KIRK protobuf text format output
fn kirk_prototext_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let output = compact_format.spec_to_prototext(spec)
            io.println(output)
            halt(exit_pass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required")
        io.println("Usage: intent prototext <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description("KIRK: Export to Protobuf text format")
}

/// The `ears` command - KIRK EARS requirements parser
fn kirk_ears_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
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
                let json_obj =
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
                        json.object([
                          #("line", json.int(e.line)),
                          #("message", json.string(e.message)),
                          #("suggestion", json.string(e.suggestion)),
                        ])
                      }),
                    ),
                    #("warnings", json.array(result.warnings, json.string)),
                  ])
                json.to_string(json_obj)
              }
              _ -> ears_parser.format_result(result)
            }

            case output_file {
              "" -> io.println(output)
              path -> {
                case simplifile.write(path, output) {
                  Ok(_) -> io.println("Written to: " <> path)
                  Error(_) -> cli_ui.print_error("Failed to write to: " <> path)
                }
              }
            }

            halt(exit_pass)
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read: " <> requirements_path)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error("requirements file path required")
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
  |> glint.description("KIRK: Parse EARS requirements to Intent behaviors")
  |> glint.flag(
    "output",
    flag.string()
      |> flag.default("text")
      |> flag.description("Output format: text, cue, json"),
  )
  |> glint.flag(
    "out",
    flag.string() |> flag.default("") |> flag.description("Output file path"),
  )
  |> glint.flag(
    "name",
    flag.string()
      |> flag.default("GeneratedSpec")
      |> flag.description("Spec name for CUE output"),
  )
}

/// The `ears-interview` command - interactive EARS requirements capture
fn ears_interview_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_name = case input.args {
      [name, ..] -> name
      [] -> "ears-session-" <> string.inspect(timestamp_ms())
    }

    io.println("")
    io.println(
      "═══════════════════════════════════════════════════════════════════",
    )
    io.println("          EARS INTERVIEW - Systematic Requirements Capture")
    io.println(
      "═══════════════════════════════════════════════════════════════════",
    )
    io.println("")
    io.println(
      "This interview will guide you through the 6 EARS patterns to capture",
    )
    io.println("complete, unambiguous requirements.")
    io.println("")
    io.println("Session: " <> session_name)
    io.println("")

    // Create new session
    let session = ears_interview.new_session(session_name)

    // Start interview loop
    run_ears_interview_loop(session)
  })
  |> glint.description("Interactive EARS requirements interview workflow")
}

/// Run the interactive EARS interview loop
fn run_ears_interview_loop(session: ears_interview.EarsSession) -> Nil {
  case ears_interview.is_complete(session) {
    True -> {
      // Interview complete
      io.println("")
      io.println(
        "═══════════════════════════════════════════════════════════════════",
      )
      io.println("                    INTERVIEW COMPLETE!")
      io.println(
        "═══════════════════════════════════════════════════════════════════",
      )
      io.println("")
      io.println(ears_interview.get_progress_summary(session))
      io.println("")

      // Export requirements
      let requirements = ears_interview.get_requirements(session)
      let requirements_text =
        requirements
        |> list.map(fn(req) { req.raw_text })
        |> string.join("\n")

      let output_file = session.session_id <> ".ears.txt"
      case simplifile.write(output_file, requirements_text) {
        Ok(_) -> {
          io.println("✓ Requirements saved to: " <> output_file)
          io.println("")
          io.println("Next steps:")
          io.println("  1. Review requirements in " <> output_file)
          io.println("  2. Run: intent ears " <> output_file <> " --output cue")
          io.println(
            "  3. Run: intent check <generated-spec.cue> --target <url>",
          )
        }
        Error(_) -> cli_ui.print_error("Failed to save requirements")
      }

      halt(exit_pass)
    }
    False -> {
      // Get current question
      let question = ears_interview.get_current_question(session)

      // Display question
      io.println(
        "─────────────────────────────────────────────────────────────────────",
      )
      io.println("Pattern: " <> pattern_name(question.pattern))
      io.println(
        "─────────────────────────────────────────────────────────────────────",
      )
      io.println("")
      io.println("Question: " <> question.question)
      io.println("")
      io.println("Example:")
      io.println("  " <> question.example)
      io.println("")
      io.println("Template:")
      io.println("  " <> question.template)
      io.println("")
      io.println("Why this matters:")
      io.println("  " <> question.why)
      io.println("")
      io.println("Enter your requirement (or 'skip' to move to next pattern):")
      io.println("")

      // Get user input
      case stdin.read_line() {
        Ok(response) -> {
          let response = string.trim(response)

          case response {
            "skip" | "SKIP" -> {
              // Skip this pattern
              io.println("")
              io.println("⊳ Skipping " <> pattern_name(question.pattern))
              io.println("")
              run_ears_interview_loop(ears_interview.advance_pattern(session))
            }
            "" -> {
              // Empty input, prompt again
              io.println("")
              cli_ui.print_error("Please enter a requirement or 'skip'")
              io.println("")
              run_ears_interview_loop(session)
            }
            _ -> {
              // Process the response
              case ears_interview.process_response(session, response) {
                Ok(result) -> {
                  // Show validation notes
                  case result.validation_notes {
                    [] -> Nil
                    notes -> {
                      io.println("")
                      io.println("⚠ Validation notes:")
                      list.each(notes, fn(note) { io.println("  • " <> note) })
                    }
                  }

                  // Show suggestions
                  case result.suggestions {
                    [] -> Nil
                    suggestions -> {
                      io.println("")
                      io.println("💡 Suggestions:")
                      list.each(suggestions, fn(s) { io.println("  • " <> s) })
                    }
                  }

                  io.println("")
                  io.println("✓ Requirement captured!")
                  io.println("")

                  // Add to session and advance
                  let session =
                    session
                    |> ears_interview.add_requirement(result.requirement)
                    |> ears_interview.advance_pattern

                  run_ears_interview_loop(session)
                }
                Error(ears_interview.InvalidRequirement(reason)) -> {
                  io.println("")
                  cli_ui.print_error("Invalid requirement: " <> reason)
                  io.println("Please try again using the EARS template.")
                  io.println("")
                  run_ears_interview_loop(session)
                }
                Error(ears_interview.PatternMismatch(_, _)) -> {
                  io.println("")
                  cli_ui.print_error(
                    "This doesn't match the "
                    <> pattern_name(question.pattern)
                    <> " pattern.",
                  )
                  io.println("Please use the template: " <> question.template)
                  io.println("")
                  run_ears_interview_loop(session)
                }
                Error(ears_interview.ParsingFailed(errors)) -> {
                  io.println("")
                  cli_ui.print_error("Parsing errors:")
                  list.each(errors, fn(e) {
                    io.println(
                      "  Line " <> string.inspect(e.line) <> ": " <> e.message,
                    )
                    io.println("  Suggestion: " <> e.suggestion)
                  })
                  io.println("")
                  run_ears_interview_loop(session)
                }
              }
            }
          }
        }
        Error(_) -> {
          cli_ui.print_error("Failed to read input")
          halt(exit_error)
        }
      }
    }
  }
}

/// Get pattern name for display
fn pattern_name(pattern: ears_interview.EarsPattern) -> String {
  case pattern {
    ears_interview.Ubiquitous -> "Ubiquitous"
    ears_interview.EventDriven -> "Event-Driven"
    ears_interview.StateDriven -> "State-Driven"
    ears_interview.Optional -> "Optional"
    ears_interview.Unwanted -> "Unwanted"
    ears_interview.Complex -> "Complex"
  }
}

/// Get current timestamp in milliseconds (simplified)
fn timestamp_ms() -> Int {
  // Simple timestamp - in real implementation would use proper time library
  1_000_000
}

// =============================================================================
// LATTICE ANALYZE COMMAND
// =============================================================================

/// The `lattice-analyze` command - apply 5 thinking models to requirements
fn lattice_analyze_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    case input.args {
      [requirements_file, ..] -> {
        // Load requirements from file
        io.println("Loading requirements from: " <> requirements_file)

        case simplifile.read(requirements_file) {
          Ok(content) -> {
            // Parse EARS requirements
            let parse_result = ears_parser.parse(content)

            case parse_result.requirements {
              [] -> {
                cli_ui.print_error("No valid requirements found in file")
                halt(exit_error)
              }
              requirements -> {
                io.println(
                  "Analyzing "
                  <> string.inspect(list.length(requirements))
                  <> " requirements with 5 mental lattice models...",
                )
                io.println("")

                // Create lattice session
                let session_id = "lattice-" <> string.inspect(timestamp_ms())
                case mental_lattice.new_session(session_id, requirements) {
                  Ok(session) -> {
                    // Run analysis
                    case mental_lattice.analyze_session(session) {
                      Ok(analyzed_session) -> {
                        case output_json {
                          True -> output_lattice_json(analyzed_session)
                          False -> output_lattice_text(analyzed_session)
                        }
                        halt(exit_pass)
                      }
                      Error(err) -> {
                        cli_ui.print_error(
                          "Analysis failed: " <> describe_lattice_error(err),
                        )
                        halt(exit_error)
                      }
                    }
                  }
                  Error(err) -> {
                    cli_ui.print_error(
                      "Failed to create session: "
                      <> describe_lattice_error(err),
                    )
                    halt(exit_error)
                  }
                }
              }
            }
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read file: " <> requirements_file)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error("Usage: intent lattice-analyze <requirements-file>")
        io.println("")
        io.println("Analyze EARS requirements using 5 mental lattice models:")
        io.println("  1. Inversion - What could fail?")
        io.println("  2. Second-Order Effects - What happens after?")
        io.println("  3. Pre-Mortem - Imagine this failed, why?")
        io.println("  4. Checklist - What are we missing?")
        io.println("  5. Circle of Competence - What's in scope?")
        io.println("")
        io.println("Options:")
        io.println("  --json    Output as JSON instead of human-readable")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Apply 5 mental lattice models to analyze EARS requirements",
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
  )
}

/// Output lattice analysis in human-readable format
fn output_lattice_text(session: mental_lattice.LatticeSession) -> Nil {
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("              MENTAL LATTICE ANALYSIS COMPLETE")
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("")
  io.println(
    "Analyzed "
    <> string.inspect(list.length(session.requirements))
    <> " requirements using "
    <> string.inspect(list.length(session.completed_models))
    <> " mental models",
  )
  io.println("")

  // Output by requirement
  list.each(session.requirements, fn(req) {
    io.println(
      "─────────────────────────────────────────────────────────────────",
    )
    io.println("Requirement: " <> req.raw_text)
    io.println(
      "─────────────────────────────────────────────────────────────────",
    )
    io.println("")

    let analyses = mental_lattice.get_analyses_for_requirement(session, req)

    list.each(analyses, fn(analysis) {
      io.println(
        "▸ "
        <> mental_lattice.model_name(analysis.model)
        <> " (confidence: "
        <> float.to_string(analysis.confidence)
        <> ")",
      )
      io.println("  " <> mental_lattice.model_description(analysis.model))
      io.println("")

      case analysis.insights {
        [] -> Nil
        insights -> {
          io.println("  Insights:")
          list.each(insights, fn(insight) { io.println("    • " <> insight) })
          io.println("")
        }
      }

      case analysis.warnings {
        [] -> Nil
        warnings -> {
          io.println("  ⚠️  Warnings:")
          list.each(warnings, fn(warning) { io.println("    • " <> warning) })
          io.println("")
        }
      }

      case analysis.questions {
        [] -> Nil
        questions -> {
          io.println("  Questions:")
          list.each(questions, fn(question) { io.println("    ? " <> question) })
          io.println("")
        }
      }
    })

    io.println("")
  })
}

/// Output lattice analysis as JSON
fn output_lattice_json(session: mental_lattice.LatticeSession) -> Nil {
  // Simplified JSON output - in real implementation would use gleam/json encoder
  io.println("{")
  io.println("  \"session_id\": \"" <> session.session_id <> "\",")
  io.println(
    "  \"requirements_count\": "
    <> string.inspect(list.length(session.requirements))
    <> ",",
  )
  io.println(
    "  \"models_applied\": "
    <> string.inspect(list.length(session.completed_models))
    <> ",",
  )
  io.println(
    "  \"analyses_count\": " <> string.inspect(list.length(session.analyses)),
  )
  io.println("}")
}

/// Describe a lattice error in human-readable format
fn describe_lattice_error(error: mental_lattice.LatticeError) -> String {
  case error {
    mental_lattice.InvalidRequirement(reason) ->
      "Invalid requirement: " <> reason
    mental_lattice.AnalysisFailed(model, reason) ->
      "Analysis failed for "
      <> mental_lattice.model_name(model)
      <> ": "
      <> reason
    mental_lattice.NoRequirementsProvided -> "No requirements provided"
  }
}

// =============================================================================
// GENERATE CONTRACT COMMAND
// =============================================================================

/// The `generate-contract` command - generate KIRK contracts from EARS requirements
fn generate_contract_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    case input.args {
      [requirements_file, ..] -> {
        // Load requirements from file
        io.println("Loading requirements from: " <> requirements_file)

        case simplifile.read(requirements_file) {
          Ok(content) -> {
            // Parse EARS requirements
            let parse_result = ears_parser.parse(content)

            case parse_result.requirements {
              [] -> {
                cli_ui.print_error("No valid requirements found in file")
                halt(exit_error)
              }
              requirements -> {
                io.println(
                  "Generating KIRK contracts for "
                  <> string.inspect(list.length(requirements))
                  <> " requirements...",
                )
                io.println("")

                // Generate contracts
                case kirk_contract.generate_contracts(requirements) {
                  Ok(contracts) -> {
                    case output_json {
                      True -> output_contracts_json(contracts)
                      False -> output_contracts_text(contracts)
                    }
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    cli_ui.print_error(
                      "Contract generation failed: "
                      <> describe_contract_error(err),
                    )
                    halt(exit_error)
                  }
                }
              }
            }
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read file: " <> requirements_file)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error(
          "Usage: intent generate-contract <requirements-file>",
        )
        io.println("")
        io.println(
          "Generate KIRK Design-by-Contract specifications from EARS requirements.",
        )
        io.println("")
        io.println("Contract elements:")
        io.println("  • Preconditions - What must be true before execution")
        io.println("  • Postconditions - What must be true after execution")
        io.println(
          "  • Second-Order Effects - What happens after this action",
        )
        io.println("")
        io.println("Options:")
        io.println("  --json    Output as JSON instead of human-readable")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Generate KIRK contracts from EARS requirements")
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
  )
}

/// Output contracts in human-readable format
fn output_contracts_text(contracts: List(kirk_contract.KirkContract)) -> Nil {
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("              KIRK CONTRACT GENERATION COMPLETE")
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("")
  io.println(
    "Generated " <> string.inspect(list.length(contracts)) <> " KIRK contracts",
  )
  io.println("")

  // Output each contract
  list.each(contracts, fn(contract) {
    io.println(kirk_contract.format_contract(contract))
    io.println("")
    io.println("Confidence: " <> float.to_string(contract.confidence))
    io.println("")
    io.println(
      "─────────────────────────────────────────────────────────────────",
    )
    io.println("")
  })
}

/// Output contracts as JSON (simplified)
fn output_contracts_json(contracts: List(kirk_contract.KirkContract)) -> Nil {
  // Simplified JSON output
  io.println("{")
  io.println(
    "  \"contracts_count\": " <> string.inspect(list.length(contracts)) <> ",",
  )
  io.println("  \"contracts\": [")

  let contract_strings =
    list.map(contracts, fn(contract) {
      let precond_count =
        list.length(contract.preconditions.required_fields)
        + list.length(contract.preconditions.field_constraints)
        + case contract.preconditions.auth_required {
          True -> 1
          False -> 0
        }

      let postcond_count =
        list.length(contract.postconditions.state_changes)
        + list.length(contract.postconditions.response_guarantees)

      "    {\"requirement\": \""
      <> contract.requirement.raw_text
      <> "\", \"preconditions\": "
      <> string.inspect(precond_count)
      <> ", \"postconditions\": "
      <> string.inspect(postcond_count)
      <> ", \"effects\": "
      <> string.inspect(list.length(contract.second_order_effects))
      <> ", \"confidence\": "
      <> float.to_string(contract.confidence)
      <> "}"
    })

  io.println(string.join(contract_strings, ",\n"))

  io.println("  ]")
  io.println("}")
}

/// Describe a contract error in human-readable format
fn describe_contract_error(error: kirk_contract.ContractError) -> String {
  case error {
    kirk_contract.InvalidRequirement(reason) ->
      "Invalid requirement: " <> reason
    kirk_contract.GenerationFailed(reason) -> "Generation failed: " <> reason
    kirk_contract.NoContractGenerated -> "No contract generated"
  }
}

// =============================================================================
// PLAN STRUCTURE COMMAND
// =============================================================================

/// The `plan-structure` command - organize contracts into epic/feature/task hierarchy
fn plan_structure_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let project_name =
      flag.get_string(input.flags, "project")
      |> result.unwrap("Project")

    case input.args {
      [requirements_file, ..] -> {
        // Load requirements from file
        io.println("Loading requirements from: " <> requirements_file)

        case simplifile.read(requirements_file) {
          Ok(content) -> {
            // Parse EARS requirements
            let parse_result = ears_parser.parse(content)

            case parse_result.requirements {
              [] -> {
                cli_ui.print_error("No valid requirements found in file")
                halt(exit_error)
              }
              requirements -> {
                io.println(
                  "Planning structure for "
                  <> string.inspect(list.length(requirements))
                  <> " requirements...",
                )
                io.println("")

                // Generate contracts first
                case kirk_contract.generate_contracts(requirements) {
                  Ok(contracts) -> {
                    // Plan structure from contracts
                    case
                      structure_planner.plan_structure(project_name, contracts)
                    {
                      Ok(structure) -> {
                        case output_json {
                          True -> output_structure_json(structure)
                          False -> output_structure_text(structure)
                        }
                        halt(exit_pass)
                      }
                      Error(err) -> {
                        cli_ui.print_error(
                          "Structure planning failed: "
                          <> describe_structure_error(err),
                        )
                        halt(exit_error)
                      }
                    }
                  }
                  Error(contract_err) -> {
                    cli_ui.print_error(
                      "Contract generation failed: "
                      <> describe_contract_error(contract_err),
                    )
                    halt(exit_error)
                  }
                }
              }
            }
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read file: " <> requirements_file)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error("Usage: intent plan-structure <requirements-file>")
        io.println("")
        io.println("Organize EARS requirements into project structure:")
        io.println(
          "  • Epic: High-level grouping (e.g., Authentication System)",
        )
        io.println(
          "  • Feature: Cohesive set of behaviors (e.g., User Login)",
        )
        io.println("  • Task: Individual implementable unit")
        io.println("")
        io.println(
          "Wave-based dependency analysis enables parallel execution planning.",
        )
        io.println("")
        io.println("Options:")
        io.println("  --project=NAME    Project name (default: 'Project')")
        io.println(
          "  --json            Output as JSON instead of human-readable",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Organize contracts into epic/feature/task hierarchy with wave-based dependencies",
  )
  |> glint.flag(
    "project",
    flag.string()
      |> flag.default("Project")
      |> flag.description("Project name"),
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
  )
}

/// Output structure in human-readable format
fn output_structure_text(structure: structure_planner.ProjectStructure) -> Nil {
  io.println(structure_planner.format_structure(structure))
}

/// Output structure as JSON (simplified)
fn output_structure_json(structure: structure_planner.ProjectStructure) -> Nil {
  // Simplified JSON output
  io.println("{")
  io.println("  \"project_name\": \"" <> structure.project_name <> "\",")
  io.println(
    "  \"total_tasks\": " <> string.inspect(structure.total_tasks) <> ",",
  )
  io.println(
    "  \"total_waves\": " <> string.inspect(structure.total_waves) <> ",",
  )
  io.println(
    "  \"parallelism_score\": "
    <> float.to_string(structure.parallelism_score)
    <> ",",
  )
  io.println(
    "  \"epics_count\": " <> string.inspect(list.length(structure.epics)),
  )
  io.println("}")
}

/// Describe a structure planning error
fn describe_structure_error(
  error: structure_planner.StructurePlanError,
) -> String {
  case error {
    structure_planner.NoContractsProvided -> "No contracts provided"
    structure_planner.CyclicDependency(task_ids) ->
      "Cyclic dependency detected in tasks: " <> string.join(task_ids, ", ")
    structure_planner.InvalidStructure(reason) ->
      "Invalid structure: " <> reason
  }
}

// =============================================================================
// GENERATE BEADS COMMAND
// =============================================================================

/// The `generate-beads` command - generate bd beads from EARS requirements
fn generate_beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let project_name =
      flag.get_string(input.flags, "project")
      |> result.unwrap("Project")

    case input.args {
      [requirements_file, ..] -> {
        // Load requirements from file
        io.println("Loading requirements from: " <> requirements_file)

        case simplifile.read(requirements_file) {
          Ok(content) -> {
            // Parse EARS requirements
            let parse_result = ears_parser.parse(content)

            case parse_result.requirements {
              [] -> {
                cli_ui.print_error("No valid requirements found in file")
                halt(exit_error)
              }
              requirements -> {
                io.println(
                  "Processing "
                  <> string.inspect(list.length(requirements))
                  <> " requirements...",
                )

                // Generate contracts
                case kirk_contract.generate_contracts(requirements) {
                  Ok(contracts) -> {
                    io.println(
                      "Generated "
                      <> string.inspect(list.length(contracts))
                      <> " contracts",
                    )

                    // Plan structure from contracts
                    case
                      structure_planner.plan_structure(project_name, contracts)
                    {
                      Ok(structure) -> {
                        io.println(
                          "Planned structure with "
                          <> string.inspect(structure.total_tasks)
                          <> " tasks in "
                          <> string.inspect(structure.total_waves)
                          <> " waves",
                        )
                        io.println("")

                        // Generate beads from structure
                        case bead_generator.generate_beads(structure) {
                          Ok(result) -> {
                            case output_json {
                              True -> output_beads_result_json(result)
                              False -> output_beads_result_text(result)
                            }
                            halt(exit_pass)
                          }
                          Error(err) -> {
                            cli_ui.print_error(
                              "Bead generation failed: "
                              <> bead_generator.describe_error(err),
                            )
                            halt(exit_error)
                          }
                        }
                      }
                      Error(structure_err) -> {
                        cli_ui.print_error(
                          "Structure planning failed: "
                          <> describe_structure_error(structure_err),
                        )
                        halt(exit_error)
                      }
                    }
                  }
                  Error(contract_err) -> {
                    cli_ui.print_error(
                      "Contract generation failed: "
                      <> describe_contract_error(contract_err),
                    )
                    halt(exit_error)
                  }
                }
              }
            }
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read file: " <> requirements_file)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error("Usage: intent generate-beads <requirements-file>")
        io.println("")
        io.println("Generate bd beads from EARS requirements:")
        io.println("  1. Parse requirements")
        io.println("  2. Generate KIRK contracts")
        io.println("  3. Plan epic/feature/task structure")
        io.println("  4. Create beads in bd database")
        io.println("")
        io.println("This creates trackable work items with:")
        io.println("  • Proper hierarchy (epic → feature → task)")
        io.println("  • Wave-based dependencies")
        io.println("  • Contract metadata embedded")
        io.println("")
        io.println("Options:")
        io.println("  --project=NAME    Project name (default: 'Project')")
        io.println(
          "  --json            Output as JSON instead of human-readable",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Generate bd beads from EARS requirements with hierarchy and dependencies",
  )
  |> glint.flag(
    "project",
    flag.string()
      |> flag.default("Project")
      |> flag.description("Project name"),
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
  )
}

/// Output bead generation result in human-readable format
fn output_beads_result_text(result: bead_generator.GenerationResult) -> Nil {
  io.println(bead_generator.format_result(result))
}

/// Output bead generation result as JSON
fn output_beads_result_json(result: bead_generator.GenerationResult) -> Nil {
  io.println("{")
  io.println(
    "  \"total_created\": " <> string.inspect(result.total_created) <> ",",
  )
  io.println("  \"epic_count\": " <> string.inspect(result.epic_count) <> ",")
  io.println(
    "  \"feature_count\": " <> string.inspect(result.feature_count) <> ",",
  )
  io.println("  \"task_count\": " <> string.inspect(result.task_count) <> ",")
  io.println(
    "  \"failed_count\": " <> string.inspect(list.length(result.failed_beads)),
  )
  io.println("}")
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
                let json_obj =
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
                        json.object([
                          #("line", json.int(e.line)),
                          #("message", json.string(e.message)),
                          #("suggestion", json.string(e.suggestion)),
                        ])
                      }),
                    ),
                    #("warnings", json.array(result.warnings, json.string)),
                    #("count", json.int(req_count)),
                  ])
                io.println(json.to_string(json_obj))
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
                      io.println("Error parsing requirements:")
                      io.println(
                        "Line " <> string.inspect(e.line) <> ": " <> e.message,
                      )
                      io.println("  ❌ Does not match any EARS pattern")
                      io.println("  💡 Suggestion: " <> e.suggestion)
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
            cli_ui.print_error("Failed to read: " <> requirements_path)
            halt(exit_error)
          }
        }
      }
      [] -> {
        cli_ui.print_error("requirements file path required")
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
  |> glint.description("Parse EARS requirements to Intent behaviors")
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

fn answer_loader_error_to_string(err: answer_loader.AnswerLoaderError) -> String {
  case err {
    answer_loader.FileNotFound(path) -> "File not found: " <> path
    answer_loader.PermissionDenied(path) ->
      "Permission denied reading: " <> path
    answer_loader.ParseError(path, msg) ->
      "Parse error in " <> path <> ": " <> msg
    answer_loader.SchemaError(msg) -> "Schema validation failed: " <> msg
    answer_loader.IoError(msg) -> "I/O error: " <> msg
  }
}

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

@external(erlang, "intent_ffi", "generate_uuid")
fn generate_uuid() -> String

@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String
