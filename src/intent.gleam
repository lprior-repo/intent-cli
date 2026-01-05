/// Intent CLI - Human-writes, AI-verifies, AI-implements
/// Contract-driven API testing tool

import argv
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/loader
import intent/output
import intent/runner
import intent/types
import intent/quality_analyzer
import intent/spec_linter
import intent/improver
import intent/interview
import intent/interview_session
import intent/interview_storage
import intent/interview_questions
import intent/spec_builder
import intent/bead_templates
import intent/stdin
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

    let is_verbose =
      flag.get_bool(input.flags, "verbose")
      |> result.unwrap(False)

    case input.args {
      [spec_path, ..] -> {
        run_check(
          spec_path,
          target_url,
          is_json,
          feature_filter,
          only_filter,
          is_verbose,
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
  |> glint.flag("target", flag.string() |> flag.default("") |> flag.description("Target base URL to test against"))
  |> glint.flag("json", flag.bool() |> flag.default(False) |> flag.description("Output results as JSON"))
  |> glint.flag("feature", flag.string() |> flag.default("") |> flag.description("Filter to a specific feature"))
  |> glint.flag("only", flag.string() |> flag.default("") |> flag.description("Run only a specific behavior"))
  |> glint.flag("verbose", flag.bool() |> flag.default(False) |> flag.description("Verbose output"))
}

fn run_check(
  spec_path: String,
  target_url: String,
  is_json: Bool,
  feature_filter: String,
  only_filter: String,
  verbose: Bool,
) -> Nil {
  // Load the spec
  case loader.load_spec(spec_path) {
    Error(e) -> {
      io.println_error("Error: " <> loader.format_error(e))
      halt(exit_invalid)
    }
    Ok(spec) -> {
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
          verbose: verbose,
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
        output.SpecResult(pass: True, ..) -> exit_pass
        output.SpecResult(blocked: blocked, ..) if blocked > 0 -> exit_blocked
        _ -> exit_fail
      }
      halt(exit_code)
    }
  }
}

/// The `validate` command - validate CUE spec without running
fn validate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.validate_cue(spec_path) {
          Ok(_) -> {
            io.println("Valid: " <> spec_path)
            halt(exit_pass)
          }
          Error(e) -> {
            io.println_error("Invalid: " <> loader.format_error(e))
            halt(exit_invalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent validate <spec.cue>")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Validate a CUE spec file without running tests")
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
  |> glint.flag("json", flag.bool() |> flag.default(False) |> flag.description("Output as JSON"))
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
  |> glint.description("Analyze spec quality and provide improvement suggestions")
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
  |> glint.description("Suggest improvements based on quality analysis and linting")
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

    let json_input =
      flag.get_string(input.flags, "answers")
      |> result.unwrap("")

    case resume_id {
      // Resume an existing session
      "" ->
        case string.lowercase(profile_str) {
          "api" -> run_interview(interview.Api, json_input, export_to)
          "cli" -> run_interview(interview.Cli, json_input, export_to)
          "event" -> run_interview(interview.Event, json_input, export_to)
          "data" -> run_interview(interview.Data, json_input, export_to)
          "workflow" -> run_interview(interview.Workflow, json_input, export_to)
          "ui" -> run_interview(interview.UI, json_input, export_to)
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

      // Resume an existing session
      id -> {
        io.println("Resuming interview session: " <> id)
        io.println_error("Session resume not yet implemented")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Guided specification discovery through structured interview")
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
    |> flag.description("Path to YAML/JSON file with pre-filled answers"),
  )
  |> glint.flag(
    "export",
    flag.string()
    |> flag.default("")
    |> flag.description("Export completed interview to spec file"),
  )
}

fn run_interview(
  profile: interview.Profile,
  _json_input: String,
  export_to: String,
) -> Nil {
  // Initialize session
  let session_id = "interview-" <> generate_uuid()
  let timestamp = current_timestamp()

  let session = interview_session.start_interview(profile, session_id, timestamp)

  // Print welcome message
  io.println("")
  io.println("═══════════════════════════════════════════════════════════════════")
  io.println("                    INTENT INTERVIEW")
  io.println("═══════════════════════════════════════════════════════════════════")
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
  let save_result = interview_storage.append_session_to_jsonl(
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

/// Main interview loop - asks questions round by round
fn interview_loop(session: interview.InterviewSession, round: Int) -> interview.InterviewSession {
  case round > 5 {
    True -> session
    False -> {
      io.println("")
      io.println("═══════════════════════════════════════════════════════════════════")
      io.println("ROUND " <> string.inspect(round) <> "/5")
      io.println("═══════════════════════════════════════════════════════════════════")
      io.println("")

      // Get questions for this round
      case interview_session.get_first_question_for_round(session, round) {
        Error(_) -> {
          io.println("(No questions for this round)")
          interview_loop(session, round + 1)
        }
        Ok(first_question) -> {
          // Ask all questions in this round
          let updated_session = ask_questions_in_round(session, round, first_question)

          // Check for blocking gaps before proceeding
          let blocking_gaps = interview_session.get_blocking_gaps(updated_session)
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
  _current_question: interview_questions.Question,
) -> interview.InterviewSession {
  let profile_str = profile_to_string(session.profile)

  // Get all questions for this round
  let questions = interview_questions.get_questions_for_round(profile_str, round)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  // Filter to unanswered questions
  let unanswered = list.filter(questions, fn(q) {
    !list.contains(answered_ids, q.id)
  })

  // Ask each unanswered question
  list.fold(unanswered, session, fn(sess, question) {
    ask_single_question(sess, question, round)
  })
}

/// Ask a single question and collect answer
fn ask_single_question(
  session: interview.InterviewSession,
  question: interview_questions.Question,
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
  let extracted = interview.extract_from_answer(
    question.id,
    answer_text,
    question.extract_into,
  )

  // Calculate confidence
  let confidence = interview.calculate_confidence(question.id, answer_text, extracted)

  // Create answer record
  let answer = interview.Answer(
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
  let updated_session = interview_session.add_answer(session, answer)

  // Check for gaps and conflicts
  let #(sess_with_gaps, _gaps) =
    interview_session.check_for_gaps(updated_session, question, answer)

  let #(sess_final, _conflicts) =
    interview_session.check_for_conflicts(sess_with_gaps, answer)

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

/// The `beads` command - generate work items from interview session
fn beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [session_id, ..] -> {
        // Load session from JSONL
        case interview_storage.get_session_from_jsonl(
          ".interview/sessions.jsonl",
          session_id,
        ) {
          Error(err) -> {
            io.println_error("Error: " <> err)
            halt(exit_error)
          }
          Ok(session) -> {
            // Generate beads from session
            let beads = bead_templates.generate_beads_from_session(session)
            let bead_count = list.length(beads)

            io.println("")
            io.println("═══════════════════════════════════════════════════════════════════")
            io.println("                    BEAD GENERATION")
            io.println("═══════════════════════════════════════════════════════════════════")
            io.println("")
            io.println("Generated " <> string.inspect(bead_count) <> " work items from session: " <> session_id)
            io.println("")

            // Export to .beads/issues.jsonl
            let jsonl_output = bead_templates.beads_to_jsonl(beads)

            case simplifile.append(".beads/issues.jsonl", jsonl_output <> "\n") {
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
                io.println_error("✗ Failed to write beads: " <> string.inspect(err))
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

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

@external(erlang, "intent_ffi", "generate_uuid")
fn generate_uuid() -> String

@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String
