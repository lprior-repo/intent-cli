/// Intent CLI - Human-writes, AI-verifies, AI-implements
/// Contract-driven API testing tool
import argv
import gleam/dict
import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/bead_feedback
import intent/bead_templates
import intent/cli_ui
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
import intent/loader
import intent/output
import intent/plan_mode
import intent/quality_analyzer
import intent/question_types.{type Question}
import intent/runner
import intent/security
import intent/spec_builder
import intent/spec_linter
import intent/stdin
import intent/types
import shellout
import simplifile

/// Exit codes
const exit_pass = 0

const exit_fail = 1

const exit_blocked = 2

const exit_invalid = 3

const exit_error = 4

pub fn main() {
  let normalized_args = normalize_cli_args(argv.load().arguments)

  let app =
    glint.new()
    |> glint.with_name("intent")
    |> glint.with_pretty_help(glint.default_pretty_help())
    |> glint.add(at: ["check"], do: check_command())
    |> glint.add(at: ["validate"], do: validate_command())
    |> glint.add(at: ["validate-bead"], do: validate_bead_command())
    |> glint.add(at: ["show"], do: show_command())
    |> glint.add(at: ["export"], do: export_command())
    |> glint.add(at: ["lint"], do: lint_command())
    |> glint.add(at: ["analyze"], do: analyze_command())
    |> glint.add(at: ["improve"], do: improve_command())
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
    |> glint.add(at: ["compact"], do: kirk_compact_command())
    |> glint.add(at: ["prototext"], do: kirk_prototext_command())
    |> glint.add(at: ["ears"], do: kirk_ears_command())
    |> glint.add(at: ["effects"], do: kirk_effects_command())
    // Plan commands
    |> glint.add(at: ["plan"], do: plan_command())
    |> glint.add(at: ["plan-approve"], do: plan_approve_command())
    |> glint.add(at: ["beads-regenerate"], do: beads_regenerate_command())

  case glint.execute(app, normalized_args) {
    Ok(glint.Out(_)) -> Nil
    Ok(glint.Help(help_text)) -> io.println(help_text)
    Error(err) -> {
      io.println_error(err)
      halt(exit_error)
    }
  }
}

pub fn normalize_cli_args(args: List(String)) -> List(String) {
  case args {
    [arg, next, ..rest] -> {
      case classify_flag(arg) {
        BoolFlag -> {
          case is_bool_literal(next) {
            True -> [
              arg <> "=" <> string.lowercase(next),
              ..normalize_cli_args(rest)
            ]
            False -> [arg <> "=true", ..normalize_cli_args([next, ..rest])]
          }
        }
        ValueFlag -> {
          case is_flag_token(next) {
            True -> [arg, ..normalize_cli_args([next, ..rest])]
            False -> [arg <> "=" <> next, ..normalize_cli_args(rest)]
          }
        }
        UnknownFlag -> [arg, ..normalize_cli_args([next, ..rest])]
      }
    }
    [arg] -> {
      case classify_flag(arg) {
        BoolFlag -> [arg <> "=true"]
        _ -> [arg]
      }
    }
    [] -> []
  }
}

type FlagKind {
  BoolFlag
  ValueFlag
  UnknownFlag
}

fn classify_flag(arg: String) -> FlagKind {
  let is_candidate = string.starts_with(arg, "--") && !string.contains(arg, "=")
  bool_to_flag_kind(is_candidate, string.drop_left(arg, 2))
}

fn bool_to_flag_kind(is_candidate: Bool, flag_name: String) -> FlagKind {
  case is_candidate {
    False -> UnknownFlag
    True -> {
      case is_known_bool_flag(flag_name) {
        True -> BoolFlag
        False -> {
          case is_known_value_flag(flag_name) {
            True -> ValueFlag
            False -> UnknownFlag
          }
        }
      }
    }
  }
}

fn is_known_bool_flag(flag_name: String) -> Bool {
  case flag_name {
    "json" -> True
    "verbose" -> True
    "quiet" -> True
    "yes" -> True
    "tokens" -> True
    "draft" -> True
    _ -> False
  }
}

fn is_known_value_flag(flag_name: String) -> Bool {
  case flag_name {
    "target" -> True
    "feature" -> True
    "only" -> True
    "profile" -> True
    "resume" -> True
    "answer" -> True
    "export" -> True
    "bead-id" -> True
    "status" -> True
    "reason" -> True
    "session" -> True
    "format" -> True
    "notes" -> True
    "strategy" -> True
    "output" -> True
    "out" -> True
    "name" -> True
    "export-answers-template" -> True
    _ -> False
  }
}

fn is_flag_token(value: String) -> Bool {
  string.starts_with(value, "--")
}

fn is_bool_literal(value: String) -> Bool {
  case string.lowercase(value) {
    "true" -> True
    "false" -> True
    _ -> False
  }
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

    let output_level = case is_json {
      True -> runner.Quiet
      False ->
        case flag.get_bool(input.flags, "verbose") {
          Ok(True) -> runner.Verbose
          _ ->
            case flag.get_bool(input.flags, "quiet") {
              Ok(True) -> runner.Quiet
              _ -> runner.Normal
            }
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
  // Load the spec
  let load_result = case is_json {
    True -> loader.load_spec_quiet(spec_path)
    False -> loader.load_spec(spec_path)
  }

  case load_result {
    Error(e) -> {
      cli_ui.print_error(loader.format_error(e))
      halt(exit_invalid)
    }
    Ok(spec) -> {
      case is_json {
        True -> Nil
        False -> cli_ui.print_header("Checking spec: " <> spec.name)
      }

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
          case is_json {
            True -> Nil
            False -> cli_ui.print_success("All checks passed!")
          }
          exit_pass
        }
        output.SpecResult(blocked: blocked, ..) if blocked > 0 -> {
          case is_json {
            True -> Nil
            False -> cli_ui.print_warning("Blocked behaviors detected")
          }
          exit_blocked
        }
        _ -> {
          case is_json {
            True -> Nil
            False -> cli_ui.print_error("Check failed")
          }
          exit_fail
        }
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
  |> glint.description("Validate a CUE spec file without running tests")
}

/// The `validate-bead` command - validate planner bead CUE against enhanced schema
fn validate_bead_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let draft_mode =
      flag.get_bool(input.flags, "draft")
      |> result.unwrap(False)

    let definition = case draft_mode {
      True -> "#DraftBead"
      False -> "#ValidBead"
    }

    case input.args {
      [bead_path, ..] -> {
        case security.validate_file_path(bead_path) {
          Error(security_error) -> {
            io.println_error(
              "Error: " <> security.format_security_error(security_error),
            )
            halt(exit_error)
          }
          Ok(validated_path) -> {
            case
              shellout.command(
                "cue",
                [
                  "vet",
                  "schema/enhanced-bead.cue",
                  validated_path,
                  "-d",
                  definition,
                ],
                ".",
                [],
              )
            {
              Ok(_) -> {
                cli_ui.print_success(
                  "Valid bead (" <> definition <> "): " <> validated_path,
                )
                halt(exit_pass)
              }
              Error(#(_, stderr)) -> {
                cli_ui.print_error("Bead validation failed:\n" <> stderr)
                halt(exit_invalid)
              }
            }
          }
        }
      }
      [] -> {
        io.println_error("Error: bead file path required")
        io.println_error(
          "Usage: intent validate-bead <bead.cue> [--draft=true]",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Validate planner bead CUE against schema/enhanced-bead.cue",
  )
  |> glint.flag(
    "draft",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Validate against #DraftBead instead of #ValidBead"),
  )
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

    let session_id =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    let answer_text =
      flag.get_string(input.flags, "answer")
      |> result.unwrap("")

    let export_answers_template =
      flag.get_string(input.flags, "export-answers-template")
      |> result.unwrap("")

    case export_answers_template {
      "" -> Nil
      path -> {
        case profile_str {
          "api" -> export_answers_template_for_profile(interview.Api, path)
          "cli" -> export_answers_template_for_profile(interview.Cli, path)
          "event" -> export_answers_template_for_profile(interview.Event, path)
          "data" -> export_answers_template_for_profile(interview.Data, path)
          "workflow" ->
            export_answers_template_for_profile(interview.Workflow, path)
          "ui" -> export_answers_template_for_profile(interview.UI, path)
          _ -> {
            io.println_error(
              "Invalid profile: "
              <> profile_str
              <> " (must be api, cli, event, data, workflow, or ui)",
            )
            halt(exit_error)
          }
        }
      }
    }

    run_ai_interview(profile_str, resume_id, session_id, answer_text, export_to)
  })
  |> glint.description(
    "AI-guided specification discovery for plan-ready requirements",
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
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Existing interview session ID to continue (alias of --resume)",
      ),
  )
  |> glint.flag(
    "answer",
    flag.string()
      |> flag.default("")
      |> flag.description("Submit answer text for the next question"),
  )
  |> glint.flag(
    "export",
    flag.string()
      |> flag.default("")
      |> flag.description("Export completed interview to spec file"),
  )
  |> glint.flag(
    "export-answers-template",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Export a JSON template of required interview answers and exit",
      ),
  )
}

fn export_answers_template_for_profile(
  profile: interview.Profile,
  output_path: String,
) -> Nil {
  let profile_str = profile_to_string(profile)
  let template = build_answers_template_json(profile_str)

  case output_path {
    "-" -> io.println(template)
    path -> {
      case simplifile.write(path, template) {
        Ok(Nil) -> io.println("✓ Answers template exported to: " <> path)
        Error(err) -> {
          io.println_error(
            "✗ Failed to export answers template: " <> string.inspect(err),
          )
          halt(exit_error)
        }
      }
    }
  }

  halt(exit_pass)
}

fn build_answers_template_json(profile: String) -> String {
  let all_questions =
    [1, 2, 3, 4, 5]
    |> list.flat_map(fn(round) {
      interview_questions.get_questions_for_round(profile, round)
    })

  let answer_entries =
    all_questions
    |> list.map(fn(q) { #(q.id, json.string("")) })

  let extract_key_entries =
    all_questions
    |> list.flat_map(fn(q) {
      q.extract_into
      |> list.map(fn(k) { #(k, json.string("")) })
    })

  let combined =
    answer_entries
    |> list.append(extract_key_entries)
    |> dedupe_template_entries

  combined
  |> json.object
  |> json.to_string
}

fn dedupe_template_entries(
  entries: List(#(String, json.Json)),
) -> List(#(String, json.Json)) {
  let by_key =
    entries
    |> list.fold(dict.new(), fn(acc, entry) {
      let #(key, value) = entry
      dict.insert(acc, key, value)
    })

  by_key
  |> dict.to_list
  |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
}

fn run_ai_interview(
  profile_str: String,
  resume_id: String,
  session_id: String,
  answer_text: String,
  export_to: String,
) -> Nil {
  case session_id != "" && resume_id != "" && session_id != resume_id {
    True -> {
      io.println(
        json.to_string(
          json.object([
            #("action", json.string("validation_error")),
            #(
              "error",
              json.object([
                #(
                  "message",
                  json.string(
                    "--session and --resume refer to different sessions",
                  ),
                ),
                #(
                  "suggestion",
                  json.string(
                    "Use only one session selector, or provide matching IDs",
                  ),
                ),
              ]),
            ),
          ]),
        ),
      )
      halt(exit_error)
    }
    False -> {
      let selected_session_id = case string.trim(session_id) {
        "" -> string.trim(resume_id)
        value -> value
      }

      let session_result = case selected_session_id {
        "" -> {
          case interview.string_to_profile(profile_str) {
            Error(err) -> Error(err)
            Ok(profile) -> {
              let now = current_timestamp()
              let created =
                interview.create_session(
                  "interview-" <> generate_uuid(),
                  profile,
                  now,
                )
              case save_interview_session(created) {
                Ok(Nil) -> Ok(created)
                Error(err) -> Error(err)
              }
            }
          }
        }
        id ->
          interview_storage.get_session_from_jsonl(
            ".interview/sessions.jsonl",
            id,
          )
      }

      case session_result {
        Error(err) -> {
          io.println(
            json.to_string(
              json.object([
                #("action", json.string("validation_error")),
                #(
                  "error",
                  json.object([
                    #("message", json.string(err)),
                    #(
                      "suggestion",
                      json.string(
                        "Start a session: intent interview --profile api",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
          )
          halt(exit_error)
        }
        Ok(session) -> {
          let maybe_next = next_unanswered_question(session)

          let result_session = case non_empty_answer(answer_text), maybe_next {
            Some(answer), Some(#(round, question)) -> {
              let updated =
                apply_ai_answer_to_session(session, question, round, answer)
              mark_session_complete_if_finished(updated)
            }
            Some(_), None -> mark_session_complete_if_finished(session)
            None, _ -> session
          }

          case save_interview_session(result_session) {
            Error(err) -> {
              io.println(
                json.to_string(
                  json.object([
                    #("action", json.string("validation_error")),
                    #(
                      "error",
                      json.object([
                        #("message", json.string(err)),
                        #(
                          "suggestion",
                          json.string("Ensure .interview/ is writable"),
                        ),
                      ]),
                    ),
                  ]),
                ),
              )
              halt(exit_error)
            }
            Ok(Nil) -> {
              case maybe_export_completed_spec(result_session, export_to) {
                Error(err) -> {
                  io.println(
                    json.to_string(
                      json.object([
                        #("action", json.string("validation_error")),
                        #("session", session_json(result_session)),
                        #(
                          "error",
                          json.object([
                            #("message", json.string(err)),
                            #(
                              "suggestion",
                              json.string("Check --export path permissions"),
                            ),
                          ]),
                        ),
                      ]),
                    ),
                  )
                  halt(exit_error)
                }
                Ok(spec_path) -> {
                  let next_after_update =
                    next_unanswered_question(result_session)
                  io.println(
                    json.to_string(ai_directive_json(
                      result_session,
                      next_after_update,
                      spec_path,
                    )),
                  )
                  halt(exit_pass)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn save_interview_session(
  session: interview.InterviewSession,
) -> Result(Nil, String) {
  interview_storage.append_session_to_jsonl(
    session,
    ".interview/sessions.jsonl",
  )
}

fn maybe_export_completed_spec(
  session: interview.InterviewSession,
  export_to: String,
) -> Result(option.Option(String), String) {
  case session.stage == interview.Complete && string.trim(export_to) != "" {
    False -> Ok(option.None)
    True -> {
      let spec_cue = spec_builder.build_spec_from_session(session)
      simplifile.write(export_to, spec_cue)
      |> result.map(fn(_) { option.Some(export_to) })
      |> result.map_error(fn(err) {
        "Failed to export spec: " <> string.inspect(err)
      })
    }
  }
}

fn ai_directive_json(
  session: interview.InterviewSession,
  next_question: option.Option(#(Int, Question)),
  spec_path: option.Option(String),
) -> json.Json {
  let base_fields = [
    #(
      "action",
      json.string(case next_question {
        Some(_) -> "ask_question"
        None -> "generate_beads"
      }),
    ),
    #("session", session_json(session)),
    #("progress", interview_progress_json(session)),
    #(
      "agent_protocol",
      json.object([
        #("target", json.string("claude_code")),
        #("contract_version", json.string("v1")),
        #("goal", json.string("turn user intent into plan-ready requirements")),
      ]),
    ),
  ]

  let with_question = case next_question {
    Some(#(round, question)) ->
      list.append(base_fields, [
        #("question", question_json(question, round)),
        #(
          "guidance",
          json.object([
            #("ask_exactly", json.bool(True)),
            #(
              "next_command",
              json.string(
                "intent interview --session "
                <> session.id
                <> " --answer \"<human answer>\"",
              ),
            ),
            #("planning_focus", json.string(planning_focus_for_round(round))),
          ]),
        ),
      ])
    None -> {
      let completion_fields = [
        #(
          "guidance",
          json.object([
            #("next_command", json.string("intent beads " <> session.id)),
            #(
              "planning_focus",
              json.string(
                "Turn this captured intent into atomic, dependency-aware work items",
              ),
            ),
          ]),
        ),
      ]

      case spec_path {
        Some(path) ->
          list.append(
            base_fields,
            list.append(completion_fields, [
              #("output", json.object([#("spec_path", json.string(path))])),
            ]),
          )
        None -> list.append(base_fields, completion_fields)
      }
    }
  }

  json.object(with_question)
}

fn session_json(session: interview.InterviewSession) -> json.Json {
  json.object([
    #("id", json.string(session.id)),
    #("profile", json.string(profile_to_string(session.profile))),
    #("created_at", json.string(session.created_at)),
    #("updated_at", json.string(session.updated_at)),
    #("stage", json.string(string.lowercase(string.inspect(session.stage)))),
  ])
}

fn interview_progress_json(session: interview.InterviewSession) -> json.Json {
  let profile = profile_to_string(session.profile)
  let total = total_questions_for_profile(profile)
  let asked = list.length(session.answers)
  let remain = case total - asked < 0 {
    True -> 0
    False -> total - asked
  }
  let percent = case total {
    0 -> 100
    _ -> {
      int.to_float(asked) /. int.to_float(total) *. 100.0
      |> float.round
    }
  }

  json.object([
    #("current_round", json.int(current_round_for_progress(session))),
    #("total_rounds", json.int(5)),
    #("questions_asked", json.int(asked)),
    #("questions_remaining", json.int(remain)),
    #("percent_complete", json.int(percent)),
  ])
}

fn current_round_for_progress(session: interview.InterviewSession) -> Int {
  case next_unanswered_question(session) {
    Some(#(round, _)) -> round
    None -> 5
  }
}

fn total_questions_for_profile(profile: String) -> Int {
  [1, 2, 3, 4, 5]
  |> list.fold(0, fn(acc, round) {
    acc
    + list.length(interview_questions.get_questions_for_round(profile, round))
  })
}

fn question_json(question: Question, round: Int) -> json.Json {
  let examples = case non_empty_answer(question.example) {
    Some(example) -> [example]
    None -> []
  }

  json.object([
    #("id", json.string(question.id)),
    #("round", json.int(round)),
    #("text", json.string(question.question)),
    #("pattern", json.string(pattern_for_category(question.category))),
    #("context", json.string(question.context)),
    #("examples", json.array(examples, json.string)),
    #(
      "priority",
      json.string(string.lowercase(string.inspect(question.priority))),
    ),
    #(
      "perspective",
      json.string(string.lowercase(string.inspect(question.perspective))),
    ),
    #("extract_into", json.array(question.extract_into, json.string)),
  ])
}

fn pattern_for_category(category: question_types.QuestionCategory) -> String {
  case category {
    question_types.HappyPath -> "ubiquitous"
    question_types.ErrorCase -> "event_driven"
    question_types.EdgeCase -> "unwanted"
    question_types.Constraint -> "state_driven"
    question_types.Dependency -> "optional"
    question_types.NonFunctional -> "complex"
  }
}

fn planning_focus_for_round(round: Int) -> String {
  case round {
    1 -> "Define the core user outcome and canonical happy path"
    2 -> "Pin down failure boundaries and status semantics"
    3 -> "Capture scale limits, abuse resistance, and adversarial behavior"
    4 -> "Lock security and compliance constraints before implementation"
    _ -> "Set production-readiness requirements: deployment, SLA, observability"
  }
}

fn next_unanswered_question(
  session: interview.InterviewSession,
) -> option.Option(#(Int, Question)) {
  let profile = profile_to_string(session.profile)
  let answered_ids =
    list.map(session.answers, fn(answer) { answer.question_id })
  find_next_question_for_round(profile, answered_ids, 1)
}

fn find_next_question_for_round(
  profile: String,
  answered_ids: List(String),
  round: Int,
) -> option.Option(#(Int, Question)) {
  case round > 5 {
    True -> option.None
    False -> {
      let questions =
        interview_questions.get_questions_for_round(profile, round)
      case first_unanswered_question(questions, answered_ids) {
        Some(question) -> option.Some(#(round, question))
        None -> find_next_question_for_round(profile, answered_ids, round + 1)
      }
    }
  }
}

fn first_unanswered_question(
  questions: List(Question),
  answered_ids: List(String),
) -> option.Option(Question) {
  case questions {
    [] -> option.None
    [question, ..rest] -> {
      case list.contains(answered_ids, question.id) {
        True -> first_unanswered_question(rest, answered_ids)
        False -> option.Some(question)
      }
    }
  }
}

fn apply_ai_answer_to_session(
  session: interview.InterviewSession,
  question: Question,
  round: Int,
  answer_text: String,
) -> interview.InterviewSession {
  let extracted =
    interview.extract_from_answer(
      question.id,
      answer_text,
      question.extract_into,
    )
  let confidence =
    interview.calculate_confidence(question.id, answer_text, extracted)

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

  let updated = interview.add_answer(session, answer)
  let #(with_gaps, _) = interview.check_for_gaps(updated, question, answer)
  let #(with_conflicts, _) = interview.check_for_conflicts(with_gaps, answer)
  maybe_mark_round_complete(with_conflicts, round)
}

fn maybe_mark_round_complete(
  session: interview.InterviewSession,
  round: Int,
) -> interview.InterviewSession {
  let profile = profile_to_string(session.profile)
  let total_round_questions =
    list.length(interview_questions.get_questions_for_round(profile, round))
  let answered_in_round =
    session.answers
    |> list.filter(fn(answer) { answer.round == round })
    |> list.length

  case total_round_questions > 0 && answered_in_round >= total_round_questions {
    True -> interview.complete_round(session)
    False -> session
  }
}

fn mark_session_complete_if_finished(
  session: interview.InterviewSession,
) -> interview.InterviewSession {
  case next_unanswered_question(session) {
    Some(_) -> session
    None -> {
      let completed_at = case string.trim(session.completed_at) {
        "" -> current_timestamp()
        value -> value
      }
      interview.InterviewSession(
        ..session,
        stage: interview.Complete,
        completed_at: completed_at,
      )
    }
  }
}

fn non_empty_answer(value: String) -> option.Option(String) {
  let normalized = string.trim(value)
  case normalized == "" {
    True -> None
    False -> Some(normalized)
  }
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

            case ensure_beads_output_dirs() {
              Error(err) -> {
                io.println_error(
                  "✗ Failed to prepare .beads output directories",
                )
                io.println_error(err)
                halt(exit_error)
              }
              Ok(Nil) -> Nil
            }

            let materialized =
              beads
              |> list.fold(#(1, [], []), fn(acc, bead) {
                let #(index, with_headers_rev, schema_files_rev) = acc
                let bead_id = bead_templates.bead_id_for_index(bead, index)
                let schema_path = ".beads/schemas/" <> bead_id <> ".cue"
                let schema_doc =
                  "package schema\n\n"
                  <> bead_templates.enhanced_bead_entry(bead, index)
                  <> "\n"

                let decorated =
                  bead_templates.with_validation_header(bead, schema_path)

                #(index + 1, [decorated, ..with_headers_rev], [
                  #(schema_path, schema_doc),
                  ..schema_files_rev
                ])
              })
              |> fn(tuple) {
                let #(_, with_headers_rev, schema_files_rev) = tuple
                #(
                  list.reverse(with_headers_rev),
                  list.reverse(schema_files_rev),
                )
              }

            let #(beads_with_headers, schema_files) = materialized

            let schema_write_result =
              list.fold(schema_files, Ok(Nil), fn(acc, item) {
                case acc {
                  Error(msg) -> Error(msg)
                  Ok(Nil) -> {
                    let #(path, content) = item
                    case simplifile.write(path, content) {
                      Ok(Nil) -> Ok(Nil)
                      Error(err) ->
                        Error(
                          "Failed to write schema file "
                          <> path
                          <> ": "
                          <> string.inspect(err),
                        )
                    }
                  }
                }
              })

            case schema_write_result {
              Error(msg) -> {
                io.println_error("✗ Failed to materialize bead schema files")
                io.println_error(msg)
                halt(exit_error)
              }
              Ok(Nil) -> Nil
            }

            // Validate generated beads against the planner schema before writing
            // customer-facing issue output.
            let validation_cue = bead_templates.beads_to_enhanced_cue(beads)
            let validation_path = ".beads/generated-beads.validation.cue"

            case simplifile.write(validation_path, validation_cue) {
              Error(err) -> {
                io.println_error(
                  "✗ Failed to write validation payload: "
                  <> string.inspect(err),
                )
                halt(exit_error)
              }
              Ok(Nil) -> {
                case
                  shellout.command(
                    "cue",
                    [
                      "vet",
                      "-d",
                      "#EnhancedBead",
                      "schema/enhanced-bead.cue",
                      validation_path,
                    ],
                    ".",
                    [],
                  )
                {
                  Error(#(_, stderr)) -> {
                    io.println_error(
                      "✗ Bead schema validation failed. Nothing was exported.",
                    )
                    io.println_error(stderr)
                    halt(exit_invalid)
                  }
                  Ok(_) -> {
                    io.println(
                      "✓ Generated beads validated against schema/enhanced-bead.cue",
                    )
                  }
                }
              }
            }

            // Export to .beads/issues.jsonl
            let jsonl_output = bead_templates.beads_to_jsonl(beads_with_headers)

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

fn ensure_beads_output_dirs() -> Result(Nil, String) {
  case shellout.command("mkdir", ["-p", ".beads/schemas"], ".", []) {
    Ok(_) -> Ok(Nil)
    Error(#(_, stderr)) -> Error(stderr)
  }
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
              "ai" -> plan_mode.format_plan_ai(plan)
              "human" -> plan_mode.format_plan_human(plan)
              _ -> {
                io.println_error("Invalid format: " <> format)
                io.println_error("Valid formats: human, json, ai")
                halt(exit_error)
                ""
              }
            }
            io.println(output)
            halt(exit_pass)
          }
        }
      }
      [] -> {
        io.println_error(
          "Usage: intent plan <session_id> [--format human|json|ai]",
        )
        io.println_error("")
        io.println_error("Display execution plan from session beads.")
        io.println_error("")
        io.println_error("Examples:")
        io.println_error(
          "  intent plan abc123              # Human-readable output",
        )
        io.println_error("  intent plan abc123 --format json  # JSON output")
        io.println_error(
          "  intent plan abc123 --format ai    # AI-ready output",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description("Display execution plan from session beads")
  |> glint.flag(
    "format",
    flag.string()
      |> flag.default("human")
      |> flag.description("Output format: human, json, or ai"),
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

    let is_valid_strategy =
      strategy == "hybrid" || strategy == "inversion" || strategy == "premortem"

    case is_valid_strategy {
      True -> Nil
      False -> {
        io.println_error("Invalid strategy: " <> strategy)
        io.println_error("Valid strategies: hybrid, inversion, premortem")
        halt(exit_error)
      }
    }

    case input.args {
      [session_id, ..] -> {
        let session_path = ".intent/session-" <> session_id <> ".cue"

        // Check session exists
        case simplifile.verify_is_file(session_path) {
          Ok(False) -> {
            io.println_error("Session not found: " <> session_id)
            io.println_error("Expected file: " <> session_path)
            halt(exit_error)
          }
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

    let is_valid_output =
      output_format == "text"
      || output_format == "cue"
      || output_format == "json"

    case is_valid_output {
      True -> Nil
      False -> {
        io.println_error("Invalid output format: " <> output_format)
        io.println_error("Valid formats: text, cue, json")
        halt(exit_error)
      }
    }

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

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

@external(erlang, "intent_ffi", "generate_uuid")
fn generate_uuid() -> String

@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String
