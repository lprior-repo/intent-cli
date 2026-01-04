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

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil
