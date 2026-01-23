/// CLI commands for spec checking and validation
///
/// This module contains the core spec-related commands:
/// - check: Run spec against target API
/// - validate: Validate CUE spec syntax and structure
/// - show: Pretty print a parsed spec
/// - export: Export spec to JSON
/// - lint: Check for anti-patterns
/// - analyze: Quality analysis
/// - improve: Suggest improvements
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/cli/common.{ExitError, ExitFail, ExitInvalid, ExitPass, exit, halt}
import intent/cli_ui
import intent/improver
import intent/loader
import intent/output
import intent/output_mode
import intent/quality_analyzer
import intent/runner
import intent/spec_linter
import intent/types

/// The `check` command - run spec against a target
pub fn check_command() -> glint.Command(Nil) {
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
        exit(ExitError)
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
  let mode = output_mode.from_json_flag(is_json)

  // Validate target URL is provided
  case string.is_empty(target_url) {
    True -> {
      cli_ui.print_error("--target URL is required", mode)
      io.println("Usage: intent check <spec.cue> --target=<url>")
      exit(ExitError)
    }
    False -> Nil
  }

  // Load the spec
  case loader.load_spec(spec_path) {
    Error(e) -> {
      cli_ui.print_error(loader.format_error(e), mode)
      exit(ExitInvalid)
    }
    Ok(spec) -> {
      cli_ui.print_header("Checking spec: " <> spec.name, mode)

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
      let result = runner.run_spec(spec, target_url, options, mode)

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
      case result {
        output.SpecResult(pass: True, ..) -> {
          cli_ui.print_success("All checks passed!", mode)
          exit(ExitPass)
        }
        output.SpecResult(blocked: blocked, ..) if blocked > 0 -> {
          cli_ui.print_warning("Blocked behaviors detected", mode)
          halt(2)
        }
        _ -> {
          cli_ui.print_error("Check failed", mode)
          exit(ExitFail)
        }
      }
    }
  }
}

/// The `validate` command - validate CUE spec syntax AND structure
pub fn validate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let mode = output_mode.Interactive
    case input.args {
      [spec_path, ..] -> {
        // Use load_spec_quiet to validate both CUE syntax AND spec structure
        case loader.load_spec_quiet(spec_path) {
          Ok(_) -> {
            cli_ui.print_success("Valid spec: " <> spec_path, mode)
            exit(ExitPass)
          }
          Error(e) -> {
            cli_ui.print_error("Invalid spec: " <> loader.format_error(e), mode)
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent validate <spec.cue>")
        exit(ExitError)
      }
    }
  })
  |> glint.description("Validate a CUE spec file (syntax and structure)")
}

/// The `show` command - pretty print a parsed spec
pub fn show_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    case input.args {
      [spec_path, ..] -> {
        case is_json {
          True ->
            case loader.export_spec_json(spec_path, loader.default_cue_exporter) {
              Ok(json_str) -> {
                io.println(json_str)
                exit(ExitPass)
              }
              Error(e) -> {
                io.println_error("Error: " <> loader.format_error(e))
                exit(ExitInvalid)
              }
            }
          False -> {
            case loader.load_spec(spec_path) {
              Ok(spec) -> {
                print_spec_summary(spec)
                exit(ExitPass)
              }
              Error(e) -> {
                io.println_error("Error: " <> loader.format_error(e))
                exit(ExitInvalid)
              }
            }
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent show <spec.cue> [--json]")
        exit(ExitError)
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
pub fn export_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.export_spec_json(spec_path, loader.default_cue_exporter) {
          Ok(json_str) -> {
            io.println(json_str)
            exit(ExitPass)
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent export <spec.cue>")
        exit(ExitError)
      }
    }
  })
  |> glint.description("Export spec to JSON format")
}

/// The `lint` command - check for specification anti-patterns
pub fn lint_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let lint_result = spec_linter.lint_spec(spec)
            case lint_result {
              spec_linter.LintValid -> {
                io.println("No linting issues found")
                exit(ExitPass)
              }
              spec_linter.LintWarnings(warnings) -> {
                io.println(spec_linter.format_warnings(warnings))
                exit(ExitFail)
              }
            }
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent lint <spec.cue>")
        exit(ExitError)
      }
    }
  })
  |> glint.description("Check spec for anti-patterns and quality issues")
}

/// The `analyze` command - analyze spec quality
pub fn analyze_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = quality_analyzer.analyze_spec(spec)
            io.println(quality_analyzer.format_report(report))
            case list.is_empty(report.issues) {
              True -> exit(ExitPass)
              False -> exit(ExitFail)
            }
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent analyze <spec.cue>")
        exit(ExitError)
      }
    }
  })
  |> glint.description(
    "Analyze spec quality and provide improvement suggestions",
  )
}

/// The `improve` command - suggest improvements
pub fn improve_command() -> glint.Command(Nil) {
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
            exit(ExitPass)
          }
          Error(e) -> {
            io.println_error("Error: " <> loader.format_error(e))
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        io.println_error("Error: spec file path required")
        io.println_error("Usage: intent improve <spec.cue>")
        exit(ExitError)
      }
    }
  })
  |> glint.description(
    "Suggest improvements based on quality analysis and linting",
  )
}
