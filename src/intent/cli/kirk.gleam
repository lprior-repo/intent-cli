/// CLI commands for KIRK quality analysis
///
/// This module contains all KIRK (Knowledge-driven Intent Requirements Kernel) commands:
/// - quality: Multi-dimensional quality analysis
/// - invert: Missing failure case analysis
/// - coverage: Test coverage including OWASP Top 10
/// - gaps: Gap detection using mental models
/// - effects: Second-order effects analysis
/// - compact: Compact Intent Notation conversion
/// - prototext: Protobuf text format export
/// - ears: EARS requirements parsing
/// - parse: Parse EARS to spec
import gleam/dict
import gleam/float
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/cli/common.{ExitError, ExitFail, ExitInvalid, ExitPass, exit, halt}
import intent/cli_ui
import intent/json_output
import intent/kirk/compact_format
import intent/output_mode
import intent/kirk/coverage_analyzer
import intent/kirk/ears_parser
import intent/kirk/effects_analyzer
import intent/kirk/gap_detector
import intent/kirk/inversion_checker
import intent/kirk/quality_analyzer as kirk_quality
import intent/loader
import simplifile

/// Exit code constants for compatibility
const exit_pass = 0

const exit_error = 4

const exit_invalid = 3

/// The `quality` command - KIRK quality analysis
pub fn quality_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)
    let mode = output_mode.from_json_flag(is_json)

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = kirk_quality.analyze_quality(spec)
            let has_issues = !list.is_empty(report.issues)
            case is_json {
              True -> {
                let data =
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
                let next_actions = case has_issues {
                  True -> [
                    json_output.next_action(
                      "intent improve " <> spec_path <> " --json",
                      "Get suggestions to fix quality issues",
                    ),
                  ]
                  False -> [
                    json_output.next_action(
                      "intent check " <> spec_path <> " --target <url> --json",
                      "Run tests against target API",
                    ),
                  ]
                }
                let exit_code = case has_issues {
                  True -> 1
                  False -> 0
                }
                let response =
                  json_output.create_full_response(
                    !has_issues,
                    "quality_result",
                    "quality",
                    data,
                    [],
                    next_actions,
                    Some(spec_path),
                    exit_code,
                  )
                json_output.output(response)
              }
              False -> io.println(kirk_quality.format_report(report))
            }
            case has_issues {
              False -> halt(exit_pass)
              True -> exit(ExitFail)
            }
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
  |> glint.description("KIRK: Analyze spec quality across multiple dimensions")
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
  )
}

/// The `invert` command - KIRK inversion analysis
pub fn invert_command() -> glint.Command(Nil) {
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
            let has_gaps =
              !list.is_empty(report.security_gaps)
              || !list.is_empty(report.usability_gaps)
              || !list.is_empty(report.integration_gaps)
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
                let next_actions = case has_gaps {
                  True -> [
                    json_output.next_action(
                      "intent improve " <> spec_path <> " --json",
                      "Get suggestions to add missing failure cases",
                    ),
                  ]
                  False -> [
                    json_output.next_action(
                      "intent effects " <> spec_path <> " --json",
                      "Analyze second-order effects",
                    ),
                  ]
                }
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
            exit(ExitPass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent invert <spec.cue> [--json]")
        exit(ExitError)
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
pub fn coverage_command() -> glint.Command(Nil) {
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
            let has_missing = !list.is_empty(report.owasp.missing)
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
                let next_actions = case has_missing {
                  True -> [
                    json_output.next_action(
                      "intent invert " <> spec_path <> " --json",
                      "Find missing failure cases",
                    ),
                    json_output.next_action(
                      "intent improve " <> spec_path <> " --json",
                      "Get suggestions to improve coverage",
                    ),
                  ]
                  False -> [
                    json_output.next_action(
                      "intent check " <> spec_path <> " --target <url> --json",
                      "Run tests against target API",
                    ),
                  ]
                }
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
            exit(ExitPass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent coverage <spec.cue> [--json]")
        exit(ExitError)
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
pub fn gaps_command() -> glint.Command(Nil) {
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
            let has_gaps = report.total_gaps > 0
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
                let next_actions = case has_gaps {
                  True -> [
                    json_output.next_action(
                      "intent improve " <> spec_path <> " --json",
                      "Get suggestions to fill gaps",
                    ),
                    json_output.next_action(
                      "intent doctor " <> spec_path <> " --json",
                      "Get prioritized fix recommendations",
                    ),
                  ]
                  False -> [
                    json_output.next_action(
                      "intent check " <> spec_path <> " --target <url> --json",
                      "Spec is complete - run tests",
                    ),
                  ]
                }
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
            exit(ExitPass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent gaps <spec.cue> [--json]")
        exit(ExitError)
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

fn ears_error_to_json(err: ears_parser.EarsError) -> json.Json {
  case err {
    ears_parser.PatternNotMatched(line, raw_text) ->
      json.object([
        #("line", json.int(line)),
        #("message", json.string("Pattern not matched")),
        #("raw_text", json.string(raw_text)),
      ])
    ears_parser.PatternMatchFailed(line, _pattern, raw_text) ->
      json.object([
        #("line", json.int(line)),
        #("message", json.string("Pattern match failed")),
        #("raw_text", json.string(raw_text)),
      ])
    ears_parser.RegexCompileFailed(line, _pattern, pattern_string) ->
      json.object([
        #("line", json.int(line)),
        #("message", json.string("Regex compile failed")),
        #("pattern", json.string(pattern_string)),
      ])
    ears_parser.ComponentExtractionFailed(line, _pattern, _missing, raw_text) ->
      json.object([
        #("line", json.int(line)),
        #("message", json.string("Component extraction failed")),
        #("raw_text", json.string(raw_text)),
      ])
  }
}

fn format_ears_error(err: ears_parser.EarsError) -> String {
  case err {
    ears_parser.PatternNotMatched(line, _raw_text) ->
      "Line " <> string.inspect(line) <> ": Pattern not matched"
    ears_parser.PatternMatchFailed(line, _pattern, _raw_text) ->
      "Line " <> string.inspect(line) <> ": Pattern match failed"
    ears_parser.RegexCompileFailed(line, _pattern, _pattern_string) ->
      "Line " <> string.inspect(line) <> ": Regex compile failed"
    ears_parser.ComponentExtractionFailed(line, _pattern, _missing, _raw_text) ->
      "Line " <> string.inspect(line) <> ": Component extraction failed"
  }
}

/// The `effects` command - KIRK second-order effects analysis
pub fn effects_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let mode = output_mode.Interactive
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let report = effects_analyzer.analyze_effects(spec)
            io.println(effects_analyzer.format_report(report))
            exit(ExitPass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent effects <spec.cue>")
        exit(ExitError)
      }
    }
  })
  |> glint.description(
    "KIRK: Analyze second-order effects (consequence tracing)",
  )
}

/// The `compact` command - KIRK compact format (CIN)
pub fn compact_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let mode = output_mode.Interactive
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
                let comparison = compact_format.compare_token_usage(spec)
                io.println("")
                io.println("-------------------------------------")
                io.println("Token Analysis:")
                io.println(
                  "  Full JSON:    ~"
                    <> string.inspect(comparison.full_tokens)
                    <> " tokens",
                )
                io.println(
                  "  Compact CIN:  ~"
                    <> string.inspect(comparison.compact_tokens)
                    <> " tokens",
                )
                io.println(
                  "  Savings:      "
                    <> string.inspect(float.round(comparison.savings_percent))
                    <> "%",
                )
              }
              False -> Nil
            }

            exit(ExitPass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent compact <spec.cue> [--tokens]")
        exit(ExitError)
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
pub fn prototext_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let mode = output_mode.Interactive
    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec(spec_path) {
          Ok(spec) -> {
            let output = compact_format.spec_to_prototext(spec)
            io.println(output)
            exit(ExitPass)
          }
          Error(e) -> {
            cli_ui.print_error(loader.format_error(e), mode)
            exit(ExitInvalid)
          }
        }
      }
      [] -> {
        cli_ui.print_error("spec file path required", mode)
        io.println("Usage: intent prototext <spec.cue>")
        exit(ExitError)
      }
    }
  })
  |> glint.description("KIRK: Export to Protobuf text format")
}

/// The `ears` command - KIRK EARS requirements parser
pub fn ears_command() -> glint.Command(Nil) {
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
                      json.array(result.errors, ears_error_to_json),
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
                  Error(_) ->
                    cli_ui.print_error("Failed to write to: " <> path, mode)
                }
              }
            }

            exit(ExitPass)
          }
          Error(_) -> {
            cli_ui.print_error("Failed to read: " <> requirements_path, mode)
            exit(ExitError)
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
        exit(ExitError)
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

/// The `parse` command - parse EARS requirements to spec
pub fn parse_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)
    let mode = output_mode.from_json_flag(is_json)

    let output_file =
      flag.get_string(input.flags, "o")
      |> result.unwrap("")

    let use_stdin =
      flag.get_bool(input.flags, "stdin")
      |> result.unwrap(False)

    let args_with_stdin = case use_stdin, input.args {
      True, [] -> ["-"]
      _, args -> args
    }

    case args_with_stdin {
      [requirements_path, ..] -> {
        let read_result = case requirements_path == "-" || use_stdin {
          True ->
            Error(
              "Reading from stdin is not yet supported. Please provide a file path.",
            )
          False ->
            simplifile.read(requirements_path)
            |> result.map_error(fn(_) { "Failed to read file" })
        }
        case read_result {
          Ok(content) -> {
            let result = ears_parser.parse(content)
            let req_count = list.length(result.requirements)
            let err_count = list.length(result.errors)

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
                    #("errors", json.array(result.errors, ears_error_to_json)),
                    #("warnings", json.array(result.warnings, json.string)),
                    #("count", json.int(req_count)),
                  ])
                io.println(json.to_string(json_obj))
              }
              False -> {
                io.println("Parsing EARS requirements...")
                print_pattern_count(ubiq, "ubiquitous")
                print_pattern_count(event, "event-driven")
                print_pattern_count(state, "state-driven")
                print_pattern_count(opt, "optional")
                print_pattern_count(unwant, "unwanted")
                print_pattern_count(complex, "complex")

                io.println("")
                io.println(
                  "Total: "
                  <> string.inspect(req_count)
                  <> " requirements parsed",
                )

                case err_count > 0 {
                  True -> {
                    io.println("")
                    io.println("Errors (" <> string.inspect(err_count) <> "):")
                    list.each(result.errors, fn(e) {
                      io.println("  " <> format_ears_error(e))
                    })
                  }
                  False -> Nil
                }
              }
            }

            case output_file {
              "" -> Nil
              path -> {
                let cue_output = ears_parser.to_cue(result, "ParsedSpec")
                case simplifile.write(path, cue_output) {
                  Ok(_) -> io.println("Written to: " <> path)
                  Error(_) ->
                    cli_ui.print_error("Failed to write to: " <> path, mode)
                }
              }
            }

            exit(ExitPass)
          }
          Error(msg) -> {
            cli_ui.print_error(msg, mode)
            exit(ExitError)
          }
        }
      }
      [] -> {
        cli_ui.print_error("requirements file path required", mode)
        io.println("Usage: intent parse <requirements.md> [--json] [-o <file>]")
        io.println("       echo 'THE SYSTEM SHALL...' | intent parse --stdin")
        exit(ExitError)
      }
    }
  })
  |> glint.description("Parse EARS requirements and generate spec")
  |> glint.flag(
    "json",
    flag.bool() |> flag.default(False) |> flag.description("Output as JSON"),
  )
  |> glint.flag(
    "o",
    flag.string() |> flag.default("") |> flag.description("Output CUE file"),
  )
  |> glint.flag(
    "stdin",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Read from stdin instead of file"),
  )
}

fn print_pattern_count(count: Int, pattern_name: String) -> Nil {
  case count > 0 {
    True ->
      io.println(
        "  Parsed "
        <> string.inspect(count)
        <> " "
        <> pattern_name
        <> " requirements",
      )
    False -> Nil
  }
}
