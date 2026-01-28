/// KIRK Analysis Commands Module
///
/// Extracts KIRK analysis commands (quality, invert, coverage, gaps, effects, ears)
/// and parse command from intent.gleam.

import gleam/dict
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/json_output
import intent/kirk/coverage_analyzer
import intent/kirk/effects_analyzer
import intent/kirk/ears_parser
import intent/kirk/gap_detector
import intent/kirk/inversion_checker
import intent/loader
import intent/output_mode
import intent/quality_analyzer
import intent/types
import simplifile

const exit_pass = 0

const exit_fail = 1

const exit_invalid = 3

const exit_error = 4

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

fn load_spec_for_mode(
  path: String,
  json_mode: Bool,
) -> Result(types.Spec, loader.LoadError) {
  case json_mode {
    True -> loader.load_spec_quiet(path)
    False -> loader.load_spec(path)
  }
}

pub fn kirk_quality_command() -> glint.Command(Nil) {
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
        let response =
          json_output.failure(
            "quality_check_failed",
            "quality",
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
  |> glint.description("KIRK: Analyze spec quality across multiple dimensions")
}

/// The `invert` command - KIRK inversion analysis
pub fn kirk_invert_command() -> glint.Command(Nil) {
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
        let response =
          json_output.failure(
            "invert_check_failed",
            "invert",
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
pub fn kirk_coverage_command() -> glint.Command(Nil) {
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
        let response =
          json_output.failure(
            "coverage_check_failed",
            "coverage",
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
  |> glint.description("KIRK: Coverage analysis including OWASP Top 10")
}

/// The `gaps` command - KIRK gap detection
pub fn kirk_gaps_command() -> glint.Command(Nil) {
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
        let response =
          json_output.failure(
            "gaps_check_failed",
            "gaps",
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
pub fn kirk_effects_command() -> glint.Command(Nil) {
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
        let response =
          json_output.failure(
            "effects_check_failed",
            "effects",
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
pub fn kirk_ears_command() -> glint.Command(Nil) {
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
pub fn parse_command() -> glint.Command(Nil) {
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
