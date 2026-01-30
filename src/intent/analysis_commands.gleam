/// Analysis Commands Module
///
/// Handles quality analysis, improvement suggestions, and health checks
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import glint
import intent/ai_errors
import intent/doctor
import intent/improver
import intent/json_output
import intent/loader
import intent/quality_analyzer
import intent/spec_linter
import intent/types

// Exit codes
const exit_pass = 0

const exit_invalid = 3

const exit_error = 4

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

/// Load spec with optional JSON-only mode (no pretty printing errors)
fn load_spec_for_mode(
  path: String,
  json_mode: Bool,
) -> Result(types.Spec, loader.LoadError) {
  case json_mode {
    True -> loader.load_spec_quiet(path)
    False -> loader.load_spec(path)
  }
}

/// The `analyze` command - alias for quality with JSON output
pub fn analyze_command() -> glint.Command(Nil) {
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
                  "intent gaps " <> spec_path,
                  "Find coverage gaps",
                ),
                json_output.next_action(
                  "intent invert " <> spec_path,
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
          usage: "intent analyze <spec.cue>",
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
pub fn improve_command() -> glint.Command(Nil) {
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
                  "intent doctor " <> spec_path,
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
        let response =
          json_output.failure(
            "improve_failed",
            "improve",
            json.object([#("usage", json.string("intent improve <spec.cue>"))]),
            [json_output.error("usage_error", "spec file path required")],
            None,
            [
              json_output.next_action(
                "intent validate <spec.cue>",
                "Validate a spec file first",
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
  |> glint.description(
    "Suggest improvements based on quality analysis and linting",
  )
}

/// The `doctor` command - health report with prioritized improvements
pub fn doctor_command() -> glint.Command(Nil) {
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
