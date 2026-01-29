/// Validate Commands Module
/// Core specification validation and introspection commands
///
/// Commands:
/// - validate: Validate CUE spec syntax and structure
/// - show: Pretty print a parsed spec
/// - export: Export spec to JSON format
/// - lint: Check for specification anti-patterns
/// - check: Run spec against API (under development)
import gleam/dynamic
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/file_watcher
import intent/json_output
import intent/loader
import intent/parser
import intent/spec_linter
import intent/watch_output

// Exit codes (duplicated to avoid circular dependency)
const exit_pass = 0

const exit_invalid = 3

const exit_error = 4

// Local halt FFI
@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

// =============================================================================
// Public Command Functions
// =============================================================================

/// The `validate` command - validate CUE spec syntax AND structure
pub fn validate_command() -> glint.Command(Nil) {
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
pub fn show_command() -> glint.Command(Nil) {
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
pub fn export_command() -> glint.Command(Nil) {
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
pub fn lint_command() -> glint.Command(Nil) {
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
pub fn check_command() -> glint.Command(Nil) {
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
