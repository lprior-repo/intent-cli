/// Utility Commands Module
///
/// This module contains utility and meta-commands for the Intent CLI,
/// including diff, help, and AI schema introspection commands.

import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import glint
import glint/flag
import intent/ai_schema
import intent/diff
import intent/json_output
import intent/loader
import intent/spec_aggregator
import intent/types

// Exit codes
const exit_pass = 0


const exit_invalid = 3

const exit_error = 4

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

// === Private Helpers ===

/// Load a spec, choosing quiet or verbose mode based on json_mode flag
fn load_spec_for_mode(
  path: String,
  json_mode: Bool,
) -> Result(types.Spec, loader.LoadError) {
  case json_mode {
    True -> loader.load_spec_quiet(path)
    False -> loader.load_spec(path)
  }
}

// === Private Types ===

type HelpEntry {
  HelpEntry(
    command: String,
    group: String,
    args: String,
    flags: String,
    description: String,
    output_action: String,
  )
}

// === Private Functions ===

fn help_command_entries() -> List(HelpEntry) {
  [
    // Spec Operations
    HelpEntry("validate", "spec", "<spec.cue>", "--watch", "Validate CUE spec syntax and structure", "validate_result"),
    HelpEntry("show", "spec", "<spec.cue>", "", "Display parsed spec contents", "show_result"),
    HelpEntry("diff", "spec", "<spec1.cue> <spec2.cue>", "", "Compare two spec versions", "diff_result"),
    HelpEntry("lint", "spec", "<spec.cue>", "", "Detect anti-patterns", "lint_result"),
    HelpEntry("improve", "spec", "<spec.cue>", "", "Suggest improvements", "improve_result"),
    // KIRK Analysis
    HelpEntry("quality", "kirk", "<spec.cue>", "", "Score across 5 dimensions (0-100)", "quality_result"),
    HelpEntry("coverage", "kirk", "<spec.cue>", "", "OWASP + edge case coverage audit", "coverage_result"),
    HelpEntry("gaps", "kirk", "<spec.cue>", "", "Find missing requirements", "gaps_result"),
    HelpEntry("invert", "kirk", "<spec.cue>", "", "Failure mode analysis", "invert_result"),
    HelpEntry("effects", "kirk", "<spec.cue>", "", "Second-order effects analysis", "effects_result"),
    HelpEntry("ears", "kirk", "<file>", "--output=cue|json", "Parse EARS requirements", "ears_result"),
    HelpEntry("doctor", "kirk", "<spec.cue>", "", "Prioritized health report", "doctor_result"),
    // Interview
    HelpEntry("interview", "interview", "", "--profile=api|cli --resume=<session-id>", "Guided spec discovery. REQUIRED: --profile flag for new interviews", "interview_result"),
    HelpEntry("sessions", "interview", "", "--profile=api|cli --delete=<id>", "List all sessions with IDs and status", "sessions_list"),
    HelpEntry("history", "interview", "", "--profile=api|cli", "Show interview snapshots", "history_result"),
    HelpEntry("export", "interview", "<session-id>", "--output=<file.cue>", "Export interview session to CUE spec", "export_result"),
    // Planning
    HelpEntry("beads", "planning", "<session-id>", "--max-items=N", "Generate atomic work items from session", "beads_result"),
    HelpEntry("plan", "planning", "<session-id>", "--rounds=1..5", "Health check + wave ordering + beads", "plan_result"),
    HelpEntry("plan-approve", "planning", "<session-id>", "--yes --notes='text'", "Approve execution plan", "plan_approve_result"),
    HelpEntry("beads-regenerate", "planning", "<spec.cue>", "", "Regenerate beads from spec file", "beads_regenerate_result"),
    HelpEntry("bead-status", "planning", "", "--bead-id=<id> --status=success|failed|blocked --reason='text'", "Update individual bead execution status", "bead_status_result"),
    HelpEntry("prompt", "planning", "<session-id>", "--max-items=N", "Generate AI implementation prompts from beads", "prompt_result"),
    HelpEntry("feedback", "planning", "", "--results=<check-output.json>", "Generate fix beads from check command failures", "feedback_result"),
    HelpEntry("kirk-beads", "planning", "<spec.cue>", "--round=N --min-severity=low|medium|high|critical", "Generate enhanced beads from KIRK analysis", "kirk_beads_result"),
    HelpEntry("bead-show", "planning", "<bead-id>", "--spec=<spec.cue>", "Show full bead details", "bead_show_result"),
    HelpEntry("bead-verify", "planning", "<bead-id>", "--spec=<spec.cue>", "Verify bead acceptance criteria", "bead_verify_result"),
    // Phase commands
    HelpEntry("vision start", "phase", "<spec.cue>", "", "Initialize Vision phase session", "vision_start_result"),
    HelpEntry("vision check", "phase", "<session-id>", "", "Validate Vision phase completeness", "vision_check_result"),
    HelpEntry("vision critique", "phase", "<session-id>", "", "Generate Vision critique questions", "vision_critique_result"),
    HelpEntry("vision respond", "phase", "<session-id>", "--answers=<file>", "Process Vision critique responses", "vision_respond_result"),
    HelpEntry("vision agree", "phase", "<session-id>", "", "Finalize Vision phase agreement", "vision_agree_result"),
    HelpEntry("spec start", "phase", "<spec.cue>", "", "Initialize Spec phase session", "spec_start_result"),
    HelpEntry("spec check", "phase", "<session-id>", "", "Validate Spec phase completeness", "spec_check_result"),
    HelpEntry("spec critique", "phase", "<session-id>", "", "Generate Spec critique questions", "spec_critique_result"),
    HelpEntry("spec respond", "phase", "<session-id>", "--answers=<file>", "Process Spec critique responses", "spec_respond_result"),
    HelpEntry("spec agree", "phase", "<session-id>", "", "Finalize Spec phase agreement", "spec_agree_result"),
    HelpEntry("shape start", "phase", "<spec.cue>", "", "Initialize Shape phase session", "shape_start_result"),
    HelpEntry("shape check", "phase", "<session-id>", "", "Validate Shape phase completeness", "shape_check_result"),
    HelpEntry("shape critique", "phase", "<session-id>", "", "Generate Shape critique questions", "shape_critique_result"),
    HelpEntry("shape respond", "phase", "<session-id>", "--answers=<file>", "Process Shape critique responses", "shape_respond_result"),
    HelpEntry("shape agree", "phase", "<session-id>", "", "Finalize Shape phase agreement", "shape_agree_result"),
    HelpEntry("ready start", "phase", "<spec.cue>", "", "Initialize Ready phase session", "ready_start_result"),
    HelpEntry("ready check", "phase", "<session-id>", "", "Validate Ready phase completeness", "ready_check_result"),
    HelpEntry("ready critique", "phase", "<session-id>", "", "Generate Ready critique questions", "ready_critique_result"),
    HelpEntry("ready respond", "phase", "<session-id>", "--answers=<file>", "Process Ready critique responses", "ready_respond_result"),
    HelpEntry("ready agree", "phase", "<session-id>", "", "Finalize Ready phase agreement", "ready_agree_result"),
    // Utilities
    HelpEntry("parse", "utility", "<file>", "", "Quick EARS validation of requirements file", "parse_result"),
    HelpEntry("analyze", "utility", "<spec.cue>", "", "Alias for quality command", "quality_result"),
    HelpEntry("ai schema", "utility", "", "--all --list --command=<cmd> --type=input|output", "JSON schema docs for all commands", "schema_result"),
    HelpEntry("ai aggregate", "utility", "<spec.cue>", "", "Run all analyses at once", "aggregate_result"),
    HelpEntry("help", "utility", "[command]", "", "Show this help", "help_result"),
  ]
}

fn help_workflow_steps() -> List(#(Int, String, String)) {
  [
    #(1, "Start an interview to discover your API spec", "intent interview --profile=api"),
    #(2, "List sessions to find your session ID", "intent sessions --profile=api"),
    #(3, "Export the interview to a CUE spec file", "intent export <session-id> --output=spec.cue"),
    #(4, "Validate the spec", "intent validate spec.cue"),
    #(5, "Analyze spec quality", "intent quality spec.cue"),
    #(6, "Find gaps in coverage", "intent gaps spec.cue"),
    #(7, "Analyze failure modes", "intent invert spec.cue"),
    #(8, "Generate work items", "intent beads <session-id>"),
    #(9, "Generate AI implementation prompts", "intent prompt <session-id>"),
  ]
}

// === Public Commands ===

pub fn diff_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec1_path, spec2_path] -> {
        case load_spec_for_mode(spec1_path, True) {
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "diff_failed",
                "diff",
                json.null(),
                [json_output.error("load_error", error_msg)],
                Some(spec1_path),
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
          Ok(spec1) -> {
            case load_spec_for_mode(spec2_path, True) {
              Error(e) -> {
                let error_msg = loader.format_error(e)
                let response =
                  json_output.failure(
                    "diff_failed",
                    "diff",
                    json.null(),
                    [json_output.error("load_error", error_msg)],
                    Some(spec2_path),
                    [],
                    exit_invalid,
                  )
                json_output.output(response)
                halt(exit_invalid)
              }
              Ok(spec2) -> {
                let spec_diff = diff.compare_specs(spec1, spec2)
                let data = diff.diff_to_json(spec_diff)
                let next_actions = case spec_diff.has_changes {
                  True -> [
                    json_output.next_action(
                      "intent quality " <> spec2_path,
                      "Analyze quality of new spec",
                    ),
                  ]
                  False -> []
                }
                let response =
                  json_output.success(
                    "diff_result",
                    "diff",
                    data,
                    None,
                    next_actions,
                  )
                json_output.output(response)
                halt(exit_pass)
              }
            }
          }
        }
      }
      _ -> {
        let error =
          json_output.error(
            "missing_arguments",
            "Two spec paths are required for diff",
          )

        let response =
          json_output.failure(
            "diff_usage",
            "diff",
            json.object([
              #(
                "usage",
                json.string("intent diff <spec1.cue> <spec2.cue>"),
              ),
              #(
                "description",
                json.string("Compare two spec versions and show differences"),
              ),
            ]),
            [error],
            None,
            [
              json_output.next_action(
                "intent diff spec1.cue spec2.cue",
                "Compare two specs",
              ),
            ],
            exit_pass,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Compare two spec versions and show differences")
}

pub fn help_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [command_name] -> {
        let response =
          json_output.success(
            "help_command",
            "help",
            json.object([
              #("hint", json.string(
                "intent " <> command_name <> " --help",
              )),
            ]),
            None,
            [
              json_output.next_action(
                "intent " <> command_name <> " --help",
                "Show full flags and usage for this command",
              ),
            ],
          )
        json_output.output(response)
        halt(exit_pass)
      }
      [] -> {
        let commands = help_command_entries()
        let workflow = help_workflow_steps()

        let data =
          json.object([
            #("tool", json.string("intent")),
            #("purpose", json.string(
              "Contract-driven API testing. CUE specs to HTTP tests to verification.",
            )),
            #("usage", json.string("intent <command> [args] [flags]")),
            #("output_format", json.string(
              "All commands return JSON: {success, action, command, data, errors, next_actions, metadata}",
            )),
            #("exit_codes", json.object([
              #("0", json.string("success")),
              #("1", json.string("spec failure")),
              #("3", json.string("invalid input")),
              #("4", json.string("error")),
            ])),
            #("workflow", json.array(workflow, fn(step) {
              json.object([
                #("step", json.int(step.0)),
                #("description", json.string(step.1)),
                #("command", json.string(step.2)),
              ])
            })),
            #("commands", json.array(commands, fn(cmd) {
              json.object([
                #("command", json.string(cmd.command)),
                #("group", json.string(cmd.group)),
                #("args", json.string(cmd.args)),
                #("flags", json.string(cmd.flags)),
                #("description", json.string(cmd.description)),
                #("output_action", json.string(cmd.output_action)),
              ])
            })),
            #("total_commands", json.int(list.length(commands))),
          ])

        let response =
          json_output.success(
            "help_result",
            "help",
            data,
            None,
            [
              json_output.next_action(
                "intent sessions --profile=api",
                "List existing interview sessions",
              ),
              json_output.next_action(
                "intent interview --profile=api",
                "Start a new API spec interview",
              ),
              json_output.next_action(
                "intent ai schema --all",
                "Get JSON schemas for all command inputs/outputs",
              ),
            ],
          )
        json_output.output(response)
        halt(exit_pass)
      }
      _ -> {
        let response =
          json_output.failure(
            "help_error",
            "help",
            json.object([]),
            [json_output.error("TOO_MANY_ARGS", "Expected: intent help [command]")],
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
    "Show detailed help for a specific command with usage examples and related commands",
  )
}

pub fn ai_schema_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let command_flag = flag.get_string(input.flags, "command")
    let type_flag = flag.get_string(input.flags, "type")
    let all_flag = flag.get_bool(input.flags, "all") |> result.unwrap(False)
    let list_flag = flag.get_bool(input.flags, "list") |> result.unwrap(False)

    case all_flag, list_flag, command_flag, type_flag {
      // --all: Return all schemas
      True, _, _, _ -> {
        case ai_schema.get_all_schemas() {
          Ok(schemas) -> {
            let schemas_json =
              json.array(schemas, fn(schema) {
                json.object([
                  #("command", json.string(schema.command)),
                  #("schema_type", json.string(schema.schema_type)),
                  #("content", json.string(schema.content)),
                ])
              })

            let data =
              json.object([
                #("schemas", schemas_json),
                #("count", json.int(list.length(schemas))),
              ])

            let next_actions = [
              json_output.next_action(
                "intent validate <spec.cue>",
                "Validate a spec using these schemas",
              ),
              json_output.next_action(
                "intent ai schema --command=<cmd> --type=input",
                "Get specific schema for a command",
              ),
            ]

            let response =
              json_output.success(
                "schema_list_result",
                "ai schema",
                data,
                None,
                next_actions,
              )

            json_output.output(response)
            halt(exit_pass)
          }
          Error(ai_schema.SchemaDirectoryNotFound) -> {
            let error =
              json_output.error(
                "schema_directory_not_found",
                "Schema directory not found: schema/ai/",
              )

            let next_actions = [
              json_output.next_action("intent help", "Show available commands"),
              json_output.next_action(
                "intent interview --profile=api",
                "Start a new interview session",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          Error(_) -> {
            let error =
              json_output.error("unknown_error", "Failed to read schemas")

            let next_actions = [
              json_output.next_action(
                "intent ai schema --list",
                "List available commands",
              ),
              json_output.next_action(
                "intent help",
                "Show all available commands",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
        }
      }

      // --list: List available command names
      _, True, _, _ -> {
        case ai_schema.list_commands() {
          Ok(commands) -> {
            let data =
              json.object([
                #("commands", json.array(commands, json.string)),
                #("count", json.int(list.length(commands))),
              ])

            let next_actions = [
              json_output.next_action(
                "intent ai schema --all",
                "List all available schemas",
              ),
              json_output.next_action(
                "intent ai schema --command=<cmd> --type=input",
                "Get specific schema for a command",
              ),
            ]

            let response =
              json_output.success(
                "schema_commands_result",
                "ai schema",
                data,
                None,
                next_actions,
              )

            json_output.output(response)
            halt(exit_pass)
          }
          Error(_) -> {
            let error =
              json_output.error(
                "failed_to_list_commands",
                "Failed to list available commands",
              )

            let next_actions = [
              json_output.next_action("intent help", "Show available commands"),
              json_output.next_action(
                "intent ai schema --all",
                "Try listing all schemas",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
        }
      }

      // --command=X --type=Y: Get specific schema
      _, _, Ok(cmd), Ok(typ) -> {
        case ai_schema.get_schema(command: cmd, schema_type: typ) {
          Ok(content) -> {
            let data =
              json.object([
                #("command", json.string(cmd)),
                #("schema_type", json.string(typ)),
                #("content", json.string(content)),
              ])

            let next_actions = [
              json_output.next_action(
                "intent validate <spec.cue>",
                "Validate a spec using this schema",
              ),
              json_output.next_action(
                "intent help " <> cmd,
                "Get help for " <> cmd <> " command",
              ),
            ]

            let response =
              json_output.success(
                "schema_result",
                "ai schema",
                data,
                None,
                next_actions,
              )

            json_output.output(response)
            halt(exit_pass)
          }
          Error(ai_schema.SchemaNotFound(command)) -> {
            let error =
              json_output.detailed_error(
                "schema_not_found",
                "Schema not found for command: " <> command,
                "schema/ai/" <> typ <> "/" <> command <> ".cue",
                "Check available schemas with: intent ai schema --all",
                "intent ai schema --all",
              )

            let data = json.object([#("command", json.string(command))])

            let next_actions = [
              json_output.next_action(
                "intent ai schema --all",
                "List all available schemas",
              ),
              json_output.next_action(
                "intent ai schema --list",
                "List available command names",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                data,
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          Error(ai_schema.InvalidSchemaType(schema_type)) -> {
            let error =
              json_output.detailed_error(
                "invalid_schema_type",
                "Invalid schema type: "
                  <> schema_type
                  <> ". Must be 'input' or 'output'",
                "command line",
                "Use --type=input or --type=output",
                "intent ai schema --command=" <> cmd <> " --type=input",
              )

            let data = json.object([#("schema_type", json.string(schema_type))])

            let next_actions = [
              json_output.next_action(
                "intent ai schema --command=" <> cmd <> " --type=input",
                "Try with input schema type",
              ),
              json_output.next_action(
                "intent ai schema --command=" <> cmd <> " --type=output",
                "Try with output schema type",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                data,
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          Error(_) -> {
            let error =
              json_output.error("unknown_error", "Failed to read schema")

            let next_actions = [
              json_output.next_action(
                "intent ai schema --all",
                "List all available schemas",
              ),
              json_output.next_action(
                "intent ai schema --list",
                "List available commands",
              ),
            ]

            let response =
              json_output.failure(
                "schema_error",
                "ai schema",
                json.object([]),
                [error],
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
        }
      }

      // Missing required flags
      _, _, _, _ -> {
        let error =
          json_output.detailed_error(
            "missing_flags",
            "Must specify either --all, --list, or both --command and --type",
            "command line",
            "Use --all to list all schemas, --list for commands, or --command=X --type=Y for specific schema",
            "intent ai schema --all",
          )

        let next_actions = [
          json_output.next_action(
            "intent ai schema --all",
            "List all available schemas",
          ),
          json_output.next_action(
            "intent ai schema --list",
            "List available command names",
          ),
          json_output.next_action("intent help", "Show command help"),
        ]

        let response =
          json_output.failure(
            "schema_error",
            "ai schema",
            json.object([]),
            [error],
            None,
            next_actions,
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Schema introspection for AI agents - get CUE schemas for command input/output",
  )
  |> glint.flag(
    "command",
    flag.string() |> flag.description("Command name (e.g., 'quality')"),
  )
  |> glint.flag(
    "type",
    flag.string() |> flag.description("Schema type: 'input' or 'output'"),
  )
  |> glint.flag(
    "all",
    flag.bool()
      |> flag.default(False)
      |> flag.description("List all available schemas"),
  )
  |> glint.flag(
    "list",
    flag.bool()
      |> flag.default(False)
      |> flag.description("List all available command names"),
  )
}

pub fn ai_aggregate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [] -> {
        let error =
          json_output.error(
            "missing_spec_paths",
            "At least one spec file path is required",
          )

        let next_actions = [
          json_output.next_action(
            "intent validate <spec.cue>",
            "Validate a spec file first",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "aggregate_error",
            "ai aggregate",
            json.object([]),
            [error],
            None,
            next_actions,
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
      spec_paths -> {
        // Load all specs
        let loaded_specs =
          spec_paths
          |> list.map(fn(path) {
            case loader.load_spec_quiet(path) {
              Ok(spec) ->
                Ok(spec_aggregator.SpecWithPath(path: path, spec: spec))
              Error(e) -> Error(#(path, e))
            }
          })

        // Separate successes from failures
        let #(specs, errors) =
          loaded_specs
          |> list.fold(#([], []), fn(acc, result) {
            let #(specs_acc, errors_acc) = acc
            case result {
              Ok(spec_with_path) -> #([spec_with_path, ..specs_acc], errors_acc)
              Error(#(path, err)) -> #(specs_acc, [#(path, err), ..errors_acc])
            }
          })

        // If any specs failed to load, report errors
        case errors != [] {
          True -> {
            let error_messages =
              errors
              |> list.map(fn(error_pair) {
                let #(path, err) = error_pair
                json_output.error(
                  "spec_load_error",
                  "Failed to load " <> path <> ": " <> loader.format_error(err),
                )
              })

            let next_actions = [
              json_output.next_action(
                "intent validate <spec.cue>",
                "Validate individual spec files",
              ),
              json_output.next_action(
                "intent quality <spec.cue>",
                "Analyze spec quality",
              ),
            ]

            let response =
              json_output.failure(
                "aggregate_error",
                "ai aggregate",
                json.object([]),
                error_messages,
                None,
                next_actions,
                exit_error,
              )

            json_output.output(response)
            halt(exit_error)
          }
          False -> {
            // All specs loaded successfully, perform aggregation
            let report = spec_aggregator.aggregate_specs(list.reverse(specs))

            let next_actions = [
              json_output.next_action(
                "intent quality <merged-spec.cue>",
                "Analyze quality of aggregated insights",
              ),
              json_output.next_action(
                "intent gaps <merged-spec.cue>",
                "Find coverage gaps",
              ),
              json_output.next_action(
                "intent beads <session-id>",
                "Generate implementation work items",
              ),
            ]

            let response =
              json_output.success(
                "ai_aggregate_result",
                "ai aggregate",
                spec_aggregator.to_json(report),
                None,
                next_actions,
              )

            json_output.output(response)
            halt(exit_pass)
          }
        }
      }
    }
  })
  |> glint.description(
    "Aggregate analysis from multiple specs to find common patterns, duplicates, and conflicts",
  )
}
