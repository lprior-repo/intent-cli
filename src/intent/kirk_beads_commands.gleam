/// KIRK Beads Commands Module
///
/// CLI commands for generating enhanced beads from KIRK analysis:
/// - kirk-beads: Generate enhanced beads from KIRK analysis
/// - bead-show: Show full bead details
/// - bead-verify: Verify acceptance criteria

import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/enhanced_bead_generator
import intent/json_output
import intent/kirk_to_beads
import intent/loader

// ============================================================================
// Exit Codes
// ============================================================================

const exit_pass = 0

const exit_fail = 1

const exit_invalid = 3

// ============================================================================
// External Function Declarations
// ============================================================================

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

// ============================================================================
// kirk-beads Command
// ============================================================================

pub fn kirk_beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let round_flag =
      flag.get_int(input.flags, "round")
      |> result.unwrap(0)

    let min_severity_flag =
      flag.get_string(input.flags, "min-severity")
      |> result.unwrap("")

    case input.args {
      [spec_path, ..] -> {
        case loader.load_spec_quiet(spec_path) {
          Ok(spec) -> {
            // Generate all beads
            let all_beads =
              kirk_to_beads.generate_all_beads(spec, spec_path)

            // Apply filters
            let filtered = case round_flag {
              0 -> all_beads
              r -> kirk_to_beads.filter_by_round(all_beads, r)
            }

            let filtered = case min_severity_flag {
              "" -> filtered
              sev -> kirk_to_beads.filter_by_min_severity(filtered, sev)
            }

            // Group by round
            let by_round =
              filtered
              |> list.group(fn(b) { b.round })

            // Build output
            let data =
              json.object([
                #("total", json.int(list.length(filtered))),
                #(
                  "by_round",
                  json.object(
                    by_round
                    |> dict.to_list
                    |> list.map(fn(pair) {
                      let #(round, beads) = pair
                      #(
                        "round_" <> int.to_string(round),
                        json.object([
                          #("count", json.int(list.length(beads))),
                          #(
                            "beads",
                            json.array(
                              beads,
                              enhanced_bead_generator.enhanced_bead_to_json,
                            ),
                          ),
                        ]),
                      )
                    }),
                  ),
                ),
                #(
                  "beads",
                  json.array(
                    filtered,
                    enhanced_bead_generator.enhanced_bead_to_json,
                  ),
                ),
              ])

            let response =
              json_output.success(
                "kirk_beads_result",
                "kirk-beads",
                data,
                Some(spec_path),
                [
                  json_output.next_action(
                    "intent bead-show <bead-id>",
                    "Show full bead details",
                  ),
                ],
              )
            json_output.output(response)
            halt(exit_pass)
          }
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "kirk_beads_failed",
                "kirk-beads",
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
            "kirk_beads_failed",
            "kirk-beads",
            json.null(),
            [
              json_output.detailed_error(
                "missing_argument",
                "No spec path provided",
                "",
                "Provide a path to a CUE spec file",
                "intent kirk-beads <spec.cue>",
              ),
            ],
            None,
            [],
            exit_invalid,
          )
        json_output.output(response)
        halt(exit_invalid)
      }
    }
  })
  |> glint.description("Generate enhanced beads from KIRK analysis")
  |> glint.flag(
    "round",
    flag.int()
      |> flag.default(0)
      |> flag.description("Filter beads by round (1-5, 0=all)"),
  )
  |> glint.flag(
    "min-severity",
    flag.string()
      |> flag.default("")
      |> flag.description("Minimum severity to include (low, medium, high, critical)"),
  )
}

// ============================================================================
// bead-show Command
// ============================================================================

pub fn bead_show_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let spec_path_flag =
      flag.get_string(input.flags, "spec")
      |> result.unwrap("")

    case input.args {
      [bead_id, ..] -> {
        case spec_path_flag {
          "" -> {
            let response =
              json_output.failure(
                "bead_show_failed",
                "bead-show",
                json.null(),
                [
                  json_output.detailed_error(
                    "missing_argument",
                    "No spec path provided",
                    "",
                    "Provide --spec flag with path to CUE spec",
                    "intent bead-show " <> bead_id <> " --spec=spec.cue",
                  ),
                ],
                None,
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
          spec_path -> {
            case loader.load_spec_quiet(spec_path) {
              Ok(spec) -> {
                let all_beads =
                  kirk_to_beads.generate_all_beads(spec, spec_path)

                case list.find(all_beads, fn(b) { b.id == bead_id }) {
                  Ok(bead) -> {
                    let data = enhanced_bead_generator.enhanced_bead_to_json(bead)
                    let response =
                      json_output.success(
                        "bead_show_result",
                        "bead-show",
                        data,
                        Some(spec_path),
                        [
                          json_output.next_action(
                            "intent bead-verify " <> bead_id <> " --spec=" <> spec_path,
                            "Verify acceptance criteria",
                          ),
                        ],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  Error(_) -> {
                    let available_ids =
                      all_beads
                      |> list.map(fn(b) { b.id })
                      |> list.take(10)
                      |> string.join(", ")

                    let response =
                      json_output.failure(
                        "bead_show_failed",
                        "bead-show",
                        json.object([
                          #("available_sample", json.string(available_ids)),
                          #("total_beads", json.int(list.length(all_beads))),
                        ]),
                        [
                          json_output.detailed_error(
                            "bead_not_found",
                            "Bead not found: " <> bead_id,
                            "",
                            "Use intent kirk-beads to see available beads",
                            "intent kirk-beads " <> spec_path,
                          ),
                        ],
                        Some(spec_path),
                        [],
                        exit_fail,
                      )
                    json_output.output(response)
                    halt(exit_fail)
                  }
                }
              }
              Error(e) -> {
                let error_msg = loader.format_error(e)
                let response =
                  json_output.failure(
                    "bead_show_failed",
                    "bead-show",
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
        }
      }
      [] -> {
        let response =
          json_output.failure(
            "bead_show_failed",
            "bead-show",
            json.null(),
            [
              json_output.detailed_error(
                "missing_argument",
                "No bead ID provided",
                "",
                "Provide a bead ID to show",
                "intent bead-show <bead-id> --spec=spec.cue",
              ),
            ],
            None,
            [],
            exit_invalid,
          )
        json_output.output(response)
        halt(exit_invalid)
      }
    }
  })
  |> glint.description("Show full bead details")
  |> glint.flag(
    "spec",
    flag.string()
      |> flag.default("")
      |> flag.description("Path to CUE spec file"),
  )
}

// ============================================================================
// bead-verify Command
// ============================================================================

pub fn bead_verify_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let spec_path_flag =
      flag.get_string(input.flags, "spec")
      |> result.unwrap("")

    case input.args {
      [bead_id, ..] -> {
        case spec_path_flag {
          "" -> {
            let response =
              json_output.failure(
                "bead_verify_failed",
                "bead-verify",
                json.null(),
                [
                  json_output.detailed_error(
                    "missing_argument",
                    "No spec path provided",
                    "",
                    "Provide --spec flag with path to CUE spec",
                    "intent bead-verify " <> bead_id <> " --spec=spec.cue",
                  ),
                ],
                None,
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
          spec_path -> {
            case loader.load_spec_quiet(spec_path) {
              Ok(spec) -> {
                let all_beads =
                  kirk_to_beads.generate_all_beads(spec, spec_path)

                case list.find(all_beads, fn(b) { b.id == bead_id }) {
                  Ok(bead) -> {
                    // For now, just report the acceptance criteria
                    // Full verification would require additional logic
                    let criteria_data =
                      bead.acceptance_criteria
                      |> list.map(fn(ac) {
                        json.object([
                          #("id", json.string(ac.id)),
                          #("description", json.string(ac.description)),
                          #("verification_type", json.string(ac.verification_type)),
                          #(
                            "check_expression",
                            json.nullable(ac.check_expression, json.string),
                          ),
                          #("verified", json.bool(ac.verified)),
                        ])
                      })

                    let data =
                      json.object([
                        #("bead_id", json.string(bead.id)),
                        #("title", json.string(bead.title)),
                        #("status", json.string(bead.status)),
                        #("criteria", json.preprocessed_array(criteria_data)),
                        #(
                          "verified_count",
                          json.int(
                            bead.acceptance_criteria
                            |> list.filter(fn(ac) { ac.verified })
                            |> list.length(),
                          ),
                        ),
                        #(
                          "total_count",
                          json.int(list.length(bead.acceptance_criteria)),
                        ),
                      ])

                    let response =
                      json_output.success(
                        "bead_verify_result",
                        "bead-verify",
                        data,
                        Some(spec_path),
                        [],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  Error(_) -> {
                    let response =
                      json_output.failure(
                        "bead_verify_failed",
                        "bead-verify",
                        json.null(),
                        [
                          json_output.detailed_error(
                            "bead_not_found",
                            "Bead not found: " <> bead_id,
                            "",
                            "Use intent kirk-beads to see available beads",
                            "intent kirk-beads " <> spec_path,
                          ),
                        ],
                        Some(spec_path),
                        [],
                        exit_fail,
                      )
                    json_output.output(response)
                    halt(exit_fail)
                  }
                }
              }
              Error(e) -> {
                let error_msg = loader.format_error(e)
                let response =
                  json_output.failure(
                    "bead_verify_failed",
                    "bead-verify",
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
        }
      }
      [] -> {
        let response =
          json_output.failure(
            "bead_verify_failed",
            "bead-verify",
            json.null(),
            [
              json_output.detailed_error(
                "missing_argument",
                "No bead ID provided",
                "",
                "Provide a bead ID to verify",
                "intent bead-verify <bead-id> --spec=spec.cue",
              ),
            ],
            None,
            [],
            exit_invalid,
          )
        json_output.output(response)
        halt(exit_invalid)
      }
    }
  })
  |> glint.description("Verify bead acceptance criteria")
  |> glint.flag(
    "spec",
    flag.string()
      |> flag.default("")
      |> flag.description("Path to CUE spec file"),
  )
}
