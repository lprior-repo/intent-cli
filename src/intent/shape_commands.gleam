/// Shape Commands Module
///
/// Commands for managing Shape phase sessions:
/// - shape_start: initialize new shape phase session
/// - shape_check: validate shape session completeness
/// - shape_critique: generate critique questions for spec
/// - shape_respond: process critique responses
/// - shape_agree: finalize shape phase agreement

import gleam/json
import gleam/option.{None}
import gleam/result
import glint
import glint/flag
import intent/ffi
import intent/json_output

const exit_pass = 0

/// Parent command for shape phase - shows available subcommands
pub fn shape_group_command() -> glint.Command(Nil) {
  glint.command(fn(_input: glint.CommandInput) {
    let data =
      json.object([
        #("phase", json.string("shape")),
        #("description", json.string("Phase 3: Shape implementation approach")),
        #(
          "subcommands",
          json.array(
            [
              #("start", "Initialize a new shape session"),
              #("check", "Validate shape session completeness"),
              #("critique", "Run Shape critique"),
              #("respond", "Submit responses to critique issues"),
              #("agree", "Finalize shape phase agreement"),
            ],
            fn(pair) {
              let #(cmd, desc) = pair
              json.object([
                #("command", json.string("intent shape " <> cmd)),
                #("description", json.string(desc)),
              ])
            },
          ),
        ),
      ])
    let response =
      json_output.success("shape_help", "shape", data, None, [
        json_output.next_action(
          "intent shape start <spec.cue>",
          "Start a new shape session",
        ),
      ])
    json_output.output(response)
  })
  |> glint.description("Shape phase: Shape implementation approach")
}

const exit_error = 4

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

pub fn shape_start_command() -> glint.Command(Nil) {
  glint.command(fn(_input: glint.CommandInput) {
    let session_id = ffi.generate_uuid()
    let timestamp = ffi.current_timestamp()

    let data =
      json.object([
        #("session_id", json.string(session_id)),
        #("phase", json.string("shape")),
        #("status", json.string("in_progress")),
        #("created_at", json.string(timestamp)),
      ])

    let next_actions = [
      json_output.next_action(
        "intent shape check --session=" <> session_id,
        "Check session status",
      ),
      json_output.next_action(
        "intent shape critique --session=" <> session_id,
        "Run critique on shape session",
      ),
    ]

    let response =
      json_output.success(
        "shape_start_result",
        "shape start",
        data,
        None,
        next_actions,
      )

    json_output.output(response)
    halt(exit_pass)
  })
  |> glint.description("Start a new shape phase session")
}

/// Check shape session status
pub fn shape_check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_check_error",
            "shape check",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("status", json.string("in_progress")),
            #("answered_count", json.int(0)),
            #("gaps", json.array([], fn(x) { json.string(x) })),
          ])

        let next_actions = [
          json_output.next_action(
            "intent shape critique --session=" <> session_id,
            "Run critique on shape session",
          ),
        ]

        let response =
          json_output.success(
            "shape_check_result",
            "shape check",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Check shape session status")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to check"),
  )
}

/// Run shape critique
pub fn shape_critique_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_critique_error",
            "shape critique",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("passed", json.bool(False)),
            #("score", json.int(0)),
            #("issues", json.array([], fn(x) { json.string(x) })),
          ])

        let next_actions = [
          json_output.next_action(
            "intent shape respond --session="
              <> session_id
              <> " --question=<qid> --answer='...'",
            "Respond to critique issues",
          ),
          json_output.next_action(
            "intent shape agree --session=" <> session_id,
            "Finalize shape if critique passed",
          ),
        ]

        let response =
          json_output.success(
            "shape_critique_result",
            "shape critique",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Run Pragmatic Tech Lead critique on shape session")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to critique"),
  )
}

/// Submit answer to shape question
pub fn shape_respond_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")
    let question_id =
      flag.get_string(input.flags, "question") |> result.unwrap("")
    let answer = flag.get_string(input.flags, "answer") |> result.unwrap("")

    case session_id, question_id, answer {
      "", _, _ | _, "", _ | _, _, "" -> {
        let error =
          json_output.error(
            "missing_required_fields",
            "session, question, and answer are required",
          )

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_respond_error",
            "shape respond",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _, _, _ -> {
        let timestamp = ffi.current_timestamp()

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("question_id", json.string(question_id)),
            #("answered", json.bool(True)),
            #("timestamp", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent shape critique --session=" <> session_id,
            "Re-run critique to check progress",
          ),
        ]

        let response =
          json_output.success(
            "shape_respond_result",
            "shape respond",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Submit answer to a shape question")
  |> glint.flag("session", flag.string() |> flag.description("Session ID"))
  |> glint.flag(
    "question",
    flag.string() |> flag.description("Question ID to answer"),
  )
  |> glint.flag("answer", flag.string() |> flag.description("Answer text"))
}

/// Finalize shape session
pub fn shape_agree_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent shape start",
            "Start a new shape session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "shape_agree_error",
            "shape agree",
            json.object([]),
            [error],
            None,
            next_actions,
            1,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let timestamp = ffi.current_timestamp()

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("status", json.string("complete")),
            #("finalized_at", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent spec start --vision-session=<vision-id> --shape-session="
              <> session_id,
            "Start Spec phase with this shape session",
          ),
          json_output.next_action(
            "intent beads <session-id>",
            "Generate implementation beads",
          ),
        ]

        let response =
          json_output.success(
            "shape_agree_result",
            "shape agree",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Finalize and complete shape session")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to finalize"),
  )
}
