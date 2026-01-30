/// Interview Commands Module
/// Handles interview command, batch processing, CUE mode, and question navigation
import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/ffi
import intent/interview
import intent/interview_questions
import intent/interview_storage
import intent/json_output
import intent/question_types.{type Question}
import intent/spec_builder
import simplifile

const exit_pass = 0

const exit_fail = 1

const exit_invalid = 3

const exit_error = 4

const sessions_jsonl = ".intent/sessions.jsonl"

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn generate_uuid() -> String {
  ffi.generate_uuid()
}

fn current_timestamp() -> String {
  ffi.current_timestamp()
}

// ============================================================================
// Public Types
// ============================================================================

/// Batch input from JSON file
pub type BatchInput {
  BatchInput(profile: String, answers: List(BatchAnswer))
}

/// Single answer in batch input
pub type BatchAnswer {
  BatchAnswer(question_id: String, response: String)
}

// ============================================================================
// Public Functions
// ============================================================================

pub fn profile_to_string(profile: interview.Profile) -> String {
  case profile {
    interview.Api -> "api"
    interview.Cli -> "cli"
    interview.Event -> "event"
    interview.Data -> "data"
    interview.Workflow -> "workflow"
    interview.UI -> "ui"
  }
}

fn parse_profile(profile_str: String) -> Result(interview.Profile, String) {
  case string.lowercase(profile_str) {
    "api" -> Ok(interview.Api)
    "cli" -> Ok(interview.Cli)
    "event" -> Ok(interview.Event)
    "data" -> Ok(interview.Data)
    "workflow" -> Ok(interview.Workflow)
    "ui" -> Ok(interview.UI)
    _ ->
      Error(
        "Unknown profile '"
        <> profile_str
        <> "'. Valid profiles: api, cli, event, data, workflow, ui",
      )
  }
}

/// Parse batch input JSON from string
pub fn parse_batch_input_from_string(
  content: String,
) -> Result(BatchInput, String) {
  case json.decode(content, dynamic.dynamic) {
    Error(_) -> Error("Invalid JSON syntax")
    Ok(data) -> {
      let profile_decoder = dynamic.field("profile", dynamic.string)
      let profile_result = profile_decoder(data)

      let answer_decoder =
        dynamic.decode2(
          BatchAnswer,
          dynamic.field("question_id", dynamic.string),
          dynamic.field("response", dynamic.string),
        )
      let answers_decoder =
        dynamic.field("answers", dynamic.list(answer_decoder))
      let answers_result = answers_decoder(data)

      case profile_result, answers_result {
        Ok(profile), Ok(answers) -> {
          case parse_profile(profile) {
            Error(_) ->
              Error(
                "Invalid profile value: '"
                <> profile
                <> "'. Must be one of: api, cli, event, data, workflow, ui",
              )
            Ok(_) -> {
              case list.is_empty(answers) {
                True -> Error("Answers array cannot be empty")
                False -> Ok(BatchInput(profile: profile, answers: answers))
              }
            }
          }
        }
        Error(_), _ -> Error("Missing required field: profile")
        _, Error(_) -> Error("Missing required field: answers")
      }
    }
  }
}

/// The `interview` command - guided specification discovery
pub fn interview_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // Check for unexpected arguments (common mistake: profile as arg instead of flag)
    case input.args {
      [arg, ..] -> {
        io.println_error("Error: Unexpected argument '" <> arg <> "'")
        io.println_error("")
        io.println_error(
          "Did you mean: intent interview --profile=" <> arg <> " ?",
        )
        io.println_error("")
        io.println_error("Valid profiles: api, cli, event, data, workflow, ui")
        halt(exit_error)
      }
      [] -> Nil
    }

    let profile_result = flag.get_string(input.flags, "profile")
    let profile_str = result.unwrap(profile_result, "api")

    let resume_id =
      flag.get_string(input.flags, "resume")
      |> result.unwrap("")

    let session_flag =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    let answer_text =
      flag.get_string(input.flags, "answer")
      |> result.unwrap("")

    let dry_run =
      flag.get_bool(input.flags, "dry-run")
      |> result.unwrap(False)

    let batch_mode =
      flag.get_bool(input.flags, "batch")
      |> result.unwrap(False)

    let input_file =
      flag.get_string(input.flags, "input")
      |> result.unwrap("")

    let export_path =
      flag.get_string(input.flags, "export")
      |> result.unwrap("")

    // Batch mode: process all answers from JSON file
    case batch_mode {
      True -> {
        case string.is_empty(input_file) {
          True -> {
            io.println_error("Error: --input flag required with --batch mode")
            halt(exit_error)
          }
          False -> {
            run_interview_batch(input_file, export_path)
          }
        }
      }
      False -> {
        Nil
      }
    }

    // CUE mode: output CUE directives for AI agents
    let has_resume = !string.is_empty(resume_id)
    let has_session = !string.is_empty(session_flag)
    let has_answer = !string.is_empty(answer_text)

    case has_resume, has_session, has_answer {
      True, _, _ -> run_interview_cue_resume(resume_id, dry_run)
      False, True, True ->
        run_interview_cue_answer(session_flag, answer_text, dry_run)
      False, False, False -> {
        case profile_result {
          Error(_) -> {
            output_cue_error(
              "No flags provided. Use --profile to start a new interview, --resume to continue one, or --session with --answer to submit a response.\n\nExamples:\n  intent interview --profile=api\n  intent interview --resume=<session-id>\n  intent interview --session=<session-id> --answer='THE SYSTEM SHALL ...'",
            )
            halt(exit_error)
          }
          Ok(_) -> {
            let profile = parse_profile(profile_str)
            case profile {
              Ok(p) -> run_interview_cue_start(p, dry_run)
              Error(msg) -> {
                output_cue_error(msg)
                halt(exit_error)
              }
            }
          }
        }
      }
      False, False, True -> {
        output_cue_error("--answer requires --session flag")
        halt(exit_error)
      }
      False, True, False -> {
        output_cue_error(
          "--session requires --answer flag (use --resume to resume a session)",
        )
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "Guided specification discovery through structured interview",
  )
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.description(
        "System profile type: api, cli, event, data, workflow, or ui",
      ),
  )
  |> glint.flag(
    "resume",
    flag.string()
      |> flag.default("")
      |> flag.description("Resume existing interview session using its ID"),
  )
  |> glint.flag(
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description("Session identifier (required with --answer flag)"),
  )
  |> glint.flag(
    "answer",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Response value for current question (use with --session)",
      ),
  )
  |> glint.flag(
    "dry-run",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Preview interview questions without persisting to session storage",
      ),
  )
  |> glint.flag(
    "batch",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Batch mode: process all answers from JSON file non-interactively",
      ),
  )
  |> glint.flag(
    "input",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Path to JSON file with answers for batch mode (required with --batch)",
      ),
  )
  |> glint.flag(
    "export",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Path to write generated CUE spec (optional, spec returned in JSON output)",
      ),
  )
}

// ============================================================================
// Private Functions
// ============================================================================

fn parse_batch_input(file_path: String) -> Result(BatchInput, String) {
  case simplifile.read(file_path) {
    Error(_) -> Error("File not found: " <> file_path)
    Ok(content) -> parse_batch_input_from_string(content)
  }
}

fn run_interview_batch(input_file: String, export_path: String) -> Nil {
  let batch_result = parse_batch_input(input_file)
  case batch_result {
    Error(msg) -> {
      case
        string.contains(msg, "File not found")
        || string.contains(msg, "Invalid JSON")
      {
        True -> {
          io.println_error("Error: " <> msg)
          halt(exit_invalid)
        }
        False -> {
          io.println_error("Error: " <> msg)
          halt(exit_error)
        }
      }
    }
    Ok(batch_input) -> {
      let profile_result = parse_profile(batch_input.profile)
      case profile_result {
        Error(msg) -> {
          io.println_error("Error: " <> msg)
          halt(exit_error)
        }
        Ok(profile) -> {
          let session_id = "interview-" <> generate_uuid()
          let timestamp = current_timestamp()
          let session = interview.create_session(session_id, profile, timestamp)

          let updated_session =
            list.fold(batch_input.answers, session, fn(sess, batch_answer) {
              let answer =
                interview.Answer(
                  question_id: batch_answer.question_id,
                  question_text: "",
                  perspective: question_types.Developer,
                  round: 1,
                  response: batch_answer.response,
                  extracted: dict.new(),
                  confidence: 1.0,
                  notes: "",
                  timestamp: timestamp,
                )
              interview.add_answer(sess, answer)
            })

          let save_result =
            interview_storage.append_session_to_jsonl(
              updated_session,
              sessions_jsonl,
            )

          case save_result {
            Error(err) -> {
              io.println_error("Error saving session: " <> err)
              halt(exit_error)
            }
            Ok(_) -> {
              let spec_content =
                spec_builder.build_spec_from_session(updated_session)

              let spec_path = case string.is_empty(export_path) {
                True -> ""
                False -> {
                  case simplifile.write(export_path, spec_content) {
                    Ok(_) -> export_path
                    Error(_) -> {
                      io.println_error("Warning: Failed to write spec to file")
                      ""
                    }
                  }
                }
              }

              let output =
                json.object([
                  #("success", json.bool(True)),
                  #("session_id", json.string(session_id)),
                  #("profile", json.string(batch_input.profile)),
                  #(
                    "answers_processed",
                    json.int(list.length(batch_input.answers)),
                  ),
                  #("spec_generated", json.bool(True)),
                  #("spec_path", json.string(spec_path)),
                ])
              io.println(json.to_string(output))
              halt(exit_pass)
            }
          }
        }
      }
    }
  }
}

fn output_cue_error(message: String) -> Nil {
  let response =
    json_output.failure(
      "validation_error",
      "interview",
      json.null(),
      [
        json_output.error("invalid_input", message),
      ],
      None,
      [
        json_output.next_action(
          "intent interview --profile=api",
          "Start a new interview with valid profile",
        ),
        json_output.next_action(
          "intent help interview",
          "See interview command help",
        ),
      ],
      exit_error,
    )
  json_output.output(response)
}

fn run_interview_cue_start(profile: interview.Profile, dry_run: Bool) -> Nil {
  let session_id = case dry_run {
    True -> "dry-run-" <> generate_uuid()
    False -> "interview-" <> generate_uuid()
  }
  let timestamp = current_timestamp()
  let session = interview.create_session(session_id, profile, timestamp)

  let save_result = case dry_run {
    True -> Ok(Nil)
    False -> interview_storage.append_session_to_jsonl(session, sessions_jsonl)
  }

  case save_result {
    Ok(_) -> {
      case interview.get_first_question_for_round(session, 1) {
        Ok(question) -> output_cue_question(session, question, 1)
        Error(_) -> {
          output_cue_error("No questions available for this profile")
          halt(exit_error)
        }
      }
    }
    Error(err) -> {
      output_cue_error("Failed to save session: " <> err)
      halt(exit_error)
    }
  }
}

fn run_interview_cue_resume(session_id: String, dry_run: Bool) -> Nil {
  let is_dry_run_session = string.starts_with(session_id, "dry-run-")

  case interview_storage.get_session_from_jsonl(sessions_jsonl, session_id) {
    Error(err) -> {
      case is_dry_run_session || dry_run {
        True -> {
          output_cue_error(
            "Cannot resume dry-run session (not saved): " <> session_id,
          )
          halt(exit_error)
        }
        False -> {
          output_cue_error("Session not found: " <> err)
          halt(exit_error)
        }
      }
    }
    Ok(session) -> {
      case session.stage {
        interview.Complete -> {
          output_cue_complete(session)
        }
        _ -> {
          let next_round = case session.rounds_completed {
            0 -> 1
            r if r < 5 -> r + 1
            _ -> 5
          }
          case get_next_unanswered_question(session, next_round) {
            Some(question) -> output_cue_question(session, question, next_round)
            None -> {
              output_cue_complete(session)
            }
          }
        }
      }
    }
  }
}

fn get_next_unanswered_question(
  session: interview.InterviewSession,
  start_round: Int,
) -> Option(Question) {
  let profile_str = profile_to_string(session.profile)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  find_unanswered_in_rounds(profile_str, answered_ids, start_round)
}

fn find_unanswered_in_rounds(
  profile_str: String,
  answered_ids: List(String),
  round: Int,
) -> Option(Question) {
  case round > 5 {
    True -> None
    False -> {
      let questions =
        interview_questions.get_questions_for_round(profile_str, round)
      let unanswered =
        list.filter(questions, fn(q) { !list.contains(answered_ids, q.id) })
      case unanswered {
        [first, ..] -> Some(first)
        [] -> find_unanswered_in_rounds(profile_str, answered_ids, round + 1)
      }
    }
  }
}

fn get_next_unanswered_question_in_round(
  session: interview.InterviewSession,
  round: Int,
) -> Option(Question) {
  let profile_str = profile_to_string(session.profile)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  let questions =
    interview_questions.get_questions_for_round(profile_str, round)
  let unanswered =
    list.filter(questions, fn(q) { !list.contains(answered_ids, q.id) })

  case unanswered {
    [first, ..] -> Some(first)
    [] -> None
  }
}

fn run_interview_cue_answer(
  session_id: String,
  answer_text: String,
  dry_run: Bool,
) -> Nil {
  let is_dry_run_session = string.starts_with(session_id, "dry-run-")

  case interview_storage.get_session_from_jsonl(sessions_jsonl, session_id) {
    Error(err) -> {
      case is_dry_run_session || dry_run {
        True -> {
          output_cue_error(
            "Cannot answer dry-run session (not saved): " <> session_id,
          )
          halt(exit_error)
        }
        False -> {
          output_cue_error("Session not found: " <> err)
          halt(exit_error)
        }
      }
    }
    Ok(session) -> {
      case string.length(string.trim(answer_text)) < 3 {
        True -> {
          output_cue_validation_error(
            "Answer too short",
            "Please provide a more detailed response",
          )
          halt(exit_fail)
        }
        False -> {
          let next_round = case session.rounds_completed {
            0 -> 1
            r if r < 5 -> r + 1
            _ -> 5
          }

          case get_next_unanswered_question(session, next_round) {
            None -> {
              output_cue_complete(session)
            }
            Some(question) -> {
              let extracted =
                interview.extract_from_answer(
                  question.id,
                  answer_text,
                  question.extract_into,
                )
              let confidence =
                interview.calculate_confidence(
                  question.id,
                  answer_text,
                  extracted,
                )

              let answer =
                interview.Answer(
                  question_id: question.id,
                  question_text: question.question,
                  perspective: question.perspective,
                  round: next_round,
                  response: answer_text,
                  extracted: extracted,
                  confidence: confidence,
                  notes: "",
                  timestamp: current_timestamp(),
                )

              let updated_session = interview.add_answer(session, answer)

              let #(sess_with_gaps, _gaps) =
                interview.check_for_gaps(updated_session, question, answer)
              let #(sess_final, _conflicts) =
                interview.check_for_conflicts(sess_with_gaps, answer)

              let save_result = case is_dry_run_session || dry_run {
                True -> Ok(Nil)
                False ->
                  interview_storage.append_session_to_jsonl(
                    sess_final,
                    sessions_jsonl,
                  )
              }

              case save_result {
                Error(err) -> {
                  output_cue_error("Failed to save session: " <> err)
                  halt(exit_error)
                }
                Ok(_) -> {
                  case
                    get_next_unanswered_question_in_round(
                      sess_final,
                      next_round,
                    )
                  {
                    Some(next_q) -> {
                      output_cue_question(sess_final, next_q, next_round)
                    }
                    None -> {
                      let sess_round_completed =
                        interview.complete_round(sess_final)

                      let round_save_result = case
                        is_dry_run_session || dry_run
                      {
                        True -> Ok(Nil)
                        False ->
                          interview_storage.append_session_to_jsonl(
                            sess_round_completed,
                            sessions_jsonl,
                          )
                      }

                      case round_save_result {
                        Error(err) -> {
                          output_cue_error(
                            "Failed to save round completion: " <> err,
                          )
                          halt(exit_error)
                        }
                        Ok(_) -> {
                          case next_round < 5 {
                            True -> {
                              case
                                get_next_unanswered_question(
                                  sess_round_completed,
                                  next_round + 1,
                                )
                              {
                                Some(next_q) ->
                                  output_cue_question(
                                    sess_round_completed,
                                    next_q,
                                    next_round + 1,
                                  )
                                None ->
                                  output_cue_complete(sess_round_completed)
                              }
                            }
                            False -> output_cue_complete(sess_round_completed)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn output_cue_question(
  session: interview.InterviewSession,
  question: Question,
  round: Int,
) -> Nil {
  let profile_str = profile_to_string(session.profile)
  let total_questions = get_total_questions(profile_str)
  let answered_count = list.length(session.answers)
  let percent = case total_questions > 0 {
    True -> { answered_count * 100 } / total_questions
    False -> 0
  }

  let pattern = infer_ears_pattern(question)
  let hint = get_ears_hint(pattern)
  let examples = get_pattern_examples(pattern)

  let category = case round {
    1 -> "basic_info"
    2 -> "behaviors"
    3 -> "edge_cases"
    4 -> "security"
    _ -> "validation"
  }

  let is_dry_run = string.starts_with(session.id, "dry-run-")

  let output =
    "{\n"
    <> "\taction: \"ask_question\"\n\n"
    <> "\tquestion: {\n"
    <> "\t\ttext: \""
    <> escape_cue_string(question.question)
    <> "\"\n"
    <> "\t\tpattern: \""
    <> pattern
    <> "\"\n"
    <> "\t\texamples: ["
    <> format_cue_string_list(examples)
    <> "]\n"
    <> "\t\thint: \""
    <> escape_cue_string(hint)
    <> "\"\n"
    <> "\t}\n\n"
    <> "\tprogress: {\n"
    <> "\t\tcurrent_step: "
    <> string.inspect(answered_count + 1)
    <> "\n"
    <> "\t\ttotal_steps: "
    <> string.inspect(total_questions)
    <> "\n"
    <> "\t\tpercent_complete: "
    <> string.inspect(percent)
    <> "\n"
    <> "\t\tcategory: \""
    <> category
    <> "\"\n"
    <> "\t}\n\n"
    <> "\tsession: {\n"
    <> "\t\tid: \""
    <> session.id
    <> "\"\n"
    <> "\t\tprofile: \""
    <> profile_str
    <> "\"\n"
    <> "\t\tstarted_at: \""
    <> session.created_at
    <> "\"\n"
    <> case is_dry_run {
      True -> "\t\tdry_run: true\n"
      False -> ""
    }
    <> "\t}\n\n"
    <> "\tnext_action: {\n"
    <> "\t\tcommand: \"intent interview --session="
    <> session.id
    <> " --answer='<your-answer>'\"\n"
    <> "\t\tdescription: \"Submit your answer to continue the interview\"\n"
    <> "\t\texample: \"intent interview --session="
    <> session.id
    <> " --answer='THE SYSTEM SHALL validate all API inputs'\"\n"
    <> "\t}\n"
    <> "}"

  io.println(output)
  halt(exit_pass)
}

fn output_cue_validation_error(message: String, suggestion: String) -> Nil {
  io.println(
    "{\n\taction: \"validation_error\"\n\terror: {\n\t\tmessage: \""
    <> escape_cue_string(message)
    <> "\"\n\t\tsuggestion: \""
    <> escape_cue_string(suggestion)
    <> "\"\n\t\tretry_allowed: true\n\t}\n}",
  )
}

fn output_cue_complete(session: interview.InterviewSession) -> Nil {
  let behaviors_count = list.length(session.answers)
  let anti_patterns_count = list.length(session.gaps)
  let is_dry_run = string.starts_with(session.id, "dry-run-")

  let spec_path = case is_dry_run {
    True -> ""
    False -> ".intent/spec-" <> session.id <> ".cue"
  }

  case is_dry_run {
    False -> {
      let spec_cue = spec_builder.build_spec_from_session(session)
      let _ = simplifile.write(spec_path, spec_cue)
      Nil
    }
    True -> Nil
  }

  let summary = case is_dry_run {
    True ->
      "DRY RUN complete. No spec generated (use without --dry-run to save)."
    False ->
      "Interview complete. Generated spec with "
      <> string.inspect(behaviors_count)
      <> " behaviors."
  }

  let output =
    "{\n"
    <> "\taction: \"interview_complete\"\n\n"
    <> "\toutput: {\n"
    <> case is_dry_run {
      False -> "\t\tspec_path: \"" <> spec_path <> "\"\n"
      True -> ""
    }
    <> "\t\tbehaviors_count: "
    <> string.inspect(behaviors_count)
    <> "\n"
    <> "\t\tanti_patterns_count: "
    <> string.inspect(anti_patterns_count)
    <> "\n"
    <> "\t\tsummary: \""
    <> escape_cue_string(summary)
    <> "\"\n"
    <> case is_dry_run {
      True -> "\t\tdry_run: true\n"
      False -> ""
    }
    <> "\t}\n\n"
    <> "\tsession: {\n"
    <> "\t\tid: \""
    <> session.id
    <> "\"\n"
    <> "\t\tprofile: \""
    <> profile_to_string(session.profile)
    <> "\"\n"
    <> "\t\tstarted_at: \""
    <> session.created_at
    <> "\"\n"
    <> "\t\tcompleted_at: \""
    <> current_timestamp()
    <> "\"\n"
    <> case is_dry_run {
      True -> "\t\tdry_run: true\n"
      False -> ""
    }
    <> "\t}\n"
    <> "}"

  io.println(output)
  halt(exit_pass)
}

fn get_total_questions(profile_str: String) -> Int {
  list.range(1, 5)
  |> list.map(fn(round) {
    interview_questions.get_questions_for_round(profile_str, round)
  })
  |> list.map(list.length)
  |> list.fold(0, fn(acc, n) { acc + n })
}

fn infer_ears_pattern(question: Question) -> String {
  let q_lower = string.lowercase(question.question)

  case
    string.contains(q_lower, "when"),
    string.contains(q_lower, "while"),
    string.contains(q_lower, "if"),
    string.contains(q_lower, "should not"),
    string.contains(q_lower, "optional")
  {
    True, True, _, _, _ -> "complex"
    True, False, _, _, _ -> "event_driven"
    False, True, _, _, _ -> "state_driven"
    _, _, True, True, _ -> "unwanted"
    _, _, _, _, True -> "optional"
    _, _, _, _, _ -> "ubiquitous"
  }
}

fn get_ears_hint(pattern: String) -> String {
  case pattern {
    "ubiquitous" -> "Use format: THE SYSTEM SHALL [behavior]"
    "event_driven" -> "Use format: WHEN [trigger] THE SYSTEM SHALL [behavior]"
    "state_driven" -> "Use format: WHILE [state] THE SYSTEM SHALL [behavior]"
    "optional" -> "Use format: WHERE [condition] THE SYSTEM SHALL [behavior]"
    "unwanted" -> "Use format: IF [condition] THE SYSTEM SHALL NOT [behavior]"
    "complex" ->
      "Use format: WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]"
    _ -> "Use EARS format: THE SYSTEM SHALL [behavior]"
  }
}

fn get_pattern_examples(pattern: String) -> List(String) {
  case pattern {
    "ubiquitous" -> [
      "THE SYSTEM SHALL validate all API inputs",
      "THE SYSTEM SHALL log all requests",
    ]
    "event_driven" -> [
      "WHEN user submits form THE SYSTEM SHALL validate data",
      "WHEN request times out THE SYSTEM SHALL retry",
    ]
    "state_driven" -> [
      "WHILE user is authenticated THE SYSTEM SHALL allow access",
      "WHILE rate limit exceeded THE SYSTEM SHALL reject requests",
    ]
    "optional" -> [
      "WHERE user has admin role THE SYSTEM SHALL allow admin actions",
    ]
    "unwanted" -> [
      "IF token is expired THE SYSTEM SHALL NOT authorize requests",
    ]
    "complex" -> [
      "WHILE in transaction WHEN error occurs THE SYSTEM SHALL rollback",
    ]
    _ -> ["THE SYSTEM SHALL [describe behavior]"]
  }
}

fn format_cue_string_list(items: List(String)) -> String {
  items
  |> list.map(fn(s) { "\"" <> escape_cue_string(s) <> "\"" })
  |> string.join(", ")
}
