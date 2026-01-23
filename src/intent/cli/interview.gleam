/// Interview CLI commands
/// Guided specification discovery through structured interview
///
/// Commands:
/// - interview: Start or resume an interview session
/// - beads: Generate work items from an interview session
/// - bead-status: Mark bead execution status
/// - history: Show session history
/// - diff: Compare two sessions
/// - sessions: List all sessions
import gleam/dict
import gleam/io
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/answer_loader
import intent/bead_feedback
import intent/bead_templates
import intent/cli_ui
import intent/output_mode
import intent/interview
import intent/interview_questions
import intent/interview_storage
import intent/question_types.{type Question}
import intent/spec_builder
import intent/stdin
import simplifile

/// Exit codes
const exit_pass = 0

const exit_fail = 1

const exit_error = 4

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

@external(erlang, "intent_ffi", "generate_uuid")
fn generate_uuid() -> String

@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String

/// The `interview` command - guided specification discovery
pub fn interview_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let profile_str =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("api")

    let resume_id =
      flag.get_string(input.flags, "resume")
      |> result.unwrap("")

    let export_to =
      flag.get_string(input.flags, "export")
      |> result.unwrap("")

    let answers_file =
      flag.get_string(input.flags, "answers")
      |> result.unwrap("")

    let strict_mode =
      flag.get_bool(input.flags, "strict")
      |> result.unwrap(False)

    let cue_mode =
      flag.get_bool(input.flags, "cue")
      |> result.unwrap(False)

    let session_flag =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    let answer_text =
      flag.get_string(input.flags, "answer")
      |> result.unwrap("")

    // CUE mode: output CUE directives for AI agents
    case cue_mode {
      True -> {
        // Check if this is answering a question or starting/resuming
        let has_session = !string.is_empty(session_flag)
        let has_answer = !string.is_empty(answer_text)

        case has_session, has_answer {
          // Submitting an answer to an existing session
          True, True -> run_interview_cue_answer(session_flag, answer_text)
          // Resume session in CUE mode
          True, False -> run_interview_cue_resume(session_flag)
          // Start new session in CUE mode
          False, False -> {
            let profile = parse_profile(profile_str)
            case profile {
              Ok(p) -> run_interview_cue_start(p)
              Error(msg) -> {
                output_cue_error(msg)
                halt(exit_error)
              }
            }
          }
          // Invalid: answer without session
          False, True -> {
            output_cue_error("--answer requires --session flag")
            halt(exit_error)
          }
        }
      }
      False -> {
        // Regular interactive mode
        case resume_id {
          "" ->
            // Use parse_profile to avoid duplicating profile matching logic
            case parse_profile(profile_str) {
              Ok(profile) ->
                run_interview(profile, answers_file, strict_mode, export_to)
              Error(msg) -> {
                io.println_error("Error: " <> msg)
                halt(exit_error)
              }
            }
          id -> run_resume_interview(id, export_to)
        }
      }
    }
  })
  |> glint.description(
    "Guided specification discovery through structured interview",
  )
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("api")
      |> flag.description(
        "System profile: api, cli, event, data, workflow, or ui",
      ),
  )
  |> glint.flag(
    "resume",
    flag.string()
      |> flag.default("")
      |> flag.description("Resume existing interview session by ID"),
  )
  |> glint.flag(
    "answers",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Path to CUE file with pre-filled answers for non-interactive mode",
      ),
  )
  |> glint.flag(
    "strict",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Strict mode: fail if answers file is missing required answers (requires --answers)",
      ),
  )
  |> glint.flag(
    "export",
    flag.string()
      |> flag.default("")
      |> flag.description("Export completed interview to spec file"),
  )
  |> glint.flag(
    "cue",
    flag.bool()
      |> flag.default(False)
      |> flag.description(
        "Output CUE directives for AI agents (non-interactive)",
      ),
  )
  |> glint.flag(
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description("Session ID for CUE mode (use with --cue)"),
  )
  |> glint.flag(
    "answer",
    flag.string()
      |> flag.default("")
      |> flag.description(
        "Submit answer to current question (use with --cue --session)",
      ),
  )
}

fn run_interview(
  profile: interview.Profile,
  answers_file: String,
  strict_mode: Bool,
  export_to: String,
) -> Nil {
  // Initialize session
  let session_id = "interview-" <> generate_uuid()
  let timestamp = current_timestamp()

  let session = interview.create_session(session_id, profile, timestamp)

  // Load answers from file if provided
  let answers_dict = case string.is_empty(answers_file) {
    True -> option.None
    False -> {
      case answer_loader.load_from_file(answers_file) {
        Ok(dict) -> {
          io.println("")
          io.println(
            "✓ Loaded "
            <> string.inspect(dict.size(dict))
            <> " pre-filled answers from: "
            <> answers_file,
          )
          option.Some(dict)
        }
        Error(err) -> {
          case strict_mode {
            True -> {
              io.println_error(
                "✗ Failed to load answers file: "
                <> answer_loader_error_to_string(err),
              )
              halt(exit_error)
              option.None
              // unreachable, but needed for type consistency
            }
            False -> {
              io.println(
                "⚠ Failed to load answers file: "
                <> answer_loader_error_to_string(err),
              )
              io.println("  Continuing in interactive mode...")
              option.None
            }
          }
        }
      }
    }
  }

  // Print welcome message
  io.println("")
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("                    INTENT INTERVIEW")
  io.println(
    "═══════════════════════════════════════════════════════════════════",
  )
  io.println("")
  io.println("Profile: " <> profile_to_display_string(profile))
  io.println("Session: " <> session_id)
  case answers_dict {
    option.None -> Nil
    option.Some(_) -> io.println("Mode: Non-interactive (answers from file)")
  }
  io.println("")
  io.println("This guided interview will help us discover and refine your")
  io.println("specification through structured questioning.")
  io.println("")
  io.println("We'll ask questions across 5 rounds × multiple perspectives:")
  io.println("  • Round 1: Core Intent (what are you building?)")
  io.println("  • Round 2: Scope & Boundaries (what's in/out?)")
  io.println("  • Round 3: Error Cases (what can go wrong?)")
  io.println("  • Round 4: Security & Compliance (how do we keep it safe?)")
  io.println("  • Round 5: Operations (how does it run in production?)")
  io.println("")
  io.println("Press Ctrl+C to save and exit at any time.")
  io.println("Session will be saved to: .interview/sessions.jsonl")
  io.println("")
  io.println("Ready? Let's begin.")
  io.println("")

  // Run the interview loop
  let final_session = interview_loop(session, 1)

  // Save session to JSONL
  let save_result =
    interview_storage.append_session_to_jsonl(
      final_session,
      ".interview/sessions.jsonl",
    )

  case save_result {
    Ok(Nil) -> {
      io.println("")
      io.println("✓ Session saved: " <> session_id)
    }
    Error(err) -> {
      io.println_error("✗ Failed to save session: " <> err)
    }
  }

  // Export to spec if requested
  case export_to {
    "" -> Nil
    path -> {
      let spec_cue = spec_builder.build_spec_from_session(final_session)
      case simplifile.write(path, spec_cue) {
        Ok(Nil) -> {
          io.println("✓ Spec exported to: " <> path)
        }
        Error(err) -> {
          io.println_error("✗ Failed to export spec: " <> string.inspect(err))
        }
      }
    }
  }

  halt(exit_pass)
}

/// Resume an existing interview session
fn run_resume_interview(session_id: String, export_to: String) -> Nil {
  let mode = output_mode.Interactive
  let jsonl_path = ".interview/sessions.jsonl"

  // Load the session from JSONL
  case interview_storage.get_session_from_jsonl(jsonl_path, session_id) {
    Error(err) -> {
      cli_ui.print_error(err, mode)
      halt(exit_error)
    }
    Ok(session) -> {
      cli_ui.print_header("Resuming Interview: " <> session.id, mode)
      cli_ui.print_info(
        "Profile: " <> profile_to_display_string(session.profile),
        mode,
      )
      io.println("")

      // Show progress
      io.println("Progress:")
      io.println(
        "  • Answers collected: "
        <> string.inspect(list.length(session.answers)),
      )
      io.println(
        "  • Gaps detected: " <> string.inspect(list.length(session.gaps)),
      )
      io.println(
        "  • Conflicts detected: "
        <> string.inspect(list.length(session.conflicts)),
      )
      io.println("")

      // Determine which round to resume from
      let next_round = case session.rounds_completed {
        0 -> 1
        r if r < 5 -> r + 1
        _ -> 5
      }

      io.println("Resuming from Round " <> string.inspect(next_round))
      io.println("")

      // Continue the interview from the next round
      let final_session = interview_loop(session, next_round)

      // Save updated session
      let save_result =
        interview_storage.append_session_to_jsonl(final_session, jsonl_path)

      case save_result {
        Ok(Nil) -> {
          io.println("")
          cli_ui.print_success("Session updated: " <> session.id, mode)
        }
        Error(err) -> {
          cli_ui.print_error("Failed to save session: " <> err, mode)
        }
      }

      // Export to spec if requested
      case export_to {
        "" -> Nil
        path -> {
          let spec_cue = spec_builder.build_spec_from_session(final_session)
          case simplifile.write(path, spec_cue) {
            Ok(Nil) -> {
              cli_ui.print_success("Spec exported to: " <> path, mode)
            }
            Error(err) -> {
              cli_ui.print_error(
                "Failed to export spec: " <> string.inspect(err),
                mode,
              )
            }
          }
        }
      }

      halt(exit_pass)
    }
  }
}

/// Main interview loop - asks questions round by round
fn interview_loop(
  session: interview.InterviewSession,
  round: Int,
) -> interview.InterviewSession {
  case round > 5 {
    True -> session
    False -> {
      io.println("")
      io.println(
        "═══════════════════════════════════════════════════════════════════",
      )
      io.println("ROUND " <> string.inspect(round) <> "/5")
      io.println(
        "═══════════════════════════════════════════════════════════════════",
      )
      io.println("")

      // Get questions for this round
      case interview.get_first_question_for_round(session, round) {
        Error(_) -> {
          io.println("(No questions for this round)")
          interview_loop(session, round + 1)
        }
        Ok(first_question) -> {
          // Ask all questions in this round
          let updated_session =
            ask_questions_in_round(session, round, first_question)

          // Check for blocking gaps before proceeding
          let blocking_gaps = interview.get_blocking_gaps(updated_session)
          case blocking_gaps {
            [] -> interview_loop(updated_session, round + 1)
            gaps -> {
              io.println("")
              io.println("⚠️ BLOCKING GAPS DETECTED:")
              list.each(gaps, fn(gap) {
                io.println("  • " <> gap.description)
                io.println("    " <> gap.why_needed)
              })
              io.println("")
              interview_loop(updated_session, round + 1)
            }
          }
        }
      }
    }
  }
}

/// Ask all unanswered questions in a round
fn ask_questions_in_round(
  session: interview.InterviewSession,
  round: Int,
  _current_question: Question,
) -> interview.InterviewSession {
  let profile_str = profile_to_string(session.profile)

  // Get all questions for this round
  let questions =
    interview_questions.get_questions_for_round(profile_str, round)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  // Filter to unanswered questions
  let unanswered =
    list.filter(questions, fn(q) { !list.contains(answered_ids, q.id) })

  // Ask each unanswered question
  list.fold(unanswered, session, fn(sess, question) {
    ask_single_question(sess, question, round)
  })
}

/// Ask a single question and collect answer
fn ask_single_question(
  session: interview.InterviewSession,
  question: Question,
  round: Int,
) -> interview.InterviewSession {
  io.println("")
  io.print("Q" <> string.inspect(question.priority) <> ": ")
  io.println(question.question)

  case string.length(question.context) > 0 {
    True -> io.println("   Context: " <> question.context)
    False -> Nil
  }

  case string.length(question.example) > 0 {
    True -> io.println("   Example: " <> question.example)
    False -> Nil
  }

  io.print("")

  // Read answer from stdin with validation
  let answer_text = case stdin.prompt_for_answer("> ") {
    Ok(text) -> text
    Error(err) -> {
      io.println_error("Error reading input: " <> err)
      io.println("")
      // Return placeholder if input fails
      "(input error - please try again)"
    }
  }

  // Extract fields from answer
  let extracted =
    interview.extract_from_answer(
      question.id,
      answer_text,
      question.extract_into,
    )

  // Calculate confidence
  let confidence =
    interview.calculate_confidence(question.id, answer_text, extracted)

  // Create answer record
  let answer =
    interview.Answer(
      question_id: question.id,
      question_text: question.question,
      perspective: question.perspective,
      round: round,
      response: answer_text,
      extracted: extracted,
      confidence: confidence,
      notes: "",
      timestamp: current_timestamp(),
    )

  // Add to session
  let updated_session = interview.add_answer(session, answer)

  // Check for gaps and conflicts
  let #(sess_with_gaps, _gaps) =
    interview.check_for_gaps(updated_session, question, answer)

  let #(sess_final, _conflicts) =
    interview.check_for_conflicts(sess_with_gaps, answer)

  sess_final
}

/// Helper: convert Profile to string for questions module
fn profile_to_string(profile: interview.Profile) -> String {
  case profile {
    interview.Api -> "api"
    interview.Cli -> "cli"
    interview.Event -> "event"
    interview.Data -> "data"
    interview.Workflow -> "workflow"
    interview.UI -> "ui"
  }
}

fn profile_to_display_string(profile: interview.Profile) -> String {
  case profile {
    interview.Api -> "API"
    interview.Cli -> "CLI"
    interview.Event -> "Event System"
    interview.Data -> "Data System"
    interview.Workflow -> "Workflow"
    interview.UI -> "User Interface"
  }
}

// =============================================================================
// CUE MODE INTERVIEW FUNCTIONS
// =============================================================================

/// Parse profile string to Profile type
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

/// Output a CUE error directive
fn output_cue_error(message: String) -> Nil {
  io.println(
    "interview: {
  status: \"error\"
  error: \""
    <> escape_cue_string(message)
    <> "\"
}",
  )
}

fn run_interview_cue_start(profile: interview.Profile) -> Nil {
  // Initialize session
  let session_id = "interview-" <> generate_uuid()
  let timestamp = current_timestamp()

  let session = interview.create_session(session_id, profile, timestamp)

  // Save initial session
  let _save_result =
    interview_storage.append_session_to_jsonl(
      session,
      ".interview/sessions.jsonl",
    )

  // Get first question
  case interview.get_first_question_for_round(session, 1) {
    Error(_) -> {
      io.println(
        "interview: {
  status: \"error\"
  error: \"No questions available for this profile\"
}",
      )
      halt(exit_error)
    }
    Ok(question) -> {
      output_cue_question(session, question, 1)
      halt(exit_pass)
    }
  }
}

fn run_interview_cue_resume(session_id: String) -> Nil {
  let jsonl_path = ".interview/sessions.jsonl"

  case interview_storage.get_session_from_jsonl(jsonl_path, session_id) {
    Error(err) -> {
      output_cue_error(err)
      halt(exit_error)
    }
    Ok(session) -> {
      // Determine current round
      let round = case session.rounds_completed {
        0 -> 1
        r if r < 5 -> r + 1
        _ -> 5
      }

      // Check if complete
      case session.stage {
        interview.Complete -> {
          output_cue_complete(session)
          halt(exit_pass)
        }
        _ -> {
          // Find next unanswered question
          let profile_str = profile_to_string(session.profile)
          let questions =
            interview_questions.get_questions_for_round(profile_str, round)
          let answered_ids = list.map(session.answers, fn(a) { a.question_id })

          case
            list.find(questions, fn(q) { !list.contains(answered_ids, q.id) })
          {
            Ok(question) -> {
              output_cue_question(session, question, round)
              halt(exit_pass)
            }
            Error(_) -> {
              // No more questions in this round, check next round
              case round < 5 {
                True -> {
                  // Update session to next round
                  let updated_session =
                    interview.InterviewSession(
                      ..session,
                      rounds_completed: round,
                    )
                  let _save =
                    interview_storage.append_session_to_jsonl(
                      updated_session,
                      jsonl_path,
                    )
                  run_interview_cue_resume(session_id)
                }
                False -> {
                  // Complete
                  let updated_session =
                    interview.InterviewSession(
                      ..session,
                      stage: interview.Complete,
                    )
                  let _save =
                    interview_storage.append_session_to_jsonl(
                      updated_session,
                      jsonl_path,
                    )
                  output_cue_complete(updated_session)
                  halt(exit_pass)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn run_interview_cue_answer(session_id: String, answer_text: String) -> Nil {
  let jsonl_path = ".interview/sessions.jsonl"

  case interview_storage.get_session_from_jsonl(jsonl_path, session_id) {
    Error(err) -> {
      output_cue_error("Session not found: " <> err)
      halt(exit_error)
    }
    Ok(session) -> {
      // Basic validation - answer must be at least 3 characters
      case string.length(string.trim(answer_text)) < 3 {
        True -> {
          output_cue_validation_error(
            "Answer too short",
            "Please provide a more detailed response",
          )
          halt(exit_fail)
        }
        False -> {
          // Determine current round
          let round = case session.rounds_completed {
            0 -> 1
            r if r < 5 -> r + 1
            _ -> 5
          }

          // Find the current unanswered question
          let profile_str = profile_to_string(session.profile)
          let questions =
            interview_questions.get_questions_for_round(profile_str, round)
          let answered_ids = list.map(session.answers, fn(a) { a.question_id })

          case list.find(questions, fn(q) { !list.contains(answered_ids, q.id) }) {
            Error(_) -> {
              // No questions left, interview is complete
              output_cue_complete(session)
              halt(exit_pass)
            }
            Ok(question) -> {
              // Extract fields from answer
              let extracted =
                interview.extract_from_answer(
                  question.id,
                  answer_text,
                  question.extract_into,
                )

              // Calculate confidence
              let confidence =
                interview.calculate_confidence(
                  question.id,
                  answer_text,
                  extracted,
                )

              // Create answer record
              let answer =
                interview.Answer(
                  question_id: question.id,
                  question_text: question.question,
                  perspective: question.perspective,
                  round: round,
                  response: answer_text,
                  extracted: extracted,
                  confidence: confidence,
                  notes: "",
                  timestamp: current_timestamp(),
                )

              // Add to session
              let updated_session = interview.add_answer(session, answer)

              // Check for gaps and conflicts
              let #(sess_with_gaps, _) =
                interview.check_for_gaps(updated_session, question, answer)
              let #(sess_final, _) =
                interview.check_for_conflicts(sess_with_gaps, answer)

              // Save updated session
              case
                interview_storage.append_session_to_jsonl(
                  sess_final,
                  jsonl_path,
                )
              {
                Error(err) -> {
                  output_cue_error("Failed to save session: " <> err)
                  halt(exit_error)
                }
                Ok(_) -> {
                  // Get next question or complete
                  let next_unanswered =
                    list.find(questions, fn(q) {
                      !list.contains(
                        list.map(sess_final.answers, fn(a) { a.question_id }),
                        q.id,
                      )
                    })
                  case next_unanswered {
                    Ok(next_q) -> {
                      output_cue_question(sess_final, next_q, round)
                      halt(exit_pass)
                    }
                    Error(_) -> {
                      // Check if there are more rounds
                      case round < 5 {
                        True -> {
                          let next_round_questions =
                            interview_questions.get_questions_for_round(
                              profile_str,
                              round + 1,
                            )
                          let next_answered_ids =
                            list.map(sess_final.answers, fn(a) { a.question_id })
                          case
                            list.find(next_round_questions, fn(q) {
                              !list.contains(next_answered_ids, q.id)
                            })
                          {
                            Ok(next_q) -> {
                              output_cue_question(sess_final, next_q, round + 1)
                              halt(exit_pass)
                            }
                            Error(_) -> {
                              output_cue_complete(sess_final)
                              halt(exit_pass)
                            }
                          }
                        }
                        False -> {
                          output_cue_complete(sess_final)
                          halt(exit_pass)
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

  io.println(
    "interview: {
  status: \"in_progress\"
  session_id: \""
    <> session.id
    <> "\"
  profile: \""
    <> profile_str
    <> "\"
  round: "
    <> string.inspect(round)
    <> "
  rounds_total: 5
  answers_count: "
    <> string.inspect(list.length(session.answers))
    <> "

  question: {
    id: \""
    <> question.id
    <> "\"
    text: \""
    <> escape_cue_string(question.question)
    <> "\"
    context: \""
    <> escape_cue_string(question.context)
    <> "\"
    example: \""
    <> escape_cue_string(question.example)
    <> "\"
    perspective: \""
    <> perspective_to_string(question.perspective)
    <> "\"
    priority: "
    <> string.inspect(question.priority)
    <> "
    extract_into: ["
    <> format_cue_string_list(question.extract_into)
    <> "]
  }

  // To answer this question, run:
  // intent interview --cue --session "
    <> session.id
    <> " --answer \"YOUR ANSWER HERE\"
}",
  )
}

fn output_cue_validation_error(message: String, suggestion: String) -> Nil {
  io.println(
    "interview: {
  status: \"validation_error\"
  error: \""
    <> escape_cue_string(message)
    <> "\"
  suggestion: \""
    <> escape_cue_string(suggestion)
    <> "\"
}",
  )
}

fn output_cue_complete(session: interview.InterviewSession) -> Nil {
  let profile_str = profile_to_string(session.profile)

  // Generate spec from session
  let spec_cue = spec_builder.build_spec_from_session(session)

  io.println(
    "interview: {
  status: \"complete\"
  session_id: \""
    <> session.id
    <> "\"
  profile: \""
    <> profile_str
    <> "\"
  answers_count: "
    <> string.inspect(list.length(session.answers))
    <> "
  gaps_count: "
    <> string.inspect(list.length(session.gaps))
    <> "
  conflicts_count: "
    <> string.inspect(list.length(session.conflicts))
    <> "

  // Generated spec (save to file with your preferred name):
  generated_spec: '''
"
    <> spec_cue
    <> "
'''

  // Next steps:
  // 1. Save the spec: intent interview --cue --session "
    <> session.id
    <> " > my-api.cue
  // 2. Generate work items: intent beads "
    <> session.id
    <> "
}",
  )
}

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn format_cue_string_list(items: List(String)) -> String {
  items
  |> list.map(fn(s) { "\"" <> escape_cue_string(s) <> "\"" })
  |> string.join(", ")
}

// =============================================================================
// BEADS COMMANDS
// =============================================================================

/// The `beads` command - generate work items from interview session
pub fn beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [session_id, ..] -> {
        // Load session from JSONL
        case
          interview_storage.get_session_from_jsonl(
            ".interview/sessions.jsonl",
            session_id,
          )
        {
          Error(err) -> {
            io.println_error("Error: " <> err)
            halt(exit_error)
          }
          Ok(session) -> {
            // Generate beads from session
            let beads = bead_templates.generate_beads_from_session(session)
            let bead_count = list.length(beads)

            io.println("")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("                    BEAD GENERATION")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("")
            io.println(
              "Generated "
              <> string.inspect(bead_count)
              <> " work items from session: "
              <> session_id,
            )
            io.println("")

            // Export to .beads/issues.jsonl
            let jsonl_output = bead_templates.beads_to_jsonl(beads)

            case
              simplifile.append(".beads/issues.jsonl", jsonl_output <> "\n")
            {
              Ok(Nil) -> {
                io.println("✓ Beads exported to: .beads/issues.jsonl")
                io.println("")

                // Show stats
                let stats = bead_templates.bead_stats(beads)
                io.println("Summary:")
                io.println("  Total beads: " <> string.inspect(stats.total))

                halt(exit_pass)
              }
              Error(err) -> {
                io.println_error(
                  "✗ Failed to write beads: " <> string.inspect(err),
                )
                halt(exit_error)
              }
            }
          }
        }
      }
      [] -> {
        io.println_error("Usage: intent beads <session_id>")
        io.println_error("")
        io.println_error("Example: intent beads interview-abc123def456")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Generate work items (beads) from an interview session")
}

/// Mark a bead with execution status (success/failed/blocked)
pub fn bead_status_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let bead_id =
      flag.get_string(input.flags, "bead-id")
      |> result.unwrap("")

    let status =
      flag.get_string(input.flags, "status")
      |> result.unwrap("")

    let reason =
      flag.get_string(input.flags, "reason")
      |> result.unwrap("")

    let session_id =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    case string.is_empty(bead_id) {
      True -> {
        io.println_error(
          "Usage: intent bead-status --bead-id <id> --status success|failed|blocked [--reason 'text'] [--session <id>]",
        )
        halt(exit_error)
      }
      False -> {
        case status {
          "success" -> {
            case
              bead_feedback.mark_bead_executed(
                session_id,
                bead_id,
                bead_feedback.Success,
                reason,
                0,
              )
            {
              Ok(Nil) -> {
                io.println("✓ Bead " <> bead_id <> " marked as success")
                halt(exit_pass)
              }
              Error(err) -> {
                io.println_error(
                  "✗ Failed to mark bead: "
                  <> bead_feedback_error_to_string(err),
                )
                halt(exit_error)
              }
            }
          }
          "failed" -> {
            case
              bead_feedback.mark_bead_failed(
                session_id,
                bead_id,
                reason,
                "execution_error",
                "Bead execution failed",
                option.None,
                0,
              )
            {
              Ok(Nil) -> {
                io.println("✓ Bead " <> bead_id <> " marked as failed")
                halt(exit_pass)
              }
              Error(err) -> {
                io.println_error(
                  "✗ Failed to mark bead: "
                  <> bead_feedback_error_to_string(err),
                )
                halt(exit_error)
              }
            }
          }
          "blocked" -> {
            case string.is_empty(reason) {
              True -> {
                io.println_error("Error: --status blocked requires --reason")
                halt(exit_error)
              }
              False -> {
                case
                  bead_feedback.mark_bead_blocked(
                    session_id,
                    bead_id,
                    reason,
                    "user_action",
                    "User blocked this bead",
                    "Manual resume required",
                    0,
                  )
                {
                  Ok(Nil) -> {
                    io.println(
                      "✓ Bead " <> bead_id <> " marked as blocked: " <> reason,
                    )
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    io.println_error(
                      "✗ Failed to mark bead: "
                      <> bead_feedback_error_to_string(err),
                    )
                    halt(exit_error)
                  }
                }
              }
            }
          }
          _ -> {
            io.println_error("Error: invalid status '" <> status <> "'")
            io.println_error("Valid statuses: success, failed, blocked")
            halt(exit_error)
          }
        }
      }
    }
  })
  |> glint.description("Mark bead execution status (success/failed/blocked)")
  |> glint.flag(
    "bead-id",
    flag.string() |> flag.default("") |> flag.description("Bead ID (required)"),
  )
  |> glint.flag(
    "status",
    flag.string()
      |> flag.default("")
      |> flag.description("Status: success, failed, or blocked (required)"),
  )
  |> glint.flag(
    "reason",
    flag.string()
      |> flag.default("")
      |> flag.description("Reason for status (required for blocked)"),
  )
  |> glint.flag(
    "session",
    flag.string() |> flag.default("") |> flag.description("Session ID"),
  )
}

// =============================================================================
// HISTORY COMMANDS
// =============================================================================

/// The `history` command - show answer history for a session
pub fn history_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)
    let mode = output_mode.from_json_flag(is_json)

    case input.args {
      [session_id, ..] -> {
        case
          interview_storage.get_session_from_jsonl(
            ".interview/sessions.jsonl",
            session_id,
          )
        {
          Error(err) -> {
            io.println_error("Error: " <> err)
            halt(exit_error)
          }
          Ok(session) -> {
            case is_json {
              True -> {
                let json_answers =
                  json.array(session.answers, fn(answer) {
                    json.object([
                      #("question_id", json.string(answer.question_id)),
                      #("question_text", json.string(answer.question_text)),
                      #("response", json.string(answer.response)),
                      #("round", json.int(answer.round)),
                      #("confidence", json.float(answer.confidence)),
                      #("timestamp", json.string(answer.timestamp)),
                    ])
                  })
                io.println(json.to_string(json_answers))
              }
              False -> {
                cli_ui.print_header("Session History: " <> session.id, mode)
                io.println("")

                list.each(session.answers, fn(answer) {
                  io.println(
                    "Round " <> string.inspect(answer.round) <> " | " <> answer.question_id,
                  )
                  io.println("Q: " <> answer.question_text)
                  io.println("A: " <> answer.response)
                  io.println(
                    "   Confidence: "
                    <> string.inspect(answer.confidence)
                    <> " | "
                    <> answer.timestamp,
                  )
                  io.println("")
                })

                io.println(
                  "Total answers: " <> string.inspect(list.length(session.answers)),
                )
              }
            }
            halt(exit_pass)
          }
        }
      }
      [] -> {
        io.println_error("Usage: intent history <session_id>")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Show answer history for an interview session")
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
  )
}

/// The `diff` command - compare two interview sessions
pub fn diff_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let mode = output_mode.Interactive
    case input.args {
      [session_id1, session_id2, ..] -> {
        let jsonl_path = ".interview/sessions.jsonl"
        case
          interview_storage.get_session_from_jsonl(jsonl_path, session_id1),
          interview_storage.get_session_from_jsonl(jsonl_path, session_id2)
        {
          Ok(session1), Ok(session2) -> {
            cli_ui.print_header("Session Diff", mode)
            io.println("")
            io.println("Session 1: " <> session1.id)
            io.println("Session 2: " <> session2.id)
            io.println("")

            // Compare answers
            let answers1_ids =
              list.map(session1.answers, fn(a) { a.question_id })
            let answers2_ids =
              list.map(session2.answers, fn(a) { a.question_id })

            // Find unique to each
            let only_in_1 =
              list.filter(answers1_ids, fn(id) {
                !list.contains(answers2_ids, id)
              })
            let only_in_2 =
              list.filter(answers2_ids, fn(id) {
                !list.contains(answers1_ids, id)
              })

            io.println(
              "Only in session 1: " <> string.inspect(list.length(only_in_1)),
            )
            list.each(only_in_1, fn(id) { io.println("  - " <> id) })

            io.println(
              "Only in session 2: " <> string.inspect(list.length(only_in_2)),
            )
            list.each(only_in_2, fn(id) { io.println("  + " <> id) })

            // Find common but different
            let common_ids =
              list.filter(answers1_ids, fn(id) {
                list.contains(answers2_ids, id)
              })

            io.println("")
            io.println(
              "Common questions: " <> string.inspect(list.length(common_ids)),
            )

            halt(exit_pass)
          }
          Error(err), _ -> {
            io.println_error("Error loading session 1: " <> err)
            halt(exit_error)
          }
          _, Error(err) -> {
            io.println_error("Error loading session 2: " <> err)
            halt(exit_error)
          }
        }
      }
      _ -> {
        io.println_error("Usage: intent diff <session_id1> <session_id2>")
        halt(exit_error)
      }
    }
  })
  |> glint.description("Compare two interview sessions")
}

/// The `sessions` command - list all interview sessions
pub fn sessions_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let jsonl_path = ".interview/sessions.jsonl"

    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)
    let mode = output_mode.from_json_flag(is_json)

    let profile_filter =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("")

    case interview_storage.list_sessions_from_jsonl(jsonl_path) {
      Error(_) -> {
        // File doesn't exist yet - treat as empty
        cli_ui.print_warning("No interview sessions found", mode)
        io.println("")
        io.println("Start a new interview with:")
        io.println("  intent interview --profile api")
        halt(exit_pass)
      }
      Ok([]) -> {
        cli_ui.print_warning("No interview sessions found", mode)
        io.println("")
        io.println("Start a new interview with:")
        io.println("  intent interview --profile api")
        halt(exit_pass)
      }
      Ok(sessions) -> {
        // Filter by profile if specified
        let filtered = case profile_filter {
          "" -> sessions
          p ->
            list.filter(sessions, fn(s) {
              profile_to_string(s.profile) == string.lowercase(p)
            })
        }

        case is_json {
          True -> {
            let json_sessions =
              json.array(filtered, interview_storage.session_to_json)
            io.println(json.to_string(json_sessions))
          }
          False -> {
            cli_ui.print_header("Interview Sessions", mode)
            io.println("")

            list.each(filtered, fn(session) {
              let status_icon = case session.stage {
                interview.Complete -> "✓"
                interview.Paused -> "⏸"
                _ -> "●"
              }

              io.println(status_icon <> " " <> session.id)
              io.println(
                "  Profile: " <> profile_to_display_string(session.profile),
              )
              io.println("  Stage: " <> stage_to_display_string(session.stage))
              io.println(
                "  Rounds: " <> string.inspect(session.rounds_completed) <> "/5",
              )
              io.println(
                "  Answers: " <> string.inspect(list.length(session.answers)),
              )
              io.println("  Created: " <> session.created_at)
              io.println("  Updated: " <> session.updated_at)
              io.println("")
            })

            io.println(
              "Total: "
              <> string.inspect(list.length(filtered))
              <> " session(s)",
            )
          }
        }

        halt(exit_pass)
      }
    }
  })
  |> glint.description("List all interview sessions")
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
  )
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("")
      |> flag.description("Filter by profile (api, cli, event, etc.)"),
  )
}

fn stage_to_display_string(stage: interview.InterviewStage) -> String {
  case stage {
    interview.Discovery -> "Discovery"
    interview.Refinement -> "Refinement"
    interview.Validation -> "Validation"
    interview.Complete -> "Complete"
    interview.Paused -> "Paused"
  }
}

fn perspective_to_string(perspective: question_types.Perspective) -> String {
  case perspective {
    question_types.User -> "user"
    question_types.Developer -> "developer"
    question_types.Ops -> "ops"
    question_types.Security -> "security"
    question_types.Business -> "business"
  }
}

// =============================================================================
// ERROR FORMATTING
// =============================================================================

fn answer_loader_error_to_string(err: answer_loader.AnswerLoaderError) -> String {
  case err {
    answer_loader.FileNotFound(path) -> "File not found: " <> path
    answer_loader.PermissionDenied(path) ->
      "Permission denied reading: " <> path
    answer_loader.ParseError(path, msg) ->
      "Parse error in " <> path <> ": " <> msg
    answer_loader.SchemaError(msg) -> "Schema validation failed: " <> msg
    answer_loader.IoError(msg) -> "I/O error: " <> msg
  }
}

fn bead_feedback_error_to_string(err: bead_feedback.FeedbackError) -> String {
  case err {
    bead_feedback.SessionNotFound(id) -> "Session not found: " <> id
    bead_feedback.WriteError(path, msg) ->
      "Write error at " <> path <> ": " <> msg
    bead_feedback.ValidationError(msg) -> "Validation error: " <> msg
  }
}
