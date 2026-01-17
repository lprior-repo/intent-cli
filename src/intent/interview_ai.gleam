/// AI-Friendly Interview Workflow
/// Provides JSONL streaming with stdin for AI agents
/// Maintains FULL interview rigor (all 5 rounds, all questions)
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import intent/interview
import intent/interview_questions
import intent/interview_storage
import intent/question_types.{type Question}
import intent/spec_builder
import intent/stdin
import intent/streaming_protocol
import simplifile

// =============================================================================
// PUBLIC API
// =============================================================================

/// Start AI mode interview - auto-creates session, streams questions via JSONL
pub fn run_ai_interview(
  profile: interview.Profile,
  generate_uuid_fn: fn() -> String,
  current_timestamp_fn: fn() -> String,
) -> Nil {
  // Create session with auto-generated ID
  let session_id = "interview-" <> generate_uuid_fn()
  let timestamp = current_timestamp_fn()
  let session = interview.create_session(session_id, profile, timestamp)

  // Save session
  case
    interview_storage.append_session_to_jsonl(
      session,
      ".interview/sessions.jsonl",
    )
  {
    Error(err) -> {
      output_error("Failed to save session: " <> err)
      Nil
    }
    Ok(_) -> {
      // Start interview loop
      interview_loop(session, 1, current_timestamp_fn)
    }
  }
}

// =============================================================================
// INTERVIEW LOOP
// =============================================================================

/// Main interview loop - ask questions, read answers from stdin
fn interview_loop(
  session: interview.InterviewSession,
  round: Int,
  current_timestamp_fn: fn() -> String,
) -> Nil {
  case round > 5 {
    True -> {
      // Interview complete
      complete_interview(session)
    }
    False -> {
      // Get next unanswered question
      case find_next_question(session, round) {
        None -> {
          // No more questions in this round, move to next
          interview_loop(session, round + 1, current_timestamp_fn)
        }
        Some(question) -> {
          // Output question in JSONL format
          output_question(session, question)

          // Read answer from stdin
          case stdin.read_line_trimmed() {
            Error(err) -> {
              output_error("Failed to read answer: " <> err)
            }
            Ok(answer_text) -> {
              // Validate and process answer
              case validate_answer(answer_text) {
                Error(msg) -> {
                  output_validation_error(msg)
                }
                Ok(validated) -> {
                  process_answer(
                    session,
                    question,
                    validated,
                    round,
                    current_timestamp_fn,
                  )
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Process a validated answer
fn process_answer(
  session: interview.InterviewSession,
  question: Question,
  answer_text: String,
  round: Int,
  current_timestamp_fn: fn() -> String,
) -> Nil {
  // Extract and calculate confidence
  let extracted =
    interview.extract_from_answer(question.id, answer_text, question.extract_into)
  let confidence =
    interview.calculate_confidence(question.id, answer_text, extracted)

  // Create answer record
  let answer =
    interview.Answer(
      question_id: question.id,
      response: answer_text,
      extracted: extracted,
      confidence: confidence,
      answered_at: current_timestamp_fn(),
    )

  // Update session with gaps and conflicts checking
  let updated_session = interview.add_answer(session, answer)
  let sess_with_gaps =
    interview.check_for_gaps(updated_session, question, answer)
  let final_session = interview.check_for_conflicts(sess_with_gaps, answer)

  // Save updated session
  case
    interview_storage.append_session_to_jsonl(
      final_session,
      ".interview/sessions.jsonl",
    )
  {
    Error(err) -> {
      output_error("Failed to save session: " <> err)
    }
    Ok(_) -> {
      // Continue with next question
      interview_loop(final_session, round, current_timestamp_fn)
    }
  }
}

// =============================================================================
// QUESTION FINDING
// =============================================================================

/// Find next unanswered question starting from a given round
fn find_next_question(
  session: interview.InterviewSession,
  start_round: Int,
) -> option.Option(Question) {
  let profile_str = interview.profile_to_string(session.profile)
  let answered_ids = list.map(session.answers, fn(a) { a.question_id })

  find_in_rounds(profile_str, answered_ids, start_round)
}

/// Search through rounds for an unanswered question
fn find_in_rounds(
  profile: String,
  answered: List(String),
  round: Int,
) -> option.Option(Question) {
  case round > 5 {
    True -> None
    False -> {
      let questions = interview_questions.get_questions_for_round(profile, round)
      case find_unanswered_in_list(questions, answered) {
        Some(q) -> Some(q)
        None -> find_in_rounds(profile, answered, round + 1)
      }
    }
  }
}

/// Find first unanswered question in a list
fn find_unanswered_in_list(
  questions: List(Question),
  answered: List(String),
) -> option.Option(Question) {
  list.find(questions, fn(q) { !list.contains(answered, q.id) })
  |> result.to_option
}

// =============================================================================
// VALIDATION
// =============================================================================

/// Validate answer text
fn validate_answer(answer: String) -> Result(String, String) {
  let trimmed = string.trim(answer)
  case string.length(trimmed) {
    0 -> Error("Answer cannot be empty")
    len if len < 3 -> Error("Answer too short - please provide more detail")
    _ -> Ok(trimmed)
  }
}

// =============================================================================
// JSONL OUTPUT
// =============================================================================

/// Output question in JSONL format
fn output_question(
  session: interview.InterviewSession,
  question: Question,
) -> Nil {
  let progress = streaming_protocol.calculate_progress(session, session.profile)
  let context = streaming_protocol.build_question_context(question)

  let question_json =
    json.object([
      #("action", json.string("ask_question")),
      #(
        "question",
        json.object([
          #("id", json.string(question.id)),
          #("text", json.string(question.question)),
          #("round", json.int(question.round)),
          #(
            "priority",
            json.string(priority_to_string(question.priority)),
          ),
          #(
            "perspective",
            json.string(perspective_to_string(question.perspective)),
          ),
          #(
            "category",
            json.string(streaming_protocol.category_to_string(
              question.category,
            )),
          ),
        ]),
      ),
      #(
        "ears",
        json.object([
          #("pattern", json.string(context.ears_info.pattern)),
          #("hint", json.string(context.ears_info.hint)),
          #(
            "examples",
            json.array(context.ears_info.examples, json.string),
          ),
          #("template", json.string(context.ears_info.template)),
        ]),
      ),
      #(
        "round_info",
        json.object([
          #("round", json.int(context.round_info.round)),
          #("name", json.string(context.round_info.round_name)),
          #("description", json.string(context.round_info.round_description)),
          #("focus", json.string(context.round_info.round_focus)),
        ]),
      ),
      #(
        "progress",
        json.object([
          #("current_step", json.int(progress.current_step)),
          #("total_steps", json.int(progress.total_steps)),
          #("percent_complete", json.int(progress.percent_complete)),
          #("round", json.int(progress.round)),
          #("round_name", json.string(progress.round_name)),
          #("rounds_completed", json.int(progress.rounds_completed)),
          #("total_rounds", json.int(progress.total_rounds)),
        ]),
      ),
      #(
        "session",
        json.object([
          #("id", json.string(session.id)),
          #(
            "profile",
            json.string(interview.profile_to_string(session.profile)),
          ),
          #("started_at", json.string(session.created_at)),
        ]),
      ),
    ])

  io.println(json.to_string(question_json))
}

/// Output interview complete message
fn complete_interview(session: interview.InterviewSession) -> Nil {
  let spec_path = ".interview/spec-" <> session.id <> ".cue"

  // Generate and save spec
  let spec_cue = spec_builder.build_spec_from_session(session)
  let _ = simplifile.write(spec_path, spec_cue)

  let complete_json =
    json.object([
      #("action", json.string("interview_complete")),
      #(
        "output",
        json.object([
          #("spec_path", json.string(spec_path)),
          #("behaviors_count", json.int(list.length(session.answers))),
          #("gaps_count", json.int(list.length(session.gaps))),
          #("conflicts_count", json.int(list.length(session.conflicts))),
          #(
            "summary",
            json.string(
              "Interview complete. Generated "
              <> int.to_string(list.length(session.answers))
              <> " behaviors.",
            ),
          ),
        ]),
      ),
      #(
        "session",
        json.object([
          #("id", json.string(session.id)),
          #(
            "profile",
            json.string(interview.profile_to_string(session.profile)),
          ),
          #("started_at", json.string(session.created_at)),
        ]),
      ),
    ])

  io.println(json.to_string(complete_json))
}

/// Output error message
fn output_error(message: String) -> Nil {
  let error_json =
    json.object([
      #("action", json.string("error")),
      #(
        "error",
        json.object([
          #("message", json.string(message)),
          #("retry_allowed", json.bool(False)),
        ]),
      ),
    ])

  io.println(json.to_string(error_json))
}

/// Output validation error
fn output_validation_error(message: String) -> Nil {
  let error_json =
    json.object([
      #("action", json.string("validation_error")),
      #(
        "error",
        json.object([
          #("message", json.string(message)),
          #("suggestion", json.string("Provide a more detailed answer")),
          #("retry_allowed", json.bool(True)),
        ]),
      ),
    ])

  io.println(json.to_string(error_json))
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

fn perspective_to_string(perspective: question_types.Perspective) -> String {
  case perspective {
    question_types.User -> "user"
    question_types.Developer -> "developer"
    question_types.Ops -> "ops"
    question_types.Security -> "security"
    question_types.Business -> "business"
  }
}

fn priority_to_string(priority: question_types.QuestionPriority) -> String {
  case priority {
    question_types.Critical -> "critical"
    question_types.Important -> "important"
    question_types.NiceTohave -> "nice_to_have"
  }
}

// Required imports at end to satisfy compiler
import gleam/io
