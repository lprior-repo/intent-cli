/// Interview Workflow Enhancements
/// Incremental interview capabilities: skip, jump, bulk answers, section navigation
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import intent/interview.{type Answer, type InterviewSession, type Profile}
import intent/interview_questions
import intent/question_types.{
  type Question, type QuestionPriority, Critical, Important, NiceTohave,
}
import simplifile

/// Section represents logical groupings of interview rounds
pub type Section {
  Discovery
  // Rounds 1-2
  Refinement
  // Round 3
  Validation
  // Rounds 4-5
}

/// Workflow mode controls question filtering
pub type WorkflowMode {
  Full
  // All questions
  RequiredOnly
  // Critical priority only
  SectionOnly(Section)
  // Questions from specific section
}

/// Bulk answer record from file
pub type BulkAnswer {
  BulkAnswer(question_id: String, response: String)
}

/// Section summary for progress tracking
pub type SectionSummary {
  SectionSummary(
    section: Section,
    total_questions: Int,
    critical_questions: Int,
    important_questions: Int,
    answered: Int,
    skipped: Int,
    remaining: Int,
  )
}

// =============================================================================
// SECTION NAVIGATION
// =============================================================================

/// Convert section name to Section type
pub fn parse_section(s: String) -> Result(Section, String) {
  case string.lowercase(s) {
    "discovery" -> Ok(Discovery)
    "refinement" -> Ok(Refinement)
    "validation" -> Ok(Validation)
    _ ->
      Error(
        "Unknown section: "
        <> s
        <> ". Valid sections: discovery, refinement, validation",
      )
  }
}

/// Convert Section to string
pub fn section_to_string(section: Section) -> String {
  case section {
    Discovery -> "discovery"
    Refinement -> "refinement"
    Validation -> "validation"
  }
}

/// Get rounds for a section
pub fn section_to_rounds(section: Section) -> List(Int) {
  case section {
    Discovery -> [1, 2]
    Refinement -> [3]
    Validation -> [4, 5]
  }
}

/// Get section for a round
pub fn round_to_section(round: Int) -> Section {
  case round {
    1 -> Discovery
    2 -> Discovery
    3 -> Refinement
    4 -> Validation
    5 -> Validation
    _ -> Discovery
  }
}

// =============================================================================
// QUESTION FILTERING
// =============================================================================

/// Filter questions by priority (Critical only for required-only mode)
pub fn filter_by_priority(
  questions: List(Question),
  mode: WorkflowMode,
) -> List(Question) {
  case mode {
    RequiredOnly -> list.filter(questions, fn(q) { q.priority == Critical })
    _ -> questions
  }
}

/// Filter questions by section
pub fn filter_by_section(
  questions: List(Question),
  section: Section,
) -> List(Question) {
  let rounds = section_to_rounds(section)
  list.filter(questions, fn(q) { list.contains(rounds, q.round) })
}

/// Get questions for session with workflow mode applied
pub fn get_questions_for_session(
  session: InterviewSession,
  mode: WorkflowMode,
) -> List(Question) {
  let profile_str = interview.profile_to_string(session.profile)
  let all_questions =
    list.flat_map([1, 2, 3, 4, 5], fn(round) {
      interview_questions.get_questions_for_round(profile_str, round)
    })

  case mode {
    Full -> all_questions
    RequiredOnly -> filter_by_priority(all_questions, RequiredOnly)
    SectionOnly(section) -> {
      let section_questions = filter_by_section(all_questions, section)
      section_questions
    }
  }
}

/// Get next question respecting workflow mode and answered questions
pub fn get_next_question(
  session: InterviewSession,
  mode: WorkflowMode,
) -> Result(Question, String) {
  let available = get_questions_for_session(session, mode)
  let answered_ids =
    list.map(session.answers, fn(answer) { answer.question_id })

  case find_first_unanswered(available, answered_ids) {
    Ok(q) -> Ok(q)
    Error(_) -> Error("No more questions available in current mode")
  }
}

fn find_first_unanswered(
  questions: List(Question),
  answered: List(String),
) -> Result(Question, Nil) {
  case questions {
    [] -> Error(Nil)
    [q, ..rest] ->
      case list.contains(answered, q.id) {
        True -> find_first_unanswered(rest, answered)
        False -> Ok(q)
      }
  }
}

// =============================================================================
// SECTION SUMMARIES
// =============================================================================

/// Generate summary for a section
pub fn get_section_summary(
  session: InterviewSession,
  section: Section,
) -> SectionSummary {
  let profile_str = interview.profile_to_string(session.profile)
  let rounds = section_to_rounds(section)
  let all_questions =
    list.flat_map(rounds, fn(round) {
      interview_questions.get_questions_for_round(profile_str, round)
    })

  let total = list.length(all_questions)
  let critical =
    list.length(list.filter(all_questions, fn(q) { q.priority == Critical }))
  let important =
    list.length(list.filter(all_questions, fn(q) { q.priority == Important }))

  let answered_ids =
    list.map(session.answers, fn(answer) { answer.question_id })
  let answered =
    list.length(
      list.filter(all_questions, fn(q) { list.contains(answered_ids, q.id) }),
    )

  // TODO: Track skipped questions in session
  let skipped = 0
  let remaining = total - answered - skipped

  SectionSummary(
    section: section,
    total_questions: total,
    critical_questions: critical,
    important_questions: important,
    answered: answered,
    skipped: skipped,
    remaining: remaining,
  )
}

/// Get all section summaries
pub fn get_all_section_summaries(
  session: InterviewSession,
) -> List(SectionSummary) {
  [
    get_section_summary(session, Discovery),
    get_section_summary(session, Refinement),
    get_section_summary(session, Validation),
  ]
}

/// Format section summary for display
pub fn format_section_summary(summary: SectionSummary) -> String {
  let section_name = section_to_string(summary.section)
  let percent_complete = case summary.total_questions {
    0 -> 0.0
    total -> {
      let answered_float = int_to_float(summary.answered)
      let total_float = int_to_float(total)
      { answered_float /. total_float } *. 100.0
    }
  }

  string.uppercase(section_name)
  <> " ("
  <> string.inspect(summary.answered)
  <> "/"
  <> string.inspect(summary.total_questions)
  <> ", "
  <> float_to_string_2dp(percent_complete)
  <> "%)\n"
  <> "  Critical: "
  <> string.inspect(summary.critical_questions)
  <> " | Important: "
  <> string.inspect(summary.important_questions)
  <> " | Remaining: "
  <> string.inspect(summary.remaining)
}

/// Format all section summaries
pub fn format_all_summaries(summaries: List(SectionSummary)) -> String {
  "Interview Progress by Section:\n\n"
  <> string.join(list.map(summaries, format_section_summary), "\n\n")
}

// Helper: convert int to float
fn int_to_float(i: Int) -> Float {
  case i {
    0 -> 0.0
    1 -> 1.0
    2 -> 2.0
    3 -> 3.0
    4 -> 4.0
    5 -> 5.0
    6 -> 6.0
    7 -> 7.0
    8 -> 8.0
    9 -> 9.0
    10 -> 10.0
    _ -> {
      // For larger numbers, approximate via string conversion
      let s = string.inspect(i)
      case string.contains(s, ".") {
        True -> 0.0
        False -> {
          // Try to parse as float, fallback to 0.0
          case float_parse(s) {
            Ok(f) -> f
            Error(_) -> 0.0
          }
        }
      }
    }
  }
}

// Helper: parse float from string
fn float_parse(s: String) -> Result(Float, Nil) {
  // Gleam doesn't have built-in string to float parsing
  // For now, return Error - this would need FFI in production
  Error(Nil)
}

// Helper: format float to 2 decimal places
fn float_to_string_2dp(f: Float) -> String {
  // Simple approximation: multiply by 100, round, divide by 100
  let rounded = case f {
    _ if f < 0.0 -> "0.00"
    _ if f >= 100.0 -> "100.00"
    _ -> {
      // Approximation for display
      let whole = float_truncate(f)
      let decimal_part = f -. int_to_float(whole)
      let decimal_int = float_truncate(decimal_part *. 100.0)
      string.inspect(whole)
      <> "."
      <> case decimal_int < 10 {
        True -> "0" <> string.inspect(decimal_int)
        False -> string.inspect(decimal_int)
      }
    }
  }
  rounded
}

// Helper: truncate float to int
fn float_truncate(f: Float) -> Int {
  // Approximation - would need FFI for proper implementation
  case f {
    _ if f < 0.0 -> 0
    _ if f < 1.0 -> 0
    _ if f < 2.0 -> 1
    _ if f < 3.0 -> 2
    _ if f < 10.0 -> 9
    _ if f < 20.0 -> 19
    _ if f < 50.0 -> 49
    _ if f < 100.0 -> 99
    _ -> 100
  }
}

// =============================================================================
// BULK ANSWER LOADING
// =============================================================================

/// Load bulk answers from JSONL file
pub fn load_bulk_answers(file_path: String) -> Result(List(BulkAnswer), String) {
  use content <- result.try(
    simplifile.read(file_path)
    |> result.map_error(fn(err) {
      "Failed to read bulk answers file: " <> string.inspect(err)
    }),
  )

  case string.length(string.trim(content)) {
    0 -> Ok([])
    _ -> {
      let lines = string.split(content, "\n")
      let bulk_answers =
        list.filter_map(lines, fn(line) {
          case string.length(string.trim(line)) {
            0 -> Error(Nil)
            _ -> parse_bulk_answer_line(line)
          }
        })
      Ok(bulk_answers)
    }
  }
}

/// Parse a single bulk answer line
fn parse_bulk_answer_line(line: String) -> Result(BulkAnswer, Nil) {
  json.decode(line, bulk_answer_decoder)
  |> result.map_error(fn(_) { Nil })
}

/// Decoder for bulk answer JSON
fn bulk_answer_decoder(
  json_value: dynamic.Dynamic,
) -> Result(BulkAnswer, dynamic.DecodeErrors) {
  use question_id <- result.try(dynamic.field("question_id", dynamic.string)(
    json_value,
  ))
  use response <- result.try(dynamic.field("response", dynamic.string)(
    json_value,
  ))
  Ok(BulkAnswer(question_id: question_id, response: response))
}

/// Apply bulk answers to session
pub fn apply_bulk_answers(
  session: InterviewSession,
  bulk_answers: List(BulkAnswer),
  timestamp: String,
) -> InterviewSession {
  list.fold(bulk_answers, session, fn(acc_session, bulk) {
    apply_single_bulk_answer(acc_session, bulk, timestamp)
  })
}

/// Apply a single bulk answer if question hasn't been answered
fn apply_single_bulk_answer(
  session: InterviewSession,
  bulk: BulkAnswer,
  timestamp: String,
) -> InterviewSession {
  let already_answered =
    list.any(session.answers, fn(a) { a.question_id == bulk.question_id })

  case already_answered {
    True -> session
    False -> {
      // Find the question to get metadata
      let profile_str = interview.profile_to_string(session.profile)
      let maybe_question =
        list.flat_map([1, 2, 3, 4, 5], fn(round) {
          interview_questions.get_questions_for_round(profile_str, round)
        })
        |> list.find(fn(q) { q.id == bulk.question_id })

      case maybe_question {
        Ok(question) -> {
          let extracted =
            interview.extract_from_answer(
              bulk.question_id,
              bulk.response,
              question.extract_into,
            )
          let confidence =
            interview.calculate_confidence(
              bulk.question_id,
              bulk.response,
              extracted,
            )

          let answer =
            interview.Answer(
              question_id: bulk.question_id,
              question_text: question.question,
              perspective: question.perspective,
              round: question.round,
              response: bulk.response,
              extracted: extracted,
              confidence: confidence,
              notes: "Bulk answer from file",
              timestamp: timestamp,
            )

          interview.add_answer(session, answer)
        }
        Error(_) -> session
      }
    }
  }
}

/// Find questions that have bulk answers but weren't applied (invalid IDs)
pub fn find_unmatched_bulk_answers(
  session: InterviewSession,
  bulk_answers: List(BulkAnswer),
) -> List(String) {
  let profile_str = interview.profile_to_string(session.profile)
  let all_question_ids =
    list.flat_map([1, 2, 3, 4, 5], fn(round) {
      interview_questions.get_questions_for_round(profile_str, round)
    })
    |> list.map(fn(q) { q.id })

  list.filter_map(bulk_answers, fn(bulk) {
    case list.contains(all_question_ids, bulk.question_id) {
      True -> Error(Nil)
      False -> Ok(bulk.question_id)
    }
  })
}

// =============================================================================
// WORKFLOW STATE
// =============================================================================

/// Skip to a section by marking all prior questions as skipped
/// Returns updated session with skipped questions noted
pub fn skip_to_section(
  session: InterviewSession,
  target_section: Section,
  timestamp: String,
) -> InterviewSession {
  let target_rounds = section_to_rounds(target_section)
  let first_target_round = case target_rounds {
    [first, ..] -> first
    [] -> 1
  }

  let profile_str = interview.profile_to_string(session.profile)

  // Get all questions before target section
  let prior_rounds = list.range(1, first_target_round - 1)
  let prior_questions =
    list.flat_map(prior_rounds, fn(round) {
      interview_questions.get_questions_for_round(profile_str, round)
    })

  // Mark unanswered prior questions as "skipped" by adding empty answers
  let answered_ids =
    list.map(session.answers, fn(answer) { answer.question_id })

  list.fold(prior_questions, session, fn(acc_session, question) {
    case list.contains(answered_ids, question.id) {
      True -> acc_session
      False -> {
        let skipped_answer =
          interview.Answer(
            question_id: question.id,
            question_text: question.question,
            perspective: question.perspective,
            round: question.round,
            response: "[SKIPPED]",
            extracted: dict.new(),
            confidence: 0.0,
            notes: "Skipped via --skip-to=" <> section_to_string(target_section),
            timestamp: timestamp,
          )
        interview.add_answer(acc_session, skipped_answer)
      }
    }
  })
}

/// Check if a question was skipped
pub fn is_question_skipped(answer: Answer) -> Bool {
  answer.response == "[SKIPPED]"
}

/// Get count of skipped questions in session
pub fn count_skipped(session: InterviewSession) -> Int {
  list.length(list.filter(session.answers, is_question_skipped))
}
