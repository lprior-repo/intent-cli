/// Spec Session Management
/// State management for Spec phase (Phase 3 of INTENT_4_PLAN.md)
/// Follows Functional Core pattern - all functions are pure
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import intent/spec_critique.{type CritiqueResult, critique_spec}
import intent/types.{type Spec, Spec}

/// Spec session status
pub type SpecStatus {
  InProgress
  ReadyForCritique
  Complete
}

/// A single answer in a spec session
pub type SpecAnswer {
  SpecAnswer(
    question_id: String,
    response: String,
    extracted: Dict(String, String),
    round: Int,
    timestamp: String,
  )
}

/// A round in the 5-round KIRK spec writing process
pub type SpecRound {
  SpecRound(
    round_number: Int,
    model_type: String,
    answers: List(SpecAnswer),
    spec_output: Option(Spec),
    rcs_score: Float,
    timestamp: String,
  )
}

/// A P0 blocking question that must be answered before proceeding
pub type BlockingQuestion {
  BlockingQuestion(
    id: String,
    question: String,
    category: String,
    resolved: Bool,
  )
}

/// Spec session - persistent state for spec phase
pub type SpecSession {
  SpecSession(
    id: String,
    vision_session_id: Option(String),
    shape_session_id: Option(String),
    created_at: String,
    updated_at: String,
    status: SpecStatus,
    rounds: List(SpecRound),
    blocking_questions: List(BlockingQuestion),
    current_round: Int,
  )
}

/// Create a new spec session
pub fn create_session(
  id: String,
  vision_session_id: Option(String),
  shape_session_id: Option(String),
  created_at: String,
) -> SpecSession {
  SpecSession(
    id: id,
    vision_session_id: vision_session_id,
    shape_session_id: shape_session_id,
    created_at: created_at,
    updated_at: created_at,
    status: InProgress,
    rounds: [],
    blocking_questions: [],
    current_round: 1,
  )
}

/// Get status as string for JSON output
pub fn get_status_string(status: SpecStatus) -> String {
  case status {
    InProgress -> "in_progress"
    ReadyForCritique -> "ready_for_critique"
    Complete -> "complete"
  }
}

/// Record an answer in the session
pub fn record_answer(
  session: SpecSession,
  question_id: String,
  response: String,
  extracted: Dict(String, String),
  round: Int,
  timestamp: String,
) -> SpecSession {
  let answer =
    SpecAnswer(
      question_id: question_id,
      response: response,
      extracted: extracted,
      round: round,
      timestamp: timestamp,
    )

  SpecSession(
    ..session,
    rounds: list.append(session.rounds, [
      SpecRound(
        round_number: round,
        model_type: "",
        answers: [answer],
        spec_output: None,
        rcs_score: 0.0,
        timestamp: timestamp,
      ),
    ]),
    updated_at: timestamp,
  )
}

/// Record a complete round with spec output and RCS score
pub fn record_round(
  session: SpecSession,
  round_number: Int,
  model_type: String,
  spec: Spec,
  rcs_score: Float,
  timestamp: String,
) -> SpecSession {
  let round =
    SpecRound(
      round_number: round_number,
      model_type: model_type,
      answers: [],
      spec_output: Some(spec),
      rcs_score: rcs_score,
      timestamp: timestamp,
    )

  SpecSession(
    ..session,
    rounds: list.append(session.rounds, [round]),
    updated_at: timestamp,
    current_round: round_number + 1,
  )
}

/// Find an answer by question ID
pub fn find_answer(
  session: SpecSession,
  question_id: String,
) -> Option(SpecAnswer) {
  session.rounds
  |> list.flat_map(fn(r) { r.answers })
  |> list.find(fn(a) { a.question_id == question_id })
  |> option.from_result
}

/// Get count of answered questions
pub fn get_answers_count(session: SpecSession) -> Int {
  session.rounds
  |> list.flat_map(fn(r) { r.answers })
  |> list.length
}

/// Add a blocking question
pub fn add_blocking_question(
  session: SpecSession,
  id: String,
  question: String,
  category: String,
) -> SpecSession {
  let blocking =
    BlockingQuestion(
      id: id,
      question: question,
      category: category,
      resolved: False,
    )

  SpecSession(
    ..session,
    blocking_questions: list.append(session.blocking_questions, [blocking]),
  )
}

/// Resolve a blocking question
pub fn resolve_blocking_question(
  session: SpecSession,
  question_id: String,
) -> SpecSession {
  let blocking =
    session.blocking_questions
    |> list.map(fn(q) {
      case q.id == question_id {
        True -> BlockingQuestion(..q, resolved: True)
        False -> q
      }
    })

  SpecSession(..session, blocking_questions: blocking)
}

/// Check if all blocking questions are resolved
pub fn all_blocking_resolved(session: SpecSession) -> Bool {
  session.blocking_questions
  |> list.all(fn(q) { q.resolved })
}

/// Check if session can advance (all 5 rounds complete, all P0 resolved)
pub fn can_advance(session: SpecSession) -> Bool {
  let all_rounds_complete = session.current_round > 5
  let p0s_resolved = all_blocking_resolved(session)
  all_rounds_complete && p0s_resolved
}

/// Set session status to ready for critique
pub fn set_status_ready_for_critique(
  session: SpecSession,
  timestamp: String,
) -> SpecSession {
  SpecSession(..session, status: ReadyForCritique, updated_at: timestamp)
}

/// Set session status to complete
pub fn set_status_complete(
  session: SpecSession,
  rcs_score: Float,
  timestamp: String,
) -> SpecSession {
  SpecSession(..session, status: Complete, updated_at: timestamp)
}

/// Get current RCS score (average of all rounds)
pub fn get_rcs_score(session: SpecSession) -> Float {
  let scores =
    session.rounds
    |> list.map(fn(r) { r.rcs_score })

  case list.length(scores) {
    0 -> 0.0
    n -> {
      let sum =
        scores
        |> list.fold(0.0, fn(acc, score) { acc +. score })

      sum /. int.to_float(n)
    }
  }
}
