/// Interview Session Controller
/// Orchestrates multi-round interview flow with gap detection and conflict resolution

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/string
import intent/interview.{
  type Answer, type ConflictResolution, type Conflict, type Gap,
  type InterviewSession, type InterviewStage, type Profile,
}
import intent/interview_questions.{
  type Question, type Perspective, type QuestionCategory, type QuestionPriority,
}

/// Initialize a new interview session
pub fn start_interview(profile: Profile, session_id: String, timestamp: String) -> InterviewSession {
  interview.InterviewSession(
    id: session_id,
    profile: profile,
    created_at: timestamp,
    updated_at: timestamp,
    completed_at: "",
    stage: interview.Discovery,
    rounds_completed: 0,
    answers: [],
    gaps: [],
    conflicts: [],
    raw_notes: "",
  )
}

/// Get first question for a given round
pub fn get_first_question_for_round(
  session: InterviewSession,
  round: Int,
) -> Result(Question, String) {
  let profile_str = profile_to_string(session.profile)
  let questions = interview_questions.get_questions_for_round(profile_str, round)

  case questions {
    [] -> Error("No questions found for round " <> string.inspect(round))
    [first, ..] -> Ok(first)
  }
}

/// Get next unanswered question in current round
pub fn get_next_question_in_round(
  session: InterviewSession,
  round: Int,
) -> Result(Question, String) {
  let profile_str = profile_to_string(session.profile)
  let answered_ids = get_answered_question_ids(session)

  case interview_questions.get_next_question(profile_str, round, answered_ids) {
    option.Some(question) -> Ok(question)
    option.None -> Error("No more unanswered questions in round " <> string.inspect(round))
  }
}

/// Add an answer to the session
pub fn add_answer(session: InterviewSession, answer: Answer) -> InterviewSession {
  interview.InterviewSession(
    ..session,
    answers: list.append(session.answers, [answer]),
    updated_at: answer.timestamp,
  )
}

/// Get all answered question IDs
fn get_answered_question_ids(session: InterviewSession) -> List(String) {
  list.map(session.answers, fn(answer) { answer.question_id })
}

/// Check for gaps after answering a question
pub fn check_for_gaps(
  session: InterviewSession,
  question: Question,
  answer: Answer,
) -> #(InterviewSession, List(Gap)) {
  let blocking_gaps = detect_blocking_gaps(question, answer)
  let updated_session = interview.InterviewSession(
    ..session,
    gaps: list.append(session.gaps, blocking_gaps),
  )
  #(updated_session, blocking_gaps)
}

/// Detect blocking gaps in the answer
fn detect_blocking_gaps(question: Question, answer: Answer) -> List(Gap) {
  // Check if answer is too vague/short for critical questions
  let response_length = string.length(string.trim(answer.response))

  case question.priority {
    interview_questions.Critical if response_length < 10 -> [
      interview.Gap(
        id: "gap_" <> question.id,
        field: question.question,
        description: "Critical question answered too briefly",
        blocking: True,
        suggested_default: "Please provide a more detailed answer",
        why_needed: "This is a critical requirement for spec generation",
        round: answer.round,
        resolved: False,
        resolution: "",
      ),
    ]
    _ -> []
  }
}

/// Check for conflicts between answers
pub fn check_for_conflicts(
  session: InterviewSession,
  new_answer: Answer,
) -> #(InterviewSession, List(Conflict)) {
  let conflicts = detect_conflicts_in_session(session, new_answer)
  let updated_session = interview.InterviewSession(
    ..session,
    conflicts: list.append(session.conflicts, conflicts),
  )
  #(updated_session, conflicts)
}

/// Detect conflicting requirements
fn detect_conflicts_in_session(
  session: InterviewSession,
  new_answer: Answer,
) -> List(Conflict) {
  let conflicts_found = list.fold(session.answers, [], fn(acc, existing) {
    let new_conflicts = detect_answer_pair_conflicts(existing, new_answer)
    list.append(acc, new_conflicts)
  })
  conflicts_found
}

/// Detect conflicts between two answers
fn detect_answer_pair_conflicts(answer1: Answer, answer2: Answer) -> List(Conflict) {
  // Check for CAP theorem conflicts
  case #(answer1.perspective, answer2.perspective) {
    #(interview_questions.Developer, interview_questions.Ops) -> {
      let ans1_lower = string.lowercase(answer1.response)
      let ans2_lower = string.lowercase(answer2.response)
      case string.contains(ans1_lower, "consistency")
        && string.contains(ans2_lower, "high latency")
      {
        True -> [
          interview.Conflict(
            id: "cap_conflict_" <> answer1.question_id <> "_" <> answer2.question_id,
            between: #(answer1.question_id, answer2.question_id),
            description: "Consistency vs. Availability tension (CAP theorem)",
            impact: "Requires architectural decision on data replication strategy",
            options: [
              interview.ConflictResolution(
                option: "Prioritize Consistency",
                description: "Strong consistency with potential latency",
                tradeoffs: "Higher latency, complex distributed coordination",
                recommendation: "Use when data correctness is critical (financial, medical)",
              ),
              interview.ConflictResolution(
                option: "Prioritize Availability",
                description: "High availability with eventual consistency",
                tradeoffs: "Temporary data inconsistency, conflict resolution needed",
                recommendation: "Use for content, social feeds, non-critical data",
              ),
            ],
            chosen: -1,
          ),
        ]
        False -> []
      }
    }
    _ -> []
  }
}

/// Mark a round as complete
pub fn complete_round(session: InterviewSession) -> InterviewSession {
  let new_stage = case session.rounds_completed + 1 {
    1 -> interview.Discovery
    2 -> interview.Discovery
    3 -> interview.Refinement
    4 -> interview.Validation
    5 -> interview.Complete
    _ -> interview.Complete
  }

  interview.InterviewSession(
    ..session,
    rounds_completed: session.rounds_completed + 1,
    stage: new_stage,
  )
}

/// Get current round number based on answers
pub fn get_current_round(session: InterviewSession) -> Int {
  case session.answers {
    [] -> 1
    answers -> {
      let max_round = list.fold(answers, 0, fn(acc, answer) {
        case answer.round > acc {
          True -> answer.round
          False -> acc
        }
      })
      // If we've answered all questions in current round, move to next
      let current_round_count = list.length(
        list.filter(answers, fn(a) { a.round == max_round }),
      )
      let questions_in_round = list.length(
        interview_questions.get_questions_for_round(
          profile_to_string(session.profile),
          max_round,
        ),
      )

      case current_round_count >= questions_in_round {
        True -> max_round + 1
        False -> max_round
      }
    }
  }
}

/// Resolve a conflict by choosing an option
pub fn resolve_conflict(
  session: InterviewSession,
  conflict_id: String,
  chosen_option: Int,
) -> Result(InterviewSession, String) {
  let updated_conflicts = list.map(session.conflicts, fn(conflict) {
    case conflict.id == conflict_id {
      True ->
        interview.Conflict(
          ..conflict,
          chosen: chosen_option,
        )
      False -> conflict
    }
  })

  Ok(interview.InterviewSession(
    ..session,
    conflicts: updated_conflicts,
  ))
}

/// Mark a gap as resolved
pub fn resolve_gap(
  session: InterviewSession,
  gap_id: String,
  resolution: String,
) -> InterviewSession {
  let updated_gaps = list.map(session.gaps, fn(gap) {
    case gap.id == gap_id {
      True ->
        interview.Gap(
          ..gap,
          resolved: True,
          resolution: resolution,
        )
      False -> gap
    }
  })

  interview.InterviewSession(
    ..session,
    gaps: updated_gaps,
  )
}

/// Get all unresolved blocking gaps
pub fn get_blocking_gaps(session: InterviewSession) -> List(Gap) {
  list.filter(session.gaps, fn(gap) {
    gap.blocking && !gap.resolved
  })
}

/// Get all unresolved conflicts
pub fn get_unresolved_conflicts(session: InterviewSession) -> List(Conflict) {
  list.filter(session.conflicts, fn(conflict) {
    conflict.chosen == -1
  })
}

/// Check if interview can proceed (no blocking gaps)
pub fn can_proceed(session: InterviewSession) -> Result(Nil, String) {
  let blocking = get_blocking_gaps(session)
  case blocking {
    [] -> Ok(Nil)
    gaps -> {
      let gap_descriptions = list.map(gaps, fn(gap) { gap.description })
      Error("Blocking gaps: " <> string.join(gap_descriptions, "; "))
    }
  }
}

/// Format progress summary
pub fn format_progress(session: InterviewSession) -> String {
  let stage_str = case session.stage {
    interview.Discovery -> "Discovery"
    interview.Refinement -> "Refinement"
    interview.Validation -> "Validation"
    interview.Complete -> "Complete"
    interview.Paused -> "Paused"
  }

  let profile_str = profile_to_string(session.profile)
  let answer_count = list.length(session.answers)
  let gap_count = list.length(session.gaps)
  let conflict_count = list.length(session.conflicts)

  "Profile: "
  <> profile_str
  <> " | Stage: "
  <> stage_str
  <> " | Answers: "
  <> string.inspect(answer_count)
  <> " | Gaps: "
  <> string.inspect(gap_count)
  <> " | Conflicts: "
  <> string.inspect(conflict_count)
}

/// Helper: convert Profile to string
fn profile_to_string(profile: Profile) -> String {
  case profile {
    interview.Api -> "api"
    interview.Cli -> "cli"
    interview.Event -> "event"
    interview.Data -> "data"
    interview.Workflow -> "workflow"
    interview.UI -> "ui"
  }
}

/// Helper: convert string to Profile
pub fn string_to_profile(s: String) -> Result(Profile, String) {
  case string.lowercase(s) {
    "api" -> Ok(interview.Api)
    "cli" -> Ok(interview.Cli)
    "event" -> Ok(interview.Event)
    "data" -> Ok(interview.Data)
    "workflow" -> Ok(interview.Workflow)
    "ui" -> Ok(interview.UI)
    _ -> Error("Unknown profile: " <> s)
  }
}
