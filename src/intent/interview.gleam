/// Interview Engine
/// Structured interrogation system for discovering and refining specifications
/// Supports 5 rounds × multiple perspectives = comprehensive requirement capture

import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import intent/interview_questions.{
  type Perspective, type QuestionCategory, type QuestionPriority,
  type Question,
}

/// Profile types - determines which questions to ask
pub type Profile {
  Api
  Cli
  Event
  Data
  Workflow
  UI
}

/// Interview stages - persistent state machine
pub type InterviewStage {
  Discovery
  Refinement
  Validation
  Complete
  Paused
}

/// A single answer with metadata
pub type Answer {
  Answer(
    question_id: String,
    question_text: String,
    perspective: Perspective,
    round: Int,
    response: String,
    extracted: Dict(String, String),
    confidence: Float,
    notes: String,
    timestamp: String,
  )
}

/// Gap - missing information blocking spec completion
pub type Gap {
  Gap(
    id: String,
    field: String,
    description: String,
    blocking: Bool,
    suggested_default: String,
    why_needed: String,
    round: Int,
    resolved: Bool,
    resolution: String,
  )
}

/// Conflict - contradictions in requirements
pub type Conflict {
  Conflict(
    id: String,
    between: #(String, String),
    description: String,
    impact: String,
    options: List(ConflictResolution),
    chosen: Int,
  )
}

pub type ConflictResolution {
  ConflictResolution(
    option: String,
    description: String,
    tradeoffs: String,
    recommendation: String,
  )
}

/// Interview session - persistent state machine
pub type InterviewSession {
  InterviewSession(
    id: String,
    profile: Profile,
    created_at: String,
    updated_at: String,
    completed_at: String,
    stage: InterviewStage,
    rounds_completed: Int,
    answers: List(Answer),
    gaps: List(Gap),
    conflicts: List(Conflict),
    raw_notes: String,
  )
}

/// Extract field from answer text (AI-driven)
/// This is where the "AI adapts" - the extraction logic learns from patterns
pub fn extract_from_answer(
  question_id: String,
  response: String,
  extract_fields: List(String),
) -> Dict(String, String) {
  list.fold(extract_fields, dict.new(), fn(acc, field) {
    let extracted_value = simple_extract(field, response)
    case extracted_value {
      Ok(value) -> dict.insert(acc, field, value)
      Error(_) -> acc
    }
  })
}

/// Simple extraction patterns - can be extended to use NLP/LLM
fn simple_extract(field: String, text: String) -> Result(String, String) {
  case field {
    // Auth-related extractions
    "auth_method" -> extract_auth_method(text)

    // Entity/data model extractions
    "entities" -> extract_entities(text)

    // Audience extraction
    "audience" -> extract_audience(text)

    // Generic field - just return the text if it's substantial
    _ -> {
      let trimmed = string.trim(text)
      case string.length(trimmed) > 0 {
        True -> Ok(trimmed)
        False -> Error("Empty response")
      }
    }
  }
}

fn extract_auth_method(text: String) -> Result(String, String) {
  let lower = string.lowercase(text)
  case string.contains(lower, "jwt") {
    True -> Ok("jwt")
    False ->
      case string.contains(lower, "oauth") {
        True -> Ok("oauth")
        False ->
          case string.contains(lower, "session") {
            True -> Ok("session")
            False ->
              case string.contains(lower, "api key") {
                True -> Ok("api_key")
                False ->
                  case string.contains(lower, "none") {
                    True -> Ok("none")
                    False -> Error("Could not identify auth method")
                  }
              }
          }
      }
  }
}

fn extract_entities(text: String) -> Result(String, String) {
  // Look for capitalized words (likely entity names)
  let words = string.split(text, " ")
  let entities = list.filter_map(words, fn(word) {
    // Remove trailing punctuation
    let clean_word = case string.ends_with(word, ",") {
      True -> string.slice(word, 0, string.length(word) - 1)
      False -> word
    }
    let first_char = string.slice(clean_word, 0, 1)
    case string.uppercase(first_char) == first_char && string.length(clean_word) > 2 {
      True -> Ok(clean_word)
      False -> Error(Nil)
    }
  })
  case entities {
    [] -> Error("No entities found")
    _ -> Ok(string.join(entities, ", "))
  }
}

fn extract_audience(text: String) -> Result(String, String) {
  let lower = string.lowercase(text)
  case string.contains(lower, "mobile") {
    True -> Ok("mobile")
    False ->
      case string.contains(lower, "web") {
        True -> Ok("web")
        False ->
          case string.contains(lower, "api") {
            True -> Ok("api")
            False ->
              case string.contains(lower, "cli") {
                True -> Ok("cli")
                False ->
                  case string.contains(lower, "internal") {
                    True -> Ok("internal")
                    False -> Error("Could not identify audience")
                  }
              }
          }
      }
  }
}

/// Get questions for a specific round and profile
/// Delegate to interview_questions module
pub fn get_questions_for_round(
  profile: Profile,
  round: Int,
) -> List(Question) {
  // Convert profile to string
  let profile_str = case profile {
    Api -> "api"
    Cli -> "cli"
    Event -> "event"
    Data -> "data"
    Workflow -> "workflow"
    UI -> "ui"
  }

  // Call the dedicated questions module
  interview_questions.get_questions_for_round(profile_str, round)
}

/// Detect gaps from collected answers
pub fn detect_gaps(
  profile: Profile,
  answers: List(Answer),
) -> List(Gap) {
  // Check critical fields for the profile
  let required_fields = case profile {
    Api -> [
      "base_url", "auth_method", "happy_path", "error_cases",
      "response_format",
    ]
    Cli -> ["command_name", "happy_path", "help_text", "exit_codes"]
    Event -> ["event_type", "payload_schema", "trigger"]
    Data -> ["data_model", "access_patterns", "retention"]
    Workflow -> ["steps", "happy_path", "error_recovery"]
    UI -> ["user_flows", "happy_path", "states"]
  }

  let answered_fields = list.fold(answers, dict.new(), fn(acc, answer) {
    list.fold(dict.to_list(answer.extracted), acc, fn(inner_acc, pair) {
      dict.insert(inner_acc, pair.0, pair.1)
    })
  })

  list.filter_map(required_fields, fn(field) {
    case dict.get(answered_fields, field) {
      Ok(_) -> Error(Nil)
      Error(_) ->
        Ok(Gap(
          id: "gap-" <> field,
          field: field,
          description: "Missing: " <> field,
          blocking: True,
          suggested_default: "",
          why_needed: "Required for " <> field,
          round: 0,
          resolved: False,
          resolution: "",
        ))
    }
  })
}

/// Detect conflicts between answers
pub fn detect_conflicts(answers: List(Answer)) -> List(Conflict) {
  // Example conflict patterns
  // "fast" + "strongly_consistent" = CAP theorem
  // "anonymous" + "audit_trail" = traceability
  // "simple" + 20_requirements = scope creep

  let lower_responses = list.map(answers, fn(ans) {
    #(ans.question_id, string.lowercase(ans.response))
  })

  let conflicts = []

  // Check for CAP theorem conflict
  let has_fast = list.any(lower_responses, fn(pair) {
    string.contains(pair.1, "fast") || string.contains(pair.1, "latency")
  })
  let has_consistent = list.any(lower_responses, fn(pair) {
    string.contains(pair.1, "consistent") || string.contains(pair.1, "accurate")
  })

  let conflicts = case has_fast && has_consistent {
    True ->
      list.append(conflicts, [
        Conflict(
          id: "conflict-cap",
          between: #("latency", "consistency"),
          description: "You want both speed AND strong consistency",
          impact: "CAP theorem: impossible to have both at scale",
          options: [
            ConflictResolution(
              option: "Prioritize latency",
              description: "Accept eventual consistency, cache aggressively",
              tradeoffs: "Data may be stale for a few seconds",
              recommendation: "Use when user experience matters more than instant accuracy",
            ),
            ConflictResolution(
              option: "Prioritize consistency",
              description: "Use strong consistency, accept higher latency",
              tradeoffs: "P99 latencies may be 100-500ms, not 10ms",
              recommendation: "Use for financial/banking systems",
            ),
          ],
          chosen: -1,
        ),
      ])
    False -> conflicts
  }

  // Check for anonymous + audit
  let has_anonymous = list.any(lower_responses, fn(pair) {
    string.contains(pair.1, "anonymous")
  })
  let has_audit = list.any(lower_responses, fn(pair) {
    string.contains(pair.1, "audit") || string.contains(pair.1, "log")
  })

  case has_anonymous && has_audit {
    True ->
      list.append(conflicts, [
        Conflict(
          id: "conflict-anon-audit",
          between: #("anonymous", "audit"),
          description: "You want anonymous users AND an audit trail",
          impact: "Can't audit completely anonymous users without some tracking",
          options: [
            ConflictResolution(
              option: "Use pseudonymous IDs",
              description: "Generate per-session IDs, don't link to user identity",
              tradeoffs: "Still traceable within a session",
              recommendation: "Most balanced approach",
            ),
            ConflictResolution(
              option: "Aggregate logging only",
              description: "Log statistics (actions per hour) not individual actions",
              tradeoffs: "Can't trace individual user behavior",
              recommendation: "For privacy-critical systems",
            ),
          ],
          chosen: -1,
        ),
      ])
    False -> conflicts
  }

  conflicts
}

/// Calculate confidence in answer extraction (0-1)
pub fn calculate_confidence(
  question_id: String,
  response: String,
  extracted: Dict(String, String),
) -> Float {
  let response_length = string.length(string.trim(response))
  let field_count = dict.size(extracted)

  // Longer responses with more fields = higher confidence
  case response_length > 50 && field_count > 0 {
    True -> 0.85
    False -> 0.6
  }
}

/// Format a question for display
pub fn format_question(question: Question) -> String {
  let priority_str = case question.priority {
    interview_questions.Critical -> "[CRITICAL]"
    interview_questions.Important -> "[IMPORTANT]"
    interview_questions.NiceTohave -> ""
  }

  let context_str = case string.length(question.context) > 0 {
    True -> "\n" <> question.context
    False -> ""
  }

  let example_str = case string.length(question.example) > 0 {
    True -> "\nExample: " <> question.example
    False -> ""
  }

  priority_str
    <> " "
    <> question.question
    <> context_str
    <> example_str
}

/// Create a new session
pub fn create_session(id: String, profile: Profile, timestamp: String) -> InterviewSession {
  InterviewSession(
    id: id,
    profile: profile,
    created_at: timestamp,
    updated_at: timestamp,
    completed_at: "",
    stage: Discovery,
    rounds_completed: 0,
    answers: [],
    gaps: [],
    conflicts: [],
    raw_notes: "",
  )
}

/// Add answer to session
pub fn add_answer(
  session: InterviewSession,
  answer: Answer,
) -> InterviewSession {
  let new_answers = list.append(session.answers, [answer])
  InterviewSession(
    ..session,
    answers: new_answers,
    updated_at: answer.timestamp,
  )
}

/// Mark round as completed
pub fn complete_round(session: InterviewSession) -> InterviewSession {
  let new_round = session.rounds_completed + 1
  InterviewSession(
    ..session,
    rounds_completed: new_round,
    stage: case new_round {
      5 -> Complete
      _ -> Refinement
    },
  )
}
