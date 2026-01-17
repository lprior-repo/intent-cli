/// Streaming Q&A Protocol for AI Agents
/// Provides rich, structured output for machine-readable interview sessions
/// Maintains ALL rigor while enabling simple request/response flow
import gleam/dict

// Removed unused import
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/interview.{type Answer, type InterviewSession, type Profile}
import intent/interview_questions
import intent/question_types.{
  type Perspective, type Question, type QuestionCategory, type QuestionPriority,
  Business, Constraint, Critical, Dependency, Developer, EdgeCase, ErrorCase,
  HappyPath, Important, NiceTohave, NonFunctional, Ops, Security, User,
}

/// Extended question context for AI agents
pub type QuestionContext {
  QuestionContext(
    question: Question,
    round_info: RoundInfo,
    ears_info: EarsInfo,
    metadata: QuestionMetadata,
  )
}

/// Round information for progress tracking
pub type RoundInfo {
  RoundInfo(
    round: Int,
    round_name: String,
    round_description: String,
    round_focus: String,
  )
}

/// EARS pattern information
pub type EarsInfo {
  EarsInfo(
    pattern: String,
    hint: String,
    examples: List(String),
    template: String,
  )
}

/// Question metadata for dependencies and constraints
pub type QuestionMetadata {
  QuestionMetadata(
    can_skip: Bool,
    depends_on: List(String),
    blocks: List(String),
    validation_rules: List(String),
  )
}

/// Progress information
pub type ProgressInfo {
  ProgressInfo(
    current_step: Int,
    total_steps: Int,
    percent_complete: Int,
    round: Int,
    round_name: String,
    rounds_completed: Int,
    total_rounds: Int,
    questions_by_round: List(RoundProgress),
    questions_by_perspective: List(PerspectiveProgress),
    questions_by_priority: List(PriorityProgress),
  )
}

/// Round-specific progress
pub type RoundProgress {
  RoundProgress(round: Int, name: String, total: Int, answered: Int)
}

/// Perspective-specific progress
pub type PerspectiveProgress {
  PerspectiveProgress(
    perspective: String,
    total: Int,
    answered: Int,
    critical: Int,
  )
}

/// Priority-specific progress
pub type PriorityProgress {
  PriorityProgress(priority: String, total: Int, answered: Int)
}

/// Answer summary for previous context
pub type AnswerSummary {
  AnswerSummary(
    question_id: String,
    question_text: String,
    response: String,
    extracted: dict.Dict(String, String),
    confidence: Float,
    timestamp: String,
  )
}

// =============================================================================
// ROUND INFORMATION
// =============================================================================

/// Get round information for a given round number
pub fn get_round_info(round: Int) -> RoundInfo {
  case round {
    1 ->
      RoundInfo(
        round: 1,
        round_name: "Discovery - Happy Path",
        round_description: "Understand core purpose and primary use cases",
        round_focus: "What should this do? Who uses it? What's the main flow?",
      )
    2 ->
      RoundInfo(
        round: 2,
        round_name: "Discovery - Behaviors",
        round_description: "Define all expected behaviors and interactions",
        round_focus: "What are all the things this should do? How do they interact?",
      )
    3 ->
      RoundInfo(
        round: 3,
        round_name: "Refinement - Edge Cases",
        round_description: "Identify edge cases and error scenarios",
        round_focus: "What can go wrong? What are the boundaries? What's undefined?",
      )
    4 ->
      RoundInfo(
        round: 4,
        round_name: "Security & Operations",
        round_description: "Address security, scalability, and operational concerns",
        round_focus: "How do we secure it? How does it scale? How do we operate it?",
      )
    5 ->
      RoundInfo(
        round: 5,
        round_name: "Validation",
        round_description: "Verify completeness and resolve conflicts",
        round_focus: "Did we cover everything? Are there contradictions? What's missing?",
      )
    _ ->
      RoundInfo(
        round: round,
        round_name: "Unknown Round",
        round_description: "",
        round_focus: "",
      )
  }
}

// =============================================================================
// EARS PATTERN INFORMATION
// =============================================================================

/// Infer EARS pattern from question
pub fn infer_ears_pattern(question: Question) -> String {
  // Check question text for pattern indicators
  let text_lower = string.lowercase(question.question)

  case question.category {
    HappyPath ->
      case
        string.contains(text_lower, "when") || string.contains(text_lower, "if")
      {
        True -> "event"
        False -> "ubiquitous"
      }
    ErrorCase -> "unwanted"
    EdgeCase ->
      case
        string.contains(text_lower, "while")
        || string.contains(text_lower, "during")
      {
        True -> "state"
        False -> "unwanted"
      }
    Constraint ->
      case string.contains(text_lower, "where") {
        True -> "optional"
        False -> "ubiquitous"
      }
    Dependency ->
      case string.contains(text_lower, "when") {
        True -> "event"
        False -> "ubiquitous"
      }
    NonFunctional -> "ubiquitous"
  }
}

/// Get EARS pattern information
pub fn get_ears_info(pattern: String) -> EarsInfo {
  case pattern {
    "ubiquitous" ->
      EarsInfo(
        pattern: "ubiquitous",
        hint: "Format: THE SYSTEM SHALL [behavior]",
        examples: [
          "THE SYSTEM SHALL accept HTTP POST requests to /users",
          "THE SYSTEM SHALL return JSON responses with 200 status",
          "THE SYSTEM SHALL validate email addresses before storage",
        ],
        template: "THE SYSTEM SHALL [behavior]",
      )
    "event" ->
      EarsInfo(
        pattern: "event",
        hint: "Format: WHEN [trigger], THE SYSTEM SHALL [response]",
        examples: [
          "WHEN a user clicks submit, THE SYSTEM SHALL validate the form",
          "WHEN an invalid token is provided, THE SYSTEM SHALL return 401",
          "WHEN the database is unavailable, THE SYSTEM SHALL retry 3 times",
        ],
        template: "WHEN [trigger], THE SYSTEM SHALL [response]",
      )
    "state" ->
      EarsInfo(
        pattern: "state",
        hint: "Format: WHILE [condition], THE SYSTEM SHALL [behavior]",
        examples: [
          "WHILE processing a request, THE SYSTEM SHALL block duplicate submissions",
          "WHILE the user is logged in, THE SYSTEM SHALL refresh the session token",
          "WHILE data is syncing, THE SYSTEM SHALL display a loading indicator",
        ],
        template: "WHILE [condition], THE SYSTEM SHALL [behavior]",
      )
    "unwanted" ->
      EarsInfo(
        pattern: "unwanted",
        hint: "Format: IF [condition], THEN THE SYSTEM SHALL [behavior]",
        examples: [
          "IF authentication fails, THEN THE SYSTEM SHALL return 401 Unauthorized",
          "IF the request is malformed, THEN THE SYSTEM SHALL return 400 Bad Request",
          "IF the rate limit is exceeded, THEN THE SYSTEM SHALL return 429 Too Many Requests",
        ],
        template: "IF [condition], THEN THE SYSTEM SHALL [behavior]",
      )
    "optional" ->
      EarsInfo(
        pattern: "optional",
        hint: "Format: WHERE [feature enabled], THE SYSTEM SHALL [behavior]",
        examples: [
          "WHERE debug mode is enabled, THE SYSTEM SHALL log request bodies",
          "WHERE premium features are active, THE SYSTEM SHALL allow unlimited requests",
          "WHERE audit logging is configured, THE SYSTEM SHALL record all changes",
        ],
        template: "WHERE [feature enabled], THE SYSTEM SHALL [behavior]",
      )
    _ ->
      EarsInfo(
        pattern: "ubiquitous",
        hint: "Format: THE SYSTEM SHALL [behavior]",
        examples: ["THE SYSTEM SHALL [behavior]"],
        template: "THE SYSTEM SHALL [behavior]",
      )
  }
}

// =============================================================================
// QUESTION METADATA
// =============================================================================

/// Build question metadata
pub fn build_question_metadata(question: Question) -> QuestionMetadata {
  QuestionMetadata(
    can_skip: question.priority != Critical,
    depends_on: question.depends_on,
    blocks: question.blocks,
    validation_rules: get_validation_rules(question),
  )
}

/// Get validation rules for a question
fn get_validation_rules(question: Question) -> List(String) {
  let base_rules = [
    "Must follow EARS pattern: "
    <> infer_ears_pattern(question)
    <> " ("
    <> get_ears_info(infer_ears_pattern(question)).template
    <> ")",
  ]

  let priority_rules = case question.priority {
    Critical -> ["Minimum 10 characters required for critical questions"]
    Important -> ["Recommended: 20+ characters for detailed answers"]
    NiceTohave -> []
  }

  let type_rules = case question.expected_type {
    "text" -> ["Free-form text response"]
    "number" -> ["Must be a valid number"]
    "boolean" -> ["Must be yes/no or true/false"]
    "list" -> ["Provide comma-separated list"]
    _ -> []
  }

  list.flatten([base_rules, priority_rules, type_rules])
}

// =============================================================================
// PROGRESS CALCULATION
// =============================================================================

/// Calculate comprehensive progress information
pub fn calculate_progress(
  session: InterviewSession,
  profile: Profile,
) -> ProgressInfo {
  let profile_str = interview.profile_to_string(profile)

  // Get all questions for this profile
  let all_questions =
    list.flat_map([1, 2, 3, 4, 5], fn(round) {
      interview_questions.get_questions_for_round(profile_str, round)
    })

  let total_steps = list.length(all_questions)
  let answered_count = list.length(session.answers)
  let percent_complete = case total_steps > 0 {
    True -> { answered_count * 100 } / total_steps
    False -> 0
  }

  // Current round
  let current_round = interview.get_current_round(session)
  let round_info = get_round_info(current_round)

  // Round-by-round progress
  let rounds_progress =
    calculate_rounds_progress(all_questions, session.answers)

  // Perspective progress
  let perspective_progress =
    calculate_perspective_progress(all_questions, session.answers)

  // Priority progress
  let priority_progress =
    calculate_priority_progress(all_questions, session.answers)

  ProgressInfo(
    current_step: answered_count + 1,
    total_steps: total_steps,
    percent_complete: percent_complete,
    round: current_round,
    round_name: round_info.round_name,
    rounds_completed: session.rounds_completed,
    total_rounds: 5,
    questions_by_round: rounds_progress,
    questions_by_perspective: perspective_progress,
    questions_by_priority: priority_progress,
  )
}

/// Calculate progress by round
fn calculate_rounds_progress(
  all_questions: List(Question),
  answers: List(Answer),
) -> List(RoundProgress) {
  let answered_ids = list.map(answers, fn(a) { a.question_id })

  list.map([1, 2, 3, 4, 5], fn(round) {
    let round_questions = list.filter(all_questions, fn(q) { q.round == round })
    let round_answered =
      list.filter(round_questions, fn(q) { list.contains(answered_ids, q.id) })

    let round_info = get_round_info(round)

    RoundProgress(
      round: round,
      name: round_info.round_name,
      total: list.length(round_questions),
      answered: list.length(round_answered),
    )
  })
}

/// Calculate progress by perspective
fn calculate_perspective_progress(
  all_questions: List(Question),
  answers: List(Answer),
) -> List(PerspectiveProgress) {
  let answered_ids = list.map(answers, fn(a) { a.question_id })

  let perspectives = [User, Developer, Ops, Security, Business]

  list.map(perspectives, fn(perspective) {
    let perspective_questions =
      list.filter(all_questions, fn(q) { q.perspective == perspective })
    let perspective_answered =
      list.filter(perspective_questions, fn(q) {
        list.contains(answered_ids, q.id)
      })
    let critical_count =
      list.filter(perspective_questions, fn(q) { q.priority == Critical })
      |> list.length()

    PerspectiveProgress(
      perspective: perspective_to_string(perspective),
      total: list.length(perspective_questions),
      answered: list.length(perspective_answered),
      critical: critical_count,
    )
  })
}

/// Calculate progress by priority
fn calculate_priority_progress(
  all_questions: List(Question),
  answers: List(Answer),
) -> List(PriorityProgress) {
  let answered_ids = list.map(answers, fn(a) { a.question_id })

  let priorities = [Critical, Important, NiceTohave]

  list.map(priorities, fn(priority) {
    let priority_questions =
      list.filter(all_questions, fn(q) { q.priority == priority })
    let priority_answered =
      list.filter(priority_questions, fn(q) {
        list.contains(answered_ids, q.id)
      })

    PriorityProgress(
      priority: priority_to_string(priority),
      total: list.length(priority_questions),
      answered: list.length(priority_answered),
    )
  })
}

// =============================================================================
// CONTEXT BUILDING
// =============================================================================

/// Build full context for a question
pub fn build_question_context(question: Question) -> QuestionContext {
  let round_info = get_round_info(question.round)
  let pattern = infer_ears_pattern(question)
  let ears_info = get_ears_info(pattern)
  let metadata = build_question_metadata(question)

  QuestionContext(
    question: question,
    round_info: round_info,
    ears_info: ears_info,
    metadata: metadata,
  )
}

/// Build answer summary from answer
pub fn build_answer_summary(answer: Answer) -> AnswerSummary {
  AnswerSummary(
    question_id: answer.question_id,
    question_text: answer.question_text,
    response: answer.response,
    extracted: answer.extracted,
    confidence: answer.confidence,
    timestamp: answer.timestamp,
  )
}

/// Get previous answer if available
pub fn get_previous_answer(session: InterviewSession) -> Option(AnswerSummary) {
  case list.last(session.answers) {
    Ok(answer) -> Some(build_answer_summary(answer))
    Error(_) -> None
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Convert perspective to string
fn perspective_to_string(perspective: Perspective) -> String {
  case perspective {
    User -> "user"
    Developer -> "developer"
    Ops -> "ops"
    Security -> "security"
    Business -> "business"
  }
}

/// Convert priority to string
fn priority_to_string(priority: QuestionPriority) -> String {
  case priority {
    Critical -> "critical"
    Important -> "important"
    NiceTohave -> "nice_to_have"
  }
}

/// Convert category to string
pub fn category_to_string(category: QuestionCategory) -> String {
  case category {
    HappyPath -> "happy_path"
    ErrorCase -> "error_case"
    EdgeCase -> "edge_case"
    Constraint -> "constraint"
    Dependency -> "dependency"
    NonFunctional -> "non_functional"
  }
}

/// Format list as CUE string array
pub fn format_cue_string_list(strings: List(String)) -> String {
  case strings {
    [] -> ""
    items -> {
      let escaped =
        list.map(items, fn(s) { "\"" <> escape_cue_string(s) <> "\"" })
      string.join(escaped, ", ")
    }
  }
}

/// Escape string for CUE output
pub fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
  |> string.replace("\r", "\\r")
}

/// Format float to 2 decimal places
pub fn format_float_2dp(f: Float) -> String {
  // Simple string formatting for floats
  let s = string.inspect(f)
  case string.contains(s, ".") {
    True -> {
      let parts = string.split(s, ".")
      case parts {
        [whole, decimal] -> {
          let decimal_trimmed = string.slice(decimal, 0, 2)
          whole <> "." <> decimal_trimmed
        }
        _ -> s
      }
    }
    False -> s <> ".00"
  }
}
