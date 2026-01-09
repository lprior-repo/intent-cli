/// AI-CUE Protocol Implementation
/// Implements the AI-driven interview protocol with CUE as the center of the universe
///
/// Key principles:
/// - All state stored in .intent/*.cue files
/// - CLI outputs #AIDirective for AI control
/// - AI is stateless relay - all state lives in CUE files
/// - JSON export via cue export for AI parsing

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/interview_questions
import intent/question_types.{type Question}
import simplifile

// =============================================================================
// TYPES: Matching schema/ai_interview.cue
// =============================================================================

/// Interview state machine states
pub type InterviewState {
  NotStarted
  RoundUbiquitous
  RoundEventDriven
  RoundStateDriven
  RoundOptional
  RoundUnwanted
  RoundComplex
  RoundInversion
  RoundPremortem
  ClarificationNeeded
  Complete
}

/// Actions the AI should take
pub type AIAction {
  AskQuestion
  ClarifyAnswer
  ConfirmUnderstanding
  PresentSummary
  AwaitApproval
  GenerateBeads
  Done
}

/// EARS patterns for requirement categorization
pub type EARSPattern {
  Ubiquitous
  EventDriven
  StateDriven
  Optional
  Unwanted
  Complex
  Inversion
  Premortem
}

/// Confidence levels for answers
pub type Confidence {
  High
  Medium
  Low
}

/// Progress through the interview
pub type Progress {
  Progress(
    current_round: Int,
    total_rounds: Int,
    questions_asked: Int,
    questions_remain: Int,
    percent_complete: Float,
  )
}

/// Answer submission from the AI
pub type AnswerSubmission {
  AnswerSubmission(
    question_id: String,
    raw_answer: String,
    parsed: Option(ParsedAnswer),
    confidence: Confidence,
    uncertainty_reason: Option(String),
  )
}

/// Parsed EARS components from answer
pub type ParsedAnswer {
  ParsedAnswer(
    pattern: Option(EARSPattern),
    trigger: Option(String),
    state: Option(String),
    condition: Option(String),
    shall: Option(String),
    shall_not: Option(String),
  )
}

/// Validation error for feedback
pub type ValidationError {
  ValidationError(
    field: String,
    message: String,
    suggestion: String,
  )
}

/// EARS requirement structure
pub type EARSRequirement {
  EARSRequirement(
    id: String,
    pattern: EARSPattern,
    trigger: Option(String),
    state: Option(String),
    condition: Option(String),
    shall: String,
    shall_not: Option(String),
    raw_text: String,
  )
}

/// Likely cause for premortem analysis
pub type LikelyCause {
  LikelyCause(
    cause: String,
    probability: Confidence,
    mitigation: String,
  )
}

/// Inversions collected during interview
pub type InversionsData {
  InversionsData(
    security: List(String),
    usability: List(String),
    integration: List(String),
  )
}

/// Premortem data collected during interview
pub type PremortemData {
  PremortemData(
    assumed_failure: String,
    likely_causes: List(LikelyCause),
  )
}

/// All collected requirements during interview
pub type CollectedRequirements {
  CollectedRequirements(
    ubiquitous: List(EARSRequirement),
    event_driven: List(EARSRequirement),
    state_driven: List(EARSRequirement),
    optional: List(EARSRequirement),
    unwanted: List(EARSRequirement),
    complex: List(EARSRequirement),
    inversions: InversionsData,
    premortem: PremortemData,
  )
}

/// The main AI directive - tells AI what to do next
pub type AIDirective {
  AIDirective(
    state: InterviewState,
    action: AIAction,
    question: Option(Question),
    progress: Progress,
    collected: CollectedRequirements,
    errors: Option(List(ValidationError)),
  )
}

// =============================================================================
// CORE FUNCTIONS
// =============================================================================

/// Create initial directive for a new interview session
pub fn create_initial_directive(profile: String, _session_id: String) -> AIDirective {
  // Get first question for round 1
  let questions = interview_questions.get_questions_for_round(profile, 1)
  let total_questions = count_total_questions(profile)

  let first_question = case questions {
    [q, ..] -> Some(q)
    [] -> None
  }

  AIDirective(
    state: NotStarted,
    action: AskQuestion,
    question: first_question,
    progress: Progress(
      current_round: 1,
      total_rounds: 5,
      questions_asked: 0,
      questions_remain: total_questions,
      percent_complete: 0.0,
    ),
    collected: empty_collected(),
    errors: None,
  )
}

/// Calculate progress based on questions answered
pub fn calculate_progress(
  questions_asked: Int,
  total_questions: Int,
  current_round: Int,
  total_rounds: Int,
) -> Progress {
  let remain = total_questions - questions_asked
  let percent = case total_questions > 0 {
    True -> int.to_float(questions_asked * 100) /. int.to_float(total_questions)
    False -> 0.0
  }

  Progress(
    current_round: current_round,
    total_rounds: total_rounds,
    questions_asked: questions_asked,
    questions_remain: remain,
    percent_complete: percent,
  )
}

/// Parse an answer submission from raw input
pub fn parse_answer_submission(
  question_id: String,
  raw_answer: String,
  confidence_str: String,
) -> Result(AnswerSubmission, String) {
  // Validate answer is not empty
  let trimmed = string.trim(raw_answer)
  case string.is_empty(trimmed) {
    True -> Error("Answer cannot be empty")
    False -> {
      let confidence = parse_confidence(confidence_str)
      let parsed = try_parse_ears(trimmed)

      Ok(AnswerSubmission(
        question_id: question_id,
        raw_answer: trimmed,
        parsed: parsed,
        confidence: confidence,
        uncertainty_reason: None,
      ))
    }
  }
}

/// Process an answer and return next directive
pub fn process_answer(
  current: AIDirective,
  submission: AnswerSubmission,
) -> AIDirective {
  // Validate the answer
  let validation_result = validate_answer(submission)

  case validation_result {
    Error(errors) -> {
      // Answer needs clarification
      AIDirective(
        ..current,
        action: ClarifyAnswer,
        errors: Some(errors),
      )
    }
    Ok(requirement) -> {
      // Answer is valid, add to collected and advance
      let new_collected = add_requirement(current.collected, requirement)
      let new_questions_asked = current.progress.questions_asked + 1
      let total = current.progress.questions_asked + current.progress.questions_remain

      let new_progress = calculate_progress(
        new_questions_asked,
        total,
        current.progress.current_round,
        current.progress.total_rounds,
      )

      // Check if interview is complete
      case new_progress.questions_remain {
        0 -> {
          AIDirective(
            state: Complete,
            action: GenerateBeads,
            question: None,
            progress: new_progress,
            collected: new_collected,
            errors: None,
          )
        }
        _ -> {
          // Get next question
          let next_question = get_next_question(current, new_questions_asked)
          let next_state = determine_state(new_progress.current_round)

          AIDirective(
            state: next_state,
            action: AskQuestion,
            question: next_question,
            progress: new_progress,
            collected: new_collected,
            errors: None,
          )
        }
      }
    }
  }
}

/// Convert directive to CUE string output (default: no KIRK extension fields)
pub fn directive_to_cue(directive: AIDirective) -> String {
  directive_to_cue_internal(directive, False)
}

/// Convert directive to CUE string output WITH KIRK analysis fields
/// Use this when --with-analysis flag is specified
pub fn directive_to_cue_with_analysis(directive: AIDirective) -> String {
  directive_to_cue_internal(directive, True)
}

/// Internal function that handles both modes
fn directive_to_cue_internal(directive: AIDirective, include_analysis: Bool) -> String {
  let state_str = state_to_string(directive.state)
  let action_str = action_to_string(directive.action)

  let question_str = case directive.question {
    None -> ""
    Some(q) -> question_to_cue(q)
  }

  let errors_str = case directive.errors {
    None -> ""
    Some(errs) -> errors_to_cue(errs)
  }

  let collected_str = case include_analysis {
    True -> collected_to_cue_with_analysis(directive.collected)
    False -> collected_to_cue_basic(directive.collected)
  }

  "// AI Interview Directive\n"
  <> "// Generated by Intent CLI\n\n"
  <> "state: \"" <> state_str <> "\"\n"
  <> "action: \"" <> action_str <> "\"\n"
  <> question_str
  <> "progress: {\n"
  <> "\tcurrent_round: " <> int.to_string(directive.progress.current_round) <> "\n"
  <> "\ttotal_rounds: " <> int.to_string(directive.progress.total_rounds) <> "\n"
  <> "\tquestions_asked: " <> int.to_string(directive.progress.questions_asked) <> "\n"
  <> "\tquestions_remain: " <> int.to_string(directive.progress.questions_remain) <> "\n"
  <> "\tpercent_complete: " <> float.to_string(directive.progress.percent_complete) <> "\n"
  <> "}\n"
  <> collected_str
  <> errors_str
}

// =============================================================================
// EMPTY/DEFAULT CONSTRUCTORS
// =============================================================================

/// Create empty collected requirements
pub fn empty_collected() -> CollectedRequirements {
  CollectedRequirements(
    ubiquitous: [],
    event_driven: [],
    state_driven: [],
    optional: [],
    unwanted: [],
    complex: [],
    inversions: InversionsData(
      security: [],
      usability: [],
      integration: [],
    ),
    premortem: PremortemData(
      assumed_failure: "",
      likely_causes: [],
    ),
  )
}

/// Add requirement to collected based on pattern
pub fn add_requirement(
  collected: CollectedRequirements,
  req: EARSRequirement,
) -> CollectedRequirements {
  case req.pattern {
    Ubiquitous -> CollectedRequirements(
      ..collected,
      ubiquitous: list.append(collected.ubiquitous, [req]),
    )
    EventDriven -> CollectedRequirements(
      ..collected,
      event_driven: list.append(collected.event_driven, [req]),
    )
    StateDriven -> CollectedRequirements(
      ..collected,
      state_driven: list.append(collected.state_driven, [req]),
    )
    Optional -> CollectedRequirements(
      ..collected,
      optional: list.append(collected.optional, [req]),
    )
    Unwanted -> CollectedRequirements(
      ..collected,
      unwanted: list.append(collected.unwanted, [req]),
    )
    Complex -> CollectedRequirements(
      ..collected,
      complex: list.append(collected.complex, [req]),
    )
    _ -> collected
  }
}

// =============================================================================
// EARS PATTERN DETECTION
// =============================================================================

/// Detect EARS pattern from answer text
pub fn detect_ears_pattern(text: String) -> EARSPattern {
  let lower = string.lowercase(text)

  // Check patterns in order of specificity
  case string.contains(lower, "if ") && string.contains(lower, "shall not") {
    True -> Unwanted
    False -> case string.contains(lower, "while ") && string.contains(lower, "when ") {
      True -> Complex
      False -> case string.contains(lower, "while ") {
        True -> StateDriven
        False -> case string.contains(lower, "when ") {
          True -> EventDriven
          False -> case string.contains(lower, "where ") {
            True -> Optional
            False -> Ubiquitous
          }
        }
      }
    }
  }
}

// =============================================================================
// VALIDATION
// =============================================================================

/// Validate session file path
pub fn validate_session_file(path: String) -> Result(Nil, String) {
  case string.starts_with(path, ".intent/session-") && string.ends_with(path, ".cue") {
    True -> Ok(Nil)
    False -> Error("Invalid session file path. Expected: .intent/session-{id}.cue")
  }
}

/// Ensure .intent directory exists
pub fn ensure_intent_directory() -> Result(Nil, String) {
  case simplifile.verify_is_directory(".intent") {
    Ok(_) -> Ok(Nil)
    Error(_) -> {
      case simplifile.create_directory(".intent") {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error("Failed to create .intent directory: " <> string.inspect(e))
      }
    }
  }
}

/// Validate answer and convert to requirement
fn validate_answer(submission: AnswerSubmission) -> Result(EARSRequirement, List(ValidationError)) {
  let answer = submission.raw_answer

  // Check minimum length
  case string.length(answer) < 5 {
    True -> Error([
      ValidationError(
        field: "raw_answer",
        message: "Answer is too short",
        suggestion: "Please provide a more detailed response",
      ),
    ])
    False -> {
      // Check for question marks (confused user)
      case string.contains(answer, "???") || string.contains(answer, "?") && string.length(answer) < 10 {
        True -> Error([
          ValidationError(
            field: "raw_answer",
            message: "Answer appears to be a question or unclear",
            suggestion: "Please provide a statement describing the requirement",
          ),
        ])
        False -> {
          // Valid answer - create requirement
          let pattern = detect_ears_pattern(answer)
          let shall = extract_shall_clause(answer)

          Ok(EARSRequirement(
            id: "req-" <> submission.question_id,
            pattern: pattern,
            trigger: extract_trigger(answer),
            state: extract_state(answer),
            condition: extract_condition(answer),
            shall: shall,
            shall_not: extract_shall_not(answer),
            raw_text: answer,
          ))
        }
      }
    }
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

fn parse_confidence(s: String) -> Confidence {
  case string.lowercase(s) {
    "high" -> High
    "medium" -> Medium
    "low" -> Low
    _ -> Medium
  }
}

fn try_parse_ears(text: String) -> Option(ParsedAnswer) {
  let pattern = detect_ears_pattern(text)

  Some(ParsedAnswer(
    pattern: Some(pattern),
    trigger: extract_trigger(text),
    state: extract_state(text),
    condition: extract_condition(text),
    shall: extract_shall(text),
    shall_not: extract_shall_not(text),
  ))
}

fn extract_trigger(text: String) -> Option(String) {
  let lower = string.lowercase(text)
  case string.split(lower, "when ") {
    [_, rest, ..] -> {
      case string.split(rest, " the system") {
        [trigger, ..] -> Some(string.trim(trigger))
        _ -> None
      }
    }
    _ -> None
  }
}

fn extract_state(text: String) -> Option(String) {
  let lower = string.lowercase(text)
  case string.split(lower, "while ") {
    [_, rest, ..] -> {
      case string.split(rest, " the system") {
        [state, ..] -> Some(string.trim(state))
        _ -> {
          case string.split(rest, ",") {
            [state, ..] -> Some(string.trim(state))
            _ -> None
          }
        }
      }
    }
    _ -> None
  }
}

fn extract_condition(text: String) -> Option(String) {
  let lower = string.lowercase(text)
  case string.split(lower, "where ") {
    [_, rest, ..] -> {
      case string.split(rest, " the system") {
        [condition, ..] -> Some(string.trim(condition))
        _ -> None
      }
    }
    _ -> {
      case string.split(lower, "if ") {
        [_, rest, ..] -> {
          case string.split(rest, " then") {
            [condition, ..] -> Some(string.trim(condition))
            _ -> None
          }
        }
        _ -> None
      }
    }
  }
}

fn extract_shall(text: String) -> Option(String) {
  let lower = string.lowercase(text)
  case string.split(lower, "shall ") {
    [_, rest, ..] -> Some(string.trim(rest))
    _ -> None
  }
}

fn extract_shall_clause(text: String) -> String {
  case extract_shall(text) {
    Some(s) -> s
    None -> text
  }
}

fn extract_shall_not(text: String) -> Option(String) {
  let lower = string.lowercase(text)
  case string.split(lower, "shall not ") {
    [_, rest, ..] -> Some(string.trim(rest))
    _ -> None
  }
}

fn count_total_questions(profile: String) -> Int {
  // Count questions across all 5 rounds
  list.range(1, 5)
  |> list.map(fn(round) {
    list.length(interview_questions.get_questions_for_round(profile, round))
  })
  |> list.fold(0, fn(acc, count) { acc + count })
}

fn get_next_question(
  current: AIDirective,
  questions_asked: Int,
) -> Option(Question) {
  // Determine which round and question index
  let profile = "api"  // TODO: get from session
  let questions = interview_questions.get_questions_for_round(profile, current.progress.current_round)

  case list.drop(questions, questions_asked) {
    [next, ..] -> Some(next)
    [] -> {
      // Try next round
      let next_round = current.progress.current_round + 1
      case next_round <= 5 {
        True -> {
          let next_round_questions = interview_questions.get_questions_for_round(profile, next_round)
          case next_round_questions {
            [first, ..] -> Some(first)
            [] -> None
          }
        }
        False -> None
      }
    }
  }
}

fn determine_state(round: Int) -> InterviewState {
  case round {
    1 -> RoundUbiquitous
    2 -> RoundEventDriven
    3 -> RoundStateDriven
    4 -> RoundOptional
    5 -> RoundUnwanted
    _ -> Complete
  }
}

fn state_to_string(state: InterviewState) -> String {
  case state {
    NotStarted -> "not_started"
    RoundUbiquitous -> "round_ubiquitous"
    RoundEventDriven -> "round_event_driven"
    RoundStateDriven -> "round_state_driven"
    RoundOptional -> "round_optional"
    RoundUnwanted -> "round_unwanted"
    RoundComplex -> "round_complex"
    RoundInversion -> "round_inversion"
    RoundPremortem -> "round_premortem"
    ClarificationNeeded -> "clarification_needed"
    Complete -> "complete"
  }
}

fn action_to_string(action: AIAction) -> String {
  case action {
    AskQuestion -> "ask_question"
    ClarifyAnswer -> "clarify_answer"
    ConfirmUnderstanding -> "confirm_understanding"
    PresentSummary -> "present_summary"
    AwaitApproval -> "await_approval"
    GenerateBeads -> "generate_beads"
    Done -> "done"
  }
}

fn question_to_cue(q: Question) -> String {
  "question: {\n"
  <> "\tid: \"" <> q.id <> "\"\n"
  <> "\ttext: \"" <> escape_cue_string(q.question) <> "\"\n"
  <> "\tcontext: \"" <> escape_cue_string(q.context) <> "\"\n"
  <> "\texample: \"" <> escape_cue_string(q.example) <> "\"\n"
  <> "\trequired: true\n"
  <> "}\n"
}

/// Basic collected output - NO KIRK extension fields (default)
/// Used when --with-analysis is NOT specified
fn collected_to_cue_basic(collected: CollectedRequirements) -> String {
  "collected: {\n"
  <> "\tubiquitous: " <> requirements_list_to_cue(collected.ubiquitous) <> "\n"
  <> "\tevent_driven: " <> requirements_list_to_cue(collected.event_driven) <> "\n"
  <> "\tstate_driven: " <> requirements_list_to_cue(collected.state_driven) <> "\n"
  <> "\toptional: " <> requirements_list_to_cue(collected.optional) <> "\n"
  <> "\tunwanted: " <> requirements_list_to_cue(collected.unwanted) <> "\n"
  <> "\tcomplex: " <> requirements_list_to_cue(collected.complex) <> "\n"
  <> "}\n"
}

/// Full collected output - WITH KIRK extension fields (inversions, premortem)
/// Used when --with-analysis flag is specified
fn collected_to_cue_with_analysis(collected: CollectedRequirements) -> String {
  "collected: {\n"
  <> "\tubiquitous: " <> requirements_list_to_cue(collected.ubiquitous) <> "\n"
  <> "\tevent_driven: " <> requirements_list_to_cue(collected.event_driven) <> "\n"
  <> "\tstate_driven: " <> requirements_list_to_cue(collected.state_driven) <> "\n"
  <> "\toptional: " <> requirements_list_to_cue(collected.optional) <> "\n"
  <> "\tunwanted: " <> requirements_list_to_cue(collected.unwanted) <> "\n"
  <> "\tcomplex: " <> requirements_list_to_cue(collected.complex) <> "\n"
  <> "\tinversions: {\n"
  <> "\t\tsecurity: " <> string_list_to_cue(collected.inversions.security) <> "\n"
  <> "\t\tusability: " <> string_list_to_cue(collected.inversions.usability) <> "\n"
  <> "\t\tintegration: " <> string_list_to_cue(collected.inversions.integration) <> "\n"
  <> "\t}\n"
  <> "\tpremortem: {\n"
  <> "\t\tassumed_failure: \"" <> escape_cue_string(collected.premortem.assumed_failure) <> "\"\n"
  <> "\t\tlikely_causes: []\n"
  <> "\t}\n"
  <> "}\n"
}

fn requirements_list_to_cue(reqs: List(EARSRequirement)) -> String {
  case reqs {
    [] -> "[]"
    _ -> {
      let items = list.map(reqs, fn(r) {
        "{\n"
        <> "\t\tid: \"" <> r.id <> "\"\n"
        <> "\t\tpattern: \"" <> pattern_to_string(r.pattern) <> "\"\n"
        <> "\t\tshall: \"" <> escape_cue_string(r.shall) <> "\"\n"
        <> "\t\traw_text: \"" <> escape_cue_string(r.raw_text) <> "\"\n"
        <> "\t}"
      })
      "[\n\t" <> string.join(items, ",\n\t") <> "\n]"
    }
  }
}

fn string_list_to_cue(items: List(String)) -> String {
  case items {
    [] -> "[]"
    _ -> "[" <> string.join(list.map(items, fn(s) { "\"" <> escape_cue_string(s) <> "\"" }), ", ") <> "]"
  }
}

fn pattern_to_string(pattern: EARSPattern) -> String {
  case pattern {
    Ubiquitous -> "ubiquitous"
    EventDriven -> "event_driven"
    StateDriven -> "state_driven"
    Optional -> "optional"
    Unwanted -> "unwanted"
    Complex -> "complex"
    Inversion -> "inversion"
    Premortem -> "premortem"
  }
}

fn errors_to_cue(errors: List(ValidationError)) -> String {
  case errors {
    [] -> ""
    _ -> {
      let items = list.map(errors, fn(e) {
        "{\n"
        <> "\t\tfield: \"" <> e.field <> "\"\n"
        <> "\t\tmessage: \"" <> escape_cue_string(e.message) <> "\"\n"
        <> "\t\tsuggestion: \"" <> escape_cue_string(e.suggestion) <> "\"\n"
        <> "\t}"
      })
      "errors: [\n\t" <> string.join(items, ",\n\t") <> "\n]\n"
    }
  }
}

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}
