//// EARS Interview Module
////
//// Provides a systematic interview workflow using the 6 EARS patterns
//// (Easy Approach to Requirements Syntax) to capture requirements.
////
//// This module guides users through articulating requirements using:
//// 1. Ubiquitous - Always-true behaviors
//// 2. Event-Driven - Triggered behaviors
//// 3. State-Driven - State-dependent behaviors
//// 4. Optional - Feature-flagged behaviors
//// 5. Unwanted - Security/safety constraints
//// 6. Complex - Combined patterns

import gleam/list
import gleam/result
import gleam/string
import intent/kirk/ears_parser

/// EARS interview session tracking progress through the 6 patterns
pub type EarsSession {
  EarsSession(
    session_id: String,
    current_pattern: EarsPattern,
    completed_patterns: List(EarsPattern),
    requirements: List(ears_parser.EarsRequirement),
    notes: String,
  )
}

/// The 6 EARS patterns in interview order
pub type EarsPattern {
  /// Pattern 1: "THE SYSTEM SHALL [behavior]"
  Ubiquitous
  /// Pattern 2: "WHEN [trigger] THE SYSTEM SHALL [behavior]"
  EventDriven
  /// Pattern 3: "WHILE [state] THE SYSTEM SHALL [behavior]"
  StateDriven
  /// Pattern 4: "WHERE [condition] THE SYSTEM SHALL [behavior]"
  Optional
  /// Pattern 5: "IF [condition] THEN THE SYSTEM SHALL NOT [behavior]"
  Unwanted
  /// Pattern 6: Combinations of patterns
  Complex
}

/// Interview question for each EARS pattern
pub type EarsQuestion {
  EarsQuestion(
    pattern: EarsPattern,
    question: String,
    example: String,
    template: String,
    why: String,
  )
}

/// Result of processing an EARS requirement
pub type ProcessResult {
  ProcessResult(
    requirement: ears_parser.EarsRequirement,
    validation_notes: List(String),
    suggestions: List(String),
  )
}

/// Error types for EARS interview operations
pub type EarsInterviewError {
  InvalidRequirement(reason: String)
  PatternMismatch(expected: EarsPattern, got: String)
  ParsingFailed(errors: List(ears_parser.EarsError))
}

/// Create a new EARS interview session
pub fn new_session(session_id: String) -> EarsSession {
  EarsSession(
    session_id: session_id,
    current_pattern: Ubiquitous,
    completed_patterns: [],
    requirements: [],
    notes: "",
  )
}

/// Get the question for the current pattern
pub fn get_current_question(session: EarsSession) -> EarsQuestion {
  get_question_for_pattern(session.current_pattern)
}

/// Get question for a specific EARS pattern
pub fn get_question_for_pattern(pattern: EarsPattern) -> EarsQuestion {
  case pattern {
    Ubiquitous ->
      EarsQuestion(
        pattern: Ubiquitous,
        question: "What must the system ALWAYS do? Describe core, unchanging behaviors.",
        example: "THE SYSTEM SHALL authenticate users before granting access",
        template: "THE SYSTEM SHALL [behavior]",
        why: "Ubiquitous requirements define the foundation - behaviors that are always active regardless of state or events.",
      )

    EventDriven ->
      EarsQuestion(
        pattern: EventDriven,
        question: "What should the system do WHEN specific events occur? Think about triggers.",
        example: "WHEN a user submits invalid credentials THE SYSTEM SHALL return a 401 error",
        template: "WHEN [trigger] THE SYSTEM SHALL [behavior]",
        why: "Event-driven requirements capture cause-and-effect relationships - what happens in response to specific events.",
      )

    StateDriven ->
      EarsQuestion(
        pattern: StateDriven,
        question: "What should the system do WHILE in a specific state? Think about state-dependent behaviors.",
        example: "WHILE processing a payment THE SYSTEM SHALL prevent duplicate submissions",
        template: "WHILE [state] THE SYSTEM SHALL [behavior]",
        why: "State-driven requirements define behaviors that only apply during specific system states.",
      )

    Optional ->
      EarsQuestion(
        pattern: Optional,
        question: "What optional features or conditional behaviors exist? Think about feature flags or configurations.",
        example: "WHERE the user has premium status THE SYSTEM SHALL enable advanced analytics",
        template: "WHERE [condition] THE SYSTEM SHALL [behavior]",
        why: "Optional requirements define behaviors that depend on configuration, feature flags, or user attributes.",
      )

    Unwanted ->
      EarsQuestion(
        pattern: Unwanted,
        question: "What must the system NEVER do? Think about security, safety, and constraints.",
        example: "IF the request lacks authentication THE SYSTEM SHALL NOT expose sensitive data",
        template: "IF [condition] THEN THE SYSTEM SHALL NOT [behavior]",
        why: "Unwanted requirements define critical constraints - behaviors that must be prevented for security or safety.",
      )

    Complex ->
      EarsQuestion(
        pattern: Complex,
        question: "Are there any complex requirements combining state AND events? Think about intricate scenarios.",
        example: "WHILE processing a transaction WHEN an error occurs THE SYSTEM SHALL rollback all changes",
        template: "WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]",
        why: "Complex requirements combine patterns to handle sophisticated scenarios with multiple conditions.",
      )
  }
}

/// Process a requirement response from the user
pub fn process_response(
  session: EarsSession,
  response: String,
) -> Result(ProcessResult, EarsInterviewError) {
  // Parse the requirement using EARS parser
  let parse_result = ears_parser.parse(response)

  // Check for parsing errors
  case parse_result.errors {
    [] -> {
      // Get the first requirement (should only be one from single response)
      case parse_result.requirements {
        [requirement, ..] -> {
          // Validate it matches the expected pattern
          use validated <- result.try(validate_pattern(
            session.current_pattern,
            requirement,
          ))

          // Generate validation notes and suggestions
          let validation_notes = generate_validation_notes(requirement)
          let suggestions = generate_suggestions(requirement)

          Ok(ProcessResult(
            requirement: validated,
            validation_notes: validation_notes,
            suggestions: suggestions,
          ))
        }
        [] ->
          Error(InvalidRequirement("No valid requirement found in response"))
      }
    }
    errors -> Error(ParsingFailed(errors))
  }
}

/// Validate that a requirement matches the expected pattern
fn validate_pattern(
  expected: EarsPattern,
  requirement: ears_parser.EarsRequirement,
) -> Result(ears_parser.EarsRequirement, EarsInterviewError) {
  let matches = case expected, requirement.pattern {
    Ubiquitous, ears_parser.Ubiquitous -> True
    EventDriven, ears_parser.EventDriven -> True
    StateDriven, ears_parser.StateDriven -> True
    Optional, ears_parser.Optional -> True
    Unwanted, ears_parser.Unwanted -> True
    Complex, ears_parser.Complex -> True
    _, _ -> False
  }

  case matches {
    True -> Ok(requirement)
    False -> Error(PatternMismatch(expected, requirement.raw_text))
  }
}

/// Generate validation notes for a requirement
fn generate_validation_notes(
  requirement: ears_parser.EarsRequirement,
) -> List(String) {
  let notes = []

  // Check if system_shall is specific enough
  let notes = case string.length(requirement.system_shall) < 10 {
    True -> ["Consider making the behavior more specific and detailed", ..notes]
    False -> notes
  }

  // Check for vague words
  let vague_words = ["good", "bad", "appropriate", "reasonable", "suitable"]
  let has_vague =
    list.any(vague_words, fn(word) {
      string.contains(string.lowercase(requirement.system_shall), word)
    })

  let notes = case has_vague {
    True -> [
      "Avoid vague terms like 'good', 'reasonable', 'appropriate' - be specific",
      ..notes
    ]
    False -> notes
  }

  list.reverse(notes)
}

/// Generate suggestions for improving a requirement
fn generate_suggestions(
  requirement: ears_parser.EarsRequirement,
) -> List(String) {
  let suggestions = []

  // Suggest adding testability criteria
  let has_measurable = {
    let lower = string.lowercase(requirement.system_shall)
    string.contains(lower, "within")
    || string.contains(lower, "before")
    || string.contains(lower, "after")
    || string.contains(lower, "less than")
    || string.contains(lower, "more than")
  }

  let suggestions = case has_measurable {
    False -> [
      "Consider adding measurable criteria (timeframes, counts, thresholds)",
      ..suggestions
    ]
    True -> suggestions
  }

  list.reverse(suggestions)
}

/// Advance to the next pattern in the interview
pub fn advance_pattern(session: EarsSession) -> EarsSession {
  let next_pattern = case session.current_pattern {
    Ubiquitous -> EventDriven
    EventDriven -> StateDriven
    StateDriven -> Optional
    Optional -> Unwanted
    Unwanted -> Complex
    Complex -> Complex
    // Stay on Complex (interview complete)
  }

  EarsSession(..session, current_pattern: next_pattern, completed_patterns: [
    session.current_pattern,
    ..session.completed_patterns
  ])
}

/// Check if the interview is complete
pub fn is_complete(session: EarsSession) -> Bool {
  list.length(session.completed_patterns) >= 6
}

/// Add a requirement to the session
pub fn add_requirement(
  session: EarsSession,
  requirement: ears_parser.EarsRequirement,
) -> EarsSession {
  EarsSession(..session, requirements: [requirement, ..session.requirements])
}

/// Get all requirements from the session
pub fn get_requirements(
  session: EarsSession,
) -> List(ears_parser.EarsRequirement) {
  list.reverse(session.requirements)
}

/// Get requirements by pattern
pub fn get_requirements_by_pattern(
  session: EarsSession,
  pattern: ears_parser.EarsPattern,
) -> List(ears_parser.EarsRequirement) {
  session.requirements
  |> list.filter(fn(req) { req.pattern == pattern })
  |> list.reverse
}

/// Get interview progress summary
pub fn get_progress_summary(session: EarsSession) -> String {
  let total_patterns = 6
  let completed = list.length(session.completed_patterns)
  let total_reqs = list.length(session.requirements)

  "Progress: "
  <> string.inspect(completed)
  <> "/"
  <> string.inspect(total_patterns)
  <> " patterns completed, "
  <> string.inspect(total_reqs)
  <> " requirements captured"
}
