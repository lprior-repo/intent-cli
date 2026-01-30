/// Session Validation Module
/// Validates interview sessions to detect corruption before export
/// Contract: Fail fast with detailed error messages for any invalid data
///
/// This module prevents silent corruption by:
/// 1. Validating session structure and required fields
/// 2. Detecting template/placeholder data that indicates corruption
/// 3. Checking for empty or missing critical data
/// 4. Validating answer quality and completeness
///
/// Bug fix: intent-cli-pn1w - Export command must fail on corrupted data
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import intent/interview.{type Answer, type InterviewSession}

// =============================================================================
// Validation Error Types (Exhaustive)
// =============================================================================

/// Validation error with context about what failed and why
pub type ValidationError {
  /// Session has no answers (empty or corrupted)
  EmptyAnswers(session_id: String)
  /// Session has placeholder/template responses indicating corruption
  TemplateDataDetected(session_id: String, field: String, example: String)
  /// Session missing required fields
  MissingRequiredField(session_id: String, field: String)
  /// Answer has invalid/corrupted data
  InvalidAnswer(session_id: String, question_id: String, reason: String)
  /// Session metadata is corrupted
  CorruptedMetadata(session_id: String, field: String, reason: String)
  /// Session stage is invalid for export
  InvalidStageForExport(session_id: String, stage: String)
}

/// Result of validation - either valid session or list of errors
pub type ValidationResult =
  Result(InterviewSession, List(ValidationError))

// =============================================================================
// Template/Placeholder Detection
// =============================================================================

/// Template keywords that indicate placeholder data (not real answers)
const template_keywords = [
  "TODO:", "FIXME:", "XXX:", "PLACEHOLDER", "TBD", "To be determined", "Fill in",
  "Add your", "Replace with", "Example:", "Sample:", "Not specified",
  "Undefined", "Unknown", "<insert", ">insert",
]

/// Empty/meaningless responses that indicate corruption
const empty_responses = [
  "", "N/A", "n/a", "None", "none", "null", "NULL", "-", "--", "...", "???",
]

/// Check if a string contains template/placeholder text
pub fn contains_template_marker(text: String) -> Bool {
  let lowercased = string.lowercase(text)
  list.any(template_keywords, fn(keyword) {
    string.contains(lowercased, string.lowercase(keyword))
  })
}

/// Check if a response is empty or meaningless
pub fn is_empty_response(response: String) -> Bool {
  let trimmed = string.trim(response)
  list.any(empty_responses, fn(empty) {
    string.lowercase(trimmed) == string.lowercase(empty)
  })
}

// =============================================================================
// Answer Validation
// =============================================================================

/// Validate a single answer
pub fn validate_answer(
  session_id: String,
  answer: Answer,
) -> Result(Answer, ValidationError) {
  // Check for empty response
  case is_empty_response(answer.response) {
    True ->
      Error(InvalidAnswer(
        session_id,
        answer.question_id,
        "Empty or meaningless response: '" <> answer.response <> "'",
      ))
    False -> {
      // Check for template markers
      case contains_template_marker(answer.response) {
        True ->
          Error(InvalidAnswer(
            session_id,
            answer.question_id,
            "Template/placeholder detected: '"
              <> string.slice(answer.response, 0, 50)
              <> "...'",
          ))
        False -> {
          // Check confidence level (should be positive)
          case answer.confidence >=. 0.0 {
            True -> Ok(answer)
            False ->
              Error(InvalidAnswer(
                session_id,
                answer.question_id,
                "Invalid confidence: "
                  <> float.to_string(answer.confidence)
                  <> " (must be >= 0.0)",
              ))
          }
        }
      }
    }
  }
}

/// Validate all answers in a session
pub fn validate_answers(
  session: InterviewSession,
) -> Result(List(Answer), List(ValidationError)) {
  case list.length(session.answers) {
    0 -> Error([EmptyAnswers(session.id)])
    _ -> {
      let results =
        list.map(session.answers, fn(answer) {
          validate_answer(session.id, answer)
        })

      // Collect all errors
      let errors =
        list.filter_map(results, fn(result) {
          case result {
            Error(err) -> Ok(err)
            Ok(_) -> Error(Nil)
          }
        })

      case list.length(errors) {
        0 -> Ok(session.answers)
        _ -> Error(errors)
      }
    }
  }
}

// =============================================================================
// Session Metadata Validation
// =============================================================================

/// Validate session ID format
pub fn validate_session_id(session_id: String) -> Bool {
  let trimmed = string.trim(session_id)
  string.length(trimmed) > 0
}

/// Validate timestamp format (ISO 8601 or reasonable format)
pub fn validate_timestamp(timestamp: String) -> Bool {
  let trimmed = string.trim(timestamp)
  // Basic check: should have date-like structure
  string.length(trimmed) >= 10 && string.contains(trimmed, "-")
}

/// Validate session metadata (ID, timestamps, etc.)
pub fn validate_metadata(
  session: InterviewSession,
) -> Result(Nil, List(ValidationError)) {
  let errors = []

  // Validate session ID
  let errors = case validate_session_id(session.id) {
    True -> errors
    False ->
      list.append(errors, [
        CorruptedMetadata(session.id, "id", "Invalid or empty session ID"),
      ])
  }

  // Validate created_at timestamp
  let errors = case validate_timestamp(session.created_at) {
    True -> errors
    False ->
      list.append(errors, [
        CorruptedMetadata(
          session.id,
          "created_at",
          "Invalid timestamp: '" <> session.created_at <> "'",
        ),
      ])
  }

  // Validate updated_at timestamp
  let errors = case validate_timestamp(session.updated_at) {
    True -> errors
    False ->
      list.append(errors, [
        CorruptedMetadata(
          session.id,
          "updated_at",
          "Invalid timestamp: '" <> session.updated_at <> "'",
        ),
      ])
  }

  // Validate rounds_completed (should be non-negative)
  let errors = case session.rounds_completed >= 0 {
    True -> errors
    False ->
      list.append(errors, [
        CorruptedMetadata(
          session.id,
          "rounds_completed",
          "Invalid rounds_completed: "
            <> int.to_string(session.rounds_completed)
            <> " (must be >= 0)",
        ),
      ])
  }

  case list.length(errors) {
    0 -> Ok(Nil)
    _ -> Error(errors)
  }
}

// =============================================================================
// Stage Validation
// =============================================================================

/// Check if session stage is valid for export
pub fn is_exportable_stage(session: InterviewSession) -> Bool {
  case session.stage {
    interview.Complete -> True
    interview.Validation -> True
    interview.Refinement -> True
    // Discovery stage is too early, Paused is ambiguous
    interview.Discovery -> False
    interview.Paused -> False
  }
}

/// Validate session stage for export
pub fn validate_stage(session: InterviewSession) -> Result(Nil, ValidationError) {
  case is_exportable_stage(session) {
    True -> Ok(Nil)
    False -> {
      let stage_str = case session.stage {
        interview.Discovery -> "discovery"
        interview.Refinement -> "refinement"
        interview.Validation -> "validation"
        interview.Complete -> "complete"
        interview.Paused -> "paused"
      }
      Error(InvalidStageForExport(
        session.id,
        "Session in '" <> stage_str <> "' stage is not ready for export",
      ))
    }
  }
}

// =============================================================================
// Complete Session Validation
// =============================================================================

/// Validate an entire session before export
/// Returns the session if valid, or a list of all validation errors
pub fn validate_session_for_export(
  session: InterviewSession,
) -> Result(InterviewSession, List(ValidationError)) {
  let all_errors = []

  // Validate metadata
  let all_errors = case validate_metadata(session) {
    Ok(_) -> all_errors
    Error(errors) -> list.append(all_errors, errors)
  }

  // Validate stage
  let all_errors = case validate_stage(session) {
    Ok(_) -> all_errors
    Error(err) -> list.append(all_errors, [err])
  }

  // Validate answers
  let all_errors = case validate_answers(session) {
    Ok(_) -> all_errors
    Error(errors) -> list.append(all_errors, errors)
  }

  case list.length(all_errors) {
    0 -> Ok(session)
    _ -> Error(all_errors)
  }
}

// =============================================================================
// Error Formatting
// =============================================================================

/// Format a validation error as a human-readable string
pub fn format_validation_error(error: ValidationError) -> String {
  case error {
    EmptyAnswers(session_id) ->
      "Session '"
      <> session_id
      <> "' has no answers (corrupted or incomplete data)"

    TemplateDataDetected(session_id, field, example) ->
      "Session '"
      <> session_id
      <> "' contains template/placeholder data in "
      <> field
      <> ": "
      <> example

    MissingRequiredField(session_id, field) ->
      "Session '" <> session_id <> "' missing required field: " <> field

    InvalidAnswer(session_id, question_id, reason) ->
      "Session '"
      <> session_id
      <> "', question '"
      <> question_id
      <> "': "
      <> reason

    CorruptedMetadata(session_id, field, reason) ->
      "Session '"
      <> session_id
      <> "' has corrupted metadata in "
      <> field
      <> ": "
      <> reason

    InvalidStageForExport(session_id, stage) ->
      "Session '" <> session_id <> "': " <> stage
  }
}

/// Format multiple validation errors as a detailed report
pub fn format_validation_errors(errors: List(ValidationError)) -> String {
  let count = list.length(errors)
  let header =
    "VALIDATION FAILED: Found "
    <> int.to_string(count)
    <> " error(s) in session data:\n\n"

  let formatted =
    list.map(errors, fn(err) { "  • " <> format_validation_error(err) })
    |> string.join("\n")

  header <> formatted
}
