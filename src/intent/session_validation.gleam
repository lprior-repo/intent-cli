/// Session Validation Module (stub for build)
/// TODO: Restore full implementation from .bak file
import gleam/dict
import gleam/list
import gleam/string
import intent/interview.{type Answer, type InterviewSession, type InterviewStage}

pub type SessionError {
  EmptyAnswers(session_id: String)
  InvalidAnswer(session_id: String, question_id: String, reason: String)
  InvalidStageForExport(session_id: String, stage: String)
  TemplateDataDetected(session_id: String, field: String, value: String)
}

pub fn contains_template_marker(text: String) -> Bool {
  let upper = string.uppercase(text)
  string.contains(upper, "TODO")
  || string.contains(upper, "FIXME")
  || string.contains(upper, "PLACEHOLDER")
  || string.contains(upper, "TBD")
}

pub fn is_empty_response(text: String) -> Bool {
  let trimmed = string.trim(text)
  let upper = string.uppercase(trimmed)
  string.length(trimmed) == 0 || trimmed == "N/A" || trimmed == "null"
}

pub fn validate_answer(
  session_id: String,
  answer: Answer,
) -> Result(Nil, SessionError) {
  case is_empty_response(answer.response) {
    True ->
      Error(InvalidAnswer(
        session_id,
        answer.question_id,
        "Empty or meaningless response",
      ))
    False -> {
      case contains_template_marker(answer.response) {
        True ->
          Error(InvalidAnswer(
            session_id,
            answer.question_id,
            "Template/placeholder detected in response",
          ))
        False -> Ok(Nil)
      }
    }
  }
}

pub fn validate_metadata(
  session: InterviewSession,
) -> Result(Nil, List(SessionError)) {
  let errors = []
  let errors = case string.length(string.trim(session.id)) == 0 {
    True ->
      list.append(errors, [InvalidAnswer(session.id, "id", "Empty session ID")])
    False -> errors
  }
  case list.length(errors) {
    0 -> Ok(Nil)
    _ -> Error(errors)
  }
}

pub fn is_exportable_stage(session: InterviewSession) -> Bool {
  case session.stage {
    interview.Complete | interview.Refinement -> True
    _ -> False
  }
}

pub fn validate_session_for_export(
  session: InterviewSession,
) -> Result(InterviewSession, List(SessionError)) {
  let errors = []
  let errors = case list.length(session.answers) == 0 {
    True -> list.append(errors, [EmptyAnswers(session.id)])
    False -> errors
  }
  let errors = case is_exportable_stage(session) {
    True -> errors
    False ->
      list.append(errors, [
        InvalidStageForExport(session.id, stage_to_string(session.stage)),
      ])
  }
  case list.length(errors) {
    0 -> Ok(session)
    _ -> Error(errors)
  }
}

fn stage_to_string(stage: InterviewStage) -> String {
  case stage {
    interview.Discovery -> "discovery"
    interview.Refinement -> "refinement"
    interview.Validation -> "validation"
    interview.Complete -> "complete"
    interview.Paused -> "paused"
  }
}

pub fn format_validation_error(error: SessionError) -> String {
  case error {
    EmptyAnswers(id) -> "Session " <> id <> " has no answers"
    InvalidAnswer(id, qid, reason) ->
      "Session " <> id <> ", question " <> qid <> ": " <> reason
    InvalidStageForExport(id, stage) ->
      "Session " <> id <> " has invalid stage for export: " <> stage
    TemplateDataDetected(id, field, value) ->
      "Session "
      <> id
      <> ", field "
      <> field
      <> " contains template data: "
      <> value
  }
}

pub fn format_validation_errors(errors: List(SessionError)) -> String {
  let count = list.length(errors)
  let header = "Validation failed: " <> string.inspect(count) <> " error(s)\n"
  let details = list.map(errors, format_validation_error)
  string.join([header, ..details], "\n  - ")
}
