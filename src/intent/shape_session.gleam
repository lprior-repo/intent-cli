/// Shape Session Management
/// State management for Shape phase (Phase 2 of INTENT_4_PLAN.md)
/// Follows Functional Core pattern - all functions are pure
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import intent/planning_types.{type FeatureShape, FeatureShape, MVPSlice, type ShapeSection, ShapeSection}

/// Shape session status
pub type ShapeStatus {
  InProgress
  ReadyForCritique
  Complete
}

/// A single answer in a shape session
pub type ShapeAnswer {
  ShapeAnswer(
    question_id: String,
    response: String,
    extracted: Dict(String, String),
    timestamp: String,
  )
}

/// A gap in the shape (missing information)
pub type ShapeGap {
  ShapeGap(field: String, description: String, blocking: Bool)
}

/// A conflict between answers
pub type ShapeConflict {
  ShapeConflict(between: #(String, String), description: String)
}

/// Shape session - persistent state for shape phase
pub type ShapeSession {
  ShapeSession(
    id: String,
    profile: String,
    created_at: String,
    updated_at: String,
    status: ShapeStatus,
    answers: List(ShapeAnswer),
    gaps: List(ShapeGap),
    conflicts: List(ShapeConflict),
  )
}

/// Create a new shape session
pub fn create_session(
  id: String,
  profile: String,
  created_at: String,
) -> ShapeSession {
  ShapeSession(
    id: id,
    profile: profile,
    created_at: created_at,
    updated_at: created_at,
    status: InProgress,
    answers: [],
    gaps: [],
    conflicts: [],
  )
}

/// Record an answer in the session
pub fn record_answer(
  session: ShapeSession,
  question_id: String,
  response: String,
  extracted: Dict(String, String),
  timestamp: String,
) -> ShapeSession {
  let answer =
    ShapeAnswer(
      question_id: question_id,
      response: response,
      extracted: extracted,
      timestamp: timestamp,
    )

  ShapeSession(
    ..session,
    answers: list.append(session.answers, [answer]),
    updated_at: timestamp,
  )
}

/// Find an answer by question ID
pub fn find_answer(
  session: ShapeSession,
  question_id: String,
) -> Option(ShapeAnswer) {
  session.answers
  |> list.find(fn(a) { a.question_id == question_id })
  |> option.from_result
}

/// Get count of answered questions
pub fn get_answered_count(session: ShapeSession) -> Int {
  list.length(session.answers)
}

/// Build a ShapeSection from session answers
/// Returns Error if required fields are missing
pub fn build_shape_section(
  session: ShapeSession,
) -> Result(ShapeSection, String) {
  // Extract required fields
  let features = get_extracted_field(session, "features")
  let critical_path = get_extracted_field(session, "critical_path")
  let mvp_description = get_extracted_field(session, "mvp_description")
  let shortcuts = get_extracted_field(session, "shortcuts")
  let post_mvp = get_extracted_field(session, "post_mvp")
  let validation_moment = get_extracted_field(session, "validation_moment")

  // Validate required fields
  use features_str <- result.try(result.replace_error(
    features,
    "Missing required field: features",
  ))
  use critical_path_str <- result.try(result.replace_error(
    critical_path,
    "Missing required field: critical_path",
  ))
  use mvp_description <- result.try(result.replace_error(
    mvp_description,
    "Missing required field: mvp_description",
  ))
  use shortcuts_str <- result.try(result.replace_error(
    shortcuts,
    "Missing required field: shortcuts",
  ))
  use post_mvp_str <- result.try(result.replace_error(
    post_mvp,
    "Missing required field: post_mvp",
  ))
  use validation_moment <- result.try(result.replace_error(
    validation_moment,
    "Missing required field: validation_moment",
  ))

  // Parse lists and features
  let feature_list = parse_features(features_str)
  let critical_path_list = parse_list(critical_path_str)
  let shortcuts_list = parse_list(shortcuts_str)
  let post_mvp_list = parse_list(post_mvp_str)

  Ok(ShapeSection(
    features: feature_list,
    critical_path: critical_path_list,
    mvp_slice: MVPSlice(
      description: mvp_description,
      features: feature_list |> list.map(fn(f) { f.name }),
      shortcuts: shortcuts_list,
    ),
    post_mvp: post_mvp_list,
    validation_moment: validation_moment,
  ))
}

/// Get an extracted field value from session answers
fn get_extracted_field(
  session: ShapeSession,
  field_name: String,
) -> Result(String, Nil) {
  session.answers
  |> list.find_map(fn(answer) {
    answer.extracted
    |> dict.get(field_name)
    |> result.map(fn(value) { string.trim(value) })
  })
}

/// Parse a comma or newline separated list
fn parse_list(text: String) -> List(String) {
  text
  |> string.split("\n")
  |> list.flat_map(fn(line) { string.split(line, ",") })
  |> list.map(string.trim)
  |> list.filter(fn(s) { !string.is_empty(s) })
}

/// Parse features from text
/// Simplified version - extracts name and uses it as description
fn parse_features(text: String) -> List(FeatureShape) {
  parse_list(text)
  |> list.map(fn(name) { FeatureShape(name: name, description: name) })
}
