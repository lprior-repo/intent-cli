/// Vision Session Management
/// State management for Vision phase (Phase 1 of INTENT_4_PLAN.md)
/// Follows Functional Core pattern - all functions are pure
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import intent/vision_types.{
  type Scenario, type VisionSection, Scenario, VisionSection,
}

/// Vision session status
pub type VisionStatus {
  InProgress
  ReadyForCritique
  Complete
}

/// A single answer in a vision session
pub type VisionAnswer {
  VisionAnswer(
    question_id: String,
    response: String,
    extracted: Dict(String, String),
    timestamp: String,
  )
}

/// A gap in the vision (missing information)
pub type VisionGap {
  VisionGap(field: String, description: String, blocking: Bool)
}

/// A conflict between answers
pub type VisionConflict {
  VisionConflict(between: #(String, String), description: String)
}

/// Vision session - persistent state for vision phase
pub type VisionSession {
  VisionSession(
    id: String,
    profile: String,
    created_at: String,
    updated_at: String,
    status: VisionStatus,
    answers: List(VisionAnswer),
    gaps: List(VisionGap),
    conflicts: List(VisionConflict),
  )
}

/// Create a new vision session
pub fn create_session(
  id: String,
  profile: String,
  created_at: String,
) -> VisionSession {
  VisionSession(
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
  session: VisionSession,
  question_id: String,
  response: String,
  extracted: Dict(String, String),
  timestamp: String,
) -> VisionSession {
  let answer =
    VisionAnswer(
      question_id: question_id,
      response: response,
      extracted: extracted,
      timestamp: timestamp,
    )

  VisionSession(
    ..session,
    answers: list.append(session.answers, [answer]),
    updated_at: timestamp,
  )
}

/// Find an answer by question ID
pub fn find_answer(
  session: VisionSession,
  question_id: String,
) -> Option(VisionAnswer) {
  session.answers
  |> list.find(fn(a) { a.question_id == question_id })
  |> option.from_result
}

/// Get count of answered questions
pub fn get_answered_count(session: VisionSession) -> Int {
  list.length(session.answers)
}

/// Build a VisionSection from session answers
/// Returns Error if required fields are missing
pub fn build_vision_section(
  session: VisionSession,
) -> Result(VisionSection, String) {
  // Extract required fields
  let press_release = get_extracted_field(session, "press_release")
  let persona = get_extracted_field(session, "persona")
  let non_personas_str = get_extracted_field(session, "non_personas")
  let north_star = get_extracted_field(session, "north_star")
  let replaces = get_extracted_field(session, "replaces")
  let vorp = get_extracted_field(session, "vorp")
  let scenarios_str = get_extracted_field(session, "scenarios")
  let out_of_scope_str = get_extracted_field(session, "out_of_scope")

  // Validate required fields
  use press_release <- result.try(result.replace_error(
    press_release,
    "Missing required field: press_release",
  ))
  use persona <- result.try(result.replace_error(
    persona,
    "Missing required field: persona",
  ))
  use non_personas_str <- result.try(result.replace_error(
    non_personas_str,
    "Missing required field: non_personas",
  ))
  use north_star <- result.try(result.replace_error(
    north_star,
    "Missing required field: north_star",
  ))
  use vorp <- result.try(result.replace_error(
    vorp,
    "Missing required field: vorp",
  ))
  use scenarios_str <- result.try(result.replace_error(
    scenarios_str,
    "Missing required field: scenarios",
  ))
  use out_of_scope_str <- result.try(result.replace_error(
    out_of_scope_str,
    "Missing required field: out_of_scope",
  ))

  // Parse lists (split by comma or newline)
  let non_personas = parse_list(non_personas_str)
  let out_of_scope = parse_list(out_of_scope_str)

  // Parse scenarios (simplified - just create one scenario from text for now)
  let scenarios = parse_scenarios(scenarios_str)

  Ok(VisionSection(
    press_release: press_release,
    persona: persona,
    non_personas: non_personas,
    north_star: north_star,
    scenarios: scenarios,
    replaces: option.from_result(replaces),
    vorp: vorp,
    out_of_scope: out_of_scope,
  ))
}

/// Get an extracted field value from session answers
fn get_extracted_field(
  session: VisionSession,
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

/// Parse scenarios from text
/// Simplified version - creates one scenario from the text
fn parse_scenarios(text: String) -> List(Scenario) {
  // For MVP, just create a basic scenario
  // TODO: Implement proper scenario parsing
  [
    Scenario(
      character: "User",
      persona: "Primary persona",
      motivation: "Achieve goal",
      simulation: text,
      outcome: "Success",
    ),
  ]
}
