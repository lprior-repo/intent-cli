//// Execution Plan Display for Intent CLI
////
//// Reads beads from .intent/session-{id}.cue, builds dependency graph,
//// outputs execution phases in human-readable or JSON format.
////
//// Implements #ExecutionPlan from schema/beads.cue:
//// - Groups beads into dependency-ordered phases
//// - Calculates total effort and risk assessment
//// - Supports both human (ASCII tree) and JSON output

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import intent/interview.{type InterviewSession}
import simplifile

// =============================================================================
// TYPES - Matching #ExecutionPlan from schema/beads.cue
// =============================================================================

/// Execution plan computed from session beads
pub type ExecutionPlan {
  ExecutionPlan(
    session_id: String,
    generated_at: String,
    phases: List(ExecutionPhase),
    total_beads: Int,
    total_effort: String,
    risk: RiskLevel,
    blockers: List(String),
    rcs_score: Float,
  )
}

/// A phase of execution containing beads that can run in parallel
pub type ExecutionPhase {
  ExecutionPhase(
    phase_number: Int,
    title: String,
    beads: List(String),
    can_parallel: Bool,
    effort: String,
  )
}

/// Risk level assessment
pub type RiskLevel {
  Low
  Medium
  High
  Critical
}

/// Bead effort estimate
pub type Effort {
  Effort5min
  Effort10min
  Effort15min
  Effort20min
  Effort30min
}

/// Bead status
pub type BeadStatus {
  Pending
  InProgress
  Blocked
  Completed
  Failed
}

/// Minimal bead representation for planning
pub type PlanBead {
  PlanBead(
    id: String,
    title: String,
    requires: List(String),
    effort: Effort,
    status: BeadStatus,
  )
}

pub type PlanError {
  SessionNotFound(session_id: String)
  ParseError(message: String)
  CyclicDependency(beads: List(String))
  MissingDependency(bead: String, missing: String)
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Load beads from session file and compute execution plan
/// This is a convenience wrapper that reads the file and delegates to compute_plan_from_content
pub fn compute_plan(session_id: String) -> Result(ExecutionPlan, PlanError) {
  compute_plan_from_file(session_id)
}

/// Load beads from session file and compute execution plan
/// Handles file I/O, then delegates to pure compute_plan_from_content
pub fn compute_plan_from_file(
  session_id: String,
) -> Result(ExecutionPlan, PlanError) {
  let session_path = ".intent/session-" <> session_id <> ".cue"

  case simplifile.read(session_path) {
    Error(_) -> Error(SessionNotFound(session_id))
    Ok(content) -> compute_plan_from_content(session_id, content, 0)
  }
}

/// Compute execution plan from session content (pure function, no file I/O)
/// This is the core computation logic, testable without filesystem dependencies
pub fn compute_plan_from_content(
  session_id: String,
  content: String,
  rounds_completed: Int,
) -> Result(ExecutionPlan, PlanError) {
  use beads <- result.try(parse_beads_from_cue(content))
  use phases <- result.try(detect_dependency_graph(beads))

  let total_effort = calculate_total_effort(beads)
  let risk = assess_risk(beads, phases)
  let blockers = find_blockers(beads)
  let timestamp = current_iso8601_timestamp()
  let rcs_score = calculate_rcs(rounds_completed)

  Ok(ExecutionPlan(
    session_id: session_id,
    generated_at: timestamp,
    phases: phases,
    total_beads: list.length(beads),
    total_effort: total_effort,
    risk: risk,
    blockers: blockers,
    rcs_score: rcs_score,
  ))
}

/// Build execution phases from bead dependencies using topological sort
pub fn detect_dependency_graph(
  beads: List(PlanBead),
) -> Result(List(ExecutionPhase), PlanError) {
  // Build a map from id to bead
  let by_id =
    beads
    |> list.map(fn(b) { #(b.id, b) })
    |> dict.from_list

  // Check all dependencies exist
  use _ <- result.try(validate_dependencies(beads, by_id))

  // Group beads into phases based on dependency levels
  build_phases(beads, by_id)
}

/// Return waves of bead IDs grouped by dependency depth
/// Each wave contains IDs of beads that can run in parallel
pub fn bead_waves(
  beads: List(PlanBead),
) -> Result(List(List(String)), PlanError) {
  // Handle empty list
  case list.is_empty(beads) {
    True -> Ok([])
    False -> {
      // Build a map from id to bead
      let by_id =
        beads
        |> list.map(fn(b) { #(b.id, b) })
        |> dict.from_list

      // Check all dependencies exist
      use _ <- result.try(validate_dependencies(beads, by_id))

      // Calculate dependency depth for each bead
      let depths = calculate_depths(beads, by_id)

      // Check for cycles (depths will be incomplete)
      let max_expected = list.length(beads)
      case dict.size(depths) < max_expected {
        True -> {
          let missing =
            beads
            |> list.filter(fn(b) { !dict.has_key(depths, b.id) })
            |> list.map(fn(b) { b.id })
          Error(CyclicDependency(missing))
        }
        False -> {
          // Group by depth level
          let grouped = group_by_depth(beads, depths)

          // Convert to List(List(String)) sorted by depth
          let waves =
            grouped
            |> dict.to_list
            |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
            |> list.map(fn(pair) {
              let #(_, bead_ids) = pair
              bead_ids
            })

          Ok(waves)
        }
      }
    }
  }
}

/// Format plan as human-readable ASCII tree
pub fn format_plan_human(plan: ExecutionPlan) -> String {
  let header =
    "╔══════════════════════════════════════════════════════════════╗\n"
    <> "║                    EXECUTION PLAN                            ║\n"
    <> "╠══════════════════════════════════════════════════════════════╣\n"
    <> "║ Session: "
    <> pad_right(plan.session_id, 51)
    <> "║\n"
    <> "║ Generated: "
    <> pad_right(plan.generated_at, 49)
    <> "║\n"
    <> "║ Total Beads: "
    <> pad_right(int.to_string(plan.total_beads), 47)
    <> "║\n"
    <> "║ Total Effort: "
    <> pad_right(plan.total_effort, 46)
    <> "║\n"
    <> "║ Risk Level: "
    <> pad_right(risk_to_string(plan.risk), 48)
    <> "║\n"
    <> "║ RCS Score: "
    <> pad_right(format_rcs_score(plan.rcs_score), 49)
    <> "║\n"
    <> "╚══════════════════════════════════════════════════════════════╝\n\n"

  let blockers_section = case list.is_empty(plan.blockers) {
    True -> ""
    False ->
      "⚠ BLOCKERS:\n"
      <> {
        plan.blockers |> list.map(fn(b) { "  • " <> b }) |> string.join("\n")
      }
      <> "\n\n"
  }

  let phases_section =
    plan.phases
    |> list.map(format_phase_human)
    |> string.join("\n")

  header <> blockers_section <> phases_section
}

/// Format plan as JSON (compatible with cue export format)
pub fn format_plan_json(plan: ExecutionPlan) -> String {
  let phases_json =
    plan.phases
    |> list.map(format_phase_json)
    |> string.join(",\n    ")

  let blockers_json =
    plan.blockers
    |> list.map(fn(b) { "\"" <> escape_json_string(b) <> "\"" })
    |> string.join(", ")

  "{\n"
  <> "  \"session_id\": \""
  <> escape_json_string(plan.session_id)
  <> "\",\n"
  <> "  \"generated_at\": \""
  <> plan.generated_at
  <> "\",\n"
  <> "  \"phases\": [\n    "
  <> phases_json
  <> "\n  ],\n"
  <> "  \"total_beads\": "
  <> int.to_string(plan.total_beads)
  <> ",\n"
  <> "  \"total_effort\": \""
  <> plan.total_effort
  <> "\",\n"
  <> "  \"risk\": \""
  <> risk_to_string(plan.risk)
  <> "\",\n"
  <> "  \"blockers\": ["
  <> blockers_json
  <> "],\n"
  <> "  \"rcs_score\": "
  <> float.to_string(plan.rcs_score)
  <> "\n"
  <> "}"
}

/// Format error as human-readable string
pub fn format_error(error: PlanError) -> String {
  case error {
    SessionNotFound(id) -> {
      // Check if user passed a file path instead of session ID
      case string.contains(id, "/") || string.ends_with(id, ".cue") {
        True ->
          "Invalid session ID: "
          <> id
          <> "\n\n"
          <> "The plan command requires a session ID, not a spec file path.\n"
          <> "To see available sessions, run: intent sessions\n"
          <> "Example: intent plan abc123"
        False ->
          "Session not found: "
          <> id
          <> "\n"
          <> "Expected file: .intent/session-"
          <> id
          <> ".cue"
      }
    }
    ParseError(msg) -> "Failed to parse session: " <> msg
    CyclicDependency(beads) ->
      "Cyclic dependency detected involving: " <> string.join(beads, ", ")
    MissingDependency(bead, missing) ->
      "Bead '" <> bead <> "' requires '" <> missing <> "' which does not exist"
  }
}

/// Calculate Round Completion Score (RCS)
/// RCS = (completed_rounds / total_planned_rounds) * 100
/// For now, we assume a standard 5-round interview process.
pub fn round_completion(session: InterviewSession) -> Float {
  let total_rounds = 5.0
  let completed = int.to_float(session.rounds_completed)

  let score = { completed /. total_rounds } *. 100.0

  // Cap at 100%
  case score >. 100.0 {
    True -> 100.0
    False -> score
  }
}

/// Calculate RCS from rounds completed (pure function, no session needed)
pub fn calculate_rcs(rounds_completed: Int) -> Float {
  let total_rounds = 5.0
  let completed = int.to_float(rounds_completed)
  let score = { completed /. total_rounds } *. 100.0
  case score >. 100.0 {
    True -> 100.0
    False -> score
  }
}

// =============================================================================
// PRIVATE: Dependency Analysis
// =============================================================================

fn validate_dependencies(
  beads: List(PlanBead),
  by_id: Dict(String, PlanBead),
) -> Result(Nil, PlanError) {
  list.try_each(beads, fn(bead) {
    list.try_each(bead.requires, fn(dep) {
      case dict.has_key(by_id, dep) {
        True -> Ok(Nil)
        False -> Error(MissingDependency(bead.id, dep))
      }
    })
  })
}

fn build_phases(
  beads: List(PlanBead),
  by_id: Dict(String, PlanBead),
) -> Result(List(ExecutionPhase), PlanError) {
  // Calculate dependency depth for each bead
  let depths = calculate_depths(beads, by_id)

  // Check for cycles (depths will be incomplete)
  let max_expected = list.length(beads)
  case dict.size(depths) < max_expected {
    True -> {
      let missing =
        beads
        |> list.filter(fn(b) { !dict.has_key(depths, b.id) })
        |> list.map(fn(b) { b.id })
      Error(CyclicDependency(missing))
    }
    False -> {
      // Group by depth level
      let grouped = group_by_depth(beads, depths)

      // Convert to phases
      let phases =
        grouped
        |> dict.to_list
        |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
        |> list.index_map(fn(pair, idx) {
          let #(_, bead_ids) = pair
          let phase_beads =
            bead_ids
            |> list.filter_map(fn(id) {
              dict.get(by_id, id) |> result.nil_error
            })

          ExecutionPhase(
            phase_number: idx + 1,
            title: "Phase " <> int.to_string(idx + 1),
            beads: bead_ids,
            can_parallel: list.length(bead_ids) > 1,
            effort: calculate_phase_effort(phase_beads),
          )
        })

      Ok(phases)
    }
  }
}

fn calculate_depths(
  beads: List(PlanBead),
  _by_id: Dict(String, PlanBead),
) -> Dict(String, Int) {
  // Iteratively calculate depths until no changes
  let initial_depths =
    beads
    |> list.filter(fn(b) { list.is_empty(b.requires) })
    |> list.map(fn(b) { #(b.id, 0) })
    |> dict.from_list

  calculate_depths_loop(beads, initial_depths, list.length(beads))
}

fn calculate_depths_loop(
  beads: List(PlanBead),
  depths: Dict(String, Int),
  max_iterations: Int,
) -> Dict(String, Int) {
  case max_iterations <= 0 {
    True -> depths
    False -> {
      // Try to calculate depth for beads without depths
      let new_depths =
        beads
        |> list.fold(depths, fn(acc, bead) {
          case dict.has_key(acc, bead.id) {
            True -> acc
            False -> {
              // Check if all dependencies have depths
              let dep_depths =
                bead.requires
                |> list.filter_map(fn(dep) {
                  dict.get(acc, dep) |> result.nil_error
                })

              case list.length(dep_depths) == list.length(bead.requires) {
                False -> acc
                True -> {
                  let max_dep = case dep_depths {
                    [] -> 0
                    _ -> list.fold(dep_depths, 0, int.max)
                  }
                  dict.insert(acc, bead.id, max_dep + 1)
                }
              }
            }
          }
        })

      // Check if we made progress
      case dict.size(new_depths) == dict.size(depths) {
        True -> depths
        False -> calculate_depths_loop(beads, new_depths, max_iterations - 1)
      }
    }
  }
}

fn group_by_depth(
  beads: List(PlanBead),
  depths: Dict(String, Int),
) -> Dict(Int, List(String)) {
  beads
  |> list.fold(dict.new(), fn(acc, bead) {
    case dict.get(depths, bead.id) {
      Error(_) -> acc
      Ok(depth) -> {
        let existing = dict.get(acc, depth) |> result.unwrap([])
        dict.insert(acc, depth, [bead.id, ..existing])
      }
    }
  })
}

// =============================================================================
// PRIVATE: Effort Calculation
// =============================================================================

fn calculate_total_effort(beads: List(PlanBead)) -> String {
  let minutes =
    beads
    |> list.map(fn(b) { effort_to_minutes(b.effort) })
    |> list.fold(0, int.add)

  format_duration(minutes)
}

fn calculate_phase_effort(beads: List(PlanBead)) -> String {
  let minutes =
    beads
    |> list.map(fn(b) { effort_to_minutes(b.effort) })
    |> list.fold(0, int.add)

  format_duration(minutes)
}

fn effort_to_minutes(effort: Effort) -> Int {
  case effort {
    Effort5min -> 5
    Effort10min -> 10
    Effort15min -> 15
    Effort20min -> 20
    Effort30min -> 30
  }
}

fn format_duration(minutes: Int) -> String {
  let hours = minutes / 60
  let mins = minutes % 60

  case hours, mins {
    0, m -> int.to_string(m) <> "min"
    h, 0 -> int.to_string(h) <> "h"
    h, m -> int.to_string(h) <> "h " <> int.to_string(m) <> "min"
  }
}

// =============================================================================
// PRIVATE: Risk Assessment
// =============================================================================

fn assess_risk(beads: List(PlanBead), phases: List(ExecutionPhase)) -> RiskLevel {
  let total = list.length(beads)
  let blocked_count =
    beads
    |> list.filter(fn(b) { b.status == Blocked || b.status == Failed })
    |> list.length

  let phase_count = list.length(phases)

  // Risk factors:
  // 1. Percentage of blocked/failed beads
  // 2. Number of phases (more phases = more complexity)
  // 3. Total beads (larger scope = more risk)

  let blocked_ratio = case total {
    0 -> 0.0
    _ -> int.to_float(blocked_count) /. int.to_float(total)
  }

  case blocked_ratio, phase_count, total {
    r, _, _ if r >. 0.3 -> Critical
    r, p, _ if r >. 0.1 || p > 10 -> High
    _, p, t if p > 5 || t > 50 -> Medium
    _, _, _ -> Low
  }
}

fn find_blockers(beads: List(PlanBead)) -> List(String) {
  beads
  |> list.filter(fn(b) { b.status == Blocked || b.status == Failed })
  |> list.map(fn(b) { b.id <> ": " <> b.title })
}

// =============================================================================
// PRIVATE: Formatting
// =============================================================================

fn format_rcs_score(score: Float) -> String {
  let rounded = float.round(score) |> int.to_string
  rounded <> "% complete"
}

fn format_phase_human(phase: ExecutionPhase) -> String {
  let parallel_indicator = case phase.can_parallel {
    True -> " [can run in parallel]"
    False -> ""
  }

  let header =
    "┌─ "
    <> phase.title
    <> " ("
    <> phase.effort
    <> ")"
    <> parallel_indicator
    <> "\n"

  let beads_list =
    phase.beads
    |> list.map(fn(id) { "│  • " <> id })
    |> string.join("\n")

  let footer = "└────────────────────────────────────────\n"

  header <> beads_list <> "\n" <> footer
}

fn format_phase_json(phase: ExecutionPhase) -> String {
  let beads_json =
    phase.beads
    |> list.map(fn(id) { "\"" <> id <> "\"" })
    |> string.join(", ")

  "{\n"
  <> "      \"phase_number\": "
  <> int.to_string(phase.phase_number)
  <> ",\n"
  <> "      \"title\": \""
  <> escape_json_string(phase.title)
  <> "\",\n"
  <> "      \"beads\": ["
  <> beads_json
  <> "],\n"
  <> "      \"can_parallel\": "
  <> bool_to_string(phase.can_parallel)
  <> ",\n"
  <> "      \"effort\": \""
  <> phase.effort
  <> "\"\n"
  <> "    }"
}

fn risk_to_string(risk: RiskLevel) -> String {
  case risk {
    Low -> "low"
    Medium -> "medium"
    High -> "high"
    Critical -> "critical"
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn pad_right(s: String, width: Int) -> String {
  let len = string.length(s)
  case len >= width {
    True -> s
    False -> s <> string.repeat(" ", width - len)
  }
}

fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

// =============================================================================
// PRIVATE: JSON Decoding for Beads
// =============================================================================

fn decode_effort(
  data: dynamic.Dynamic,
) -> Result(Effort, List(dynamic.DecodeError)) {
  case dynamic.string(data) {
    Ok("5min") -> Ok(Effort5min)
    Ok("10min") -> Ok(Effort10min)
    Ok("15min") -> Ok(Effort15min)
    Ok("20min") -> Ok(Effort20min)
    Ok("30min") -> Ok(Effort30min)
    Ok(other) ->
      Error([
        dynamic.DecodeError(
          expected: "effort (5min|10min|15min|20min|30min)",
          found: other,
          path: [],
        ),
      ])
    Error(e) -> Error(e)
  }
}

fn decode_status(
  data: dynamic.Dynamic,
) -> Result(BeadStatus, List(dynamic.DecodeError)) {
  case dynamic.string(data) {
    Ok("pending") -> Ok(Pending)
    Ok("in_progress") -> Ok(InProgress)
    Ok("blocked") -> Ok(Blocked)
    Ok("completed") -> Ok(Completed)
    Ok("failed") -> Ok(Failed)
    Ok(other) ->
      Error([
        dynamic.DecodeError(
          expected: "status (pending|in_progress|blocked|completed|failed)",
          found: other,
          path: [],
        ),
      ])
    Error(e) -> Error(e)
  }
}

fn decode_bead(
  data: dynamic.Dynamic,
) -> Result(PlanBead, List(dynamic.DecodeError)) {
  use id <- result.try(dynamic.field("id", dynamic.string)(data))
  use title <- result.try(dynamic.field("title", dynamic.string)(data))
  use requires_opt <- result.try(dynamic.optional_field(
    "requires",
    dynamic.list(dynamic.string),
  )(data))
  use effort <- result.try(dynamic.field("effort", decode_effort)(data))
  use status <- result.try(dynamic.field("status", decode_status)(data))

  let requires = option.unwrap(requires_opt, [])
  Ok(PlanBead(
    id: id,
    title: title,
    requires: requires,
    effort: effort,
    status: status,
  ))
}

/// Decode a JSON array of beads
pub fn decode_beads_json(
  json_string: String,
) -> Result(List(PlanBead), PlanError) {
  case json.decode(json_string, dynamic.list(decode_bead)) {
    Ok(beads) -> Ok(beads)
    Error(_) -> Error(ParseError("Invalid beads JSON format"))
  }
}

// =============================================================================
// PRIVATE: CUE Parsing (Simplified)
// =============================================================================

/// Parse beads from CUE session content
/// This is a simplified parser - for full CUE support, use cue export
fn parse_beads_from_cue(content: String) -> Result(List(PlanBead), PlanError) {
  // If content starts with '[', treat as JSON from cue export
  case string.starts_with(string.trim(content), "[") {
    True -> decode_beads_json(content)
    False -> {
      // Legacy CUE format - return empty for now
      case string.contains(content, "beads:") {
        False -> Ok([])
        True -> Ok([])
      }
    }
  }
}

// =============================================================================
// PRIVATE: External Functions
// =============================================================================

@external(erlang, "intent_ffi", "current_iso8601_timestamp")
fn current_iso8601_timestamp() -> String
