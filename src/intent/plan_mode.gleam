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
import gleam/int
import gleam/list
// Option not used - all fields required
import gleam/result
// Set not needed - using dict for lookups
import gleam/string
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
pub fn compute_plan(session_id: String) -> Result(ExecutionPlan, PlanError) {
  let session_path = ".intent/session-" <> session_id <> ".cue"

  case simplifile.read(session_path) {
    Error(_) -> Error(SessionNotFound(session_id))
    Ok(content) -> {
      use beads <- result.try(parse_beads_from_cue(content))
      use phases <- result.try(detect_dependency_graph(beads))

      let total_effort = calculate_total_effort(beads)
      let risk = assess_risk(beads, phases)
      let blockers = find_blockers(beads)
      let timestamp = current_iso8601_timestamp()

      Ok(ExecutionPlan(
        session_id: session_id,
        generated_at: timestamp,
        phases: phases,
        total_beads: list.length(beads),
        total_effort: total_effort,
        risk: risk,
        blockers: blockers,
      ))
    }
  }
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

/// Format plan as human-readable ASCII tree
pub fn format_plan_human(plan: ExecutionPlan) -> String {
  let header =
    "╔══════════════════════════════════════════════════════════════╗\n"
    <> "║                    EXECUTION PLAN                            ║\n"
    <> "╠══════════════════════════════════════════════════════════════╣\n"
    <> "║ Session: " <> pad_right(plan.session_id, 51) <> "║\n"
    <> "║ Generated: " <> pad_right(plan.generated_at, 49) <> "║\n"
    <> "║ Total Beads: " <> pad_right(int.to_string(plan.total_beads), 47) <> "║\n"
    <> "║ Total Effort: " <> pad_right(plan.total_effort, 46) <> "║\n"
    <> "║ Risk Level: " <> pad_right(risk_to_string(plan.risk), 48) <> "║\n"
    <> "╚══════════════════════════════════════════════════════════════╝\n\n"

  let blockers_section = case list.is_empty(plan.blockers) {
    True -> ""
    False ->
      "⚠ BLOCKERS:\n"
      <> { plan.blockers |> list.map(fn(b) { "  • " <> b }) |> string.join("\n") }
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
  <> "  \"session_id\": \"" <> escape_json_string(plan.session_id) <> "\",\n"
  <> "  \"generated_at\": \"" <> plan.generated_at <> "\",\n"
  <> "  \"phases\": [\n    " <> phases_json <> "\n  ],\n"
  <> "  \"total_beads\": " <> int.to_string(plan.total_beads) <> ",\n"
  <> "  \"total_effort\": \"" <> plan.total_effort <> "\",\n"
  <> "  \"risk\": \"" <> risk_to_string(plan.risk) <> "\",\n"
  <> "  \"blockers\": [" <> blockers_json <> "]\n"
  <> "}"
}

/// Format error as human-readable string
pub fn format_error(error: PlanError) -> String {
  case error {
    SessionNotFound(id) ->
      "Session not found: " <> id <> "\n"
      <> "Expected file: .intent/session-" <> id <> ".cue"
    ParseError(msg) -> "Failed to parse session: " <> msg
    CyclicDependency(beads) ->
      "Cyclic dependency detected involving: " <> string.join(beads, ", ")
    MissingDependency(bead, missing) ->
      "Bead '" <> bead <> "' requires '" <> missing <> "' which does not exist"
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
            |> list.filter_map(fn(id) { dict.get(by_id, id) |> result.nil_error })

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
                |> list.filter_map(fn(dep) { dict.get(acc, dep) |> result.nil_error })

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

fn format_phase_human(phase: ExecutionPhase) -> String {
  let parallel_indicator = case phase.can_parallel {
    True -> " [can run in parallel]"
    False -> ""
  }

  let header =
    "┌─ " <> phase.title <> " (" <> phase.effort <> ")" <> parallel_indicator <> "\n"

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
  <> "      \"phase_number\": " <> int.to_string(phase.phase_number) <> ",\n"
  <> "      \"title\": \"" <> escape_json_string(phase.title) <> "\",\n"
  <> "      \"beads\": [" <> beads_json <> "],\n"
  <> "      \"can_parallel\": " <> bool_to_string(phase.can_parallel) <> ",\n"
  <> "      \"effort\": \"" <> phase.effort <> "\"\n"
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
// PRIVATE: CUE Parsing (Simplified)
// =============================================================================

/// Parse beads from CUE session content
/// This is a simplified parser - for full CUE support, use cue export
fn parse_beads_from_cue(content: String) -> Result(List(PlanBead), PlanError) {
  // Look for beads array in the CUE content
  // Format: beads: [{ id: "...", ... }, ...]

  case string.contains(content, "beads:") {
    False -> Ok([])  // No beads section
    True -> {
      // Simple extraction - look for bead patterns
      // In production, we'd use cue export for proper parsing
      extract_beads_simple(content)
    }
  }
}

fn extract_beads_simple(_content: String) -> Result(List(PlanBead), PlanError) {
  // This is a placeholder - real implementation would:
  // 1. Call `cue export .intent/session-{id}.cue -e session.beads --out json`
  // 2. Parse the JSON output
  //
  // For now, return empty list to allow module to compile
  // The CLI command will call cue export directly
  Ok([])
}

// =============================================================================
// PRIVATE: External Functions
// =============================================================================

@external(erlang, "intent_ffi", "current_iso8601_timestamp")
fn current_iso8601_timestamp() -> String
