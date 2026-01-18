//// Tests for plan_mode.gleam
//// Verifies pure computation logic can operate without file I/O
////
//// Key verification: compute_plan_from_content is a pure function that
//// accepts session content as a String, enabling testing without filesystem.

import gleam/list
import gleeunit/should
import intent/plan_mode.{
  Effort10min, Effort15min, Effort5min, ExecutionPlan, Low, Medium, ParseError,
  Pending, PlanBead,
}

// =============================================================================
// TEST: compute_plan_from_content - Pure function tests (no file I/O)
// =============================================================================

pub fn compute_plan_from_content_with_empty_content_test() {
  // Empty content should return plan with no beads
  let result = plan_mode.compute_plan_from_content("test-session", "")

  should.be_ok(result)

  let plan = case result {
    Ok(p) -> p
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(plan.session_id, "test-session")
  should.equal(plan.total_beads, 0)
  should.equal(plan.phases, [])
}

pub fn compute_plan_from_content_with_no_beads_section_test() {
  // Content without beads: section should return empty plan
  let content =
    "
// Some CUE content without beads
name: \"test\"
version: \"1.0\"
"

  let result = plan_mode.compute_plan_from_content("no-beads", content)

  should.be_ok(result)

  let plan = case result {
    Ok(p) -> p
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(plan.session_id, "no-beads")
  should.equal(plan.total_beads, 0)
}

pub fn compute_plan_from_content_preserves_session_id_test() {
  // Session ID should be preserved in output
  let session_ids = ["abc123", "my-session", "session-with-dashes"]

  list.each(session_ids, fn(session_id) {
    let result = plan_mode.compute_plan_from_content(session_id, "")
    should.be_ok(result)

    let plan = case result {
      Ok(p) -> p
      Error(_) -> panic as "Expected Ok"
    }

    should.equal(plan.session_id, session_id)
  })
}

// =============================================================================
// TEST: detect_dependency_graph - Dependency analysis
// =============================================================================

pub fn detect_dependency_graph_empty_list_test() {
  // Empty list should return empty phases
  let result = plan_mode.detect_dependency_graph([])

  should.be_ok(result)

  let phases = case result {
    Ok(p) -> p
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(phases, [])
}

pub fn detect_dependency_graph_single_bead_test() {
  // Single bead with no dependencies -> single phase
  let beads = [
    PlanBead(
      id: "bead-1",
      title: "First bead",
      requires: [],
      effort: Effort10min,
      status: Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  should.be_ok(result)

  let phases = case result {
    Ok(p) -> p
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(list.length(phases), 1)

  let first_phase = case phases {
    [p, ..] -> p
    [] -> panic as "Expected at least one phase"
  }

  should.equal(first_phase.phase_number, 1)
  should.equal(first_phase.beads, ["bead-1"])
}

pub fn detect_dependency_graph_two_independent_beads_test() {
  // Two beads with no dependencies -> one phase, can parallel
  let beads = [
    PlanBead(
      id: "bead-a",
      title: "Bead A",
      requires: [],
      effort: Effort5min,
      status: Pending,
    ),
    PlanBead(
      id: "bead-b",
      title: "Bead B",
      requires: [],
      effort: Effort5min,
      status: Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  should.be_ok(result)

  let phases = case result {
    Ok(p) -> p
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(list.length(phases), 1)

  let first_phase = case phases {
    [p, ..] -> p
    [] -> panic as "Expected at least one phase"
  }

  should.be_true(first_phase.can_parallel)
  should.equal(list.length(first_phase.beads), 2)
}

pub fn detect_dependency_graph_sequential_deps_test() {
  // Chain: A <- B <- C (three phases)
  let beads = [
    PlanBead(
      id: "bead-a",
      title: "Bead A",
      requires: [],
      effort: Effort5min,
      status: Pending,
    ),
    PlanBead(
      id: "bead-b",
      title: "Bead B",
      requires: ["bead-a"],
      effort: Effort10min,
      status: Pending,
    ),
    PlanBead(
      id: "bead-c",
      title: "Bead C",
      requires: ["bead-b"],
      effort: Effort15min,
      status: Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  should.be_ok(result)

  let phases = case result {
    Ok(p) -> p
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(list.length(phases), 3)
}

pub fn detect_dependency_graph_missing_dep_test() {
  // Bead with missing dependency should error
  let beads = [
    PlanBead(
      id: "bead-1",
      title: "Bead 1",
      requires: ["nonexistent"],
      effort: Effort5min,
      status: Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  should.be_error(result)

  case result {
    Error(plan_mode.MissingDependency(bead, missing)) -> {
      should.equal(bead, "bead-1")
      should.equal(missing, "nonexistent")
    }
    _ -> panic as "Expected MissingDependency error"
  }
}

// =============================================================================
// TEST: format_plan_human - Human-readable output
// =============================================================================

pub fn format_plan_human_includes_session_id_test() {
  let plan =
    ExecutionPlan(
      session_id: "my-test-session",
      generated_at: "2024-01-01T00:00:00Z",
      phases: [],
      total_beads: 0,
      total_effort: "0min",
      risk: Low,
      blockers: [],
    )

  let output = plan_mode.format_plan_human(plan)

  should.be_true(
    output
    |> contains_string("my-test-session"),
  )
}

pub fn format_plan_human_includes_effort_test() {
  let plan =
    ExecutionPlan(
      session_id: "test",
      generated_at: "2024-01-01T00:00:00Z",
      phases: [],
      total_beads: 5,
      total_effort: "1h 30min",
      risk: Medium,
      blockers: [],
    )

  let output = plan_mode.format_plan_human(plan)

  should.be_true(
    output
    |> contains_string("1h 30min"),
  )
}

// =============================================================================
// TEST: format_plan_json - JSON output
// =============================================================================

pub fn format_plan_json_valid_structure_test() {
  let plan =
    ExecutionPlan(
      session_id: "json-test",
      generated_at: "2024-01-01T00:00:00Z",
      phases: [],
      total_beads: 0,
      total_effort: "0min",
      risk: Low,
      blockers: [],
    )

  let output = plan_mode.format_plan_json(plan)

  // Should be valid JSON structure
  should.be_true(
    output
    |> contains_string("\"session_id\""),
  )
  should.be_true(
    output
    |> contains_string("\"json-test\""),
  )
  should.be_true(
    output
    |> contains_string("\"phases\""),
  )
  should.be_true(
    output
    |> contains_string("\"total_beads\""),
  )
}

pub fn format_plan_json_escapes_special_chars_test() {
  let plan =
    ExecutionPlan(
      session_id: "test\"with\"quotes",
      generated_at: "2024-01-01T00:00:00Z",
      phases: [],
      total_beads: 0,
      total_effort: "0min",
      risk: Low,
      blockers: [],
    )

  let output = plan_mode.format_plan_json(plan)

  // Quotes should be escaped
  should.be_true(
    output
    |> contains_string("\\\""),
  )
}

// =============================================================================
// TEST: format_error - Error messages
// =============================================================================

pub fn format_error_parse_error_test() {
  let error = ParseError("Invalid CUE syntax")
  let output = plan_mode.format_error(error)

  should.be_true(
    output
    |> contains_string("Invalid CUE syntax"),
  )
}

// =============================================================================
// Helpers
// =============================================================================

fn contains_string(haystack: String, needle: String) -> Bool {
  case haystack {
    "" -> needle == ""
    _ -> {
      let haystack_len = string_length(haystack)
      let needle_len = string_length(needle)
      case needle_len > haystack_len {
        True -> False
        False -> check_contains(haystack, needle, 0, haystack_len - needle_len)
      }
    }
  }
}

fn check_contains(
  haystack: String,
  needle: String,
  pos: Int,
  max_pos: Int,
) -> Bool {
  case pos > max_pos {
    True -> False
    False -> {
      case string_slice(haystack, pos, string_length(needle)) == needle {
        True -> True
        False -> check_contains(haystack, needle, pos + 1, max_pos)
      }
    }
  }
}

@external(erlang, "string", "length")
fn string_length(s: String) -> Int

@external(erlang, "string", "slice")
fn string_slice(s: String, start: Int, length: Int) -> String
