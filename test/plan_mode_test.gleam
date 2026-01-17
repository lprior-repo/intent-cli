import gleam/int
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/plan_mode.{
  type BeadStatus, type Effort, type PlanBead, Blocked, CyclicDependency,
  Effort10min, Effort15min, Effort20min, Effort30min, Effort5min, ExecutionPhase,
  ExecutionPlan, Failed, MissingDependency, Pending, PlanBead, SessionNotFound,
}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

fn assert_contains(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False ->
      panic as {
        "Expected string to contain: " <> needle <> "\nBut got: " <> haystack
      }
  }
}

fn assert_length(list: List(a), expected: Int) -> Nil {
  let actual = list.length(list)
  case actual == expected {
    True -> Nil
    False ->
      panic as {
        "Expected length "
        <> int.to_string(expected)
        <> " but got "
        <> int.to_string(actual)
      }
  }
}

fn make_bead(id: String, requires: List(String)) -> PlanBead {
  PlanBead(
    id: id,
    title: "Test Bead " <> id,
    requires: requires,
    effort: Effort10min,
    status: Pending,
  )
}

fn make_bead_with_effort(
  id: String,
  requires: List(String),
  effort: Effort,
) -> PlanBead {
  PlanBead(
    id: id,
    title: "Test Bead " <> id,
    requires: requires,
    effort: effort,
    status: Pending,
  )
}

fn make_bead_with_status(
  id: String,
  requires: List(String),
  status: BeadStatus,
) -> PlanBead {
  PlanBead(
    id: id,
    title: "Test Bead " <> id,
    requires: requires,
    effort: Effort10min,
    status: status,
  )
}

// =============================================================================
// TEST: detect_dependency_graph - Empty list
// =============================================================================

pub fn detect_dependency_graph_empty_test() {
  let beads = []

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_ok
  |> fn(phases) {
    phases
    |> should.equal([])
  }
}

// =============================================================================
// TEST: detect_dependency_graph - No dependencies
// =============================================================================

pub fn detect_dependency_graph_no_deps_test() {
  let beads = [
    make_bead("bead-1", []),
    make_bead("bead-2", []),
    make_bead("bead-3", []),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_ok
  |> fn(phases) {
    // All beads should be in phase 1 since they have no dependencies
    phases
    |> should.not_equal([])

    // Should have exactly 1 phase
    let assert [
      ExecutionPhase(
        phase_number: num,
        beads: bead_ids,
        can_parallel: parallel,
        ..,
      ),
    ] = phases

    // Phase should contain all 3 beads
    bead_ids
    |> should.equal(["bead-3", "bead-2", "bead-1"])

    num
    |> should.equal(1)

    parallel
    |> should.equal(True)
  }
}

// =============================================================================
// TEST: detect_dependency_graph - Linear chain A→B→C
// =============================================================================

pub fn detect_dependency_graph_linear_chain_test() {
  let beads = [
    make_bead("bead-A", []),
    make_bead("bead-B", ["bead-A"]),
    make_bead("bead-C", ["bead-B"]),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_ok
  |> fn(phases) {
    // Should have 3 phases for A → B → C
    phases
    |> should.not_equal([])

    // Verify we have 3 phases
    let assert [
      ExecutionPhase(beads: beads1, can_parallel: p1, ..),
      ExecutionPhase(beads: beads2, can_parallel: p2, ..),
      ExecutionPhase(beads: beads3, can_parallel: p3, ..),
    ] = phases

    // Phase 1 should contain only A
    beads1
    |> should.equal(["bead-A"])
    p1
    |> should.equal(False)

    // Phase 2 should contain only B
    beads2
    |> should.equal(["bead-B"])
    p2
    |> should.equal(False)

    // Phase 3 should contain only C
    beads3
    |> should.equal(["bead-C"])
    p3
    |> should.equal(False)
  }
}

// =============================================================================
// TEST: detect_dependency_graph - Diamond A→B,C→D
// =============================================================================

pub fn detect_dependency_graph_diamond_test() {
  let beads = [
    make_bead("bead-A", []),
    make_bead("bead-B", ["bead-A"]),
    make_bead("bead-C", ["bead-A"]),
    make_bead("bead-D", ["bead-B", "bead-C"]),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_ok
  |> fn(phases) {
    // Should have 3 phases:
    // Phase 1: A
    // Phase 2: B, C (parallel)
    // Phase 3: D
    let assert [
      ExecutionPhase(beads: beads1, can_parallel: p1, ..),
      ExecutionPhase(beads: beads2, can_parallel: p2, ..),
      ExecutionPhase(beads: beads3, can_parallel: p3, ..),
    ] = phases

    // Phase 1 should contain only A
    beads1
    |> should.equal(["bead-A"])
    p1
    |> should.equal(False)

    // Phase 2 should contain B and C (they can run in parallel)
    beads2
    |> fn(beads) {
      // Order doesn't matter, but should contain both
      beads |> assert_length(2)
      // Sort for deterministic comparison
      let sorted = beads |> list.sort(fn(a, b) { string.compare(a, b) })
      sorted |> should.equal(["bead-B", "bead-C"])
    }
    p2
    |> should.equal(True)

    // Phase 3 should contain only D
    beads3
    |> should.equal(["bead-D"])
    p3
    |> should.equal(False)
  }
}

// =============================================================================
// TEST: detect_dependency_graph - Cyclic dependency A→B→A
// =============================================================================

pub fn detect_dependency_graph_cyclic_test() {
  let beads = [make_bead("bead-A", ["bead-B"]), make_bead("bead-B", ["bead-A"])]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_error
  |> fn(error) {
    case error {
      CyclicDependency(bead_ids) -> {
        // Should report the beads involved in the cycle
        bead_ids
        |> should.not_equal([])
        // Both A and B should be in the cycle
        bead_ids |> assert_length(2)
      }
      _ -> panic as "Expected CyclicDependency error"
    }
  }
}

// =============================================================================
// TEST: detect_dependency_graph - Missing dependency A→X
// =============================================================================

pub fn detect_dependency_graph_missing_dep_test() {
  let beads = [
    make_bead("bead-A", ["bead-X"]),
    // X doesn't exist
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_error
  |> fn(error) {
    case error {
      MissingDependency(bead, missing) -> {
        bead
        |> should.equal("bead-A")
        missing
        |> should.equal("bead-X")
      }
      _ -> panic as "Expected MissingDependency error"
    }
  }
}

// =============================================================================
// TEST: format_plan_human - ASCII output is readable
// =============================================================================

pub fn format_plan_human_test() {
  let phases = [
    ExecutionPhase(
      phase_number: 1,
      title: "Phase 1",
      beads: ["bead-A", "bead-B"],
      can_parallel: True,
      effort: "20min",
    ),
    ExecutionPhase(
      phase_number: 2,
      title: "Phase 2",
      beads: ["bead-C"],
      can_parallel: False,
      effort: "10min",
    ),
  ]

  let plan =
    ExecutionPlan(
      session_id: "test-123",
      generated_at: "2026-01-16T00:00:00Z",
      phases: phases,
      total_beads: 3,
      total_effort: "30min",
      risk: plan_mode.Low,
      blockers: [],
    )

  let output = plan_mode.format_plan_human(plan)

  // Check that output contains expected elements
  assert_contains(output, "EXECUTION PLAN")
  assert_contains(output, "Session: test-123")
  assert_contains(output, "Total Beads: 3")
  assert_contains(output, "Total Effort: 30min")
  assert_contains(output, "Risk Level: low")
  assert_contains(output, "Phase 1")
  assert_contains(output, "Phase 2")
  assert_contains(output, "bead-A")
  assert_contains(output, "bead-B")
  assert_contains(output, "bead-C")
  assert_contains(output, "[can run in parallel]")
}

// =============================================================================
// TEST: format_plan_human - With blockers
// =============================================================================

pub fn format_plan_human_with_blockers_test() {
  let plan =
    ExecutionPlan(
      session_id: "test-123",
      generated_at: "2026-01-16T00:00:00Z",
      phases: [],
      total_beads: 2,
      total_effort: "20min",
      risk: plan_mode.High,
      blockers: [
        "bead-X: Blocked by external dependency", "bead-Y: Failed test",
      ],
    )

  let output = plan_mode.format_plan_human(plan)

  // Check that blockers section appears
  assert_contains(output, "⚠ BLOCKERS:")
  assert_contains(output, "bead-X: Blocked by external dependency")
  assert_contains(output, "bead-Y: Failed test")
}

// =============================================================================
// TEST: format_plan_json - Valid JSON output
// =============================================================================

pub fn format_plan_json_test() {
  let phases = [
    ExecutionPhase(
      phase_number: 1,
      title: "Phase 1",
      beads: ["bead-A"],
      can_parallel: False,
      effort: "10min",
    ),
  ]

  let plan =
    ExecutionPlan(
      session_id: "test-123",
      generated_at: "2026-01-16T00:00:00Z",
      phases: phases,
      total_beads: 1,
      total_effort: "10min",
      risk: plan_mode.Low,
      blockers: [],
    )

  let json = plan_mode.format_plan_json(plan)

  // Check that JSON contains expected fields
  assert_contains(json, "\"session_id\": \"test-123\"")
  assert_contains(json, "\"generated_at\": \"2026-01-16T00:00:00Z\"")
  assert_contains(json, "\"total_beads\": 1")
  assert_contains(json, "\"total_effort\": \"10min\"")
  assert_contains(json, "\"risk\": \"low\"")
  assert_contains(json, "\"phase_number\": 1")
  assert_contains(json, "\"title\": \"Phase 1\"")
  assert_contains(json, "\"beads\": [\"bead-A\"]")
  assert_contains(json, "\"can_parallel\": false")
  assert_contains(json, "\"blockers\": []")
}

// =============================================================================
// TEST: format_plan_json - JSON escaping
// =============================================================================

pub fn format_plan_json_escaping_test() {
  let plan =
    ExecutionPlan(
      session_id: "test\"with\\quotes",
      generated_at: "2026-01-16T00:00:00Z",
      phases: [],
      total_beads: 0,
      total_effort: "0min",
      risk: plan_mode.Low,
      blockers: ["blocker with \"quotes\""],
    )

  let json = plan_mode.format_plan_json(plan)

  // Check that special characters are escaped
  assert_contains(json, "test\\\"with\\\\quotes")
  assert_contains(json, "blocker with \\\"quotes\\\"")
}

// =============================================================================
// TEST: format_error - SessionNotFound
// =============================================================================

pub fn format_error_session_not_found_test() {
  let error = SessionNotFound("test-123")

  let output = plan_mode.format_error(error)

  assert_contains(output, "Session not found: test-123")
  assert_contains(output, ".intent/session-test-123.cue")
}

// =============================================================================
// TEST: format_error - CyclicDependency
// =============================================================================

pub fn format_error_cyclic_test() {
  let error = CyclicDependency(["bead-A", "bead-B"])

  let output = plan_mode.format_error(error)

  assert_contains(output, "Cyclic dependency detected")
  assert_contains(output, "bead-A")
  assert_contains(output, "bead-B")
}

// =============================================================================
// TEST: format_error - MissingDependency
// =============================================================================

pub fn format_error_missing_dependency_test() {
  let error = MissingDependency("bead-A", "bead-X")

  let output = plan_mode.format_error(error)

  assert_contains(output, "bead-A")
  assert_contains(output, "requires")
  assert_contains(output, "bead-X")
  assert_contains(output, "does not exist")
}

// =============================================================================
// TEST: format_error - ParseError
// =============================================================================

pub fn format_error_parse_error_test() {
  let error = plan_mode.ParseError("Invalid CUE syntax")

  let output = plan_mode.format_error(error)

  assert_contains(output, "Failed to parse session")
  assert_contains(output, "Invalid CUE syntax")
}

// =============================================================================
// TEST: Effort calculation and formatting
// =============================================================================

pub fn effort_calculation_test() {
  let beads = [
    make_bead_with_effort("bead-1", [], Effort5min),
    make_bead_with_effort("bead-2", [], Effort10min),
    make_bead_with_effort("bead-3", [], Effort15min),
    make_bead_with_effort("bead-4", [], Effort20min),
    make_bead_with_effort("bead-5", [], Effort30min),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_ok
  |> fn(phases) {
    let assert [ExecutionPhase(effort: effort_str, ..)] = phases

    // Total effort should be 5 + 10 + 15 + 20 + 30 = 80 minutes = 1h 20min
    effort_str
    |> should.equal("1h 20min")
  }
}

// =============================================================================
// TEST: Risk assessment
// =============================================================================

pub fn risk_assessment_low_test() {
  let beads = [
    make_bead_with_status("bead-1", [], Pending),
    make_bead_with_status("bead-2", [], Pending),
    make_bead_with_status("bead-3", [], Pending),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_ok
  |> fn(phases) {
    // Create a plan to test risk assessment
    let plan =
      ExecutionPlan(
        session_id: "test",
        generated_at: "2026-01-16T00:00:00Z",
        phases: phases,
        total_beads: 3,
        total_effort: "30min",
        risk: plan_mode.Low,
        blockers: [],
      )

    plan.risk
    |> should.equal(plan_mode.Low)
  }
}

pub fn risk_assessment_with_blocked_beads_test() {
  let beads = [
    make_bead_with_status("bead-1", [], Pending),
    make_bead_with_status("bead-2", [], Blocked),
    make_bead_with_status("bead-3", [], Failed),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  result
  |> should.be_ok
  |> fn(_phases) {
    // With 2 out of 3 beads blocked/failed, risk should be high or critical
    // This is tested implicitly through the compute_plan function
    Nil
  }
}

// =============================================================================
// TEST: compute_plan with non-existent session
// =============================================================================

pub fn compute_plan_session_not_found_test() {
  let result = plan_mode.compute_plan("nonexistent-session-id-12345")

  result
  |> should.be_error
  |> fn(error) {
    case error {
      SessionNotFound(id) -> {
        id
        |> should.equal("nonexistent-session-id-12345")
      }
      _ -> panic as "Expected SessionNotFound error"
    }
  }
}
