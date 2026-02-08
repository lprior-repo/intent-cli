// Comprehensive test suite for plan_mode module
// Tests dependency resolution, cycle detection
import gleeunit
import gleeunit/should
import intent/plan_mode
import gleam/list
import gleam/string

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// EFFORT CONVERSION TESTS
// =============================================================================

pub fn effort_to_label_5min_test() {
  let result = plan_mode.effort_to_label(plan_mode.Effort5min)
  should.equal(result, "5min")
}

pub fn effort_to_label_10min_test() {
  let result = plan_mode.effort_to_label(plan_mode.Effort10min)
  should.equal(result, "10min")
}

pub fn effort_to_label_30min_test() {
  let result = plan_mode.effort_to_label(plan_mode.Effort30min)
  should.equal(result, "30min")
}

// =============================================================================
// BEAD STATUS STRING CONVERSION TESTS
// =============================================================================

pub fn bead_status_to_string_pending_test() {
  let result = plan_mode.bead_status_to_string(plan_mode.Pending)
  should.equal(result, "pending")
}

pub fn bead_status_to_string_in_progress_test() {
  let result = plan_mode.bead_status_to_string(plan_mode.InProgress)
  should.equal(result, "in_progress")
}

pub fn bead_status_to_string_blocked_test() {
  let result = plan_mode.bead_status_to_string(plan_mode.Blocked)
  should.equal(result, "blocked")
}

pub fn bead_status_to_string_completed_test() {
  let result = plan_mode.bead_status_to_string(plan_mode.Completed)
  should.equal(result, "completed")
}

pub fn bead_status_to_string_failed_test() {
  let result = plan_mode.bead_status_to_string(plan_mode.Failed)
  should.equal(result, "failed")
}

// =============================================================================
// DEPENDENCY GRAPH DETECTION TESTS
// =============================================================================

pub fn detect_dependency_graph_no_dependencies_test() {
  let beads = [
    plan_mode.PlanBead(
      id: "a",
      title: "A",
      requires: [],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "b",
      title: "B",
      requires: [],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  case result {
    Ok(phases) -> {
      list.length(phases)
      |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

pub fn detect_dependency_graph_simple_chain_test() {
  let beads = [
    plan_mode.PlanBead(
      id: "a",
      title: "A",
      requires: [],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "b",
      title: "B",
      requires: ["a"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  case result {
    Ok(phases) -> {
      list.length(phases)
      |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

pub fn detect_dependency_graph_parallel_branches_test() {
  let beads = [
    plan_mode.PlanBead(
      id: "a",
      title: "A",
      requires: [],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "b",
      title: "B",
      requires: ["a"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "c",
      title: "C",
      requires: ["a"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  case result {
    Ok(phases) -> {
      list.length(phases)
      |> should.equal(2)

      // Check that second phase has parallel execution
      let phase2_opt = list.last(phases)
      case phase2_opt {
        Ok(phase2) -> {
          list.length(phase2.beads)
          |> should.equal(2)
          phase2.can_parallel
          |> should.equal(True)
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn detect_dependency_graph_cyclic_dependency_test() {
  let beads = [
    plan_mode.PlanBead(
      id: "a",
      title: "A",
      requires: ["b"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "b",
      title: "B",
      requires: ["a"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  case result {
    Error(plan_mode.CyclicDependency(cycle_beads)) -> {
      list.length(cycle_beads)
      |> should.equal(2)
    }
    Ok(_) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn detect_dependency_graph_self_cycle_test() {
  let beads = [
    plan_mode.PlanBead(
      id: "a",
      title: "A",
      requires: ["a"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  case result {
    Error(plan_mode.CyclicDependency(cycle_beads)) -> {
      list.length(cycle_beads)
      |> should.equal(1)
    }
    Ok(_) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn detect_dependency_graph_missing_dependency_test() {
  let beads = [
    plan_mode.PlanBead(
      id: "a",
      title: "A",
      requires: ["z"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  case result {
    Error(plan_mode.MissingDependency(bead_id, missing_dep)) -> {
      bead_id
      |> should.equal("a")
      missing_dep
      |> should.equal("z")
    }
    Ok(_) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn detect_dependency_graph_complex_diamond_test() {
  let beads = [
    plan_mode.PlanBead(
      id: "a",
      title: "A",
      requires: [],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "b",
      title: "B",
      requires: ["a"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "c",
      title: "C",
      requires: ["a"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
    plan_mode.PlanBead(
      id: "d",
      title: "D",
      requires: ["b", "c"],
      effort: plan_mode.Effort5min,
      status: plan_mode.Pending,
    ),
  ]

  let result = plan_mode.detect_dependency_graph(beads)

  case result {
    Ok(phases) -> {
      list.length(phases)
      |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// ERROR MESSAGE TESTS
// =============================================================================

pub fn format_error_session_not_found_test() {
  let error = plan_mode.SessionNotFound("test-session")
  let msg = plan_mode.format_error(error)

  msg
  |> string.contains("Session not found: test-session")
  |> should.equal(True)
}

pub fn format_error_parse_error_test() {
  let error = plan_mode.ParseError("Invalid CUE format")
  let msg = plan_mode.format_error(error)

  msg
  |> should.equal("Failed to parse session: Invalid CUE format")
}

pub fn format_error_cyclic_dependency_test() {
  let error = plan_mode.CyclicDependency(["a", "b", "c"])
  let msg = plan_mode.format_error(error)

  msg
  |> string.contains("Cyclic dependency")
  |> should.equal(True)
  msg
  |> string.contains("a")
  |> should.equal(True)
  msg
  |> string.contains("b")
  |> should.equal(True)
}

pub fn format_error_missing_dependency_test() {
  let error = plan_mode.MissingDependency("bead-1", "bead-2")
  let msg = plan_mode.format_error(error)

  msg
  |> string.contains("bead-1")
  |> should.equal(True)
  msg
  |> string.contains("bead-2")
  |> should.equal(True)
  msg
  |> string.contains("does not exist")
  |> should.equal(True)
}

pub fn risk_to_string_test() {
  should.equal(plan_mode.risk_to_string(plan_mode.Low), "low")
  should.equal(plan_mode.risk_to_string(plan_mode.Medium), "medium")
  should.equal(plan_mode.risk_to_string(plan_mode.High), "high")
  should.equal(plan_mode.risk_to_string(plan_mode.Critical), "critical")
}
