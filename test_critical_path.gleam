// Minimal manual test for find_critical_path
import gleam/io
import intent/plan_mode.{Effort10min, Effort15min, Effort5min, Pending, PlanBead}

pub fn main() {
  // Test 1: Empty list
  let result1 = plan_mode.find_critical_path([])
  case result1 {
    Ok([]) -> io.println("✓ Test 1 passed: empty list")
    _ -> io.println("✗ Test 1 failed")
  }

  // Test 2: Single bead
  let beads2 = [
    PlanBead(
      id: "bead-1",
      title: "First bead",
      requires: [],
      effort: Effort10min,
      status: Pending,
    ),
  ]
  let result2 = plan_mode.find_critical_path(beads2)
  case result2 {
    Ok(["bead-1"]) -> io.println("✓ Test 2 passed: single bead")
    _ -> io.println("✗ Test 2 failed")
  }

  // Test 3: Linear chain A -> B -> C
  let beads3 = [
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
  let result3 = plan_mode.find_critical_path(beads3)
  case result3 {
    Ok(["bead-a", "bead-b", "bead-c"]) ->
      io.println("✓ Test 3 passed: linear chain")
    _ -> io.println("✗ Test 3 failed")
  }

  io.println("\nAll manual tests completed!")
}
