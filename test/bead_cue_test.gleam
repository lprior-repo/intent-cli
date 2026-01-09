/// Bead CUE Generation Tests
/// TDD: Tests for generating beads as CUE (not JSONL)
import gleam/string
import gleeunit
import gleeunit/should
import intent/bead_cue.{
  type Bead, Bead, Expectation, TestSpec, bead_to_cue, beads_to_cue,
  generate_bead_id,
}

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// Bead ID Generation
// =============================================================================

/// Test: Generate valid bead IDs in PREFIX-NNN format
pub fn bead_cue_generate_id_test() {
  let id = generate_bead_id("AUTH", 1)

  id
  |> should.equal("AUTH-001")
}

/// Test: Generate IDs with larger numbers
pub fn bead_cue_generate_id_large_number_test() {
  let id = generate_bead_id("API", 42)

  id
  |> should.equal("API-042")
}

/// Test: Generate IDs with triple digits
pub fn bead_cue_generate_id_triple_digit_test() {
  let id = generate_bead_id("TEST", 123)

  id
  |> should.equal("TEST-123")
}

// =============================================================================
// Single Bead to CUE
// =============================================================================

/// Test: Convert minimal bead to CUE
pub fn bead_cue_minimal_bead_test() {
  let bead = make_test_bead("AUTH-001", "Implement login", "5min")
  let cue = bead_to_cue(bead)

  // Must contain required fields
  cue
  |> string.contains("id: \"AUTH-001\"")
  |> should.be_true()

  cue
  |> string.contains("title: \"Implement login\"")
  |> should.be_true()

  cue
  |> string.contains("effort: \"5min\"")
  |> should.be_true()

  cue
  |> string.contains("status: \"pending\"")
  |> should.be_true()
}

/// Test: Bead contains what and why
pub fn bead_cue_what_why_test() {
  let bead =
    Bead(
      ..make_test_bead("AUTH-001", "Implement login", "5min"),
      what: "Add JWT authentication to /login endpoint",
      why: "Users need to authenticate to access protected resources",
    )
  let cue = bead_to_cue(bead)

  cue
  |> string.contains("what:")
  |> should.be_true()

  cue
  |> string.contains("why:")
  |> should.be_true()
}

/// Test: Bead with test specification
pub fn bead_cue_test_spec_test() {
  let spec =
    TestSpec(
      command: "gleam test",
      expect: Expectation(
        exit_code: 0,
        stdout_contains: "tests, 0 failures",
        stdout_matches: "",
        stderr_empty: True,
        status: 0,
        body_contains: "",
        header_present: "",
      ),
    )
  let bead =
    Bead(
      ..make_test_bead("AUTH-001", "Implement login", "5min"),
      test_spec: spec,
    )
  let cue = bead_to_cue(bead)

  cue
  |> string.contains("test: {")
  |> should.be_true()

  cue
  |> string.contains("command:")
  |> should.be_true()

  cue
  |> string.contains("expect: {")
  |> should.be_true()
}

/// Test: Bead with done_when criteria
pub fn bead_cue_done_when_test() {
  let bead =
    Bead(..make_test_bead("AUTH-001", "Implement login", "5min"), done_when: [
      "Login endpoint returns JWT",
      "Tests pass",
      "Docs updated",
    ])
  let cue = bead_to_cue(bead)

  cue
  |> string.contains("done_when: [")
  |> should.be_true()

  cue
  |> string.contains("\"Login endpoint returns JWT\"")
  |> should.be_true()
}

/// Test: Bead with edge cases
pub fn bead_cue_edge_cases_test() {
  let bead =
    Bead(..make_test_bead("AUTH-001", "Implement login", "5min"), edge_cases: [
      "Invalid credentials",
      "Expired token",
      "Missing password",
    ])
  let cue = bead_to_cue(bead)

  cue
  |> string.contains("edge_cases: [")
  |> should.be_true()

  cue
  |> string.contains("\"Invalid credentials\"")
  |> should.be_true()
}

/// Test: Bead with dependencies
pub fn bead_cue_requires_test() {
  let bead =
    Bead(
      ..make_test_bead("AUTH-002", "Implement protected route", "10min"),
      requires: ["AUTH-001"],
    )
  let cue = bead_to_cue(bead)

  cue
  |> string.contains("requires: [")
  |> should.be_true()

  cue
  |> string.contains("\"AUTH-001\"")
  |> should.be_true()
}

// =============================================================================
// Multiple Beads to CUE
// =============================================================================

/// Test: Convert multiple beads to session CUE
pub fn bead_cue_multiple_beads_test() {
  let beads = [
    make_test_bead("AUTH-001", "Implement login", "5min"),
    make_test_bead("AUTH-002", "Implement logout", "5min"),
    make_test_bead("AUTH-003", "Implement token refresh", "10min"),
  ]
  let cue = beads_to_cue("session-abc123", beads)

  // Should wrap in session structure
  cue
  |> string.contains("beads: [")
  |> should.be_true()

  // Should contain all beads
  cue
  |> string.contains("AUTH-001")
  |> should.be_true()

  cue
  |> string.contains("AUTH-002")
  |> should.be_true()

  cue
  |> string.contains("AUTH-003")
  |> should.be_true()
}

// =============================================================================
// Effort Validation
// =============================================================================

/// Test: Valid effort values
pub fn bead_cue_valid_effort_test() {
  // All valid effort values per schema
  let efforts = ["5min", "10min", "15min", "20min", "30min"]

  efforts
  |> should.equal(["5min", "10min", "15min", "20min", "30min"])
}

// =============================================================================
// Status Validation
// =============================================================================

/// Test: Valid status values
pub fn bead_cue_valid_status_test() {
  // All valid status values per schema
  let statuses = ["pending", "in_progress", "blocked", "completed", "failed"]

  statuses
  |> should.equal(["pending", "in_progress", "blocked", "completed", "failed"])
}

// =============================================================================
// Helper Functions
// =============================================================================

fn make_test_bead(id: String, title: String, effort: String) -> Bead {
  Bead(
    id: id,
    title: title,
    what: "Test implementation",
    why: "Test purpose",
    test_spec: TestSpec(
      command: "gleam test",
      expect: Expectation(
        exit_code: 0,
        stdout_contains: "",
        stdout_matches: "",
        stderr_empty: True,
        status: 0,
        body_contains: "",
        header_present: "",
      ),
    ),
    done_when: ["Implementation complete"],
    file: "src/test.gleam",
    edge_cases: [],
    requires: [],
    effort: effort,
    status: "pending",
  )
}
