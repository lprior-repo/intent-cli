//// Integration tests for the complete 4-phase planning flow
//// Vision -> Shape -> Spec -> Ready
////
//// These tests verify each phase can complete and pass gates to the next phase.
//// Following the TDD15 workflow: Phase 4 (RED) -> Phase 5 (GREEN) -> Phase 6 (REFACTOR)
////
//// Design by Contract:
//// - Preconditions: Valid phase data with all required fields
//// - Postconditions: Each phase passes its gate criteria before advancing
//// - Invariants: Phase transitions follow strict ordering (Vision->Shape->Spec->Ready)

import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import intent/phase_state.{
  Complete, InProgress, NotStarted, Ready, Shape, Spec, Vision, type GateCheck,
}
import intent/planning_types.{
  Blocker, DimensionScore, FeatureShape, KIRKHealth, MVPSlice, Plan, ReadyReport,
  ShapeSection, SpecSection, type Plan, type ReadyReport, type ShapeSection,
  type SpecSection,
}
import intent/ready_critique
import intent/shape_critique
import intent/vision_critique
import intent/vision_types.{type VisionSection, Scenario, VisionSection}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test Helpers - Create valid phase data
// ============================================================================

/// Create a valid VisionSection that passes gate criteria
fn make_valid_vision() -> VisionSection {
  VisionSection(
    press_release: "Intent CLI eliminates the gap between API specification and testing by providing contract-driven testing with 10x faster feedback loops and 100x fewer integration failures.",
    persona: "Backend engineers building microservices with 5+ REST APIs who need contract testing",
    non_personas: [
      "Frontend developers who only consume APIs",
      "Manual testers who don't write specifications",
    ],
    north_star: "Engineers write one CUE specification and get comprehensive API contract tests, documentation, and implementation hints automatically. Testing time drops from hours to seconds.",
    scenarios: [
      Scenario(
        character: "Sarah",
        persona: "Senior backend engineer",
        motivation: "Needs to ensure API contracts are honored across 12 microservices",
        simulation: "Writes CUE spec for user-api, runs intent check, sees failures with actionable fixes",
        outcome: "All contract violations fixed in 15 minutes instead of days of debugging",
      ),
      Scenario(
        character: "Mike",
        persona: "Tech lead",
        motivation: "Onboarding new team member to existing API",
        simulation: "New engineer reads CUE spec, understands behaviors without reading implementation code",
        outcome: "Productive contribution within 2 hours instead of 2 days",
      ),
    ],
    replaces: Some("Manual Postman collections and ad-hoc curl scripts"),
    vorp: "10x faster: 30 seconds for full API verification vs 4+ hours of manual testing. 100x fewer production incidents from contract mismatches.",
    out_of_scope: [
      "Performance testing",
      "Load testing",
      "UI testing",
      "Mobile app testing",
    ],
  )
}

/// Create a valid ShapeSection that passes gate criteria
fn make_valid_shape() -> ShapeSection {
  ShapeSection(
    features: [
      FeatureShape(
        name: "Spec Validation",
        description: "Validate CUE specifications for correctness",
      ),
      FeatureShape(
        name: "Contract Checking",
        description: "Run specs against live APIs to verify contracts",
      ),
      FeatureShape(
        name: "Quality Analysis",
        description: "KIRK analysis for spec quality and coverage",
      ),
    ],
    critical_path: [
      "Parse CUE specs",
      "Validate syntax",
      "Execute HTTP requests",
      "Check responses against contracts",
    ],
    mvp_slice: MVPSlice(
      description: "Single-spec contract checking with JSON output for AI integration",
      features: ["validate", "check", "quality"],
      shortcuts: [
        "Single environment only",
        "No parallel execution",
        "CLI-only interface",
      ],
    ),
    post_mvp: [
      "Multi-environment support",
      "Parallel test execution",
      "CI/CD integration",
      "Watch mode",
    ],
    validation_moment: "Engineer runs 'intent check spec.cue --target localhost:8080' and sees contract violations with specific fix suggestions in under 5 seconds",
  )
}

/// Create a valid SpecSection that passes gate criteria
fn make_valid_spec() -> SpecSection {
  SpecSection(
    name: "intent-cli",
    description: "Contract-driven API testing CLI",
    rounds_complete: 5,
    kirk_health: KIRKHealth(
      coverage_score: 0.85,
      quality_score: 0.82,
      gaps: [],
      inversions: [],
      effects: [],
    ),
  )
}

/// Create a valid ReadyReport that passes gate criteria
fn make_valid_ready() -> ReadyReport {
  ReadyReport(
    replacement: DimensionScore(
      score: 90,
      reasoning: "Clear value proposition with well-defined audience and success criteria",
      issues: [],
    ),
    empathy: DimensionScore(
      score: 85,
      reasoning: "Strong error handling with anti-pattern awareness",
      issues: [],
    ),
    actionable: DimensionScore(
      score: 85,
      reasoning: "Excellent response checks with clear guidance",
      issues: [],
    ),
    discoverable: DimensionScore(
      score: 80,
      reasoning: "Good naming and organization",
      issues: [],
    ),
    yet_complete: DimensionScore(
      score: 90,
      reasoning: "Fully complete and ready to implement",
      issues: [],
    ),
    overall_readiness: 86,
    blockers: [],
    recommendations: [],
  )
}

/// Create a complete Plan with all 4 phases
fn make_complete_plan() -> Plan {
  Plan(
    id: "test-plan-001",
    created_at: "2026-01-25T12:00:00Z",
    updated_at: "2026-01-25T12:00:00Z",
    vision: make_valid_vision(),
    shape: make_valid_shape(),
    spec: Some(make_valid_spec()),
    ready: Some(make_valid_ready()),
  )
}

// ============================================================================
// Phase 1: VISION Gate Tests
// ============================================================================

pub fn vision_gate_passes_with_valid_data_test() {
  let vision = make_valid_vision()
  let checks = phase_state.check_vision_gate(vision)

  // All checks should pass
  checks
  |> list.all(fn(check: GateCheck) { check.passed })
  |> should.be_true()
}

pub fn vision_gate_fails_with_empty_press_release_test() {
  let vision = VisionSection(..make_valid_vision(), press_release: "")
  let checks = phase_state.check_vision_gate(vision)

  // Should have at least one failing check
  checks
  |> list.any(fn(check: GateCheck) { !check.passed })
  |> should.be_true()

  // Specifically the press_release criterion should fail
  let press_release_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "press_release" })

  case press_release_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

pub fn vision_gate_fails_with_empty_scenarios_test() {
  let vision = VisionSection(..make_valid_vision(), scenarios: [])
  let checks = phase_state.check_vision_gate(vision)

  let scenarios_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "scenarios" })

  case scenarios_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

pub fn vision_critique_passes_with_valid_data_test() {
  let vision = make_valid_vision()
  let result = vision_critique.critique_vision(vision)

  result.passed |> should.be_true()
  { result.score >= 70 } |> should.be_true()
}

// ============================================================================
// Phase 2: SHAPE Gate Tests
// ============================================================================

pub fn shape_gate_passes_with_valid_data_test() {
  let shape = make_valid_shape()
  let checks = phase_state.check_shape_gate(shape)

  checks
  |> list.all(fn(check: GateCheck) { check.passed })
  |> should.be_true()
}

pub fn shape_gate_fails_with_empty_features_test() {
  let shape = ShapeSection(..make_valid_shape(), features: [])
  let checks = phase_state.check_shape_gate(shape)

  let features_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "features" })

  case features_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

pub fn shape_gate_fails_with_empty_critical_path_test() {
  let shape = ShapeSection(..make_valid_shape(), critical_path: [])
  let checks = phase_state.check_shape_gate(shape)

  let critical_path_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "critical_path" })

  case critical_path_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

pub fn shape_critique_passes_with_valid_data_test() {
  let shape = make_valid_shape()
  let result = shape_critique.critique_shape(shape)

  result.passed |> should.be_true()
  { result.score >= 70 } |> should.be_true()
}

// ============================================================================
// Phase 3: SPEC Gate Tests
// ============================================================================

pub fn spec_gate_passes_with_valid_data_test() {
  let spec = make_valid_spec()
  let checks = phase_state.check_spec_gate(spec)

  checks
  |> list.all(fn(check: GateCheck) { check.passed })
  |> should.be_true()
}

pub fn spec_gate_fails_with_incomplete_rounds_test() {
  let spec = SpecSection(..make_valid_spec(), rounds_complete: 3)
  let checks = phase_state.check_spec_gate(spec)

  let rounds_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "rounds_complete" })

  case rounds_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

pub fn spec_gate_fails_with_low_quality_score_test() {
  let spec =
    SpecSection(
      ..make_valid_spec(),
      kirk_health: KIRKHealth(
        coverage_score: 0.5,
        quality_score: 0.5,
        gaps: ["Missing error handling"],
        inversions: [],
        effects: [],
      ),
    )
  let checks = phase_state.check_spec_gate(spec)

  let quality_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "quality_score" })

  case quality_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Phase 4: READY Gate Tests
// ============================================================================

pub fn ready_gate_passes_with_valid_data_test() {
  let ready = make_valid_ready()
  let checks = phase_state.check_ready_gate(ready)

  checks
  |> list.all(fn(check: GateCheck) { check.passed })
  |> should.be_true()
}

pub fn ready_gate_fails_with_low_readiness_test() {
  let ready = ReadyReport(..make_valid_ready(), overall_readiness: 60)
  let checks = phase_state.check_ready_gate(ready)

  let readiness_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "overall_readiness" })

  case readiness_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

pub fn ready_gate_fails_with_critical_blockers_test() {
  let ready =
    ReadyReport(..make_valid_ready(), blockers: [
      Blocker(
        severity: planning_types.Critical,
        description: "Missing authentication flow",
        affected_areas: ["security"],
      ),
    ])
  let checks = phase_state.check_ready_gate(ready)

  let blockers_check =
    checks
    |> list.find(fn(check: GateCheck) { check.criterion == "blockers" })

  case blockers_check {
    Ok(check) -> check.passed |> should.be_false()
    Error(_) -> should.fail()
  }
}

pub fn ready_critique_passes_with_valid_data_test() {
  let ready = make_valid_ready()
  let result = ready_critique.critique_ready(ready)

  result.passed |> should.be_true()
  { result.score >= 70 } |> should.be_true()
}

// ============================================================================
// Phase State Machine Tests
// ============================================================================

pub fn initial_state_starts_at_vision_test() {
  let state = phase_state.create_initial_state()

  state.current_phase |> should.equal(Vision)
  state.status |> should.equal(NotStarted)
}

pub fn advance_from_vision_to_shape_test() {
  let state = phase_state.create_initial_state()
  let result = phase_state.advance_phase(state)

  case result {
    Ok(new_state) -> {
      new_state.current_phase |> should.equal(Shape)
      new_state.status |> should.equal(NotStarted)
    }
    Error(_) -> should.fail()
  }
}

pub fn advance_from_shape_to_spec_test() {
  let state =
    phase_state.PhaseState(
      current_phase: Shape,
      status: Complete,
      gate_checks: [],
      updated_at: "2026-01-25T12:00:00Z",
    )
  let result = phase_state.advance_phase(state)

  case result {
    Ok(new_state) -> {
      new_state.current_phase |> should.equal(Spec)
    }
    Error(_) -> should.fail()
  }
}

pub fn advance_from_spec_to_ready_test() {
  let state =
    phase_state.PhaseState(
      current_phase: Spec,
      status: Complete,
      gate_checks: [],
      updated_at: "2026-01-25T12:00:00Z",
    )
  let result = phase_state.advance_phase(state)

  case result {
    Ok(new_state) -> {
      new_state.current_phase |> should.equal(Ready)
    }
    Error(_) -> should.fail()
  }
}

pub fn advance_from_ready_fails_test() {
  let state =
    phase_state.PhaseState(
      current_phase: Ready,
      status: Complete,
      gate_checks: [],
      updated_at: "2026-01-25T12:00:00Z",
    )
  let result = phase_state.advance_phase(state)

  case result {
    Ok(_) -> should.fail()
    Error(msg) -> msg |> should.equal("Already at final phase (Ready)")
  }
}

// ============================================================================
// Full 4-Phase Flow Integration Tests
// ============================================================================

pub fn complete_flow_all_gates_pass_test() {
  let plan = make_complete_plan()

  // Check Vision gate
  let vision_checks = phase_state.check_vision_gate(plan.vision)
  phase_state.all_gates_passed(vision_checks) |> should.be_true()

  // Check Shape gate
  let shape_checks = phase_state.check_shape_gate(plan.shape)
  phase_state.all_gates_passed(shape_checks) |> should.be_true()

  // Check Spec gate
  case plan.spec {
    Some(spec) -> {
      let spec_checks = phase_state.check_spec_gate(spec)
      phase_state.all_gates_passed(spec_checks) |> should.be_true()
    }
    None -> should.fail()
  }

  // Check Ready gate
  case plan.ready {
    Some(ready) -> {
      let ready_checks = phase_state.check_ready_gate(ready)
      phase_state.all_gates_passed(ready_checks) |> should.be_true()
    }
    None -> should.fail()
  }
}

pub fn can_advance_through_all_phases_test() {
  let plan = make_complete_plan()

  // Start at Vision
  let state = phase_state.create_initial_state()

  // Can advance from Vision
  let vision_result = phase_state.can_advance(state, plan)
  case vision_result {
    Ok(can) -> can |> should.be_true()
    Error(_) -> should.fail()
  }

  // Advance to Shape
  let state_shape = case phase_state.advance_phase(state) {
    Ok(s) -> s
    Error(_) -> panic as "Should advance to Shape"
  }
  state_shape.current_phase |> should.equal(Shape)

  // Can advance from Shape
  let shape_result = phase_state.can_advance(state_shape, plan)
  case shape_result {
    Ok(can) -> can |> should.be_true()
    Error(_) -> should.fail()
  }

  // Advance to Spec
  let state_spec = case phase_state.advance_phase(state_shape) {
    Ok(s) -> s
    Error(_) -> panic as "Should advance to Spec"
  }
  state_spec.current_phase |> should.equal(Spec)

  // Can advance from Spec
  let spec_result = phase_state.can_advance(state_spec, plan)
  case spec_result {
    Ok(can) -> can |> should.be_true()
    Error(_) -> should.fail()
  }

  // Advance to Ready
  let state_ready = case phase_state.advance_phase(state_spec) {
    Ok(s) -> s
    Error(_) -> panic as "Should advance to Ready"
  }
  state_ready.current_phase |> should.equal(Ready)

  // Can advance from Ready (still true because ready gates pass)
  let ready_result = phase_state.can_advance(state_ready, plan)
  case ready_result {
    Ok(can) -> can |> should.be_true()
    Error(_) -> should.fail()
  }
}

pub fn flow_blocked_when_vision_fails_test() {
  let invalid_vision = VisionSection(..make_valid_vision(), press_release: "")
  let plan = Plan(..make_complete_plan(), vision: invalid_vision)

  let state = phase_state.create_initial_state()
  let result = phase_state.can_advance(state, plan)

  case result {
    Ok(_) -> should.fail()
    Error(failures) -> {
      // Should have at least one failure about press_release
      failures
      |> list.any(fn(f) { f == "press_release: Press release is empty" })
      |> should.be_true()
    }
  }
}

pub fn flow_blocked_when_shape_fails_test() {
  let invalid_shape = ShapeSection(..make_valid_shape(), features: [])
  let plan = Plan(..make_complete_plan(), shape: invalid_shape)

  let state =
    phase_state.PhaseState(
      current_phase: Shape,
      status: InProgress,
      gate_checks: [],
      updated_at: "2026-01-25T12:00:00Z",
    )
  let result = phase_state.can_advance(state, plan)

  case result {
    Ok(_) -> should.fail()
    Error(failures) -> {
      failures
      |> list.any(fn(f) { f == "features: No features defined" })
      |> should.be_true()
    }
  }
}

pub fn flow_blocked_when_spec_missing_test() {
  let plan = Plan(..make_complete_plan(), spec: None)

  let state =
    phase_state.PhaseState(
      current_phase: Spec,
      status: InProgress,
      gate_checks: [],
      updated_at: "2026-01-25T12:00:00Z",
    )
  let result = phase_state.can_advance(state, plan)

  case result {
    Ok(_) -> should.fail()
    Error(failures) -> {
      failures
      |> list.any(fn(f) { f == "spec_exists: Spec section not yet created" })
      |> should.be_true()
    }
  }
}

pub fn flow_blocked_when_ready_missing_test() {
  let plan = Plan(..make_complete_plan(), ready: None)

  let state =
    phase_state.PhaseState(
      current_phase: Ready,
      status: InProgress,
      gate_checks: [],
      updated_at: "2026-01-25T12:00:00Z",
    )
  let result = phase_state.can_advance(state, plan)

  case result {
    Ok(_) -> should.fail()
    Error(failures) -> {
      failures
      |> list.any(fn(f) { f == "ready_exists: Ready section not yet created" })
      |> should.be_true()
    }
  }
}

// ============================================================================
// Critique Integration Tests
// ============================================================================

pub fn all_critiques_pass_for_valid_plan_test() {
  let plan = make_complete_plan()

  // Vision critique
  let vision_result = vision_critique.critique_vision(plan.vision)
  vision_result.passed |> should.be_true()

  // Shape critique
  let shape_result = shape_critique.critique_shape(plan.shape)
  shape_result.passed |> should.be_true()

  // Ready critique
  case plan.ready {
    Some(ready) -> {
      let ready_result = ready_critique.critique_ready(ready)
      ready_result.passed |> should.be_true()
    }
    None -> should.fail()
  }
}

pub fn critique_scores_meet_thresholds_test() {
  let plan = make_complete_plan()

  // Vision should score >= 70
  let vision_result = vision_critique.critique_vision(plan.vision)
  { vision_result.score >= 70 } |> should.be_true()

  // Shape should score >= 70
  let shape_result = shape_critique.critique_shape(plan.shape)
  { shape_result.score >= 70 } |> should.be_true()

  // Ready should score >= 70
  case plan.ready {
    Some(ready) -> {
      let ready_result = ready_critique.critique_ready(ready)
      { ready_result.score >= 70 } |> should.be_true()
    }
    None -> should.fail()
  }
}
