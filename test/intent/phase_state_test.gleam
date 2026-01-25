import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import intent/phase_state.{
  type GateCheck, BlockedOnGate, GateCheck, InProgress, NotStarted, Ready, Shape,
  Vision,
}
import intent/planning_types.{
  Critical, DimensionScore, KIRKHealth, MVPSlice, Plan, ReadyReport,
  ShapeSection, SpecSection,
}
import intent/vision_types.{Scenario, VisionSection}

pub fn main() {
  gleeunit.main()
}

pub fn create_initial_state_test() {
  let state = phase_state.create_initial_state()
  state.current_phase |> should.equal(Vision)
  state.status |> should.equal(NotStarted)
  state.gate_checks |> should.equal([])
}

pub fn check_vision_gate_incomplete_test() {
  let vision =
    VisionSection(
      press_release: "",
      persona: "",
      non_personas: [],
      north_star: "",
      scenarios: [],
      replaces: None,
      vorp: "",
      out_of_scope: [],
    )
  let checks = phase_state.check_vision_gate(vision)
  checks |> should_have_failing_check("press_release")
  checks |> should_have_failing_check("persona")
  checks |> should_have_failing_check("scenarios")
}

pub fn check_vision_gate_complete_test() {
  let vision =
    VisionSection(
      press_release: "We are building a revolutionary planning tool",
      persona: "Engineering teams doing contract-first API development",
      non_personas: ["Non-technical users"],
      north_star: "Zero-ambiguity planning from vision to implementation",
      scenarios: [
        Scenario(
          character: "Alice",
          persona: "Backend engineer",
          motivation: "Reduce spec ambiguity",
          simulation: "Uses interview to build spec",
          outcome: "Clear, testable requirements",
        ),
      ],
      replaces: Some("Manual planning docs"),
      vorp: "10x reduction in spec ambiguity",
      out_of_scope: ["UI implementation", "Deployment automation"],
    )
  let checks = phase_state.check_vision_gate(vision)
  checks |> phase_state.all_gates_passed() |> should.be_true()
}

pub fn check_shape_gate_incomplete_test() {
  let shape =
    ShapeSection(
      features: [],
      critical_path: [],
      mvp_slice: MVPSlice(description: "", features: [], shortcuts: []),
      post_mvp: [],
      validation_moment: "",
    )
  let checks = phase_state.check_shape_gate(shape)
  checks |> should_have_failing_check("features")
  checks |> should_have_failing_check("mvp_slice")
}

pub fn check_spec_gate_incomplete_test() {
  let spec =
    SpecSection(
      name: "Test Spec",
      description: "Testing",
      rounds_complete: 3,
      kirk_health: KIRKHealth(
        coverage_score: 0.5,
        quality_score: 0.6,
        gaps: ["Missing security tests"],
        inversions: [],
        effects: [],
      ),
    )
  let checks = phase_state.check_spec_gate(spec)
  checks |> should_have_failing_check("rounds_complete")
}

pub fn check_spec_gate_complete_test() {
  let spec =
    SpecSection(
      name: "Test Spec",
      description: "Testing",
      rounds_complete: 5,
      kirk_health: KIRKHealth(
        coverage_score: 0.85,
        quality_score: 0.8,
        gaps: [],
        inversions: [],
        effects: [],
      ),
    )
  let checks = phase_state.check_spec_gate(spec)
  checks |> phase_state.all_gates_passed() |> should.be_true()
}

pub fn check_ready_gate_low_score_test() {
  let ready =
    ReadyReport(
      replacement: DimensionScore(score: 60, reasoning: "Weak VORP", issues: []),
      empathy: DimensionScore(score: 70, reasoning: "Some friction", issues: []),
      actionable: DimensionScore(
        score: 80,
        reasoning: "Good errors",
        issues: [],
      ),
      discoverable: DimensionScore(
        score: 75,
        reasoning: "Decent UX",
        issues: [],
      ),
      yet_complete: DimensionScore(
        score: 85,
        reasoning: "Close to north star",
        issues: [],
      ),
      overall_readiness: 74,
      blockers: [],
      recommendations: [],
    )
  let checks = phase_state.check_ready_gate(ready)
  checks |> should_have_failing_check("overall_readiness")
}

pub fn check_ready_gate_critical_blocker_test() {
  let ready =
    ReadyReport(
      replacement: DimensionScore(
        score: 90,
        reasoning: "Strong VORP",
        issues: [],
      ),
      empathy: DimensionScore(score: 90, reasoning: "Low friction", issues: []),
      actionable: DimensionScore(
        score: 90,
        reasoning: "Great errors",
        issues: [],
      ),
      discoverable: DimensionScore(
        score: 90,
        reasoning: "Excellent UX",
        issues: [],
      ),
      yet_complete: DimensionScore(
        score: 90,
        reasoning: "Matches north star",
        issues: [],
      ),
      overall_readiness: 90,
      blockers: [
        planning_types.Blocker(
          severity: Critical,
          description: "Security vulnerability found",
          affected_areas: ["authentication"],
        ),
      ],
      recommendations: [],
    )
  let checks = phase_state.check_ready_gate(ready)
  checks |> should_have_failing_check("blockers")
}

pub fn advance_phase_from_vision_test() {
  let state = phase_state.create_initial_state()
  case phase_state.advance_phase(state) {
    Ok(new_state) -> new_state.current_phase |> should.equal(Shape)
    Error(_) -> should.fail()
  }
}

pub fn advance_phase_from_ready_test() {
  let state =
    phase_state.PhaseState(
      current_phase: Ready,
      status: InProgress,
      gate_checks: [],
      updated_at: "2026-01-25T15:00:00Z",
    )
  case phase_state.advance_phase(state) {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }
}

pub fn can_advance_with_failing_gates_test() {
  let state =
    phase_state.PhaseState(
      current_phase: Vision,
      status: BlockedOnGate,
      gate_checks: [
        GateCheck(
          criterion: "press_release",
          passed: False,
          reason: "Press release is empty",
        ),
      ],
      updated_at: "2026-01-25T15:00:00Z",
    )
  let incomplete_vision =
    VisionSection(
      press_release: "",
      persona: "",
      non_personas: [],
      north_star: "",
      scenarios: [],
      replaces: None,
      vorp: "",
      out_of_scope: [],
    )
  let plan =
    Plan(
      id: "test-plan",
      created_at: "2026-01-25T15:00:00Z",
      updated_at: "2026-01-25T15:00:00Z",
      vision: incomplete_vision,
      shape: ShapeSection(
        features: [],
        critical_path: [],
        mvp_slice: MVPSlice(description: "", features: [], shortcuts: []),
        post_mvp: [],
        validation_moment: "",
      ),
      spec: None,
      ready: None,
    )
  case phase_state.can_advance(state, plan) {
    Error(failures) -> failures |> should.not_equal([])
    Ok(_) -> should.fail()
  }
}

fn should_have_failing_check(checks: List(GateCheck), criterion: String) {
  let has_failing =
    checks
    |> list.any(fn(check) { check.criterion == criterion && !check.passed })
  has_failing |> should.be_true()
}
