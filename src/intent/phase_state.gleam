/// Phase State Machine for 4-phase planning workflow
///
/// Enforces gate-based progression through:
/// Vision → Shape → Spec → Ready
///
/// Each phase has specific gate criteria that must be satisfied
/// before advancing to the next phase.
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import intent/ffi
import intent/planning_types.{
  type ReadyReport, type ShapeSection, type SpecSection, Critical,
}
import intent/vision_types.{type VisionSection}

// =============================================================================
// TYPES
// =============================================================================

/// The four phases of the planning workflow
pub type PhaseName {
  Vision
  Shape
  Spec
  Ready
}

/// Status of the current phase
pub type PhaseStatus {
  NotStarted
  InProgress
  BlockedOnGate
  Complete
}

/// Result of a single gate criterion check
pub type GateCheck {
  GateCheck(criterion: String, passed: Bool, reason: String)
}

/// State machine tracking current phase and gate validation
pub type PhaseState {
  PhaseState(
    current_phase: PhaseName,
    status: PhaseStatus,
    gate_checks: List(GateCheck),
    updated_at: String,
  )
}

// =============================================================================
// STATE CREATION
// =============================================================================

/// Create initial state at Vision phase
pub fn create_initial_state() -> PhaseState {
  PhaseState(
    current_phase: Vision,
    status: NotStarted,
    gate_checks: [],
    updated_at: current_timestamp(),
  )
}

// =============================================================================
// GATE CHECKS
// =============================================================================

/// Check Vision phase gate criteria
/// Requirements:
/// - press_release non-empty
/// - persona non-empty
/// - north_star non-empty
/// - vorp non-empty
/// - at least 1 scenario
/// - at least 1 out_of_scope item
pub fn check_vision_gate(vision: VisionSection) -> List(GateCheck) {
  [
    GateCheck(
      criterion: "press_release",
      passed: !string.is_empty(vision.press_release),
      reason: case string.is_empty(vision.press_release) {
        True -> "Press release is empty"
        False -> "Press release defined"
      },
    ),
    GateCheck(
      criterion: "persona",
      passed: !string.is_empty(vision.persona),
      reason: case string.is_empty(vision.persona) {
        True -> "Persona is empty"
        False -> "Persona defined"
      },
    ),
    GateCheck(
      criterion: "north_star",
      passed: !string.is_empty(vision.north_star),
      reason: case string.is_empty(vision.north_star) {
        True -> "North star is empty"
        False -> "North star defined"
      },
    ),
    GateCheck(
      criterion: "vorp",
      passed: !string.is_empty(vision.vorp),
      reason: case string.is_empty(vision.vorp) {
        True -> "VORP is empty"
        False -> "VORP defined"
      },
    ),
    GateCheck(
      criterion: "scenarios",
      passed: !list.is_empty(vision.scenarios),
      reason: case list.is_empty(vision.scenarios) {
        True -> "No scenarios provided"
        False ->
          "Has "
          <> int.to_string(list.length(vision.scenarios))
          <> " scenario(s)"
      },
    ),
    GateCheck(
      criterion: "out_of_scope",
      passed: !list.is_empty(vision.out_of_scope),
      reason: case list.is_empty(vision.out_of_scope) {
        True -> "No out-of-scope items defined"
        False ->
          "Has "
          <> int.to_string(list.length(vision.out_of_scope))
          <> " out-of-scope item(s)"
      },
    ),
  ]
}

/// Check Shape phase gate criteria
/// Requirements:
/// - at least 1 feature
/// - critical_path non-empty
/// - mvp_slice has description
/// - validation_moment non-empty
pub fn check_shape_gate(shape: ShapeSection) -> List(GateCheck) {
  [
    GateCheck(
      criterion: "features",
      passed: !list.is_empty(shape.features),
      reason: case list.is_empty(shape.features) {
        True -> "No features defined"
        False ->
          "Has " <> int.to_string(list.length(shape.features)) <> " feature(s)"
      },
    ),
    GateCheck(
      criterion: "critical_path",
      passed: !list.is_empty(shape.critical_path),
      reason: case list.is_empty(shape.critical_path) {
        True -> "Critical path is empty"
        False -> "Critical path defined"
      },
    ),
    GateCheck(
      criterion: "mvp_slice",
      passed: !string.is_empty(shape.mvp_slice.description),
      reason: case string.is_empty(shape.mvp_slice.description) {
        True -> "MVP slice description is empty"
        False -> "MVP slice described"
      },
    ),
    GateCheck(
      criterion: "validation_moment",
      passed: !string.is_empty(shape.validation_moment),
      reason: case string.is_empty(shape.validation_moment) {
        True -> "Validation moment is empty"
        False -> "Validation moment defined"
      },
    ),
  ]
}

/// Check Spec phase gate criteria
/// Requirements:
/// - rounds_complete = 5
/// - quality_score > 0.7
pub fn check_spec_gate(spec: SpecSection) -> List(GateCheck) {
  [
    GateCheck(
      criterion: "rounds_complete",
      passed: spec.rounds_complete == 5,
      reason: case spec.rounds_complete == 5 {
        True -> "All 5 rounds complete"
        False ->
          "Only "
          <> int.to_string(spec.rounds_complete)
          <> " of 5 rounds complete"
      },
    ),
    GateCheck(
      criterion: "quality_score",
      passed: spec.kirk_health.quality_score >. 0.7,
      reason: case spec.kirk_health.quality_score >. 0.7 {
        True -> "Quality score acceptable"
        False -> "Quality score too low"
      },
    ),
  ]
}

/// Check Ready phase gate criteria
/// Requirements:
/// - overall_readiness >= 80
/// - no Critical blockers
pub fn check_ready_gate(ready: ReadyReport) -> List(GateCheck) {
  let has_critical_blockers =
    ready.blockers
    |> list.any(fn(blocker) { blocker.severity == Critical })

  [
    GateCheck(
      criterion: "overall_readiness",
      passed: ready.overall_readiness >= 80,
      reason: case ready.overall_readiness >= 80 {
        True -> "Readiness score meets threshold"
        False ->
          "Readiness score "
          <> int.to_string(ready.overall_readiness)
          <> " below threshold of 80"
      },
    ),
    GateCheck(
      criterion: "blockers",
      passed: !has_critical_blockers,
      reason: case has_critical_blockers {
        True -> "Has Critical severity blockers"
        False -> "No Critical blockers"
      },
    ),
  ]
}

/// Check if all gate checks have passed
pub fn all_gates_passed(checks: List(GateCheck)) -> Bool {
  checks
  |> list.all(fn(check) { check.passed })
}

// =============================================================================
// PHASE ADVANCEMENT
// =============================================================================

/// Advance to the next phase in the workflow
/// Returns Error if already at Ready (final phase)
pub fn advance_phase(state: PhaseState) -> Result(PhaseState, String) {
  case state.current_phase {
    Vision ->
      Ok(
        PhaseState(
          ..state,
          current_phase: Shape,
          status: NotStarted,
          updated_at: current_timestamp(),
        ),
      )
    Shape ->
      Ok(
        PhaseState(
          ..state,
          current_phase: Spec,
          status: NotStarted,
          updated_at: current_timestamp(),
        ),
      )
    Spec ->
      Ok(
        PhaseState(
          ..state,
          current_phase: Ready,
          status: NotStarted,
          updated_at: current_timestamp(),
        ),
      )
    Ready -> Error("Already at final phase (Ready)")
  }
}

/// Check if can advance to next phase based on current Plan state
/// Returns Ok(True) if gates pass, or Error with list of failing criteria
pub fn can_advance(
  state: PhaseState,
  plan: planning_types.Plan,
) -> Result(Bool, List(String)) {
  let checks = case state.current_phase {
    Vision -> check_vision_gate(plan.vision)
    Shape -> check_shape_gate(plan.shape)
    Spec ->
      case plan.spec {
        option.Some(spec) -> check_spec_gate(spec)
        option.None -> [
          GateCheck(
            criterion: "spec_exists",
            passed: False,
            reason: "Spec section not yet created",
          ),
        ]
      }
    Ready ->
      case plan.ready {
        option.Some(ready) -> check_ready_gate(ready)
        option.None -> [
          GateCheck(
            criterion: "ready_exists",
            passed: False,
            reason: "Ready section not yet created",
          ),
        ]
      }
  }

  case all_gates_passed(checks) {
    True -> Ok(True)
    False -> {
      let failures =
        checks
        |> list.filter(fn(check) { !check.passed })
        |> list.map(fn(check) { check.criterion <> ": " <> check.reason })

      Error(failures)
    }
  }
}

// =============================================================================
// UTILITIES
// =============================================================================

/// Get current timestamp in ISO8601 format using FFI
fn current_timestamp() -> String {
  ffi.current_iso8601_timestamp()
}
