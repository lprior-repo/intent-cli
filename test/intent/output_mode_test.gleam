/// Tests for output_mode module - Robot mode support
import gleeunit
import gleeunit/should
import intent/output_mode

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Robot Mode Creation Tests
// ============================================================================

pub fn test_robot_mode_exists() {
  let mode = output_mode.Robot
  mode
  |> should.equal(output_mode.Robot)
}

pub fn test_from_robot_flag_true_creates_robot_mode() {
  output_mode.from_robot_flag(True)
  |> should.equal(output_mode.Robot)
}

pub fn test_from_robot_flag_false_creates_interactive_mode() {
  output_mode.from_robot_flag(False)
  |> should.equal(output_mode.Interactive)
}

// ============================================================================
// Robot Mode Detection Tests
// ============================================================================

pub fn test_is_robot_returns_true_for_robot_mode() {
  output_mode.Robot
  |> output_mode.is_robot
  |> should.be_true
}

pub fn test_is_robot_returns_false_for_interactive_mode() {
  output_mode.Interactive
  |> output_mode.is_robot
  |> should.be_false
}

pub fn test_is_robot_returns_false_for_json_mode() {
  output_mode.Json
  |> output_mode.is_robot
  |> should.be_false
}

pub fn test_is_robot_returns_false_for_quiet_mode() {
  output_mode.Quiet
  |> output_mode.is_robot
  |> should.be_false
}

// ============================================================================
// Robot Mode Behavior Tests
// ============================================================================

pub fn test_robot_mode_is_not_interactive() {
  output_mode.Robot
  |> output_mode.is_interactive
  |> should.be_false
}

pub fn test_robot_mode_should_not_show_spinner() {
  output_mode.Robot
  |> output_mode.should_show_spinner
  |> should.be_false
}

pub fn test_robot_mode_should_not_show_colors() {
  output_mode.Robot
  |> output_mode.should_show_colors
  |> should.be_false
}

pub fn test_robot_mode_is_json_compatible() {
  // Robot mode should behave like JSON mode for compatibility
  output_mode.Robot
  |> output_mode.is_json
  |> should.be_true
}

// ============================================================================
// Flag Combination Tests
// ============================================================================

pub fn test_from_flags_robot_overrides_json() {
  output_mode.from_flags_with_robot(
    is_robot: True,
    is_json: True,
    is_quiet: False,
  )
  |> should.equal(output_mode.Robot)
}

pub fn test_from_flags_robot_overrides_quiet() {
  output_mode.from_flags_with_robot(
    is_robot: True,
    is_json: False,
    is_quiet: True,
  )
  |> should.equal(output_mode.Robot)
}

pub fn test_from_flags_json_takes_precedence_over_quiet() {
  output_mode.from_flags_with_robot(
    is_robot: False,
    is_json: True,
    is_quiet: True,
  )
  |> should.equal(output_mode.Json)
}

pub fn test_from_flags_quiet_when_no_robot_or_json() {
  output_mode.from_flags_with_robot(
    is_robot: False,
    is_json: False,
    is_quiet: True,
  )
  |> should.equal(output_mode.Quiet)
}

pub fn test_from_flags_interactive_when_all_false() {
  output_mode.from_flags_with_robot(
    is_robot: False,
    is_json: False,
    is_quiet: False,
  )
  |> should.equal(output_mode.Interactive)
}

// ============================================================================
// Backward Compatibility Tests
// ============================================================================

pub fn test_existing_from_json_flag_still_works() {
  output_mode.from_json_flag(True)
  |> should.equal(output_mode.Json)
}

pub fn test_existing_from_quiet_flag_still_works() {
  output_mode.from_quiet_flag(True)
  |> should.equal(output_mode.Quiet)
}

pub fn test_existing_from_flags_still_works() {
  output_mode.from_flags(True, False)
  |> should.equal(output_mode.Json)
}
