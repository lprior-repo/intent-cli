import gleeunit
import gleeunit/should
import intent/effects_analyzer
import gleam/list
import gleam/string

pub fn main() -> Nil {
  gleeunit.main()
}

// Test: Analyze behavior with state changes
pub fn state_change_detection_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  // Should detect state change
  let state_effects =
    effects
    |> list.filter(fn(e) { e.type_ == effects_analyzer.StateChange })

  list.length(state_effects)
  |> should.equal(1)
}

// Test: Analyze behavior with cascade effects
pub fn cascade_effect_detection_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  // Should detect cascade effects
  let cascade_effects =
    effects
    |> list.filter(fn(e) { e.type_ == effects_analyzer.Cascade })

  list.length(cascade_effects)
  |> should.equal(1)
}

// Test: Analyze behavior for race conditions
pub fn race_condition_detection_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  // Should detect race condition
  let race_effects =
    effects
    |> list.filter(fn(e) { e.type_ == effects_analyzer.RaceCondition })

  list.length(race_effects)
  |> should.equal(1)
}

// Test: Analyze behavior with notifications
pub fn notification_detection_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  // Should detect notification effect
  let notification_effects =
    effects
    |> list.filter(fn(e) { e.type_ == effects_analyzer.Notification })

  list.length(notification_effects)
  |> should.equal(1)
}

// Test: Suggest compensating behaviors
pub fn compensating_behavior_suggestions_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  // Effects should include suggestions
  let has_suggestions =
    effects
    |> list.any(fn(e) { string.length(e.suggestion) > 0 })

  has_suggestions
  |> should.be_true
}

// Test: Analyze multiple behaviors
pub fn multiple_behaviors_analysis_test() {
  let result = effects_analyzer.analyze_spec(Nil)

  result
  |> should.be_ok

  let assert Ok(analysis) = result

  // Should have 2 behavior effects
  list.length(analysis.behavior_effects)
  |> should.equal(2)
}

// Test: Format output as JSON
pub fn json_output_format_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  let json_result = effects_analyzer.format_effects_json(effects)

  json_result
  |> should.be_ok

  // JSON should be valid
  let assert Ok(json) = json_result
  string.contains(json, "[")
  |> should.be_true
}

// Test: Format output for CLI display
pub fn cli_output_format_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  let output = effects_analyzer.format_effects_cli("delete-user", effects)

  // CLI output should contain behavior name
  string.contains(output, "delete-user")
  |> should.be_true

  // Should contain effect markers
  string.contains(output, "Effects:")
  |> should.be_true
}

// Test: Rollback detection
pub fn rollback_detection_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  // Should detect rollback requirement
  let rollback_effects =
    effects
    |> list.filter(fn(e) { e.type_ == effects_analyzer.RollbackRequired })

  list.length(rollback_effects)
  |> should.equal(1)
}

// Test: All effect types present
pub fn all_effect_types_present_test() {
  let effects = effects_analyzer.analyze_behavior(Nil)

  // Should have 5 effect types total
  list.length(effects)
  |> should.equal(5)
}
