import gleam/json
import gleam/list
import gleam/string

/// Effect types for second-order analysis
pub type EffectType {
  StateChange
  Notification
  Cascade
  RaceCondition
  RollbackRequired
}

/// Individual effect finding
pub type Effect {
  Effect(
    type_: EffectType,
    description: String,
    severity: Severity,
    suggestion: String,
  )
}

/// Severity levels
pub type Severity {
  High
  Medium
  Low
}

/// Analysis result for a single behavior
pub type BehaviorEffect {
  BehaviorEffect(
    behavior_name: String,
    effects: List(Effect),
  )
}

/// Analysis result for entire spec
pub type SpecAnalysis {
  SpecAnalysis(
    spec_name: String,
    behavior_effects: List(BehaviorEffect),
  )
}

/// Analyze a single behavior for second-order effects
pub fn analyze_behavior(behavior: a) -> List(Effect) {
  let effects = []

  // Analyze based on HTTP method
  let method_effects = analyze_http_method(behavior)
  let effects = list.append(effects, method_effects)

  // Check for cascade effects
  let cascade_effects = analyze_cascade_effects(behavior)
  let effects = list.append(effects, cascade_effects)

  // Check for race conditions
  let race_effects = analyze_race_conditions(behavior)
  let effects = list.append(effects, race_effects)

  // Check for rollback requirements
  let rollback_effects = analyze_rollback_needs(behavior)
  let effects = list.append(effects, rollback_effects)

  effects
}

/// Analyze HTTP method for implications
fn analyze_http_method(_behavior: a) -> List(Effect) {
  // POST operations create state changes
  [
    Effect(
      type_: StateChange,
      description: "Creates new resource in database",
      severity: Medium,
      suggestion: "Add behavior to test duplicate creation",
    ),
    Effect(
      type_: Notification,
      description: "May trigger welcome/notification events",
      severity: Low,
      suggestion: "Add behavior to test notification failure handling",
    ),
  ]
}

/// Analyze potential cascade effects
fn analyze_cascade_effects(_behavior: a) -> List(Effect) {
  [
    Effect(
      type_: Cascade,
      description: "Deletion may orphan related records",
      severity: High,
      suggestion: "Add behavior to test orphaned data cleanup",
    ),
  ]
}

/// Analyze potential race conditions
fn analyze_race_conditions(_behavior: a) -> List(Effect) {
  [
    Effect(
      type_: RaceCondition,
      description: "Concurrent updates may conflict",
      severity: Medium,
      suggestion: "Add behavior to test concurrent modification",
    ),
  ]
}

/// Analyze rollback requirements
fn analyze_rollback_needs(_behavior: a) -> List(Effect) {
  [
    Effect(
      type_: RollbackRequired,
      description: "Operation should be reversible",
      severity: Medium,
      suggestion: "Add compensating delete behavior",
    ),
  ]
}

/// Analyze entire spec
pub fn analyze_spec(_spec: a) -> Result(SpecAnalysis, String) {
  // Return mock analysis for now
  Ok(SpecAnalysis(
    spec_name: "Mock Spec",
    behavior_effects: [
      BehaviorEffect(
        behavior_name: "mock-behavior-1",
        effects: analyze_behavior(Nil),
      ),
      BehaviorEffect(
        behavior_name: "mock-behavior-2",
        effects: analyze_behavior(Nil),
      ),
    ],
  ))
}

/// Format effects as JSON
pub fn format_effects_json(effects: List(Effect)) -> Result(String, String) {
  let json_objects =
    list.map(effects, fn(effect) {
      let type_str = effect_type_to_string(effect.type_)
      let severity_str = severity_to_string(effect.severity)

      json.object([
        #("type", json.string(type_str)),
        #("description", json.string(effect.description)),
        #("severity", json.string(severity_str)),
        #("suggestion", json.string(effect.suggestion)),
      ])
    })

  Ok(json.array(from: json_objects, of: fn(_) { json.object([]) }) |> json.to_string())
}

/// Format effects for CLI display
pub fn format_effects_cli(behavior_name: String, effects: List(Effect)) -> String {
  let header = "Analyzing: " <> behavior_name <> "\n\nSecond-Order Effects:\n"

  let effect_lines =
    list.map(effects, fn(effect) {
      let icon = effect_type_to_icon(effect.type_)
      let severity = severity_to_string(effect.severity)

      "  " <> icon <> " " <> effect_type_to_string(effect.type_) <> ": "
      <> effect.description <> "\n"
      <> "     Severity: " <> severity <> "\n"
      <> "     → " <> effect.suggestion <> "\n"
    })

  string.join([header, ..effect_lines], "")
}

/// Convert effect type to string
fn effect_type_to_string(type_: EffectType) -> String {
  case type_ {
    StateChange -> "state_change"
    Notification -> "notification"
    Cascade -> "cascade"
    RaceCondition -> "race_condition"
    RollbackRequired -> "rollback_required"
  }
}

/// Convert effect type to display icon
fn effect_type_to_icon(type_: EffectType) -> String {
  case type_ {
    StateChange -> "📝"
    Notification -> "📧"
    Cascade -> "🔗"
    RaceCondition -> "⚠️"
    RollbackRequired -> "🔄"
  }
}

/// Convert severity to string
fn severity_to_string(severity: Severity) -> String {
  case severity {
    High -> "high"
    Medium -> "medium"
    Low -> "low"
  }
}
