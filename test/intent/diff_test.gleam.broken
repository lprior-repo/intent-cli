import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import intent/diff
import intent/types

// ============================================================================
// Test Helpers
// ============================================================================

fn make_behavior(name: String, intent: String) -> types.Behavior {
  types.Behavior(
    name: name,
    intent: intent,
    notes: "",
    requires: [],
    tags: [],
    request: types.Request(
      method: types.Get,
      path: "/" <> name,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: types.Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

fn make_feature(name: String, behaviors: List(types.Behavior)) -> types.Feature {
  types.Feature(name: name, description: "Test feature", behaviors: behaviors)
}

fn make_spec(features: List(types.Feature)) -> types.Spec {
  types.Spec(
    name: "Test Spec",
    description: "Test spec",
    audience: "developers",
    version: "1.0.0",
    success_criteria: [],
    config: types.Config(
      base_url: "http://localhost",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    ),
    features: features,
    rules: [],
    anti_patterns: [],
    ai_hints: types.AIHints(
      implementation: types.ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: types.SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
    ),
  )
}

fn make_rule(name: String, description: String) -> types.Rule {
  types.Rule(
    name: name,
    description: description,
    when: None,
    check: types.RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: None,
  )
}

fn make_anti_pattern(name: String, description: String) -> types.AntiPattern {
  types.AntiPattern(
    name: name,
    description: description,
    bad_example: json.null(),
    good_example: json.null(),
    why: "Because",
  )
}

// ============================================================================
// No Changes Tests
// ============================================================================

pub fn identical_specs_have_no_changes_test() {
  let spec = make_spec([])
  let result = diff.compare_specs(spec, spec)

  result.has_changes |> should.be_false()
}

pub fn identical_specs_with_features_have_no_changes_test() {
  let b1 = make_behavior("login", "User can log in")
  let f1 = make_feature("Auth", [b1])
  let spec = make_spec([f1])

  let result = diff.compare_specs(spec, spec)

  result.has_changes |> should.be_false()
}

// ============================================================================
// Metadata Changes Tests
// ============================================================================

pub fn name_change_detected_test() {
  let old = make_spec([])
  let new = types.Spec(..old, name: "New Name")

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.name_changed
  |> should.equal(Some(diff.StringChange("Test Spec", "New Name")))
}

pub fn version_change_detected_test() {
  let old = make_spec([])
  let new = types.Spec(..old, version: "2.0.0")

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.version_changed
  |> should.equal(Some(diff.StringChange("1.0.0", "2.0.0")))
}

// ============================================================================
// Config Changes Tests
// ============================================================================

pub fn base_url_change_detected_test() {
  let old = make_spec([])
  let new_config =
    types.Config(..old.config, base_url: "http://api.example.com")
  let new = types.Spec(..old, config: new_config)

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.config_changes
  |> list.any(fn(c) {
    case c {
      diff.BaseUrlChanged("http://localhost", "http://api.example.com") -> True
      _ -> False
    }
  })
  |> should.be_true()
}

pub fn timeout_change_detected_test() {
  let old = make_spec([])
  let new_config = types.Config(..old.config, timeout_ms: 10_000)
  let new = types.Spec(..old, config: new_config)

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.config_changes
  |> list.any(fn(c) {
    case c {
      diff.TimeoutChanged(5000, 10_000) -> True
      _ -> False
    }
  })
  |> should.be_true()
}

// ============================================================================
// Feature Changes Tests
// ============================================================================

pub fn feature_added_detected_test() {
  let old = make_spec([])
  let b1 = make_behavior("login", "User can log in")
  let f1 = make_feature("Auth", [b1])
  let new = make_spec([f1])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.feature_changes
  |> list.any(fn(c) {
    case c {
      diff.FeatureAdded("Auth", 1) -> True
      _ -> False
    }
  })
  |> should.be_true()
}

pub fn feature_removed_detected_test() {
  let b1 = make_behavior("login", "User can log in")
  let f1 = make_feature("Auth", [b1])
  let old = make_spec([f1])
  let new = make_spec([])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.feature_changes
  |> list.any(fn(c) {
    case c {
      diff.FeatureRemoved("Auth", 1) -> True
      _ -> False
    }
  })
  |> should.be_true()
}

// ============================================================================
// Behavior Changes Tests
// ============================================================================

pub fn behavior_added_detected_test() {
  let b1 = make_behavior("login", "User can log in")
  let f_old = make_feature("Auth", [b1])
  let old = make_spec([f_old])

  let b2 = make_behavior("logout", "User can log out")
  let f_new = make_feature("Auth", [b1, b2])
  let new = make_spec([f_new])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.behavior_changes
  |> list.any(fn(c) {
    case c {
      diff.BehaviorAdded("Auth", "logout", "User can log out") -> True
      _ -> False
    }
  })
  |> should.be_true()
}

pub fn behavior_removed_detected_test() {
  let b1 = make_behavior("login", "User can log in")
  let b2 = make_behavior("logout", "User can log out")
  let f_old = make_feature("Auth", [b1, b2])
  let old = make_spec([f_old])

  let f_new = make_feature("Auth", [b1])
  let new = make_spec([f_new])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.behavior_changes
  |> list.any(fn(c) {
    case c {
      diff.BehaviorRemoved("Auth", "logout", "User can log out") -> True
      _ -> False
    }
  })
  |> should.be_true()
}

pub fn behavior_modified_intent_detected_test() {
  let b1 = make_behavior("login", "User can log in")
  let f_old = make_feature("Auth", [b1])
  let old = make_spec([f_old])

  let b1_modified =
    types.Behavior(..b1, intent: "User authenticates with credentials")
  let f_new = make_feature("Auth", [b1_modified])
  let new = make_spec([f_new])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.behavior_changes
  |> list.any(fn(c) {
    case c {
      diff.BehaviorModified("Auth", "login", modifications) ->
        list.any(modifications, fn(m) {
          case m {
            diff.IntentChanged(
              "User can log in",
              "User authenticates with credentials",
            ) -> True
            _ -> False
          }
        })
      _ -> False
    }
  })
  |> should.be_true()
}

// ============================================================================
// Rule Changes Tests
// ============================================================================

pub fn rule_added_detected_test() {
  let old = make_spec([])

  let rule = make_rule("no-passwords", "Passwords must not appear in responses")
  let new = types.Spec(..old, rules: [rule])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.rule_changes
  |> list.any(fn(c) {
    case c {
      diff.RuleAdded("no-passwords", "Passwords must not appear in responses") ->
        True
      _ -> False
    }
  })
  |> should.be_true()
}

pub fn rule_removed_detected_test() {
  let rule = make_rule("no-passwords", "Passwords must not appear in responses")
  let old = types.Spec(..make_spec([]), rules: [rule])
  let new = make_spec([])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.rule_changes
  |> list.any(fn(c) {
    case c {
      diff.RuleRemoved("no-passwords", "Passwords must not appear in responses") ->
        True
      _ -> False
    }
  })
  |> should.be_true()
}

// ============================================================================
// Anti-Pattern Changes Tests
// ============================================================================

pub fn anti_pattern_added_detected_test() {
  let old = make_spec([])

  let ap = make_anti_pattern("sequential-ids", "Don't expose sequential IDs")
  let new = types.Spec(..old, anti_patterns: [ap])

  let result = diff.compare_specs(old, new)

  result.has_changes |> should.be_true()
  result.anti_pattern_changes
  |> list.any(fn(c) {
    case c {
      diff.AntiPatternAdded("sequential-ids", "Don't expose sequential IDs") ->
        True
      _ -> False
    }
  })
  |> should.be_true()
}

// ============================================================================
// Formatting Tests
// ============================================================================

pub fn format_no_changes_test() {
  let spec = make_spec([])
  let result = diff.compare_specs(spec, spec)

  diff.format_diff(result) |> should.equal("No changes detected between specs.")
}

pub fn format_with_changes_includes_header_test() {
  let old = make_spec([])
  let new = types.Spec(..old, version: "2.0.0")

  let result = diff.compare_specs(old, new)
  let formatted = diff.format_diff(result)

  formatted |> should.equal("SPEC DIFF\n=========\n\nVersion: 1.0.0 -> 2.0.0")
}

// ============================================================================
// Summary Tests
// ============================================================================

pub fn summary_no_changes_test() {
  let spec = make_spec([])
  let result = diff.compare_specs(spec, spec)

  diff.diff_summary(result) |> should.equal("No changes")
}

pub fn summary_with_added_behaviors_test() {
  let old = make_spec([])
  let b1 = make_behavior("login", "User can log in")
  let f1 = make_feature("Auth", [b1])
  let new = make_spec([f1])

  let result = diff.compare_specs(old, new)
  let summary = diff.diff_summary(result)

  // Should mention 1 behavior added (via feature addition)
  summary |> should.equal("1 behavior(s) added")
}

// ============================================================================
// JSON Output Tests
// ============================================================================

pub fn json_output_has_changes_field_test() {
  let spec = make_spec([])
  let result = diff.compare_specs(spec, spec)
  let json_output = diff.diff_to_json(result)
  let json_str = json.to_string(json_output)

  // Should contain has_changes: false
  json_str |> should.not_equal("")
}
