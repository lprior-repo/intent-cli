//// Shared test helpers for Intent CLI tests
//// This module provides common factory functions for creating test data

import gleam/dict
import gleam/json
import gleam/option.{None, Some}
import intent/planning_types.{
  type Plan, FeatureShape, MVPSlice, Plan, ShapeSection, SpecSection,
}
import intent/types.{
  type Behavior, type Config, type Feature, type Request, type Spec, AIHints,
  Behavior, Config, Feature, ImplementationHints, Request, Response,
  SecurityHints, Spec,
}
import intent/vision_types.{Scenario, VisionSection}

// ============================================================================
// Behavior Factories
// ============================================================================

/// Create a minimal behavior for testing
/// Takes name and list of required behaviors (dependencies)
pub fn make_test_behavior(name: String, requires: List(String)) -> Behavior {
  Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: Request(
      method: types.Get,
      path: "/" <> name,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

/// Create a behavior with custom HTTP method
pub fn make_test_behavior_with_method(
  name: String,
  method: types.Method,
  requires: List(String),
) -> Behavior {
  Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: Request(
      method: method,
      path: "/" <> name,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: 200,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

/// Create a behavior with custom expected status
pub fn make_test_behavior_with_status(
  name: String,
  expected_status: Int,
  requires: List(String),
) -> Behavior {
  Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: Request(
      method: types.Get,
      path: "/" <> name,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: expected_status,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

// ============================================================================
// Feature Factories
// ============================================================================

/// Create a feature from a list of behaviors
pub fn make_test_feature(name: String, behaviors: List(Behavior)) -> Feature {
  Feature(
    name: name,
    description: "Test feature: " <> name,
    behaviors: behaviors,
  )
}

// ============================================================================
// Spec Factories
// ============================================================================

/// Create a minimal spec from a list of features
pub fn make_test_spec(features: List(Feature)) -> Spec {
  Spec(
    name: "Test Spec",
    description: "Test spec for tests",
    audience: "developers",
    version: "1.0.0",
    success_criteria: [],
    config: Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    ),
    features: features,
    rules: [],
    anti_patterns: [],
    ai_hints: AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
    ),
  )
}

/// Create a spec with a custom name
pub fn make_test_spec_with_name(name: String, features: List(Feature)) -> Spec {
  Spec(..make_test_spec(features), name: name)
}

/// Create a spec from a list of behaviors (wraps in default feature)
pub fn make_test_spec_from_behaviors(behaviors: List(Behavior)) -> Spec {
  make_test_spec([make_test_feature("Default", behaviors)])
}

// ============================================================================
// Config and Request Factories
// ============================================================================

/// Create a minimal config for testing
pub fn make_test_config() -> Config {
  Config(
    base_url: "http://localhost:8080",
    timeout_ms: 5000,
    headers: dict.new(),
    allow_localhost: False,
  )
}

/// Create a config with custom base URL
pub fn make_test_config_with_url(base_url: String) -> Config {
  Config(
    base_url: base_url,
    timeout_ms: 5000,
    headers: dict.new(),
    allow_localhost: False,
  )
}

/// Create a minimal GET request for testing
pub fn make_test_request(path: String) -> Request {
  Request(
    method: types.Get,
    path: path,
    headers: dict.new(),
    query: dict.new(),
    body: json.null(),
  )
}

// ============================================================================
// Plan Factories (for Plan schema testing)
// ============================================================================

/// Create a complete, well-formed plan with no inversions
pub fn make_complete_plan() -> Plan {
  let vision =
    VisionSection(
      press_release: "Revolutionary meal planning app that saves time and reduces food waste",
      persona: "Busy professionals aged 25-45 with families",
      non_personas: ["Students", "Single diners", "Meal prep services"],
      north_star: "User plans weekly meals in under 5 minutes with zero food waste",
      scenarios: [
        Scenario(
          character: "Sarah",
          persona: "Working parent",
          motivation: "Save time and reduce waste",
          simulation: "Plans week in 3 minutes using smart suggestions",
          outcome: "Zero food waste, saved $50/week",
        ),
      ],
      replaces: Some("Manual meal planning and shopping lists"),
      vorp: "Automated suggestions save 30 minutes/week vs manual planning",
      out_of_scope: ["Recipe creation", "Grocery delivery", "Calorie tracking"],
    )

  let shape =
    ShapeSection(
      features: [
        FeatureShape(
          name: "Meal Planning",
          description: "Quick weekly meal planning interface",
        ),
        FeatureShape(
          name: "Shopping Lists",
          description: "Auto-generated shopping lists",
        ),
      ],
      critical_path: ["Meal Planning", "Shopping Lists"],
      mvp_slice: MVPSlice(
        description: "Basic meal planning with manual recipe selection",
        features: ["Meal Planning", "Shopping Lists"],
        shortcuts: [
          "Hardcode 50 recipes",
          "Skip personalization",
          "Manual shopping list editing",
        ],
      ),
      post_mvp: ["Recipe suggestions", "Dietary preferences", "Budget tracking"],
      validation_moment: "User successfully plans a week of meals and generates shopping list",
    )

  Plan(
    id: "test-plan-001",
    created_at: "2026-01-25T16:00:00Z",
    updated_at: "2026-01-25T16:00:00Z",
    vision: vision,
    shape: shape,
    spec: None,
    ready: None,
  )
}

/// Create a plan with empty persona (should trigger inversion)
pub fn make_plan_with_empty_persona() -> Plan {
  let plan = make_complete_plan()
  let vision = VisionSection(..plan.vision, persona: "")
  Plan(..plan, vision: vision)
}

/// Create a plan with empty scenarios (should trigger inversion)
pub fn make_plan_with_empty_scenarios() -> Plan {
  let plan = make_complete_plan()
  let vision = VisionSection(..plan.vision, scenarios: [])
  Plan(..plan, vision: vision)
}

/// Create a plan with no shortcuts in MVP (should trigger inversion)
pub fn make_plan_with_no_shortcuts() -> Plan {
  let plan = make_complete_plan()
  let mvp = MVPSlice(..plan.shape.mvp_slice, shortcuts: [])
  let shape = ShapeSection(..plan.shape, mvp_slice: mvp)
  Plan(..plan, shape: shape)
}

/// Create a plan with overly long critical path (should trigger inversion)
pub fn make_plan_with_long_critical_path() -> Plan {
  let plan = make_complete_plan()
  let shape =
    ShapeSection(..plan.shape, critical_path: [
      "F1",
      "F2",
      "F3",
      "F4",
      "F5",
      "F6",
      "F7",
    ])
  Plan(..plan, shape: shape)
}

/// Create a plan with misaligned vision and shape (should trigger inversion)
pub fn make_plan_with_misaligned_vision_shape() -> Plan {
  let plan = make_complete_plan()
  // Shape features don't appear in scenarios or critical path doesn't match MVP
  let shape =
    ShapeSection(
      ..plan.shape,
      critical_path: ["Unrelated Feature"],
      mvp_slice: MVPSlice(..plan.shape.mvp_slice, features: [
        "Different Feature",
      ]),
    )
  Plan(..plan, shape: shape)
}
