//// Shared test helpers for Intent CLI tests
//// This module provides common factory functions for creating test data

import gleam/dict
import gleam/json
import gleam/option.{None}
import intent/types.{
  type Behavior, type Config, type Feature, type Request, type Spec, AIHints,
  Behavior, Config, Feature, ImplementationHints, Request, Response,
  SecurityHints, Spec,
}

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
      codebase: None,
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
  )
}

/// Create a config with custom base URL
pub fn make_test_config_with_url(base_url: String) -> Config {
  Config(base_url: base_url, timeout_ms: 5000, headers: dict.new())
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
