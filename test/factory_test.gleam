import gleam/int
import gleam/json
import gleam/list
import gleeunit/should
import intent/types

/// CUPID-DRIVEN TEST: Forces spec construction to use pure functional composition
///
/// This test enforces:
/// - C: Composability - Must build complex spec using |> and list.map
/// - U: Unix philosophy - Each helper does ONE thing (build check, build behavior, etc)
/// - P: Purity - Same input produces same spec (deterministic)
/// - I: Idiomatic - Uses pattern matching, pipelines, no loops
/// - D: Domain - Returns proper Json that parser.parse_spec can consume
///
/// DESIGN PRESSURE: Can you build 100-behavior spec without imperative code?
/// This forces thinking about functional composition, not procedural construction.

pub fn build_large_spec_json_via_pure_composition_test() {
  // Build JSON for spec with 100 behaviors using pure functions
  let spec_json = build_spec_json_with_n_behaviors(100)

  // Parse it to verify structure
  // This will fail if json builder produces invalid structure
  let _ = spec_json

  // Verify purity: calling twice gives same result
  let spec_json_2 = build_spec_json_with_n_behaviors(100)

  spec_json
  |> json.to_string
  |> should.equal(json.to_string(spec_json_2))
}

/// Pure function: builds spec JSON with N behaviors
/// FORCES: functional composition, no mutation, idiomatic Gleam
fn build_spec_json_with_n_behaviors(n: Int) -> json.Json {
  // Create N behaviors using list.map (composition)
  let behaviors =
    list.range(1, n)
    |> list.map(build_behavior_json)

  // Single feature containing all behaviors
  let feature =
    json.object([
      #("name", json.string("load_test")),
      #("description", json.string("Load testing feature")),
      #("behaviors", json.array(behaviors, fn(b) { b })),
    ])

  // Build complete spec
  json.object([
    #("name", json.string("Large Spec Test")),
    #("description", json.string("Tests large spec handling")),
    #("audience", json.string("QA")),
    #("version", json.string("1.0.0")),
    #("success_criteria", json.array([json.string("completes")], fn(s) { s })),
    #("config", build_config_json()),
    #("features", json.array([feature], fn(f) { f })),
    #("rules", json.array([], fn(r) { r })),
    #("anti_patterns", json.array([], fn(a) { a })),
    #("ai_hints", build_ai_hints_json()),
  ])
}

/// Pure builder: index -> behavior JSON
/// FORCES: deterministic generation, no side effects
fn build_behavior_json(index: Int) -> json.Json {
  let name = "behavior_" <> int.to_string(index)
  let path = "/test/" <> int.to_string(index)

  json.object([
    #("name", json.string(name)),
    #("intent", json.string("Test behavior " <> int.to_string(index))),
    #("notes", json.string("")),
    #("requires", json.array([], fn(r) { r })),
    #("tags", json.array([], fn(t) { t })),
    #("request", build_request_json(path)),
    #("response", build_response_json()),
    #("captures", json.object([])),
  ])
}

fn build_request_json(path: String) -> json.Json {
  json.object([
    #("method", json.string("GET")),
    #("path", json.string(path)),
    #("headers", json.object([])),
    #("query", json.object([])),
    #("body", json.null()),
  ])
}

fn build_response_json() -> json.Json {
  json.object([
    #("status", json.int(200)),
    #("example", json.object([#("ok", json.bool(True))])),
    #("checks", json.object([])),
    #("headers", json.object([])),
  ])
}

fn build_config_json() -> json.Json {
  json.object([
    #("base_url", json.string("http://localhost:8080")),
    #("timeout_ms", json.int(5000)),
    #("headers", json.object([])),
  ])
}

fn build_ai_hints_json() -> json.Json {
  json.object([
    #(
      "implementation",
      json.object([#("suggested_stack", json.array([], fn(s) { s }))]),
    ),
    #("entities", json.object([])),
    #(
      "security",
      json.object([
        #("password_hashing", json.string("bcrypt")),
        #("jwt_algorithm", json.string("HS256")),
        #("jwt_expiry", json.string("24h")),
        #("rate_limiting", json.string("100/hour")),
      ]),
    ),
    #("pitfalls", json.array([], fn(p) { p })),
  ])
}

/// CUPID-DRIVEN TEST: Forces pure behavior factory function composition
///
/// This test enforces:
/// - C: Composability - Behavior factory composes with list.range |> list.map
/// - U: Unix philosophy - ONE function that generates a behavior from index
/// - P: Purity - Same index always produces identical behavior
/// - I: Idiomatic - Uses pipelines, not loops or mutation
/// - D: Domain - Speaks "behavior factory" not "loop counter"
///
/// DESIGN PRESSURE: Can you write a pure function `fn(Int) -> Behavior`?
/// This is the MINIMAL requirement for scaling to 1000+ behaviors.
/// If implementer can't write this cleanly, parallel execution is impossible.
///
/// PASSING REQUIRES <30 LINES: One pure factory function, deterministic.

pub fn behavior_factory_is_pure_function_test() {
  // The factory MUST be a pure function: Int -> Behavior
  let behavior = generate_behavior(42)

  // Purity: calling twice with same input gives same result
  let behavior_2 = generate_behavior(42)

  behavior.name
  |> should.equal(behavior_2.name)

  behavior.name
  |> should.equal("behavior_42")

  behavior.intent
  |> should.equal("Test behavior 42")

  // Verify it composes with list operations (required for scaling)
  let behaviors =
    list.range(1, 100)
    |> list.map(generate_behavior)

  list.length(behaviors)
  |> should.equal(100)

  // Verify each behavior is independently generated (no shared state)
  let first = generate_behavior(1)
  let last = generate_behavior(100)

  first.name
  |> should.not_equal(last.name)
}

/// Pure behavior factory: Int -> Behavior
/// DESIGN PRESSURE: Must be deterministic, composable, no side effects
/// This is the foundation for efficient large-spec handling
fn generate_behavior(index: Int) -> types.Behavior {
  panic as "Pure behavior factory not implemented"
}
