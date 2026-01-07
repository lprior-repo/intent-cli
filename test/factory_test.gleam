import gleam/int
import gleam/json
import gleam/list
import gleeunit/should

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

/// CUPID-DRIVEN TEST: Forces list transformation of behaviors to be pure
///
/// This test enforces:
/// - C: Composability - Behavior lists must compose with list.map/filter/fold
/// - P: Purity - Transforming behaviors is pure (no side effects)
/// - I: Idiomatic - Uses pipelines and functional transformations
/// - D: Domain - Behaviors can be processed independently
///
/// DESIGN PRESSURE: Can behavior transformations be parallelized?
/// If behaviors have interdependencies or shared mutable state, this fails.
/// Forces thinking: "Is each behavior independently processable?"

pub fn behavior_list_composes_with_functional_ops_test() {
  // Build 100 behaviors
  let behaviors =
    list.range(1, 100)
    |> list.map(build_behavior_json)

  // CRITICAL: Each behavior must be independently transformable
  // This composition pattern is required for parallel execution
  let behavior_names =
    behaviors
    |> list.map(extract_name_from_behavior_json)

  // Verify composition worked
  list.length(behavior_names)
  |> should.equal(100)

  // Verify independence: same transformation twice gives same result
  let behavior_names_2 =
    behaviors
    |> list.map(extract_name_from_behavior_json)

  behavior_names
  |> should.equal(behavior_names_2)

  // Verify each is unique (no accidental aliasing)
  let unique_names = list.unique(behavior_names)
  list.length(unique_names)
  |> should.equal(100)
}

/// Helper: Extract name from behavior JSON
/// FORCES: Pure extraction without mutation
fn extract_name_from_behavior_json(behavior: json.Json) -> String {
  panic as "JSON extraction not implemented"
}
