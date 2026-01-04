import gleam/dict
import gleam/list
import gleam/option.{None}
import gleeunit
import gleeunit/should
import intent/interpolate
import intent/resolver
import intent/types

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Resolver Tests
// ============================================================================

fn make_behavior(name: String, requires: List(String)) -> types.Behavior {
  types.Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: requires,
    tags: [],
    request: types.Request(
      method: types.Get,
      path: "/" <> name,
      headers: dict.new(),
      query: dict.new(),
      body: None,
    ),
    response: types.Response(
      status: 200,
      example: None,
      checks: dict.new(),
      headers: None,
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
    audience: "",
    version: "1.0.0",
    success_criteria: [],
    config: types.Config(
      base_url: "http://localhost",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
    features: features,
    rules: [],
    anti_patterns: [],
    ai_hints: None,
  )
}

pub fn resolver_simple_no_deps_test() {
  // Three behaviors with no dependencies - all should be executable
  let b1 = make_behavior("first", [])
  let b2 = make_behavior("second", [])
  let b3 = make_behavior("third", [])

  let spec = make_spec([make_feature("Feature A", [b1, b2, b3])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_linear_dependency_chain_test() {
  // b3 -> b2 -> b1 (b3 depends on b2, b2 depends on b1)
  let b1 = make_behavior("first", [])
  let b2 = make_behavior("second", ["first"])
  let b3 = make_behavior("third", ["second"])

  let spec = make_spec([make_feature("Feature A", [b1, b2, b3])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)

      // Verify order: first, second, third
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      names
      |> should.equal(["first", "second", "third"])
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_multiple_deps_on_one_test() {
  // b2 and b3 both depend on b1
  let b1 = make_behavior("base", [])
  let b2 = make_behavior("child-a", ["base"])
  let b3 = make_behavior("child-b", ["base"])

  let spec = make_spec([make_feature("Feature A", [b1, b2, b3])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)

      // First should be base
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let assert [first, ..] = names
      first
      |> should.equal("base")
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_missing_dependency_test() {
  // b1 depends on "nonexistent" which doesn't exist
  let b1 = make_behavior("first", ["nonexistent"])

  let spec = make_spec([make_feature("Feature A", [b1])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(_) -> should.fail()
    Error(resolver.MissingDependency(behavior, missing)) -> {
      behavior
      |> should.equal("first")
      missing
      |> should.equal("nonexistent")
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_cyclic_dependency_test() {
  // b1 -> b2 -> b1 (cycle)
  let b1 = make_behavior("first", ["second"])
  let b2 = make_behavior("second", ["first"])

  let spec = make_spec([make_feature("Feature A", [b1, b2])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(_) -> should.fail()
    Error(resolver.CyclicDependency(_)) -> should.be_ok(Ok(Nil))
    Error(_) -> should.fail()
  }
}

pub fn resolver_duplicate_name_test() {
  // Two behaviors with the same name
  let b1 = make_behavior("same-name", [])
  let b2 = make_behavior("same-name", [])

  let spec = make_spec([make_feature("Feature A", [b1, b2])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(_) -> should.fail()
    Error(resolver.DuplicateBehaviorName(name)) -> {
      name
      |> should.equal("same-name")
    }
    Error(_) -> should.fail()
  }
}

pub fn resolver_cross_feature_deps_test() {
  // b2 in Feature B depends on b1 in Feature A
  let b1 = make_behavior("base", [])
  let b2 = make_behavior("dependent", ["base"])

  let spec =
    make_spec([
      make_feature("Feature A", [b1]),
      make_feature("Feature B", [b2]),
    ])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(2)

      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      names
      |> should.equal(["base", "dependent"])
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Interpolation Tests
// ============================================================================

pub fn interpolate_simple_variable_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("user_id", json_string("12345"))

  let result = interpolate.interpolate_string(ctx, "/users/${user_id}")

  result
  |> should.be_ok

  case result {
    Ok(s) ->
      s
      |> should.equal("/users/12345")
    Error(_) -> should.fail()
  }
}

pub fn interpolate_multiple_variables_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("org", json_string("acme"))
    |> interpolate.set_variable("team", json_string("dev"))

  let result = interpolate.interpolate_string(ctx, "/orgs/${org}/teams/${team}")

  case result {
    Ok(s) ->
      s
      |> should.equal("/orgs/acme/teams/dev")
    Error(_) -> should.fail()
  }
}

pub fn interpolate_missing_variable_test() {
  let ctx = interpolate.new_context()

  let result = interpolate.interpolate_string(ctx, "/users/${unknown}")

  result
  |> should.be_error
}

pub fn interpolate_no_variables_test() {
  let ctx = interpolate.new_context()

  let result = interpolate.interpolate_string(ctx, "/users/static")

  case result {
    Ok(s) ->
      s
      |> should.equal("/users/static")
    Error(_) -> should.fail()
  }
}

// Helper to create a JSON string value
import gleam/json

fn json_string(s: String) -> json.Json {
  json.string(s)
}
