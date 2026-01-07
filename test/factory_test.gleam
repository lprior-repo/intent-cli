import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleeunit/should
import intent/http_client
import intent/interpolate
import intent/spec_builder
import intent/types

/// CUPID-DRIVEN: Spec builder MUST compose via list operations
///
/// DESIGN PRESSURE: Can you build a Spec with 50 behaviors without loops/mutation?
///
/// Forces:
/// - C: Composability - list.range |> list.map builds behaviors
/// - U: Single purpose - One builder fn per type (Behavior, Feature, Spec)
/// - P: Purity - Deterministic generation from index
/// - I: Idiomatic - Pipelines, no imperative code
/// - D: Domain - Builder functions express spec construction
///
/// 30-LINE RULE: Implementer needs ~20 lines for pure builder in src/intent/spec_builder.gleam
///
/// DISCOMFORT: "How do I build N behaviors without for-loops?"
/// Answer forces functional thinking: list.range |> list.map(builder_fn)

pub fn spec_with_many_behaviors_composes_functionally_test() {
  // Must exist: src/intent/spec_builder.gleam with create_test_spec
  let spec = spec_builder.create_test_spec(50)

  // Verify it built 1 feature
  list.length(spec.features)
  |> should.equal(1)

  // Verify the feature has 50 behaviors
  let assert [feature] = spec.features
  list.length(feature.behaviors)
  |> should.equal(50)

  // Verify purity: same input = same output (deterministic)
  let spec2 = spec_builder.create_test_spec(50)
  spec.name
  |> should.equal(spec2.name)

  let assert [feature2] = spec2.features
  list.length(feature2.behaviors)
  |> should.equal(50)
}

/// CUPID-DRIVEN: Batch checker MUST be pure for parallel map
///
/// DESIGN PRESSURE: Can check_response run 1000 times in parallel via list.map?
///
/// Forces:
/// - C: Composability - check_many composes check_response via list.map
/// - U: Single purpose - check_response is PURE: (expected, actual, ctx) -> result
/// - P: Purity - No side effects = safe for parallel list.map
/// - I: Idiomatic - behaviors |> list.map(check_one) creates results list
/// - D: Domain - "batch checking" speaks to performance requirements
///
/// 30-LINE RULE: Implementer needs ~15 lines in src/intent/spec_builder.gleam:
///   pub fn check_many(behaviors, results, ctx) {
///     list.map2(behaviors, results, fn(b, r) { checker.check_response(...) })
///   }
///
/// DISCOMFORT: "Is check_response pure enough for parallel execution?"
/// Forces auditing checker.gleam for hidden side effects (IO, mutation, etc)
///
/// PERFORMANCE GATE: This test must complete fast (< 100ms for 1000 checks)
/// If it's slow, check_response has O(n²) bugs or isn't parallelizable
pub fn check_response_is_pure_enough_for_batch_checking_test() {
  // Build 1000 dummy behaviors and execution results
  let behaviors =
    list.range(1, 1000)
    |> list.map(fn(i) {
      types.Behavior(
        name: "behavior_" <> int.to_string(i),
        intent: "test",
        notes: "",
        requires: [],
        tags: [],
        request: types.Request(
          method: types.Get,
          path: "/test",
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
    })

  let results =
    list.range(1, 1000)
    |> list.map(fn(_) {
      http_client.ExecutionResult(
        status: 200,
        body: json.null(),
        headers: dict.new(),
        raw_body: "null",
        elapsed_ms: 10,
        request_method: types.Get,
        request_path: "/test",
      )
    })

  let ctx = interpolate.new_context()

  // FORCE: Must use spec_builder.check_many which uses list.map internally
  // This proves check_response is composable via pure list operations
  let check_results = spec_builder.check_many(behaviors, results, ctx)

  // Verify all 1000 checks completed
  list.length(check_results)
  |> should.equal(1000)

  // Verify all passed (status 200 = 200, no checks = no failures)
  check_results
  |> list.all(fn(r) { r.status_ok })
  |> should.be_true
}
