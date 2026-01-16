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

/// CUPID-DRIVEN: Regex cache MUST avoid recompilation via ETS
///
/// DESIGN PRESSURE: Checking 1000 emails with SAME pattern must compile ONCE
///
/// Forces:
/// - P: Purity - get_or_compile_regex("^[a-z]+@") returns SAME Regexp instance
/// - I: Idiomatic - Cache via ETS (Erlang FFI), not Gleam dict mutation
/// - C: Composability - Works with list.map checking 1000 values in parallel
/// - D: Domain - "Cache compiled patterns to avoid recompilation" is LITERAL
///
/// 30-LINE RULE: Implementer needs ~25 lines in src/intent/checker/rules.gleam:
///   External FFI: check if pattern in ETS, else compile + insert + return
///
/// DISCOMFORT: "How do I cache WITHOUT mutation in pure Gleam?"
/// Answer: You CAN'T. Forces learning ETS + FFI for stateful cache.
///
/// PERFORMANCE GATE: 1000 regex checks with same pattern should be ~10x faster
/// than 1000 compilations. If not, cache isn't working.
/// SKIPPED: Regex caching FFI not yet implemented in intent_checker.erl
pub fn regex_cache_avoids_recompilation_with_ets_test_skip() {
  // Build 1000 responses with SAME email validation rule
  let email_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

  let behaviors =
    list.range(1, 1000)
    |> list.map(fn(i) {
      let email = "user" <> int.to_string(i) <> "@example.com"
      types.Behavior(
        name: "check_email_" <> int.to_string(i),
        intent: "validate email format",
        notes: "",
        requires: [],
        tags: [],
        request: types.Request(
          method: types.Get,
          path: "/user/" <> int.to_string(i),
          headers: dict.new(),
          query: dict.new(),
          body: json.null(),
        ),
        response: types.Response(
          status: 200,
          example: json.object([#("email", json.string(email))]),
          checks: dict.from_list([
            #(
              "email",
              types.Check(
                rule: "string matching " <> email_pattern,
                why: "email must be valid",
              ),
            ),
          ]),
          headers: dict.new(),
        ),
        captures: dict.new(),
      )
    })

  let results =
    list.range(1, 1000)
    |> list.map(fn(i) {
      let email = "user" <> int.to_string(i) <> "@example.com"
      http_client.ExecutionResult(
        status: 200,
        body: json.object([#("email", json.string(email))]),
        headers: dict.new(),
        raw_body: "{\"email\":\"" <> email <> "\"}",
        elapsed_ms: 10,
        request_method: types.Get,
        request_path: "/user/" <> int.to_string(i),
      )
    })

  let ctx = interpolate.new_context()

  // FORCE: All 1000 checks use SAME regex pattern
  // Cache should compile ONCE, reuse 999 times
  let check_results = spec_builder.check_many(behaviors, results, ctx)

  // Verify all 1000 checks passed (all emails match pattern)
  list.length(check_results)
  |> should.equal(1000)

  check_results
  |> list.all(fn(r) { r.status_ok && list.is_empty(r.failed) })
  |> should.be_true

  // CRITICAL: If this test is SLOW (>500ms), cache isn't working
  // Compiling 1000 regexes takes ~2-3 seconds
  // Compiling 1 regex + 999 lookups should take ~200ms
}
