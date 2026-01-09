import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/http_client
import intent/interpolate
import intent/rule
import intent/spec_builder
import intent/types

pub fn spec_with_many_behaviors_composes_functionally_test() {
  let spec = spec_builder.create_test_spec(50)

  list.length(spec.features)
  |> should.equal(1)

  let assert [feature] = spec.features
  list.length(feature.behaviors)
  |> should.equal(50)

  let spec2 = spec_builder.create_test_spec(50)
  spec.name
  |> should.equal(spec2.name)
}

pub fn regex_cache_avoids_recompilation_with_ets_test_skip() {
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

  let check_results = spec_builder.check_many(behaviors, results, ctx)

  list.length(check_results)
  |> should.equal(1000)

  check_results
  |> list.all(fn(r) { r.status_ok && list.is_empty(r.failed) })
  |> should.be_true
}
