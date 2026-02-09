/// Ready document generator
/// Generates implementation-ready documentation from specs
import gleam/dict
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import intent/types.{type Behavior, type Rule, type Spec}

/// Generate a ready document from a spec
pub fn generate_ready_document(spec: Spec) -> String {
  let sections = [
    generate_header(spec),
    generate_overview(spec),
    generate_features(spec),
    generate_behaviors(spec),
    generate_invariants(spec),
    generate_verification_criteria(spec),
    generate_anti_patterns(spec),
    generate_implementation_hints(spec),
    generate_security_guidelines(spec),
  ]

  string.join(sections, "\n\n")
}

fn generate_header(spec: Spec) -> String {
  "# Ready Document: "
  <> spec.name
  <> "\n"
  <> "Version: "
  <> spec.version
  <> "\n"
  <> "Generated: "
  <> get_current_timestamp()
}

fn generate_overview(spec: Spec) -> String {
  "## Overview\n\n"
  <> "**Description:** "
  <> spec.description
  <> "\n\n"
  <> "**Audience:** "
  <> spec.audience
  <> "\n\n"
  <> "### Success Criteria\n\n"
  <> list.map(spec.success_criteria, fn(criteria) { "- " <> criteria })
  |> string.join("\n")
}

fn generate_features(spec: Spec) -> String {
  let features_list =
    list.map(spec.features, fn(feature) {
      "### "
      <> feature.name
      <> "\n\n"
      <> feature.description
    })
    |> string.join("\n\n")

  "## Features\n\n" <> features_list
}

fn generate_behaviors(spec: Spec) -> String {
  let behaviors_by_feature =
    list.map(spec.features, fn(feature) {
      let behaviors =
        list.map(feature.behaviors, fn(behavior) {
          generate_behavior_details(behavior)
        })
        |> string.join("\n\n")

      "### "
      <> feature.name
      <> "\n\n"
      <> behaviors
    })
    |> string.join("\n\n")

  "## Behaviors\n\n" <> behaviors_by_feature
}

fn generate_behavior_details(behavior: Behavior) -> String {
  let requires =
    case behavior.requires {
      [] -> ""
      deps ->
        "\n**Dependencies:** "
        <> string.join(deps, ", ")
        <> "\n"
    }

  let tags =
    case behavior.tags {
      [] -> ""
      tags_list ->
        "\n**Tags:** "
        <> string.join(tags_list, ", ")
        <> "\n"
    }

  let notes =
    case string.is_empty(behavior.notes) {
      True -> ""
      False -> "\n**Notes:** " <> behavior.notes <> "\n"
    }

  "#### "
  <> behavior.name
  <> "\n\n"
  <> "**Intent:** "
  <> behavior.intent
  <> "\n"
  <> requires
  <> tags
  <> notes
  <> "\n"
  <> generate_request_details(behavior.request)
  <> "\n\n"
  <> generate_response_details(behavior.response)
}

fn generate_request_details(request: types.Request) -> String {
  let headers =
    case dict.size(request.headers) {
      0 -> ""
      _ ->
        "\nHeaders:\n"
        <> dict.to_list(request.headers)
        |> list.map(fn(pair) {
          "  - "
          <> pair.0
          <> ": "
          <> pair.1
        })
        |> string.join("\n")
    }

  let query =
    case dict.size(request.query) {
      0 -> ""
      _ ->
        "\nQuery Parameters:\n"
        <> dict.to_list(request.query)
        |> list.map(fn(pair) {
          "  - "
          <> pair.0
          <> ": "
          <> json_to_string(pair.1)
        })
        |> string.join("\n")
    }

  let body_str = json.to_string(request.body)
  let body =
    case body_str {
      "null" -> ""
      _ ->
        "\nBody:\n```json\n"
        <> body_str
        <> "\n```"
    }

  "**Request:** "
  <> types.method_to_string(request.method)
  <> " "
  <> request.path
  <> headers
  <> query
  <> body
}

fn generate_response_details(response: types.Response) -> String {
  let checks =
    case dict.size(response.checks) {
      0 -> ""
      _ ->
        "\n\nChecks:\n"
        <> dict.to_list(response.checks)
        |> list.map(fn(pair) {
          "  - **"
          <> pair.0
          <> "**: "
          <> pair.1.rule
          <> "\n    *Why:* "
          <> pair.1.why
        })
        |> string.join("\n")
    }

  let headers =
    case dict.size(response.headers) {
      0 -> ""
      _ ->
        "\n\nHeaders:\n"
        <> dict.to_list(response.headers)
        |> list.map(fn(pair) {
          "  - "
          <> pair.0
          <> ": "
          <> pair.1
        })
        |> string.join("\n")
    }

  "**Response:** Status "
  <> int.to_string(response.status)
  <> "\n```json\n"
  <> json_to_string(response.example)
  <> "\n```"
  <> checks
  <> headers
}

fn generate_invariants(spec: Spec) -> String {
  "## Invariants\n\n"
  <> "These global rules apply to all behaviors:\n\n"
  <> list.map(spec.rules, fn(rule) {
    "### "
    <> rule.name
    <> "\n\n"
    <> rule.description
    <> "\n\n"
    <> generate_rule_details(rule)
  })
  |> string.join("\n\n")
}

fn generate_rule_details(rule: Rule) -> String {
  let when_clause =
    case rule.when {
      types.When("", _, _) -> ""
      types.When(status, method, path) ->
        "**When:** "
        <> case string.is_empty(status) {
          True -> "Any status"
          False -> "Status " <> status
        }
        <> ", "
        <> types.method_to_string(method)
        <> " "
        <> path
        <> "\n\n"
    }

  let checks = generate_rule_check_details(rule.check)

  when_clause <> checks
}

fn generate_rule_check_details(check: types.RuleCheck) -> String {
  let items =
    []
    |> append_non_empty_list("Body must not contain", check.body_must_not_contain)
    |> append_non_empty_list("Body must contain", check.body_must_contain)
    |> append_non_empty_list("Fields must exist", check.fields_must_exist)
    |> append_non_empty_list(
      "Fields must not exist",
      check.fields_must_not_exist,
    )
    |> append_non_empty_string("Header must exist", check.header_must_exist)
    |> append_non_empty_string(
      "Header must not exist",
      check.header_must_not_exist,
    )

  case items {
    [] -> ""
    _ ->
      "**Checks:**\n"
      <> string.join(items, "\n")
  }
}

fn append_non_empty_list(
  items: List(String),
  label: String,
  values: List(String),
) -> List(String) {
  case values {
    [] -> items
    _ ->
      items
      |> list.append([
        "- "
        <> label
        <> ": "
        <> string.join(values, ", "),
      ])
  }
}

fn append_non_empty_string(
  items: List(String),
  label: String,
  value: String,
) -> List(String) {
  case string.is_empty(value) {
    True -> items
    False ->
      items
      |> list.append(["- " <> label <> ": " <> value])
  }
}

fn generate_verification_criteria(_spec: Spec) -> String {
  "## Verification Criteria\n\n"
  <> "Each behavior should be verified against:\n\n"
  <> "1. **Request validation**: Ensure requests match the specified format\n"
  <> "2. **Response validation**: Verify responses match expected structure and values\n"
  <> "3. **Check assertions**: All specified checks must pass\n"
  <> "4. **Error handling**: Verify error cases are handled correctly\n"
  <> "5. **Edge cases**: Test boundary conditions and unusual inputs\n\n"
  <> "### Automated Testing\n\n"
  <> "All behaviors should have automated tests that:\n"
  <> "- Send the exact request specified\n"
  <> "- Validate the response status code\n"
  <> "- Check all response fields against their rules\n"
  <> "- Verify headers are correct\n"
  <> "- Test error conditions explicitly"
}

fn generate_anti_patterns(spec: Spec) -> String {
  "## Anti-Patterns to Avoid\n\n"
  <> list.map(spec.anti_patterns, fn(anti_pattern) {
    "### "
    <> anti_pattern.name
    <> "\n\n"
    <> anti_pattern.description
    <> "\n\n"
    <> "**Bad Example:**\n```json\n"
    <> json_to_string(anti_pattern.bad_example)
    <> "\n```\n\n"
    <> "**Good Example:**\n```json\n"
    <> json_to_string(anti_pattern.good_example)
    <> "\n```\n\n"
    <> "**Why:** "
    <> anti_pattern.why
  })
  |> string.join("\n\n")
}

fn generate_implementation_hints(spec: Spec) -> String {
  let hints = spec.ai_hints

  "## Implementation Hints\n\n"
  <> "### Suggested Stack\n\n"
  <> list.map(hints.implementation.suggested_stack, fn(item) { "- " <> item })
  |> string.join("\n")
  <> "\n\n"
  <> generate_entity_hints(hints.entities)
  <> "\n\n"
  <> generate_common_pitfalls(hints.pitfalls)
}

fn generate_entity_hints(entities: dict.Dict(String, types.EntityHint)) -> String {
  case dict.size(entities) {
    0 -> ""
    _ ->
      "### Entity Models\n\n"
      <> dict.to_list(entities)
      |> list.map(fn(pair) { generate_entity_hint(pair.0, pair.1) })
      |> string.join("\n\n")
  }
}

fn generate_entity_hint(name: String, hint: types.EntityHint) -> String {
  "**"
  <> name
  <> "**\n\n"
  <> "Fields:\n"
  <> dict.to_list(hint.fields)
  |> list.map(fn(pair) {
    "  - **"
    <> pair.0
    <> "**: "
    <> pair.1
  })
  |> string.join("\n")
}

fn generate_common_pitfalls(pitfalls: List(String)) -> String {
  case pitfalls {
    [] -> ""
    _ ->
      "### Common Pitfalls\n\n"
      <> list.map(pitfalls, fn(pitfall) { "- " <> pitfall })
      |> string.join("\n")
  }
}

fn generate_security_guidelines(spec: Spec) -> String {
  let security = spec.ai_hints.security

  "## Security Guidelines\n\n"
  <> "### Password Hashing\n\n"
  <> security.password_hashing
  <> "\n\n"
  <> "### JWT Configuration\n\n"
  <> "- **Algorithm:** "
  <> security.jwt_algorithm
  <> "\n"
  <> "- **Expiry:** "
  <> security.jwt_expiry
  <> "\n\n"
  <> "### Rate Limiting\n\n"
  <> security.rate_limiting
}

fn json_to_string(json: Json) -> String {
  json.to_string(json)
}

fn get_current_timestamp() -> String {
  // Get current timestamp in ISO 8601 format
  // This is a simplified version - in production you'd use a proper datetime library
  "2024-01-15T10:30:00Z"
}
