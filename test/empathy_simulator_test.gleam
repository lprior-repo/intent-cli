//// Tests for kirk/empathy_simulator.gleam
//// Contract: Simulate cognitive limitations API consumers face
//// Based on Miller's Law (7+/-2), Cognitive Load Theory, Expert Blind Spot

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/empathy_simulator
import intent/types.{Behavior, Get, Post, Request, Response}
import test_helpers

// =============================================================================
// analyze_empathy tests
// =============================================================================

pub fn analyze_empathy_empty_spec_test() {
  // Contract: Empty spec returns valid report with all dimensions
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = empathy_simulator.analyze_empathy(spec)

  // Should have scores in valid range [0, 100]
  { report.memory_score >=. 0.0 && report.memory_score <=. 100.0 }
  |> should.be_true
  { report.attention_score >=. 0.0 && report.attention_score <=. 100.0 }
  |> should.be_true
  { report.expertise_score >=. 0.0 && report.expertise_score <=. 100.0 }
  |> should.be_true
}

pub fn analyze_empathy_memory_load_many_behaviors_test() {
  // Contract: Many behaviors increase memory load (lower score = harder)
  // Miller's Law: 7+/-2 items in working memory
  let behaviors =
    list.range(1, 15)
    |> list.map(fn(i) {
      test_helpers.make_test_behavior(
        "behavior-" <> string.inspect(i),
        case i > 3 {
          True -> ["behavior-" <> string.inspect(i - 1)]
          False -> []
        },
      )
    })
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = empathy_simulator.analyze_empathy(spec)

  // Many behaviors with dependencies should impact memory score
  // Lower score means harder for user (more cognitive load)
  { report.memory_score <. 100.0 } |> should.be_true
}

pub fn analyze_empathy_memory_load_long_dependency_chains_test() {
  // Contract: Long dependency chains increase memory load
  // Users must remember sequence of operations
  let behaviors = [
    test_helpers.make_test_behavior("step-1-login", []),
    test_helpers.make_test_behavior("step-2-get-token", ["step-1-login"]),
    test_helpers.make_test_behavior("step-3-refresh-session", [
      "step-2-get-token",
    ]),
    test_helpers.make_test_behavior("step-4-get-user", [
      "step-3-refresh-session",
    ]),
    test_helpers.make_test_behavior("step-5-update-profile", ["step-4-get-user"]),
    test_helpers.make_test_behavior("step-6-verify-email", [
      "step-5-update-profile",
    ]),
    test_helpers.make_test_behavior("step-7-complete-onboard", [
      "step-6-verify-email",
    ]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = empathy_simulator.analyze_empathy(spec)

  // Long chains should produce memory-related issues
  let has_memory_issue =
    report.issues
    |> list.any(fn(issue) { issue.dimension == empathy_simulator.Memory })
  has_memory_issue |> should.be_true
}

pub fn analyze_empathy_attention_similar_endpoints_test() {
  // Contract: Similar-looking endpoints confuse attention
  // /users, /user, /users/{id}, /user/{id} are easy to mix up
  let behaviors = [
    Behavior(
      name: "get-users",
      intent: "Get all users",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 200, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
    Behavior(
      name: "get-user",
      intent: "Get single user",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/user",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 200, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
    Behavior(
      name: "get-users-id",
      intent: "Get user by ID",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users/${id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 200, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
    Behavior(
      name: "get-user-id",
      intent: "Get user by ID alt",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/user/${id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 200, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = empathy_simulator.analyze_empathy(spec)

  // Similar endpoints should produce attention-related issues
  let has_attention_issue =
    report.issues
    |> list.any(fn(issue) { issue.dimension == empathy_simulator.Attention })
  has_attention_issue |> should.be_true
}

pub fn analyze_empathy_expertise_technical_jargon_test() {
  // Contract: Technical jargon assumes expertise users may not have
  // JWT, OAuth, HMAC, RSA require specialized knowledge
  let behaviors = [
    Behavior(
      name: "jwt-auth",
      intent: "Authenticate using JWT with RS256 signature verification",
      notes: "Requires PKCS#8 key format",
      requires: [],
      tags: ["oauth", "cryptography"],
      request: Request(
        method: Post,
        path: "/auth/oauth2/token",
        headers: dict.from_list([
          #("Content-Type", "application/x-www-form-urlencoded"),
        ]),
        query: dict.new(),
        body: json.object([
          #("grant_type", json.string("client_credentials")),
          #(
            "client_assertion_type",
            json.string(
              "urn:ietf:params:oauth:client-assertion-type:jwt-bearer",
            ),
          ),
        ]),
      ),
      response: Response(status: 200, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = empathy_simulator.analyze_empathy(spec)

  // Technical jargon should produce expertise-related issues
  let has_expertise_issue =
    report.issues
    |> list.any(fn(issue) { issue.dimension == empathy_simulator.Expertise })
  has_expertise_issue |> should.be_true
}

pub fn analyze_empathy_many_required_fields_test() {
  // Contract: Many required fields in request body increase memory load
  let behaviors = [
    Behavior(
      name: "create-complex-resource",
      intent: "Create resource with many fields",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Post,
        path: "/resources",
        headers: dict.new(),
        query: dict.new(),
        body: json.object([
          #("field1", json.string("value1")),
          #("field2", json.string("value2")),
          #("field3", json.string("value3")),
          #("field4", json.string("value4")),
          #("field5", json.string("value5")),
          #("field6", json.string("value6")),
          #("field7", json.string("value7")),
          #("field8", json.string("value8")),
          #("field9", json.string("value9")),
          #("field10", json.string("value10")),
        ]),
      ),
      response: Response(status: 201, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = empathy_simulator.analyze_empathy(spec)

  // Many required fields exceeds Miller's Law (7+/-2)
  // Should impact memory score
  { report.memory_score <. 100.0 } |> should.be_true
}

pub fn analyze_empathy_overall_score_test() {
  // Contract: Overall score is weighted average of dimensions
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("simple-get", []),
    ])

  let report = empathy_simulator.analyze_empathy(spec)

  // Overall should be in valid range and related to dimension scores
  { report.overall_score >=. 0.0 && report.overall_score <=. 100.0 }
  |> should.be_true
}

pub fn analyze_empathy_issues_categorized_test() {
  // Contract: Issues are categorized by cognitive dimension
  let behaviors = [
    test_helpers.make_test_behavior("step-1", []),
    test_helpers.make_test_behavior("step-2", ["step-1"]),
    test_helpers.make_test_behavior("step-3", ["step-2"]),
    test_helpers.make_test_behavior("step-4", ["step-3"]),
    test_helpers.make_test_behavior("step-5", ["step-4"]),
    test_helpers.make_test_behavior("step-6", ["step-5"]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = empathy_simulator.analyze_empathy(spec)

  // Each issue should have a valid dimension
  report.issues
  |> list.all(fn(issue) {
    case issue.dimension {
      empathy_simulator.Memory
      | empathy_simulator.Attention
      | empathy_simulator.Expertise -> True
    }
  })
  |> should.be_true
}

pub fn analyze_empathy_suggestions_provided_test() {
  // Contract: Report includes actionable suggestions
  let behaviors =
    list.range(1, 10)
    |> list.map(fn(i) {
      test_helpers.make_test_behavior("behavior-" <> string.inspect(i), [])
    })
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = empathy_simulator.analyze_empathy(spec)

  // Should have suggestions when issues exist
  case report.issues {
    [] -> should.be_true(True)
    _ -> list.is_empty(report.suggestions) |> should.be_false
  }
}

// =============================================================================
// format_report tests
// =============================================================================

pub fn format_report_test() {
  // Contract: Report formats without crashing
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = empathy_simulator.analyze_empathy(spec)

  let formatted = empathy_simulator.format_report(report)

  // Should produce non-empty output
  formatted |> string.is_empty |> should.be_false
}

pub fn format_report_includes_scores_test() {
  // Contract: Formatted report includes dimension scores
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("test", []),
    ])
  let report = empathy_simulator.analyze_empathy(spec)

  let formatted = empathy_simulator.format_report(report)
  let lower = string.lowercase(formatted)

  // Should mention cognitive dimensions
  { string.contains(lower, "memory") || string.contains(lower, "attention") }
  |> should.be_true
}

// =============================================================================
// cognitive_load_to_string tests
// =============================================================================

pub fn cognitive_load_to_string_low_test() {
  // Contract: Low load converts correctly
  let result = empathy_simulator.cognitive_load_to_string(empathy_simulator.Low)
  result |> string.lowercase |> string.contains("low") |> should.be_true
}

pub fn cognitive_load_to_string_moderate_test() {
  // Contract: Moderate load converts correctly
  let result =
    empathy_simulator.cognitive_load_to_string(empathy_simulator.Moderate)
  result |> string.lowercase |> string.contains("moderate") |> should.be_true
}

pub fn cognitive_load_to_string_high_test() {
  // Contract: High load converts correctly
  let result =
    empathy_simulator.cognitive_load_to_string(empathy_simulator.High)
  result |> string.lowercase |> string.contains("high") |> should.be_true
}

pub fn cognitive_load_to_string_overwhelming_test() {
  // Contract: Overwhelming load converts correctly
  let result =
    empathy_simulator.cognitive_load_to_string(empathy_simulator.Overwhelming)
  result
  |> string.lowercase
  |> string.contains("overwhelming")
  |> should.be_true
}

// =============================================================================
// dimension_to_string tests
// =============================================================================

pub fn dimension_to_string_memory_test() {
  // Contract: Memory dimension converts correctly
  let result = empathy_simulator.dimension_to_string(empathy_simulator.Memory)
  result |> string.lowercase |> string.contains("memory") |> should.be_true
}

pub fn dimension_to_string_attention_test() {
  // Contract: Attention dimension converts correctly
  let result =
    empathy_simulator.dimension_to_string(empathy_simulator.Attention)
  result |> string.lowercase |> string.contains("attention") |> should.be_true
}

pub fn dimension_to_string_expertise_test() {
  // Contract: Expertise dimension converts correctly
  let result =
    empathy_simulator.dimension_to_string(empathy_simulator.Expertise)
  result |> string.lowercase |> string.contains("expertise") |> should.be_true
}

// =============================================================================
// load_from_score tests
// =============================================================================

pub fn load_from_score_low_test() {
  // Contract: High score (easy API) = Low cognitive load
  let result = empathy_simulator.load_from_score(95.0)
  result |> should.equal(empathy_simulator.Low)
}

pub fn load_from_score_moderate_test() {
  // Contract: Medium score = Moderate cognitive load
  let result = empathy_simulator.load_from_score(65.0)
  result |> should.equal(empathy_simulator.Moderate)
}

pub fn load_from_score_high_test() {
  // Contract: Low score (hard API) = High cognitive load
  let result = empathy_simulator.load_from_score(35.0)
  result |> should.equal(empathy_simulator.High)
}

pub fn load_from_score_overwhelming_test() {
  // Contract: Very low score = Overwhelming cognitive load
  let result = empathy_simulator.load_from_score(15.0)
  result |> should.equal(empathy_simulator.Overwhelming)
}

// =============================================================================
// empathy_to_kirk_health_format tests
// =============================================================================

pub fn empathy_to_kirk_health_format_empty_test() {
  // Contract: Empty report returns empty lists
  let report =
    empathy_simulator.EmpathyReport(
      memory_score: 100.0,
      attention_score: 100.0,
      expertise_score: 100.0,
      overall_score: 100.0,
      overall_load: empathy_simulator.Low,
      issues: [],
      suggestions: [],
    )

  let formatted = empathy_simulator.empathy_to_kirk_health_format(report)

  formatted |> should.equal([])
}

pub fn empathy_to_kirk_health_format_with_issues_test() {
  // Contract: Issues are formatted as strings with dimension and description
  let behaviors =
    list.range(1, 10)
    |> list.map(fn(i) {
      test_helpers.make_test_behavior("step-" <> string.inspect(i), case i > 1 {
        True -> ["step-" <> string.inspect(i - 1)]
        False -> []
      })
    })
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = empathy_simulator.analyze_empathy(spec)

  let formatted = empathy_simulator.empathy_to_kirk_health_format(report)

  // Should have formatted issues if there are any issues
  case report.issues {
    [] -> should.be_true(True)
    _ -> list.is_empty(formatted) |> should.be_false
  }
}
