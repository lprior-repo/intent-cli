import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/spec_critique.{
  type CritiqueIssue, type CritiqueResult, CoverageGaps, Critical, CritiqueIssue,
  CritiqueResult, EdgeCaseGaps, FailureBlastRadius, Warning, critique_spec,
  validate_coverage_gaps, validate_edge_cases, validate_failure_blast_radius,
}
import intent/types.{
  type Behavior, type Feature, type Spec, AIHints, AntiPattern, Behavior, Check,
  Config, Feature, Get, ImplementationHints, Post, Request, Response,
  SecurityHints, Spec,
}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Test Fixtures
// =============================================================================

fn perfect_spec() -> Spec {
  Spec(
    name: "User API",
    description: "Complete user management API with edge cases",
    audience: "Backend engineers",
    version: "1.0.0",
    success_criteria: [
      "All CRUD operations tested",
      "Error handling validated",
      "Edge cases covered",
    ],
    config: Config(
      base_url: "http://api.example.com",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    ),
    features: [
      Feature(
        name: "User Management",
        description: "CRUD operations for users",
        behaviors: [
          make_behavior(
            "get_user_success",
            Get,
            200,
            tags: ["happy-path"],
            checks: dict.from_list([
              #("id", Check("body.id > 0", "User ID must be positive")),
            ]),
          ),
          make_behavior(
            "get_user_not_found",
            Get,
            404,
            tags: ["error", "edge-case"],
            checks: dict.from_list([
              #("error", Check("body.error != null", "Error message required")),
            ]),
          ),
          make_behavior(
            "create_user_success",
            Post,
            201,
            tags: ["happy-path"],
            checks: dict.new(),
          ),
          make_behavior(
            "create_user_validation_error",
            Post,
            422,
            tags: ["error", "edge-case", "validation"],
            checks: dict.new(),
          ),
          make_behavior(
            "create_user_duplicate",
            Post,
            409,
            tags: ["error", "edge-case", "duplicate"],
            checks: dict.new(),
          ),
        ],
      ),
    ],
    rules: [],
    anti_patterns: [
      AntiPattern(
        name: "Exposing internal IDs",
        description: "Don't leak database IDs in errors",
        bad_example: json.object([
          #("error", json.string("User 12345 not found")),
        ]),
        good_example: json.object([#("error", json.string("User not found"))]),
        why: "Prevents information disclosure",
      ),
    ],
    ai_hints: AIHints(
      implementation: ImplementationHints(["gleam", "wisp"]),
      entities: dict.new(),
      security: SecurityHints("bcrypt", "HS256", "1h", "100/hour"),
      pitfalls: [
        "Validate email format before creating user",
        "Handle duplicate email gracefully",
      ],
    ),
  )
}

fn make_behavior(
  name: String,
  method: types.Method,
  status: Int,
  tags tags: List(String),
  checks checks: dict.Dict(String, Check),
) -> Behavior {
  Behavior(
    name: name,
    intent: "Test " <> name,
    notes: "",
    requires: [],
    tags: tags,
    request: Request(
      method: method,
      path: "/users",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: status,
      example: json.null(),
      checks: checks,
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

fn minimal_spec() -> Spec {
  Spec(
    name: "Minimal",
    description: "Minimal spec",
    audience: "Test",
    version: "1.0.0",
    success_criteria: [],
    config: Config("http://test", 1000, dict.new(), False),
    features: [],
    rules: [],
    anti_patterns: [],
    ai_hints: AIHints(
      ImplementationHints([]),
      dict.new(),
      SecurityHints("", "", "", ""),
      [],
    ),
  )
}

// =============================================================================
// Coverage Gaps Tests
// =============================================================================

pub fn validate_coverage_gaps_perfect_spec_test() {
  let spec = perfect_spec()
  let issues = validate_coverage_gaps(spec)
  issues |> should.equal([])
}

pub fn validate_coverage_gaps_no_behaviors_test() {
  let spec = minimal_spec()
  let issues = validate_coverage_gaps(spec)
  issues |> should.not_equal([])
  let has_critical =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(CoverageGaps, Critical, _, _) -> True
        _ -> False
      }
    })
  has_critical |> should.be_true
}

pub fn validate_coverage_gaps_no_error_behaviors_test() {
  let spec =
    Spec(..minimal_spec(), features: [
      Feature("Test", "Test", [
        make_behavior("success", Get, 200, tags: [], checks: dict.new()),
      ]),
    ])
  let issues = validate_coverage_gaps(spec)
  let has_error_warning =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(CoverageGaps, _, msg, _) ->
          string.contains(msg, "error") || string.contains(msg, "4xx")
        _ -> False
      }
    })
  has_error_warning |> should.be_true
}

pub fn validate_coverage_gaps_single_method_warning_test() {
  let spec =
    Spec(..minimal_spec(), features: [
      Feature("Test", "Test", [
        make_behavior("b1", Get, 200, tags: [], checks: dict.new()),
        make_behavior("b2", Get, 404, tags: [], checks: dict.new()),
      ]),
    ])
  let issues = validate_coverage_gaps(spec)
  let has_method_warning =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(CoverageGaps, Warning, msg, _) ->
          string.contains(msg, "method")
        _ -> False
      }
    })
  has_method_warning |> should.be_true
}

// =============================================================================
// Edge Case Tests
// =============================================================================

pub fn validate_edge_cases_perfect_spec_test() {
  let spec = perfect_spec()
  let issues = validate_edge_cases(spec)
  issues |> should.equal([])
}

pub fn validate_edge_cases_no_tags_test() {
  let spec =
    Spec(
      ..minimal_spec(),
      features: [
        Feature("Test", "Test", [
          make_behavior("b1", Get, 200, tags: [], checks: dict.new()),
          make_behavior("b2", Post, 201, tags: [], checks: dict.new()),
          make_behavior("b3", Get, 404, tags: [], checks: dict.new()),
        ]),
      ],
      anti_patterns: [
        AntiPattern("test", "test", json.null(), json.null(), "test reason"),
      ],
    )
  let issues = validate_edge_cases(spec)
  let has_tag_warning =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(EdgeCaseGaps, Warning, msg, _) ->
          string.contains(msg, "edge") || string.contains(msg, "tag")
        _ -> False
      }
    })
  has_tag_warning |> should.be_true
}

pub fn validate_edge_cases_no_anti_patterns_test() {
  let spec =
    Spec(
      ..minimal_spec(),
      features: [
        Feature("Test", "Test", [
          make_behavior("b1", Get, 200, tags: ["edge-case"], checks: dict.new()),
        ]),
      ],
      anti_patterns: [],
    )
  let issues = validate_edge_cases(spec)
  let has_anti_pattern_critical =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(EdgeCaseGaps, Critical, msg, _) ->
          string.contains(msg, "anti")
        _ -> False
      }
    })
  has_anti_pattern_critical |> should.be_true
}

// =============================================================================
// Failure Blast Radius Tests
// =============================================================================

pub fn validate_failure_blast_radius_perfect_spec_test() {
  let spec = perfect_spec()
  let issues = validate_failure_blast_radius(spec)
  issues |> should.equal([])
}

pub fn validate_failure_blast_radius_no_pitfalls_test() {
  let spec =
    Spec(
      ..perfect_spec(),
      ai_hints: AIHints(
        ImplementationHints([]),
        dict.new(),
        SecurityHints("", "", "", ""),
        pitfalls: [],
      ),
    )
  let issues = validate_failure_blast_radius(spec)
  let has_pitfall_warning =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(FailureBlastRadius, Warning, msg, _) ->
          string.contains(msg, "pitfall")
        _ -> False
      }
    })
  has_pitfall_warning |> should.be_true
}

pub fn validate_failure_blast_radius_no_error_behaviors_test() {
  let spec =
    Spec(..minimal_spec(), features: [
      Feature("Test", "Test", [
        make_behavior("success", Get, 200, tags: [], checks: dict.new()),
      ]),
    ])
  let issues = validate_failure_blast_radius(spec)
  let has_error_critical =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(FailureBlastRadius, Critical, msg, _) ->
          string.contains(msg, "error")
        _ -> False
      }
    })
  has_error_critical |> should.be_true
}

// =============================================================================
// Overall Critique Tests
// =============================================================================

pub fn critique_spec_perfect_passes_test() {
  let spec = perfect_spec()
  let result = critique_spec(spec)
  result.passed |> should.be_true
  result.score |> should.equal(100)
  result.issues |> should.equal([])
}

pub fn critique_spec_warnings_reduce_score_test() {
  let spec =
    Spec(
      ..perfect_spec(),
      ai_hints: AIHints(
        ImplementationHints([]),
        dict.new(),
        SecurityHints("", "", "", ""),
        pitfalls: [],
      ),
    )
  let result = critique_spec(spec)
  // Should pass but with reduced score (95 = 100 - 5 for warning)
  result.passed |> should.be_true
  { result.score < 100 && result.score >= 70 } |> should.be_true
}

pub fn critique_spec_multiple_criticals_fail_hard_test() {
  let spec = minimal_spec()
  let result = critique_spec(spec)
  result.passed |> should.be_false
  { result.score < 30 } |> should.be_true
}

pub fn critique_spec_score_calculation_test() {
  // Spec with exactly 1 critical (25 penalty) and 1 warning (5 penalty)
  // Expected score: 100 - 25 - 5 = 70 (exactly at threshold)
  let spec =
    Spec(
      ..minimal_spec(),
      features: [
        Feature("Test", "Test", [
          make_behavior("b1", Get, 200, tags: [], checks: dict.new()),
          make_behavior("b2", Get, 404, tags: [], checks: dict.new()),
        ]),
      ],
      anti_patterns: [],
      // This should give: 1 critical (no anti-patterns) + 1 warning (no edge tags)
    )
  let result = critique_spec(spec)
  // Should be exactly at or near threshold
  { result.score >= 65 && result.score <= 75 } |> should.be_true
}
