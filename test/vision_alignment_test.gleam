/// Tests for vision alignment checker
/// These tests verify that we can detect drift between Phase 1 (Vision) and Phase 4 (Spec)
import gleam/dict
import gleam/json
import gleam/option.{Some}
import gleeunit
import gleeunit/should
import intent/planning_types.{DimensionScore}
import intent/types.{type Spec, AIHints, Config, Feature, Spec}
import intent/vision_alignment
import intent/vision_types.{Scenario, VisionSection}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Test Data Builders
// =============================================================================

fn build_vision() -> VisionSection {
  VisionSection(
    press_release: "A revolutionary API testing tool for AI agents",
    persona: "AI agent developers who need contract-driven testing",
    non_personas: ["Manual QA testers", "Human users"],
    north_star: "AI agents can validate API contracts without human intervention",
    scenarios: [
      Scenario(
        character: "Claude",
        persona: "AI agent",
        motivation: "Validate API contract",
        simulation: "Claude reads CUE spec and verifies API responses",
        outcome: "All tests pass without human help",
      ),
    ],
    replaces: Some("Manual API testing"),
    vorp: "Deterministic contract verification with AI-native interface",
    out_of_scope: ["UI testing", "Performance testing"],
  )
}

fn build_spec_aligned() -> Spec {
  Spec(
    name: "Intent API Spec",
    description: "Contract-driven API testing for AI agents",
    audience: "AI agent developers who need contract-driven testing",
    version: "1.0.0",
    success_criteria: [
      "AI agents can validate API contracts without human intervention",
      "Deterministic verification of responses",
    ],
    config: Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: True,
    ),
    features: [
      Feature(
        name: "Contract Verification",
        description: "Verify API responses against contracts",
        behaviors: [],
      ),
    ],
    rules: [],
    anti_patterns: [],
    ai_hints: AIHints(
      implementation: ["Use deterministic verification"],
      entities: [],
      security: [],
      pitfalls: [],
    ),
  )
}

fn build_spec_drifted_persona() -> Spec {
  let aligned = build_spec_aligned()
  Spec(..aligned, audience: "Human developers who manually test APIs")
}

fn build_spec_drifted_northstar() -> Spec {
  let aligned = build_spec_aligned()
  Spec(..aligned, success_criteria: ["Fast API testing", "Easy debugging"])
}

fn build_spec_scope_creep() -> Spec {
  let aligned = build_spec_aligned()
  Spec(..aligned, features: [
    Feature(
      name: "Contract Verification",
      description: "Verify API responses",
      behaviors: [],
    ),
    Feature(
      name: "UI Testing",
      description: "Test user interfaces",
      behaviors: [],
    ),
    Feature(
      name: "Performance Benchmarking",
      description: "Benchmark API performance",
      behaviors: [],
    ),
  ])
}

// =============================================================================
// Persona Alignment Tests
// =============================================================================

pub fn test_persona_alignment_identical_test() {
  let vision = build_vision()
  let spec = build_spec_aligned()

  let result = vision_alignment.check_persona_alignment(vision, spec)

  result.score
  |> should.equal(100)
}

pub fn test_persona_alignment_similar_test() {
  let vision = build_vision()
  let spec =
    Spec(
      ..build_spec_aligned(),
      audience: "AI developers building contract testing tools",
    )

  let result = vision_alignment.check_persona_alignment(vision, spec)

  // Should be high score (80+) due to keyword overlap: "AI", "contract", "developers"
  result.score
  |> should.be_true(fn(score) { score >= 80 && score < 100 })
}

pub fn test_persona_alignment_different_test() {
  let vision = build_vision()
  let spec = build_spec_drifted_persona()

  let result = vision_alignment.check_persona_alignment(vision, spec)

  // Should be low score due to different target audience
  result.score
  |> should.be_true(fn(score) { score < 60 })

  result.issues
  |> should.not_equal([])
}

// =============================================================================
// North Star Alignment Tests
// =============================================================================

pub fn test_north_star_full_coverage_test() {
  let vision = build_vision()
  let spec = build_spec_aligned()

  let result = vision_alignment.check_north_star_alignment(vision, spec)

  // Success criteria covers the north star goal
  result.score
  |> should.be_true(fn(score) { score >= 90 })
}

pub fn test_north_star_partial_coverage_test() {
  let vision = build_vision()
  let spec = build_spec_drifted_northstar()

  let result = vision_alignment.check_north_star_alignment(vision, spec)

  // Success criteria doesn't cover north star
  result.score
  |> should.be_true(fn(score) { score < 50 })

  result.issues
  |> should.not_equal([])
}

pub fn test_north_star_no_coverage_test() {
  let vision = build_vision()
  let spec = Spec(..build_spec_aligned(), success_criteria: [])

  let result = vision_alignment.check_north_star_alignment(vision, spec)

  result.score
  |> should.equal(0)
}

// =============================================================================
// Scope Integrity Tests
// =============================================================================

pub fn test_scope_integrity_perfect_test() {
  let vision = build_vision()
  let spec = build_spec_aligned()

  let result = vision_alignment.check_scope_integrity(vision, spec)

  // No scope creep or reduction
  result.score
  |> should.be_true(fn(score) { score >= 90 })
}

pub fn test_scope_creep_detection_test() {
  let vision = build_vision()
  let spec = build_spec_scope_creep()

  let result = vision_alignment.check_scope_integrity(vision, spec)

  // Should detect UI Testing and Performance Benchmarking as out of scope
  result.score
  |> should.be_true(fn(score) { score < 80 })

  result.issues
  |> should.not_equal([])

  // Check that issues mention the out-of-scope features
  let issues_text = result.reasoning
  issues_text
  |> should.not_equal("")
}

pub fn test_scope_reduction_detection_test() {
  let vision = build_vision()
  // Spec with no features matching the scenario
  let spec = Spec(..build_spec_aligned(), features: [])

  let result = vision_alignment.check_scope_integrity(vision, spec)

  // Should detect missing scenario coverage
  result.score
  |> should.be_true(fn(score) { score < 50 })

  result.issues
  |> should.not_equal([])
}

// =============================================================================
// VORP Delivery Tests
// =============================================================================

pub fn test_vorp_delivery_full_test() {
  let vision = build_vision()
  let spec = build_spec_aligned()

  let result = vision_alignment.check_vorp_delivery(vision, spec)

  // "Deterministic" and "contract" keywords present
  result.score
  |> should.be_true(fn(score) { score >= 70 })
}

pub fn test_vorp_delivery_partial_test() {
  let vision = build_vision()
  let spec =
    Spec(
      ..build_spec_aligned(),
      description: "A basic API testing tool",
      ai_hints: AIHints(
        implementation: [],
        entities: [],
        security: [],
        pitfalls: [],
      ),
    )

  let result = vision_alignment.check_vorp_delivery(vision, spec)

  // Missing key differentiation points
  result.score
  |> should.be_true(fn(score) { score < 50 })
}

// =============================================================================
// Integration Tests
// =============================================================================

pub fn test_analyze_alignment_integration_test() {
  let vision = build_vision()
  let spec = build_spec_aligned()

  let result = vision_alignment.analyze_alignment(vision, spec)

  // Overall alignment should be high for well-aligned spec
  result.overall_alignment
  |> should.be_true(fn(score) { score >= 80 })

  // Should have 4 dimension scores
  result.persona_alignment.score
  |> should.be_true(fn(s) { s >= 0 && s <= 100 })

  result.north_star_alignment.score
  |> should.be_true(fn(s) { s >= 0 && s <= 100 })

  result.scope_integrity.score
  |> should.be_true(fn(s) { s >= 0 && s <= 100 })

  result.vorp_delivery.score
  |> should.be_true(fn(s) { s >= 0 && s <= 100 })
}

pub fn test_analyze_alignment_with_drift_test() {
  let vision = build_vision()
  let spec = build_spec_drifted_persona()

  let result = vision_alignment.analyze_alignment(vision, spec)

  // Overall alignment should be lower due to persona drift
  result.overall_alignment
  |> should.be_true(fn(score) { score < 70 })

  // Should have recommendations
  result.recommendations
  |> should.not_equal([])
}

pub fn test_analyze_alignment_json_output_test() {
  let vision = build_vision()
  let spec = build_spec_aligned()

  let result = vision_alignment.analyze_alignment(vision, spec)
  let json_output = vision_alignment.alignment_report_to_json(result)

  // Should produce valid JSON
  json.to_string(json_output)
  |> should.not_equal("")
}
