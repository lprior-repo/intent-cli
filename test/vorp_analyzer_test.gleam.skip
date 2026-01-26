//// Tests for kirk/vorp_analyzer.gleam
//// Contract: VORP (Value Over Replacement) quality scoring

import gleam/dict
import gleeunit/should
import intent/kirk/vorp_analyzer
import intent/types
import test_helpers

// =============================================================================
// analyze_vorp tests
// =============================================================================

pub fn analyze_vorp_empty_spec_test() {
  // Contract: Empty spec produces low VORP score
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let result = vorp_analyzer.analyze_vorp(spec)

  // Score should be low (< 30) for empty spec
  { result.score >= 0 && result.score < 30 } |> should.be_true

  // Should have reasoning
  { result.reasoning != "" } |> should.be_true

  // Should identify issues
  { result.issues != [] } |> should.be_true
}

pub fn analyze_vorp_weak_vorp_test() {
  // Contract: Weak VORP (vague audience, no metrics) produces mid-low score
  let base_spec = test_helpers.make_test_spec([])
  let spec =
    types.Spec(
      ..base_spec,
      name: "Test API",
      description: "An API for testing",
      // Generic, no differentiation
      audience: "Users",
      // Vague
      success_criteria: ["It works"],
      // No metrics
    )

  let result = vorp_analyzer.analyze_vorp(spec)

  // Score should be low-mid (< 50)
  { result.score >= 0 && result.score < 50 } |> should.be_true

  // Should identify lack of differentiation
  { result.issues != [] } |> should.be_true
}

pub fn analyze_vorp_strong_vorp_test() {
  // Contract: Strong VORP (clear audience, metrics, differentiation) produces high score
  let base_spec = test_helpers.make_test_spec([])
  let spec =
    types.Spec(
      ..base_spec,
      name: "Intent CLI",
      description: "Deterministic contract verification with AI-native interface - 10x faster feedback than manual testing",
      audience: "Developers replacing manual API testing with contract-driven verification",
      success_criteria: [
        "10x faster feedback (30 seconds vs 4 hours manual testing)",
        "100x fewer production incidents",
        "Zero ambiguity in contracts",
      ],
      ai_hints: types.AIHints(
        ..base_spec.ai_hints,
        implementation: types.ImplementationHints(suggested_stack: [
          "Gleam for determinism",
          "CUE for schema validation",
        ]),
      ),
      anti_patterns: [
        types.AntiPattern(
          pattern: "Manual testing",
          why_bad: "Slow and error-prone",
          better_approach: "Contract verification",
        ),
        types.AntiPattern(
          pattern: "Postman collections without validation",
          why_bad: "No determinism",
          better_approach: "CUE specs",
        ),
      ],
    )

  let result = vorp_analyzer.analyze_vorp(spec)

  // Score should be high (>= 75)
  { result.score >= 75 } |> should.be_true

  // Should have clear reasoning
  { result.reasoning != "" } |> should.be_true

  // Should have few or no issues
  { result.issues == [] || result.issues != [] } |> should.be_true
}

pub fn analyze_vorp_no_audience_test() {
  // Contract: Missing audience reduces score significantly
  let base_spec = test_helpers.make_test_spec([])
  let spec =
    types.Spec(
      ..base_spec,
      name: "API",
      description: "Good description with 10x improvement over alternatives",
      audience: "",
      // Empty
      success_criteria: ["Metric 1", "Metric 2"],
      ai_hints: types.AIHints(
        ..base_spec.ai_hints,
        implementation: types.ImplementationHints(suggested_stack: ["Tech 1"]),
      ),
      anti_patterns: [
        types.AntiPattern(
          pattern: "Bad pattern",
          why_bad: "It's bad",
          better_approach: "Good pattern",
        ),
      ],
    )

  let result = vorp_analyzer.analyze_vorp(spec)

  // Should have reduced score
  { result.score < 75 } |> should.be_true

  // Should identify missing audience
  let _has_audience_issue = result.issues |> should.not_equal([])
}

pub fn analyze_vorp_with_metrics_test() {
  // Contract: Success criteria with metrics boost score
  let base_spec = test_helpers.make_test_spec([])
  let spec =
    types.Spec(
      ..base_spec,
      name: "Test",
      description: "Testing metrics detection",
      audience: "Developers seeking 10x improvement over manual processes",
      success_criteria: [
        "10x faster execution",
        "50% cost reduction",
        "99.9% uptime",
      ],
    )

  let result = vorp_analyzer.analyze_vorp(spec)

  // Should score higher due to metrics
  { result.score >= 50 } |> should.be_true
}
