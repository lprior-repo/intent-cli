/// Tests for quality analyzer error handling
import gleam/dict
import gleam/string
import gleeunit/should
import intent/quality_analyzer
import intent/types.{AIHints, Config, Feature, Implementation, Spec}

pub fn analyze_empty_spec_returns_error_test() {
  let empty_spec =
    Spec(
      name: "Empty Spec",
      description: "Test spec with no features",
      audience: "testers",
      version: "1.0.0",
      success_criteria: [],
      config: Config(
        base_url: "http://localhost",
        timeout_ms: 5000,
        headers: dict.new(),
      ),
      features: [],
      rules: [],
      anti_patterns: [],
      ai_hints: AIHints(
        implementation: Implementation(
          suggested_stack: [],
          language_features: [],
          libraries: dict.new(),
        ),
        pitfalls: [],
      ),
    )

  case quality_analyzer.analyze_spec(empty_spec) {
    Error(quality_analyzer.EmptySpec(_)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn analyze_spec_with_no_behaviors_returns_error_test() {
  let spec =
    Spec(
      name: "Spec with features but no behaviors",
      description: "Test spec",
      audience: "testers",
      version: "1.0.0",
      success_criteria: [],
      config: Config(
        base_url: "http://localhost",
        timeout_ms: 5000,
        headers: dict.new(),
      ),
      features: [
        Feature(
          name: "Empty Feature",
          description: "Feature with no behaviors",
          behaviors: [],
        ),
      ],
      rules: [],
      anti_patterns: [],
      ai_hints: AIHints(
        implementation: Implementation(
          suggested_stack: [],
          language_features: [],
          libraries: dict.new(),
        ),
        pitfalls: [],
      ),
    )

  case quality_analyzer.analyze_spec(spec) {
    Error(quality_analyzer.IncompleteData(_)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn format_error_ai_produces_cue_format_test() {
  let error =
    quality_analyzer.EmptySpec(
      "Spec has no features or behaviors. Cannot perform quality analysis on empty specification.",
    )

  let formatted = quality_analyzer.format_error_ai(error)

  // Check for CUE structure
  formatted
  |> string.contains("action: \"validation_error\"")
  |> should.be_true

  formatted
  |> string.contains("type: \"empty_spec\"")
  |> should.be_true

  formatted
  |> string.contains("recovery:")
  |> should.be_true
}

pub fn format_error_text_produces_readable_format_test() {
  let error =
    quality_analyzer.MissingRequiredField("name", "spec.name is required")

  let formatted = quality_analyzer.format_error_text(error)

  // Check for human-readable structure
  formatted
  |> string.contains("Error:")
  |> should.be_true

  formatted
  |> string.contains("Recovery Steps:")
  |> should.be_true

  formatted
  |> string.contains("field: name")
  |> should.be_true
}
