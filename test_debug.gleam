import gleam/dict
import gleam/io
import gleam/int
import gleam/list
import intent/kirk/ready
import intent/ready_critique
import intent/types
import test_helpers

pub fn main() {
  // Test 1: Empty spec
  io.println("=== Test 1: Empty Spec ===")
  let empty_spec =
    types.Spec(
      name: "",
      description: "",
      audience: "",
      version: "",
      success_criteria: [],
      config: test_helpers.make_test_config(),
      features: [],
      rules: [],
      anti_patterns: [],
      ai_hints: types.AIHints(
        implementation: types.ImplementationHints(suggested_stack: []),
        entities: dict.new(),
        security: types.SecurityHints(
          password_hashing: "",
          jwt_algorithm: "",
          jwt_expiry: "",
          rate_limiting: "",
        ),
        pitfalls: [],
      ),
    )

  let report = ready.analyze_ready(empty_spec)
  io.println("Overall readiness: " <> int.to_string(report.overall_readiness))
  io.println("Blockers count: " <> int.to_string(list.length(report.blockers)))
}
