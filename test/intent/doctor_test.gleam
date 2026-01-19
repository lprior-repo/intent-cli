import gleeunit/should
import intent/doctor
import intent/spec_linter
import test_helpers

pub fn doctor_analysis_test() {
  let behavior = test_helpers.make_test_behavior("get-users", [])
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let report = doctor.run_doctor(spec)

  // Verify Quality
  { report.quality.overall_score < 100 } |> should.be_true

  // Verify Linting
  case report.lint {
    spec_linter.LintWarnings(warnings) -> {
      { warnings != [] } |> should.be_true
    }
    _ -> should.fail()
  }

  // Verify Suggestions
  { report.suggestions != [] } |> should.be_true
}
