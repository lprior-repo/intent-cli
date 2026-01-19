import gleam/io
import gleam/json.{type Json}
import gleam/option.{Some}
import intent/cli_ui
import intent/improver.{type ImprovementSuggestion}
import intent/json_output
import intent/output_mode
import intent/quality_analyzer.{type QualityReport}
import intent/spec_linter.{type LintResult}
import intent/types.{type Spec}

pub type DoctorReport {
  DoctorReport(
    quality: QualityReport,
    lint: LintResult,
    suggestions: List(ImprovementSuggestion),
  )
}

pub fn run_doctor(spec: Spec) -> DoctorReport {
  let quality = quality_analyzer.analyze_spec(spec)
  let lint = spec_linter.lint_spec(spec)
  
  let context =
    improver.ImprovementContext(
      quality_report: quality,
      lint_result: lint,
      spec: spec,
    )
  let suggestions = improver.suggest_improvements(context)

  DoctorReport(quality, lint, suggestions)
}

pub fn print_report(report: DoctorReport, mode: output_mode.OutputMode) -> Nil {
  cli_ui.print_header("Doctor Health Report", mode)
  
  // 1. Quality
  cli_ui.print_info("1. Quality Analysis", mode)
  io.println(quality_analyzer.format_report(report.quality))
  io.println("")

  // 2. Linting
  cli_ui.print_info("2. Anti-Pattern Check", mode)
  case report.lint {
    spec_linter.LintValid -> cli_ui.print_success("No anti-patterns found", mode)
    spec_linter.LintWarnings(warnings) -> 
      io.println(spec_linter.format_warnings(warnings))
  }
  io.println("")

  // 3. Improvements
  cli_ui.print_info("3. Prescriptions", mode)
  io.println(improver.format_improvements(report.suggestions))
}

pub fn json_output(report: DoctorReport, spec_path: String) -> Nil {
  let data = json.object([
    #("quality", quality_to_json(report.quality)),
    #("lint", lint_to_json(report.lint)),
    #("suggestions", json.array(report.suggestions, suggestion_to_json))
  ])

  let response = json_output.create_response(
    "doctor_report",
    "doctor",
    data,
    Some(spec_path),
    0
  )

  json_output.output(response)
}

fn quality_to_json(report: QualityReport) -> Json {
  json.object([
    #("overall_score", json.int(report.overall_score)),
    #("coverage_score", json.int(report.coverage_score)),
    #("clarity_score", json.int(report.clarity_score)),
    #("testability_score", json.int(report.testability_score)),
    #("ai_readiness_score", json.int(report.ai_readiness_score)),
    #("issues", json.array(report.issues, fn(issue) {
      json.string(quality_analyzer.format_issue(issue))
    }))
  ])
}

fn lint_to_json(result: LintResult) -> Json {
  case result {
    spec_linter.LintValid -> json.object([
      #("status", json.string("valid")),
      #("warnings", json.array([], fn(_) { json.null() }))
    ])
    spec_linter.LintWarnings(warnings) -> json.object([
      #("status", json.string("warnings")),
      #("warnings", json.array(warnings, lint_warning_to_json))
    ])
  }
}

fn lint_warning_to_json(warning: spec_linter.LintWarning) -> Json {
  case warning {
    spec_linter.AntiPatternDetected(behavior, pattern, details) -> json.object([
      #("type", json.string("anti_pattern")),
      #("behavior", json.string(behavior)),
      #("pattern", json.string(pattern)),
      #("details", json.string(details))
    ])
    spec_linter.VagueRule(behavior, field, rule) -> json.object([
      #("type", json.string("vague_rule")),
      #("behavior", json.string(behavior)),
      #("field", json.string(field)),
      #("rule", json.string(rule))
    ])
    spec_linter.MissingExample(behavior) -> json.object([
      #("type", json.string("missing_example")),
      #("behavior", json.string(behavior))
    ])
    spec_linter.UnusedAntiPattern(pattern) -> json.object([
      #("type", json.string("unused_anti_pattern")),
      #("pattern", json.string(pattern))
    ])
    spec_linter.NamingConvention(behavior, suggestion) -> json.object([
      #("type", json.string("naming_convention")),
      #("behavior", json.string(behavior)),
      #("suggestion", json.string(suggestion))
    ])
    spec_linter.DuplicateBehavior(b1, b2, similarity) -> json.object([
      #("type", json.string("duplicate_behavior")),
      #("behavior1", json.string(b1)),
      #("behavior2", json.string(b2)),
      #("similarity", json.string(similarity))
    ])
  }
}

fn suggestion_to_json(suggestion: ImprovementSuggestion) -> Json {
  json.object([
    #("title", json.string(suggestion.title)),
    #("description", json.string(suggestion.description)),
    #("reasoning", json.string(suggestion.reasoning)),
    #("impact_score", json.int(suggestion.impact_score))
  ])
}