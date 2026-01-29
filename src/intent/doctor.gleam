/// Doctor Command - Health and Quality Analysis
///
/// Provides comprehensive health checks and quality reports for specifications:
/// - Quality analysis across 5 dimensions
/// - Linting for anti-patterns
/// - Improvement suggestions
/// - System health checks (CUE binary, config files, schemas)
///
/// The doctor command combines multiple analysis tools to provide
/// actionable recommendations for improving specifications.
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{Some}
import gleam/string
import intent/improver.{type ImprovementSuggestion}
import intent/json_output
import intent/output_mode
import intent/quality_analyzer.{type QualityReport}
import intent/spec_linter.{type LintResult}
import intent/types.{type Spec}
import shellout
import simplifile

pub type DoctorReport {
  DoctorReport(
    quality: QualityReport,
    lint: LintResult,
    suggestions: List(ImprovementSuggestion),
  )
}

pub type HealthReport {
  HealthReport(checks: List(HealthCheck))
}

pub type HealthCheck {
  HealthCheck(
    name: String,
    status: HealthStatus,
    message: String,
    details: String,
  )
}

pub type HealthStatus {
  HealthOk
  HealthWarning
  HealthError
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

pub fn run_health_check() -> HealthReport {
  let checks = [
    check_cue_binary(),
    check_project_config(),
    check_schema_files(),
  ]

  HealthReport(checks: checks)
}

fn check_cue_binary() -> HealthCheck {
  case shellout.command("cue", ["version"], ".", []) {
    Ok(output) -> {
      let version = case string.split(output, "\n") {
        [first_line, ..] -> first_line
        [] -> "unknown"
      }
      HealthCheck(
        name: "CUE Binary",
        status: HealthOk,
        message: "CUE is installed",
        details: version,
      )
    }
    Error(#(_, stderr)) -> {
      HealthCheck(
        name: "CUE Binary",
        status: HealthError,
        message: "CUE binary not found",
        details: stderr,
      )
    }
  }
}

fn check_project_config() -> HealthCheck {
  let config_files = ["gleam.toml", "cue.mod", "manifest.toml"]
  let existing =
    list.filter(config_files, fn(f) {
      case simplifile.verify_is_file(f) {
        Ok(True) -> True
        _ -> False
      }
    })

  case existing {
    [] ->
      HealthCheck(
        name: "Project Config",
        status: HealthError,
        message: "No project configuration files found",
        details: "Expected one of: " <> string.join(config_files, ", "),
      )
    files -> {
      let file_list = string.join(files, ", ")
      case list.length(files) {
        3 ->
          HealthCheck(
            name: "Project Config",
            status: HealthOk,
            message: "All project configuration files present",
            details: file_list,
          )
        _ ->
          HealthCheck(
            name: "Project Config",
            status: HealthWarning,
            message: "Some configuration files missing",
            details: "Found: " <> file_list,
          )
      }
    }
  }
}

fn check_schema_files() -> HealthCheck {
  let required_schemas = ["schema/intent.cue", "schema/questions.cue"]
  let optional_schemas = [
    "schema/custom-questions.cue",
    "schema/interview.cue",
    "schema/kirk.cue",
  ]

  let existing_required =
    list.filter(required_schemas, fn(f) {
      case simplifile.verify_is_file(f) {
        Ok(True) -> True
        _ -> False
      }
    })

  let missing_required =
    list.filter(required_schemas, fn(f) { !list.contains(existing_required, f) })

  case missing_required {
    [] -> {
      let existing_optional =
        list.filter(optional_schemas, fn(f) {
          case simplifile.verify_is_file(f) {
            Ok(True) -> True
            _ -> False
          }
        })

      case list.length(existing_optional) {
        n if n > 0 ->
          HealthCheck(
            name: "Schema Files",
            status: HealthOk,
            message: "All required schema files present",
            details: "Required: all present, Optional: "
              <> string.inspect(n)
              <> " found",
          )
        _ ->
          HealthCheck(
            name: "Schema Files",
            status: HealthOk,
            message: "All required schema files present",
            details: "Required: all present, Optional: none",
          )
      }
    }
    missing ->
      HealthCheck(
        name: "Schema Files",
        status: HealthError,
        message: "Required schema files missing",
        details: "Missing: " <> string.join(missing, ", "),
      )
  }
}

pub fn print_report(report: DoctorReport, _mode: output_mode.OutputMode) -> Nil {
  io.println("Doctor Health Report")
  io.println("")

  // 1. Quality
  io.println("1. Quality Analysis")
  io.println(quality_analyzer.format_report(report.quality))
  io.println("")

  // 2. Linting
  io.println("2. Anti-Pattern Check")
  case report.lint {
    spec_linter.LintValid -> io.println("No anti-patterns found")
    spec_linter.LintWarnings(warnings) ->
      io.println(spec_linter.format_warnings(warnings))
  }
  io.println("")

  // 3. Improvements
  io.println("3. Prescriptions")
  io.println(improver.format_improvements(report.suggestions))
}

pub fn print_health_report(
  report: HealthReport,
  _mode: output_mode.OutputMode,
) -> Nil {
  io.println("Intent Health Report")
  io.println("")

  list.each(report.checks, fn(check) {
    let icon = case check.status {
      HealthOk -> "✓"
      HealthWarning -> "⚠"
      HealthError -> "✗"
    }
    let status_text = case check.status {
      HealthOk -> "OK"
      HealthWarning -> "WARNING"
      HealthError -> "ERROR"
    }
    io.println(icon <> " [" <> status_text <> "] " <> check.name)
    io.println("    " <> check.message)
    case check.details {
      "" -> Nil
      details -> io.println("    " <> details)
    }
    io.println("")
  })

  let ok_count = list.filter(report.checks, fn(c) { c.status == HealthOk })
  let warning_count =
    list.filter(report.checks, fn(c) { c.status == HealthWarning })
  let error_count =
    list.filter(report.checks, fn(c) { c.status == HealthError })

  io.println("Summary: ")
  io.println("  ✓ OK: " <> string.inspect(list.length(ok_count)))
  io.println("  ⚠ Warnings: " <> string.inspect(list.length(warning_count)))
  io.println("  ✗ Errors: " <> string.inspect(list.length(error_count)))
}

pub fn json_output(report: DoctorReport, spec_path: String) -> Nil {
  let data =
    json.object([
      #("quality", quality_to_json(report.quality)),
      #("lint", lint_to_json(report.lint)),
      #("suggestions", json.array(report.suggestions, suggestion_to_json)),
    ])

  let next_actions = [
    json_output.next_action(
      "intent improve " <> spec_path,
      "Get detailed improvement suggestions",
    ),
    json_output.next_action(
      "intent validate " <> spec_path,
      "Verify spec structure and syntax",
    ),
  ]

  let response =
    json_output.create_response(
      "doctor_report",
      "doctor",
      data,
      Some(spec_path),
      0,
    )
    |> json_output.with_next_actions(next_actions)

  json_output.output(response)
}

pub fn json_output_health_report(report: HealthReport) -> Nil {
  let data =
    json.object([
      #("checks", json.array(report.checks, health_check_to_json)),
    ])

  let response =
    json_output.create_response("health_report", "doctor", data, option.None, 0)

  json_output.output(response)
}

fn health_check_to_json(check: HealthCheck) -> Json {
  let status_str = case check.status {
    HealthOk -> "ok"
    HealthWarning -> "warning"
    HealthError -> "error"
  }
  json.object([
    #("name", json.string(check.name)),
    #("status", json.string(status_str)),
    #("message", json.string(check.message)),
    #("details", json.string(check.details)),
  ])
}

fn quality_to_json(report: QualityReport) -> Json {
  json.object([
    #("overall_score", json.int(report.overall_score)),
    #("coverage_score", json.int(report.coverage_score)),
    #("clarity_score", json.int(report.clarity_score)),
    #("testability_score", json.int(report.testability_score)),
    #("ai_readiness_score", json.int(report.ai_readiness_score)),
    #(
      "issues",
      json.array(report.issues, fn(issue) {
        json.string(quality_analyzer.format_issue(issue))
      }),
    ),
  ])
}

fn lint_to_json(result: LintResult) -> Json {
  case result {
    spec_linter.LintValid ->
      json.object([
        #("status", json.string("valid")),
        #("warnings", json.array([], fn(_) { json.null() })),
      ])
    spec_linter.LintWarnings(warnings) ->
      json.object([
        #("status", json.string("warnings")),
        #("warnings", json.array(warnings, lint_warning_to_json)),
      ])
  }
}

fn lint_warning_to_json(warning: spec_linter.LintWarning) -> Json {
  case warning {
    spec_linter.AntiPatternDetected(behavior, pattern, details) ->
      json.object([
        #("type", json.string("anti_pattern")),
        #("behavior", json.string(behavior)),
        #("pattern", json.string(pattern)),
        #("details", json.string(details)),
      ])
    spec_linter.VagueRule(behavior, field, rule) ->
      json.object([
        #("type", json.string("vague_rule")),
        #("behavior", json.string(behavior)),
        #("field", json.string(field)),
        #("rule", json.string(rule)),
      ])
    spec_linter.MissingExample(behavior) ->
      json.object([
        #("type", json.string("missing_example")),
        #("behavior", json.string(behavior)),
      ])
    spec_linter.UnusedAntiPattern(pattern) ->
      json.object([
        #("type", json.string("unused_anti_pattern")),
        #("pattern", json.string(pattern)),
      ])
    spec_linter.NamingConvention(behavior, suggestion) ->
      json.object([
        #("type", json.string("naming_convention")),
        #("behavior", json.string(behavior)),
        #("suggestion", json.string(suggestion)),
      ])
    spec_linter.DuplicateBehavior(b1, b2, similarity) ->
      json.object([
        #("type", json.string("duplicate_behavior")),
        #("behavior1", json.string(b1)),
        #("behavior2", json.string(b2)),
        #("similarity", json.string(similarity)),
      ])
  }
}

fn suggestion_to_json(suggestion: ImprovementSuggestion) -> Json {
  json.object([
    #("title", json.string(suggestion.title)),
    #("description", json.string(suggestion.description)),
    #("reasoning", json.string(suggestion.reasoning)),
    #("impact_score", json.int(suggestion.impact_score)),
  ])
}
