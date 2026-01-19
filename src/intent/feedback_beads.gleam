//// Feedback-to-Bead Generator
////
//// Converts check results (failures) into fix beads for AI consumption.
//// This bridges the gap between Intent's check system and the bead tracking system.
////
//// Architecture: Functional Core / Imperative Shell
//// - Pure functions: parse_check_results(), generate_fix_beads(), bead_to_*
//// - No I/O in this module - pure transformation only
////
//// Usage:
//// ```gleam
//// let results = feedback_beads.parse_check_results(json_content)
//// let beads = feedback_beads.generate_fix_beads(results, spec)
//// ```
////
//// Expected input format (from `intent check --json`):
//// ```json
//// {
////   "pass": false,
////   "score": { "passed": 5, "failed": 3, "blocked": 0, "total": 8 },
////   "summary": "...",
////   "failures": [
////     {
////       "feature": "Auth",
////       "behavior": "login",
////       "intent": "User logs in with credentials",
////       "problems": [
////         { "field": "token", "rule": "exists", "expected": "present", "actual": "missing", "explanation": "..." }
////       ],
////       "request_sent": { "method": "POST", "url": "/api/login", "headers": {} },
////       "response_received": { "status": 200, "body": {...} },
////       "hint": "...",
////       "see_also": []
////     }
////   ]
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import intent/bead_templates.{type BeadRecord}
import intent/output.{type BehaviorFailure, type Problem, type SpecResult}

pub type FixBead {
  FixBead(
    title: String,
    description: String,
    profile_type: String,
    priority: Int,
    issue_type: String,
    labels: List(String),
    ai_hints: String,
    acceptance_criteria: List(String),
    dependencies: List(String),
  )
}

pub type CheckResults {
  CheckResults(
    pass: Bool,
    passed: Int,
    failed: Int,
    blocked: Int,
    total: Int,
    summary: String,
    failures: List(FixBeadFailure),
  )
}

pub type FixBeadFailure {
  FixBeadFailure(
    feature: String,
    behavior: String,
    intent: String,
    problems: List(FixBeadProblem),
    request_method: String,
    request_url: String,
    response_status: Int,
    hint: String,
  )
}

pub type FixBeadProblem {
  FixBeadProblem(
    field: String,
    rule: String,
    expected: String,
    actual: String,
    explanation: String,
  )
}

pub type ParseError {
  InvalidJson(String)
  MissingField(String)
}

pub fn parse_check_results(json_content: String) -> Result(CheckResults, ParseError) {
  case json.decode(json_content, decode_spec_result) {
    Ok(result) -> Ok(convert_spec_result(result))
    Error(e) -> Error(InvalidJson(format_json_error(e)))
  }
}

fn format_json_error(e: json.DecodeError) -> String {
  case e {
    json.UnexpectedFormat(errs) ->
      "Unexpected format: " <> string.join(list.map(errs, describe_decode_error), ", ")
    json.UnexpectedByte(b) -> "Unexpected byte: " <> string.inspect(b)
    json.UnexpectedEndOfInput -> "Unexpected end of input"
    json.UnexpectedSequence(s) -> "Unexpected sequence: " <> s
  }
}

fn describe_decode_error(err: json.DecodeError) -> String {
  "expected " <> err.expected <> " at " <> string.join(err.path, ".") <> ", found " <> err.found
}

fn decode_spec_result(dyn: json.Dynamic) -> Result(SpecResult, List(json.DecodeError)) {
  let pass_result = json.field("pass", json.bool)(dyn)
  let score_result = json.field("score", decode_score)(dyn)
  let summary_result = json.field("summary", json.string)(dyn)
  let failures_result = json.field("failures", json.list(decode_failure))(dyn)

  case pass_result, score_result, summary_result, failures_result {
    Ok(pass), Ok(score), Ok(summary), Ok(failures) -> {
      let blocked_result = json.field("blocked", json.list(decode_blocked))(dyn)
      let violations_result = json.field("rule_violations", json.list(decode_violation_group))(dyn)
      let patterns_result = json.field("anti_patterns_detected", json.list(decode_pattern))(dyn)

      let blocked = case blocked_result {
        Ok(b) -> b
        Error(_) -> []
      }
      let violations = case violations_result {
        Ok(v) -> v
        Error(_) -> []
      }
      let patterns = case patterns_result {
        Ok(p) -> p
        Error(_) -> []
      }

      Ok(output.SpecResult(
        pass: pass,
        passed: score.passed,
        failed: score.failed,
        blocked: score.blocked,
        total: score.total,
        summary: summary,
        failures: failures,
        blocked_behaviors: blocked,
        rule_violations: violations,
        anti_patterns_detected: patterns,
      ))
    }
    _, _, _, _ -> Error([])
  }
}

type Score {
  Score(passed: Int, failed: Int, blocked: Int, total: Int)
}

fn decode_score(dyn: json.Dynamic) -> Result(Score, List(json.DecodeError)) {
  let passed = json.field("passed", json.int)(dyn)
  let failed = json.field("failed", json.int)(dyn)
  let blocked = json.field("blocked", json.int)(dyn)
  let total = json.field("total", json.int)(dyn)

  case passed, failed, blocked, total {
    Ok(p), Ok(f), Ok(b), Ok(t) -> Ok(Score(passed: p, failed: f, blocked: b, total: t))
    _, _, _, _ -> Error([])
  }
}

fn decode_failure(dyn: json.Dynamic) -> Result(output.BehaviorFailure, List(json.DecodeError)) {
  let feature = json.field("feature", json.string)(dyn)
  let behavior = json.field("behavior", json.string)(dyn)
  let intent = json.field("intent", json.string)(dyn)
  let problems = json.field("problems", json.list(decode_problem))(dyn)
  let request = json.field("request_sent", decode_request)(dyn)
  let response = json.field("response_received", decode_response)(dyn)
  let hint = json.field("hint", json.string)(dyn)
  let see_also = json.field("see_also", json.list(json.string))(dyn)

  case feature, behavior, intent, problems, request, response, hint, see_also {
    Ok(f), Ok(b), Ok(i), Ok(p), Ok(r), Ok(resp), Ok(h), Ok(so) -> {
      Ok(output.BehaviorFailure(
        feature: f,
        behavior: b,
        intent: i,
        problems: p,
        request_sent: output.RequestSummary(method: r.method, url: r.url, headers: r.headers),
        response_received: output.ResponseSummary(status: resp.status, body: resp.body),
        hint: h,
        see_also: so,
      ))
    }
    _, _, _, _, _, _, _, _ -> Error([])
  }
}

type DecodedRequest {
  DecodedRequest(method: String, url: String, headers: Dict(String, String))
}

fn decode_request(dyn: json.Dynamic) -> Result(DecodedRequest, List(json.DecodeError)) {
  let method = json.field("method", json.string)(dyn)
  let url = json.field("url", json.string)(dyn)
  let headers = json.field("headers", decode_headers)(dyn)

  case method, url, headers {
    Ok(m), Ok(u), Ok(h) -> Ok(DecodedRequest(method: m, url: u, headers: h))
    _, _, _ -> Error([])
  }
}

fn decode_headers(dyn: json.Dynamic) -> Result(Dict(String, String), List(json.DecodeError)) {
  case json.object(dyn) {
    Ok(obj) -> {
      let entries = dict.to_list(obj)
      let results = list.map(entries, fn(pair) {
        case json.string(pair.1) {
          Ok(s) -> Ok(#(pair.0, s))
          Error(_) -> Error(Nil)
        }
      })
      let valid = list.filter_map(results, Ok)
      Ok(dict.from_list(valid))
    }
    Error(_) -> Ok(dict.new())
  }
}

type DecodedResponse {
  DecodedResponse(status: Int, body: Json)
}

fn decode_response(dyn: json.Dynamic) -> Result(DecodedResponse, List(json.DecodeError)) {
  let status = json.field("status", json.int)(dyn)
  let body = json.field("body", json.dynamic)(dyn)

  case status, body {
    Ok(s), Ok(b) -> Ok(DecodedResponse(status: s, body: b))
    _, _ -> Error([])
  }
}

fn decode_problem(dyn: json.Dynamic) -> Result(output.Problem, List(json.DecodeError)) {
  let field = json.field("field", json.string)(dyn)
  let rule = json.field("rule", json.string)(dyn)
  let expected = json.field("expected", json.string)(dyn)
  let actual = json.field("actual", json.string)(dyn)
  let explanation = json.field("explanation", json.string)(dyn)

  case field, rule, expected, actual, explanation {
    Ok(f), Ok(r), Ok(e), Ok(a), Ok(ex) -> {
      Ok(output.Problem(field: f, rule: r, expected: e, actual: a, explanation: ex))
    }
    _, _, _, _, _ -> Error([])
  }
}

fn decode_blocked(dyn: json.Dynamic) -> Result(output.BlockedBehavior, List(json.DecodeError)) {
  let behavior = json.field("behavior", json.string)(dyn)
  let reason = json.field("reason", json.string)(dyn)
  let hint = json.field("hint", json.string)(dyn)

  case behavior, reason, hint {
    Ok(b), Ok(r), Ok(h) -> Ok(output.BlockedBehavior(behavior: b, reason: r, hint: h))
    _, _, _ -> Error([])
  }
}

fn decode_violation_group(dyn: json.Dynamic) -> Result(output.RuleViolationGroup, List(json.DecodeError)) {
  let rule = json.field("rule", json.string)(dyn)
  let description = json.field("description", json.string)(dyn)
  let violations = json.field("violations", json.list(decode_violation))(dyn)

  case rule, description, violations {
    Ok(r), Ok(d), Ok(v) -> Ok(output.RuleViolationGroup(rule: r, description: d, violations: v))
    _, _, _ -> Error([])
  }
}

fn decode_violation(dyn: json.Dynamic) -> Result(output.BehaviorViolation, List(json.DecodeError)) {
  let behavior = json.field("behavior", json.string)(dyn)
  let violations = json.field("violations", json.list(json.string))(dyn)
  let response = json.optional_field("response", json.dynamic)(dyn)

  case behavior, violations {
    Ok(b), Ok(v) -> Ok(output.BehaviorViolation(behavior: b, violations: v, response: response))
    _, _ -> Error([])
  }
}

fn decode_pattern(dyn: json.Dynamic) -> Result(output.AntiPatternResult, List(json.DecodeError)) {
  let pattern = json.field("pattern", json.string)(dyn)
  case pattern {
    Ok(p) -> Ok(output.AntiPatternResult)
    Error(_) -> Ok(output.NoAntiPatterns)
  }
}

fn convert_spec_result(result: SpecResult) -> CheckResults {
  CheckResults(
    pass: result.pass,
    passed: result.passed,
    failed: result.failed,
    blocked: result.blocked,
    total: result.total,
    summary: result.summary,
    failures: list.map(result.failures, convert_failure),
  )
}

fn convert_failure(failure: output.BehaviorFailure) -> FixBeadFailure {
  FixBeadFailure(
    feature: failure.feature,
    behavior: failure.behavior,
    intent: failure.intent,
    problems: list.map(failure.problems, convert_problem),
    request_method: failure.request_sent.method,
    request_url: failure.request_sent.url,
    response_status: failure.response_received.status,
    hint: failure.hint,
  )
}

fn convert_problem(problem: output.Problem) -> FixBeadProblem {
  FixBeadProblem(
    field: problem.field,
    rule: problem.rule,
    expected: problem.expected,
    actual: problem.actual,
    explanation: problem.explanation,
  )
}

pub fn generate_fix_beads(results: CheckResults) -> List(BeadRecord) {
  list.map(results.failures, failure_to_bead)
}

fn failure_to_bead(failure: FixBeadFailure) -> BeadRecord {
  let problem_summary = summarize_problems(failure.problems)
  let title = "Fix: " <> failure.behavior <> " - " <> failure.feature
  let description = build_bead_description(failure, problem_summary)

  BeadRecord(
    title: title,
    description: description,
    profile_type: "api",
    priority: determine_priority(failure),
    issue_type: "fix_behavior",
    labels: ["fix", "failure", failure.feature],
    ai_hints: build_ai_hints(failure),
    acceptance_criteria: build_acceptance_criteria(failure),
    dependencies: [],
  )
}

fn summarize_problems(problems: List(FixBeadProblem)) -> String {
  case problems {
    [] -> "Unknown failure"
    [single] ->
      single.field
      <> " ("
      <> single.rule
      <> "): expected "
      <> single.expected
      <> ", got "
      <> single.actual
    many ->
      "Multiple issues: "
      <> string.join(list.map(many, fn(p) { p.field }), ", ")
  }
}

fn build_bead_description(failure: FixBeadFailure, problem_summary: String) -> String {
  "## Intent\n"
  <> failure.intent
  <> "\n\n"
  <> "## Failure\n"
  <> problem_summary
  <> "\n\n"
  <> "## Request\n"
  <> failure.request_method
  <> " "
  <> failure.request_url
  <> "\n\n"
  <> "## Response\n"
  <> "Status: "
  <> int.to_string(failure.response_status)
  <> "\n\n"
  <> "## Problems\n"
  <> string.join(
    list.map(failure.problems, fn(p) {
      "- **"
      <> p.field
      <> "** ("
      <> p.rule
      <> "):\n  "
      <> p.explanation
      <> "\n  Expected: "
      <> p.expected
      <> "\n  Actual: "
      <> p.actual
    }),
    "\n\n",
  )
  <> case failure.hint {
    "" -> ""
    hint -> "\n\n## Hint\n" <> hint
  }
}

fn determine_priority(failure: FixBeadFailure) -> Int {
  case failure.response_status {
    s if s >= 500 -> 1
    s if s >= 400 -> 2
    _ -> 3
  }
}

fn build_ai_hints(failure: FixBeadFailure) -> String {
  let status_hint = case failure.response_status {
    s if s >= 500 -> "Check server-side error handling and logging"
    s if s >= 400 -> "Check request validation and authentication"
    _ -> "Check response data transformation"
  }

  let field_hints = case failure.problems {
    [] -> ""
    problems -> {
      "\nField-specific issues:\n" <> string.join(list.map(problems, fn(p) { "- " <> p.field <> ": " <> p.explanation }), "\n")
    }
  }

  "## Fix Guidance\n"
  <> status_hint
  <> field_hints
  <> "\n\n"
  <> "## Implementation Notes\n"
  <> "- Review the behavior intent: " <> failure.intent
  <> "\n"
  <> "- Test with actual API response to confirm fix"
}

fn build_acceptance_criteria(failure: FixBeadFailure) -> List(String) {
  let status_criteria = case failure.response_status {
    s if s >= 500 -> "Fix 5xx errors (server error handling)"
    s if s >= 400 -> "Fix 4xx errors (client request validation)"
    _ -> "Response returns expected data"
  }

  let field_criteria = list.map(failure.problems, fn(p) {
    "Field '"
    <> p.field
    <> "' passes '"
    <> p.rule
    <> "' check"
  })

  list.append([status_criteria], field_criteria)
}

pub fn beads_to_json_output(beads: List(BeadRecord)) -> String {
  bead_templates.beads_to_jsonl(beads)
}

pub fn format_feedback_summary(results: CheckResults) -> String {
  let status = case results.pass {
    True -> "PASS"
    False -> "FAIL"
  }

  "## Check Results\n"
  <> status
  <> "\n\n"
  <> "Score:\n"
  <> "- Passed: " <> int.to_string(results.passed) <> "\n"
  <> "- Failed: " <> int.to_string(results.failed) <> "\n"
  <> "- Blocked: " <> int.to_string(results.blocked) <> "\n"
  <> "- Total: " <> int.to_string(results.total) <> "\n\n"
  <> "## Summary\n"
  <> results.summary
  <> case results.failures {
    [] -> ""
    failures ->
      "\n\n## Generated Fix Beads\n"
      <> int.to_string(list.length(failures))
      <> " fix bead(s) generated for failed behaviors"
  }
}
