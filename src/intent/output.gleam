/// Output formatters for Intent results
/// Generates JSON and human-readable output
import gleam/dict.{type Dict}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/anti_patterns.{type AntiPatternResult}
import intent/checker/types as checker_types
import intent/http_client.{type ExecutionResult}
import intent/json_output
import intent/types.{type Behavior, method_to_string}

/// Overall result of running a spec
pub type SpecResult {
  SpecResult(
    pass: Bool,
    passed: Int,
    failed: Int,
    blocked: Int,
    total: Int,
    summary: String,
    failures: List(BehaviorFailure),
    error_failures: List(BehaviorErrorInfo),
    blocked_behaviors: List(BlockedBehavior),
    rule_violations: List(RuleViolationGroup),
    anti_patterns_detected: List(AntiPatternResult),
  )
}

/// A behavior that failed
pub type BehaviorFailure {
  BehaviorFailure(
    feature: String,
    behavior: String,
    intent: String,
    problems: List(Problem),
    request_sent: RequestSummary,
    response_received: ResponseSummary,
    hint: String,
    see_also: List(String),
  )
}

/// A single problem in a behavior
pub type Problem {
  Problem(
    field: String,
    rule: String,
    expected: String,
    actual: String,
    explanation: String,
  )
}

/// Summary of a sent request
pub type RequestSummary {
  RequestSummary(method: String, url: String, headers: Dict(String, String))
}

/// Summary of a received response
pub type ResponseSummary {
  ResponseSummary(status: Int, body: Json)
}

/// A behavior that couldn't run due to dependencies
pub type BlockedBehavior {
  BlockedBehavior(behavior: String, reason: String, hint: String)
}

/// A behavior that failed due to a network or execution error
/// (separate from BehaviorFailure which has response data)
pub type BehaviorErrorInfo {
  BehaviorErrorInfo(
    behavior: String,
    error_type: String,
    message: String,
    hint: String,
  )
}

/// A group of rule violations
pub type RuleViolationGroup {
  RuleViolationGroup(
    rule: String,
    description: String,
    violations: List(BehaviorViolation),
  )
}

/// A rule violation in a specific behavior
pub type BehaviorViolation {
  BehaviorViolation(
    behavior: String,
    violations: List(String),
    response: Option(Json),
  )
}

/// Convert a SpecResult to JSON
pub fn spec_result_to_json(result: SpecResult) -> Json {
  json.object([
    #("pass", json.bool(result.pass)),
    #(
      "score",
      json.object([
        #("passed", json.int(result.passed)),
        #("failed", json.int(result.failed)),
        #("blocked", json.int(result.blocked)),
        #("total", json.int(result.total)),
      ]),
    ),
    #("summary", json.string(result.summary)),
    #("failures", json.array(result.failures, behavior_failure_to_json)),
    #(
      "error_failures",
      json.array(result.error_failures, behavior_error_to_json),
    ),
    #("blocked", json.array(result.blocked_behaviors, blocked_behavior_to_json)),
    #(
      "rule_violations",
      json.array(result.rule_violations, rule_violation_group_to_json),
    ),
    #(
      "anti_patterns_detected",
      json.array(result.anti_patterns_detected, anti_pattern_result_to_json),
    ),
  ])
}

fn behavior_failure_to_json(failure: BehaviorFailure) -> Json {
  json.object([
    #("feature", json.string(failure.feature)),
    #("behavior", json.string(failure.behavior)),
    #("intent", json.string(failure.intent)),
    #("problems", json.array(failure.problems, problem_to_json)),
    #("request_sent", request_summary_to_json(failure.request_sent)),
    #("response_received", response_summary_to_json(failure.response_received)),
    #("hint", json.string(failure.hint)),
    #("see_also", json.array(failure.see_also, json.string)),
  ])
}

fn problem_to_json(problem: Problem) -> Json {
  json.object([
    #("field", json.string(problem.field)),
    #("rule", json.string(problem.rule)),
    #("expected", json.string(problem.expected)),
    #("actual", json.string(problem.actual)),
    #("explanation", json.string(problem.explanation)),
  ])
}

fn request_summary_to_json(req: RequestSummary) -> Json {
  json.object([
    #("method", json.string(req.method)),
    #("url", json.string(req.url)),
    #(
      "headers",
      json.object(
        req.headers
        |> dict.to_list
        |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) }),
      ),
    ),
  ])
}

fn response_summary_to_json(resp: ResponseSummary) -> Json {
  json.object([#("status", json.int(resp.status)), #("body", resp.body)])
}

fn blocked_behavior_to_json(blocked: BlockedBehavior) -> Json {
  json.object([
    #("behavior", json.string(blocked.behavior)),
    #("reason", json.string(blocked.reason)),
    #("hint", json.string(blocked.hint)),
  ])
}

fn behavior_error_to_json(error: BehaviorErrorInfo) -> Json {
  json.object([
    #("behavior", json.string(error.behavior)),
    #("error_type", json.string(error.error_type)),
    #("message", json.string(error.message)),
    #("hint", json.string(error.hint)),
  ])
}

fn rule_violation_group_to_json(group: RuleViolationGroup) -> Json {
  json.object([
    #("rule", json.string(group.rule)),
    #("description", json.string(group.description)),
    #("violations", json.array(group.violations, behavior_violation_to_json)),
  ])
}

fn behavior_violation_to_json(violation: BehaviorViolation) -> Json {
  json.object([
    #("behavior", json.string(violation.behavior)),
    #("violations", json.array(violation.violations, json.string)),
    #("response", option.unwrap(violation.response, json.null())),
  ])
}

fn anti_pattern_result_to_json(result: AntiPatternResult) -> Json {
  case result {
    anti_patterns.NoAntiPatterns -> json.null()
    anti_patterns.AntiPatternDetected(name, desc, found, bad, good) ->
      json.object([
        #("pattern", json.string(name)),
        #("description", json.string(desc)),
        #("found", json.string(found)),
        #("see_bad_example", bad),
        #("see_good_example", good),
      ])
  }
}

/// Convert a SpecResult to human-readable text
pub fn spec_result_to_text(result: SpecResult) -> String {
  let header = case result.pass {
    True -> "PASS"
    False -> "FAIL"
  }

  let score =
    "Passed: "
    <> int.to_string(result.passed)
    <> " / Failed: "
    <> int.to_string(result.failed)
    <> " / Blocked: "
    <> int.to_string(result.blocked)
    <> " / Total: "
    <> int.to_string(result.total)

  let failures_text = case result.failures {
    [] -> ""
    failures ->
      "\n\nFAILURES:\n"
      <> string.join(list.map(failures, format_failure), "\n\n")
  }

  let error_failures_text = case result.error_failures {
    [] -> ""
    errors ->
      "\n\nERRORS:\n"
      <> string.join(list.map(errors, format_error_failure), "\n\n")
  }

  let blocked_text = case result.blocked_behaviors {
    [] -> ""
    blocked ->
      "\n\nBLOCKED:\n" <> string.join(list.map(blocked, format_blocked), "\n")
  }

  let rules_text = case result.rule_violations {
    [] -> ""
    violations ->
      "\n\nRULE VIOLATIONS:\n"
      <> string.join(list.map(violations, format_rule_violation_group), "\n")
  }

  let anti_patterns_text = case result.anti_patterns_detected {
    [] -> ""
    patterns ->
      "\n\nANTI-PATTERNS DETECTED:\n"
      <> string.join(
        list.map(patterns, anti_patterns.format_anti_pattern),
        "\n",
      )
  }

  header
  <> "\n"
  <> score
  <> "\n"
  <> result.summary
  <> failures_text
  <> error_failures_text
  <> blocked_text
  <> rules_text
  <> anti_patterns_text
}

fn format_failure(failure: BehaviorFailure) -> String {
  let problems_text =
    failure.problems
    |> list.map(fn(p) {
      "  - "
      <> p.field
      <> ": "
      <> p.explanation
      <> "\n    Expected: "
      <> p.expected
      <> "\n    Actual: "
      <> p.actual
    })
    |> string.join("\n")

  "["
  <> failure.feature
  <> "] "
  <> failure.behavior
  <> "\n"
  <> "Intent: "
  <> failure.intent
  <> "\n"
  <> "Problems:\n"
  <> problems_text
  <> "\n"
  <> "Request: "
  <> failure.request_sent.method
  <> " "
  <> failure.request_sent.url
  <> "\n"
  <> "Response: "
  <> int.to_string(failure.response_received.status)
  <> case failure.hint {
    "" -> ""
    hint -> "\nHint: " <> hint
  }
}

fn format_blocked(blocked: BlockedBehavior) -> String {
  "- "
  <> blocked.behavior
  <> ": "
  <> blocked.reason
  <> case blocked.hint {
    "" -> ""
    hint -> " (" <> hint <> ")"
  }
}

fn format_error_failure(error: BehaviorErrorInfo) -> String {
  "- "
  <> error.behavior
  <> " ["
  <> error.error_type
  <> "]: "
  <> error.message
  <> case error.hint {
    "" -> ""
    hint -> "\n  Hint: " <> hint
  }
}

fn format_rule_violation_group(group: RuleViolationGroup) -> String {
  let violations_text =
    group.violations
    |> list.map(fn(v) {
      "  - " <> v.behavior <> ": " <> string.join(v.violations, ", ")
    })
    |> string.join("\n")

  group.rule <> " (" <> group.description <> "):\n" <> violations_text
}

/// Create a BehaviorFailure from check results
pub fn create_failure(
  feature_name: String,
  behavior: Behavior,
  check_result: checker_types.ResponseCheckResult,
  execution: ExecutionResult,
  base_url: String,
) -> BehaviorFailure {
  let problems =
    check_result.failed
    |> list.map(fn(check) {
      case check {
        checker_types.CheckFailed(field, rule, expected, actual, explanation) ->
          Problem(field, rule, expected, actual, explanation)
        checker_types.CheckPassed(_, _) -> Problem("", "", "", "", "")
        // Shouldn't happen
      }
    })

  // Add status mismatch as a problem if applicable
  let problems = case check_result.status_ok {
    True -> problems
    False -> [
      Problem(
        "status",
        "equals " <> int.to_string(check_result.status_expected),
        int.to_string(check_result.status_expected),
        int.to_string(check_result.status_actual),
        "HTTP status code mismatch",
      ),
      ..problems
    ]
  }

  let url = base_url <> behavior.request.path

  BehaviorFailure(
    feature: feature_name,
    behavior: behavior.name,
    intent: behavior.intent,
    problems: problems,
    request_sent: RequestSummary(
      method: method_to_string(behavior.request.method),
      url: url,
      headers: behavior.request.headers,
    ),
    response_received: ResponseSummary(
      status: execution.status,
      body: execution.body,
    ),
    hint: generate_hint(behavior, check_result),
    see_also: behavior.requires,
  )
}

fn generate_hint(
  _behavior: Behavior,
  check_result: checker_types.ResponseCheckResult,
) -> String {
  case check_result.status_ok {
    False ->
      case check_result.status_actual {
        404 ->
          "The resource might not exist. Check that prerequisite behaviors ran successfully."
        401 ->
          "Authentication may be required. Check that the auth token is being passed correctly."
        403 ->
          "Access denied. Check permissions and that the correct user is authenticated."
        500 -> "Server error. Check server logs for details."
        _ -> ""
      }
    True ->
      case list.length(check_result.failed) {
        0 -> ""
        _ -> "Check the field paths and expected values in the spec."
      }
  }
}

/// Create a BlockedBehavior
pub fn create_blocked(
  behavior_name: String,
  failed_dependency: String,
) -> BlockedBehavior {
  BlockedBehavior(
    behavior: behavior_name,
    reason: "Requires '" <> failed_dependency <> "' which failed",
    hint: "Fix '" <> failed_dependency <> "' first, then this will run",
  )
}

/// Create a BehaviorErrorInfo from error details
pub fn create_error_info(
  behavior_name: String,
  error_type: String,
  message: String,
) -> BehaviorErrorInfo {
  let hint = case error_type {
    "CONNECTION_REFUSED" ->
      "Check that the API server is running and accessible"
    "TIMEOUT" -> "The server took too long to respond. Check server health"
    "DNS_FAILURE" -> "Could not resolve hostname. Check the URL configuration"
    "SSL_ERROR" -> "SSL/TLS connection failed. Check certificate validity"
    "NETWORK_UNREACHABLE" -> "Network is unreachable. Check connectivity"
    "PERMISSION_DENIED" -> "Permission denied. Check firewall/access rules"
    "URL_PARSE_ERROR" -> "Invalid URL format. Check base_url and path"
    "INTERPOLATION_ERROR" ->
      "Variable substitution failed. Check captured values"
    "RESPONSE_PARSE_ERROR" ->
      "Could not parse response. Check Content-Type and body format"
    "SSRF_BLOCKED" -> "Request blocked for security. URL may be unsafe"
    _ -> ""
  }
  BehaviorErrorInfo(
    behavior: behavior_name,
    error_type: error_type,
    message: message,
    hint: hint,
  )
}

/// Wrap SpecResult in action-based JSON schema for AI consumption
pub fn spec_result_to_action_json(result: SpecResult, spec_name: String) -> Json {
  let exit_code = case result.pass {
    True -> 0
    False ->
      case result.blocked {
        0 -> 1
        _ -> 2
      }
  }

  let data = spec_result_to_json(result)

  // Build next_actions based on result
  let next_actions = case result.pass {
    True -> [
      json_output.next_action(
        "intent coverage " <> spec_name <> " --json",
        "Verify edge case coverage",
      ),
      json_output.next_action(
        "intent analyze " <> spec_name <> " --json",
        "Check overall quality score",
      ),
    ]
    False -> [
      json_output.next_action(
        "intent feedback " <> spec_name <> " --results <output.json>",
        "Generate fix tasks from failures",
      ),
      json_output.next_action(
        "intent doctor " <> spec_name <> " --json",
        "Get prioritized recommendations",
      ),
    ]
  }

  // Build errors from failures
  let errors =
    result.failures
    |> list.map(fn(f) {
      let problem_msg =
        f.problems
        |> list.map(fn(p) { p.explanation })
        |> string.join("; ")
      json_output.JsonError(
        code: "CHECK_FAILED",
        message: problem_msg,
        location: Some("behavior: " <> f.behavior),
        fix_hint: case f.hint {
          "" -> None
          h -> Some(h)
        },
        fix_command: Some(
          "intent prompt " <> spec_name <> " --behavior '" <> f.behavior <> "'",
        ),
      )
    })

  json_output.create_full_response(
    result.pass,
    "check_result",
    "check",
    data,
    errors,
    next_actions,
    Some(spec_name),
    exit_code,
  )
  |> json_output.to_json
}
