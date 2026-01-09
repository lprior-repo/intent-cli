/// Spec Builder
/// Converts interview session answers into valid CUE specifications

import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import intent/case_insensitive.{contains_any_ignore_case}
import intent/checker.{type ResponseCheckResult}
import intent/http_client.{type ExecutionResult}
import intent/interpolate.{type Context}
import intent/interview.{type Answer, type InterviewSession, type Profile}
import intent/types.{
  type Behavior, type Spec, AIHints, Behavior,
  Config, Feature, Get, ImplementationHints,
  Request, Response, SecurityHints, Spec,
}

/// Generated CUE code
pub type GeneratedCUE {
  GeneratedCUE(package: String, imports: List(String), body: String)
}

/// Build a CUE spec from a completed interview session
pub fn build_spec_from_session(session: InterviewSession) -> String {
  build_spec_from_session_impl(session, False)
}

/// Build a CUE spec with KIRK analysis fields (inversions, pre_mortem)
/// Used when --with-analysis flag is set
pub fn build_spec_from_session_with_analysis(session: InterviewSession) -> String {
  build_spec_from_session_impl(session, True)
}

/// Build a LightSpec CUE from a light-mode interview session
/// Used when --light flag is set (simpler output, fewer questions)
pub fn build_light_spec_from_session(session: InterviewSession) -> String {
  let name = extract_name_from_answers(session.answers)
  let description = extract_description_from_answers(session.answers)
  let behaviors = extract_light_behaviors_from_answers(session.answers)

  // Generate minimal LightSpec CUE
  "// LightSpec: Minimal specification generated from light-mode interview\n"
  <> "// Session: " <> session.id <> "\n"
  <> "package api\n\n"
  <> "name: \"" <> escape_cue_string(name) <> "\"\n"
  <> "description: \"" <> escape_cue_string(description) <> "\"\n\n"
  <> "behaviors: [\n"
  <> behaviors
  <> "]\n"
}

/// Extract name from answers (first answer typically contains intent/name)
fn extract_name_from_answers(answers: List(Answer)) -> String {
  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, ["name", "what", "api", "command", "main"])
  })
  |> list.first()
  |> result.map(fn(a) { string.trim(a.response) })
  |> result.unwrap("Untitled Specification")
}

/// Extract description from answers
fn extract_description_from_answers(answers: List(Answer)) -> String {
  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, ["description", "what", "accomplish", "purpose"])
  })
  |> list.first()
  |> result.map(fn(a) { string.trim(a.response) })
  |> result.unwrap("Light specification from interview")
}

/// Extract behaviors in LightBehavior format from answers
fn extract_light_behaviors_from_answers(answers: List(Answer)) -> String {
  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, ["flow", "step", "behavior", "happen"])
  })
  |> list.flat_map(fn(answer) {
    // Split response into steps if it contains arrows or numbered items
    let response = string.trim(answer.response)
    case string.contains(response, "→") {
      True ->
        response
        |> string.split("→")
        |> list.map(string.trim)
      False ->
        case string.contains(response, "\n") {
          True ->
            response
            |> string.split("\n")
            |> list.map(string.trim)
            |> list.filter(fn(s) { string.length(s) > 0 })
          False -> [response]
        }
    }
  })
  |> list.index_map(fn(step, idx) {
    let behavior_name = "step_" <> int.to_string(idx + 1)
    "\t{\n"
    <> "\t\tname: \"" <> escape_cue_string(behavior_name) <> "\"\n"
    <> "\t\tintent: \"" <> escape_cue_string(step) <> "\"\n"
    <> "\t\trequest: {\n"
    <> "\t\t\tmethod: \"GET\"\n"
    <> "\t\t\tpath: \"/\"\n"
    <> "\t\t}\n"
    <> "\t\tresponse: {\n"
    <> "\t\t\tstatus: 200\n"
    <> "\t\t}\n"
    <> "\t},\n"
  })
  |> string.concat()
}

import gleam/result

fn build_spec_from_session_impl(session: InterviewSession, include_analysis: Bool) -> String {
  let features = extract_features_from_answers(session.answers)
  let behaviors = extract_behaviors_from_answers(session.answers, session.profile)
  let constraints = extract_constraints_from_answers(session.answers)
  let security = extract_security_requirements(session.answers)
  let non_functional =
    extract_non_functional_requirements(session.answers)

  // Build a minimal Spec type for CUE generation
  let body = build_spec_body(
    features,
    behaviors,
    constraints,
    security,
    non_functional,
  )

  // Add KIRK analysis fields if requested
  let full_body = case include_analysis {
    True -> {
      body <> "\n" <> build_kirk_analysis_section(session.answers)
    }
    False -> body
  }

  let spec = GeneratedCUE(
    package: "package api",
    imports: [],
    body: full_body,
  )

  spec.package <> "\n\n" <> spec.body
}

/// Build KIRK analysis section from interview answers
fn build_kirk_analysis_section(answers: List(Answer)) -> String {
  // Extract inversion-related answers (questions about what could go wrong)
  let inversions = extract_inversions_from_answers(answers)

  // Extract pre-mortem answers (failure scenarios)
  let pre_mortem = extract_premortem_from_answers(answers)

  // Build the KIRK section
  "\n// KIRK Analysis (generated with --with-analysis)\n"
  <> "kirk: {\n"
  <> "\tinversions: [\n"
  <> inversions
  <> "\t]\n"
  <> "\tpre_mortem: [\n"
  <> pre_mortem
  <> "\t]\n"
  <> "}\n"
}

/// Extract inversion points from error/security related answers
fn extract_inversions_from_answers(answers: List(Answer)) -> String {
  let inversion_keywords = ["wrong", "fail", "error", "problem", "security", "attack", "vulnerability"]

  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, inversion_keywords)
    || contains_any_ignore_case(answer.response, inversion_keywords)
  })
  |> list.map(fn(answer) {
    let escaped = escape_cue_string(string.trim(answer.response))
    "\t\t\"" <> escaped <> "\",\n"
  })
  |> string.concat()
}

/// Extract pre-mortem scenarios from answers
fn extract_premortem_from_answers(answers: List(Answer)) -> String {
  let premortem_keywords = ["deadline", "late", "missing", "blocked", "dependency", "risk", "scale"]

  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, premortem_keywords)
    || contains_any_ignore_case(answer.response, premortem_keywords)
  })
  |> list.map(fn(answer) {
    let escaped = escape_cue_string(string.trim(answer.response))
    "\t\t\"" <> escaped <> "\",\n"
  })
  |> string.concat()
}

/// Escape strings for CUE output
fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}

/// Extract feature names/titles from answers
pub fn extract_features_from_answers(answers: List(Answer)) -> List(String) {
  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, ["feature", "capability"])
  })
  |> list.map(fn(answer) {
    let trimmed = string.trim(answer.response)
    case string.length(trimmed) > 0 {
      True -> trimmed
      False -> ""
    }
  })
  |> list.filter(fn(s) { s != "" })
}

/// Extract API behaviors (methods, paths, status codes)
pub fn extract_behaviors_from_answers(
  answers: List(Answer),
  _profile: Profile,
) -> String {
  let api_answers = list.filter(answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["endpoint", "path", "method"])
  })

  case api_answers {
    [] ->
      "// Define API behaviors here
behaviors: {
  // Add endpoint definitions
}"
    answers ->
      "// API behaviors from interview
behaviors: {
"
      <> {
        list.map(answers, fn(answer) {
          "  // " <> answer.question_text <> "
  // " <> string.trim(answer.response)
        })
        |> string.join("\n")
      }
      <> "\n}"
  }
}

/// Extract constraints from answers
pub fn extract_constraints_from_answers(answers: List(Answer)) -> List(String) {
  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, ["constraint", "limit", "requirement"])
  })
  |> list.map(fn(answer) {
    string.trim(answer.response)
  })
  |> list.filter(fn(s) { s != "" })
}

/// Extract security requirements from answers
pub fn extract_security_requirements(answers: List(Answer)) -> String {
  let security_answers = list.filter(answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["auth", "security", "permission"])
  })

  case security_answers {
    [] ->
      "security: {
  authentication: \"todo\"
  authorization: \"todo\"
}"
    answers ->
      "security: {
"
      <> {
        list.map(answers, fn(answer) {
          "  // " <> answer.question_text <> "
  requirement: \"" <> string.trim(answer.response) <> "\""
        })
        |> string.join("\n")
      }
      <> "\n}"
  }
}

/// Extract non-functional requirements (SLA, scale, monitoring)
pub fn extract_non_functional_requirements(answers: List(Answer)) -> List(String) {
  answers
  |> list.filter(fn(answer) {
    contains_any_ignore_case(answer.question_text, ["sla", "scale", "performance", "monitoring", "latency"])
  })
  |> list.map(fn(answer) {
    string.trim(answer.response)
  })
  |> list.filter(fn(s) { s != "" })
}

/// Build the main body of the spec
fn build_spec_body(
  features: List(String),
  behaviors: String,
  constraints: List(String),
  security: String,
  non_functional: List(String),
) -> String {
  let features_section = case features {
    [] ->
      "// Features
features: {
  // Add feature definitions
}"
    features ->
      "// Features extracted from interview
features: {
" <> {
        list.map(features, fn(feature) {
          "  \"" <> feature <> "\": true"
        })
        |> string.join("\n")
      } <> "\n}"
  }

  let constraints_section = case constraints {
    [] -> ""
    constraints ->
      "\n\n// Constraints and requirements
constraints: {
" <> {
        list.map(constraints, fn(constraint) {
          "  // " <> constraint
        })
        |> string.join("\n")
      } <> "\n}"
  }

  let non_functional_section = case non_functional {
    [] -> ""
    nf ->
      "\n\n// Non-functional requirements
nonFunctional: {
" <> {
        list.map(nf, fn(requirement) {
          "  // " <> requirement
        })
        |> string.join("\n")
      } <> "\n}"
  }

  features_section
  <> "\n\n"
  <> behaviors
  <> "\n\n"
  <> security
  <> constraints_section
  <> non_functional_section
}

/// Create a test spec with N behaviors - pure functional composition
pub fn create_test_spec(behavior_count: Int) -> Spec {
  let behaviors =
    list.range(1, behavior_count)
    |> list.map(fn(i) { make_behavior("b" <> int.to_string(i)) })
  Spec(
    name: "test", description: "test", audience: "test", version: "1.0.0",
    success_criteria: [], config: Config("http://test", 1000, dict.new()),
    features: [Feature("test-feature", "test", behaviors)],
    rules: [], anti_patterns: [],
    ai_hints: AIHints(ImplementationHints([]), dict.new(), SecurityHints("", "", "", ""), [], option.None),
  )
}

fn make_behavior(name: String) -> Behavior {
  Behavior(name:, intent: "test", notes: "", requires: [], tags: [],
    request: Request(Get, "/", dict.new(), dict.new(), json.null()),
    response: Response(200, json.null(), dict.new(), dict.new()), captures: dict.new())
}

/// Batch check behaviors against results - pure map operation
pub fn check_many(
  behaviors: List(Behavior),
  results: List(ExecutionResult),
  ctx: Context,
) -> List(ResponseCheckResult) {
  list.map2(behaviors, results, fn(b, r) {
    checker.check_response(b.response, r, ctx)
  })
}
