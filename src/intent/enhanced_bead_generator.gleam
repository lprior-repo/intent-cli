//// Enhanced Bead Generator
////
//// Generates enriched work items (beads) with:
//// - KIRK source tracing (which analysis finding spawned this bead)
//// - EARS requirements patterns
//// - Contract specifications (pre/post conditions)
//// - BDD test scenarios
//// - ATDD acceptance criteria
//// - Type definitions needed
////
//// This provides AI agents with comprehensive, self-documenting implementation context.

import gleam/dict
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/bead_templates
import intent/types

// =============================================================================
// TYPES
// =============================================================================

/// Traceability back to the KIRK analysis that spawned a bead
pub type KirkSource {
  KirkSource(
    analysis_type: String,
    finding_id: String,
    severity: String,
    category: String,
    original_text: String,
    suggestion: Option(String),
  )
}

/// Machine-readable acceptance criterion (ATDD)
pub type AcceptanceCriterion {
  AcceptanceCriterion(
    id: String,
    description: String,
    verification_type: String,
    check_expression: Option(String),
    verified: Bool,
  )
}

/// Simplified EARS pattern information for beads
pub type EarsPatternInfo {
  EarsPatternInfo(
    pattern_type: String,
    trigger: Option(String),
    state: Option(String),
    condition: Option(String),
    behavior: String,
  )
}

/// Contract check with metadata
pub type ContractCheck {
  ContractCheck(rule: String, why: String, check_name: String)
}

/// Contract specifications for a bead
pub type BeadContracts {
  BeadContracts(
    preconditions: List(String),
    postconditions: List(ContractCheck),
    invariants: List(String),
  )
}

/// BDD-style test scenario
pub type TestCase {
  TestCase(
    name: String,
    given: List(String),
    when: String,
    then: String,
    assertion: String,
  )
}

/// Type signature needed for implementation
pub type TypeDefinition {
  TypeDefinition(name: String, signature: String, purpose: String)
}

/// Enhanced bead with all AI implementation context
pub type EnhancedBead {
  EnhancedBead(
    id: String,
    title: String,
    description: String,
    // Source tracing
    source_type: String,
    kirk_sources: List(KirkSource),
    spec_path: Option(String),
    behavior_name: Option(String),
    // Methodology components
    ears_patterns: List(EarsPatternInfo),
    contracts: BeadContracts,
    scenarios: List(TestCase),
    acceptance_criteria: List(AcceptanceCriterion),
    types_needed: List(TypeDefinition),
    // Execution metadata
    effort: String,
    priority: Int,
    status: String,
    dependencies: List(String),
    blocks: List(String),
    round: Int,
    // Classification
    profile_type: String,
    issue_type: String,
    labels: List(String),
    // AI hints
    ai_hints: String,
    pitfalls: List(String),
  )
}

// =============================================================================
// LEGACY COMPAT TYPE (used by existing generate_enhanced_bead)
// =============================================================================

/// Legacy enhanced bead record wrapping a base BeadRecord
pub type EnhancedBeadRecord {
  EnhancedBeadRecord(
    base: bead_templates.BeadRecord,
    ears_patterns: List(EarsPatternInfo),
    contracts: BeadContracts,
    types: List(TypeDefinition),
    tests: List(TestCase),
  )
}

// =============================================================================
// CONSTRUCTORS
// =============================================================================

/// Create an empty BeadContracts
pub fn empty_contracts() -> BeadContracts {
  BeadContracts(preconditions: [], postconditions: [], invariants: [])
}

/// Generate a unique bead ID from components
pub fn make_bead_id(
  issue_type: String,
  category: String,
  index: Int,
) -> String {
  "bead-"
  <> slugify(issue_type)
  <> "-"
  <> slugify(category)
  <> "-"
  <> string.pad_left(int.to_string(index), 3, "0")
}

fn slugify(s: String) -> String {
  s
  |> string.lowercase
  |> string.replace(" ", "-")
  |> string.replace("_", "-")
}

/// Map severity string to priority int (1=highest, 5=lowest)
pub fn severity_to_priority(severity: String) -> Int {
  case string.lowercase(severity) {
    "critical" -> 1
    "high" -> 2
    "medium" -> 3
    "low" -> 4
    _ -> 3
  }
}

/// Map severity string to effort estimate
pub fn severity_to_effort(severity: String) -> String {
  case string.lowercase(severity) {
    "critical" -> "30min"
    "high" -> "20min"
    "medium" -> "15min"
    "low" -> "10min"
    _ -> "15min"
  }
}

// =============================================================================
// JSON SERIALIZATION
// =============================================================================

pub fn enhanced_bead_to_json(bead: EnhancedBead) -> Json {
  json.object([
    #("id", json.string(bead.id)),
    #("title", json.string(bead.title)),
    #("description", json.string(bead.description)),
    #("source_type", json.string(bead.source_type)),
    #(
      "kirk_sources",
      json.array(bead.kirk_sources, kirk_source_to_json),
    ),
    #("spec_path", json.nullable(bead.spec_path, json.string)),
    #("behavior_name", json.nullable(bead.behavior_name, json.string)),
    #(
      "ears_patterns",
      json.array(bead.ears_patterns, ears_pattern_to_json),
    ),
    #("contracts", contracts_to_json(bead.contracts)),
    #("scenarios", json.array(bead.scenarios, test_case_to_json)),
    #(
      "acceptance_criteria",
      json.array(bead.acceptance_criteria, acceptance_criterion_to_json),
    ),
    #(
      "types_needed",
      json.array(bead.types_needed, type_definition_to_json),
    ),
    #("effort", json.string(bead.effort)),
    #("priority", json.int(bead.priority)),
    #("status", json.string(bead.status)),
    #("dependencies", json.array(bead.dependencies, json.string)),
    #("blocks", json.array(bead.blocks, json.string)),
    #("round", json.int(bead.round)),
    #("profile_type", json.string(bead.profile_type)),
    #("issue_type", json.string(bead.issue_type)),
    #("labels", json.array(bead.labels, json.string)),
    #("ai_hints", json.string(bead.ai_hints)),
    #("pitfalls", json.array(bead.pitfalls, json.string)),
  ])
}

fn kirk_source_to_json(source: KirkSource) -> Json {
  json.object([
    #("analysis_type", json.string(source.analysis_type)),
    #("finding_id", json.string(source.finding_id)),
    #("severity", json.string(source.severity)),
    #("category", json.string(source.category)),
    #("original_text", json.string(source.original_text)),
    #("suggestion", json.nullable(source.suggestion, json.string)),
  ])
}

fn ears_pattern_to_json(pattern: EarsPatternInfo) -> Json {
  json.object([
    #("pattern_type", json.string(pattern.pattern_type)),
    #("trigger", json.nullable(pattern.trigger, json.string)),
    #("state", json.nullable(pattern.state, json.string)),
    #("condition", json.nullable(pattern.condition, json.string)),
    #("behavior", json.string(pattern.behavior)),
  ])
}

fn contracts_to_json(contracts: BeadContracts) -> Json {
  json.object([
    #("preconditions", json.array(contracts.preconditions, json.string)),
    #(
      "postconditions",
      json.array(contracts.postconditions, fn(c) {
        json.object([
          #("check_name", json.string(c.check_name)),
          #("rule", json.string(c.rule)),
          #("why", json.string(c.why)),
        ])
      }),
    ),
    #("invariants", json.array(contracts.invariants, json.string)),
  ])
}

fn test_case_to_json(tc: TestCase) -> Json {
  json.object([
    #("name", json.string(tc.name)),
    #("given", json.array(tc.given, json.string)),
    #("when", json.string(tc.when)),
    #("then", json.string(tc.then)),
    #("assertion", json.string(tc.assertion)),
  ])
}

fn acceptance_criterion_to_json(ac: AcceptanceCriterion) -> Json {
  json.object([
    #("id", json.string(ac.id)),
    #("description", json.string(ac.description)),
    #("verification_type", json.string(ac.verification_type)),
    #(
      "check_expression",
      json.nullable(ac.check_expression, json.string),
    ),
    #("verified", json.bool(ac.verified)),
  ])
}

fn type_definition_to_json(td: TypeDefinition) -> Json {
  json.object([
    #("name", json.string(td.name)),
    #("signature", json.string(td.signature)),
    #("purpose", json.string(td.purpose)),
  ])
}

// =============================================================================
// EXTRACTION FUNCTIONS
// =============================================================================

/// Extract contract specifications from behavior
pub fn extract_contracts_from_behavior(
  behavior: types.Behavior,
) -> BeadContracts {
  let preconditions = behavior.requires

  let postconditions =
    behavior.response.checks
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(check_name, check) = pair
      ContractCheck(rule: check.rule, why: check.why, check_name: check_name)
    })

  let invariants = []

  BeadContracts(
    preconditions: preconditions,
    postconditions: postconditions,
    invariants: invariants,
  )
}

/// Extract EARS patterns from intent text
pub fn extract_ears_patterns(intent: String) -> List(EarsPatternInfo) {
  let intent_upper = string.uppercase(intent)

  case try_event_driven_pattern(intent_upper, intent) {
    Ok(pattern) -> [pattern]
    Error(_) ->
      case try_state_driven_pattern(intent_upper, intent) {
        Ok(pattern) -> [pattern]
        Error(_) ->
          case try_optional_pattern(intent_upper, intent) {
            Ok(pattern) -> [pattern]
            Error(_) ->
              case try_unwanted_pattern(intent_upper, intent) {
                Ok(pattern) -> [pattern]
                Error(_) -> [make_ubiquitous_pattern(intent)]
              }
          }
      }
  }
}

fn try_event_driven_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "WHEN")
    && string.contains(intent_upper, "THE SYSTEM SHALL")
  {
    True ->
      parse_ears_pattern(
        intent_upper,
        "WHEN",
        "THE SYSTEM SHALL",
        fn(trigger, behavior) {
          EarsPatternInfo(
            pattern_type: "EventDriven",
            trigger: Some(trigger),
            state: None,
            condition: None,
            behavior: behavior,
          )
        },
      )
    False -> Error(Nil)
  }
}

fn try_state_driven_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "WHILE")
    && string.contains(intent_upper, "THE SYSTEM SHALL")
  {
    True ->
      parse_ears_pattern(
        intent_upper,
        "WHILE",
        "THE SYSTEM SHALL",
        fn(state, behavior) {
          EarsPatternInfo(
            pattern_type: "StateDriven",
            trigger: None,
            state: Some(state),
            condition: None,
            behavior: behavior,
          )
        },
      )
    False -> Error(Nil)
  }
}

fn try_optional_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "WHERE")
    && string.contains(intent_upper, "THE SYSTEM SHALL")
  {
    True ->
      parse_ears_pattern(
        intent_upper,
        "WHERE",
        "THE SYSTEM SHALL",
        fn(condition, behavior) {
          EarsPatternInfo(
            pattern_type: "Optional",
            trigger: None,
            state: None,
            condition: Some(condition),
            behavior: behavior,
          )
        },
      )
    False -> Error(Nil)
  }
}

fn try_unwanted_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "SHALL NOT")
    || string.contains(intent_upper, "THEN THE SYSTEM SHALL NOT")
  {
    True -> {
      let parts = string.split(intent_upper, "SHALL NOT")
      case parts {
        [before, after] -> {
          let condition =
            before
            |> string.replace("IF", "")
            |> string.replace("THEN THE SYSTEM", "")
            |> string.trim
            |> string.lowercase

          let behavior = string.trim(after) |> string.lowercase

          Ok(EarsPatternInfo(
            pattern_type: "Unwanted",
            trigger: None,
            state: None,
            condition: Some(condition),
            behavior: behavior,
          ))
        }
        _ -> Error(Nil)
      }
    }
    False -> Error(Nil)
  }
}

fn parse_ears_pattern(
  text: String,
  keyword: String,
  separator: String,
  builder: fn(String, String) -> EarsPatternInfo,
) -> Result(EarsPatternInfo, Nil) {
  let parts = string.split(text, separator)
  case parts {
    [before, after] -> {
      let prefix =
        before
        |> string.replace(keyword, "")
        |> string.trim
        |> string.lowercase

      let behavior = string.trim(after) |> string.lowercase

      Ok(builder(prefix, behavior))
    }
    _ -> Error(Nil)
  }
}

fn make_ubiquitous_pattern(intent: String) -> EarsPatternInfo {
  EarsPatternInfo(
    pattern_type: "Ubiquitous",
    trigger: None,
    state: None,
    condition: None,
    behavior: intent,
  )
}

/// Generate type definitions from behavior
pub fn generate_type_definitions(
  behavior: types.Behavior,
) -> List(TypeDefinition) {
  let method_str = types.method_to_string(behavior.request.method)
  let path_clean =
    behavior.request.path
    |> string.replace("/", "_")
    |> string.replace("{", "")
    |> string.replace("}", "")

  let handler_name = "handle" <> path_clean
  let handler_sig =
    "fn " <> handler_name <> "(request: Request) -> Response<ResponseBody>"

  let handler_def =
    TypeDefinition(
      name: handler_name,
      signature: handler_sig,
      purpose: method_str
        <> " "
        <> behavior.request.path
        <> " - "
        <> behavior.intent,
    )

  let request_body_def = case json.to_string(behavior.request.body) {
    "null" -> None
    _ ->
      Some(TypeDefinition(
        name: "RequestBody",
        signature: "type RequestBody",
        purpose: "Request payload for " <> behavior.request.path,
      ))
  }

  let response_body_def = case json.to_string(behavior.response.example) {
    "null" -> None
    _ ->
      Some(TypeDefinition(
        name: "ResponseBody",
        signature: "type ResponseBody",
        purpose: "Response payload for " <> behavior.request.path,
      ))
  }

  [handler_def]
  |> prepend_option(response_body_def)
  |> prepend_option(request_body_def)
}

fn prepend_option(lst: List(a), opt: Option(a)) -> List(a) {
  case opt {
    Some(value) -> [value, ..lst]
    None -> lst
  }
}

/// Generate test cases from behavior checks
pub fn generate_test_cases(behavior: types.Behavior) -> List(TestCase) {
  let method_str = types.method_to_string(behavior.request.method)
  let when_text =
    behavior.intent <> " with " <> method_str <> " " <> behavior.request.path

  behavior.response.checks
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(check_name, check) = pair
    TestCase(
      name: check_name <> " validation",
      given: behavior.requires,
      when: when_text,
      then: check.why,
      assertion: check.rule,
    )
  })
}

/// Generate enhanced bead from behavior (legacy interface)
pub fn generate_enhanced_bead(
  behavior: types.Behavior,
  profile_type: String,
) -> EnhancedBeadRecord {
  let base =
    bead_templates.BeadRecord(
      title: "Implement " <> behavior.name,
      description: behavior.intent,
      profile_type: profile_type,
      priority: 3,
      issue_type: "behavior_implementation",
      labels: behavior.tags,
      ai_hints: "Implement behavior: " <> behavior.intent,
      acceptance_criteria: [
        "Behavior implementation complete",
        "All checks pass",
        "Tests written and passing",
      ],
      dependencies: behavior.requires,
    )

  let contracts = extract_contracts_from_behavior(behavior)
  let ears_patterns = extract_ears_patterns(behavior.intent)
  let type_defs = generate_type_definitions(behavior)
  let test_cases = generate_test_cases(behavior)

  EnhancedBeadRecord(
    base: base,
    ears_patterns: ears_patterns,
    contracts: contracts,
    types: type_defs,
    tests: test_cases,
  )
}

/// Generate a full EnhancedBead from a behavior with all methodology components
pub fn behavior_to_enhanced_bead(
  behavior: types.Behavior,
  spec_path: String,
  index: Int,
) -> EnhancedBead {
  let contracts = extract_contracts_from_behavior(behavior)
  let ears_patterns = extract_ears_patterns(behavior.intent)
  let type_defs = generate_type_definitions(behavior)
  let test_cases = generate_test_cases(behavior)
  let method_str = types.method_to_string(behavior.request.method)

  let acceptance_criteria =
    behavior.response.checks
    |> dict.to_list
    |> list.index_map(fn(pair, i) {
      let #(_name, check) = pair
      AcceptanceCriterion(
        id: "AC-" <> string.pad_left(int.to_string(i + 1), 3, "0"),
        description: check.why,
        verification_type: "automated",
        check_expression: Some(check.rule),
        verified: False,
      )
    })

  EnhancedBead(
    id: make_bead_id("behavior", behavior.name, index),
    title: "Implement " <> behavior.name,
    description: behavior.intent,
    source_type: "spec",
    kirk_sources: [],
    spec_path: Some(spec_path),
    behavior_name: Some(behavior.name),
    ears_patterns: ears_patterns,
    contracts: contracts,
    scenarios: test_cases,
    acceptance_criteria: acceptance_criteria,
    types_needed: type_defs,
    effort: "15min",
    priority: 3,
    status: "pending",
    dependencies: behavior.requires,
    blocks: [],
    round: 1,
    profile_type: "api",
    issue_type: "behavior_implementation",
    labels: behavior.tags,
    ai_hints: "Implement "
      <> method_str
      <> " "
      <> behavior.request.path
      <> " - "
      <> behavior.intent,
    pitfalls: [],
  )
}
