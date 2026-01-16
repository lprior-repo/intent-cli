//// Mental Lattice Module
////
//// Applies 5 thinking models to requirements for comprehensive analysis:
//// 1. Inversion - What could fail? What should we NOT do?
//// 2. Second-Order Effects - What happens AFTER that? Long-term consequences?
//// 3. Pre-Mortem - Imagine this failed. Why did it fail?
//// 4. Checklist - What are we missing? What did we forget?
//// 5. Circle of Competence - What's in scope? What's outside our expertise?
////
//// This module provides Railway-Oriented error handling and produces
//// structured analysis results that can be used to refine requirements.

import gleam/list
import gleam/result
import gleam/string
import intent/kirk/ears_parser

/// The 5 mental lattice models for requirement analysis
pub type LatticeModel {
  /// What could fail? What should we NOT do?
  Inversion
  /// What happens AFTER that? Long-term consequences?
  SecondOrder
  /// Imagine this failed. Why did it fail?
  PreMortem
  /// What are we missing? What did we forget?
  Checklist
  /// What's in scope? What's outside our expertise?
  CircleOfCompetence
}

/// Result of applying a lattice model to a requirement
pub type LatticeAnalysis {
  LatticeAnalysis(
    model: LatticeModel,
    requirement: ears_parser.EarsRequirement,
    insights: List(String),
    warnings: List(String),
    questions: List(String),
    confidence: Float,
  )
}

/// Session tracking which models have been applied
pub type LatticeSession {
  LatticeSession(
    session_id: String,
    requirements: List(ears_parser.EarsRequirement),
    analyses: List(LatticeAnalysis),
    completed_models: List(LatticeModel),
  )
}

/// Error types for lattice analysis operations
pub type LatticeError {
  InvalidRequirement(reason: String)
  AnalysisFailed(model: LatticeModel, reason: String)
  NoRequirementsProvided
}

/// Create a new lattice analysis session
pub fn new_session(
  session_id: String,
  requirements: List(ears_parser.EarsRequirement),
) -> Result(LatticeSession, LatticeError) {
  case requirements {
    [] -> Error(NoRequirementsProvided)
    reqs ->
      Ok(
        LatticeSession(
          session_id: session_id,
          requirements: reqs,
          analyses: [],
          completed_models: [],
        ),
      )
  }
}

/// Apply inversion thinking: What could fail? What should we NOT do?
///
/// Examines negative cases, failure modes, and anti-patterns to identify risks.
pub fn apply_inversion(
  requirement: ears_parser.EarsRequirement,
) -> Result(LatticeAnalysis, LatticeError) {
  let insights = []
  let warnings = []
  let questions = []

  // Analyze for failure modes in the behavior
  let behavior_lower = string.lowercase(requirement.system_shall)

  // Check for positive-only thinking (no failure handling)
  let insights = case
    string.contains(behavior_lower, "error")
    || string.contains(behavior_lower, "fail")
    || string.contains(behavior_lower, "invalid")
  {
    False -> [
      "Consider: What happens when this behavior fails?",
      "Consider: What should NOT happen during this behavior?",
      ..insights
    ]
    True -> insights
  }

  // Check for missing negative constraints
  let questions = case requirement.pattern {
    ears_parser.Unwanted -> questions
    _ -> [
      "What is the opposite of this requirement?",
      "Under what conditions should this behavior NOT occur?",
      "What would break if we did the inverse?",
      ..questions
    ]
  }

  // Check for unbounded operations
  let warnings = case
    string.contains(behavior_lower, "all")
    || string.contains(behavior_lower, "every")
  {
    True -> [
      "Warning: Unbounded operations detected - consider limits/pagination",
      ..warnings
    ]
    False -> warnings
  }

  Ok(LatticeAnalysis(
    model: Inversion,
    requirement: requirement,
    insights: list.reverse(insights),
    warnings: list.reverse(warnings),
    questions: list.reverse(questions),
    confidence: calculate_confidence(insights, warnings, questions),
  ))
}

/// Apply second-order thinking: What happens AFTER that?
///
/// Examines cascading effects, long-term consequences, and ripple impacts.
pub fn apply_second_order(
  requirement: ears_parser.EarsRequirement,
) -> Result(LatticeAnalysis, LatticeError) {
  let insights = []
  let warnings = []
  let questions = []

  let behavior_lower = string.lowercase(requirement.system_shall)

  // Check for state-changing operations
  let insights = case
    string.contains(behavior_lower, "create")
    || string.contains(behavior_lower, "update")
    || string.contains(behavior_lower, "delete")
    || string.contains(behavior_lower, "send")
  {
    True -> [
      "This behavior changes state - consider downstream dependencies",
      "Consider: What other systems/behaviors depend on this change?",
      ..insights
    ]
    False -> insights
  }

  // Check for notification/event triggers
  let questions = [
    "What happens to dependent behaviors after this completes?",
    "What second-order effects could this have on other features?",
    "Does this trigger any events or notifications?",
    ..questions
  ]

  // Check for temporal dependencies
  let warnings = case requirement.pattern {
    ears_parser.EventDriven -> [
      "Event-driven behavior - verify event ordering and idempotency",
      ..warnings
    ]
    ears_parser.StateDriven -> [
      "State-driven behavior - verify state transition safety",
      ..warnings
    ]
    _ -> warnings
  }

  Ok(LatticeAnalysis(
    model: SecondOrder,
    requirement: requirement,
    insights: list.reverse(insights),
    warnings: list.reverse(warnings),
    questions: list.reverse(questions),
    confidence: calculate_confidence(insights, warnings, questions),
  ))
}

/// Apply pre-mortem analysis: Imagine this failed. Why did it fail?
///
/// Projects into the future, assumes failure, and works backward to causes.
pub fn apply_pre_mortem(
  requirement: ears_parser.EarsRequirement,
) -> Result(LatticeAnalysis, LatticeError) {
  let insights = []
  let warnings = []
  let questions = []

  let behavior_lower = string.lowercase(requirement.system_shall)

  // Common failure modes
  let questions = [
    "Imagine: This requirement failed in production. What was the root cause?",
    "Imagine: Users are complaining about this behavior. What went wrong?",
    "Imagine: This created a security incident. What was the vulnerability?",
    ..questions
  ]

  // Check for external dependencies (common failure points)
  let insights = case
    string.contains(behavior_lower, "api")
    || string.contains(behavior_lower, "service")
    || string.contains(behavior_lower, "database")
    || string.contains(behavior_lower, "network")
  {
    True -> [
      "External dependency detected - failure mode: service unavailable",
      "External dependency detected - failure mode: timeout",
      "External dependency detected - failure mode: invalid response",
      ..insights
    ]
    False -> insights
  }

  // Check for data operations (common failure points)
  let warnings = case
    string.contains(behavior_lower, "parse")
    || string.contains(behavior_lower, "decode")
    || string.contains(behavior_lower, "validate")
  {
    True -> [
      "Data operation detected - failure mode: malformed input",
      "Data operation detected - failure mode: unexpected format",
      ..warnings
    ]
    False -> warnings
  }

  Ok(LatticeAnalysis(
    model: PreMortem,
    requirement: requirement,
    insights: list.reverse(insights),
    warnings: list.reverse(warnings),
    questions: list.reverse(questions),
    confidence: calculate_confidence(insights, warnings, questions),
  ))
}

/// Apply checklist thinking: What are we missing? What did we forget?
///
/// Systematically verifies completeness against known categories.
pub fn apply_checklist(
  requirement: ears_parser.EarsRequirement,
) -> Result(LatticeAnalysis, LatticeError) {
  let insights = []
  let warnings = []
  let questions = []

  let behavior_lower = string.lowercase(requirement.system_shall)

  // Security checklist
  let questions = case
    string.contains(behavior_lower, "auth")
    || string.contains(behavior_lower, "user")
    || string.contains(behavior_lower, "data")
  {
    True -> [
      "Security: Is authentication required?",
      "Security: Is authorization checked?",
      "Security: Is sensitive data encrypted?",
      "Security: Are inputs validated against injection attacks?",
      ..questions
    ]
    False -> questions
  }

  // Performance checklist
  let questions = [
    "Performance: What are the acceptable response times?",
    "Performance: What are the scale limits?",
    "Performance: Is caching needed?",
    ..questions
  ]

  // Observability checklist
  let insights = [
    "Observability: Consider logging for debugging",
    "Observability: Consider metrics for monitoring",
    "Observability: Consider tracing for distributed calls",
    ..insights
  ]

  // Error handling checklist
  let warnings = case
    string.contains(behavior_lower, "error")
    || string.contains(behavior_lower, "fail")
  {
    True -> warnings
    False -> [
      "Error handling: No error cases mentioned - add failure scenarios",
      ..warnings
    ]
  }

  // Testing checklist
  let questions = [
    "Testing: What are the happy path test cases?",
    "Testing: What are the edge cases?",
    "Testing: What are the error cases?",
    ..questions
  ]

  Ok(LatticeAnalysis(
    model: Checklist,
    requirement: requirement,
    insights: list.reverse(insights),
    warnings: list.reverse(warnings),
    questions: list.reverse(questions),
    confidence: calculate_confidence(insights, warnings, questions),
  ))
}

/// Apply circle of competence: What's in scope? What's outside our expertise?
///
/// Identifies scope boundaries and areas requiring specialist knowledge.
pub fn apply_circle_of_competence(
  requirement: ears_parser.EarsRequirement,
) -> Result(LatticeAnalysis, LatticeError) {
  let insights = []
  let warnings = []
  let questions = []

  let behavior_lower = string.lowercase(requirement.system_shall)

  // Check for specialized domains
  let specialist_domains = [
    #("crypto", "Cryptography"),
    #("encrypt", "Cryptography"),
    #("hash", "Cryptography"),
    #("secure", "Security"),
    #("auth", "Authentication/Authorization"),
    #("payment", "Payment Processing"),
    #("compliance", "Regulatory Compliance"),
    #("gdpr", "Data Privacy/GDPR"),
    #("hipaa", "Healthcare Compliance"),
    #("ai", "Machine Learning/AI"),
    #("ml", "Machine Learning/AI"),
    #("blockchain", "Distributed Systems/Blockchain"),
  ]

  let detected_domains =
    specialist_domains
    |> list.filter(fn(domain) {
      let #(keyword, _name) = domain
      string.contains(behavior_lower, keyword)
    })
    |> list.map(fn(domain) {
      let #(_keyword, name) = domain
      name
    })

  let warnings = case detected_domains {
    [] -> warnings
    domains -> {
      let domain_list = string.join(domains, ", ")
      [
        "Specialist domain detected: "
          <> domain_list
          <> " - consider expert review",
        ..warnings
      ]
    }
  }

  // Check for integration complexity
  let insights = case
    string.contains(behavior_lower, "integrate")
    || string.contains(behavior_lower, "third-party")
    || string.contains(behavior_lower, "external")
  {
    True -> [
      "Integration complexity detected - verify API contracts",
      "Integration complexity detected - consider vendor lock-in",
      ..insights
    ]
    False -> insights
  }

  // Scope questions
  let questions = [
    "Is this requirement within our team's core competency?",
    "Do we have the expertise to implement this correctly?",
    "Should this be delegated to a specialist team?",
    "What research or training is needed before implementation?",
    ..questions
  ]

  Ok(LatticeAnalysis(
    model: CircleOfCompetence,
    requirement: requirement,
    insights: list.reverse(insights),
    warnings: list.reverse(warnings),
    questions: list.reverse(questions),
    confidence: calculate_confidence(insights, warnings, questions),
  ))
}

/// Apply all 5 lattice models to a requirement
pub fn apply_all_models(
  requirement: ears_parser.EarsRequirement,
) -> Result(List(LatticeAnalysis), LatticeError) {
  use inversion <- result.try(apply_inversion(requirement))
  use second_order <- result.try(apply_second_order(requirement))
  use pre_mortem <- result.try(apply_pre_mortem(requirement))
  use checklist <- result.try(apply_checklist(requirement))
  use circle <- result.try(apply_circle_of_competence(requirement))

  Ok([inversion, second_order, pre_mortem, checklist, circle])
}

/// Apply all models to all requirements in a session
pub fn analyze_session(
  session: LatticeSession,
) -> Result(LatticeSession, LatticeError) {
  let all_analyses =
    session.requirements
    |> list.map(apply_all_models)
    |> result.all
    |> result.map(list.flatten)

  case all_analyses {
    Ok(analyses) ->
      Ok(
        LatticeSession(
          ..session,
          analyses: analyses,
          completed_models: [
            Inversion,
            SecondOrder,
            PreMortem,
            Checklist,
            CircleOfCompetence,
          ],
        ),
      )
    Error(err) -> Error(err)
  }
}

/// Get analyses for a specific model
pub fn get_analyses_by_model(
  session: LatticeSession,
  model: LatticeModel,
) -> List(LatticeAnalysis) {
  session.analyses
  |> list.filter(fn(analysis) { analysis.model == model })
}

/// Get all analyses for a specific requirement
pub fn get_analyses_for_requirement(
  session: LatticeSession,
  requirement: ears_parser.EarsRequirement,
) -> List(LatticeAnalysis) {
  session.analyses
  |> list.filter(fn(analysis) {
    analysis.requirement.raw_text == requirement.raw_text
  })
}

/// Calculate confidence score based on analysis output
fn calculate_confidence(
  insights: List(String),
  warnings: List(String),
  questions: List(String),
) -> Float {
  let insight_count = list.length(insights)
  let warning_count = list.length(warnings)
  let question_count = list.length(questions)
  let total = insight_count + warning_count + question_count

  case total {
    0 -> 0.3
    // Low confidence if no findings
    1 -> 0.5
    2 -> 0.6
    3 -> 0.7
    4 -> 0.8
    _ -> 0.9
    // High confidence if many findings
  }
}

/// Convert lattice model to display name
pub fn model_name(model: LatticeModel) -> String {
  case model {
    Inversion -> "Inversion"
    SecondOrder -> "Second-Order Effects"
    PreMortem -> "Pre-Mortem"
    Checklist -> "Checklist"
    CircleOfCompetence -> "Circle of Competence"
  }
}

/// Get description of what a model analyzes
pub fn model_description(model: LatticeModel) -> String {
  case model {
    Inversion -> "What could fail? What should we NOT do?"
    SecondOrder -> "What happens AFTER that? Long-term consequences?"
    PreMortem -> "Imagine this failed. Why did it fail?"
    Checklist -> "What are we missing? What did we forget?"
    CircleOfCompetence -> "What's in scope? What's outside our expertise?"
  }
}
