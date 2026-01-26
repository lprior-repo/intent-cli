/// Vision Critique Protocol - Skeptical PM Persona
///
/// This module implements validation logic for the Vision phase following
/// the "Skeptical PM" critique protocol from INTENT_4_PLAN.md
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import intent/vision_types.{type VisionSection}

// =============================================================================
// Types - Vision Phase Specific
// =============================================================================

// Scenario and VisionSection are imported from vision_types to maintain
// a single source of truth for these core planning types

/// The three critique questions from Skeptical PM
pub type CritiqueQuestion {
  ProblemReality
  PersonaValidation
  VorpStrength
}

/// Issue severity levels
pub type Severity {
  Critical
  Warning
}

/// A single critique issue with context and suggestion
pub type CritiqueIssue {
  CritiqueIssue(
    question: CritiqueQuestion,
    severity: Severity,
    message: String,
    suggestion: String,
  )
}

/// Overall critique result with pass/fail and scoring
pub type CritiqueResult {
  CritiqueResult(passed: Bool, issues: List(CritiqueIssue), score: Int)
}

// =============================================================================
// Constants
// =============================================================================

const buzzwords = [
  "revolutionary", "game-changing", "innovative", "next-gen", "cutting-edge",
  "disruptive", "transformative",
]

const weak_vorp_phrases = [
  "better", "improved", "enhanced", "faster", "easier", "simpler",
]

const vague_personas = [
  "developers", "users", "engineers", "people", "companies", "customers",
  "teams",
]

const min_scenarios = 2

const min_non_personas = 1

const pass_threshold = 70

const critical_penalty = 25

const warning_penalty = 5

// =============================================================================
// Helper Functions (DRY)
// =============================================================================

/// Add an issue to the issues list (eliminates duplication)
fn add_issue(
  issues: List(CritiqueIssue),
  question: CritiqueQuestion,
  severity: Severity,
  message: String,
  suggestion: String,
) -> List(CritiqueIssue) {
  list.append(issues, [CritiqueIssue(question, severity, message, suggestion)])
}

// =============================================================================
// Problem Reality Validation
// =============================================================================

/// Validate that the problem is concrete and not wishful thinking
pub fn validate_problem_reality(vision: VisionSection) -> List(CritiqueIssue) {
  let issues = []

  // Check for empty press release
  let issues = case string.trim(vision.press_release) {
    "" ->
      add_issue(
        issues,
        ProblemReality,
        Critical,
        "Press release is empty",
        "Write a concrete 2-3 sentence press release describing the specific problem and its impact",
      )
    _ -> issues
  }

  // Check for buzzword overload
  let buzzword_count =
    buzzwords
    |> list.filter(fn(word) {
      string.contains(string.lowercase(vision.press_release), word)
    })
    |> list.length

  let issues = case buzzword_count > 2 {
    True ->
      add_issue(
        issues,
        ProblemReality,
        Warning,
        "Press release contains multiple buzzwords ("
          <> int.to_string(buzzword_count)
          <> " found)",
        "Replace generic terms with specific pain points and measurable impacts",
      )
    False -> issues
  }

  // Check for vague north star
  let north_star_len = string.length(string.trim(vision.north_star))
  let issues = case north_star_len {
    0 ->
      add_issue(
        issues,
        ProblemReality,
        Critical,
        "North star is empty",
        "Define a clear, achievable ideal user journey with specific outcomes",
      )
    n if n < 20 ->
      add_issue(
        issues,
        ProblemReality,
        Warning,
        "North star is too vague or short",
        "Expand the north star to include specific actions, outcomes, and success criteria",
      )
    _ -> issues
  }

  // Check for missing boundaries
  let issues = case list.length(vision.out_of_scope) {
    0 ->
      add_issue(
        issues,
        ProblemReality,
        Warning,
        "No boundaries defined (out_of_scope is empty)",
        "Define what's explicitly out of scope to show understanding of problem boundaries",
      )
    _ -> issues
  }

  issues
}

// =============================================================================
// Persona Validation
// =============================================================================

/// Validate that the persona is specific and validated (not assumed)
pub fn validate_persona(vision: VisionSection) -> List(CritiqueIssue) {
  let issues = []

  // Check for vague persona
  let is_vague =
    vague_personas
    |> list.any(fn(vague) {
      string.lowercase(vision.persona) == vague
      || string.starts_with(string.lowercase(vision.persona), vague <> " ")
    })

  let issues = case is_vague {
    True ->
      add_issue(
        issues,
        PersonaValidation,
        Critical,
        "Persona is too vague: '" <> vision.persona <> "'",
        "Be specific about role, context, and constraints (e.g., 'Backend engineers building microservices with 5+ REST APIs')",
      )
    False -> issues
  }

  // Check for missing non-personas
  let issues = case list.length(vision.non_personas) {
    0 ->
      add_issue(
        issues,
        PersonaValidation,
        Critical,
        "No non-personas defined (who is this NOT for?)",
        "Define at least "
          <> int.to_string(min_non_personas)
          <> " non-persona(s) to clarify boundaries and validate understanding",
      )
    _ -> issues
  }

  // Check for insufficient scenarios
  let scenario_count = list.length(vision.scenarios)
  let issues = case scenario_count {
    0 ->
      add_issue(
        issues,
        PersonaValidation,
        Critical,
        "No scenarios provided",
        "Create at least "
          <> int.to_string(min_scenarios)
          <> " concrete scenarios with character, motivation, simulation, and outcome",
      )
    1 ->
      add_issue(
        issues,
        PersonaValidation,
        Warning,
        "Only 1 scenario provided, need at least "
          <> int.to_string(min_scenarios),
        "Add more scenarios to validate the persona across different contexts",
      )
    _ -> issues
  }

  // Check for empty scenario fields
  let empty_scenarios =
    vision.scenarios
    |> list.filter(fn(scenario) {
      string.trim(scenario.character) == ""
      || string.trim(scenario.motivation) == ""
      || string.trim(scenario.outcome) == ""
    })
    |> list.length

  let issues = case empty_scenarios > 0 {
    True ->
      add_issue(
        issues,
        PersonaValidation,
        Warning,
        int.to_string(empty_scenarios) <> " scenario(s) have empty fields",
        "Fill in all scenario fields (character, motivation, simulation, outcome) with concrete details",
      )
    False -> issues
  }

  issues
}

// =============================================================================
// VORP Strength Validation
// =============================================================================

/// Validate that the VORP is 10x better, not 10%
pub fn validate_vorp_strength(vision: VisionSection) -> List(CritiqueIssue) {
  let issues = []

  // Check for missing replaces field
  let issues = case vision.replaces {
    option.None ->
      add_issue(
        issues,
        VorpStrength,
        Critical,
        "No replacement product defined (replaces field is None)",
        "Identify what users currently use to solve this problem - can't measure VORP without a baseline",
      )
    option.Some(replaces) ->
      case string.trim(replaces) {
        "" ->
          add_issue(
            issues,
            VorpStrength,
            Critical,
            "Replacement product is empty string",
            "Describe the current solution users employ (even if it's 'manual process' or 'spreadsheets')",
          )
        _ -> issues
      }
  }

  // Check for empty VORP
  let issues = case string.trim(vision.vorp) {
    "" ->
      add_issue(
        issues,
        VorpStrength,
        Critical,
        "VORP is empty",
        "Describe the 10x improvement with concrete metrics (time, cost, errors, etc.)",
      )
    _ -> issues
  }

  // Check for weak VORP language
  let weak_phrase_count =
    weak_vorp_phrases
    |> list.filter(fn(phrase) {
      string.contains(string.lowercase(vision.vorp), phrase)
    })
    |> list.length

  let issues = case weak_phrase_count > 2 {
    True ->
      add_issue(
        issues,
        VorpStrength,
        Warning,
        "VORP uses weak comparative language ("
          <> int.to_string(weak_phrase_count)
          <> " weak phrases)",
        "Replace 'better/faster/easier' with concrete metrics showing 10x improvement (e.g., '10x faster: 30 seconds vs 4 hours')",
      )
    False -> issues
  }

  // Check for presence of metrics
  let has_metrics =
    string.contains(vision.vorp, "10x")
    || string.contains(vision.vorp, "100x")
    || {
      let digits =
        vision.vorp
        |> string.to_graphemes
        |> list.filter(fn(char) {
          case char {
            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
            _ -> False
          }
        })
      list.length(digits) >= 2
    }

  let issues = case has_metrics {
    False ->
      add_issue(
        issues,
        VorpStrength,
        Warning,
        "VORP lacks concrete metrics or quantified improvements",
        "Add specific numbers showing order-of-magnitude improvement (10x time savings, 100x fewer errors, etc.)",
      )
    True -> issues
  }

  issues
}

// =============================================================================
// Overall Critique
// =============================================================================

/// Run all critique validators and aggregate results with scoring
pub fn critique_vision(vision: VisionSection) -> CritiqueResult {
  let problem_issues = validate_problem_reality(vision)
  let persona_issues = validate_persona(vision)
  let vorp_issues = validate_vorp_strength(vision)

  let all_issues = list.flatten([problem_issues, persona_issues, vorp_issues])

  let critical_count =
    all_issues
    |> list.filter(fn(issue) {
      case issue.severity {
        Critical -> True
        Warning -> False
      }
    })
    |> list.length

  let warning_count =
    all_issues
    |> list.filter(fn(issue) {
      case issue.severity {
        Warning -> True
        Critical -> False
      }
    })
    |> list.length

  let raw_score =
    100
    - { critical_count * critical_penalty }
    - { warning_count * warning_penalty }
  let score = case raw_score < 0 {
    True -> 0
    False -> raw_score
  }

  let passed = score >= pass_threshold

  CritiqueResult(passed: passed, issues: all_issues, score: score)
}
