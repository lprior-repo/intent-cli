// KIRK Empathy Simulator
// Simulates cognitive limitations API consumers face
// Based on:
// - Miller's Law: 7 +/- 2 items in working memory
// - Cognitive Load Theory: intrinsic, extraneous, germane load
// - Expert Blind Spot: experts assume too much knowledge

import gleam/dict
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import intent/types.{type Behavior, type Spec}

// =============================================================================
// TYPES
// =============================================================================

pub type EmpathyReport {
  EmpathyReport(
    memory_score: Float,
    attention_score: Float,
    expertise_score: Float,
    overall_score: Float,
    overall_load: CognitiveLoad,
    issues: List(CognitiveIssue),
    suggestions: List(String),
  )
}

pub type CognitiveIssue {
  CognitiveIssue(
    dimension: CognitiveDimension,
    description: String,
    severity: IssueSeverity,
    suggestion: String,
  )
}

pub type CognitiveDimension {
  Memory
  Attention
  Expertise
}

pub type CognitiveLoad {
  Low
  Moderate
  High
  Overwhelming
}

pub type IssueSeverity {
  Info
  Warning
  Severe
  Critical
}

// =============================================================================
// CONSTANTS (based on cognitive psychology research)
// =============================================================================

// Miller's Law: working memory capacity
const millers_number_lower = 5

const millers_number_upper = 9

// Technical jargon that assumes expertise
const expert_jargon = [
  // Cryptography
  "jwt", "rs256", "hs256", "hmac", "rsa", "pkcs", "pem", "sha256", "sha512",
  "aes", "encrypt", "decrypt", "cipher", "hash",
  // OAuth/Auth protocols
  "oauth", "oauth2", "oidc", "saml", "client_credentials", "authorization_code",
  "grant_type", "client_assertion", "bearer", "refresh_token",
  // HTTP/API internals
  "idempotent", "idempotency", "etag", "correlation", "x-request-id",
  "content-negotiation", "hateoas",
  // Infrastructure
  "webhook", "websocket", "grpc", "protobuf", "graphql", "subscription",
  // Security
  "csrf", "xss", "sqli", "injection", "sanitize", "cors",
]

// =============================================================================
// MAIN ANALYSIS
// =============================================================================

pub fn analyze_empathy(spec: Spec) -> EmpathyReport {
  let behaviors = get_all_behaviors(spec)

  let memory_issues = analyze_memory_load(behaviors, spec)
  let attention_issues = analyze_attention_demands(behaviors)
  let expertise_issues = analyze_expertise_assumptions(behaviors, spec)

  let all_issues =
    list.concat([memory_issues, attention_issues, expertise_issues])

  // Calculate scores (higher = easier for user, lower cognitive load)
  let memory_score = calculate_dimension_score(memory_issues)
  let attention_score = calculate_dimension_score(attention_issues)
  let expertise_score = calculate_dimension_score(expertise_issues)

  // Weighted average
  let overall_score =
    memory_score *. 0.4 +. attention_score *. 0.3 +. expertise_score *. 0.3

  let overall_load = load_from_score(overall_score)

  let suggestions = generate_suggestions(all_issues)

  EmpathyReport(
    memory_score: memory_score,
    attention_score: attention_score,
    expertise_score: expertise_score,
    overall_score: overall_score,
    overall_load: overall_load,
    issues: all_issues,
    suggestions: suggestions,
  )
}

fn get_all_behaviors(spec: Spec) -> List(Behavior) {
  spec.features
  |> list.flat_map(fn(f) { f.behaviors })
}

fn calculate_dimension_score(issues: List(CognitiveIssue)) -> Float {
  // Start at 100, deduct based on issue severity
  let deductions =
    issues
    |> list.fold(0.0, fn(acc, issue) {
      acc
      +. case issue.severity {
        Critical -> 25.0
        Severe -> 15.0
        Warning -> 8.0
        Info -> 3.0
      }
    })

  float.max(0.0, 100.0 -. deductions)
}

// =============================================================================
// MEMORY LOAD ANALYSIS
// Miller's Law: humans can hold 7 +/- 2 items in working memory
// =============================================================================

fn analyze_memory_load(
  behaviors: List(Behavior),
  spec: Spec,
) -> List(CognitiveIssue) {
  let mut_issues = []

  // Check number of behaviors to track
  let behavior_count = list.length(behaviors)
  let mut_issues = case behavior_count > millers_number_upper {
    True -> [
      CognitiveIssue(
        dimension: Memory,
        description: "API has "
          <> int.to_string(behavior_count)
          <> " behaviors - exceeds working memory capacity (7+/-2)",
        severity: Warning,
        suggestion: "Group related behaviors into clearly labeled sections or features",
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  // Check dependency chain lengths
  let max_chain_length = find_max_dependency_chain(behaviors)
  let mut_issues = case max_chain_length > millers_number_lower {
    True -> [
      CognitiveIssue(
        dimension: Memory,
        description: "Dependency chain of "
          <> int.to_string(max_chain_length)
          <> " steps requires remembering sequence",
        severity: case max_chain_length > millers_number_upper {
          True -> Severe
          False -> Warning
        },
        suggestion: "Break long sequences into checkpoints or provide state management helpers",
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  // Check request body complexity
  let complex_bodies =
    behaviors
    |> list.filter(fn(b) {
      count_json_fields(b.request.body) > millers_number_upper
    })

  let mut_issues = case complex_bodies {
    [] -> mut_issues
    _ -> [
      CognitiveIssue(
        dimension: Memory,
        description: int.to_string(list.length(complex_bodies))
          <> " request(s) have more than "
          <> int.to_string(millers_number_upper)
          <> " fields in body",
        severity: Warning,
        suggestion: "Use sensible defaults for optional fields; document required vs optional clearly",
      ),
      ..mut_issues
    ]
  }

  // Check if there are many features to remember
  let feature_count = list.length(spec.features)
  let mut_issues = case feature_count > millers_number_upper {
    True -> [
      CognitiveIssue(
        dimension: Memory,
        description: "API has "
          <> int.to_string(feature_count)
          <> " features - may overwhelm users",
        severity: Info,
        suggestion: "Consider grouping features into logical categories",
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  mut_issues
}

fn find_max_dependency_chain(behaviors: List(Behavior)) -> Int {
  let behavior_names =
    behaviors
    |> list.map(fn(b) { b.name })

  behaviors
  |> list.map(fn(b) {
    calculate_chain_depth(b.name, behaviors, behavior_names, 0)
  })
  |> list.fold(0, int.max)
}

fn calculate_chain_depth(
  name: String,
  behaviors: List(Behavior),
  all_names: List(String),
  depth: Int,
) -> Int {
  case depth > 20 {
    // Prevent infinite recursion
    True -> depth
    False -> {
      let behavior =
        behaviors
        |> list.find(fn(b) { b.name == name })

      case behavior {
        Error(_) -> depth
        Ok(b) -> {
          case b.requires {
            [] -> depth
            deps -> {
              deps
              |> list.filter(fn(d) { list.contains(all_names, d) })
              |> list.map(fn(dep) {
                calculate_chain_depth(dep, behaviors, all_names, depth + 1)
              })
              |> list.fold(depth, int.max)
            }
          }
        }
      }
    }
  }
}

fn count_json_fields(json_value: json.Json) -> Int {
  // Approximate count based on JSON string representation
  let json_str = json.to_string(json_value)
  // Count colons as proxy for fields (imperfect but reasonable)
  string.to_graphemes(json_str)
  |> list.filter(fn(c) { c == ":" })
  |> list.length
}

// =============================================================================
// ATTENTION DEMANDS ANALYSIS
// Similar items compete for attention and cause confusion
// =============================================================================

fn analyze_attention_demands(behaviors: List(Behavior)) -> List(CognitiveIssue) {
  let mut_issues = []

  // Check for similar-looking paths
  let paths =
    behaviors
    |> list.map(fn(b) { normalize_path(b.request.path) })

  let similar_paths = find_similar_paths(paths)
  let mut_issues = case similar_paths {
    [] -> mut_issues
    pairs -> [
      CognitiveIssue(
        dimension: Attention,
        description: "Found "
          <> int.to_string(list.length(pairs))
          <> " pairs of easily confused paths",
        severity: Warning,
        suggestion: "Use distinct, unambiguous path naming; avoid /user vs /users confusion",
      ),
      ..mut_issues
    ]
  }

  // Check for behaviors with similar names
  let names =
    behaviors
    |> list.map(fn(b) { b.name })

  let similar_names = find_similar_names(names)
  let mut_issues = case similar_names {
    [] -> mut_issues
    pairs -> [
      CognitiveIssue(
        dimension: Attention,
        description: "Found "
          <> int.to_string(list.length(pairs))
          <> " pairs of similar behavior names",
        severity: Info,
        suggestion: "Use descriptive, distinct names that clearly indicate purpose",
      ),
      ..mut_issues
    ]
  }

  // Check for deeply nested response structures
  let deep_responses =
    behaviors
    |> list.filter(fn(b) {
      let example_str = json.to_string(b.response.example)
      // Count nesting depth by counting consecutive braces/brackets
      count_nesting_depth(example_str) > 4
    })

  let mut_issues = case deep_responses {
    [] -> mut_issues
    _ -> [
      CognitiveIssue(
        dimension: Attention,
        description: int.to_string(list.length(deep_responses))
          <> " response(s) have deep nesting (>4 levels)",
        severity: Warning,
        suggestion: "Flatten response structures or provide helper methods for common access patterns",
      ),
      ..mut_issues
    ]
  }

  mut_issues
}

fn normalize_path(path: String) -> String {
  path
  |> string.replace("${", "{")
  |> string.lowercase
}

fn find_similar_paths(paths: List(String)) -> List(#(String, String)) {
  // Find paths that differ only by trailing 's' or by parameterization
  paths
  |> list.combination_pairs
  |> list.filter(fn(pair) {
    let #(a, b) = pair
    paths_are_confusable(a, b)
  })
}

fn paths_are_confusable(a: String, b: String) -> Bool {
  // Remove params for comparison
  let a_base =
    string.replace(a, "{id}", "")
    |> string.replace("{", "")
    |> string.replace("}", "")
  let b_base =
    string.replace(b, "{id}", "")
    |> string.replace("{", "")
    |> string.replace("}", "")

  // Check if one is plural of other
  { a_base <> "s" == b_base || b_base <> "s" == a_base }
  // Or if they're very similar after normalization
  || { string.length(a_base) > 3 && levenshtein_distance(a_base, b_base) <= 2 }
}

fn levenshtein_distance(a: String, b: String) -> Int {
  // Simplified Levenshtein for small strings
  let a_chars = string.to_graphemes(a)
  let b_chars = string.to_graphemes(b)
  let a_len = list.length(a_chars)
  let b_len = list.length(b_chars)

  case a_len, b_len {
    0, _ -> b_len
    _, 0 -> a_len
    _, _ -> {
      // Simple character difference count (approximation)
      let shared =
        a_chars
        |> list.filter(fn(c) { list.contains(b_chars, c) })
        |> list.length
      int.max(a_len, b_len) - shared
    }
  }
}

fn find_similar_names(names: List(String)) -> List(#(String, String)) {
  names
  |> list.combination_pairs
  |> list.filter(fn(pair) {
    let #(a, b) = pair
    let a_lower = string.lowercase(a)
    let b_lower = string.lowercase(b)
    // Check for prefix similarity
    string.starts_with(a_lower, b_lower)
    || string.starts_with(b_lower, a_lower)
    || levenshtein_distance(a_lower, b_lower) <= 2
  })
}

fn count_nesting_depth(json_str: String) -> Int {
  let chars = string.to_graphemes(json_str)
  count_max_depth(chars, 0, 0)
}

fn count_max_depth(chars: List(String), current: Int, max: Int) -> Int {
  case chars {
    [] -> max
    [c, ..rest] -> {
      let new_current = case c {
        "{" | "[" -> current + 1
        "}" | "]" -> int.max(0, current - 1)
        _ -> current
      }
      count_max_depth(rest, new_current, int.max(max, new_current))
    }
  }
}

// =============================================================================
// EXPERTISE ASSUMPTIONS ANALYSIS
// Technical jargon assumes knowledge users may not have
// =============================================================================

fn analyze_expertise_assumptions(
  behaviors: List(Behavior),
  spec: Spec,
) -> List(CognitiveIssue) {
  let mut_issues = []

  // Collect all text from behaviors
  let behavior_text =
    behaviors
    |> list.flat_map(fn(b) {
      [
        b.name,
        b.intent,
        b.notes,
        json.to_string(b.request.body),
        ..list.map(dict.to_list(b.request.headers), fn(h) { h.0 <> " " <> h.1 })
      ]
    })
    |> string.join(" ")
    |> string.lowercase

  // Check for technical jargon
  let jargon_found =
    expert_jargon
    |> list.filter(fn(term) { string.contains(behavior_text, term) })

  let mut_issues = case jargon_found {
    [] -> mut_issues
    terms -> {
      let severity = case list.length(terms) {
        n if n >= 5 -> Severe
        n if n >= 3 -> Warning
        _ -> Info
      }
      [
        CognitiveIssue(
          dimension: Expertise,
          description: "Uses technical jargon that may confuse non-experts: "
            <> string.join(list.take(terms, 5), ", ")
            <> case list.length(terms) > 5 {
            True -> " (+" <> int.to_string(list.length(terms) - 5) <> " more)"
            False -> ""
          },
          severity: severity,
          suggestion: "Add glossary or tooltips explaining technical terms; provide examples for complex concepts",
        ),
        ..mut_issues
      ]
    }
  }

  // Check AI hints for assumed expertise
  let pitfalls = spec.ai_hints.pitfalls
  let mut_issues = case list.length(pitfalls) {
    0 -> [
      CognitiveIssue(
        dimension: Expertise,
        description: "No common pitfalls documented",
        severity: Info,
        suggestion: "Document common mistakes and their solutions to help users avoid expert blind spots",
      ),
      ..mut_issues
    ]
    _ -> mut_issues
  }

  // Check if there are complex auth flows
  let auth_behaviors =
    behaviors
    |> list.filter(fn(b) {
      let name_lower = string.lowercase(b.name)
      let intent_lower = string.lowercase(b.intent)
      string.contains(name_lower, "auth")
      || string.contains(name_lower, "token")
      || string.contains(name_lower, "login")
      || string.contains(intent_lower, "auth")
    })

  let mut_issues = case list.length(auth_behaviors) > 3 {
    True -> [
      CognitiveIssue(
        dimension: Expertise,
        description: "Complex auth flow with "
          <> int.to_string(list.length(auth_behaviors))
          <> " auth-related behaviors",
        severity: Warning,
        suggestion: "Provide a step-by-step auth guide; consider OAuth libraries or SDKs",
      ),
      ..mut_issues
    ]
    False -> mut_issues
  }

  mut_issues
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

pub fn load_from_score(score: Float) -> CognitiveLoad {
  case score {
    s if s >=. 80.0 -> Low
    s if s >=. 50.0 -> Moderate
    s if s >=. 25.0 -> High
    _ -> Overwhelming
  }
}

fn generate_suggestions(issues: List(CognitiveIssue)) -> List(String) {
  issues
  |> list.map(fn(issue) { issue.suggestion })
  |> list.unique
}

// =============================================================================
// STRING CONVERSIONS
// =============================================================================

pub fn cognitive_load_to_string(load: CognitiveLoad) -> String {
  case load {
    Low -> "low"
    Moderate -> "moderate"
    High -> "high"
    Overwhelming -> "overwhelming"
  }
}

pub fn dimension_to_string(dim: CognitiveDimension) -> String {
  case dim {
    Memory -> "memory"
    Attention -> "attention"
    Expertise -> "expertise"
  }
}

pub fn severity_to_string(sev: IssueSeverity) -> String {
  case sev {
    Info -> "info"
    Warning -> "warning"
    Severe -> "severe"
    Critical -> "critical"
  }
}

// =============================================================================
// FORMATTING
// =============================================================================

pub fn format_report(report: EmpathyReport) -> String {
  let header =
    "===================================\n"
    <> "  KIRK Empathy Simulator Report    \n"
    <> "  Cognitive Load Analysis          \n"
    <> "===================================\n\n"

  let overall =
    "Overall Cognitive Load: "
    <> string.uppercase(cognitive_load_to_string(report.overall_load))
    <> " (score: "
    <> int.to_string(float.round(report.overall_score))
    <> "/100)\n\n"

  let dimensions =
    "Dimension Scores:\n"
    <> "  Memory:    "
    <> int.to_string(float.round(report.memory_score))
    <> "/100 - "
    <> describe_score(report.memory_score)
    <> "\n"
    <> "  Attention: "
    <> int.to_string(float.round(report.attention_score))
    <> "/100 - "
    <> describe_score(report.attention_score)
    <> "\n"
    <> "  Expertise: "
    <> int.to_string(float.round(report.expertise_score))
    <> "/100 - "
    <> describe_score(report.expertise_score)
    <> "\n\n"

  let issues_section = format_issues(report.issues)
  let suggestions_section = format_suggestions(report.suggestions)

  header <> overall <> dimensions <> issues_section <> suggestions_section
}

fn describe_score(score: Float) -> String {
  case score {
    s if s >=. 90.0 -> "Excellent"
    s if s >=. 80.0 -> "Good"
    s if s >=. 60.0 -> "Moderate"
    s if s >=. 40.0 -> "Needs Improvement"
    _ -> "Problematic"
  }
}

fn format_issues(issues: List(CognitiveIssue)) -> String {
  case issues {
    [] -> "Issues: None detected - API is cognitively accessible!\n\n"
    _ ->
      "Issues Found ("
      <> int.to_string(list.length(issues))
      <> "):\n"
      <> {
        issues
        |> list.map(fn(issue) {
          "  ["
          <> string.uppercase(dimension_to_string(issue.dimension))
          <> "] "
          <> issue.description
        })
        |> string.join("\n")
      }
      <> "\n\n"
  }
}

fn format_suggestions(suggestions: List(String)) -> String {
  case suggestions {
    [] -> ""
    _ ->
      "Suggestions:\n"
      <> {
        suggestions
        |> list.index_map(fn(s, i) { "  " <> int.to_string(i + 1) <> ". " <> s })
        |> string.join("\n")
      }
      <> "\n"
  }
}

// =============================================================================
// KIRK HEALTH FORMAT INTEGRATION
// =============================================================================

/// Convert EmpathyReport to KIRKHealth-compatible format
/// Returns list of formatted issue strings
pub fn empathy_to_kirk_health_format(report: EmpathyReport) -> List(String) {
  report.issues
  |> list.map(fn(issue) {
    "["
    <> string.uppercase(dimension_to_string(issue.dimension))
    <> ":"
    <> string.uppercase(severity_to_string(issue.severity))
    <> "] "
    <> issue.description
    <> " - "
    <> issue.suggestion
  })
}
