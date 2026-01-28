/// Core types for Intent specification
/// These types mirror the CUE schema definitions
import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/option.{type Option}
import intent/planning_types.{type Inversions, type PreMortem, type QualityScore}

/// The main specification type - ai_hints is optional
/// KIRK extensions (inversions, pre_mortem, quality_score) are also optional
pub type Spec {
  Spec(
    name: String,
    description: String,
    audience: String,
    version: String,
    success_criteria: List(String),
    config: Config,
    features: List(Feature),
    rules: List(Rule),
    anti_patterns: List(AntiPattern),
    ai_hints: Option(AIHints),
    inversions: Option(Inversions),
    pre_mortem: Option(PreMortem),
    quality_score: Option(QualityScore),
  )
}

/// Configuration for spec execution
pub type Config {
  Config(
    base_url: String,
    timeout_ms: Int,
    headers: Dict(String, String),
    allow_localhost: Option(Bool),
  )
}

/// A feature groups related behaviors
pub type Feature {
  Feature(name: String, description: String, behaviors: List(Behavior))
}

/// A single behavior/test case
pub type Behavior {
  Behavior(
    name: String,
    intent: String,
    notes: String,
    requires: List(String),
    tags: List(String),
    request: Request,
    response: Response,
    captures: Dict(String, String),
  )
}

/// HTTP methods
pub type Method {
  Get
  Post
  Put
  Patch
  Delete
  Head
  Options
}

/// Convert method to string
pub fn method_to_string(method: Method) -> String {
  case method {
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Patch -> "PATCH"
    Delete -> "DELETE"
    Head -> "HEAD"
    Options -> "OPTIONS"
  }
}

/// Parse method from string
pub fn method_from_string(s: String) -> Result(Method, String) {
  case s {
    "GET" -> Ok(Get)
    "POST" -> Ok(Post)
    "PUT" -> Ok(Put)
    "PATCH" -> Ok(Patch)
    "DELETE" -> Ok(Delete)
    "HEAD" -> Ok(Head)
    "OPTIONS" -> Ok(Options)
    _ -> Error("Unknown HTTP method: " <> s)
  }
}

/// HTTP request definition - all fields required
pub type Request {
  Request(
    method: Method,
    path: String,
    headers: Dict(String, String),
    query: Dict(String, Json),
    body: Json,
  )
}

/// Expected response
pub type Response {
  Response(status: Int, example: Json, checks: Dict(String, Check))
}

/// A single check with rule and explanation
pub type Check {
  Check(rule: String, why: String)
}

/// Global rules that apply to all responses
/// The 'when' and 'example' fields are optional
pub type Rule {
  Rule(
    name: String,
    description: String,
    when: Option(When),
    check: RuleCheck,
    example: Option(Json),
  )
}

/// Conditions for when a rule applies
/// All fields are optional - missing fields mean "any value matches"
pub type When {
  When(status: Option(String), method: Option(Method), path: Option(String))
}

/// Rule checks for global rules - all fields optional
pub type RuleCheck {
  RuleCheck(
    body_must_not_contain: Option(List(String)),
    body_must_contain: Option(List(String)),
    fields_must_exist: Option(List(String)),
    fields_must_not_exist: Option(List(String)),
    header_must_exist: Option(String),
    header_must_not_exist: Option(String),
  )
}

/// Anti-patterns with good/bad examples
pub type AntiPattern {
  AntiPattern(
    name: String,
    description: String,
    bad_example: Json,
    good_example: Json,
    why: String,
  )
}

/// AI implementation hints - all fields required
pub type AIHints {
  AIHints(
    implementation: ImplementationHints,
    entities: Dict(String, EntityHint),
    security: SecurityHints,
    pitfalls: List(String),
  )
}

pub type ImplementationHints {
  ImplementationHints(suggested_stack: List(String))
}

pub type EntityHint {
  EntityHint(fields: Dict(String, String))
}

pub type SecurityHints {
  SecurityHints(
    password_hashing: String,
    jwt_algorithm: String,
    jwt_expiry: String,
    rate_limiting: String,
  )
}
