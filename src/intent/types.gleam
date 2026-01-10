/// Core types for the Intent specification
/// These types mirror the CUE schema definitions
import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/option.{type Option}

/// The main specification type - all fields required
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
    ai_hints: AIHints,
  )
}

/// Configuration for spec execution
pub type Config {
  Config(base_url: String, timeout_ms: Int, headers: Dict(String, String))
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

/// Expected response - all fields required
pub type Response {
  Response(
    status: Int,
    example: Json,
    checks: Dict(String, Check),
    headers: Dict(String, String),
  )
}

/// A single check with rule and explanation
pub type Check {
  Check(rule: String, why: String)
}

/// Global rules that apply to all responses - all fields required
pub type Rule {
  Rule(
    name: String,
    description: String,
    when: When,
    check: RuleCheck,
    example: Json,
  )
}

/// Conditions for when a rule applies
/// intent-cli-5zd: Fields are optional (None = wildcard, matches everything)
pub type When {
  When(
    status: option.Option(String),
    method: option.Option(Method),
    path: option.Option(String),
  )
}

/// Rule checks for global rules - all fields required
pub type RuleCheck {
  RuleCheck(
    body_must_not_contain: List(String),
    body_must_contain: List(String),
    fields_must_exist: List(String),
    fields_must_not_exist: List(String),
    header_must_exist: String,
    header_must_not_exist: String,
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
    codebase: Option(CodebaseContext),
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

// =============================================================================
// Codebase Context Types - For AI to understand existing patterns
// =============================================================================

/// Codebase context for AI to understand existing project patterns
pub type CodebaseContext {
  CodebaseContext(
    patterns: Option(CodebasePatterns),
    stack: Option(CodebaseStack),
    entry_points: List(EntryPoint),
    boundaries: List(Boundary),
  )
}

/// Common patterns in the codebase
pub type CodebasePatterns {
  CodebasePatterns(
    error_handling: String,
    auth_middleware: String,
    validation: String,
    testing: String,
  )
}

/// Technology stack
pub type CodebaseStack {
  CodebaseStack(
    language: String,
    framework: String,
    database: String,
    orm: String,
    testing: String,
  )
}

/// Entry point into the codebase
pub type EntryPoint {
  EntryPoint(name: String, path: String, description: String)
}

/// Architectural boundary
pub type Boundary {
  Boundary(name: String, description: String, modules: List(String))
}

// =============================================================================
// Light Spec Types - Minimal spec for simple tasks
// =============================================================================

/// Simplified behavior for light specs (HTTP/API focused)
/// No requires, tags, captures, or notes - just the essentials
pub type LightBehavior {
  LightBehavior(
    name: String,
    intent: String,
    request: LightRequest,
    response: LightResponse,
  )
}

/// Simplified request for light behaviors
pub type LightRequest {
  LightRequest(method: Method, path: String, body: Json)
}

/// Simplified response for light behaviors
pub type LightResponse {
  LightResponse(status: Int, checks: Dict(String, Check))
}

/// Minimal spec for simple tasks
/// No config block required, no rules required
pub type LightSpec {
  LightSpec(
    name: String,
    description: String,
    behaviors: List(LightBehavior),
    anti_patterns: List(AntiPattern),
    ai_hints: Option(AIHints),
  )
}
