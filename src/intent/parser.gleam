/// Parser for Intent specs from JSON (exported from CUE)
import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/json.{type Json}
import gleam/list
import gleam/result
import intent/planning_types.{
  type DimensionScore, type FeatureShape, type KIRKHealth, type MVPSlice,
  type Plan, type ReadyReport, type ShapeSection, type SpecSection, Critical,
  DimensionScore, FeatureShape, High, KIRKHealth, Low, MVPSlice, Medium, Plan,
  ReadyReport, ShapeSection, SpecSection,
}
import intent/types.{
  type AIHints, type AntiPattern, type Behavior, type Check, type Config,
  type EntityHint, type Feature, type ImplementationHints, type Method,
  type Request, type Response, type Rule, type RuleCheck, type SecurityHints,
  type Spec, type When, AIHints, AntiPattern, Behavior, Check, Config, Delete,
  EntityHint, Feature, Get, Head, ImplementationHints, Options, Patch, Post, Put,
  Request, Response, Rule, RuleCheck, SecurityHints, Spec, When,
}
import intent/vision_types.{
  type Scenario, type VisionSection, Scenario, VisionSection,
}

/// Parse a spec from a JSON value
/// All fields are required - no backwards compatibility defaults
pub fn parse_spec(data: Dynamic) -> Result(Spec, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use audience <- result.try(dynamic.field("audience", dynamic.string)(data))
  use version <- result.try(dynamic.field("version", dynamic.string)(data))
  use success_criteria <- result.try(dynamic.field(
    "success_criteria",
    dynamic.list(dynamic.string),
  )(data))
  use config <- result.try(dynamic.field("config", parse_config)(data))
  use features <- result.try(dynamic.field(
    "features",
    dynamic.list(parse_feature),
  )(data))
  use rules <- result.try(dynamic.field("rules", dynamic.list(parse_rule))(data))
  use anti_patterns <- result.try(dynamic.field(
    "anti_patterns",
    dynamic.list(parse_anti_pattern),
  )(data))
  // ai_hints is optional in schema - provide default if missing
  let ai_hints =
    dynamic.field("ai_hints", parse_ai_hints)(data)
    |> result.unwrap(default_ai_hints())

  Ok(Spec(
    name: name,
    description: description,
    audience: audience,
    version: version,
    success_criteria: success_criteria,
    config: config,
    features: features,
    rules: rules,
    anti_patterns: anti_patterns,
    ai_hints: ai_hints,
  ))
}

fn parse_config(data: Dynamic) -> Result(Config, List(DecodeError)) {
  use base_url <- result.try(dynamic.field("base_url", dynamic.string)(data))
  use timeout_ms <- result.try(dynamic.field("timeout_ms", dynamic.int)(data))
  use headers <- result.try(dynamic.field("headers", parse_string_dict)(data))
  Ok(Config(base_url, timeout_ms, headers, allow_localhost: False))
}

fn parse_string_dict(
  data: Dynamic,
) -> Result(Dict(String, String), List(DecodeError)) {
  dynamic.dict(dynamic.string, dynamic.string)(data)
}

fn parse_feature(data: Dynamic) -> Result(Feature, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use behaviors <- result.try(dynamic.field(
    "behaviors",
    dynamic.list(parse_behavior),
  )(data))
  Ok(Feature(name, description, behaviors))
}

fn parse_behavior(data: Dynamic) -> Result(Behavior, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use intent <- result.try(dynamic.field("intent", dynamic.string)(data))
  use notes <- result.try(dynamic.field("notes", dynamic.string)(data))
  use requires <- result.try(dynamic.field(
    "requires",
    dynamic.list(dynamic.string),
  )(data))
  use tags <- result.try(dynamic.field("tags", dynamic.list(dynamic.string))(
    data,
  ))
  use request <- result.try(dynamic.field("request", parse_request)(data))
  use response <- result.try(dynamic.field("response", parse_response)(data))
  use captures <- result.try(dynamic.field("captures", parse_string_dict)(data))
  Ok(Behavior(
    name: name,
    intent: intent,
    notes: notes,
    requires: requires,
    tags: tags,
    request: request,
    response: response,
    captures: captures,
  ))
}

fn parse_method(data: Dynamic) -> Result(Method, List(DecodeError)) {
  data
  |> dynamic.string
  |> result.then(fn(s) {
    case s {
      "GET" -> Ok(Get)
      "POST" -> Ok(Post)
      "PUT" -> Ok(Put)
      "PATCH" -> Ok(Patch)
      "DELETE" -> Ok(Delete)
      "HEAD" -> Ok(Head)
      "OPTIONS" -> Ok(Options)
      _ ->
        Error([
          dynamic.DecodeError(expected: "HTTP method", found: s, path: []),
        ])
    }
  })
}

fn parse_request(data: Dynamic) -> Result(Request, List(DecodeError)) {
  use method <- result.try(dynamic.field("method", parse_method)(data))
  use path <- result.try(dynamic.field("path", dynamic.string)(data))
  use headers <- result.try(dynamic.field("headers", parse_string_dict)(data))
  use query <- result.try(dynamic.field("query", parse_json_dict)(data))
  use body <- result.try(dynamic.field("body", parse_json_value)(data))
  Ok(Request(method, path, headers, query, body))
}

fn parse_json_dict(
  data: Dynamic,
) -> Result(Dict(String, Json), List(DecodeError)) {
  data
  |> dynamic.dict(dynamic.string, dynamic.dynamic)
  |> result.map(fn(d) { dict.map_values(d, fn(_, v) { dynamic_to_json(v) }) })
}

fn parse_json_value(data: Dynamic) -> Result(Json, List(DecodeError)) {
  Ok(dynamic_to_json(data))
}

/// Convert a Dynamic value to Json
pub fn dynamic_to_json(data: Dynamic) -> Json {
  case dynamic.classify(data) {
    "Nil" -> json.null()
    "Bool" ->
      case dynamic.bool(data) {
        Ok(b) -> json.bool(b)
        Error(_) -> json.null()
      }
    "Int" ->
      case dynamic.int(data) {
        Ok(i) -> json.int(i)
        Error(_) -> json.null()
      }
    "Float" ->
      case dynamic.float(data) {
        Ok(f) -> json.float(f)
        Error(_) -> json.null()
      }
    "String" | "BitArray" ->
      case dynamic.string(data) {
        Ok(s) -> json.string(s)
        Error(_) -> json.null()
      }
    "List" | "Tuple" ->
      case dynamic.list(dynamic.dynamic)(data) {
        Ok(items) -> json.array(items, dynamic_to_json)
        Error(_) -> json.null()
      }
    "Dict" | "Map" ->
      case dynamic.dict(dynamic.string, dynamic.dynamic)(data) {
        Ok(d) ->
          d
          |> dict.to_list
          |> list.map(fn(pair) { #(pair.0, dynamic_to_json(pair.1)) })
          |> json.object
        Error(_) -> json.null()
      }
    _ -> json.null()
  }
}

fn parse_response(data: Dynamic) -> Result(Response, List(DecodeError)) {
  use status <- result.try(dynamic.field("status", dynamic.int)(data))
  use example <- result.try(dynamic.field("example", parse_json_value)(data))
  use checks <- result.try(dynamic.field("checks", parse_checks)(data))
  // Headers are optional - use empty dict if not present
  let headers =
    dynamic.field("headers", parse_string_dict)(data)
    |> result.unwrap(dict.new())
  Ok(Response(status, example, checks, headers))
}

fn parse_checks(data: Dynamic) -> Result(Dict(String, Check), List(DecodeError)) {
  data
  |> dynamic.dict(dynamic.string, parse_check)
}

fn parse_check(data: Dynamic) -> Result(Check, List(DecodeError)) {
  use rule <- result.try(dynamic.field("rule", dynamic.string)(data))
  use why <- result.try(dynamic.field("why", dynamic.string)(data))
  Ok(Check(rule, why))
}

fn parse_rule(data: Dynamic) -> Result(Rule, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use when <- result.try(dynamic.optional_field("when", parse_when)(data))
  use check <- result.try(dynamic.field("check", parse_rule_check)(data))
  use example <- result.try(dynamic.optional_field("example", parse_json_value)(
    data,
  ))
  Ok(Rule(name, description, when, check, example))
}

fn parse_when(data: Dynamic) -> Result(When, List(DecodeError)) {
  use status <- result.try(dynamic.optional_field("status", dynamic.string)(
    data,
  ))
  use method <- result.try(dynamic.optional_field("method", parse_method)(data))
  use path <- result.try(dynamic.optional_field("path", dynamic.string)(data))
  Ok(When(status, method, path))
}

fn parse_rule_check(data: Dynamic) -> Result(RuleCheck, List(DecodeError)) {
  let body_must_not_contain =
    dynamic.field("body_must_not_contain", dynamic.list(dynamic.string))(data)
    |> result.unwrap([])
  let body_must_contain =
    dynamic.field("body_must_contain", dynamic.list(dynamic.string))(data)
    |> result.unwrap([])
  let fields_must_exist =
    dynamic.field("fields_must_exist", dynamic.list(dynamic.string))(data)
    |> result.unwrap([])
  let fields_must_not_exist =
    dynamic.field("fields_must_not_exist", dynamic.list(dynamic.string))(data)
    |> result.unwrap([])
  let header_must_exist =
    dynamic.field("header_must_exist", dynamic.string)(data)
    |> result.unwrap("")
  let header_must_not_exist =
    dynamic.field("header_must_not_exist", dynamic.string)(data)
    |> result.unwrap("")
  Ok(RuleCheck(
    body_must_not_contain,
    body_must_contain,
    fields_must_exist,
    fields_must_not_exist,
    header_must_exist,
    header_must_not_exist,
  ))
}

fn parse_anti_pattern(data: Dynamic) -> Result(AntiPattern, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use bad_example <- result.try(dynamic.field("bad_example", parse_json_value)(
    data,
  ))
  use good_example <- result.try(dynamic.field("good_example", parse_json_value)(
    data,
  ))
  use why <- result.try(dynamic.field("why", dynamic.string)(data))

  Ok(AntiPattern(
    name: name,
    description: description,
    bad_example: bad_example,
    good_example: good_example,
    why: why,
  ))
}

/// Default empty AI hints when not provided in spec
fn default_ai_hints() -> AIHints {
  AIHints(
    implementation: ImplementationHints(suggested_stack: []),
    entities: dict.new(),
    security: SecurityHints(
      password_hashing: "",
      jwt_algorithm: "",
      jwt_expiry: "",
      rate_limiting: "",
    ),
    pitfalls: [],
  )
}

fn parse_ai_hints(data: Dynamic) -> Result(AIHints, List(DecodeError)) {
  use implementation <- result.try(dynamic.field(
    "implementation",
    parse_implementation_hints,
  )(data))
  use entities <- result.try(dynamic.field("entities", parse_entities)(data))
  use security <- result.try(dynamic.field("security", parse_security_hints)(
    data,
  ))
  use pitfalls <- result.try(dynamic.field(
    "pitfalls",
    dynamic.list(dynamic.string),
  )(data))
  Ok(AIHints(implementation, entities, security, pitfalls))
}

fn parse_implementation_hints(
  data: Dynamic,
) -> Result(ImplementationHints, List(DecodeError)) {
  use suggested_stack <- result.try(dynamic.field(
    "suggested_stack",
    dynamic.list(dynamic.string),
  )(data))
  Ok(ImplementationHints(suggested_stack))
}

fn parse_entities(
  data: Dynamic,
) -> Result(Dict(String, EntityHint), List(DecodeError)) {
  dynamic.dict(dynamic.string, parse_entity_hint)(data)
}

fn parse_entity_hint(data: Dynamic) -> Result(EntityHint, List(DecodeError)) {
  use fields <- result.try(dynamic.field("fields", parse_string_dict)(data))
  Ok(EntityHint(fields))
}

fn parse_security_hints(
  data: Dynamic,
) -> Result(SecurityHints, List(DecodeError)) {
  use password_hashing <- result.try(dynamic.field(
    "password_hashing",
    dynamic.string,
  )(data))
  use jwt_algorithm <- result.try(dynamic.field("jwt_algorithm", dynamic.string)(
    data,
  ))
  use jwt_expiry <- result.try(dynamic.field("jwt_expiry", dynamic.string)(data))
  use rate_limiting <- result.try(dynamic.field("rate_limiting", dynamic.string)(
    data,
  ))
  Ok(SecurityHints(password_hashing, jwt_algorithm, jwt_expiry, rate_limiting))
}

// ============================================================================
// Plan Parser Functions
// ============================================================================

/// Parse a Plan from a JSON value
/// Vision and Shape are required, Spec and Ready are optional
pub fn parse_plan(data: Dynamic) -> Result(Plan, List(DecodeError)) {
  use id <- result.try(dynamic.field("id", dynamic.string)(data))
  use created_at <- result.try(dynamic.field("created_at", dynamic.string)(data))
  use updated_at <- result.try(dynamic.field("updated_at", dynamic.string)(data))
  use vision <- result.try(dynamic.field("vision", parse_vision_section)(data))
  use shape <- result.try(dynamic.field("shape", parse_shape_section)(data))
  use spec <- result.try(dynamic.optional_field("spec", parse_spec_section)(
    data,
  ))
  use ready <- result.try(dynamic.optional_field("ready", parse_ready_report)(
    data,
  ))

  Ok(Plan(
    id: id,
    created_at: created_at,
    updated_at: updated_at,
    vision: vision,
    shape: shape,
    spec: spec,
    ready: ready,
  ))
}

fn parse_vision_section(
  data: Dynamic,
) -> Result(VisionSection, List(DecodeError)) {
  use press_release <- result.try(dynamic.field("press_release", dynamic.string)(
    data,
  ))
  use persona <- result.try(dynamic.field("persona", dynamic.string)(data))
  use non_personas <- result.try(dynamic.field(
    "non_personas",
    dynamic.list(dynamic.string),
  )(data))
  use north_star <- result.try(dynamic.field("north_star", dynamic.string)(data))
  use scenarios <- result.try(dynamic.field(
    "scenarios",
    dynamic.list(parse_scenario),
  )(data))
  use replaces <- result.try(dynamic.optional_field("replaces", dynamic.string)(
    data,
  ))
  use vorp <- result.try(dynamic.field("vorp", dynamic.string)(data))
  use out_of_scope <- result.try(dynamic.field(
    "out_of_scope",
    dynamic.list(dynamic.string),
  )(data))

  Ok(VisionSection(
    press_release: press_release,
    persona: persona,
    non_personas: non_personas,
    north_star: north_star,
    scenarios: scenarios,
    replaces: replaces,
    vorp: vorp,
    out_of_scope: out_of_scope,
  ))
}

fn parse_scenario(data: Dynamic) -> Result(Scenario, List(DecodeError)) {
  use character <- result.try(dynamic.field("character", dynamic.string)(data))
  use persona <- result.try(dynamic.field("persona", dynamic.string)(data))
  use motivation <- result.try(dynamic.field("motivation", dynamic.string)(data))
  use simulation <- result.try(dynamic.field("simulation", dynamic.string)(data))
  use outcome <- result.try(dynamic.field("outcome", dynamic.string)(data))

  Ok(Scenario(
    character: character,
    persona: persona,
    motivation: motivation,
    simulation: simulation,
    outcome: outcome,
  ))
}

fn parse_shape_section(data: Dynamic) -> Result(ShapeSection, List(DecodeError)) {
  use features <- result.try(dynamic.field(
    "features",
    dynamic.list(parse_feature_shape),
  )(data))
  use critical_path <- result.try(dynamic.field(
    "critical_path",
    dynamic.list(dynamic.string),
  )(data))
  use mvp_slice <- result.try(dynamic.field("mvp_slice", parse_mvp_slice)(data))
  use post_mvp <- result.try(dynamic.field(
    "post_mvp",
    dynamic.list(dynamic.string),
  )(data))
  use validation_moment <- result.try(dynamic.field(
    "validation_moment",
    dynamic.string,
  )(data))

  Ok(ShapeSection(
    features: features,
    critical_path: critical_path,
    mvp_slice: mvp_slice,
    post_mvp: post_mvp,
    validation_moment: validation_moment,
  ))
}

fn parse_feature_shape(data: Dynamic) -> Result(FeatureShape, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))

  Ok(FeatureShape(name: name, description: description))
}

fn parse_mvp_slice(data: Dynamic) -> Result(MVPSlice, List(DecodeError)) {
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use features <- result.try(dynamic.field(
    "features",
    dynamic.list(dynamic.string),
  )(data))
  use shortcuts <- result.try(dynamic.field(
    "shortcuts",
    dynamic.list(dynamic.string),
  )(data))

  Ok(MVPSlice(
    description: description,
    features: features,
    shortcuts: shortcuts,
  ))
}

fn parse_spec_section(data: Dynamic) -> Result(SpecSection, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use rounds_complete <- result.try(dynamic.field(
    "rounds_complete",
    dynamic.int,
  )(data))
  use kirk_health <- result.try(dynamic.field("kirk_health", parse_kirk_health)(
    data,
  ))

  Ok(SpecSection(
    name: name,
    description: description,
    rounds_complete: rounds_complete,
    kirk_health: kirk_health,
  ))
}

fn parse_kirk_health(data: Dynamic) -> Result(KIRKHealth, List(DecodeError)) {
  use coverage_score <- result.try(dynamic.field(
    "coverage_score",
    dynamic.float,
  )(data))
  use quality_score <- result.try(dynamic.field("quality_score", dynamic.float)(
    data,
  ))
  use gaps <- result.try(dynamic.field("gaps", dynamic.list(dynamic.string))(
    data,
  ))
  use inversions <- result.try(dynamic.field(
    "inversions",
    dynamic.list(dynamic.string),
  )(data))
  use effects <- result.try(dynamic.field(
    "effects",
    dynamic.list(dynamic.string),
  )(data))

  Ok(KIRKHealth(
    coverage_score: coverage_score,
    quality_score: quality_score,
    gaps: gaps,
    inversions: inversions,
    effects: effects,
  ))
}

fn parse_ready_report(data: Dynamic) -> Result(ReadyReport, List(DecodeError)) {
  use replacement <- result.try(dynamic.field(
    "replacement",
    parse_dimension_score,
  )(data))
  use empathy <- result.try(dynamic.field("empathy", parse_dimension_score)(
    data,
  ))
  use actionable <- result.try(dynamic.field(
    "actionable",
    parse_dimension_score,
  )(data))
  use discoverable <- result.try(dynamic.field(
    "discoverable",
    parse_dimension_score,
  )(data))
  use yet_complete <- result.try(dynamic.field(
    "yet_complete",
    parse_dimension_score,
  )(data))
  use overall_readiness <- result.try(dynamic.field(
    "overall_readiness",
    dynamic.int,
  )(data))
  use blockers <- result.try(dynamic.field(
    "blockers",
    dynamic.list(parse_blocker),
  )(data))
  use recommendations <- result.try(dynamic.field(
    "recommendations",
    dynamic.list(parse_recommendation),
  )(data))

  Ok(ReadyReport(
    replacement: replacement,
    empathy: empathy,
    actionable: actionable,
    discoverable: discoverable,
    yet_complete: yet_complete,
    overall_readiness: overall_readiness,
    blockers: blockers,
    recommendations: recommendations,
  ))
}

fn parse_dimension_score(
  data: Dynamic,
) -> Result(DimensionScore, List(DecodeError)) {
  use score <- result.try(dynamic.field("score", dynamic.int)(data))
  use reasoning <- result.try(dynamic.field("reasoning", dynamic.string)(data))
  use issues <- result.try(dynamic.field("issues", dynamic.list(dynamic.string))(
    data,
  ))

  Ok(DimensionScore(score: score, reasoning: reasoning, issues: issues))
}

fn parse_blocker(
  data: Dynamic,
) -> Result(planning_types.Blocker, List(DecodeError)) {
  use severity <- result.try(dynamic.field("severity", parse_blocker_severity)(
    data,
  ))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use affected_areas <- result.try(dynamic.field(
    "affected_areas",
    dynamic.list(dynamic.string),
  )(data))

  Ok(planning_types.Blocker(
    severity: severity,
    description: description,
    affected_areas: affected_areas,
  ))
}

fn parse_blocker_severity(
  data: Dynamic,
) -> Result(planning_types.BlockerSeverity, List(DecodeError)) {
  data
  |> dynamic.string
  |> result.then(fn(s) {
    case s {
      "critical" -> Ok(Critical)
      "high" -> Ok(High)
      "medium" -> Ok(Medium)
      "low" -> Ok(Low)
      _ ->
        Error([
          dynamic.DecodeError(
            expected: "blocker severity (critical|high|medium|low)",
            found: s,
            path: [],
          ),
        ])
    }
  })
}

fn parse_recommendation(
  data: Dynamic,
) -> Result(planning_types.Recommendation, List(DecodeError)) {
  use priority <- result.try(dynamic.field("priority", dynamic.int)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use rationale <- result.try(dynamic.field("rationale", dynamic.string)(data))

  Ok(planning_types.Recommendation(
    priority: priority,
    description: description,
    rationale: rationale,
  ))
}
