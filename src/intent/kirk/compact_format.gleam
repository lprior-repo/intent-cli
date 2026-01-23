/// KIRK Compact Intent Notation (CIN)
/// Token-efficient format for AI prompts
/// Stub implementation - functions return placeholder data
import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import intent/types.{type Spec}

/// Compact representation of a spec
pub type CompactSpec {
  CompactSpec(
    name: String,
    version: String,
    description: String,
    features: List(CompactFeature),
    rules: List(CompactRule),
    anti_patterns: List(CompactAntiPattern),
  )
}

pub type CompactFeature {
  CompactFeature(name: String, behaviors: List(CompactBehavior))
}

pub type CompactBehavior {
  CompactBehavior(
    name: String,
    intent: String,
    requires: List(String),
    request: String,
    status: Int,
    checks: List(CompactCheck),
    captures: List(String),
  )
}

pub type CompactCheck {
  CompactCheck(field: String, rule: String, why: String)
}

pub type CompactRule {
  CompactRule(
    name: String,
    when_condition: String,
    must_not_contain: List(String),
    must_contain: List(String),
  )
}

pub type CompactAntiPattern {
  CompactAntiPattern(name: String, bad_example: String, good_example: String)
}

pub type TokenComparison {
  TokenComparison(full_tokens: Int, compact_tokens: Int, savings_percent: Float)
}

/// Convert a spec to compact format
pub fn spec_to_compact(spec: Spec) -> CompactSpec {
  CompactSpec(
    name: spec.name,
    version: spec.version,
    description: spec.description,
    features: list.map(spec.features, fn(f) {
      CompactFeature(
        name: f.name,
        behaviors: list.map(f.behaviors, fn(b) {
          CompactBehavior(
            name: b.name,
            intent: b.intent,
            requires: b.requires,
            request: types.method_to_string(b.request.method)
              <> " "
              <> b.request.path,
            status: b.response.status,
            checks: b.response.checks
              |> dict.to_list
              |> list.map(fn(pair) {
                let #(field, check) = pair
                CompactCheck(field: field, rule: check.rule, why: check.why)
              }),
            captures: [],
          )
        }),
      )
    }),
    rules: [],
    anti_patterns: [],
  )
}

/// Format compact spec as string
pub fn format_compact(compact: CompactSpec) -> String {
  let header =
    "SPEC "
    <> compact.name
    <> " "
    <> compact.version
    <> "\n"
    <> compact.description
    <> "\n\n"

  let features =
    compact.features
    |> list.map(format_feature)
    |> string.join("\n\n")

  header <> features
}

fn format_feature(feature: CompactFeature) -> String {
  let header = "F \"" <> feature.name <> "\"\n"
  let behaviors =
    feature.behaviors
    |> list.map(format_behavior)
    |> string.join("\n")
  header <> behaviors
}

fn format_behavior(behavior: CompactBehavior) -> String {
  "B "
  <> behavior.name
  <> " \""
  <> behavior.intent
  <> "\"\n"
  <> "  "
  <> behavior.request
  <> "\n"
  <> "  "
  <> int.to_string(behavior.status)
}

/// Compare token usage between full and compact formats
pub fn compare_token_usage(spec: Spec) -> TokenComparison {
  let compact = spec_to_compact(spec)
  let compact_text = format_compact(compact)
  let compact_tokens = estimate_tokens(compact_text)
  // Estimate full format at ~2x compact
  let full_tokens = compact_tokens * 2
  let savings = case full_tokens {
    0 -> 0.0
    _ -> int.to_float(full_tokens - compact_tokens) /. int.to_float(full_tokens)
  }
  TokenComparison(
    full_tokens: full_tokens,
    compact_tokens: compact_tokens,
    savings_percent: savings *. 100.0,
  )
}

/// Estimate token count (rough approximation: ~4 chars per token)
pub fn estimate_tokens(text: String) -> Int {
  string.length(text) / 4
}

/// Convert spec to prototext format
pub fn spec_to_prototext(spec: Spec) -> String {
  "spec {\n"
  <> "  name: \""
  <> spec.name
  <> "\"\n"
  <> "  version: \""
  <> spec.version
  <> "\"\n"
  <> "  description: \""
  <> spec.description
  <> "\"\n"
  <> "}\n"
}
