/// Spec diff tracking - compares two specs and reports changes
/// 
/// FUNCTIONAL CORE Architecture
/// =============================
/// This module is purely functional with no I/O operations.
/// All functions are pure comparisons and transformations.
///
/// Features:
/// - Detect added/removed/modified behaviors
/// - Detect added/removed/modified rules
/// - Detect added/removed features
/// - Detect config changes
/// - Detect anti-pattern changes
/// - Human-readable and JSON output formats
import gleam/dict
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/types.{
  type AntiPattern, type Behavior, type Check, type Config, type Feature,
  type Request, type Response, type Rule, type RuleCheck, type Spec, type When,
}

// ============================================================================
// Diff Types
// ============================================================================

/// Overall spec diff result
pub type SpecDiff {
  SpecDiff(
    name_changed: Option(StringChange),
    description_changed: Option(StringChange),
    version_changed: Option(StringChange),
    config_changes: List(ConfigChange),
    feature_changes: List(FeatureChange),
    behavior_changes: List(BehaviorChange),
    rule_changes: List(RuleChange),
    anti_pattern_changes: List(AntiPatternChange),
    success_criteria_changes: ListChange,
    has_changes: Bool,
  )
}

/// A simple string field change
pub type StringChange {
  StringChange(old: String, new: String)
}

/// Changes to a list of strings
pub type ListChange {
  ListChange(added: List(String), removed: List(String))
}

/// Configuration changes
pub type ConfigChange {
  BaseUrlChanged(old: String, new: String)
  TimeoutChanged(old: Int, new: Int)
  AllowLocalhostChanged(old: Bool, new: Bool)
  HeadersChanged(
    added: List(String),
    removed: List(String),
    modified: List(String),
  )
}

/// Feature-level changes
pub type FeatureChange {
  FeatureAdded(name: String, behavior_count: Int)
  FeatureRemoved(name: String, behavior_count: Int)
  FeatureModified(
    name: String,
    description_changed: Option(StringChange),
    behavior_changes: List(BehaviorChange),
  )
}

/// Behavior changes
pub type BehaviorChange {
  BehaviorAdded(feature: String, name: String, intent: String)
  BehaviorRemoved(feature: String, name: String, intent: String)
  BehaviorModified(
    feature: String,
    name: String,
    modifications: List(BehaviorModification),
  )
}

/// Specific modifications to a behavior
pub type BehaviorModification {
  IntentChanged(old: String, new: String)
  NotesChanged(old: String, new: String)
  RequestChanged(changes: List(RequestChange))
  ResponseChanged(changes: List(ResponseChange))
  RequiresChanged(added: List(String), removed: List(String))
  TagsChanged(added: List(String), removed: List(String))
  CapturesChanged(added: List(String), removed: List(String))
}

/// Request changes
pub type RequestChange {
  MethodChanged(old: String, new: String)
  PathChanged(old: String, new: String)
  RequestHeadersChanged(added: List(String), removed: List(String))
  QueryChanged(added: List(String), removed: List(String))
  BodyChanged
}

/// Response changes
pub type ResponseChange {
  StatusChanged(old: Int, new: Int)
  ChecksAdded(checks: List(String))
  ChecksRemoved(checks: List(String))
  ChecksModified(checks: List(String))
  ResponseHeadersChanged(added: List(String), removed: List(String))
  ExampleChanged
}

/// Rule changes
pub type RuleChange {
  RuleAdded(name: String, description: String)
  RuleRemoved(name: String, description: String)
  RuleModified(name: String, modifications: List(RuleModification))
}

/// Specific modifications to a rule
pub type RuleModification {
  RuleDescriptionChanged(old: String, new: String)
  RuleWhenChanged
  RuleCheckChanged(field: String)
}

/// Anti-pattern changes
pub type AntiPatternChange {
  AntiPatternAdded(name: String, description: String)
  AntiPatternRemoved(name: String, description: String)
  AntiPatternModified(name: String, fields_changed: List(String))
}

// ============================================================================
// FUNCTIONAL CORE - Pure Comparison Functions
// ============================================================================

/// Compare two specs and return a diff
pub fn compare_specs(old: Spec, new: Spec) -> SpecDiff {
  let name_changed = compare_string(old.name, new.name)
  let description_changed = compare_string(old.description, new.description)
  let version_changed = compare_string(old.version, new.version)
  let config_changes = compare_config(old.config, new.config)
  let feature_changes = compare_features(old.features, new.features)
  let behavior_changes = extract_behavior_changes(feature_changes)
  let rule_changes = compare_rules(old.rules, new.rules)
  let anti_pattern_changes =
    compare_anti_patterns(old.anti_patterns, new.anti_patterns)
  let success_criteria_changes =
    compare_string_lists(old.success_criteria, new.success_criteria)

  let has_changes =
    option.is_some(name_changed)
    || option.is_some(description_changed)
    || option.is_some(version_changed)
    || !list.is_empty(config_changes)
    || !list.is_empty(feature_changes)
    || !list.is_empty(rule_changes)
    || !list.is_empty(anti_pattern_changes)
    || !list.is_empty(success_criteria_changes.added)
    || !list.is_empty(success_criteria_changes.removed)

  SpecDiff(
    name_changed: name_changed,
    description_changed: description_changed,
    version_changed: version_changed,
    config_changes: config_changes,
    feature_changes: feature_changes,
    behavior_changes: behavior_changes,
    rule_changes: rule_changes,
    anti_pattern_changes: anti_pattern_changes,
    success_criteria_changes: success_criteria_changes,
    has_changes: has_changes,
  )
}

/// Compare two strings, return Some(change) if different
fn compare_string(old: String, new: String) -> Option(StringChange) {
  case old == new {
    True -> None
    False -> Some(StringChange(old, new))
  }
}

/// Compare two string lists, return added and removed items
fn compare_string_lists(old: List(String), new: List(String)) -> ListChange {
  let added = list.filter(new, fn(item) { !list.contains(old, item) })
  let removed = list.filter(old, fn(item) { !list.contains(new, item) })
  ListChange(added: added, removed: removed)
}

/// Compare configurations
fn compare_config(old: Config, new: Config) -> List(ConfigChange) {
  let changes = []

  let changes = case old.base_url == new.base_url {
    True -> changes
    False -> [BaseUrlChanged(old.base_url, new.base_url), ..changes]
  }

  let changes = case old.timeout_ms == new.timeout_ms {
    True -> changes
    False -> [TimeoutChanged(old.timeout_ms, new.timeout_ms), ..changes]
  }

  let changes = case old.allow_localhost == new.allow_localhost {
    True -> changes
    False -> [
      AllowLocalhostChanged(
        option.unwrap(old.allow_localhost, False),
        option.unwrap(new.allow_localhost, False),
      ),
      ..changes
    ]
  }

  let old_headers = dict.keys(old.headers)
  let new_headers = dict.keys(new.headers)
  let added_headers =
    list.filter(new_headers, fn(h) { !list.contains(old_headers, h) })
  let removed_headers =
    list.filter(old_headers, fn(h) { !list.contains(new_headers, h) })
  let common_headers =
    list.filter(old_headers, fn(h) { list.contains(new_headers, h) })
  let modified_headers =
    list.filter(common_headers, fn(h) {
      dict.get(old.headers, h) != dict.get(new.headers, h)
    })

  case
    list.is_empty(added_headers)
    && list.is_empty(removed_headers)
    && list.is_empty(modified_headers)
  {
    True -> changes
    False -> [
      HeadersChanged(added_headers, removed_headers, modified_headers),
      ..changes
    ]
  }
}

/// Compare features
fn compare_features(
  old: List(Feature),
  new: List(Feature),
) -> List(FeatureChange) {
  let old_names = list.map(old, fn(f) { f.name })
  let new_names = list.map(new, fn(f) { f.name })

  // Find added features
  let added =
    new
    |> list.filter(fn(f) { !list.contains(old_names, f.name) })
    |> list.map(fn(f) { FeatureAdded(f.name, list.length(f.behaviors)) })

  // Find removed features
  let removed =
    old
    |> list.filter(fn(f) { !list.contains(new_names, f.name) })
    |> list.map(fn(f) { FeatureRemoved(f.name, list.length(f.behaviors)) })

  // Find modified features
  let modified =
    old
    |> list.filter(fn(f) { list.contains(new_names, f.name) })
    |> list.filter_map(fn(old_feature) {
      case list.find(new, fn(nf) { nf.name == old_feature.name }) {
        Ok(new_feature) ->
          case compare_single_feature(old_feature, new_feature) {
            Some(change) -> Ok(change)
            None -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    })

  list.concat([added, removed, modified])
}

/// Compare a single feature
fn compare_single_feature(old: Feature, new: Feature) -> Option(FeatureChange) {
  let description_changed = compare_string(old.description, new.description)
  let behavior_changes =
    compare_behaviors(old.name, old.behaviors, new.behaviors)

  case option.is_none(description_changed) && list.is_empty(behavior_changes) {
    True -> None
    False ->
      Some(FeatureModified(
        name: old.name,
        description_changed: description_changed,
        behavior_changes: behavior_changes,
      ))
  }
}

/// Compare behaviors within a feature
fn compare_behaviors(
  feature_name: String,
  old: List(Behavior),
  new: List(Behavior),
) -> List(BehaviorChange) {
  let old_names = list.map(old, fn(b) { b.name })
  let new_names = list.map(new, fn(b) { b.name })

  // Find added behaviors
  let added =
    new
    |> list.filter(fn(b) { !list.contains(old_names, b.name) })
    |> list.map(fn(b) { BehaviorAdded(feature_name, b.name, b.intent) })

  // Find removed behaviors
  let removed =
    old
    |> list.filter(fn(b) { !list.contains(new_names, b.name) })
    |> list.map(fn(b) { BehaviorRemoved(feature_name, b.name, b.intent) })

  // Find modified behaviors
  let modified =
    old
    |> list.filter(fn(b) { list.contains(new_names, b.name) })
    |> list.filter_map(fn(old_behavior) {
      case list.find(new, fn(nb) { nb.name == old_behavior.name }) {
        Ok(new_behavior) ->
          case
            compare_single_behavior(feature_name, old_behavior, new_behavior)
          {
            Some(change) -> Ok(change)
            None -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    })

  list.concat([added, removed, modified])
}

/// Compare a single behavior
fn compare_single_behavior(
  feature_name: String,
  old: Behavior,
  new: Behavior,
) -> Option(BehaviorChange) {
  let modifications = []

  // Check intent
  let modifications = case old.intent == new.intent {
    True -> modifications
    False -> [IntentChanged(old.intent, new.intent), ..modifications]
  }

  // Check notes
  let modifications = case old.notes == new.notes {
    True -> modifications
    False -> [NotesChanged(old.notes, new.notes), ..modifications]
  }

  // Check requires
  let requires_change = compare_string_lists(old.requires, new.requires)
  let modifications = case
    list.is_empty(requires_change.added)
    && list.is_empty(requires_change.removed)
  {
    True -> modifications
    False -> [
      RequiresChanged(requires_change.added, requires_change.removed),
      ..modifications
    ]
  }

  // Check tags
  let tags_change = compare_string_lists(old.tags, new.tags)
  let modifications = case
    list.is_empty(tags_change.added) && list.is_empty(tags_change.removed)
  {
    True -> modifications
    False -> [
      TagsChanged(tags_change.added, tags_change.removed),
      ..modifications
    ]
  }

  // Check captures
  let old_captures = dict.keys(old.captures)
  let new_captures = dict.keys(new.captures)
  let captures_change = compare_string_lists(old_captures, new_captures)
  let modifications = case
    list.is_empty(captures_change.added)
    && list.is_empty(captures_change.removed)
  {
    True -> modifications
    False -> [
      CapturesChanged(captures_change.added, captures_change.removed),
      ..modifications
    ]
  }

  // Check request
  let request_changes = compare_request(old.request, new.request)
  let modifications = case list.is_empty(request_changes) {
    True -> modifications
    False -> [RequestChanged(request_changes), ..modifications]
  }

  // Check response
  let response_changes = compare_response(old.response, new.response)
  let modifications = case list.is_empty(response_changes) {
    True -> modifications
    False -> [ResponseChanged(response_changes), ..modifications]
  }

  case list.is_empty(modifications) {
    True -> None
    False -> Some(BehaviorModified(feature_name, old.name, modifications))
  }
}

/// Compare requests
fn compare_request(old: Request, new: Request) -> List(RequestChange) {
  let changes = []

  // Check method
  let old_method = types.method_to_string(old.method)
  let new_method = types.method_to_string(new.method)
  let changes = case old_method == new_method {
    True -> changes
    False -> [MethodChanged(old_method, new_method), ..changes]
  }

  // Check path
  let changes = case old.path == new.path {
    True -> changes
    False -> [PathChanged(old.path, new.path), ..changes]
  }

  // Check headers
  let old_headers = dict.keys(old.headers)
  let new_headers = dict.keys(new.headers)
  let headers_change = compare_string_lists(old_headers, new_headers)
  let changes = case
    list.is_empty(headers_change.added) && list.is_empty(headers_change.removed)
  {
    True -> changes
    False -> [
      RequestHeadersChanged(headers_change.added, headers_change.removed),
      ..changes
    ]
  }

  // Check query
  let old_query = dict.keys(old.query)
  let new_query = dict.keys(new.query)
  let query_change = compare_string_lists(old_query, new_query)
  let changes = case
    list.is_empty(query_change.added) && list.is_empty(query_change.removed)
  {
    True -> changes
    False -> [QueryChanged(query_change.added, query_change.removed), ..changes]
  }

  // Check body (simplified - just detect if changed)
  let old_body_str = json.to_string(old.body)
  let new_body_str = json.to_string(new.body)
  let changes = case old_body_str == new_body_str {
    True -> changes
    False -> [BodyChanged, ..changes]
  }

  changes
}

/// Compare responses
fn compare_response(old: Response, new: Response) -> List(ResponseChange) {
  let changes = []

  // Check status
  let changes = case old.status == new.status {
    True -> changes
    False -> [StatusChanged(old.status, new.status), ..changes]
  }

  // Check checks
  let old_checks = dict.keys(old.checks)
  let new_checks = dict.keys(new.checks)
  let added_checks =
    list.filter(new_checks, fn(c) { !list.contains(old_checks, c) })
  let removed_checks =
    list.filter(old_checks, fn(c) { !list.contains(new_checks, c) })
  let common_checks =
    list.filter(old_checks, fn(c) { list.contains(new_checks, c) })
  let modified_checks =
    list.filter(common_checks, fn(c) {
      case dict.get(old.checks, c), dict.get(new.checks, c) {
        Ok(old_check), Ok(new_check) -> !check_equals(old_check, new_check)
        _, _ -> False
      }
    })

  let changes = case list.is_empty(added_checks) {
    True -> changes
    False -> [ChecksAdded(added_checks), ..changes]
  }

  let changes = case list.is_empty(removed_checks) {
    True -> changes
    False -> [ChecksRemoved(removed_checks), ..changes]
  }

  let changes = case list.is_empty(modified_checks) {
    True -> changes
    False -> [ChecksModified(modified_checks), ..changes]
  }

  // Check headers
  let old_headers = dict.keys(old.headers)
  let new_headers = dict.keys(new.headers)
  let headers_change = compare_string_lists(old_headers, new_headers)
  let changes = case
    list.is_empty(headers_change.added) && list.is_empty(headers_change.removed)
  {
    True -> changes
    False -> [
      ResponseHeadersChanged(headers_change.added, headers_change.removed),
      ..changes
    ]
  }

  // Check example (simplified - just detect if changed)
  let old_example_str = json.to_string(old.example)
  let new_example_str = json.to_string(new.example)
  let changes = case old_example_str == new_example_str {
    True -> changes
    False -> [ExampleChanged, ..changes]
  }

  changes
}

/// Check if two Check values are equal
fn check_equals(a: Check, b: Check) -> Bool {
  a.rule == b.rule && a.why == b.why
}

/// Compare rules
fn compare_rules(old: List(Rule), new: List(Rule)) -> List(RuleChange) {
  let old_names = list.map(old, fn(r) { r.name })
  let new_names = list.map(new, fn(r) { r.name })

  // Find added rules
  let added =
    new
    |> list.filter(fn(r) { !list.contains(old_names, r.name) })
    |> list.map(fn(r) { RuleAdded(r.name, r.description) })

  // Find removed rules
  let removed =
    old
    |> list.filter(fn(r) { !list.contains(new_names, r.name) })
    |> list.map(fn(r) { RuleRemoved(r.name, r.description) })

  // Find modified rules
  let modified =
    old
    |> list.filter(fn(r) { list.contains(new_names, r.name) })
    |> list.filter_map(fn(old_rule) {
      case list.find(new, fn(nr) { nr.name == old_rule.name }) {
        Ok(new_rule) ->
          case compare_single_rule(old_rule, new_rule) {
            Some(change) -> Ok(change)
            None -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    })

  list.concat([added, removed, modified])
}

/// Compare a single rule
fn compare_single_rule(old: Rule, new: Rule) -> Option(RuleChange) {
  let modifications = []

  // Check description
  let modifications = case old.description == new.description {
    True -> modifications
    False -> [
      RuleDescriptionChanged(old.description, new.description),
      ..modifications
    ]
  }

  // Check when condition
  let modifications = case when_equals(old.when, new.when) {
    True -> modifications
    False -> [RuleWhenChanged, ..modifications]
  }

  // Check rule check conditions
  let check_modifications = compare_rule_check(old.check, new.check)
  let modifications = list.concat([modifications, check_modifications])

  case list.is_empty(modifications) {
    True -> None
    False -> Some(RuleModified(old.name, modifications))
  }
}

/// Compare When conditions
fn when_equals(old: Option(When), new: Option(When)) -> Bool {
  case old, new {
    None, None -> True
    Some(o), Some(n) ->
      o.status == n.status
      && option_method_equals(o.method, n.method)
      && o.path == n.path
    _, _ -> False
  }
}

/// Compare optional methods
fn option_method_equals(
  old: Option(types.Method),
  new: Option(types.Method),
) -> Bool {
  case old, new {
    None, None -> True
    Some(o), Some(n) -> types.method_to_string(o) == types.method_to_string(n)
    _, _ -> False
  }
}

/// Compare rule checks
fn compare_rule_check(old: RuleCheck, new: RuleCheck) -> List(RuleModification) {
  let modifications = []

  let modifications = case
    old.body_must_not_contain == new.body_must_not_contain
  {
    True -> modifications
    False -> [RuleCheckChanged("body_must_not_contain"), ..modifications]
  }

  let modifications = case old.body_must_contain == new.body_must_contain {
    True -> modifications
    False -> [RuleCheckChanged("body_must_contain"), ..modifications]
  }

  let modifications = case old.fields_must_exist == new.fields_must_exist {
    True -> modifications
    False -> [RuleCheckChanged("fields_must_exist"), ..modifications]
  }

  let modifications = case
    old.fields_must_not_exist == new.fields_must_not_exist
  {
    True -> modifications
    False -> [RuleCheckChanged("fields_must_not_exist"), ..modifications]
  }

  let modifications = case old.header_must_exist == new.header_must_exist {
    True -> modifications
    False -> [RuleCheckChanged("header_must_exist"), ..modifications]
  }

  let modifications = case
    old.header_must_not_exist == new.header_must_not_exist
  {
    True -> modifications
    False -> [RuleCheckChanged("header_must_not_exist"), ..modifications]
  }

  modifications
}

/// Compare anti-patterns
fn compare_anti_patterns(
  old: List(AntiPattern),
  new: List(AntiPattern),
) -> List(AntiPatternChange) {
  let old_names = list.map(old, fn(a) { a.name })
  let new_names = list.map(new, fn(a) { a.name })

  // Find added anti-patterns
  let added =
    new
    |> list.filter(fn(a) { !list.contains(old_names, a.name) })
    |> list.map(fn(a) { AntiPatternAdded(a.name, a.description) })

  // Find removed anti-patterns
  let removed =
    old
    |> list.filter(fn(a) { !list.contains(new_names, a.name) })
    |> list.map(fn(a) { AntiPatternRemoved(a.name, a.description) })

  // Find modified anti-patterns
  let modified =
    old
    |> list.filter(fn(a) { list.contains(new_names, a.name) })
    |> list.filter_map(fn(old_ap) {
      case list.find(new, fn(na) { na.name == old_ap.name }) {
        Ok(new_ap) ->
          case compare_single_anti_pattern(old_ap, new_ap) {
            Some(change) -> Ok(change)
            None -> Error(Nil)
          }
        Error(_) -> Error(Nil)
      }
    })
  list.concat([added, removed, modified])
}

/// Compare a single anti-pattern
fn compare_single_anti_pattern(
  old: AntiPattern,
  new: AntiPattern,
) -> Option(AntiPatternChange) {
  let changed_fields = []

  let changed_fields = case old.description == new.description {
    True -> changed_fields
    False -> ["description", ..changed_fields]
  }

  let changed_fields = case old.why == new.why {
    True -> changed_fields
    False -> ["why", ..changed_fields]
  }

  let changed_fields = case
    json.to_string(old.bad_example) == json.to_string(new.bad_example)
  {
    True -> changed_fields
    False -> ["bad_example", ..changed_fields]
  }

  let changed_fields = case
    json.to_string(old.good_example) == json.to_string(new.good_example)
  {
    True -> changed_fields
    False -> ["good_example", ..changed_fields]
  }

  case list.is_empty(changed_fields) {
    True -> None
    False -> Some(AntiPatternModified(old.name, changed_fields))
  }
}

/// Extract flat list of behavior changes from feature changes
fn extract_behavior_changes(
  feature_changes: List(FeatureChange),
) -> List(BehaviorChange) {
  feature_changes
  |> list.flat_map(fn(fc) {
    case fc {
      FeatureAdded(_name, _) -> []
      FeatureRemoved(_name, _) -> []
      FeatureModified(_, _, behavior_changes) -> behavior_changes
    }
  })
}

// ============================================================================
// Formatting Functions - Human Readable Output
// ============================================================================

/// Format a SpecDiff as human-readable text
pub fn format_diff(diff: SpecDiff) -> String {
  case diff.has_changes {
    False -> "No changes detected between specs."
    True -> format_diff_details(diff)
  }
}

fn format_diff_details(diff: SpecDiff) -> String {
  let sections = []

  // Metadata changes
  let sections = case diff.name_changed {
    Some(change) -> ["Name: " <> change.old <> " -> " <> change.new, ..sections]
    None -> sections
  }

  let sections = case diff.description_changed {
    Some(_change) -> ["Description changed", ..sections]
    None -> sections
  }

  let sections = case diff.version_changed {
    Some(change) -> [
      "Version: " <> change.old <> " -> " <> change.new,
      ..sections
    ]
    None -> sections
  }

  // Config changes
  let config_text = format_config_changes(diff.config_changes)
  let sections = case config_text {
    "" -> sections
    text -> [text, ..sections]
  }

  // Feature changes
  let feature_text = format_feature_changes(diff.feature_changes)
  let sections = case feature_text {
    "" -> sections
    text -> [text, ..sections]
  }

  // Rule changes
  let rule_text = format_rule_changes(diff.rule_changes)
  let sections = case rule_text {
    "" -> sections
    text -> [text, ..sections]
  }

  // Anti-pattern changes
  let ap_text = format_anti_pattern_changes(diff.anti_pattern_changes)
  let sections = case ap_text {
    "" -> sections
    text -> [text, ..sections]
  }

  // Success criteria changes
  let criteria_text =
    format_list_change("Success Criteria", diff.success_criteria_changes)
  let sections = case criteria_text {
    "" -> sections
    text -> [text, ..sections]
  }

  "SPEC DIFF\n=========\n\n" <> string.join(list.reverse(sections), "\n\n")
}

fn format_config_changes(changes: List(ConfigChange)) -> String {
  case list.is_empty(changes) {
    True -> ""
    False -> {
      let lines =
        list.map(changes, fn(change) {
          case change {
            BaseUrlChanged(old, new) -> "  - base_url: " <> old <> " -> " <> new
            TimeoutChanged(old, new) ->
              "  - timeout_ms: "
              <> int.to_string(old)
              <> " -> "
              <> int.to_string(new)
            AllowLocalhostChanged(old, new) ->
              "  - allow_localhost: "
              <> bool_to_string(old)
              <> " -> "
              <> bool_to_string(new)
            HeadersChanged(added, removed, modified) ->
              format_headers_change(added, removed, modified)
          }
        })
      "CONFIG CHANGES:\n" <> string.join(lines, "\n")
    }
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn format_headers_change(
  added: List(String),
  removed: List(String),
  modified: List(String),
) -> String {
  let parts = []
  let parts = case list.is_empty(added) {
    True -> parts
    False -> ["added: " <> string.join(added, ", "), ..parts]
  }
  let parts = case list.is_empty(removed) {
    True -> parts
    False -> ["removed: " <> string.join(removed, ", "), ..parts]
  }
  let parts = case list.is_empty(modified) {
    True -> parts
    False -> ["modified: " <> string.join(modified, ", "), ..parts]
  }
  "  - headers: " <> string.join(list.reverse(parts), "; ")
}

fn format_feature_changes(changes: List(FeatureChange)) -> String {
  case list.is_empty(changes) {
    True -> ""
    False -> {
      let lines =
        list.map(changes, fn(change) {
          case change {
            FeatureAdded(name, count) ->
              "  + [ADDED] "
              <> name
              <> " ("
              <> int.to_string(count)
              <> " behaviors)"
            FeatureRemoved(name, count) ->
              "  - [REMOVED] "
              <> name
              <> " ("
              <> int.to_string(count)
              <> " behaviors)"
            FeatureModified(name, _, behavior_changes) ->
              "  ~ [MODIFIED] "
              <> name
              <> "\n"
              <> format_behavior_changes_indented(behavior_changes, "      ")
          }
        })
      "FEATURE CHANGES:\n" <> string.join(lines, "\n")
    }
  }
}

fn format_behavior_changes_indented(
  changes: List(BehaviorChange),
  indent: String,
) -> String {
  changes
  |> list.map(fn(change) {
    case change {
      BehaviorAdded(_, name, intent) ->
        indent <> "+ [ADDED] " <> name <> " - " <> truncate(intent, 50)
      BehaviorRemoved(_, name, intent) ->
        indent <> "- [REMOVED] " <> name <> " - " <> truncate(intent, 50)
      BehaviorModified(_, name, modifications) ->
        indent
        <> "~ [MODIFIED] "
        <> name
        <> ": "
        <> format_modifications_summary(modifications)
    }
  })
  |> string.join("\n")
}

fn format_modifications_summary(
  modifications: List(BehaviorModification),
) -> String {
  modifications
  |> list.map(fn(mod) {
    case mod {
      IntentChanged(_, _) -> "intent"
      NotesChanged(_, _) -> "notes"
      RequestChanged(_) -> "request"
      ResponseChanged(_) -> "response"
      RequiresChanged(_, _) -> "requires"
      TagsChanged(_, _) -> "tags"
      CapturesChanged(_, _) -> "captures"
    }
  })
  |> string.join(", ")
}

fn format_rule_changes(changes: List(RuleChange)) -> String {
  case list.is_empty(changes) {
    True -> ""
    False -> {
      let lines =
        list.map(changes, fn(change) {
          case change {
            RuleAdded(name, desc) ->
              "  + [ADDED] " <> name <> ": " <> truncate(desc, 50)
            RuleRemoved(name, desc) ->
              "  - [REMOVED] " <> name <> ": " <> truncate(desc, 50)
            RuleModified(name, modifications) ->
              "  ~ [MODIFIED] "
              <> name
              <> ": "
              <> format_rule_modifications_summary(modifications)
          }
        })
      "RULE CHANGES:\n" <> string.join(lines, "\n")
    }
  }
}

fn format_rule_modifications_summary(
  modifications: List(RuleModification),
) -> String {
  modifications
  |> list.map(fn(mod) {
    case mod {
      RuleDescriptionChanged(_, _) -> "description"
      RuleWhenChanged -> "when condition"
      RuleCheckChanged(field) -> field
    }
  })
  |> string.join(", ")
}

fn format_anti_pattern_changes(changes: List(AntiPatternChange)) -> String {
  case list.is_empty(changes) {
    True -> ""
    False -> {
      let lines =
        list.map(changes, fn(change) {
          case change {
            AntiPatternAdded(name, desc) ->
              "  + [ADDED] " <> name <> ": " <> truncate(desc, 50)
            AntiPatternRemoved(name, desc) ->
              "  - [REMOVED] " <> name <> ": " <> truncate(desc, 50)
            AntiPatternModified(name, fields) ->
              "  ~ [MODIFIED] " <> name <> ": " <> string.join(fields, ", ")
          }
        })
      "ANTI-PATTERN CHANGES:\n" <> string.join(lines, "\n")
    }
  }
}

fn format_list_change(label: String, change: ListChange) -> String {
  case list.is_empty(change.added) && list.is_empty(change.removed) {
    True -> ""
    False -> {
      let parts = []
      let parts = case list.is_empty(change.added) {
        True -> parts
        False -> ["  + Added: " <> string.join(change.added, ", "), ..parts]
      }
      let parts = case list.is_empty(change.removed) {
        True -> parts
        False -> ["  - Removed: " <> string.join(change.removed, ", "), ..parts]
      }
      label <> " CHANGES:\n" <> string.join(list.reverse(parts), "\n")
    }
  }
}

fn truncate(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len) <> "..."
    False -> s
  }
}

// ============================================================================
// JSON Output
// ============================================================================

/// Convert a SpecDiff to JSON
pub fn diff_to_json(diff: SpecDiff) -> Json {
  json.object([
    #("has_changes", json.bool(diff.has_changes)),
    #("name_changed", option_string_change_to_json(diff.name_changed)),
    #(
      "description_changed",
      option_string_change_to_json(diff.description_changed),
    ),
    #("version_changed", option_string_change_to_json(diff.version_changed)),
    #("config_changes", json.array(diff.config_changes, config_change_to_json)),
    #(
      "feature_changes",
      json.array(diff.feature_changes, feature_change_to_json),
    ),
    #(
      "behavior_changes",
      json.array(diff.behavior_changes, behavior_change_to_json),
    ),
    #("rule_changes", json.array(diff.rule_changes, rule_change_to_json)),
    #(
      "anti_pattern_changes",
      json.array(diff.anti_pattern_changes, anti_pattern_change_to_json),
    ),
    #(
      "success_criteria_changes",
      list_change_to_json(diff.success_criteria_changes),
    ),
  ])
}

fn option_string_change_to_json(change: Option(StringChange)) -> Json {
  case change {
    None -> json.null()
    Some(c) ->
      json.object([
        #("old", json.string(c.old)),
        #("new", json.string(c.new)),
      ])
  }
}

fn list_change_to_json(change: ListChange) -> Json {
  json.object([
    #("added", json.array(change.added, json.string)),
    #("removed", json.array(change.removed, json.string)),
  ])
}

fn config_change_to_json(change: ConfigChange) -> Json {
  case change {
    BaseUrlChanged(old, new) ->
      json.object([
        #("type", json.string("base_url_changed")),
        #("old", json.string(old)),
        #("new", json.string(new)),
      ])
    TimeoutChanged(old, new) ->
      json.object([
        #("type", json.string("timeout_changed")),
        #("old", json.int(old)),
        #("new", json.int(new)),
      ])
    AllowLocalhostChanged(old, new) ->
      json.object([
        #("type", json.string("allow_localhost_changed")),
        #("old", json.bool(old)),
        #("new", json.bool(new)),
      ])
    HeadersChanged(added, removed, modified) ->
      json.object([
        #("type", json.string("headers_changed")),
        #("added", json.array(added, json.string)),
        #("removed", json.array(removed, json.string)),
        #("modified", json.array(modified, json.string)),
      ])
  }
}

fn feature_change_to_json(change: FeatureChange) -> Json {
  case change {
    FeatureAdded(name, count) ->
      json.object([
        #("type", json.string("added")),
        #("name", json.string(name)),
        #("behavior_count", json.int(count)),
      ])
    FeatureRemoved(name, count) ->
      json.object([
        #("type", json.string("removed")),
        #("name", json.string(name)),
        #("behavior_count", json.int(count)),
      ])
    FeatureModified(name, desc_changed, behavior_changes) ->
      json.object([
        #("type", json.string("modified")),
        #("name", json.string(name)),
        #("description_changed", option_string_change_to_json(desc_changed)),
        #(
          "behavior_changes",
          json.array(behavior_changes, behavior_change_to_json),
        ),
      ])
  }
}

fn behavior_change_to_json(change: BehaviorChange) -> Json {
  case change {
    BehaviorAdded(feature, name, intent) ->
      json.object([
        #("type", json.string("added")),
        #("feature", json.string(feature)),
        #("name", json.string(name)),
        #("intent", json.string(intent)),
      ])
    BehaviorRemoved(feature, name, intent) ->
      json.object([
        #("type", json.string("removed")),
        #("feature", json.string(feature)),
        #("name", json.string(name)),
        #("intent", json.string(intent)),
      ])
    BehaviorModified(feature, name, modifications) ->
      json.object([
        #("type", json.string("modified")),
        #("feature", json.string(feature)),
        #("name", json.string(name)),
        #(
          "modifications",
          json.array(modifications, behavior_modification_to_json),
        ),
      ])
  }
}

fn behavior_modification_to_json(mod: BehaviorModification) -> Json {
  case mod {
    IntentChanged(old, new) ->
      json.object([
        #("field", json.string("intent")),
        #("old", json.string(old)),
        #("new", json.string(new)),
      ])
    NotesChanged(old, new) ->
      json.object([
        #("field", json.string("notes")),
        #("old", json.string(old)),
        #("new", json.string(new)),
      ])
    RequestChanged(changes) ->
      json.object([
        #("field", json.string("request")),
        #("changes", json.array(changes, request_change_to_json)),
      ])
    ResponseChanged(changes) ->
      json.object([
        #("field", json.string("response")),
        #("changes", json.array(changes, response_change_to_json)),
      ])
    RequiresChanged(added, removed) ->
      json.object([
        #("field", json.string("requires")),
        #("added", json.array(added, json.string)),
        #("removed", json.array(removed, json.string)),
      ])
    TagsChanged(added, removed) ->
      json.object([
        #("field", json.string("tags")),
        #("added", json.array(added, json.string)),
        #("removed", json.array(removed, json.string)),
      ])
    CapturesChanged(added, removed) ->
      json.object([
        #("field", json.string("captures")),
        #("added", json.array(added, json.string)),
        #("removed", json.array(removed, json.string)),
      ])
  }
}

fn request_change_to_json(change: RequestChange) -> Json {
  case change {
    MethodChanged(old, new) ->
      json.object([
        #("field", json.string("method")),
        #("old", json.string(old)),
        #("new", json.string(new)),
      ])
    PathChanged(old, new) ->
      json.object([
        #("field", json.string("path")),
        #("old", json.string(old)),
        #("new", json.string(new)),
      ])
    RequestHeadersChanged(added, removed) ->
      json.object([
        #("field", json.string("headers")),
        #("added", json.array(added, json.string)),
        #("removed", json.array(removed, json.string)),
      ])
    QueryChanged(added, removed) ->
      json.object([
        #("field", json.string("query")),
        #("added", json.array(added, json.string)),
        #("removed", json.array(removed, json.string)),
      ])
    BodyChanged ->
      json.object([
        #("field", json.string("body")),
        #("changed", json.bool(True)),
      ])
  }
}

fn response_change_to_json(change: ResponseChange) -> Json {
  case change {
    StatusChanged(old, new) ->
      json.object([
        #("field", json.string("status")),
        #("old", json.int(old)),
        #("new", json.int(new)),
      ])
    ChecksAdded(checks) ->
      json.object([
        #("field", json.string("checks")),
        #("action", json.string("added")),
        #("checks", json.array(checks, json.string)),
      ])
    ChecksRemoved(checks) ->
      json.object([
        #("field", json.string("checks")),
        #("action", json.string("removed")),
        #("checks", json.array(checks, json.string)),
      ])
    ChecksModified(checks) ->
      json.object([
        #("field", json.string("checks")),
        #("action", json.string("modified")),
        #("checks", json.array(checks, json.string)),
      ])
    ResponseHeadersChanged(added, removed) ->
      json.object([
        #("field", json.string("headers")),
        #("added", json.array(added, json.string)),
        #("removed", json.array(removed, json.string)),
      ])
    ExampleChanged ->
      json.object([
        #("field", json.string("example")),
        #("changed", json.bool(True)),
      ])
  }
}

fn rule_change_to_json(change: RuleChange) -> Json {
  case change {
    RuleAdded(name, desc) ->
      json.object([
        #("type", json.string("added")),
        #("name", json.string(name)),
        #("description", json.string(desc)),
      ])
    RuleRemoved(name, desc) ->
      json.object([
        #("type", json.string("removed")),
        #("name", json.string(name)),
        #("description", json.string(desc)),
      ])
    RuleModified(name, modifications) ->
      json.object([
        #("type", json.string("modified")),
        #("name", json.string(name)),
        #("modifications", json.array(modifications, rule_modification_to_json)),
      ])
  }
}

fn rule_modification_to_json(mod: RuleModification) -> Json {
  case mod {
    RuleDescriptionChanged(old, new) ->
      json.object([
        #("field", json.string("description")),
        #("old", json.string(old)),
        #("new", json.string(new)),
      ])
    RuleWhenChanged ->
      json.object([
        #("field", json.string("when")),
        #("changed", json.bool(True)),
      ])
    RuleCheckChanged(field) ->
      json.object([
        #("field", json.string("check")),
        #("subfield", json.string(field)),
      ])
  }
}

fn anti_pattern_change_to_json(change: AntiPatternChange) -> Json {
  case change {
    AntiPatternAdded(name, desc) ->
      json.object([
        #("type", json.string("added")),
        #("name", json.string(name)),
        #("description", json.string(desc)),
      ])
    AntiPatternRemoved(name, desc) ->
      json.object([
        #("type", json.string("removed")),
        #("name", json.string(name)),
        #("description", json.string(desc)),
      ])
    AntiPatternModified(name, fields) ->
      json.object([
        #("type", json.string("modified")),
        #("name", json.string(name)),
        #("fields_changed", json.array(fields, json.string)),
      ])
  }
}

// ============================================================================
// Summary Functions
// ============================================================================

/// Get a summary of changes for quick overview
pub fn diff_summary(diff: SpecDiff) -> String {
  case diff.has_changes {
    False -> "No changes"
    True -> {
      let parts = []

      // Count behavior changes
      let added_behaviors = count_behavior_additions(diff.feature_changes)
      let removed_behaviors = count_behavior_removals(diff.feature_changes)
      let modified_behaviors =
        count_behavior_modifications(diff.feature_changes)

      let parts = case added_behaviors {
        0 -> parts
        n -> [int.to_string(n) <> " behavior(s) added", ..parts]
      }

      let parts = case removed_behaviors {
        0 -> parts
        n -> [int.to_string(n) <> " behavior(s) removed", ..parts]
      }

      let parts = case modified_behaviors {
        0 -> parts
        n -> [int.to_string(n) <> " behavior(s) modified", ..parts]
      }

      // Count rule changes
      let added_rules =
        list.count(diff.rule_changes, fn(r) {
          case r {
            RuleAdded(_, _) -> True
            _ -> False
          }
        })
      let removed_rules =
        list.count(diff.rule_changes, fn(r) {
          case r {
            RuleRemoved(_, _) -> True
            _ -> False
          }
        })

      let parts = case added_rules {
        0 -> parts
        n -> [int.to_string(n) <> " rule(s) added", ..parts]
      }

      let parts = case removed_rules {
        0 -> parts
        n -> [int.to_string(n) <> " rule(s) removed", ..parts]
      }

      // Count config changes
      let parts = case list.is_empty(diff.config_changes) {
        True -> parts
        False -> [
          int.to_string(list.length(diff.config_changes)) <> " config change(s)",
          ..parts
        ]
      }

      string.join(list.reverse(parts), ", ")
    }
  }
}

fn count_behavior_additions(changes: List(FeatureChange)) -> Int {
  changes
  |> list.fold(0, fn(acc, change) {
    case change {
      FeatureAdded(_, count) -> acc + count
      FeatureModified(_, _, behavior_changes) ->
        acc
        + list.count(behavior_changes, fn(bc) {
          case bc {
            BehaviorAdded(_, _, _) -> True
            _ -> False
          }
        })
      _ -> acc
    }
  })
}

fn count_behavior_removals(changes: List(FeatureChange)) -> Int {
  changes
  |> list.fold(0, fn(acc, change) {
    case change {
      FeatureRemoved(_, count) -> acc + count
      FeatureModified(_, _, behavior_changes) ->
        acc
        + list.count(behavior_changes, fn(bc) {
          case bc {
            BehaviorRemoved(_, _, _) -> True
            _ -> False
          }
        })
      _ -> acc
    }
  })
}

fn count_behavior_modifications(changes: List(FeatureChange)) -> Int {
  changes
  |> list.fold(0, fn(acc, change) {
    case change {
      FeatureModified(_, _, behavior_changes) ->
        acc
        + list.count(behavior_changes, fn(bc) {
          case bc {
            BehaviorModified(_, _, _) -> True
            _ -> False
          }
        })
      _ -> acc
    }
  })
}
