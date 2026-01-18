//// Spec to Beads Converter
//// Transforms CUE specifications into atomic Bead work items.
////
//// Each Behavior in each Feature becomes one Bead with:
//// - Unique ID: `bead-<feature-slug>-<behavior-slug>-<uuid8>`
//// - Priority derived from behavior tags (see below)
//// - Labels from feature name, HTTP method, and behavior tags
////
//// ## Priority Tags
//// Add these tags to behaviors to control priority (highest wins):
//// - `critical` → 5 (urgent, blocking)
//// - `high` → 4 (important)
//// - (no tag) → 3 (default, normal)
//// - `low` → 2 (can wait)
//// - `nice-to-have` → 1 (optional)

import gleam/list
import gleam/result
import gleam/string
import intent/bead_types.{type Bead, Bead, Open, Task}
import intent/types.{type Behavior, type Feature, type Spec, method_to_string}

/// Convert a Spec into a list of Beads (one per behavior)
pub fn spec_to_beads(spec: Spec) -> Result(List(Bead), String) {
  spec.features
  |> list.flat_map(feature_to_beads)
  |> Ok
}

/// Convert a Feature into a list of Beads (one per behavior)
fn feature_to_beads(feature: Feature) -> List(Bead) {
  feature.behaviors
  |> list.map(fn(behavior) { behavior_to_bead(feature.name, behavior) })
}

/// Convert a single Behavior into a Bead
fn behavior_to_bead(feature_name: String, behavior: Behavior) -> Bead {
  let id = generate_bead_id(feature_name, behavior.name)
  let title = generate_title(feature_name, behavior.name)
  let priority = derive_priority(behavior.tags)
  let labels = collect_labels(feature_name, behavior)
  let timestamp = current_timestamp()

  Bead(
    id: id,
    title: title,
    status: Open,
    priority: priority,
    issue_type: Task,
    created_at: timestamp,
    created_by: "spec_to_beads",
    updated_at: timestamp,
    labels: labels,
  )
}

/// Generate unique bead ID: bead-<feature>-<behavior>-<uuid>
fn generate_bead_id(feature_name: String, behavior_name: String) -> String {
  let feature_slug = slugify(feature_name)
  let behavior_slug = slugify(behavior_name)
  let uuid_short =
    generate_uuid()
    |> string.slice(0, 8)

  "bead-" <> feature_slug <> "-" <> behavior_slug <> "-" <> uuid_short
}

/// Generate title from feature and behavior names
fn generate_title(feature_name: String, behavior_name: String) -> String {
  "Implement " <> feature_name <> ": " <> behavior_name
}

/// Derive priority from behavior tags (highest priority tag wins)
/// Priority levels: critical=5, high=4, default=3, low=2, nice-to-have=1
fn derive_priority(tags: List(String)) -> Int {
  // Priority tags in precedence order (highest first)
  let priority_tags = [
    #("critical", 5),
    #("high", 4),
    #("low", 2),
    #("nice-to-have", 1),
  ]

  priority_tags
  |> list.find_map(fn(pair) {
    let #(tag, priority) = pair
    case list.contains(tags, tag) {
      True -> Ok(priority)
      False -> Error(Nil)
    }
  })
  |> result.unwrap(3)
}

/// Collect labels from feature, tags, and metadata
fn collect_labels(feature_name: String, behavior: Behavior) -> List(String) {
  let feature_label = slugify(feature_name)
  let method_label = method_to_string(behavior.request.method)
  let base_labels = ["spec-generated", "behavior", feature_label, method_label]

  list.concat([base_labels, behavior.tags])
  |> list.unique()
}

/// Slugify string: lowercase, replace spaces/special chars with hyphens
fn slugify(s: String) -> String {
  s
  |> string.lowercase()
  |> string.replace(" ", "-")
  |> string.replace("_", "-")
}

// FFI for UUID generation
@external(erlang, "intent_ffi", "generate_uuid")
fn generate_uuid() -> String

// FFI for current timestamp (ISO 8601)
@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String
