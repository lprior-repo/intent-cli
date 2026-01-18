/// Tests for spec_to_beads module
/// Phase 4 (RED): These tests should FAIL until implementation exists
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleeunit/should
import intent/bead_types.{Open, Task}
import intent/spec_builder
import intent/spec_to_beads
import intent/types.{
  type Behavior, type Feature, type Spec, AIHints, Behavior, Config, Feature,
  Get, ImplementationHints, Post, Request, Response, SecurityHints, Spec,
}

// Helper to create minimal test spec
fn minimal_spec(features: List(Feature)) -> Spec {
  Spec(
    name: "test-spec",
    description: "Test specification",
    audience: "testers",
    version: "1.0.0",
    success_criteria: [],
    config: Config("http://test", 1000, dict.new(), allow_localhost: False),
    features: features,
    rules: [],
    anti_patterns: [],
    ai_hints: AIHints(
      ImplementationHints([]),
      dict.new(),
      SecurityHints("", "", "", ""),
      [],
    ),
  )
}

// Helper to create test behavior with customizable fields
fn make_test_behavior(
  name: String,
  tags: List(String),
  method: types.Method,
) -> Behavior {
  Behavior(
    name: name,
    intent: "Test intent for " <> name,
    notes: "",
    requires: [],
    tags: tags,
    request: Request(method, "/" <> name, dict.new(), dict.new(), json.null()),
    response: Response(200, json.null(), dict.new(), dict.new()),
    captures: dict.new(),
  )
}

// ============================================================
// Core Functionality Tests
// ============================================================

pub fn empty_features_returns_empty_list_test() {
  let spec = minimal_spec([])

  spec_to_beads.spec_to_beads(spec)
  |> should.be_ok()
  |> should.equal([])
}

pub fn single_behavior_creates_one_bead_test() {
  let behavior = make_test_behavior("create-user", [], Get)
  let feature = Feature("User Management", "User CRUD", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  list.length(beads)
  |> should.equal(1)

  let assert [bead] = beads
  bead.status
  |> should.equal(Open)

  bead.issue_type
  |> should.equal(Task)
}

pub fn bead_title_includes_feature_and_behavior_name_test() {
  let behavior = make_test_behavior("create-recipe", [], Post)
  let feature = Feature("Recipe Management", "Recipe CRUD", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads

  // Title should include feature and behavior name
  bead.title
  |> should.equal("Implement Recipe Management: create-recipe")
}

// ============================================================
// Priority Derivation Tests
// ============================================================

pub fn priority_critical_tag_returns_5_test() {
  let behavior = make_test_behavior("urgent-task", ["critical"], Get)
  let feature = Feature("Urgent", "Urgent work", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  bead.priority
  |> should.equal(5)
}

pub fn priority_high_tag_returns_4_test() {
  let behavior = make_test_behavior("important-task", ["high"], Get)
  let feature = Feature("Important", "Important work", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  bead.priority
  |> should.equal(4)
}

pub fn priority_low_tag_returns_2_test() {
  let behavior = make_test_behavior("minor-task", ["low"], Get)
  let feature = Feature("Minor", "Minor work", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  bead.priority
  |> should.equal(2)
}

pub fn priority_nice_to_have_tag_returns_1_test() {
  let behavior = make_test_behavior("optional-task", ["nice-to-have"], Get)
  let feature = Feature("Optional", "Optional work", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  bead.priority
  |> should.equal(1)
}

pub fn priority_default_no_tags_returns_3_test() {
  let behavior = make_test_behavior("normal-task", [], Get)
  let feature = Feature("Normal", "Normal work", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  bead.priority
  |> should.equal(3)
}

pub fn priority_highest_tag_wins_test() {
  // If both "critical" and "low" present, "critical" wins
  let behavior = make_test_behavior("conflicting", ["low", "critical"], Get)
  let feature = Feature("Conflict", "Priority conflict", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  bead.priority
  |> should.equal(5)
}

// ============================================================
// Label Collection Tests
// ============================================================

pub fn labels_include_feature_name_slugified_test() {
  let behavior = make_test_behavior("test", [], Get)
  let feature = Feature("Recipe Management", "Test", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  list.contains(bead.labels, "recipe-management")
  |> should.be_true()
}

pub fn labels_include_http_method_test() {
  let behavior = make_test_behavior("create", [], Post)
  let feature = Feature("Test", "Test", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  list.contains(bead.labels, "POST")
  |> should.be_true()
}

pub fn labels_include_behavior_tags_test() {
  let behavior =
    make_test_behavior("test", ["indian", "chicken", "dinner"], Get)
  let feature = Feature("Test", "Test", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  list.contains(bead.labels, "indian")
  |> should.be_true()
  list.contains(bead.labels, "chicken")
  |> should.be_true()
  list.contains(bead.labels, "dinner")
  |> should.be_true()
}

pub fn labels_include_spec_generated_marker_test() {
  let behavior = make_test_behavior("test", [], Get)
  let feature = Feature("Test", "Test", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  list.contains(bead.labels, "spec-generated")
  |> should.be_true()
}

// ============================================================
// Multiple Features/Behaviors Tests
// ============================================================

pub fn multiple_behaviors_create_multiple_beads_test() {
  let b1 = make_test_behavior("create", [], Post)
  let b2 = make_test_behavior("read", [], Get)
  let b3 = make_test_behavior("update", [], types.Put)
  let feature = Feature("User", "User CRUD", [b1, b2, b3])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  list.length(beads)
  |> should.equal(3)
}

pub fn multiple_features_create_beads_from_all_test() {
  let f1 =
    Feature("Users", "User management", [
      make_test_behavior("create-user", [], Post),
    ])
  let f2 =
    Feature("Recipes", "Recipe management", [
      make_test_behavior("list-recipes", [], Get),
    ])
  let spec = minimal_spec([f1, f2])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  list.length(beads)
  |> should.equal(2)
}

pub fn empty_behaviors_in_feature_skipped_test() {
  let f1 = Feature("Empty", "No behaviors", [])
  let f2 =
    Feature("Real", "Has behaviors", [make_test_behavior("test", [], Get)])
  let spec = minimal_spec([f1, f2])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  // Only 1 bead from feature with behaviors
  list.length(beads)
  |> should.equal(1)
}

// ============================================================
// Unique ID Tests
// ============================================================

pub fn all_beads_have_unique_ids_test() {
  let behaviors =
    list.range(1, 10)
    |> list.map(fn(i) { make_test_behavior("b" <> int.to_string(i), [], Get) })
  let feature = Feature("Test", "Test", behaviors)
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let ids = list.map(beads, fn(b) { b.id })
  let unique_ids = list.unique(ids)

  list.length(ids)
  |> should.equal(list.length(unique_ids))
}

pub fn bead_id_contains_feature_and_behavior_slug_test() {
  let behavior = make_test_behavior("create-recipe", [], Post)
  let feature = Feature("Recipe Management", "Test", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads

  // ID should contain slugified feature and behavior
  // Format: bead-<feature>-<behavior>-<uuid>
  should.be_true(
    bead.id
    |> string_starts_with("bead-recipe-management-create-recipe-"),
  )
}

// Helper for string prefix check
fn string_starts_with(s: String, prefix: String) -> Bool {
  case s {
    _ if s == prefix -> True
    _ ->
      case prefix {
        "" -> True
        _ -> {
          let s_len =
            s
            |> gleam_stdlib_string_length()
          let p_len =
            prefix
            |> gleam_stdlib_string_length()
          case s_len >= p_len {
            True -> {
              let slice =
                s
                |> gleam_stdlib_string_slice(0, p_len)
              slice == prefix
            }
            False -> False
          }
        }
      }
  }
}

@external(erlang, "string", "length")
fn gleam_stdlib_string_length(s: String) -> Int

@external(erlang, "string", "slice")
fn gleam_stdlib_string_slice(s: String, start: Int, len: Int) -> String

// ============================================================
// Timestamp Tests
// ============================================================

pub fn bead_has_created_at_timestamp_test() {
  let behavior = make_test_behavior("test", [], Get)
  let feature = Feature("Test", "Test", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads

  // created_at should not be empty
  bead.created_at
  |> should.not_equal("")
}

pub fn bead_created_by_is_spec_to_beads_test() {
  let behavior = make_test_behavior("test", [], Get)
  let feature = Feature("Test", "Test", [behavior])
  let spec = minimal_spec([feature])

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  let assert [bead] = beads
  bead.created_by
  |> should.equal("spec_to_beads")
}

// ============================================================
// Integration with spec_builder Test
// ============================================================

pub fn works_with_spec_builder_created_spec_test() {
  let spec = spec_builder.create_test_spec(5)

  let beads =
    spec_to_beads.spec_to_beads(spec)
    |> should.be_ok()

  list.length(beads)
  |> should.equal(5)
}
