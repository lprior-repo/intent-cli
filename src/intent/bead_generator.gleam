//// Bead Generator Module
////
//// Transforms project structures from structure_planner into bd (beads)
//// issue tracker work items with proper hierarchy and dependencies.
////
//// This module generates:
//// 1. **Epic beads**: High-level feature groupings
//// 2. **Feature beads**: Cohesive sets of behaviors
//// 3. **Task beads**: Individual implementable units
////
//// Beads are created with:
//// - Proper parent-child relationships (epic → feature → task)
//// - Wave-based dependency chains
//// - KIRK contract metadata embedded in descriptions
//// - Railway-Oriented error handling throughout
////
//// ## Workflow
////
//// 1. Transform ProjectStructure to BeadRecord list
//// 2. Create epics first (no dependencies)
//// 3. Create features with epic dependencies
//// 4. Create tasks with feature dependencies
//// 5. Add inter-task dependencies based on contract preconditions
////
//// All bd commands execute with --json for structured output parsing.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import intent/bd_integration
import intent/bead_templates.{type BeadRecord, BeadRecord}
import intent/kirk_contract.{type KirkContract}
import intent/structure_planner.{
  type Epic, type Feature, type ProjectStructure, type Task,
}

/// Error types for bead generation operations
pub type BeadGenError {
  /// ProjectStructure is invalid or empty
  InvalidStructure(reason: String)
  /// Bead creation in bd database failed
  BeadCreationFailed(bead_title: String, error: bd_integration.BdError)
  /// Dependency creation failed
  DependencyFailed(from: String, to: String, reason: String)
  /// No beads were generated from structure
  NoBeadsGenerated
}

/// Result of generating beads from a project structure
pub type GenerationResult {
  GenerationResult(
    total_created: Int,
    epic_count: Int,
    feature_count: Int,
    task_count: Int,
    failed_beads: List(String),
    bead_ids: Dict(String, String),
  )
}

/// Dependency mapping for bd dep add commands
pub type DependencyMapping {
  DependencyMapping(from_id: String, to_id: String, reason: String)
}

/// Generate beads from project structure
///
/// This is the main entry point. It transforms the entire ProjectStructure
/// into bd beads with proper hierarchy and dependencies.
pub fn generate_beads(
  structure: ProjectStructure,
) -> Result(GenerationResult, BeadGenError) {
  // Validate structure
  use _ <- result.try(validate_structure(structure))

  // Transform structure to bead records
  let bead_records = structure_to_bead_records(structure)

  // Validate we generated beads
  use _ <- result.try(require_beads(bead_records))

  // Create beads in bd database
  let creation_results = bd_integration.create_beads(bead_records)

  // Analyze results
  let result = analyze_creation_results(creation_results, bead_records)

  Ok(result)
}

/// Validate project structure before generation
fn validate_structure(structure: ProjectStructure) -> Result(Nil, BeadGenError) {
  // Check for empty epics list
  case structure.epics {
    [] -> Error(InvalidStructure("No epics in project structure"))
    epics -> {
      // Check that at least one epic has features
      let has_features =
        list.any(epics, fn(epic) { list.length(epic.features) > 0 })

      case has_features {
        True -> Ok(Nil)
        False -> Error(InvalidStructure("No features found in any epic"))
      }
    }
  }
}

/// Require that beads were generated
fn require_beads(beads: List(BeadRecord)) -> Result(Nil, BeadGenError) {
  case beads {
    [] -> Error(NoBeadsGenerated)
    _ -> Ok(Nil)
  }
}

/// Transform ProjectStructure to list of BeadRecords
///
/// Creates beads in dependency order:
/// 1. Epic beads
/// 2. Feature beads (depend on epics)
/// 3. Task beads (depend on features)
fn structure_to_bead_records(structure: ProjectStructure) -> List(BeadRecord) {
  let epic_beads =
    list.flat_map(structure.epics, fn(epic) { epic_to_bead_records(epic) })

  epic_beads
}

/// Transform Epic to BeadRecords (epic + all features + all tasks)
fn epic_to_bead_records(epic: Epic) -> List(BeadRecord) {
  // Create epic bead
  let epic_bead = epic_to_bead(epic)

  // Create feature beads
  let feature_beads =
    list.flat_map(epic.features, fn(feature) {
      feature_to_bead_records(feature, epic.id)
    })

  // Epic bead comes first, then features
  [epic_bead, ..feature_beads]
}

/// Transform Feature to BeadRecords (feature + all tasks)
fn feature_to_bead_records(
  feature: Feature,
  epic_id: String,
) -> List(BeadRecord) {
  // Create feature bead
  let feature_bead = feature_to_bead(feature, epic_id)

  // Create task beads
  let task_beads = list.map(feature.tasks, fn(task) { task_to_bead(task) })

  // Feature bead comes first, then tasks
  [feature_bead, ..task_beads]
}

/// Transform Epic to BeadRecord
fn epic_to_bead(epic: Epic) -> BeadRecord {
  let description =
    epic.description
    <> "\n\nEstimated Waves: "
    <> int.to_string(epic.estimated_waves)
    <> "\nFeatures: "
    <> int.to_string(list.length(epic.features))

  BeadRecord(
    title: epic.name,
    description: description,
    profile_type: "planning",
    priority: 1,
    // Epics are high priority
    issue_type: "epic",
    labels: ["generated", "planning", "epic"],
    ai_hints: "This epic groups related features. Review structure_planner output for context.",
    acceptance_criteria: [
      "All features completed", "All tasks within features completed",
      "Wave dependencies satisfied",
    ],
    dependencies: [],
  )
}

/// Transform Feature to BeadRecord
fn feature_to_bead(feature: Feature, epic_id: String) -> BeadRecord {
  let description =
    feature.description
    <> "\n\nEpic: "
    <> epic_id
    <> "\nTasks: "
    <> int.to_string(list.length(feature.tasks))

  BeadRecord(
    title: feature.name,
    description: description,
    profile_type: "planning",
    priority: 2,
    // Features are medium-high priority
    issue_type: "feature",
    labels: ["generated", "planning", "feature"],
    ai_hints: "This feature groups related tasks. Review KIRK contracts for implementation details.",
    acceptance_criteria: [
      "All tasks completed", "Feature dependencies satisfied",
      "Contracts validated",
    ],
    dependencies: feature.dependencies,
  )
}

/// Transform Task to BeadRecord
fn task_to_bead(task: Task) -> BeadRecord {
  let contract = task.contract

  // Build description from task and contract
  let description =
    task.description
    <> "\n\n## KIRK Contract\n\n"
    <> "Requirement: "
    <> contract.requirement.raw_text
    <> "\n\n### Preconditions\n"
    <> format_preconditions(contract)
    <> "\n\n### Postconditions\n"
    <> format_postconditions(contract)
    <> "\n\n### Second-Order Effects\n"
    <> format_second_order_effects(contract)

  // Extract AI hints from contract
  let ai_hints =
    "Contract Confidence: "
    <> float_to_percent(contract.confidence)
    <> "\nWave: "
    <> int.to_string(task.wave)
    <> "\n\nImplementation hints: Review preconditions and postconditions carefully."

  // Build acceptance criteria from postconditions
  let acceptance_criteria =
    [
      "All preconditions validated", "All postconditions satisfied",
      "Second-order effects documented",
    ]
    |> list.append(extract_acceptance_from_postconditions(contract))

  BeadRecord(
    title: task.name,
    description: description,
    profile_type: "implementation",
    priority: calculate_task_priority(task),
    issue_type: "task",
    labels: ["generated", "implementation", "wave-" <> int.to_string(task.wave)],
    ai_hints: ai_hints,
    acceptance_criteria: acceptance_criteria,
    dependencies: task.dependencies,
  )
}

/// Format preconditions for description
fn format_preconditions(contract: KirkContract) -> String {
  let auth_line = case contract.preconditions.auth_required {
    True -> "- Authentication required\n"
    False -> "- No authentication required\n"
  }

  let fields_line = case contract.preconditions.required_fields {
    [] -> ""
    fields -> "- Required fields: " <> string.join(fields, ", ") <> "\n"
  }

  let constraints_lines =
    contract.preconditions.field_constraints
    |> list.map(fn(c) { "- " <> c.field <> ": " <> c.constraint })
    |> string.join("\n")

  auth_line <> fields_line <> constraints_lines
}

/// Format postconditions for description
fn format_postconditions(contract: KirkContract) -> String {
  let state_changes = case contract.postconditions.state_changes {
    [] -> "- No state changes\n"
    changes ->
      changes
      |> list.map(fn(change) { "- " <> change })
      |> string.join("\n")
  }

  let guarantees =
    contract.postconditions.response_guarantees
    |> list.map(fn(g) { "- " <> g.aspect <> ": " <> g.guarantee })
    |> string.join("\n")

  case guarantees {
    "" -> state_changes
    _ -> state_changes <> "\n" <> guarantees
  }
}

/// Format second-order effects for description
fn format_second_order_effects(contract: KirkContract) -> String {
  case contract.second_order_effects {
    [] -> "None identified"
    effects ->
      effects
      |> list.map(fn(effect) { "- " <> effect })
      |> string.join("\n")
  }
}

/// Extract acceptance criteria from postconditions
fn extract_acceptance_from_postconditions(
  contract: KirkContract,
) -> List(String) {
  let state_criteria =
    contract.postconditions.state_changes
    |> list.map(fn(change) { "State change: " <> change })

  let guarantee_criteria =
    contract.postconditions.response_guarantees
    |> list.map(fn(g) { g.aspect <> ": " <> g.guarantee })

  list.append(state_criteria, guarantee_criteria)
}

/// Calculate task priority based on wave and dependencies
fn calculate_task_priority(task: Task) -> Int {
  case task.wave {
    1 -> 3
    // Wave 1 tasks are high priority (no blockers)
    2 -> 2
    // Wave 2 tasks are medium priority
    _ -> 1
    // Later wave tasks are lower priority
  }
}

/// Convert float to percentage string
fn float_to_percent(value: Float) -> String {
  let percent = value *. 100.0
  case percent {
    p if p >=. 100.0 -> "100%"
    p if p <=. 0.0 -> "0%"
    p -> {
      let truncated = float.truncate(p)
      int.to_string(truncated) <> "%"
    }
  }
}

/// Analyze creation results and build GenerationResult
fn analyze_creation_results(
  results: List(
    Result(bd_integration.BeadCreationResult, bd_integration.BdError),
  ),
  bead_records: List(BeadRecord),
) -> GenerationResult {
  let total_created =
    results
    |> list.filter(fn(r) { result.is_ok(r) })
    |> list.length

  let failed_beads =
    results
    |> list.zip(bead_records)
    |> list.filter_map(fn(pair) {
      let #(result, bead) = pair
      case result {
        Error(_) -> Ok(bead.title)
        Ok(_) -> Error(Nil)
      }
    })

  // Count by type
  let epics =
    bead_records
    |> list.filter(fn(b) { b.issue_type == "epic" })
    |> list.length

  let features =
    bead_records
    |> list.filter(fn(b) { b.issue_type == "feature" })
    |> list.length

  let tasks =
    bead_records
    |> list.filter(fn(b) { b.issue_type == "task" })
    |> list.length

  GenerationResult(
    total_created: total_created,
    epic_count: epics,
    feature_count: features,
    task_count: tasks,
    failed_beads: failed_beads,
    bead_ids: dict.new(),
  )
}

/// Format GenerationResult as human-readable text
pub fn format_result(result: GenerationResult) -> String {
  let lines = [
    "Bead Generation Complete",
    "═══════════════════════════════════════════════════════════════════",
    "",
    "Summary:",
    "  Total Beads Created: " <> int.to_string(result.total_created),
    "  Epics: " <> int.to_string(result.epic_count),
    "  Features: " <> int.to_string(result.feature_count),
    "  Tasks: " <> int.to_string(result.task_count),
    "",
  ]

  let failure_lines = case result.failed_beads {
    [] -> ["All beads created successfully!", ""]
    failures -> {
      [
        "Failed Beads (" <> int.to_string(list.length(failures)) <> "):",
        ..list.map(failures, fn(title) { "  - " <> title })
      ]
      |> list.append([""])
    }
  }

  let next_steps = [
    "Next Steps:", "  1. Review created beads: bd list --status=open",
    "  2. Add any custom dependencies: bd dep add <from> <to>",
    "  3. Start working: bd ready", "",
  ]

  string.join(list.flatten([lines, failure_lines, next_steps]), "\n")
}

/// Describe a BeadGenError in human-readable format
pub fn describe_error(error: BeadGenError) -> String {
  case error {
    InvalidStructure(reason) -> "Invalid project structure: " <> reason

    BeadCreationFailed(title, bd_error) ->
      "Failed to create bead '"
      <> title
      <> "': "
      <> bd_integration.describe_error(bd_error)

    DependencyFailed(from, to, reason) ->
      "Failed to add dependency from '"
      <> from
      <> "' to '"
      <> to
      <> "': "
      <> reason

    NoBeadsGenerated -> "No beads were generated from project structure"
  }
}
