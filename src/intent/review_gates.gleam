//// Review Gates Module
////
//// Provides interactive review and approval checkpoints throughout the
//// planning pipeline. Users can review, approve, reject, or edit artifacts
//// at each stage before proceeding to the next step.
////
//// ## Review Workflow
////
//// 1. **Display**: Show artifact with clear formatting
//// 2. **Review**: Present options (approve/reject/edit/skip)
//// 3. **Action**: Execute user's choice
//// 4. **Checkpoint**: Save state for resumption
////
//// ## Checkpoint System
////
//// Checkpoints are saved to `.intent/checkpoints/` directory:
//// - `requirements.json`: EARS requirements checkpoint
//// - `contracts.json`: KIRK contracts checkpoint
//// - `structure.json`: Project structure checkpoint
//// - `beads.json`: Generated beads checkpoint
////
//// This enables users to:
//// - Stop and resume the pipeline at any stage
//// - Iterate on specific stages without re-running entire pipeline
//// - Track progress through multi-session work
////
//// All review operations use Railway-Oriented error handling.

import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import intent/bead_generator.{type GenerationResult}
import intent/kirk/ears_parser.{type EarsRequirement}
import intent/kirk_contract.{type KirkContract}
import intent/structure_planner.{type ProjectStructure}
import simplifile

/// User's decision after reviewing an artifact
pub type ReviewDecision {
  /// User approves, proceed to next stage
  Approved
  /// User rejects, stop pipeline
  Rejected(reason: String)
  /// User wants to edit, provide feedback
  NeedsEdit(feedback: String)
  /// Skip review, proceed automatically
  Skipped
}

/// Checkpoint type identifying pipeline stage
pub type CheckpointType {
  RequirementsCheckpoint
  ContractsCheckpoint
  StructureCheckpoint
  BeadsCheckpoint
}

/// Complete checkpoint with data and metadata
pub type Checkpoint {
  Checkpoint(
    checkpoint_type: CheckpointType,
    timestamp: String,
    artifact_count: Int,
    notes: String,
  )
}

/// Error types for review gate operations
pub type ReviewGateError {
  /// Failed to save checkpoint
  CheckpointSaveFailed(checkpoint_type: CheckpointType, reason: String)
  /// Failed to load checkpoint
  CheckpointLoadFailed(checkpoint_type: CheckpointType, reason: String)
  /// Invalid review decision
  InvalidDecision(reason: String)
  /// Checkpoint directory error
  CheckpointDirError(reason: String)
}

/// Directory for storing checkpoints
const checkpoint_dir = ".intent/checkpoints"

/// Review EARS requirements
///
/// Displays requirements with pattern breakdown and asks for approval.
pub fn review_requirements(requirements: List(EarsRequirement)) -> String {
  let header = [
    "Review EARS Requirements",
    "═══════════════════════════════════════════════════════════════════",
    "",
    "Total Requirements: " <> int.to_string(list.length(requirements)),
    "",
  ]

  let requirement_lines =
    requirements
    |> list.index_map(fn(req, idx) {
      let num = int.to_string(idx + 1)
      [
        "Requirement " <> num <> ":",
        "  Pattern: " <> pattern_to_string(req.pattern),
        "  System Shall: " <> req.system_shall,
        "  Raw Text: " <> req.raw_text,
        "",
      ]
    })
    |> list.flatten

  let footer = [
    "Review Questions:", "  • Are all requirements clear and specific?",
    "  • Is the appropriate EARS pattern used for each?",
    "  • Are there missing requirements?",
    "  • Do requirements avoid vague terms?", "",
  ]

  string.join(list.flatten([header, requirement_lines, footer]), "\n")
}

/// Review KIRK contracts
///
/// Displays contracts with preconditions, postconditions, and confidence.
pub fn review_contracts(contracts: List(KirkContract)) -> String {
  let header = [
    "Review KIRK Contracts",
    "═══════════════════════════════════════════════════════════════════",
    "",
    "Total Contracts: " <> int.to_string(list.length(contracts)),
    "",
  ]

  let contract_lines =
    contracts
    |> list.index_map(fn(contract, idx) {
      let num = int.to_string(idx + 1)
      let confidence = float_to_percent(contract.confidence)

      [
        "Contract " <> num <> " (Confidence: " <> confidence <> "):",
        "  Requirement: " <> contract.requirement.system_shall,
        "  Preconditions:",
        "    - Auth Required: "
          <> bool_to_string(contract.preconditions.auth_required),
        "    - Required Fields: "
          <> int.to_string(list.length(contract.preconditions.required_fields)),
        "    - Constraints: "
          <> int.to_string(list.length(contract.preconditions.field_constraints)),
        "  Postconditions:",
        "    - State Changes: "
          <> int.to_string(list.length(contract.postconditions.state_changes)),
        "    - Response Guarantees: "
          <> int.to_string(list.length(
          contract.postconditions.response_guarantees,
        )),
        "  Second-Order Effects: "
          <> int.to_string(list.length(contract.second_order_effects)),
        "",
      ]
    })
    |> list.flatten

  let footer = [
    "Review Questions:",
    "  • Do preconditions capture all necessary input requirements?",
    "  • Are postconditions complete and testable?",
    "  • Are second-order effects identified?",
    "  • Is contract confidence acceptable?", "",
  ]

  string.join(list.flatten([header, contract_lines, footer]), "\n")
}

/// Review project structure
///
/// Displays epic/feature/task hierarchy with wave analysis.
pub fn review_structure(structure: ProjectStructure) -> String {
  let header = [
    "Review Project Structure",
    "═══════════════════════════════════════════════════════════════════",
    "",
    "Project: " <> structure.project_name,
    "Total Tasks: " <> int.to_string(structure.total_tasks),
    "Total Waves: " <> int.to_string(structure.total_waves),
    "Parallelism Score: " <> float_to_percent(structure.parallelism_score),
    "",
  ]

  let epic_lines =
    structure.epics
    |> list.map(fn(epic) {
      let feature_count = list.length(epic.features)
      let task_count =
        epic.features
        |> list.flat_map(fn(f) { f.tasks })
        |> list.length

      [
        "Epic: " <> epic.name,
        "  Description: " <> epic.description,
        "  Features: " <> int.to_string(feature_count),
        "  Tasks: " <> int.to_string(task_count),
        "  Estimated Waves: " <> int.to_string(epic.estimated_waves),
        "",
      ]
    })
    |> list.flatten

  let footer = [
    "Review Questions:", "  • Is the epic/feature/task hierarchy logical?",
    "  • Are tasks properly sized (not too large/small)?",
    "  • Is wave-based dependency order correct?",
    "  • Is parallelism score acceptable for your team?", "",
  ]

  string.join(list.flatten([header, epic_lines, footer]), "\n")
}

/// Review bead generation result
///
/// Displays summary of created beads with success/failure counts.
pub fn review_beads(result: GenerationResult) -> String {
  let header = [
    "Review Generated Beads",
    "═══════════════════════════════════════════════════════════════════",
    "",
    "Total Beads Created: " <> int.to_string(result.total_created),
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

  let footer = [
    "Review Questions:", "  • Were all expected beads created?",
    "  • Should failed beads be retried?",
    "  • Are bead relationships (epic → feature → task) correct?",
    "  • Are priorities and wave numbers appropriate?", "", "Next Steps:",
    "  • Review beads in bd: bd list --status=open",
    "  • Check dependencies: bd show <id>", "  • Start work: bd ready", "",
  ]

  string.join(list.flatten([header, failure_lines, footer]), "\n")
}

/// Save checkpoint for requirements
pub fn save_requirements_checkpoint(
  requirements: List(EarsRequirement),
  notes: String,
) -> Result(Nil, ReviewGateError) {
  use _ <- result.try(ensure_checkpoint_dir())

  let checkpoint =
    Checkpoint(
      checkpoint_type: RequirementsCheckpoint,
      timestamp: "now",
      // TODO: Add proper timestamp
      artifact_count: list.length(requirements),
      notes: notes,
    )

  save_checkpoint(checkpoint, "requirements.json")
}

/// Save checkpoint for contracts
pub fn save_contracts_checkpoint(
  contracts: List(KirkContract),
  notes: String,
) -> Result(Nil, ReviewGateError) {
  use _ <- result.try(ensure_checkpoint_dir())

  let checkpoint =
    Checkpoint(
      checkpoint_type: ContractsCheckpoint,
      timestamp: "now",
      artifact_count: list.length(contracts),
      notes: notes,
    )

  save_checkpoint(checkpoint, "contracts.json")
}

/// Save checkpoint for structure
pub fn save_structure_checkpoint(
  structure: ProjectStructure,
  notes: String,
) -> Result(Nil, ReviewGateError) {
  use _ <- result.try(ensure_checkpoint_dir())

  let checkpoint =
    Checkpoint(
      checkpoint_type: StructureCheckpoint,
      timestamp: "now",
      artifact_count: structure.total_tasks,
      notes: notes,
    )

  save_checkpoint(checkpoint, "structure.json")
}

/// Save checkpoint for beads
pub fn save_beads_checkpoint(
  result: GenerationResult,
  notes: String,
) -> Result(Nil, ReviewGateError) {
  use _ <- result.try(ensure_checkpoint_dir())

  let checkpoint =
    Checkpoint(
      checkpoint_type: BeadsCheckpoint,
      timestamp: "now",
      artifact_count: result.total_created,
      notes: notes,
    )

  save_checkpoint(checkpoint, "beads.json")
}

/// Ensure checkpoint directory exists
fn ensure_checkpoint_dir() -> Result(Nil, ReviewGateError) {
  case simplifile.create_directory_all(checkpoint_dir) {
    Ok(_) -> Ok(Nil)
    Error(_) ->
      Error(CheckpointDirError("Failed to create checkpoint directory"))
  }
}

/// Save checkpoint to file
fn save_checkpoint(
  checkpoint: Checkpoint,
  filename: String,
) -> Result(Nil, ReviewGateError) {
  let path = checkpoint_dir <> "/" <> filename

  // Build JSON manually (simplified)
  let json_content =
    "{\n"
    <> "  \"checkpoint_type\": \""
    <> checkpoint_type_to_string(checkpoint.checkpoint_type)
    <> "\",\n"
    <> "  \"timestamp\": \""
    <> checkpoint.timestamp
    <> "\",\n"
    <> "  \"artifact_count\": "
    <> int.to_string(checkpoint.artifact_count)
    <> ",\n"
    <> "  \"notes\": \""
    <> checkpoint.notes
    <> "\"\n"
    <> "}\n"

  case simplifile.write(path, json_content) {
    Ok(_) -> Ok(Nil)
    Error(_) ->
      Error(CheckpointSaveFailed(checkpoint.checkpoint_type, "Write failed"))
  }
}

/// Convert checkpoint type to string
fn checkpoint_type_to_string(checkpoint_type: CheckpointType) -> String {
  case checkpoint_type {
    RequirementsCheckpoint -> "requirements"
    ContractsCheckpoint -> "contracts"
    StructureCheckpoint -> "structure"
    BeadsCheckpoint -> "beads"
  }
}

/// Convert EARS pattern to string
fn pattern_to_string(pattern: ears_parser.EarsPattern) -> String {
  case pattern {
    ears_parser.Ubiquitous -> "Ubiquitous"
    ears_parser.EventDriven -> "Event-Driven"
    ears_parser.StateDriven -> "State-Driven"
    ears_parser.Optional -> "Optional"
    ears_parser.Unwanted -> "Unwanted"
    ears_parser.Complex -> "Complex"
  }
}

/// Convert boolean to string
fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "Yes"
    False -> "No"
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

/// Describe a ReviewGateError in human-readable format
pub fn describe_error(error: ReviewGateError) -> String {
  case error {
    CheckpointSaveFailed(checkpoint_type, reason) ->
      "Failed to save "
      <> checkpoint_type_to_string(checkpoint_type)
      <> " checkpoint: "
      <> reason

    CheckpointLoadFailed(checkpoint_type, reason) ->
      "Failed to load "
      <> checkpoint_type_to_string(checkpoint_type)
      <> " checkpoint: "
      <> reason

    InvalidDecision(reason) -> "Invalid review decision: " <> reason

    CheckpointDirError(reason) -> "Checkpoint directory error: " <> reason
  }
}

/// Format review decision for display
pub fn format_decision(decision: ReviewDecision) -> String {
  case decision {
    Approved -> "✓ Approved - Proceeding to next stage"
    Rejected(reason) -> "✗ Rejected - " <> reason
    NeedsEdit(feedback) -> "✎ Needs Edit - " <> feedback
    Skipped -> "⊳ Skipped - Automatic approval"
  }
}
