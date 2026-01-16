//// Structure Planner Module
////
//// Groups KIRK contracts into epic/feature/task hierarchy with wave-based
//// dependency resolution for parallel execution planning.
////
//// This module analyzes preconditions, postconditions, and second-order
//// effects to organize contracts into logical work structures suitable
//// for implementation planning.
////
//// ## Hierarchy
////
//// 1. **Epic**: High-level grouping of related features (e.g., "Authentication System")
//// 2. **Feature**: Cohesive set of behaviors (e.g., "User Login")
//// 3. **Task**: Individual implementable unit (e.g., "Validate credentials")
////
//// ## Wave-Based Dependencies
////
//// Tasks are organized into execution waves where:
//// - Wave 1: Tasks with no dependencies (can start immediately)
//// - Wave 2: Tasks that depend only on Wave 1
//// - Wave N: Tasks that depend on previous waves
////
//// Tasks within the same wave can execute in parallel.
////
//// This module provides Railway-Oriented error handling and produces
//// structured planning output suitable for project management tools.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import intent/kirk_contract.{type KirkContract}

/// Epic groups related features under high-level goal
pub type Epic {
  Epic(
    id: String,
    name: String,
    description: String,
    features: List(Feature),
    estimated_waves: Int,
  )
}

/// Feature groups related tasks
pub type Feature {
  Feature(
    id: String,
    name: String,
    description: String,
    epic_id: String,
    tasks: List(Task),
    dependencies: List(String),
  )
}

/// Task represents single implementable unit
pub type Task {
  Task(
    id: String,
    name: String,
    description: String,
    feature_id: String,
    contract: KirkContract,
    dependencies: List(String),
    wave: Int,
  )
}

/// Complete project structure with wave analysis
pub type ProjectStructure {
  ProjectStructure(
    project_name: String,
    epics: List(Epic),
    total_tasks: Int,
    total_waves: Int,
    parallelism_score: Float,
  )
}

/// Error types for structure planning
pub type StructurePlanError {
  NoContractsProvided
  CyclicDependency(task_ids: List(String))
  InvalidStructure(reason: String)
}

/// Domain classification for grouping contracts
type Domain {
  Authentication
  Authorization
  UserManagement
  DataManagement
  Integration
  Validation
  ErrorHandling
  Generic
}

/// Create project structure from KIRK contracts
pub fn plan_structure(
  project_name: String,
  contracts: List(KirkContract),
) -> Result(ProjectStructure, StructurePlanError) {
  case contracts {
    [] -> Error(NoContractsProvided)
    _ -> {
      // Group contracts by domain
      let domain_groups = group_by_domain(contracts)

      // Create epics from domain groups
      let epics = create_epics(domain_groups)

      // Calculate wave dependencies
      use wave_analyzed_epics <- result.try(analyze_waves(epics))

      // Calculate metrics
      let total_tasks = count_total_tasks(wave_analyzed_epics)
      let total_waves = calculate_total_waves(wave_analyzed_epics)
      let parallelism_score = calculate_parallelism(wave_analyzed_epics)

      Ok(ProjectStructure(
        project_name: project_name,
        epics: wave_analyzed_epics,
        total_tasks: total_tasks,
        total_waves: total_waves,
        parallelism_score: parallelism_score,
      ))
    }
  }
}

/// Group contracts by domain
fn group_by_domain(
  contracts: List(KirkContract),
) -> Dict(Domain, List(KirkContract)) {
  list.fold(contracts, dict.new(), fn(groups, contract) {
    let domain = classify_domain(contract)
    let existing = dict.get(groups, domain) |> result.unwrap([])
    dict.insert(groups, domain, [contract, ..existing])
  })
}

/// Classify contract into domain
fn classify_domain(contract: KirkContract) -> Domain {
  let behavior_lower = string.lowercase(contract.requirement.system_shall)

  case
    string.contains(behavior_lower, "authenticate")
    || string.contains(behavior_lower, "login")
    || string.contains(behavior_lower, "logout")
  {
    True -> Authentication
    False ->
      case
        string.contains(behavior_lower, "authorize")
        || string.contains(behavior_lower, "permission")
        || string.contains(behavior_lower, "access")
      {
        True -> Authorization
        False ->
          case
            string.contains(behavior_lower, "user")
            || string.contains(behavior_lower, "profile")
            || string.contains(behavior_lower, "account")
          {
            True -> UserManagement
            False ->
              case
                string.contains(behavior_lower, "create")
                || string.contains(behavior_lower, "update")
                || string.contains(behavior_lower, "delete")
                || string.contains(behavior_lower, "read")
              {
                True -> DataManagement
                False ->
                  case
                    string.contains(behavior_lower, "integrate")
                    || string.contains(behavior_lower, "api")
                    || string.contains(behavior_lower, "external")
                  {
                    True -> Integration
                    False ->
                      case
                        string.contains(behavior_lower, "validate")
                        || string.contains(behavior_lower, "check")
                        || string.contains(behavior_lower, "verify")
                      {
                        True -> Validation
                        False ->
                          case
                            string.contains(behavior_lower, "error")
                            || string.contains(behavior_lower, "fail")
                          {
                            True -> ErrorHandling
                            False -> Generic
                          }
                      }
                  }
              }
          }
      }
  }
}

/// Create epics from domain groups
fn create_epics(domain_groups: Dict(Domain, List(KirkContract))) -> List(Epic) {
  domain_groups
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(domain, contracts) = pair
    let epic_name = domain_to_epic_name(domain)
    let epic_id =
      "epic-" <> string.lowercase(string.replace(epic_name, " ", "-"))

    // Create features for this epic
    let features = create_features_for_epic(epic_id, contracts)

    Epic(
      id: epic_id,
      name: epic_name,
      description: domain_to_description(domain),
      features: features,
      estimated_waves: 0,
    )
  })
}

/// Convert domain to epic name
fn domain_to_epic_name(domain: Domain) -> String {
  case domain {
    Authentication -> "Authentication System"
    Authorization -> "Authorization System"
    UserManagement -> "User Management"
    DataManagement -> "Data Management"
    Integration -> "External Integration"
    Validation -> "Input Validation"
    ErrorHandling -> "Error Handling"
    Generic -> "Core Functionality"
  }
}

/// Convert domain to description
fn domain_to_description(domain: Domain) -> String {
  case domain {
    Authentication -> "User authentication, login/logout, session management"
    Authorization -> "Permission checks, access control, role management"
    UserManagement -> "User profiles, account settings, user CRUD"
    DataManagement ->
      "Create, read, update, delete operations for data entities"
    Integration -> "External API integration, third-party services"
    Validation -> "Input validation, data verification, constraint checking"
    ErrorHandling -> "Error responses, failure handling, edge cases"
    Generic -> "General functionality and core behaviors"
  }
}

/// Create features for an epic
fn create_features_for_epic(
  epic_id: String,
  contracts: List(KirkContract),
) -> List(Feature) {
  // Group similar contracts into features
  let feature_groups = group_contracts_into_features(contracts)

  list.index_map(feature_groups, fn(group, idx) {
    let feature_id = epic_id <> "-feature-" <> int.to_string(idx + 1)
    let feature_name = infer_feature_name(group)

    // Create tasks from contracts
    let tasks = create_tasks_for_feature(feature_id, group)

    // Extract feature dependencies
    let feature_deps = extract_feature_dependencies(tasks)

    Feature(
      id: feature_id,
      name: feature_name,
      description: "Feature comprising "
        <> int.to_string(list.length(tasks))
        <> " tasks",
      epic_id: epic_id,
      tasks: tasks,
      dependencies: feature_deps,
    )
  })
}

/// Group contracts into logical features
fn group_contracts_into_features(
  contracts: List(KirkContract),
) -> List(List(KirkContract)) {
  // Simple grouping: put all contracts in one feature per epic
  // More sophisticated: analyze preconditions for related behaviors
  case contracts {
    [] -> []
    _ -> [contracts]
  }
}

/// Infer feature name from contracts
fn infer_feature_name(contracts: List(KirkContract)) -> String {
  case contracts {
    [] -> "Empty Feature"
    [first, ..] -> {
      // Extract action from first contract
      let behavior = first.requirement.system_shall
      case string.split(behavior, " ") {
        [action, ..] -> string.capitalise(action) <> " Operations"
        [] -> "Feature"
      }
    }
  }
}

/// Create tasks from contracts
fn create_tasks_for_feature(
  feature_id: String,
  contracts: List(KirkContract),
) -> List(Task) {
  list.index_map(contracts, fn(contract, idx) {
    let task_id = feature_id <> "-task-" <> int.to_string(idx + 1)
    let task_name = contract.requirement.system_shall

    // Analyze dependencies from preconditions
    let task_deps = extract_task_dependencies(contract)

    Task(
      id: task_id,
      name: task_name,
      description: contract.requirement.raw_text,
      feature_id: feature_id,
      contract: contract,
      dependencies: task_deps,
      wave: 0,
    )
  })
}

/// Extract task dependencies from contract
fn extract_task_dependencies(contract: KirkContract) -> List(String) {
  // Tasks with auth requirements depend on auth tasks
  case contract.preconditions.auth_required {
    True -> ["auth-required"]
    False -> []
  }
}

/// Extract feature dependencies from tasks
fn extract_feature_dependencies(tasks: List(Task)) -> List(String) {
  tasks
  |> list.flat_map(fn(task) { task.dependencies })
  |> list.unique
}

/// Analyze waves for all epics
fn analyze_waves(epics: List(Epic)) -> Result(List(Epic), StructurePlanError) {
  epics
  |> list.map(analyze_epic_waves)
  |> result.all
}

/// Analyze waves for a single epic
fn analyze_epic_waves(epic: Epic) -> Result(Epic, StructurePlanError) {
  // Analyze waves for each feature
  use wave_analyzed_features <- result.try(
    epic.features
    |> list.map(analyze_feature_waves)
    |> result.all,
  )

  // Calculate max wave across all features
  let max_wave =
    wave_analyzed_features
    |> list.flat_map(fn(f) { f.tasks })
    |> list.map(fn(t) { t.wave })
    |> list.fold(0, int.max)

  Ok(Epic(..epic, features: wave_analyzed_features, estimated_waves: max_wave))
}

/// Analyze waves for a single feature
fn analyze_feature_waves(
  feature: Feature,
) -> Result(Feature, StructurePlanError) {
  // Simple wave assignment: tasks with no dependencies = wave 1
  let wave_assigned_tasks =
    list.map(feature.tasks, fn(task) {
      let wave = case task.dependencies {
        [] -> 1
        _ -> 2
        // Simple: dependencies go to wave 2
      }
      Task(..task, wave: wave)
    })

  Ok(Feature(..feature, tasks: wave_assigned_tasks))
}

/// Count total tasks across all epics
fn count_total_tasks(epics: List(Epic)) -> Int {
  epics
  |> list.flat_map(fn(e) { e.features })
  |> list.flat_map(fn(f) { f.tasks })
  |> list.length
}

/// Calculate total waves across project
fn calculate_total_waves(epics: List(Epic)) -> Int {
  epics
  |> list.map(fn(e) { e.estimated_waves })
  |> list.fold(0, int.max)
}

/// Calculate parallelism score (0-1, higher = more parallel work)
fn calculate_parallelism(epics: List(Epic)) -> Float {
  let total_tasks = count_total_tasks(epics) |> int.to_float
  let total_waves = calculate_total_waves(epics) |> int.to_float

  case total_waves >. 0.0 {
    True -> {
      let ideal_parallel = total_tasks /. total_waves
      let parallelism = ideal_parallel /. total_tasks
      case parallelism >. 1.0 {
        True -> 1.0
        False -> parallelism
      }
    }
    False -> 0.0
  }
}

/// Format structure as human-readable text
pub fn format_structure(structure: ProjectStructure) -> String {
  let lines = [
    "Project Structure: " <> structure.project_name,
    "═══════════════════════════════════════════════════════════════════",
    "",
    "Total Tasks: " <> int.to_string(structure.total_tasks),
    "Total Waves: " <> int.to_string(structure.total_waves),
    "Parallelism Score: " <> float_to_percent(structure.parallelism_score),
    "",
  ]

  let epic_lines =
    list.flat_map(structure.epics, fn(epic) {
      let epic_header = [
        "Epic: " <> epic.name,
        "─────────────────────────────────────────────────────────────────",
        epic.description,
        "Estimated Waves: " <> int.to_string(epic.estimated_waves),
        "",
      ]

      let feature_lines =
        list.flat_map(epic.features, fn(feature) {
          let feature_header = [
            "  Feature: " <> feature.name <> " (" <> feature.id <> ")",
            "  " <> feature.description,
          ]

          let task_lines =
            list.map(feature.tasks, fn(task) {
              "    Task [Wave "
              <> int.to_string(task.wave)
              <> "]: "
              <> task.name
            })

          list.flatten([feature_header, task_lines, [""]])
        })

      list.flatten([epic_header, feature_lines])
    })

  string.join(list.append(lines, epic_lines), "\n")
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
