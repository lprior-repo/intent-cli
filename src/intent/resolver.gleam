/// Behavior dependency resolver
/// Topologically sorts behaviors based on their `requires` dependencies
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import intent/types.{type Behavior, type Spec}

/// A resolved behavior with its feature context
pub type ResolvedBehavior {
  ResolvedBehavior(feature_name: String, behavior: Behavior)
}

/// Error types for resolution
pub type ResolveError {
  CyclicDependency(behaviors: List(String))
  MissingDependency(behavior: String, missing: String)
  DuplicateBehaviorName(name: String)
}

/// Resolve all behaviors in a spec into execution order
pub fn resolve_execution_order(
  spec: Spec,
) -> Result(List(ResolvedBehavior), ResolveError) {
  // First, collect all behaviors with their feature context
  let all_behaviors = collect_all_behaviors(spec)

  // Check for duplicate names
  use _ <- result.try(check_duplicates(all_behaviors))

  // Build dependency graph
  use graph <- result.try(build_dependency_graph(all_behaviors))

  // Topological sort
  topological_sort(all_behaviors, graph)
}

/// Filter behaviors by a specific feature name
pub fn filter_by_feature(
  behaviors: List(ResolvedBehavior),
  feature_name: String,
) -> List(ResolvedBehavior) {
  list.filter(behaviors, fn(rb) { rb.feature_name == feature_name })
}

/// Filter behaviors by a specific behavior name
pub fn filter_by_name(
  behaviors: List(ResolvedBehavior),
  name: String,
) -> List(ResolvedBehavior) {
  list.filter(behaviors, fn(rb) { rb.behavior.name == name })
}

/// Get behaviors that depend on a given behavior
pub fn get_dependents(
  behaviors: List(ResolvedBehavior),
  name: String,
) -> List(ResolvedBehavior) {
  list.filter(behaviors, fn(rb) { list.contains(rb.behavior.requires, name) })
}

fn collect_all_behaviors(spec: Spec) -> List(ResolvedBehavior) {
  spec.features
  |> list.flat_map(fn(feature) {
    feature.behaviors
    |> list.map(fn(behavior) {
      ResolvedBehavior(feature_name: feature.name, behavior: behavior)
    })
  })
}

fn check_duplicates(
  behaviors: List(ResolvedBehavior),
) -> Result(Nil, ResolveError) {
  let names = list.map(behaviors, fn(rb) { rb.behavior.name })
  check_duplicates_loop(names, set.new())
}

fn check_duplicates_loop(
  names: List(String),
  seen: Set(String),
) -> Result(Nil, ResolveError) {
  case names {
    [] -> Ok(Nil)
    [name, ..rest] ->
      case set.contains(seen, name) {
        True -> Error(DuplicateBehaviorName(name))
        False -> check_duplicates_loop(rest, set.insert(seen, name))
      }
  }
}

fn build_dependency_graph(
  behaviors: List(ResolvedBehavior),
) -> Result(Dict(String, List(String)), ResolveError) {
  let behavior_names =
    behaviors
    |> list.map(fn(rb) { rb.behavior.name })
    |> set.from_list

  behaviors
  |> list.try_fold(dict.new(), fn(graph, rb) {
    // Verify all dependencies exist
    use _ <- result.try(
      list.try_each(rb.behavior.requires, fn(dep) {
        case set.contains(behavior_names, dep) {
          True -> Ok(Nil)
          False -> Error(MissingDependency(rb.behavior.name, dep))
        }
      }),
    )

    Ok(dict.insert(graph, rb.behavior.name, rb.behavior.requires))
  })
}

/// Topological sort using Kahn's algorithm
fn topological_sort(
  behaviors: List(ResolvedBehavior),
  graph: Dict(String, List(String)),
) -> Result(List(ResolvedBehavior), ResolveError) {
  // Build a map from name to ResolvedBehavior
  let by_name =
    behaviors
    |> list.map(fn(rb) { #(rb.behavior.name, rb) })
    |> dict.from_list

  // Calculate in-degrees
  let in_degrees = calculate_in_degrees(behaviors, graph)

  // Find all nodes with in-degree 0
  let initial_queue =
    in_degrees
    |> dict.filter(fn(_, degree) { degree == 0 })
    |> dict.keys

  kahn_loop(initial_queue, in_degrees, graph, by_name, [])
}

fn calculate_in_degrees(
  behaviors: List(ResolvedBehavior),
  graph: Dict(String, List(String)),
) -> Dict(String, Int) {
  // Initialize all to 0
  let initial =
    behaviors
    |> list.map(fn(rb) { #(rb.behavior.name, 0) })
    |> dict.from_list

  // Count incoming edges (how many dependencies a node has)
  // A node's in-degree is the number of things it depends on
  dict.fold(graph, initial, fn(degrees, node, deps) {
    // The node's in-degree is the number of dependencies it has
    dict.insert(degrees, node, list.length(deps))
  })
}

fn kahn_loop(
  queue: List(String),
  in_degrees: Dict(String, Int),
  graph: Dict(String, List(String)),
  by_name: Dict(String, ResolvedBehavior),
  result: List(ResolvedBehavior),
) -> Result(List(ResolvedBehavior), ResolveError) {
  case queue {
    [] -> {
      // Check if all nodes were processed
      let remaining =
        in_degrees
        |> dict.filter(fn(_, degree) { degree > 0 })
        |> dict.keys

      case remaining {
        [] -> Ok(list.reverse(result))
        _ -> Error(CyclicDependency(remaining))
      }
    }
    [node, ..rest_queue] -> {
      // Get the behavior for this node
      case dict.get(by_name, node) {
        Ok(rb) -> {
          // Update in-degrees for dependents
          let #(new_degrees, new_ready) =
            update_in_degrees_for_dependents(node, graph, in_degrees)

          // Add newly ready nodes to queue
          let updated_queue = list.append(rest_queue, new_ready)

          kahn_loop(updated_queue, new_degrees, graph, by_name, [rb, ..result])
        }
        Error(_) -> {
          // This shouldn't happen, skip
          kahn_loop(rest_queue, in_degrees, graph, by_name, result)
        }
      }
    }
  }
}

fn update_in_degrees_for_dependents(
  completed: String,
  graph: Dict(String, List(String)),
  in_degrees: Dict(String, Int),
) -> #(Dict(String, Int), List(String)) {
  // Find all nodes that depend on the completed node
  let dependents =
    graph
    |> dict.filter(fn(_, deps) { list.contains(deps, completed) })
    |> dict.keys

  // Decrement their in-degrees
  let #(new_degrees, newly_ready) =
    list.fold(dependents, #(in_degrees, []), fn(acc, dep) {
      let #(degrees, ready) = acc
      case dict.get(degrees, dep) {
        Ok(count) -> {
          let new_count = count - 1
          let new_degrees = dict.insert(degrees, dep, new_count)
          case new_count == 0 {
            True -> #(new_degrees, [dep, ..ready])
            False -> #(new_degrees, ready)
          }
        }
        Error(_) -> acc
      }
    })

  #(new_degrees, newly_ready)
}

/// Format a resolve error as a human-readable string
pub fn format_error(error: ResolveError) -> String {
  case error {
    CyclicDependency(behaviors) ->
      "Cyclic dependency detected involving: "
      <> list_to_string(behaviors, ", ")
    MissingDependency(behavior, missing) ->
      "Behavior '"
      <> behavior
      <> "' requires '"
      <> missing
      <> "' which does not exist"
    DuplicateBehaviorName(name) -> "Duplicate behavior name: " <> name
  }
}

fn list_to_string(items: List(String), sep: String) -> String {
  case items {
    [] -> ""
    [item] -> item
    [item, ..rest] -> item <> sep <> list_to_string(rest, sep)
  }
}

// =============================================================================
// AI-FRIENDLY ERROR FORMATTING
// =============================================================================

/// Format error as AI-friendly CUE structure
/// Returns structured error with action, context, suggestion, and recovery steps
pub fn format_error_ai(error: ResolveError) -> String {
  case error {
    CyclicDependency(behaviors) -> {
      let cycle = list_to_string(behaviors, " -> ")
      let cycle_json = json_string_array(behaviors)
      "{\n"
      <> "    action: \"dependency_error\"\n"
      <> "    error: {\n"
      <> "        type: \"circular_dependency\"\n"
      <> "        message: \"Circular dependency detected in behavior dependencies\"\n"
      <> "        context: {\n"
      <> "            cycle_path: \""
      <> escape_json_string(cycle)
      <> "\"\n"
      <> "            behaviors_involved: "
      <> cycle_json
      <> "\n"
      <> "            cycle_length: "
      <> string.inspect(list.length(behaviors))
      <> "\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Break the circular dependency by removing one 'requires' link\"\n"
      <> "    recovery: [\n"
      <> "        \"Review the 'requires' field for each behavior in the cycle\",\n"
      <> "        \"Identify which dependency can be safely removed\",\n"
      <> "        \"Consider reordering behavior execution to avoid the cycle\",\n"
      <> "        \"Ensure dependencies form a Directed Acyclic Graph (DAG)\",\n"
      <> "        \"Run 'intent validate' after fixing to verify resolution\"\n"
      <> "    ]\n"
      <> "}"
    }

    MissingDependency(behavior, missing) ->
      "{\n"
      <> "    action: \"dependency_error\"\n"
      <> "    error: {\n"
      <> "        type: \"missing_dependency\"\n"
      <> "        message: \"Behavior requires a dependency that does not exist\"\n"
      <> "        context: {\n"
      <> "            behavior: \""
      <> escape_json_string(behavior)
      <> "\"\n"
      <> "            missing_dependency: \""
      <> escape_json_string(missing)
      <> "\"\n"
      <> "            dependency_type: \"behavior\"\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Add the missing behavior or remove the dependency reference\"\n"
      <> "    recovery: [\n"
      <> "        \"Check if '"
      <> escape_json_string(missing)
      <> "' is defined in any feature\",\n"
      <> "        \"Verify the behavior name is spelled correctly\",\n"
      <> "        \"Add a new behavior named '"
      <> escape_json_string(missing)
      <> "' if needed\",\n"
      <> "        \"Remove '"
      <> escape_json_string(missing)
      <> "' from the 'requires' list of '"
      <> escape_json_string(behavior)
      <> "'\",\n"
      <> "        \"Run 'intent validate' to verify all dependencies exist\"\n"
      <> "    ]\n"
      <> "}"

    DuplicateBehaviorName(name) ->
      "{\n"
      <> "    action: \"validation_error\"\n"
      <> "    error: {\n"
      <> "        type: \"duplicate_behavior_name\"\n"
      <> "        message: \"Multiple behaviors have the same name\"\n"
      <> "        context: {\n"
      <> "            duplicate_name: \""
      <> escape_json_string(name)
      <> "\"\n"
      <> "            constraint: \"Behavior names must be unique across all features\"\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Rename one of the duplicate behaviors to be unique\"\n"
      <> "    recovery: [\n"
      <> "        \"Search the spec for all behaviors named '"
      <> escape_json_string(name)
      <> "'\",\n"
      <> "        \"Rename one or more to be unique (e.g., '"
      <> escape_json_string(name)
      <> "_v2', '"
      <> escape_json_string(name)
      <> "_alt')\",\n"
      <> "        \"Update any 'requires' references to use the new name\",\n"
      <> "        \"Consider using feature prefixes (e.g., 'auth_"
      <> escape_json_string(name)
      <> "', 'user_"
      <> escape_json_string(name)
      <> "')\",\n"
      <> "        \"Run 'intent validate' to verify uniqueness\"\n"
      <> "    ]\n"
      <> "}"
  }
}

/// Format error as human-readable text with context and recovery steps
pub fn format_error_text(error: ResolveError) -> String {
  case error {
    CyclicDependency(behaviors) -> {
      let cycle = list_to_string(behaviors, " -> ")
      "Error: Circular dependency detected in behavior dependencies\n\n"
      <> "Context:\n"
      <> "  cycle_path: "
      <> cycle
      <> "\n"
      <> "  behaviors_involved: "
      <> list_to_string(behaviors, ", ")
      <> "\n"
      <> "  cycle_length: "
      <> string.inspect(list.length(behaviors))
      <> "\n\n"
      <> "Suggestion: Break the circular dependency by removing one 'requires' link\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Review the 'requires' field for each behavior in the cycle\n"
      <> "  2. Identify which dependency can be safely removed\n"
      <> "  3. Consider reordering behavior execution to avoid the cycle\n"
      <> "  4. Ensure dependencies form a Directed Acyclic Graph (DAG)\n"
      <> "  5. Run 'intent validate' after fixing to verify resolution"
    }

    MissingDependency(behavior, missing) ->
      "Error: Behavior requires a dependency that does not exist\n\n"
      <> "Context:\n"
      <> "  behavior: "
      <> behavior
      <> "\n"
      <> "  missing_dependency: "
      <> missing
      <> "\n"
      <> "  dependency_type: behavior\n\n"
      <> "Suggestion: Add the missing behavior or remove the dependency reference\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Check if '"
      <> missing
      <> "' is defined in any feature\n"
      <> "  2. Verify the behavior name is spelled correctly\n"
      <> "  3. Add a new behavior named '"
      <> missing
      <> "' if needed\n"
      <> "  4. Remove '"
      <> missing
      <> "' from the 'requires' list of '"
      <> behavior
      <> "'\n"
      <> "  5. Run 'intent validate' to verify all dependencies exist"

    DuplicateBehaviorName(name) ->
      "Error: Multiple behaviors have the same name\n\n"
      <> "Context:\n"
      <> "  duplicate_name: "
      <> name
      <> "\n"
      <> "  constraint: Behavior names must be unique across all features\n\n"
      <> "Suggestion: Rename one of the duplicate behaviors to be unique\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Search the spec for all behaviors named '"
      <> name
      <> "'\n"
      <> "  2. Rename one or more to be unique (e.g., '"
      <> name
      <> "_v2', '"
      <> name
      <> "_alt')\n"
      <> "  3. Update any 'requires' references to use the new name\n"
      <> "  4. Consider using feature prefixes (e.g., 'auth_"
      <> name
      <> "', 'user_"
      <> name
      <> "')\n"
      <> "  5. Run 'intent validate' to verify uniqueness"
  }
}

/// Escape special characters in JSON strings
fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

/// Convert a list of strings to a JSON array representation
fn json_string_array(items: List(String)) -> String {
  let escaped = list.map(items, fn(s) { "\"" <> escape_json_string(s) <> "\"" })
  "[" <> list_to_string(escaped, ", ") <> "]"
}
