/// Main test runner - orchestrates behavior execution and validation

import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import gleam_community/ansi
import intent/anti_patterns
import intent/checker
import intent/http_client.{type ExecutionResult, type ExecutionError}
import intent/interpolate.{type Context}
import intent/output.{type SpecResult}
import intent/resolver.{type ResolvedBehavior}
import intent/rules_engine
import intent/types.{type Behavior, type Config, type Spec}
import spinner

/// Options for running the spec
pub type RunOptions {
  RunOptions(
    feature_filter: Option(String),
    behavior_filter: Option(String),
    verbose: Bool,
  )
}

/// Default run options
pub fn default_options() -> RunOptions {
  RunOptions(feature_filter: None, behavior_filter: None, verbose: False)
}

/// Run a spec and return the results
pub fn run_spec(
  spec: Spec,
  target_url: String,
  options: RunOptions,
) -> SpecResult {
  // Override base_url with target if provided
  let config = case string.is_empty(target_url) {
    True -> spec.config
    False -> types.Config(..spec.config, base_url: target_url)
  }

  // Resolve behavior execution order
  case resolver.resolve_execution_order(spec) {
    Error(e) -> {
      output.SpecResult(
        pass: False,
        passed: 0,
        failed: 0,
        blocked: 0,
        total: 0,
        summary: "Failed to resolve behavior order: " <> resolver.format_error(e),
        failures: [],
        blocked_behaviors: [],
        rule_violations: [],
        anti_patterns_detected: [],
      )
    }
    Ok(resolved) -> {
      // Apply filters
      let filtered = apply_filters(resolved, options)
      let total = list.length(filtered)

      // Start spinner for execution
      let sp =
        spinner.new("Running " <> string.inspect(total) <> " behaviors...")
        |> spinner.with_colour(ansi.cyan)
        |> spinner.start

      // Execute behaviors in order
      let #(results, _ctx, _failed_set) =
        execute_behaviors_with_spinner(filtered, config, spec, set.new(), sp)

      // Stop spinner
      spinner.stop(sp)

      // Collect results
      let passed =
        list.count(results, fn(r) {
          case r {
            BehaviorPassed(_) -> True
            _ -> False
          }
        })
      let failed =
        list.count(results, fn(r) {
          case r {
            BehaviorFailed(_, _) -> True
            _ -> False
          }
        })
      let blocked =
        list.count(results, fn(r) {
          case r {
            BehaviorBlocked(_, _) -> True
            _ -> False
          }
        })

      // Collect failures
      let failures =
        list.filter_map(results, fn(r) {
          case r {
            BehaviorFailed(failure, _) -> Ok(failure)
            _ -> Error(Nil)
          }
        })

      // Collect blocked
      let blocked_behaviors =
        list.filter_map(results, fn(r) {
          case r {
            BehaviorBlocked(name, dep) -> Ok(output.create_blocked(name, dep))
            _ -> Error(Nil)
          }
        })

      // Collect rule violations
      let rule_violations = collect_rule_violations(results, spec.rules)

      // Collect anti-patterns
      let anti_patterns = collect_anti_patterns(results, spec.anti_patterns)

      let pass = failed == 0 && blocked == 0

      let summary = case pass {
        True -> "All " <> string.inspect(passed) <> " behaviors passed"
        False ->
          string.inspect(failed)
          <> " failures, "
          <> string.inspect(blocked)
          <> " blocked out of "
          <> string.inspect(total)
          <> " behaviors"
      }

      output.SpecResult(
        pass: pass,
        passed: passed,
        failed: failed,
        blocked: blocked,
        total: total,
        summary: summary,
        failures: failures,
        blocked_behaviors: blocked_behaviors,
        rule_violations: rule_violations,
        anti_patterns_detected: anti_patterns,
      )
    }
  }
}

/// Internal result type for a single behavior
type BehaviorResult {
  BehaviorPassed(execution: ExecutionResult)
  BehaviorFailed(failure: output.BehaviorFailure, execution: ExecutionResult)
  BehaviorBlocked(name: String, failed_dependency: String)
  BehaviorError(name: String, error: ExecutionError)
}

fn apply_filters(
  behaviors: List(ResolvedBehavior),
  options: RunOptions,
) -> List(ResolvedBehavior) {
  behaviors
  |> list.filter(fn(rb) {
    let feature_ok = case options.feature_filter {
      None -> True
      Some(f) -> rb.feature_name == f
    }
    let behavior_ok = case options.behavior_filter {
      None -> True
      Some(b) -> rb.behavior.name == b
    }
    feature_ok && behavior_ok
  })
}

fn execute_behaviors_with_spinner(
  behaviors: List(ResolvedBehavior),
  config: Config,
  spec: Spec,
  failed_set: Set(String),
  sp: spinner.Spinner,
) -> #(List(BehaviorResult), Context, Set(String)) {
  list.fold(
    behaviors,
    #([], interpolate.new_context(), failed_set),
    fn(acc, rb) {
      let #(results, ctx, failed) = acc
      // Update spinner text with current behavior
      spinner.set_text(sp, "Testing: " <> rb.behavior.name)
      let #(result, new_ctx, new_failed) =
        execute_single_behavior(rb, config, spec, ctx, failed)
      #([result, ..results], new_ctx, new_failed)
    },
  )
  |> fn(tuple) {
    let #(results, ctx, failed) = tuple
    #(list.reverse(results), ctx, failed)
  }
}

fn execute_single_behavior(
  rb: ResolvedBehavior,
  config: Config,
  _spec: Spec,
  ctx: Context,
  failed_set: Set(String),
) -> #(BehaviorResult, Context, Set(String)) {
  // Check if any dependencies failed
  let blocked_by = list.find(rb.behavior.requires, fn(dep) {
    set.contains(failed_set, dep)
  })

  case blocked_by {
    Ok(dep) -> {
      let result = BehaviorBlocked(rb.behavior.name, dep)
      #(result, ctx, set.insert(failed_set, rb.behavior.name))
    }
    Error(_) -> {
      // Execute the request
      case http_client.execute_request(config, rb.behavior.request, ctx) {
        Error(e) -> {
          let result = BehaviorError(rb.behavior.name, e)
          #(result, ctx, set.insert(failed_set, rb.behavior.name))
        }
        Ok(execution) -> {
          // Update context with response body
          let ctx = interpolate.set_response_body(ctx, execution.body)

          // Update context with request body if present
          let ctx = interpolate.set_request_body(ctx, rb.behavior.request.body)

          // Check the response
          let check_result =
            checker.check_response(rb.behavior.response, execution, ctx)

          // Check if passed
          let passed =
            check_result.status_ok && list.is_empty(check_result.failed)

          case passed {
            True -> {
              // Capture values
              let new_ctx = apply_captures(ctx, rb.behavior, execution)
              let result = BehaviorPassed(execution)
              #(result, new_ctx, failed_set)
            }
            False -> {
              let failure =
                output.create_failure(
                  rb.feature_name,
                  rb.behavior,
                  check_result,
                  execution,
                  config.base_url,
                )
              let result = BehaviorFailed(failure, execution)
              #(result, ctx, set.insert(failed_set, rb.behavior.name))
            }
          }
        }
      }
    }
  }
}

fn apply_captures(
  ctx: Context,
  behavior: Behavior,
  _execution: ExecutionResult,
) -> Context {
  dict.fold(behavior.captures, ctx, fn(acc_ctx, name, path) {
    case interpolate.extract_capture(acc_ctx, path) {
      Ok(value) -> interpolate.set_variable(acc_ctx, name, value)
      Error(_) -> acc_ctx
    }
  })
}

fn collect_rule_violations(
  results: List(BehaviorResult),
  rules: List(types.Rule),
) -> List(output.RuleViolationGroup) {
  // Group violations by rule
  results
  |> list.flat_map(fn(result) {
    case result {
      BehaviorPassed(execution) -> check_rules_for_execution(execution, rules, "")
      BehaviorFailed(failure, execution) ->
        check_rules_for_execution(execution, rules, failure.behavior)
      _ -> []
    }
  })
  |> group_violations_by_rule
}

fn check_rules_for_execution(
  execution: ExecutionResult,
  rules: List(types.Rule),
  behavior_name: String,
) -> List(#(String, String, output.BehaviorViolation)) {
  rules
  |> list.flat_map(fn(rule) {
    let results = rules_engine.check_rules([rule], execution, behavior_name)
    list.filter_map(results, fn(r) {
      case r {
        rules_engine.RuleFailed(name, desc, violations) ->
          Ok(#(
            name,
            desc,
            output.BehaviorViolation(
              behavior: behavior_name,
              violations: list.map(violations, rules_engine.format_violation),
              response: Some(execution.body),
            ),
          ))
        _ -> Error(Nil)
      }
    })
  })
}

fn group_violations_by_rule(
  violations: List(#(String, String, output.BehaviorViolation)),
) -> List(output.RuleViolationGroup) {
  violations
  |> list.group(fn(v) { v.0 })
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(rule_name, items) = pair
    let description = case items {
      [#(_, desc, _), ..] -> desc
      [] -> ""
    }
    let behavior_violations = list.map(items, fn(item) { item.2 })
    output.RuleViolationGroup(
      rule: rule_name,
      description: description,
      violations: behavior_violations,
    )
  })
}

fn collect_anti_patterns(
  results: List(BehaviorResult),
  patterns: List(types.AntiPattern),
) -> List(anti_patterns.AntiPatternResult) {
  results
  |> list.flat_map(fn(result) {
    case result {
      BehaviorPassed(execution) ->
        anti_patterns.check_anti_patterns(patterns, execution, "")
      BehaviorFailed(failure, execution) ->
        anti_patterns.check_anti_patterns(patterns, execution, failure.behavior)
      _ -> []
    }
  })
}
