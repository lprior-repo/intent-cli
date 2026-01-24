/// Main test runner - orchestrates behavior execution and validation
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import gleam_community/ansi
import intent/anti_patterns
import intent/checker
import intent/checker/types as checker_types
import intent/http_client.{type ExecutionError, type ExecutionResult}
import intent/interpolate.{type Context}
import intent/output.{type SpecResult}
import intent/output_mode.{type OutputMode}
import intent/resolver.{type ResolvedBehavior}
import intent/rules_engine
import intent/types.{type Behavior, type Config, type Request, type Spec}
import spinner

/// Abstraction for behavior execution - enables dependency injection for testing
/// This allows tests to mock HTTP responses without making real network requests
pub type BehaviorExecutor {
  BehaviorExecutor(
    execute: fn(Config, Request, Context) ->
      Result(ExecutionResult, ExecutionError),
  )
}

/// Default executor that uses http_client for real HTTP requests
pub fn default_executor() -> BehaviorExecutor {
  BehaviorExecutor(execute: http_client.execute_request)
}

/// Create an executor with a specific timeout
pub fn executor_with_timeout(timeout_ms: Int) -> BehaviorExecutor {
  BehaviorExecutor(execute: fn(config, req, ctx) {
    http_client.execute_request_with_timeout(config, req, ctx, timeout_ms)
  })
}

/// UI callbacks for progress indication during spec execution
/// Allows CLI layer to inject spinner/progress UI without runner knowing about specific UI implementation
pub type UiCallbacks {
  UiCallbacks(
    on_start: fn(Int) -> Nil,
    on_behavior: fn(String) -> Nil,
    on_complete: fn() -> Nil,
  )
}

/// No-op callbacks for testing or non-interactive modes (JSON output)
pub fn noop_callbacks() -> UiCallbacks {
  UiCallbacks(
    on_start: fn(_) { Nil },
    on_behavior: fn(_) { Nil },
    on_complete: fn() { Nil },
  )
}

/// Output verbosity level for spec execution
/// - Quiet: Minimal output, errors only
/// - Normal: Standard output with pass/fail summary
/// - Verbose: Detailed output including request/response details
pub type OutputLevel {
  Quiet
  Normal
  Verbose
}

/// Options for running the spec
pub type RunOptions {
  RunOptions(
    feature_filter: Option(String),
    behavior_filter: Option(String),
    output_level: OutputLevel,
    timeout_ms: Option(Int),
    allow_localhost: Bool,
  )
}

/// Default timeout in milliseconds (30 seconds per AI guardrails spec)
pub const default_timeout_ms = 30_000

/// Default run options with Normal output level and no timeout override
pub fn default_options() -> RunOptions {
  RunOptions(
    feature_filter: None,
    behavior_filter: None,
    output_level: Normal,
    timeout_ms: None,
    allow_localhost: False,
  )
}

/// Set timeout override on RunOptions
pub fn with_timeout(options: RunOptions, timeout_ms: Int) -> RunOptions {
  RunOptions(..options, timeout_ms: Some(timeout_ms))
}

/// Get effective timeout: CLI flag overrides spec config
pub fn effective_timeout(cli_timeout: Option(Int), spec_timeout: Int) -> Int {
  case cli_timeout {
    Some(t) -> t
    None -> spec_timeout
  }
}

/// Check if output level is verbose
pub fn is_verbose(options: RunOptions) -> Bool {
  case options.output_level {
    Verbose -> True
    _ -> False
  }
}

/// Check if output level is quiet
pub fn is_quiet(options: RunOptions) -> Bool {
  case options.output_level {
    Quiet -> True
    _ -> False
  }
}

/// Run a spec and return the results (uses default HTTP executor)
/// If options.timeout_ms is set, it overrides the spec's config.timeout_ms
pub fn run_spec(
  spec: Spec,
  target_url: String,
  options: RunOptions,
  mode: OutputMode,
) -> SpecResult {
  // Determine effective timeout: CLI flag overrides spec config
  let timeout = effective_timeout(options.timeout_ms, spec.config.timeout_ms)
  let executor = executor_with_timeout(timeout)
  run_spec_with_executor(spec, target_url, options, executor, mode)
}

/// Run a spec with a custom executor - enables dependency injection for testing
/// This allows tests to mock HTTP responses without making real network requests
pub fn run_spec_with_executor(
  spec: Spec,
  target_url: String,
  options: RunOptions,
  executor: BehaviorExecutor,
  mode: OutputMode,
) -> SpecResult {
  // Create spinner-based UI callbacks when in interactive mode
  let ui = case output_mode.should_show_spinner(mode) {
    True -> create_spinner_callbacks()
    False -> noop_callbacks()
  }
  run_spec_with_executor_and_ui(spec, target_url, options, executor, mode, ui)
}

/// Run a spec with custom executor and UI callbacks - full dependency injection
/// This is the core implementation that allows complete control over both
/// HTTP execution and UI feedback
pub fn run_spec_with_executor_and_ui(
  spec: Spec,
  target_url: String,
  options: RunOptions,
  executor: BehaviorExecutor,
  _mode: OutputMode,
  ui: UiCallbacks,
) -> SpecResult {
  // Override base_url and allow_localhost from CLI options
  let config = case string.is_empty(target_url) {
    True ->
      types.Config(..spec.config, allow_localhost: options.allow_localhost)
    False ->
      types.Config(
        ..spec.config,
        base_url: target_url,
        allow_localhost: options.allow_localhost,
      )
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
        summary: "Failed to resolve behavior order: "
          <> resolver.format_error(e),
        failures: [],
        error_failures: [],
        blocked_behaviors: [],
        rule_violations: [],
        anti_patterns_detected: [],
      )
    }
    Ok(resolved) -> {
      // Apply filters
      let filtered = apply_filters(resolved, options)
      let total = list.length(filtered)

      // Notify UI of start
      ui.on_start(total)

      // Execute behaviors in order with the provided executor
      let #(results, _ctx, _failed_set) =
        execute_behaviors_with_callbacks(
          filtered,
          config,
          spec,
          set.new(),
          ui,
          executor,
        )

      // Notify UI of completion
      ui.on_complete()

      // Collect results
      let passed =
        list.count(results, fn(r) {
          case r {
            BehaviorPassed(_) -> True
            _ -> False
          }
        })

      // BUG FIX (intent-cli-clm.3): Count BehaviorError as failures
      // Previously, BehaviorError was NOT counted, causing pass=True even when
      // network errors occurred (connection refused, timeout, DNS failure)
      let failed =
        list.count(results, fn(r) {
          case r {
            BehaviorFailed(_, _) -> True
            BehaviorError(_, _) -> True
            // NOW COUNTED AS FAILURE
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

      // Collect failures for detailed reporting
      // Note: BehaviorError (network errors) are counted in `failed` above
      // but not included here since they lack response data for BehaviorFailure
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

      // Collect error failures (network/execution errors)
      let error_failures =
        list.filter_map(results, fn(r) {
          case r {
            BehaviorError(name, error) -> {
              let #(error_type, message) = case error {
                http_client.UrlParseError(msg) -> #("URL_PARSE_ERROR", msg)
                http_client.InterpolationError(msg) -> #(
                  "INTERPOLATION_ERROR",
                  msg,
                )
                http_client.RequestError(code:, message:, details: _) -> {
                  let error_type = case code {
                    http_client.Timeout -> "TIMEOUT"
                    http_client.ConnectionRefused -> "CONNECTION_REFUSED"
                    http_client.DNSFailure -> "DNS_FAILURE"
                    http_client.SSLError -> "SSL_ERROR"
                    http_client.NetworkUnreachable -> "NETWORK_UNREACHABLE"
                    http_client.PermissionDenied -> "PERMISSION_DENIED"
                    http_client.Unknown -> "REQUEST_ERROR"
                  }
                  #(error_type, message)
                }
                http_client.ResponseParseError(msg) -> #(
                  "RESPONSE_PARSE_ERROR",
                  msg,
                )
                http_client.SSRFBlocked(msg) -> #("SSRF_BLOCKED", msg)
              }
              Ok(output.create_error_info(name, error_type, message))
            }
            _ -> Error(Nil)
          }
        })

      // Collect rule violations
      let rule_violations = collect_rule_violations(results, spec.rules)

      // Collect anti-patterns
      let anti_patterns = collect_anti_patterns(results, spec.anti_patterns)

      // BUG FIX: pass is only True when failed == 0 (including errors) AND blocked == 0
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
        error_failures: error_failures,
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

/// Execute behaviors with UI callbacks for progress indication
fn execute_behaviors_with_callbacks(
  behaviors: List(ResolvedBehavior),
  config: Config,
  spec: Spec,
  failed_set: Set(String),
  ui: UiCallbacks,
  executor: BehaviorExecutor,
) -> #(List(BehaviorResult), Context, Set(String)) {
  list.fold(
    behaviors,
    #([], interpolate.new_context(), failed_set),
    fn(acc, rb) {
      let #(results, ctx, failed) = acc
      // Notify UI of current behavior
      ui.on_behavior(rb.behavior.name)
      let #(result, new_ctx, new_failed) =
        execute_single_behavior(rb, config, spec, ctx, failed, executor)
      #([result, ..results], new_ctx, new_failed)
    },
  )
  |> fn(tuple) {
    let #(results, ctx, failed) = tuple
    #(list.reverse(results), ctx, failed)
  }
}

/// Create spinner-based UI callbacks for interactive mode
/// The spinner is created and managed within these callbacks
fn create_spinner_callbacks() -> UiCallbacks {
  // We need to use a mutable reference to hold the spinner
  // since callbacks are closures and Gleam is immutable
  let spinner_ref = spinner_ref_new()

  UiCallbacks(
    on_start: fn(total) {
      let sp =
        spinner.new("Running " <> string.inspect(total) <> " behaviors...")
        |> spinner.with_colour(ansi.cyan)
        |> spinner.start
      spinner_ref_set(spinner_ref, sp)
      Nil
    },
    on_behavior: fn(name) {
      case spinner_ref_get(spinner_ref) {
        Some(sp) -> spinner.set_text(sp, "Testing: " <> name)
        None -> Nil
      }
    },
    on_complete: fn() {
      case spinner_ref_get(spinner_ref) {
        Some(sp) -> spinner.stop(sp)
        None -> Nil
      }
    },
  )
}

/// FFI for mutable spinner reference
@external(erlang, "intent_runner_ffi", "spinner_ref_new")
fn spinner_ref_new() -> dynamic.Dynamic

@external(erlang, "intent_runner_ffi", "spinner_ref_set")
fn spinner_ref_set(ref: dynamic.Dynamic, spinner: spinner.Spinner) -> Nil

@external(erlang, "intent_runner_ffi", "spinner_ref_get")
fn spinner_ref_get(ref: dynamic.Dynamic) -> Option(spinner.Spinner)

fn execute_single_behavior(
  rb: ResolvedBehavior,
  config: Config,
  _spec: Spec,
  ctx: Context,
  failed_set: Set(String),
  executor: BehaviorExecutor,
) -> #(BehaviorResult, Context, Set(String)) {
  // Check if any dependencies failed
  let blocked_by =
    list.find(rb.behavior.requires, fn(dep) { set.contains(failed_set, dep) })

  case blocked_by {
    Ok(dep) -> {
      let result = BehaviorBlocked(rb.behavior.name, dep)
      #(result, ctx, set.insert(failed_set, rb.behavior.name))
    }
    Error(_) -> {
      // Execute the request using the injected executor
      case executor.execute(config, rb.behavior.request, ctx) {
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
                  convert_response_check_result(check_result),
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

/// Convert checker.ResponseCheckResult to checker_types.ResponseCheckResult
/// This bridges the gap between the duplicate type definitions until they're fully consolidated
fn convert_response_check_result(
  result: checker.ResponseCheckResult,
) -> checker_types.ResponseCheckResult {
  // Convert CheckResult items
  let passed =
    list.map(result.passed, fn(check) {
      case check {
        checker.CheckPassed(field, rule) ->
          checker_types.CheckPassed(field, rule)
        checker.CheckFailed(field, rule, expected, actual, explanation) ->
          checker_types.CheckFailed(field, rule, expected, actual, explanation)
      }
    })

  let failed =
    list.map(result.failed, fn(check) {
      case check {
        checker.CheckPassed(field, rule) ->
          checker_types.CheckPassed(field, rule)
        checker.CheckFailed(field, rule, expected, actual, explanation) ->
          checker_types.CheckFailed(field, rule, expected, actual, explanation)
      }
    })

  checker_types.ResponseCheckResult(
    passed: passed,
    failed: failed,
    status_ok: result.status_ok,
    status_expected: result.status_expected,
    status_actual: result.status_actual,
  )
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
      BehaviorPassed(execution) ->
        check_rules_for_execution(execution, rules, "")
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
