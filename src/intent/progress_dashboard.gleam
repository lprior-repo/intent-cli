/// Progress dashboard JSON output for AI-friendly test run monitoring
///
/// This module provides structured JSON output for tracking test run progress,
/// following the action-based schema from json_output.gleam.
///
/// Example output:
/// ```json
/// {
///   "action": "progress_update",
///   "command": "check",
///   "data": {
///     "status": "running",
///     "progress": { "current": 5, "total": 10, "percentage": 50 },
///     "counts": { "passed": 3, "failed": 1, "blocked": 0, "pending": 6 },
///     "timing": { "started_at": "...", "elapsed_ms": 5000, ... },
///     "current_behavior": { "name": "create_user", "feature": "Users" }
///   },
///   "metadata": { ... },
///   "spec_path": "api.cue"
/// }
/// ```
import gleam/int
import gleam/io
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import intent/json_output

/// Status of the test run
pub type ProgressStatus {
  Running
  Completed
  Failed
}

/// Progress counts for pass/fail/blocked behaviors
pub type ProgressCounts {
  ProgressCounts(passed: Int, failed: Int, blocked: Int, pending: Int)
}

/// Timing information for the test run
pub type TimingInfo {
  TimingInfo(
    started_at: String,
    elapsed_ms: Int,
    estimated_remaining_ms: Option(Int),
    avg_behavior_ms: Option(Int),
  )
}

/// Information about the currently running behavior
pub type CurrentBehavior {
  CurrentBehavior(name: String, feature: String)
}

/// Overall progress information
pub type Progress {
  Progress(current: Int, total: Int, percentage: Int)
}

/// Complete progress dashboard state
pub type ProgressDashboard {
  ProgressDashboard(
    status: ProgressStatus,
    progress: Progress,
    counts: ProgressCounts,
    timing: TimingInfo,
    current_behavior: Option(CurrentBehavior),
    spec_path: Option(String),
  )
}

/// Create a new progress dashboard at the start of a test run
pub fn create_dashboard(
  total_behaviors: Int,
  spec_path: Option(String),
) -> ProgressDashboard {
  ProgressDashboard(
    status: Running,
    progress: Progress(current: 0, total: total_behaviors, percentage: 0),
    counts: ProgressCounts(
      passed: 0,
      failed: 0,
      blocked: 0,
      pending: total_behaviors,
    ),
    timing: TimingInfo(
      started_at: current_timestamp(),
      elapsed_ms: 0,
      estimated_remaining_ms: None,
      avg_behavior_ms: None,
    ),
    current_behavior: None,
    spec_path: spec_path,
  )
}

/// Update the dashboard when starting a new behavior
pub fn start_behavior(
  dashboard: ProgressDashboard,
  behavior_name: String,
  feature_name: String,
) -> ProgressDashboard {
  ProgressDashboard(
    ..dashboard,
    current_behavior: Some(CurrentBehavior(
      name: behavior_name,
      feature: feature_name,
    )),
  )
}

/// Update the dashboard when a behavior passes
pub fn record_pass(
  dashboard: ProgressDashboard,
  elapsed_ms: Int,
) -> ProgressDashboard {
  let new_current = dashboard.progress.current + 1
  let new_passed = dashboard.counts.passed + 1
  let new_pending = dashboard.counts.pending - 1

  update_progress_internal(
    dashboard,
    new_current,
    new_passed,
    dashboard.counts.failed,
    dashboard.counts.blocked,
    new_pending,
    elapsed_ms,
  )
}

/// Update the dashboard when a behavior fails
pub fn record_fail(
  dashboard: ProgressDashboard,
  elapsed_ms: Int,
) -> ProgressDashboard {
  let new_current = dashboard.progress.current + 1
  let new_failed = dashboard.counts.failed + 1
  let new_pending = dashboard.counts.pending - 1

  update_progress_internal(
    dashboard,
    new_current,
    dashboard.counts.passed,
    new_failed,
    dashboard.counts.blocked,
    new_pending,
    elapsed_ms,
  )
}

/// Update the dashboard when a behavior is blocked
pub fn record_blocked(
  dashboard: ProgressDashboard,
  elapsed_ms: Int,
) -> ProgressDashboard {
  let new_current = dashboard.progress.current + 1
  let new_blocked = dashboard.counts.blocked + 1
  let new_pending = dashboard.counts.pending - 1

  update_progress_internal(
    dashboard,
    new_current,
    dashboard.counts.passed,
    dashboard.counts.failed,
    new_blocked,
    new_pending,
    elapsed_ms,
  )
}

/// Mark the dashboard as completed
pub fn complete(
  dashboard: ProgressDashboard,
  elapsed_ms: Int,
) -> ProgressDashboard {
  let final_status = case dashboard.counts.failed > 0 {
    True -> Failed
    False -> Completed
  }

  ProgressDashboard(
    ..dashboard,
    status: final_status,
    timing: TimingInfo(
      ..dashboard.timing,
      elapsed_ms: elapsed_ms,
      estimated_remaining_ms: Some(0),
    ),
    current_behavior: None,
  )
}

/// Internal helper to update progress calculations
fn update_progress_internal(
  dashboard: ProgressDashboard,
  current: Int,
  passed: Int,
  failed: Int,
  blocked: Int,
  pending: Int,
  elapsed_ms: Int,
) -> ProgressDashboard {
  let total = dashboard.progress.total
  let percentage = case total {
    0 -> 100
    _ -> { current * 100 } / total
  }

  // Calculate average behavior time
  let avg_ms = case current {
    0 -> None
    n -> Some(elapsed_ms / n)
  }

  // Estimate remaining time
  let estimated_remaining = case avg_ms {
    None -> None
    Some(avg) -> Some(avg * pending)
  }

  ProgressDashboard(
    ..dashboard,
    status: Running,
    progress: Progress(current: current, total: total, percentage: percentage),
    counts: ProgressCounts(
      passed: passed,
      failed: failed,
      blocked: blocked,
      pending: pending,
    ),
    timing: TimingInfo(
      ..dashboard.timing,
      elapsed_ms: elapsed_ms,
      estimated_remaining_ms: estimated_remaining,
      avg_behavior_ms: avg_ms,
    ),
  )
}

/// Convert status to string
fn status_to_string(status: ProgressStatus) -> String {
  case status {
    Running -> "running"
    Completed -> "completed"
    Failed -> "failed"
  }
}

/// Convert progress to JSON
fn progress_to_json(progress: Progress) -> Json {
  json.object([
    #("current", json.int(progress.current)),
    #("total", json.int(progress.total)),
    #("percentage", json.int(progress.percentage)),
  ])
}

/// Convert counts to JSON
fn counts_to_json(counts: ProgressCounts) -> Json {
  json.object([
    #("passed", json.int(counts.passed)),
    #("failed", json.int(counts.failed)),
    #("blocked", json.int(counts.blocked)),
    #("pending", json.int(counts.pending)),
  ])
}

/// Convert timing to JSON
fn timing_to_json(timing: TimingInfo) -> Json {
  json.object([
    #("started_at", json.string(timing.started_at)),
    #("elapsed_ms", json.int(timing.elapsed_ms)),
    #(
      "estimated_remaining_ms",
      option_int_to_json(timing.estimated_remaining_ms),
    ),
    #("avg_behavior_ms", option_int_to_json(timing.avg_behavior_ms)),
  ])
}

/// Convert optional int to JSON
fn option_int_to_json(opt: Option(Int)) -> Json {
  case opt {
    Some(n) -> json.int(n)
    None -> json.null()
  }
}

/// Convert current behavior to JSON
fn current_behavior_to_json(behavior: Option(CurrentBehavior)) -> Json {
  case behavior {
    Some(b) ->
      json.object([
        #("name", json.string(b.name)),
        #("feature", json.string(b.feature)),
      ])
    None -> json.null()
  }
}

/// Convert dashboard data to JSON (inner data object)
fn dashboard_data_to_json(dashboard: ProgressDashboard) -> Json {
  json.object([
    #("status", json.string(status_to_string(dashboard.status))),
    #("progress", progress_to_json(dashboard.progress)),
    #("counts", counts_to_json(dashboard.counts)),
    #("timing", timing_to_json(dashboard.timing)),
    #("current_behavior", current_behavior_to_json(dashboard.current_behavior)),
  ])
}

/// Convert dashboard to full JSON response following action-based schema
pub fn to_json(dashboard: ProgressDashboard) -> Json {
  let exit_code = case dashboard.status {
    Running -> 0
    Completed -> 0
    Failed -> 1
  }

  json_output.create_response(
    "progress_update",
    "check",
    dashboard_data_to_json(dashboard),
    dashboard.spec_path,
    exit_code,
  )
  |> json_output.to_json
}

/// Output dashboard JSON to stdout
pub fn output(dashboard: ProgressDashboard) -> Nil {
  dashboard
  |> to_json
  |> json.to_string
  |> io.println
}

/// Create a summary dashboard from final results
/// Useful for generating a final progress summary after test run
pub fn from_results(
  passed: Int,
  failed: Int,
  blocked: Int,
  total: Int,
  elapsed_ms: Int,
  spec_path: Option(String),
) -> ProgressDashboard {
  let status = case failed > 0 || blocked > 0 {
    True -> Failed
    False -> Completed
  }

  let avg_ms = case total {
    0 -> None
    n -> Some(elapsed_ms / n)
  }

  ProgressDashboard(
    status: status,
    progress: Progress(current: total, total: total, percentage: 100),
    counts: ProgressCounts(
      passed: passed,
      failed: failed,
      blocked: blocked,
      pending: 0,
    ),
    timing: TimingInfo(
      started_at: current_timestamp(),
      elapsed_ms: elapsed_ms,
      estimated_remaining_ms: Some(0),
      avg_behavior_ms: avg_ms,
    ),
    current_behavior: None,
    spec_path: spec_path,
  )
}

/// Convert elapsed milliseconds to human-readable duration string
pub fn format_duration(ms: Int) -> String {
  case ms {
    n if n < 1000 -> int.to_string(n) <> "ms"
    n if n < 60_000 -> {
      let secs = n / 1000
      let remaining_ms = n % 1000
      int.to_string(secs)
      <> "."
      <> pad_left(int.to_string(remaining_ms / 100), 1, "0")
      <> "s"
    }
    n -> {
      let mins = n / 60_000
      let secs = { n % 60_000 } / 1000
      int.to_string(mins) <> "m " <> int.to_string(secs) <> "s"
    }
  }
}

/// Pad string on the left
fn pad_left(s: String, min_len: Int, pad_char: String) -> String {
  case int.compare(string_length(s), min_len) {
    order.Lt -> pad_left(pad_char <> s, min_len, pad_char)
    _ -> s
  }
}

import gleam/order
import gleam/string

fn string_length(s: String) -> Int {
  string.length(s)
}

/// Get current timestamp in ISO 8601 format
@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String
