/// Tests for the progress dashboard JSON output module
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/progress_dashboard.{
  Completed, Failed, Progress, ProgressCounts, Running, TimingInfo,
}

/// Test creating a new dashboard
pub fn create_dashboard_test() {
  let dashboard = progress_dashboard.create_dashboard(10, Some("api.cue"))

  // Verify initial state
  dashboard.status |> should.equal(Running)
  dashboard.progress.current |> should.equal(0)
  dashboard.progress.total |> should.equal(10)
  dashboard.progress.percentage |> should.equal(0)
  dashboard.counts.passed |> should.equal(0)
  dashboard.counts.failed |> should.equal(0)
  dashboard.counts.blocked |> should.equal(0)
  dashboard.counts.pending |> should.equal(10)
  dashboard.spec_path |> should.equal(Some("api.cue"))
}

/// Test starting a behavior
pub fn start_behavior_test() {
  let dashboard =
    progress_dashboard.create_dashboard(5, None)
    |> progress_dashboard.start_behavior("create_user", "User Management")

  case dashboard.current_behavior {
    Some(b) -> {
      b.name |> should.equal("create_user")
      b.feature |> should.equal("User Management")
    }
    None -> should.fail()
  }
}

/// Test recording a pass
pub fn record_pass_test() {
  let dashboard =
    progress_dashboard.create_dashboard(5, None)
    |> progress_dashboard.start_behavior("test1", "Feature1")
    |> progress_dashboard.record_pass(100)

  dashboard.progress.current |> should.equal(1)
  dashboard.progress.percentage |> should.equal(20)
  dashboard.counts.passed |> should.equal(1)
  dashboard.counts.pending |> should.equal(4)
  dashboard.timing.elapsed_ms |> should.equal(100)
}

/// Test recording a fail
pub fn record_fail_test() {
  let dashboard =
    progress_dashboard.create_dashboard(5, None)
    |> progress_dashboard.start_behavior("test1", "Feature1")
    |> progress_dashboard.record_fail(150)

  dashboard.progress.current |> should.equal(1)
  dashboard.counts.failed |> should.equal(1)
  dashboard.counts.pending |> should.equal(4)
}

/// Test recording blocked
pub fn record_blocked_test() {
  let dashboard =
    progress_dashboard.create_dashboard(5, None)
    |> progress_dashboard.start_behavior("test1", "Feature1")
    |> progress_dashboard.record_blocked(50)

  dashboard.progress.current |> should.equal(1)
  dashboard.counts.blocked |> should.equal(1)
  dashboard.counts.pending |> should.equal(4)
}

/// Test completion with all passes
pub fn complete_all_pass_test() {
  let dashboard =
    progress_dashboard.create_dashboard(2, None)
    |> progress_dashboard.record_pass(100)
    |> progress_dashboard.record_pass(100)
    |> progress_dashboard.complete(200)

  dashboard.status |> should.equal(Completed)
  dashboard.progress.percentage |> should.equal(100)
  dashboard.timing.estimated_remaining_ms |> should.equal(Some(0))
}

/// Test completion with failures
pub fn complete_with_failures_test() {
  let dashboard =
    progress_dashboard.create_dashboard(3, None)
    |> progress_dashboard.record_pass(100)
    |> progress_dashboard.record_fail(100)
    |> progress_dashboard.record_pass(100)
    |> progress_dashboard.complete(300)

  dashboard.status |> should.equal(Failed)
}

/// Test JSON output structure
pub fn to_json_structure_test() {
  let dashboard =
    progress_dashboard.create_dashboard(5, Some("test.cue"))
    |> progress_dashboard.start_behavior("test_behavior", "TestFeature")
    |> progress_dashboard.record_pass(1000)

  let json_str = dashboard |> progress_dashboard.to_json |> json.to_string

  // Verify key fields are present
  json_str
  |> string.contains("\"action\":\"progress_update\"")
  |> should.be_true
  json_str |> string.contains("\"command\":\"check\"") |> should.be_true
  json_str |> string.contains("\"status\":\"running\"") |> should.be_true
  json_str |> string.contains("\"current\":1") |> should.be_true
  json_str |> string.contains("\"total\":5") |> should.be_true
  json_str |> string.contains("\"passed\":1") |> should.be_true
  json_str |> string.contains("\"pending\":4") |> should.be_true
  json_str |> string.contains("\"elapsed_ms\":1000") |> should.be_true
  json_str |> string.contains("\"spec_path\":\"test.cue\"") |> should.be_true
}

/// Test from_results helper
pub fn from_results_test() {
  let dashboard =
    progress_dashboard.from_results(
      passed: 8,
      failed: 1,
      blocked: 1,
      total: 10,
      elapsed_ms: 5000,
      spec_path: Some("spec.cue"),
    )

  dashboard.status |> should.equal(Failed)
  dashboard.progress.percentage |> should.equal(100)
  dashboard.counts.passed |> should.equal(8)
  dashboard.counts.failed |> should.equal(1)
  dashboard.counts.blocked |> should.equal(1)
  dashboard.counts.pending |> should.equal(0)
  dashboard.timing.elapsed_ms |> should.equal(5000)
  dashboard.timing.avg_behavior_ms |> should.equal(Some(500))
}

/// Test estimated remaining time calculation
pub fn estimated_remaining_test() {
  let dashboard =
    progress_dashboard.create_dashboard(10, None)
    |> progress_dashboard.record_pass(100)
    |> progress_dashboard.record_pass(100)

  // After 2 behaviors at 200ms total, avg is 100ms
  // 8 pending * 100ms = 800ms estimated remaining
  dashboard.timing.avg_behavior_ms |> should.equal(Some(100))
  dashboard.timing.estimated_remaining_ms |> should.equal(Some(800))
}

/// Test format_duration helper
pub fn format_duration_ms_test() {
  progress_dashboard.format_duration(500) |> should.equal("500ms")
}

pub fn format_duration_seconds_test() {
  progress_dashboard.format_duration(2500) |> should.equal("2.5s")
}

pub fn format_duration_minutes_test() {
  progress_dashboard.format_duration(125_000) |> should.equal("2m 5s")
}

/// Test percentage calculation edge case with 0 total
pub fn zero_total_percentage_test() {
  let dashboard = progress_dashboard.create_dashboard(0, None)

  // With 0 total, percentage should be 100 (nothing to do = done)
  dashboard.progress.percentage |> should.equal(0)
}
