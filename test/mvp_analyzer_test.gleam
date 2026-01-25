//// Tests for kirk/mvp_analyzer.gleam
//// Contract: MVP critical path analysis and feature prioritization

import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/mvp_analyzer
import intent/planning_types.{
  type ShapeSection, FeatureShape, MVPSlice, ShapeSection,
}

// =============================================================================
// analyze_mvp tests
// =============================================================================

pub fn analyze_mvp_empty_shape_test() {
  // Contract: Empty shape returns report with no critical features
  let shape = make_empty_shape()

  let report = mvp_analyzer.analyze_mvp(shape)

  report.critical_path_features |> should.equal([])
  report.total_features |> should.equal(0)
  report.critical_count |> should.equal(0)
  report.mvp_clarity_score |> should.equal(0)
}

pub fn analyze_mvp_with_critical_path_test() {
  // Contract: Features on critical_path are identified
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Auth", description: "User authentication"),
        FeatureShape(name: "Profile", description: "User profiles"),
        FeatureShape(name: "Settings", description: "User settings"),
      ],
      critical_path: ["Auth", "Profile"],
      mvp_slice: MVPSlice(
        description: "Minimum auth flow",
        features: ["Auth"],
        shortcuts: [],
      ),
      post_mvp: ["Settings"],
      validation_moment: "User can login and view profile",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  // Should identify 2 critical features
  report.critical_count |> should.equal(2)
  report.total_features |> should.equal(3)

  // Critical features should be Auth and Profile
  let critical_names =
    report.critical_path_features
    |> list.map(fn(f) { f.name })

  critical_names |> list.contains("Auth") |> should.be_true
  critical_names |> list.contains("Profile") |> should.be_true
  critical_names |> list.contains("Settings") |> should.be_false
}

pub fn analyze_mvp_all_critical_test() {
  // Contract: All features can be on critical path
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Login", description: "Login flow"),
        FeatureShape(name: "Register", description: "Registration flow"),
      ],
      critical_path: ["Login", "Register"],
      mvp_slice: MVPSlice(
        description: "Basic auth",
        features: ["Login", "Register"],
        shortcuts: [],
      ),
      post_mvp: [],
      validation_moment: "User can register and login",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  report.critical_count |> should.equal(2)
  report.total_features |> should.equal(2)
  report.critical_ratio |> should.equal(100.0)
}

pub fn analyze_mvp_no_critical_test() {
  // Contract: No critical path = low clarity score
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Feature1", description: "Some feature"),
        FeatureShape(name: "Feature2", description: "Another feature"),
      ],
      critical_path: [],
      mvp_slice: MVPSlice(description: "", features: [], shortcuts: []),
      post_mvp: [],
      validation_moment: "",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  report.critical_count |> should.equal(0)
  report.critical_ratio |> should.equal(0.0)
  // Low clarity score when no critical path defined
  { report.mvp_clarity_score < 50 } |> should.be_true
}

// =============================================================================
// MVP clarity scoring tests
// =============================================================================

pub fn clarity_score_well_defined_test() {
  // Contract: Well-defined MVP gets high clarity score
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Auth", description: "User authentication with JWT"),
        FeatureShape(name: "Profile", description: "User profile management"),
        FeatureShape(name: "Billing", description: "Payment processing"),
      ],
      critical_path: ["Auth", "Profile"],
      mvp_slice: MVPSlice(
        description: "Minimal viable authentication flow",
        features: ["Auth"],
        shortcuts: ["No OAuth integration", "No social login"],
      ),
      post_mvp: ["Profile", "Billing"],
      validation_moment: "User can register, login, and see their email",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  // Well-defined MVP should score high (>= 65)
  // Score breakdown: critical_path(25) + validation(25) + shortcuts(20) + post_mvp(15) + focus(15) = 100
  { report.mvp_clarity_score >= 65 } |> should.be_true
}

pub fn clarity_score_missing_validation_moment_test() {
  // Contract: Missing validation_moment reduces clarity score
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Feature1", description: "A feature"),
      ],
      critical_path: ["Feature1"],
      mvp_slice: MVPSlice(
        description: "Some MVP",
        features: ["Feature1"],
        shortcuts: [],
      ),
      post_mvp: [],
      validation_moment: "",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  // Should have an issue about missing validation moment
  report.issues
  |> list.any(fn(issue) {
    string.contains(string.lowercase(issue), "validation")
  })
  |> should.be_true
}

pub fn clarity_score_missing_shortcuts_test() {
  // Contract: No shortcuts documented indicates unclear scope
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Auth", description: "Authentication"),
        FeatureShape(name: "Billing", description: "Payment processing"),
      ],
      critical_path: ["Auth", "Billing"],
      mvp_slice: MVPSlice(
        description: "Full product",
        features: ["Auth", "Billing"],
        shortcuts: [],
      ),
      post_mvp: [],
      validation_moment: "User can pay",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  // No shortcuts = potentially unclear scope
  report.issues
  |> list.any(fn(issue) { string.contains(string.lowercase(issue), "shortcut") })
  |> should.be_true
}

// =============================================================================
// Critical path feature details tests
// =============================================================================

pub fn critical_feature_includes_details_test() {
  // Contract: Critical features include name, description, and reason
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Auth", description: "User authentication"),
      ],
      critical_path: ["Auth"],
      mvp_slice: MVPSlice(
        description: "Auth only",
        features: ["Auth"],
        shortcuts: [],
      ),
      post_mvp: [],
      validation_moment: "Login works",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  case list.first(report.critical_path_features) {
    Ok(feature) -> {
      feature.name |> should.equal("Auth")
      feature.description |> should.equal("User authentication")
      // Should have a reason for being critical
      feature.reason |> string.is_empty |> should.be_false
    }
    Error(_) -> should.fail()
  }
}

pub fn critical_feature_in_mvp_slice_test() {
  // Contract: Features in mvp_slice.features are marked as such
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Auth", description: "Authentication"),
        FeatureShape(name: "Profile", description: "Profiles"),
      ],
      critical_path: ["Auth", "Profile"],
      mvp_slice: MVPSlice(
        description: "Auth MVP",
        features: ["Auth"],
        shortcuts: [],
      ),
      post_mvp: [],
      validation_moment: "Login",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  // Auth should be in_mvp_slice, Profile should not
  let auth =
    report.critical_path_features
    |> list.find(fn(f) { f.name == "Auth" })

  let profile =
    report.critical_path_features
    |> list.find(fn(f) { f.name == "Profile" })

  case auth, profile {
    Ok(a), Ok(p) -> {
      a.in_mvp_slice |> should.be_true
      p.in_mvp_slice |> should.be_false
    }
    _, _ -> should.fail()
  }
}

// =============================================================================
// Recommendations tests
// =============================================================================

pub fn recommendations_for_unclear_mvp_test() {
  // Contract: Recommendations generated for low clarity MVPs
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "F1", description: ""),
        FeatureShape(name: "F2", description: ""),
      ],
      critical_path: [],
      mvp_slice: MVPSlice(description: "", features: [], shortcuts: []),
      post_mvp: [],
      validation_moment: "",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  // Should have recommendations
  report.recommendations |> list.is_empty |> should.be_false
}

pub fn recommendations_for_large_mvp_test() {
  // Contract: Warning when MVP includes too many features
  let many_features =
    ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"]
    |> list.map(fn(name) { FeatureShape(name: name, description: "Feature") })

  let shape =
    ShapeSection(
      features: many_features,
      critical_path: ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"],
      mvp_slice: MVPSlice(
        description: "Everything",
        features: ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"],
        shortcuts: [],
      ),
      post_mvp: [],
      validation_moment: "All done",
    )

  let report = mvp_analyzer.analyze_mvp(shape)

  // Should warn about large MVP scope
  let has_scope_warning =
    report.recommendations
    |> list.any(fn(rec) {
      string.contains(string.lowercase(rec), "scope")
      || string.contains(string.lowercase(rec), "reduce")
      || string.contains(string.lowercase(rec), "large")
    })

  has_scope_warning |> should.be_true
}

// =============================================================================
// format_report tests
// =============================================================================

pub fn format_report_empty_test() {
  // Contract: Empty report formats without crashing
  let shape = make_empty_shape()
  let report = mvp_analyzer.analyze_mvp(shape)

  let formatted = mvp_analyzer.format_report(report)

  // Should produce valid string output
  formatted |> string.is_empty |> should.be_false
}

pub fn format_report_includes_clarity_score_test() {
  // Contract: Report includes clarity score
  let shape =
    ShapeSection(
      features: [FeatureShape(name: "Auth", description: "Auth")],
      critical_path: ["Auth"],
      mvp_slice: MVPSlice(description: "MVP", features: ["Auth"], shortcuts: []),
      post_mvp: [],
      validation_moment: "Login works",
    )
  let report = mvp_analyzer.analyze_mvp(shape)

  let formatted = mvp_analyzer.format_report(report)

  // Should mention clarity somewhere in the report
  formatted |> string.lowercase |> string.contains("clarity") |> should.be_true
}

pub fn format_report_includes_critical_features_test() {
  // Contract: Report lists critical features
  let shape =
    ShapeSection(
      features: [
        FeatureShape(name: "Auth", description: "Authentication"),
        FeatureShape(name: "Dashboard", description: "Main dashboard"),
      ],
      critical_path: ["Auth"],
      mvp_slice: MVPSlice(description: "MVP", features: ["Auth"], shortcuts: []),
      post_mvp: ["Dashboard"],
      validation_moment: "Can login",
    )
  let report = mvp_analyzer.analyze_mvp(shape)

  let formatted = mvp_analyzer.format_report(report)

  // Should include critical feature name
  formatted |> string.contains("Auth") |> should.be_true
}

// =============================================================================
// JSON output tests
// =============================================================================

pub fn mvp_report_to_json_test() {
  // Contract: Report converts to JSON correctly
  let shape =
    ShapeSection(
      features: [FeatureShape(name: "Auth", description: "Auth")],
      critical_path: ["Auth"],
      mvp_slice: MVPSlice(description: "MVP", features: ["Auth"], shortcuts: []),
      post_mvp: [],
      validation_moment: "Login",
    )
  let report = mvp_analyzer.analyze_mvp(shape)

  let json_output = mvp_analyzer.mvp_report_to_action_json(report, "test-plan")

  // Should produce valid JSON (no crash)
  let json_str = json.to_string(json_output)
  json_str |> string.is_empty |> should.be_false

  // Should include expected fields
  json_str |> string.contains("clarity") |> should.be_true
  json_str |> string.contains("critical") |> should.be_true
}

// =============================================================================
// Helper functions
// =============================================================================

fn make_empty_shape() -> ShapeSection {
  ShapeSection(
    features: [],
    critical_path: [],
    mvp_slice: MVPSlice(description: "", features: [], shortcuts: []),
    post_mvp: [],
    validation_moment: "",
  )
}
