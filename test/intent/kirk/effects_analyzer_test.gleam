//// Test coverage for intent/kirk/effects_analyzer.gleam
////
//// Tests the second-order effects analysis system, including:
//// - DELETE behavior orphan detection
//// - POST behavior create effects
//// - Cascade operation warnings
//// - State dependency tracking
//// - Report formatting
////
//// DbC Postconditions Verified:
//// - total_second_order_effects >= 0
//// - coverage_score in range [0.0, 100.0]
//// - Every behavior in spec appears in behavior_effects

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/effects_analyzer
import intent/types.{
  Behavior, Delete, Get, Post, Put, Request, Response,
}
import test_helpers

// =============================================================================
// EMPTY SPEC TESTS (Edge Case)
// =============================================================================

pub fn analyze_effects_empty_spec_test() {
  // GIVEN: An empty spec with no behaviors
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: Report is empty with 100% coverage (no unverified effects)
  report.behavior_effects
  |> list.length()
  |> should.equal(0)

  report.orphaned_resources
  |> list.length()
  |> should.equal(0)

  report.cascade_warnings
  |> list.length()
  |> should.equal(0)

  report.state_dependencies
  |> list.length()
  |> should.equal(0)

  report.total_second_order_effects
  |> should.equal(0)

  // DbC postcondition: Empty spec = 100% coverage (no missing verifications)
  report.coverage_score
  |> should.equal(100.0)
}

// =============================================================================
// DELETE BEHAVIOR TESTS (Orphan Detection)
// =============================================================================

pub fn analyze_effects_delete_behavior_test() {
  // GIVEN: A spec with a generic DELETE behavior
  let delete_behavior =
    Behavior(
      name: "delete-post",
      intent: "Delete a blog post",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/posts/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([delete_behavior])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: DELETE triggers orphan detection warnings
  report.orphaned_resources
  |> list.length()
  |> should.equal(0)
  // No orphans for generic "post" resource (not user/org)

  // THEN: Behavior appears in effects report
  report.behavior_effects
  |> list.length()
  |> should.equal(1)

  // THEN: DELETE has second-order effects
  let assert [behavior_effects] = report.behavior_effects
  behavior_effects.behavior_name
  |> should.equal("delete-post")

  { list.length(behavior_effects.second_order) > 0 }
  |> should.be_true()
  // DELETE always has effects (orphans, references, audit logs)

  // DbC postcondition: total_effects >= 0
  report.total_second_order_effects
  |> should.not_equal(0)
}

pub fn analyze_effects_user_delete_test() {
  // GIVEN: A DELETE behavior for user resource (critical pattern)
  let user_delete =
    Behavior(
      name: "delete-user",
      intent: "Delete user account",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/users/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([user_delete])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: User DELETE triggers orphaned resource detection
  report.orphaned_resources
  |> list.length()
  |> should.equal(2)
  // user content + user sessions

  // THEN: Orphaned resources include "user content" and "user sessions"
  let orphan_types =
    report.orphaned_resources
    |> list.map(fn(r) { r.resource_type })

  orphan_types
  |> list.contains("user content")
  |> should.be_true()

  orphan_types
  |> list.contains("user sessions")
  |> should.be_true()

  // THEN: User DELETE has CRITICAL severity effects (session invalidation)
  let assert [behavior_effects] = report.behavior_effects

  let has_critical_effect =
    behavior_effects.second_order
    |> list.any(fn(eff) {
      case eff.severity {
        effects_analyzer.Critical -> True
        _ -> False
      }
    })

  has_critical_effect
  |> should.be_true()

  // THEN: Cascade warnings for user operations
  report.cascade_warnings
  |> list.length()
  |> should.equal(1)

  let assert [cascade] = report.cascade_warnings
  cascade.operation
  |> should.equal("delete-user")

  cascade.cascades_to
  |> list.contains("sessions")
  |> should.be_true()
}

pub fn analyze_effects_org_delete_test() {
  // GIVEN: DELETE behavior for organization (cascade pattern)
  let org_delete =
    Behavior(
      name: "delete-organization",
      intent: "Delete organization",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/organizations/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([org_delete])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: Org DELETE triggers orphan detection
  report.orphaned_resources
  |> list.length()
  |> should.equal(2)
  // members + projects

  // THEN: Cascade warnings include members and projects
  let assert [cascade] = report.cascade_warnings

  cascade.cascades_to
  |> list.contains("members")
  |> should.be_true()

  cascade.cascades_to
  |> list.contains("projects")
  |> should.be_true()

  cascade.requires_transaction
  |> should.be_true()
}

// =============================================================================
// CREATE BEHAVIOR TESTS (POST)
// =============================================================================

pub fn analyze_effects_create_behavior_test() {
  // GIVEN: A POST behavior that creates a resource
  let create_behavior =
    Behavior(
      name: "create-user",
      intent: "Create new user",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Post,
        path: "/users",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 201,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([create_behavior])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: POST triggers create effects
  let assert [behavior_effects] = report.behavior_effects

  // THEN: Create effects include: retrievable, appears in listings, updates stats
  let effect_descriptions =
    behavior_effects.second_order
    |> list.map(fn(e) { e.description })

  effect_descriptions
  |> list.any(fn(desc) {
    desc == "Resource can now be retrieved via GET"
  })
  |> should.be_true()

  effect_descriptions
  |> list.any(fn(desc) {
    desc == "Resource appears in listing endpoints"
  })
  |> should.be_true()

  effect_descriptions
  |> list.any(fn(desc) {
    desc == "Resource count/statistics are updated"
  })
  |> should.be_true()

  // THEN: All POST effects are Info severity
  behavior_effects.second_order
  |> list.all(fn(eff) {
    case eff.severity {
      effects_analyzer.Info -> True
      _ -> False
    }
  })
  |> should.be_true()
}

// =============================================================================
// UPDATE BEHAVIOR TESTS (PUT/PATCH)
// =============================================================================

pub fn analyze_effects_update_behavior_test() {
  // GIVEN: A PUT behavior that updates a resource
  let update_behavior =
    Behavior(
      name: "update-user",
      intent: "Update user profile",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Put,
        path: "/users/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([update_behavior])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: PUT triggers update effects
  let assert [behavior_effects] = report.behavior_effects

  let effect_descriptions =
    behavior_effects.second_order
    |> list.map(fn(e) { e.description })

  // THEN: Update effects include cache invalidation
  effect_descriptions
  |> list.any(fn(desc) {
    desc == "Cache entries may need invalidation"
  })
  |> should.be_true()

  // THEN: Update effects include reflection in reads
  effect_descriptions
  |> list.any(fn(desc) {
    desc == "Updated values are reflected in subsequent reads"
  })
  |> should.be_true()

  // THEN: Cascade warning for user update
  report.cascade_warnings
  |> list.length()
  |> should.equal(1)
}

// =============================================================================
// STATE DEPENDENCY TESTS
// =============================================================================

pub fn analyze_effects_with_dependencies_test() {
  // GIVEN: A behavior with `requires` dependencies (state dependency)
  let dependent_behavior =
    Behavior(
      name: "delete-post-after-auth",
      intent: "Delete post as authenticated user",
      notes: "",
      requires: ["authenticate", "verify-ownership"],
      tags: [],
      request: Request(
        method: Delete,
        path: "/posts/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([dependent_behavior])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: Behavior appears in state_dependencies
  report.state_dependencies
  |> list.length()
  |> should.equal(1)

  let assert [state_dep] = report.state_dependencies

  state_dep.behavior
  |> should.equal("delete-post-after-auth")

  state_dep.depends_on
  |> should.equal(["authenticate", "verify-ownership"])

  // THEN: State mutations reflect DELETE operation
  state_dep.state_mutations
  |> should.equal(["removes resource"])

  // THEN: DELETE requires SERIALIZABLE isolation
  state_dep.isolation_level
  |> should.equal("SERIALIZABLE")
}

pub fn analyze_effects_read_isolation_level_test() {
  // GIVEN: A GET behavior (read-only)
  let read_behavior =
    Behavior(
      name: "get-user",
      intent: "Retrieve user",
      notes: "",
      requires: ["authenticate"],
      tags: [],
      request: Request(
        method: Get,
        path: "/users/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([read_behavior])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: State dependency has READ_COMMITTED isolation
  let assert [state_dep] = report.state_dependencies

  state_dep.isolation_level
  |> should.equal("READ_COMMITTED")

  // THEN: No state mutations for GET
  state_dep.state_mutations
  |> should.equal([])
}

// =============================================================================
// CASCADE DETECTION TESTS
// =============================================================================

pub fn analyze_effects_cascade_detection_test() {
  // GIVEN: Multiple behaviors that trigger cascades
  let user_delete =
    Behavior(
      name: "delete-user",
      intent: "Delete user",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/users/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let team_delete =
    Behavior(
      name: "delete-team",
      intent: "Delete team",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/teams/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([user_delete, team_delete])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: Multiple cascade warnings detected
  report.cascade_warnings
  |> list.length()
  |> should.equal(2)

  // THEN: All cascade operations require transactions
  report.cascade_warnings
  |> list.all(fn(c) { c.requires_transaction })
  |> should.be_true()

  // THEN: Cascades are identified
  let cascade_ops =
    report.cascade_warnings
    |> list.map(fn(c) { c.operation })

  cascade_ops
  |> list.contains("delete-user")
  |> should.be_true()

  cascade_ops
  |> list.contains("delete-team")
  |> should.be_true()
}

// =============================================================================
// SPECIAL BEHAVIOR NAME TESTS (Payment, Notification, etc.)
// =============================================================================

pub fn analyze_effects_payment_behavior_test() {
  // GIVEN: A behavior with "payment" in name (critical financial effect)
  let payment_behavior =
    Behavior(
      name: "process-payment",
      intent: "Charge customer card",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Post,
        path: "/payments",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 201,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([payment_behavior])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: Payment behavior has CRITICAL severity effect
  let assert [behavior_effects] = report.behavior_effects

  let has_critical_financial =
    behavior_effects.second_order
    |> list.any(fn(eff) {
      case eff.severity, eff.description {
        effects_analyzer.Critical, desc ->
          desc == "Financial transaction is recorded"
        _, _ -> False
      }
    })

  has_critical_financial
  |> should.be_true()
}

pub fn analyze_effects_notification_behavior_test() {
  // GIVEN: A behavior with "email" in name (external dependency)
  let email_behavior =
    Behavior(
      name: "send-welcome-email",
      intent: "Send welcome email to new user",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Post,
        path: "/emails/welcome",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 202,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([email_behavior])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: Email behavior triggers external dependency effect
  let assert [behavior_effects] = report.behavior_effects

  let has_notification_effect =
    behavior_effects.second_order
    |> list.any(fn(eff) {
      eff.description == "External notification system is triggered"
    })

  has_notification_effect
  |> should.be_true()
}

// =============================================================================
// COVERAGE SCORE TESTS (DbC Postcondition)
// =============================================================================

pub fn analyze_effects_coverage_score_range_test() {
  // GIVEN: A spec with behaviors
  let behaviors = [
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: DbC postcondition - coverage_score in [0.0, 100.0]
  { report.coverage_score >=. 0.0 }
  |> should.be_true()

  { report.coverage_score <=. 100.0 }
  |> should.be_true()
}

pub fn analyze_effects_total_effects_nonnegative_test() {
  // GIVEN: Any spec
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("get-user", []),
  ])

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: DbC postcondition - total_second_order_effects >= 0
  { report.total_second_order_effects >= 0 }
  |> should.be_true()
}

pub fn analyze_effects_all_behaviors_in_report_test() {
  // GIVEN: A spec with multiple behaviors
  let behaviors = [
    test_helpers.make_test_behavior("behavior-a", []),
    test_helpers.make_test_behavior("behavior-b", []),
    test_helpers.make_test_behavior("behavior-c", []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing effects
  let report = effects_analyzer.analyze_effects(spec)

  // THEN: DbC postcondition - every behavior appears in behavior_effects
  report.behavior_effects
  |> list.length()
  |> should.equal(3)

  let behavior_names =
    report.behavior_effects
    |> list.map(fn(be) { be.behavior_name })

  behavior_names
  |> list.contains("behavior-a")
  |> should.be_true()

  behavior_names
  |> list.contains("behavior-b")
  |> should.be_true()

  behavior_names
  |> list.contains("behavior-c")
  |> should.be_true()
}

// =============================================================================
// FORMAT REPORT TESTS
// =============================================================================

pub fn format_report_test() {
  // GIVEN: An effects report with data
  let behavior =
    test_helpers.make_test_behavior_with_method("delete-user", Delete, [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])
  let report = effects_analyzer.analyze_effects(spec)

  // WHEN: Formatting the report
  let formatted = effects_analyzer.format_report(report)

  // THEN: Report produces valid UTF-8 string (DbC invariant)
  formatted
  |> should.not_equal("")

  // THEN: Report contains key sections
  string.contains(formatted, "=== Second-Order Effects Analysis ===")
  |> should.be_true()

  string.contains(formatted, "Total second-order effects identified:")
  |> should.be_true()

  string.contains(formatted, "Verification coverage:")
  |> should.be_true()
}

pub fn format_report_empty_spec_test() {
  // GIVEN: An empty report
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = effects_analyzer.analyze_effects(spec)

  // WHEN: Formatting
  let formatted = effects_analyzer.format_report(report)

  // THEN: Produces valid output without crashing
  string.contains(formatted, "Total second-order effects identified: 0")
  |> should.be_true()

  string.contains(formatted, "100.0%")
  |> should.be_true()
}

pub fn format_report_with_orphans_test() {
  // GIVEN: A report with orphaned resources
  let behavior =
    Behavior(
      name: "delete-user",
      intent: "Delete user",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Delete,
        path: "/users/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])
  let report = effects_analyzer.analyze_effects(spec)

  // WHEN: Formatting
  let formatted = effects_analyzer.format_report(report)

  // THEN: Orphans section is present
  string.contains(formatted, "--- Orphaned Resources Risks ---")
  |> should.be_true()

  string.contains(formatted, "user content")
  |> should.be_true()

  string.contains(formatted, "user sessions")
  |> should.be_true()
}

pub fn format_report_with_cascades_test() {
  // GIVEN: A report with cascade warnings
  let behavior =
    test_helpers.make_test_behavior_with_method("delete-user", Delete, [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])
  let report = effects_analyzer.analyze_effects(spec)

  // WHEN: Formatting
  let formatted = effects_analyzer.format_report(report)

  // THEN: Cascade section is present
  string.contains(formatted, "--- Cascade Operations ---")
  |> should.be_true()

  string.contains(formatted, "[REQUIRES TRANSACTION]")
  |> should.be_true()
}

pub fn format_report_with_dependencies_test() {
  // GIVEN: A report with state dependencies
  let behavior =
    Behavior(
      name: "delete-post",
      intent: "Delete post",
      notes: "",
      requires: ["authenticate"],
      tags: [],
      request: Request(
        method: Delete,
        path: "/posts/{id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 204,
        example: json.null(),
        checks: dict.new(),
        headers: dict.new(),
      ),
      captures: dict.new(),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])
  let report = effects_analyzer.analyze_effects(spec)

  // WHEN: Formatting
  let formatted = effects_analyzer.format_report(report)

  // THEN: State dependencies section is present
  string.contains(formatted, "--- State Dependencies ---")
  |> should.be_true()

  string.contains(formatted, "Depends on:")
  |> should.be_true()

  string.contains(formatted, "authenticate")
  |> should.be_true()

  string.contains(formatted, "SERIALIZABLE")
  |> should.be_true()
}
