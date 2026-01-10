/// Regeneration Strategies Module
///
/// Implements 4 explicit strategies for regenerating failed beads:
/// 1. Inversion-driven: Analyze failures by inverting expectations
/// 2. Second-order-driven: Trace cascading effects of failures
/// 3. Pre-mortem-driven: Work backwards from imagined failure
/// 4. Hybrid: Combine all three methods for comprehensive analysis
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string

/// Regeneration strategy types
pub type RegenerationStrategy {
  InversionDriven
  SecondOrderDriven
  PremortemDriven
  Hybrid
  NotApplied
}

/// Result of applying a regeneration strategy
pub type RegenerationResult {
  RegenerationResult(
    strategy: RegenerationStrategy,
    insights: List(String),
    suggested_changes: List(String),
    confidence: Float,
  )
}

/// Error information from failed bead execution
pub type BeadError {
  BeadError(error_type: String, message: String, context: Option(String))
}

/// Convert strategy to string representation
pub fn strategy_to_string(strategy: RegenerationStrategy) -> String {
  case strategy {
    InversionDriven -> "inversion_driven"
    SecondOrderDriven -> "second_order_driven"
    PremortemDriven -> "premortem_driven"
    Hybrid -> "hybrid"
    NotApplied -> "not_applied"
  }
}

/// Parse strategy from string
pub fn strategy_from_string(s: String) -> Result(RegenerationStrategy, String) {
  case string.lowercase(s) {
    "inversion" | "inversion_driven" -> Ok(InversionDriven)
    "second_order" | "second_order_driven" -> Ok(SecondOrderDriven)
    "premortem" | "premortem_driven" -> Ok(PremortemDriven)
    "hybrid" -> Ok(Hybrid)
    _ ->
      Error(
        "Unknown strategy: "
        <> s
        <> ". Valid: inversion, second_order, premortem, hybrid",
      )
  }
}

/// Auto-select strategy based on error type
pub fn auto_select_strategy(error: BeadError) -> RegenerationStrategy {
  case error.error_type {
    "security" -> InversionDriven
    "auth" -> InversionDriven
    "validation" -> InversionDriven
    "integration" -> SecondOrderDriven
    "dependency" -> SecondOrderDriven
    "network" -> SecondOrderDriven
    "timeout" -> PremortemDriven
    "resource" -> PremortemDriven
    _ -> Hybrid
  }
}

/// Apply inversion-driven analysis
/// Ask: "What would make this fail?" then prevent that
pub fn apply_inversion(
  error: BeadError,
  bead_intent: String,
) -> RegenerationResult {
  let insights = [
    "Inverted expectation: Instead of success, what failures are possible?",
    "Error type '"
      <> error.error_type
      <> "' suggests checking: "
      <> inversion_checks(error.error_type),
  ]

  let changes = [
    "Add explicit failure handling for: " <> error.error_type,
    "Add edge case test: when " <> bead_intent <> " with invalid input",
    "Verify preconditions before execution",
  ]

  RegenerationResult(
    strategy: InversionDriven,
    insights: insights,
    suggested_changes: changes,
    confidence: 0.75,
  )
}

fn inversion_checks(error_type: String) -> String {
  case error_type {
    "security" -> "authentication, authorization, input sanitization"
    "auth" -> "token validity, permission scope, session state"
    "validation" -> "required fields, type constraints, format rules"
    _ -> "error handling, boundary conditions, resource limits"
  }
}

/// Apply second-order-driven analysis
/// Ask: "What happened after the initial failure?"
pub fn apply_second_order(
  error: BeadError,
  dependencies: List(String),
) -> RegenerationResult {
  let dep_analysis = case dependencies {
    [] -> "No dependencies - failure is isolated"
    deps -> "Cascading effects on: " <> string.join(deps, ", ")
  }

  let insights = [
    "Tracing second-order effects of failure",
    dep_analysis,
    "Error '" <> error.message <> "' may have triggered downstream issues",
  ]

  let changes = [
    "Add dependency health check before execution",
    "Implement rollback mechanism for partial failures",
    "Add circuit breaker for cascading failure prevention",
  ]

  RegenerationResult(
    strategy: SecondOrderDriven,
    insights: insights,
    suggested_changes: changes,
    confidence: 0.7,
  )
}

/// Apply pre-mortem-driven analysis
/// Ask: "We failed - what was the root cause?"
pub fn apply_premortem(error: BeadError) -> RegenerationResult {
  let context_hint = case error.context {
    Some(ctx) -> "Context suggests: " <> ctx
    None -> "No context provided - consider adding error context"
  }

  let insights = [
    "Pre-mortem analysis: Imagining we already failed",
    "Root cause hypothesis: " <> error.message,
    context_hint,
  ]

  let changes = [
    "Add more detailed error context to failure points",
    "Implement retry logic with exponential backoff",
    "Add monitoring/alerting for early detection",
  ]

  RegenerationResult(
    strategy: PremortemDriven,
    insights: insights,
    suggested_changes: changes,
    confidence: 0.65,
  )
}

/// Apply hybrid analysis - combines all three methods
pub fn apply_hybrid(
  error: BeadError,
  bead_intent: String,
  dependencies: List(String),
) -> RegenerationResult {
  let inversion_result = apply_inversion(error, bead_intent)
  let second_order_result = apply_second_order(error, dependencies)
  let premortem_result = apply_premortem(error)

  // Combine insights from all three
  let all_insights =
    list.concat([
      ["=== HYBRID ANALYSIS ==="],
      ["--- Inversion ---"],
      inversion_result.insights,
      ["--- Second Order ---"],
      second_order_result.insights,
      ["--- Pre-mortem ---"],
      premortem_result.insights,
    ])

  // Deduplicate and combine changes
  let all_changes =
    list.concat([
      inversion_result.suggested_changes,
      second_order_result.suggested_changes,
      premortem_result.suggested_changes,
    ])
    |> list.unique()

  // Hybrid has highest confidence due to comprehensive analysis
  let avg_confidence =
    {
      inversion_result.confidence
      +. second_order_result.confidence
      +. premortem_result.confidence
    }
    /. 3.0

  RegenerationResult(
    strategy: Hybrid,
    insights: all_insights,
    suggested_changes: all_changes,
    confidence: avg_confidence +. 0.1,
    // Boost for comprehensive analysis
  )
}

/// Main regeneration entry point
pub fn regenerate(
  error: BeadError,
  bead_intent: String,
  dependencies: List(String),
  strategy: Option(RegenerationStrategy),
) -> RegenerationResult {
  let selected_strategy = case strategy {
    Some(s) -> s
    None -> auto_select_strategy(error)
  }

  case selected_strategy {
    InversionDriven -> apply_inversion(error, bead_intent)
    SecondOrderDriven -> apply_second_order(error, dependencies)
    PremortemDriven -> apply_premortem(error)
    Hybrid -> apply_hybrid(error, bead_intent, dependencies)
    NotApplied ->
      RegenerationResult(
        strategy: NotApplied,
        insights: ["No regeneration applied"],
        suggested_changes: [],
        confidence: 0.0,
      )
  }
}

/// Format regeneration result for display
pub fn format_result(result: RegenerationResult) -> String {
  let strategy_str = "Strategy: " <> strategy_to_string(result.strategy)
  let confidence_str = "Confidence: " <> float_to_percent(result.confidence)

  let insights_str = case result.insights {
    [] -> "  (no insights)"
    insights ->
      insights
      |> list.map(fn(i) { "  - " <> i })
      |> string.join("\n")
  }

  let changes_str = case result.suggested_changes {
    [] -> "  (no changes suggested)"
    changes ->
      changes
      |> list.map(fn(c) { "  - " <> c })
      |> string.join("\n")
  }

  strategy_str
  <> "\n"
  <> confidence_str
  <> "\n\n"
  <> "Insights:\n"
  <> insights_str
  <> "\n\n"
  <> "Suggested Changes:\n"
  <> changes_str
}

fn float_to_percent(f: Float) -> String {
  let percent = f *. 100.0
  // Simple float formatting without external deps
  case float.compare(percent, 80.0) {
    order.Gt | order.Eq -> "High (80%+)"
    _ ->
      case float.compare(percent, 60.0) {
        order.Gt | order.Eq -> "Medium (60-80%)"
        _ -> "Low (<60%)"
      }
  }
}
