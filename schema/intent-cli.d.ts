/**
 * Intent CLI JSON Output Type Definitions
 *
 * Complete TypeScript interfaces for Intent CLI's JSON output format.
 * All commands that support --json=true return these strongly-typed structures.
 *
 * @version 0.1.0
 * @see https://github.com/your-org/intent-cli/blob/main/docs/JSON_SCHEMA.md
 */

// ============================================================================
// Base Response Types
// ============================================================================

/**
 * Base JSON response structure for all Intent CLI commands
 *
 * @template T - Command-specific data type
 */
export interface JsonResponse<T = unknown> {
  /** Whether the command achieved its goal */
  success: boolean;

  /** Type of result (e.g., "check_result", "quality_report") */
  action: string;

  /** Command that produced this output (e.g., "check", "quality") */
  command: string;

  /** Command-specific output data */
  data: T;

  /** Structured errors (empty if success: true) */
  errors: JsonError[];

  /** Suggested follow-up commands for workflow guidance */
  next_actions: NextAction[];

  /** Timestamp, version, exit code, correlation ID, duration */
  metadata: JsonMetadata;

  /** Path to spec file if applicable */
  spec_path: string | null;
}

/**
 * Structured error information
 */
export interface JsonError {
  /** Machine-readable error code */
  code: string;

  /** Human-readable error message */
  message: string;

  /** File path and line number (e.g., "spec.cue:42") */
  location?: string | null;

  /** Suggestion for fixing the error */
  fix_hint?: string | null;

  /** Command to run for automated fix */
  fix_command?: string | null;
}

/**
 * Suggested follow-up command for workflow guidance
 */
export interface NextAction {
  /** Full command to execute */
  command: string;

  /** Why this action is recommended */
  reason: string;
}

/**
 * Metadata included in all JSON responses
 */
export interface JsonMetadata {
  /** ISO 8601 timestamp */
  timestamp: string;

  /** Intent CLI version */
  version: string;

  /** Unix exit code (0=success, 1=fail, 3=invalid, 4=error) */
  exit_code: number;

  /** UUID v4 for request tracing */
  correlation_id: string;

  /** Command execution time in milliseconds */
  duration_ms: number;
}

// ============================================================================
// Common Enums
// ============================================================================

export type Severity = "low" | "medium" | "high" | "critical";

export type LintSeverity = "error" | "warning" | "info";

export type HealthStatus = "ok" | "warning" | "error";

export type GapType =
  | "inversion"
  | "second_order"
  | "checklist"
  | "coverage"
  | "security";

export type EffectSeverity =
  | "info"
  | "warning"
  | "danger"
  | "critical";

export type EffectCategory =
  | "resource_lifecycle"
  | "data_integrity"
  | "system_state"
  | "security_implication"
  | "performance_impact"
  | "external_dependency";

export type BehaviorStatus = "passed" | "failed" | "skipped";

export type LintCategory =
  | "anti_pattern"
  | "vague_rule"
  | "missing_example"
  | "unused_anti_pattern"
  | "naming_convention"
  | "duplicate_behavior";

export type ImprovementType =
  | "add_missing_test"
  | "refine_vague_rule"
  | "add_response_example"
  | "rename_for_clarity"
  | "simplify_rule"
  | "add_explanation";

export type FailureType =
  | "check_failed"
  | "status_mismatch"
  | "connection_error"
  | "timeout";

// ============================================================================
// Command-Specific Data Types
// ============================================================================

/**
 * validate command output
 */
export interface ValidateData {
  /** Whether spec is syntactically valid */
  valid: boolean;

  /** Validation result message */
  message: string;

  /** Basic spec metadata if valid */
  spec?: {
    name: string;
    description: string;
    version: string;
  };
}

/**
 * check command output
 */
export interface CheckData {
  /** Total behaviors tested */
  total: number;

  /** Passed behaviors */
  passed: number;

  /** Failed behaviors */
  failed: number;

  /** Skipped behaviors */
  skipped: number;

  /** Overall test suite result */
  success: boolean;

  /** Total execution time in milliseconds */
  duration_ms: number;

  /** Individual behavior results */
  behaviors: BehaviorResult[];
}

/**
 * Individual behavior test result
 */
export interface BehaviorResult {
  /** Behavior name */
  name: string;

  /** Feature name */
  feature: string;

  /** Test result status */
  status: BehaviorStatus;

  /** Execution time in milliseconds */
  duration_ms: number;

  /** Request details */
  request: {
    method: string;
    path: string;
    url: string;
  };

  /** Response details */
  response: {
    status: number;
    body?: unknown;
  };

  /** Validation check results */
  checks: CheckResult[];

  /** Error message if failed */
  error?: string;
}

/**
 * Individual validation check result
 */
export interface CheckResult {
  /** Field being checked */
  field: string;

  /** Validation rule */
  rule: string;

  /** Expected value */
  expected: unknown;

  /** Actual value */
  actual: unknown;

  /** Check result */
  passed: boolean;

  /** Check explanation */
  why: string;
}

/**
 * quality command output
 */
export interface QualityData {
  /** Aggregate quality score (0-100) */
  overall_score: number;

  /** Test coverage completeness (0-100) */
  coverage_score: number;

  /** Specification clarity (0-100) */
  clarity_score: number;

  /** How testable the spec is (0-100) */
  testability_score: number;

  /** Readiness for AI implementation (0-100) */
  ai_readiness_score: number;

  /** Identified problems */
  issues: string[];

  /** Improvement recommendations */
  suggestions: string[];
}

/**
 * coverage command output
 */
export interface CoverageData {
  /** Overall coverage percentage (0-100) */
  overall_score: number;

  /** HTTP method counts */
  methods: Record<string, number>;

  /** Status code counts */
  status_codes: Record<string, number>;

  /** Paths with their methods */
  paths: Record<string, string[]>;

  /** Edge case coverage */
  edge_cases: {
    tested: string[];
    suggested: string[];
  };

  /** OWASP Top 10 coverage */
  owasp: {
    score: number;
    categories: Record<string, boolean>;
    missing: string[];
  };
}

/**
 * gaps command output
 */
export interface GapsData {
  /** Total gaps found */
  total_gaps: number;

  /** Failure mode gaps */
  inversion_gaps: Gap[];

  /** Cascading effect gaps */
  second_order_gaps: Gap[];

  /** Systematic coverage gaps */
  checklist_gaps: Gap[];

  /** Test coverage gaps */
  coverage_gaps: Gap[];

  /** Security-related gaps */
  security_gaps: Gap[];

  /** Severity distribution */
  severity_breakdown: {
    critical: number;
    high: number;
    medium: number;
    low: number;
  };
}

/**
 * Individual gap description
 */
export interface Gap {
  /** Gap type */
  gap_type: GapType;

  /** What's missing */
  description: string;

  /** Gap severity */
  severity: Severity;

  /** How to fix */
  suggestion: string;

  /** Which thinking model detected this */
  mental_model: string;
}

/**
 * invert command output
 */
export interface InvertData {
  /** Inversion coverage score (0-100) */
  score: number;

  /** Security failure mode gaps */
  security_gaps: InversionGap[];

  /** Usability failure mode gaps */
  usability_gaps: InversionGap[];

  /** Integration failure mode gaps */
  integration_gaps: InversionGap[];

  /** Suggested behaviors to add */
  suggested_behaviors: SuggestedBehavior[];
}

/**
 * Failure mode gap
 */
export interface InversionGap {
  /** Gap category */
  category: string;

  /** What's missing */
  description: string;

  /** Gap severity */
  severity: Severity;

  /** Failure scenario */
  what_could_fail: string;
}

/**
 * Suggested behavior to add
 */
export interface SuggestedBehavior {
  /** Behavior name */
  name: string;

  /** What to test */
  intent: string;

  /** HTTP method */
  method: string;

  /** Request path */
  path: string;

  /** Expected status code */
  expected_status: number;

  /** Category */
  category: string;
}

/**
 * effects command output
 */
export interface EffectsData {
  /** Total effects identified */
  total_second_order_effects: number;

  /** Verification coverage percentage (0-100) */
  coverage_score: number;

  /** Effects by behavior */
  behavior_effects: BehaviorEffects[];

  /** Resources left orphaned */
  orphaned_resources: OrphanedResource[];

  /** Cascade operation warnings */
  cascade_warnings: CascadeWarning[];

  /** State mutation dependencies */
  state_dependencies: StateDependency[];
}

/**
 * Effects for a specific behavior
 */
export interface BehaviorEffects {
  /** Behavior name */
  behavior_name: string;

  /** Direct effect */
  first_order: string;

  /** Cascading effects */
  second_order: SecondOrderEffect[];

  /** Missing verification behaviors */
  missing_verifications: string[];
}

/**
 * Second-order effect
 */
export interface SecondOrderEffect {
  /** Effect description */
  description: string;

  /** Effect severity */
  severity: EffectSeverity;

  /** Effect category */
  category: EffectCategory;

  /** Whether verification exists */
  has_verification: boolean;
}

/**
 * Orphaned resource
 */
export interface OrphanedResource {
  /** Resource type */
  resource_type: string;

  /** Causing behavior */
  caused_by: string;

  /** Description */
  description: string;

  /** Mitigation strategy */
  mitigation: string;
}

/**
 * Cascade operation warning
 */
export interface CascadeWarning {
  /** Operation name */
  operation: string;

  /** Affected resources */
  cascades_to: string[];

  /** Whether atomic transaction needed */
  requires_transaction: boolean;

  /** Warning description */
  description: string;
}

/**
 * State mutation dependency
 */
export interface StateDependency {
  /** Behavior name */
  behavior: string;

  /** Required preconditions */
  depends_on: string[];

  /** State fields mutated */
  state_mutations: string[];

  /** Transaction isolation level */
  isolation_level: string;
}

/**
 * doctor command output
 */
export interface DoctorData {
  /** Quality scores and issues */
  quality: {
    overall_score: number;
    coverage_score: number;
    clarity_score: number;
    testability_score: number;
    ai_readiness_score: number;
    issues: string[];
  };

  /** Linting results */
  lint: {
    status: "valid" | "warnings";
    warnings: LintWarning[];
  };

  /** Prioritized suggestions (sorted by impact_score) */
  suggestions: DoctorSuggestion[];
}

/**
 * Prioritized improvement suggestion
 */
export interface DoctorSuggestion {
  /** Suggestion title */
  title: string;

  /** Detailed description */
  description: string;

  /** Why this matters */
  reasoning: string;

  /** Priority ranking (0-100) */
  impact_score: number;
}

/**
 * lint command output
 */
export interface LintData {
  /** Linting status */
  status: "valid" | "warnings";

  /** Lint warnings */
  warnings: LintWarning[];
}

/**
 * Lint warning
 */
export interface LintWarning {
  /** Warning severity */
  severity: LintSeverity;

  /** Warning category */
  category: LintCategory;

  /** Warning message */
  message: string;

  /** Location information (flexible fields) */
  location: {
    behavior?: string;
    field?: string;
    anti_pattern?: string;
    behavior1?: string;
    behavior2?: string;
    [key: string]: string | undefined;
  };
}

/**
 * improve command output
 */
export interface ImproveData {
  /** Improvement suggestions (sorted by impact_score) */
  suggestions: ImprovementSuggestion[];

  /** Total suggestion count */
  total_count: number;
}

/**
 * Concrete improvement suggestion
 */
export interface ImprovementSuggestion {
  /** Suggestion title */
  title: string;

  /** Detailed description */
  description: string;

  /** Reasoning */
  reasoning: string;

  /** Priority ranking (0-100) */
  impact_score: number;

  /** Proposed change details */
  proposed_change: {
    type: ImprovementType;
    behavior_name?: string;
    field?: string;
    old_name?: string;
    new_name?: string;
    test_description?: string;
    better_rule?: string;
    simpler_rule?: string;
    explanation?: string;
    example?: unknown;
    [key: string]: unknown;
  };
}

/**
 * prompt command output
 */
export interface PromptData {
  /** Session ID */
  session_id: string;

  /** Implementation prompts */
  prompts: ImplementationPrompt[];

  /** Total prompt count */
  total: number;
}

/**
 * AI implementation prompt
 */
export interface ImplementationPrompt {
  /** Bead ID */
  bead_id: string;

  /** Task summary */
  task_summary: string;

  /** Context section */
  context_section: string;

  /** Requirements list */
  requirements: string[];

  /** Acceptance criteria */
  acceptance_criteria: string[];

  /** Relevant code files */
  relevant_code: FileContext[];

  /** Suggested implementation approach */
  suggested_approach: string;

  /** Things to avoid */
  pitfalls_to_avoid: string[];

  /** Critical guardrails */
  guardrail_block: string;

  /** Verification steps */
  verification_steps: string[];
}

/**
 * File context for implementation
 */
export interface FileContext {
  /** File path */
  path: string;

  /** Programming language */
  language: string;

  /** File purpose */
  purpose: string;

  /** Code snippet (optional) */
  content_snippet?: string;

  /** Relevant line references (optional) */
  relevant_lines?: LineReference[];
}

/**
 * Line reference in code
 */
export interface LineReference {
  /** Line number */
  line_number: number;

  /** Line content */
  content: string;

  /** Why this line is relevant */
  reason: string;
}

/**
 * feedback command output
 */
export interface FeedbackData {
  /** Input check results file */
  source_file: string;

  /** Generated fix beads */
  fix_beads: FixBead[];

  /** Total fixes generated */
  total_fixes: number;

  /** Behaviors analyzed */
  behaviors_analyzed: number;
}

/**
 * Fix bead for failed behavior
 */
export interface FixBead {
  /** Behavior name */
  behavior_name: string;

  /** Feature name */
  feature: string;

  /** Failure type */
  failure_type: FailureType;

  /** Failure description */
  description: string;

  /** Priority (0-100) */
  priority: number;

  /** Fix suggestion */
  fix_suggestion: string;

  /** Related validation checks */
  related_checks: string[];
}

/**
 * beads command output
 */
export interface BeadsData {
  /** Session ID */
  session_id: string;

  /** Generated beads */
  beads: BeadRecord[];

  /** Total bead count */
  total: number;
}

/**
 * Work item bead
 */
export interface BeadRecord {
  /** Bead title */
  title: string;

  /** Detailed description */
  description: string;

  /** Profile type */
  profile_type: string;

  /** Priority (0-100) */
  priority: number;

  /** Issue type */
  issue_type: string;

  /** Labels */
  labels: string[];

  /** AI hints */
  ai_hints: string;

  /** Acceptance criteria */
  acceptance_criteria: string[];

  /** Dependencies */
  dependencies: string[];
}

/**
 * ready start command output
 */
export interface ReadyStartData {
  /** Session ID */
  session_id: string;

  /** Spec file path */
  spec_path: string;

  /** Phase name */
  phase: string;

  /** Session status */
  status: string;

  /** Creation timestamp */
  created_at: string;
}

// ============================================================================
// Type-Safe Response Types
// ============================================================================

export type ValidateResponse = JsonResponse<ValidateData>;
export type CheckResponse = JsonResponse<CheckData>;
export type QualityResponse = JsonResponse<QualityData>;
export type CoverageResponse = JsonResponse<CoverageData>;
export type GapsResponse = JsonResponse<GapsData>;
export type InvertResponse = JsonResponse<InvertData>;
export type EffectsResponse = JsonResponse<EffectsData>;
export type DoctorResponse = JsonResponse<DoctorData>;
export type LintResponse = JsonResponse<LintData>;
export type ImproveResponse = JsonResponse<ImproveData>;
export type PromptResponse = JsonResponse<PromptData>;
export type FeedbackResponse = JsonResponse<FeedbackData>;
export type BeadsResponse = JsonResponse<BeadsData>;
export type ReadyStartResponse = JsonResponse<ReadyStartData>;

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Parse Intent CLI JSON output with type safety
 *
 * @template T - Expected data type
 * @param json - JSON string from Intent CLI
 * @returns Parsed JSON response
 *
 * @example
 * ```typescript
 * const json = await execIntentCommand(['quality', 'spec.cue']);
 * const response = parseIntentOutput<QualityData>(json);
 * console.log(`Score: ${response.data.overall_score}`);
 * ```
 */
export function parseIntentOutput<T>(json: string): JsonResponse<T> {
  return JSON.parse(json) as JsonResponse<T>;
}

/**
 * Check if response indicates success
 *
 * @param response - JSON response from Intent CLI
 * @returns true if command succeeded
 *
 * @example
 * ```typescript
 * const response = parseIntentOutput<CheckData>(json);
 * if (isSuccess(response)) {
 *   console.log('All checks passed!');
 * }
 * ```
 */
export function isSuccess<T>(response: JsonResponse<T>): boolean {
  return response.success && response.metadata.exit_code === 0;
}

/**
 * Extract error messages from response
 *
 * @param response - JSON response from Intent CLI
 * @returns Array of error messages
 *
 * @example
 * ```typescript
 * const response = parseIntentOutput<ValidateData>(json);
 * if (!isSuccess(response)) {
 *   const errors = getErrorMessages(response);
 *   console.error('Errors:', errors.join('\n'));
 * }
 * ```
 */
export function getErrorMessages<T>(response: JsonResponse<T>): string[] {
  return response.errors.map(err => err.message);
}

/**
 * Get next action commands
 *
 * @param response - JSON response from Intent CLI
 * @returns Array of suggested commands
 *
 * @example
 * ```typescript
 * const response = parseIntentOutput<QualityData>(json);
 * const nextCommands = getNextActions(response);
 * console.log('Suggested:', nextCommands);
 * ```
 */
export function getNextActions<T>(response: JsonResponse<T>): string[] {
  return response.next_actions.map(action => action.command);
}

/**
 * Get exit code from response
 *
 * @param response - JSON response from Intent CLI
 * @returns Exit code (0=success, 1=fail, 3=invalid, 4=error)
 *
 * @example
 * ```typescript
 * const response = parseIntentOutput<CheckData>(json);
 * const exitCode = getExitCode(response);
 * process.exit(exitCode);
 * ```
 */
export function getExitCode<T>(response: JsonResponse<T>): number {
  return response.metadata.exit_code;
}

/**
 * Get correlation ID for tracing
 *
 * @param response - JSON response from Intent CLI
 * @returns UUID correlation ID
 *
 * @example
 * ```typescript
 * const response = parseIntentOutput<QualityData>(json);
 * const correlationId = getCorrelationId(response);
 * logger.info(`Quality check ${correlationId}: score=${response.data.overall_score}`);
 * ```
 */
export function getCorrelationId<T>(response: JsonResponse<T>): string {
  return response.metadata.correlation_id;
}

/**
 * Check if response has errors
 *
 * @param response - JSON response from Intent CLI
 * @returns true if errors present
 */
export function hasErrors<T>(response: JsonResponse<T>): boolean {
  return response.errors.length > 0;
}

/**
 * Get command execution duration
 *
 * @param response - JSON response from Intent CLI
 * @returns Duration in milliseconds
 */
export function getDuration<T>(response: JsonResponse<T>): number {
  return response.metadata.duration_ms;
}
