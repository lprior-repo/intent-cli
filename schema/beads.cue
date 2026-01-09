// schema/beads.cue - Atomic Work Unit Schema
// CUE is the center of the universe - beads are typed work contracts
//
// Architecture: Option B (Append-Friendly via CUE Unification)
// - Session file: .intent/session-{id}.cue (interview state, beads)
// - Feedback file: .intent/feedback-{id}.cue (append execution results)
// - CUE unifies these automatically: cue export .intent/*-{id}.cue

package intent

import "list"

// =============================================================================
// BEAD: Atomic Work Unit (5-30 min of focused work)
// =============================================================================

#Bead: {
	// Identity
	id:    #BeadID
	title: string

	// What to implement (imperative, actionable)
	what: string

	// Why it matters (business value, user impact)
	why: string

	// How to verify completion
	test: #TestSpec

	// Observable completion criteria
	done_when: [...string] & list.MinItems(1)

	// Primary file to modify
	file: string

	// Edge cases to handle
	edge_cases: [...string]

	// Dependencies (must complete first)
	requires: [...#BeadID]

	// Effort estimate
	effort: #Effort

	// Current state
	status: #BeadStatus

	// =========== MENTAL LATTICE EXTENSIONS ===========
	// Optional KIRK metadata: EARS clarity, KIRK contracts, inversions, quality
	kirk?: #BeadKirkMetadata

	// =========== AI-CUE INTEGRATION ===========
	// Optional codebase context for AI to understand where to edit
	codebase?: #BeadCodebaseContext

	// AI-friendly format fields (BEAD-FORMAT)
	input_example?:  string
	output_example?: string
	must_return?:    [...string]
	must_not?:       [...string]
}

// Simplified codebase context for beads (references full #CodebaseContext in intent.cue)
#BeadCodebaseContext: {
	// Language and framework detected
	language?:  string
	framework?: string

	// Entry point for this bead's implementation
	entry_point?: {
		name: string
		path: string
	}

	// Boundaries this bead should respect
	boundaries?: [...{
		name:    string
		modules: [...string]
	}]
}

// Bead ID format: PREFIX-NNN (e.g., AUTH-001, API-042, CUE-007)
#BeadID: =~"^[A-Z]+-[0-9]{3}$"

#Effort: "5min" | "10min" | "15min" | "20min" | "30min"

#BeadStatus: "pending" | "in_progress" | "blocked" | "completed" | "failed"

// =============================================================================
// TEST SPEC: Verification Contract
// =============================================================================

#TestSpec: {
	// Command to execute (or "manual" for human verification)
	command: string

	// What constitutes success
	expect: #Expectation
}

#Expectation: {
	// Command-based verification
	exit_code?:       int
	stdout_contains?: string
	stdout_matches?:  string // regex
	stderr_empty?:    bool

	// HTTP-based verification
	status?:         int
	body_contains?:  string
	header_present?: string
	header_equals?: {[string]: string}
}

// =============================================================================
// BEAD FEEDBACK: Execution Results (Append-Only)
// =============================================================================

#BeadFeedback: {
	bead_id:     #BeadID
	result:      #BeadResult
	reason:      string
	executed_at: string // ISO8601
	duration_ms: int & >=0

	// On failure: what went wrong
	error?: {
		type:    string
		message: string
		trace?:  string
	}

	// On blocked: what's needed to proceed
	blocked_by?: {
		type:          "dependency" | "question" | "external"
		details:       string
		unblocks_when: string
	}
}

#BeadResult: "success" | "failed" | "blocked" | "skipped"

// =============================================================================
// EXECUTION PLAN: Dependency-Ordered Phases
// =============================================================================

#ExecutionPlan: {
	session_id:   string
	generated_at: string // ISO8601

	// Beads grouped into execution phases
	phases: [...#ExecutionPhase]

	// Totals
	total_beads:  int & >=0
	total_effort: string // e.g., "2h 15min"

	// Risk assessment
	risk:     #RiskLevel
	blockers: [...string]
}

#ExecutionPhase: {
	phase_number: int & >=1
	title:        string
	beads:        [...#BeadID] & list.MinItems(1)
	can_parallel: bool
	effort:       string
}

#RiskLevel: "low" | "medium" | "high" | "critical"

// =============================================================================
// SESSION: Complete State (Split Across Files for Append)
// =============================================================================

// Main session file: .intent/session-{id}.cue
#Session: {
	id:         string
	created_at: string
	updated_at: string

	// Interview state - structural definition (compatible with ai_interview.cue#InterviewSession)
	interview: {
		id:         string
		started_at: string
		// Allow additional fields from the full InterviewSession type
		...
	}

	// Generated beads
	beads: [...#Bead]

	// Feedback comes from separate file via unification
	feedback: [...#BeadFeedback]

	// Plan approval
	approval?: #PlanApproval
}

#PlanApproval: {
	approved:    bool
	approved_at: string
	approved_by: "human" | "ci" | "auto"
	notes?:      string
}

// =============================================================================
// KIRK METADATA: Mental Lattice Validation
// =============================================================================
// Captures EARS clarity, KIRK contracts, inversion analysis, quality scores
// All fields optional - beads without metadata are still valid

#BeadKirkMetadata: {
	// EARS: Requirement clarity and pattern matching
	ears?: #BeadEARS

	// KIRK: Design by Contract (pre/post/invariants)
	contract?: #BeadContract

	// Inversion: Failure modes and edge case coverage
	inversion?: #BeadInversion

	// Quality: Multi-dimensional quality scoring
	quality?: #BeadQuality

	// Regeneration: Strategy for improving failed beads
	regeneration?: #RegenerationStrategy

	// Audit: When was this metadata generated?
	audit?: #BeadKirkAudit
}

// ===== EARS ANALYSIS =====
#BeadEARS: {
	// Which EARS pattern does "what" field follow?
	pattern: "ubiquitous" | "event_driven" | "state_driven" | "optional" | "unwanted" | "complex"

	// Extracted EARS components
	trigger?:   string  // When [trigger]
	condition?: string  // Where [condition]
	state?:     string  // While [state]

	// Clarity score (0-100)
	clarity_score: int & >=0 & <=100

	// Is pattern correctly matched to test specification?
	pattern_matches_test: bool
}

// ===== KIRK CONTRACTS =====
#BeadContract: {
	// What must be true before execution?
	preconditions?: [...string] & list.UniqueItems()

	// What must be true after execution?
	postconditions?: [...string] & list.UniqueItems()

	// What must ALWAYS be true?
	invariants?: [...string] & list.UniqueItems()
}

// ===== INVERSION ANALYSIS =====
#BeadInversion: {
	// Identified risks by category
	security_risks?:     [...#IdentifiedRisk]
	usability_risks?:    [...#IdentifiedRisk]
	integration_risks?:  [...#IdentifiedRisk]

	// What percentage of identified risks are covered by edge_cases?
	edge_case_coverage: int & >=0 & <=100

	// What edge cases should be added?
	suggested_edge_cases?: [...string]
}

#IdentifiedRisk: {
	risk:        string
	severity:    "low" | "medium" | "high" | "critical"
	mitigation:  string
}

// ===== QUALITY ANALYSIS =====
#BeadQuality: {
	// Multi-dimensional quality scores (0-100)
	completeness: int & >=0 & <=100
	testability:  int & >=0 & <=100
	clarity:      int & >=0 & <=100

	// Overall quality (average, but weighted toward testability)
	overall: int & >=0 & <=100

	// Issues found and their severity
	issues?: [...#QualityIssue]
}

#QualityIssue: {
	field:      string  // e.g., "done_when"
	issue:      string
	severity:   "info" | "warning" | "error" | "critical"
	suggestion: string
}

// ===== REGENERATION STRATEGY =====
// Strategic decision made in user interviews:
// - Hybrid (all three): Use inversion + second-order + pre-mortem together
// - Quality threshold: 80+ required for executable beads
// - Full enhancement: Update Phase 1 beads retroactively now
// - No versioning: Assume schema immutable
#RegenerationStrategy: {
	// When this bead failed, what strategy was used to regenerate it?
	strategy: "inversion_driven" | "second_order_driven" | "premortem_driven" | "hybrid" | "human_guided" | "not_applied"

	// For hybrid strategy: which analyses were performed?
	hybrid_methods?: [...string]

	// Root cause identified (if any)
	root_cause?: string

	// Beads that replaced this one (if regenerated)
	replaced_by?: [...#BeadID]

	// Was regeneration successful?
	regeneration_successful?: bool
}

// ===== AUDIT TRAIL =====
#BeadKirkAudit: {
	// When was KIRK analysis generated?
	analyzed_at: string  // ISO8601

	// What tool analyzed it?
	analyzer: string  // e.g., "intent analyze-beads"

	// Schema version used
	schema_version: string  // e.g., "1.0"

	// Analyst notes
	notes?: string
}

// =============================================================================
// FILE LAYOUT (Option B: Append-Friendly)
// =============================================================================
//
// .intent/
//   session-abc123.cue     # Main session: interview + beads
//   feedback-abc123.cue    # Append-only: execution feedback
//
// Usage:
//   # Validate everything
//   cue vet .intent/*-abc123.cue schema/*.cue
//
//   # Export unified session
//   cue export .intent/*-abc123.cue -e session --out json
//
//   # Append feedback (CLI does this)
//   echo 'session: feedback: [{bead_id:"AUTH-001",result:"success",...}]' >> .intent/feedback-abc123.cue
//
// CUE automatically unifies session.feedback from both files.

// =============================================================================
// MENTAL LATTICE CONFIGURATION: Strategic Decisions
// =============================================================================
// These are encoded in CUE to guide bead generation and validation
// They come from user decisions and should be referenced by all tools

#MentalLatticeConfig: {
	// REGENERATION STRATEGY: How to improve failed beads
	// User chose: "hybrid" (inversion + second-order + pre-mortem)
	regeneration_strategy: "inversion_driven" | "second_order_driven" | "premortem_driven" | "hybrid"

	// QUALITY THRESHOLD: What score before bead is executable?
	// User chose: 80 (strict)
	quality_threshold: int & >=0 & <=100

	// P1 BEADS APPROACH: Enhance existing or keep legacy?
	// User chose: "full_enhancement" (update now)
	p1_beads_approach: "full_enhancement" | "keep_legacy" | "gradual_optional"

	// SCHEMA VERSIONING: How to handle schema changes
	// User chose: "no_versioning" (immutable)
	schema_versioning: "version_in_bead" | "global_version" | "no_versioning" | "lazy_evolution"

	// OUTPUT FORMATS: How to display analysis
	// User chose: "all_via_flag" (human, json, markdown via --format)
	output_formats: "cli_tables" | "json" | "all_via_flag"
}