// schema/mental-lattice-config.cue
// Mental Lattice Framework Configuration
//
// Encodes strategic decisions made during planning phase:
// - Regeneration strategy: hybrid (inversion + second-order + pre-mortem)
// - Quality threshold: 80+ for executable beads
// - P1 beads approach: full enhancement now
// - Schema versioning: no versioning (immutable)
// - Output formats: all via --format flag (human, json, markdown)

package intent

// Strategic configuration guiding all Mental Lattice operations
ml_config: #MentalLatticeConfig & {
	regeneration_strategy: "hybrid"
	quality_threshold: 80
	p1_beads_approach: "full_enhancement"
	schema_versioning: "no_versioning"
	output_formats: "all_via_flag"
}

// ============================================================================
// PHASE 1 BEADS: MENTAL LATTICE ENHANCEMENTS
// ============================================================================
// Each P1 bead is enhanced with KIRK metadata derived from analysis:
// - EARS: Requirement clarity check
// - KIRK: Preconditions, postconditions, invariants
// - Inversion: Security, usability, integration risks
// - Quality: Completeness, testability, clarity scores
// - Regeneration: Strategy for improving if it fails

// ============================================================================
// [P1-T1.1] answer_loader.gleam - Load CUE answers for non-interactive mode
// ============================================================================
p1_t1_1_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "ubiquitous"
		trigger: "answers requested for non-interactive interview"
		clarity_score: 95
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			"CUE file exists at provided path",
			"Process has read permissions on file",
			"File contains valid CUE syntax",
			"File structure matches expected #AnswerMap schema",
			"Answer map keys are valid question IDs",
		]
		postconditions: [
			"Returns Result(Dict(QuestionID, Answer), String)",
			"Dict contains only valid question IDs",
			"All values are non-empty, trimmed strings",
			"Dict is immutable after construction",
			"Errors are descriptive (file not found vs parse vs schema mismatch)",
		]
		invariants: [
			"No side effects (file is read-only)",
			"No state mutation (pure function)",
			"Error types match expected error categories",
		]
	}

	inversion: {
		security_risks: [
			{
				risk: "File path traversal (../../secrets.cue)"
				severity: "high"
				mitigation: "Validate path is within expected directory, reject .."
			},
			{
				risk: "Reading sensitive file unintentionally"
				severity: "medium"
				mitigation: "Require explicit path, show full path in errors"
			},
		]
		usability_risks: [
			{
				risk: "File not found → cryptic error"
				severity: "high"
				mitigation: "Clear error: 'File not found: /path/to/answers.cue'"
			},
			{
				risk: "Invalid CUE syntax → unhelpful parse error"
				severity: "high"
				mitigation: "Show parse error with line number and context"
			},
			{
				risk: "Schema mismatch → silent truncation"
				severity: "high"
				mitigation: "Validate all keys against interview schema before use"
			},
			{
				risk: "Empty string answers → confusing behavior"
				severity: "medium"
				mitigation: "Define policy: reject empty vs accept vs require explicit empty"
			},
		]
		integration_risks: [
			{
				risk: "Very large file (>1MB) → memory pressure"
				severity: "low"
				mitigation: "Document expected file size bounds"
			},
			{
				risk: "Symlinked file → resolution issues"
				severity: "low"
				mitigation: "Follow symlinks correctly, resolve to actual path"
			},
			{
				risk: "Unicode in answers → encoding issues"
				severity: "low"
				mitigation: "UTF-8 validation, handle BOM marker"
			},
		]
		edge_case_coverage: 70
		suggested_edge_cases: [
			"File not found → clear error message with path shown",
			"Invalid CUE syntax → parse error with line number",
			"Answer key doesn't match any question → skip unknown, warn user",
			"Empty string answer → reject with suggestion to omit key",
			"Answer too long (>10KB) → warn about length",
			"Symlinked file → resolve and load from actual path",
			"File with BOM marker → handle UTF-8 BOM correctly",
			"Path with spaces → handle correctly",
			"File permissions denied → OS-level error handling",
			"Circular symlink → detect and error",
		]
	}

	quality: {
		completeness: 95  // Clear contract, well-defined inputs/outputs
		testability: 90   // Pure function, easy to test
		clarity: 95       // Single responsibility, clear naming
		overall: 93
		issues: [
			{
				field: "edge_cases"
				issue: "Missing file permission error handling"
				severity: "warning"
				suggestion: "Add test for EACCES (permission denied)"
			},
		]
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "Phase 1 full enhancement - apply immediately"
	}
}

// ============================================================================
// [P1-T1.2] interview.gleam - ask_single_question with dict lookup
// ============================================================================
p1_t1_2_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "optional"
		trigger: "question needs to be answered"
		condition: "pre-filled answers dict is provided"
		state: "during interview loop"
		clarity_score: 90
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			"ask_single_question() function exists and is callable",
			"Optional dict parameter can be added (Option type)",
			"Strict mode flag parameter is addable (bool)",
			"Question validation logic is available",
			"All existing callers must still compile (backwards compatible)",
		]
		postconditions: [
			"Returns same Answer type (no signature breaking)",
			"If dict provided and contains answer → use dict answer",
			"If dict provided but answer missing, strict=false → prompt user",
			"If dict provided but answer missing, strict=true → error",
			"If dict not provided → original behavior (prompt always)",
			"Answer is validated regardless of source (dict or prompt)",
			"All existing tests pass unchanged",
		]
		invariants: [
			"Answer validation applies regardless of source",
			"Dict is immutable (thread-safe)",
			"Original function behavior preserved when dict=None",
		]
	}

	inversion: {
		security_risks: [
			{
				risk: "Dict contains wrong answer type (int instead of string)"
				severity: "high"
				mitigation: "Validate answer against question type before use"
			},
		]
		usability_risks: [
			{
				risk: "Strict mode without dict → ambiguous behavior"
				severity: "high"
				mitigation: "Define clear semantics: what does strict mean with no dict?"
			},
			{
				risk: "Dict lookup case-sensitivity → key matching issues"
				severity: "medium"
				mitigation: "Document key naming convention (match schema exactly)"
			},
			{
				risk: "Dict answer is empty string → accepted or rejected?"
				severity: "medium"
				mitigation: "Policy: reject empty, user must omit key instead"
			},
			{
				risk: "Backwards compatibility broken → existing code fails"
				severity: "high"
				mitigation: "Make dict parameter optional, default to None (original behavior)"
			},
		]
		integration_risks: [
			{
				risk: "Dict has extra answers (not in interview) → wasted memory"
				severity: "low"
				mitigation: "Ignore extra keys gracefully, no error"
			},
		]
		edge_case_coverage: 75
		suggested_edge_cases: [
			"Dict provided, answer missing, strict=false → prompt user",
			"Dict provided, answer missing, strict=true → error with key",
			"Dict answer wrong type → validation error",
			"Dict contains extra answers → ignore gracefully",
			"Dict answer empty string → reject",
			"Backward compatibility: no dict → original behavior",
			"Dict lookup returns whitespace → trim before validation",
			"Question ID case mismatch → documentation",
		]
	}

	quality: {
		completeness: 85  // Clear interface but edge cases to handle
		testability: 85   // Needs mocking for stdin fallback
		clarity: 90       // Clear purpose but complex interactions
		overall: 87
		issues: [
			{
				field: "requirements"
				issue: "Strict mode semantics need clear definition"
				severity: "error"
				suggestion: "Document: strict=true without dict means what exactly?"
			},
			{
				field: "edge_cases"
				issue: "Missing whitespace handling in dict values"
				severity: "warning"
				suggestion: "Trim dict values, or expect caller to trim?"
			},
		]
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "Phase 1 full enhancement - complex interactions need testing"
	}
}

// ============================================================================
// [P1-T1.3] intent.gleam - CLI integration (--answers, --strict)
// ============================================================================
p1_t1_3_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "event_driven"
		trigger: "--answers flag provided with file path"
		condition: "--strict flag provided"
		state: "during interview initialization"
		clarity_score: 85
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			"run_interview() function exists",
			"Flag parsing infrastructure exists (glint/flag)",
			"answer_loader module is callable",
			"interview_loop accepts answers dict and strict flag",
		]
		postconditions: [
			"Help text documents both --answers and --strict",
			"Answers loaded from file when --answers provided",
			"Strict mode enforced when --strict provided",
			"Clear error if --answers path invalid",
			"Integration tests cover all flag combinations",
		]
		invariants: [
			"--strict without --answers is an error or warning",
			"File path resolution is normalized",
		]
	}

	inversion: {
		security_risks: [
			{
				risk: "Path traversal in --answers argument"
				severity: "high"
				mitigation: "Normalize path early, reject dangerous patterns"
			},
		]
		usability_risks: [
			{
				risk: "Flag provided but file invalid → confusing error"
				severity: "high"
				mitigation: "Validate file early, show full path in error"
			},
			{
				risk: "--strict without --answers → confusing"
				severity: "high"
				mitigation: "Error or warn clearly: --strict requires --answers"
			},
			{
				risk: "File path resolution issues (relative vs absolute)"
				severity: "medium"
				mitigation: "Normalize path, show resolved path in errors"
			},
			{
				risk: "Wrong file provided accidentally"
				severity: "low"
				mitigation: "Show file being loaded with full path, ask confirmation"
			},
		]
		integration_risks: [
			{
				risk: "Empty answers file"
				severity: "low"
				mitigation: "Load empty dict, not error"
			},
		]
		edge_case_coverage: 70
		suggested_edge_cases: [
			"--strict without --answers → error with helpful message",
			"File path is relative → resolve relative to cwd",
			"File path contains ~/ → expand to home directory",
			"File not in cwd → try to resolve, show full path",
			"Empty answers file → load empty dict, not error",
			"Both --answers and stdin → --answers takes precedence",
		]
	}

	quality: {
		completeness: 80
		testability: 85
		clarity: 85
		overall: 83
		issues: [
			{
				field: "requirements"
				issue: "Interaction between --answers and --strict not fully specified"
				severity: "error"
				suggestion: "Add tests for all combinations and document behavior"
			},
		]
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "Phase 1 full enhancement - needs clear flag interaction docs"
	}
}

// ============================================================================
// [P1-T1.4] bead_feedback.gleam - Append-only CUE feedback tracking
// ============================================================================
p1_t1_4_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "ubiquitous"
		trigger: "bead execution completes"
		state: "multiple beads may finish concurrently"
		clarity_score: 95
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			".intent/ directory exists (created by session)",
			"Session ID is valid",
			"Write permissions on .intent/ directory",
			"#BeadFeedback schema is defined",
			"Bead ID exists in session",
			"File locking mechanism available",
		]
		postconditions: [
			"#BeadFeedback appended to feedback file",
			"Append is atomic (all or nothing)",
			"File validates with cue vet after append",
			"Timestamp is ISO8601 UTC",
			"Duration_ms is accurate",
			"Error/blocked_by fields populated when relevant",
			"load_feedback() can parse via cue export",
		]
		invariants: [
			"No partial writes (atomic append)",
			"File remains valid CUE after each append",
			"Timestamp is UTC (no timezone ambiguity)",
		]
	}

	inversion: {
		security_risks: [
			{
				risk: "Directory traversal in session ID"
				severity: "high"
				mitigation: "Validate session ID format strictly (alphanumeric)"
			},
		]
		usability_risks: [
			{
				risk: "Concurrent writes → file corruption"
				severity: "critical"
				mitigation: "Implement file locking (flock Unix, LockFile Windows)"
			},
			{
				risk: "Append fails mid-write → invalid file"
				severity: "critical"
				mitigation: "Atomic append: write to temp, sync, rename"
			},
			{
				risk: "Disk full during append → partial write"
				severity: "high"
				mitigation: "Check disk space before write, handle ENOSPC"
			},
			{
				risk: "Session doesn't exist → where to write feedback?"
				severity: "high"
				mitigation: "Error if session missing, suggest session creation"
			},
			{
				risk: "Bead ID doesn't exist → orphaned feedback"
				severity: "medium"
				mitigation: "Validate bead ID before appending"
			},
		]
		integration_risks: [
			{
				risk: "Very large feedback file (1000+ entries)"
				severity: "low"
				mitigation: "Consider archiving old feedback"
			},
			{
				risk: "Unification edge cases in CUE"
				severity: "low"
				mitigation: "Test CUE unification with large feedback sets"
			},
		]
		edge_case_coverage: 65
		suggested_edge_cases: [
			"Concurrent appends (two beads finish simultaneously) → atomic writes",
			"Disk full during append → graceful error, preserve file integrity",
			"Session doesn't exist → clear error message",
			"Bead ID doesn't match any in session → warn but still append",
			"Timestamp precision → use nanoseconds, format ISO8601",
			"Error object present → validate error.type and message",
			"Blocked reason missing when result='blocked' → error",
			"Very large feedback array → performance test with 1000+ entries",
		]
	}

	quality: {
		completeness: 90
		testability: 75  // Concurrency testing is complex
		clarity: 95
		overall: 87
		issues: [
			{
				field: "requirements"
				issue: "File locking strategy not specified (OS-specific)"
				severity: "error"
				suggestion: "Document: use flock (Unix), LockFile (Windows), or library?"
			},
			{
				field: "testing"
				issue: "Concurrent append testing is hard without proper tooling"
				severity: "error"
				suggestion: "Add integration tests with thread spawning"
			},
		]
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "CRITICAL: Concurrency is main risk - file locking must be specified"
	}
}

// ============================================================================
// [P1-T1.5] bead-status command - CLI interface for feedback
// ============================================================================
p1_t1_5_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "event_driven"
		trigger: "user runs: intent bead-status <id> --status X --reason Y"
		condition: "status=blocked"
		state: "during bead execution phase"
		clarity_score: 90
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			"bead_feedback module exists",
			"Session ID is valid (or inferred from env)",
			"Bead ID is valid format (WORD-NNN)",
			"Status enum is valid",
			"If status=blocked, reason is provided",
		]
		postconditions: [
			"Feedback appended to .intent/feedback-{session}.cue",
			"Success message shows: 'Bead {id} marked as {status}'",
			"Exit code 0 on success, non-zero on error",
			"JSON output available for parsing",
		]
		invariants: [
			"Bead ID format always validated",
			"Blocked status requires reason",
		]
	}

	inversion: {
		security_risks: [
			{
				risk: "Session ID in command line → visible in process list"
				severity: "low"
				mitigation: "Document: don't put secrets in session IDs"
			},
		]
		usability_risks: [
			{
				risk: "Invalid bead ID → silent failure"
				severity: "high"
				mitigation: "Validate format early, show error"
			},
			{
				risk: "Session doesn't exist → orphaned feedback"
				severity: "high"
				mitigation: "Require session ID or infer from env, validate it exists"
			},
			{
				risk: "Duplicate status updates (bead marked twice) → confusing"
				severity: "medium"
				mitigation: "Warn if bead already has status, show previous value"
			},
			{
				risk: "No reason for blocked bead → incomplete"
				severity: "high"
				mitigation: "Enforce --reason for --status blocked"
			},
			{
				risk: "Status value typo (e.g., 'failde') → confusing"
				severity: "medium"
				mitigation: "Error with valid options"
			},
		]
		integration_risks: [
			{
				risk: "CI parsing JSON output"
				severity: "low"
				mitigation: "Ensure --json flag produces parseable JSON"
			},
		]
		edge_case_coverage: 75
		suggested_edge_cases: [
			"Bead already marked as completed, try to mark failed → warn, update",
			"Invalid status value (typo) → error with valid options",
			"Session ID missing → infer from .intent/ or require --session",
			"--status blocked without --reason → error, show required",
			"Multiple --reason arguments → use last one",
			"--json flag for CI parsing",
		]
	}

	quality: {
		completeness: 85
		testability: 85
		clarity: 90
		overall: 87
		issues: [
			{
				field: "requirements"
				issue: "Session ID inference needs documentation"
				severity: "warning"
				suggestion: "How to infer session ID? From env var? From .intent/?"
			},
		]
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "Phase 1 full enhancement - straightforward command"
	}
}

// ============================================================================
// [P1-T1.6] beads-regenerate command - Smart bead improvement
// ============================================================================
p1_t1_6_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "complex"
		trigger: "beads have failed or are blocked"
		condition: "feedback explains the failure reason"
		state: "after initial execution phase"
		clarity_score: 70  // Complex requirement
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			"Session file exists with beads",
			"Feedback file exists with execution results",
			"At least one failed or blocked bead exists",
			"Original successful beads are backed up",
			"Analysis logic available (inversion + second-order + pre-mortem)",
		]
		postconditions: [
			"New beads generated with different IDs",
			"Session file updated with new beads",
			"Original successful beads preserved unchanged",
			"Dependency graph remains acyclic",
			"Regeneration reason logged in bead metadata",
			"Old beads marked as 'superseded' or archived",
		]
		invariants: [
			"No circular dependencies in new beads",
			"New beads are distinct from original (different IDs)",
		]
	}

	inversion: {
		security_risks: []
		usability_risks: [
			{
				risk: "Regeneration quality worse than original → regression"
				severity: "critical"
				mitigation: "Compare new to old, show diff, require approval"
			},
			{
				risk: "New beads create circular dependencies"
				severity: "critical"
				mitigation: "Validate DAG after generation, reject if cyclic"
			},
			{
				risk: "No failed/blocked beads → error or no-op?"
				severity: "high"
				mitigation: "Either error clearly or silently no-op (document choice)"
			},
			{
				risk: "Root cause analysis missing → same failures regenerated"
				severity: "high"
				mitigation: "Use inversion + pre-mortem to understand failure"
			},
			{
				risk: "History lost (can't see what was regenerated)"
				severity: "medium"
				mitigation: "Keep original beads, mark as superseded"
			},
		]
		integration_risks: [
			{
				risk: "Regeneration creates many new beads (combinatorial explosion)"
				severity: "medium"
				mitigation: "Limit new beads per failed bead, group related ones"
			},
		]
		edge_case_coverage: 60
		suggested_edge_cases: [
			"No failed beads → error or no-op (document clearly)",
			"Regeneration creates same beads → detect, offer rollback",
			"New beads have circular dependencies → reject with error",
			"Backup of original beads → preserve for rollback",
			"Root cause: 'external service timeout' → regenerate with retry logic",
			"Root cause: 'missing file' → regenerate with file creation step",
			"Approval workflow for regeneration",
		]
	}

	quality: {
		completeness: 75  // Complex requirement, many unknowns
		testability: 65   // Hard to test quality of regeneration
		clarity: 60       // "Adjusted approach" is vague
		overall: 67
		issues: [
			{
				field: "what"
				issue: "Vague: what is 'adjusted approach'?"
				severity: "critical"
				suggestion: "Define strategies: inversion-driven, second-order-driven, pre-mortem-driven"
			},
			{
				field: "testing"
				issue: "How to verify regenerated beads are better?"
				severity: "error"
				suggestion: "Add quality comparison metrics, approval workflow"
			},
			{
				field: "requirements"
				issue: "Missing rollback mechanism if regeneration fails"
				severity: "error"
				suggestion: "Backup original beads, support rollback command"
			},
		]
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "MOST COMPLEX: Needs clear definition of regeneration strategies and approval workflow"
	}
}

// ============================================================================
// [P1-T1.7] plan_mode.gleam - Dependency graph analysis
// ============================================================================
p1_t1_7_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "ubiquitous"
		trigger: "user requests execution plan"
		state: "after beads are generated"
		clarity_score: 95
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			"Session file loaded with beads array",
			"Each bead has valid requires array (bead IDs only)",
			"Dependency graph is acyclic (no circular dependencies)",
			"#ExecutionPlan schema is defined",
		]
		postconditions: [
			"ExecutionPlan computed from session.beads",
			"Phases are topologically ordered",
			"Parallel-safe beads identified",
			"Effort aggregated correctly",
			"Risk calculated (low/medium/high/critical)",
			"Human format readable, JSON matches schema",
		]
		invariants: [
			"No circular dependencies",
			"Phases are ordered by dependencies",
		]
	}

	inversion: {
		security_risks: []
		usability_risks: [
			{
				risk: "Circular dependencies → infinite loop"
				severity: "critical"
				mitigation: "Detect cycles upfront, error with cycle path"
			},
			{
				risk: "Large dependency graph (100+ beads) → performance"
				severity: "high"
				mitigation: "Use efficient topological sort (Kahn's algorithm)"
			},
			{
				risk: "Bead requires non-existent bead"
				severity: "high"
				mitigation: "Validate all bead IDs before processing"
			},
			{
				risk: "No beads in session → empty plan"
				severity: "low"
				mitigation: "Generate empty plan or error (document choice)"
			},
		]
		integration_risks: [
			{
				risk: "Very large plan rendering (100+ beads) is hard to read"
				severity: "low"
				mitigation: "Tree format, truncate if >100 beads, pagination"
			},
		]
		edge_case_coverage: 85
		suggested_edge_cases: [
			"Circular dependencies (A → B → C → A) → detect and error with cycle",
			"100+ beads → efficient sort, consider pagination",
			"Bead requires non-existent ID → validation error",
			"Empty session (no beads) → empty plan vs error",
			"Single bead (no dependencies) → phase with 1 bead",
			"All beads parallel → single phase with all",
			"Dependency chain (A → B → C → ...) → single serial phase",
		]
	}

	quality: {
		completeness: 95
		testability: 90
		clarity: 95
		overall: 93
		issues: []
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "Phase 1 full enhancement - well-defined algorithmic task"
	}
}

// ============================================================================
// [P1-T1.8] plan command - CLI display of execution plan
// ============================================================================
p1_t1_8_kirk: #BeadKirkMetadata & {
	ears: {
		pattern: "event_driven"
		trigger: "user runs: intent plan <session_id> --format human|json"
		state: "after beads are generated"
		clarity_score: 95
		pattern_matches_test: true
	}

	contract: {
		preconditions: [
			"plan_mode module exists",
			"Session ID is valid",
			"Format is valid (human, json, markdown)",
			"Session file exists and is readable",
		]
		postconditions: [
			"Plan loaded from session file",
			"Human format shows ASCII dependency tree",
			"JSON format validates against schema",
			"Markdown format is documentation-ready",
			"Phases show parallelization hints",
			"Effort totals displayed correctly",
		]
		invariants: [
			"All output formats show same information",
			"Exit code 0 on success, non-zero on error",
		]
	}

	inversion: {
		security_risks: []
		usability_risks: [
			{
				risk: "Session doesn't exist → clear error"
				severity: "high"
				mitigation: "Error message with full path"
			},
			{
				risk: "Invalid format flag → confusing"
				severity: "medium"
				mitigation: "Error with valid options: human, json, markdown"
			},
			{
				risk: "Very large plan (200+ beads) → hard to read"
				severity: "low"
				mitigation: "Ask about pagination or summary"
			},
		]
		integration_risks: [
			{
				risk: "JSON on large plan → stream output or chunk"
				severity: "low"
				mitigation: "Test with 200+ bead session"
			},
		]
		edge_case_coverage: 85
		suggested_edge_cases: [
			"Session doesn't exist → clear error with path",
			"Invalid format (typo) → show valid options",
			"Plan is very large (200+ beads) → pagination or summary",
			"--json on large plan → efficient streaming",
			"Human format with colors → terminal detection",
		]
	}

	quality: {
		completeness: 95
		testability: 90
		clarity: 95
		overall: 93
		issues: []
	}

	regeneration: {
		strategy: "not_applied"
		hybrid_methods: []
	}

	audit: {
		analyzed_at: "2026-01-08T00:00:00Z"
		analyzer: "manual-planning"
		schema_version: "1.0"
		notes: "Phase 1 full enhancement - straightforward wrapper command"
	}
}
