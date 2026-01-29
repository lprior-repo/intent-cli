// Enhanced Bead Schema
// Self-documenting, schema-validated work units with KIRK + ATDD + BDD + EARS + DBC

package schema

// =============================================================================
// CORE BEAD TYPE
// =============================================================================

#EnhancedBead: {
	id:          string
	title:       string
	description: string

	// Source tracing
	source_type:   "spec" | "kirk" | "interview" | "feedback"
	kirk_sources:  [...#KirkSource]
	spec_path:     string | null
	behavior_name: string | null

	// Methodology components
	ears_patterns:      [...#EarsPatternInfo]
	contracts:          #BeadContracts
	scenarios:          [...#BddScenario]
	acceptance_criteria: [...#AcceptanceCriterion]
	types_needed:       [...#TypeDefinition]

	// Execution metadata
	effort:       "5min" | "10min" | "15min" | "20min" | "30min"
	priority:     int & >=1 & <=5
	status:       "pending" | "in_progress" | "blocked" | "completed"
	dependencies: [...string]
	blocks:       [...string]
	round:        int & >=1 & <=5

	// Classification
	profile_type: string
	issue_type:   string
	labels:       [...string]

	// AI hints
	ai_hints: string
	pitfalls: [...string]
}

// =============================================================================
// KIRK SOURCE TRACEABILITY
// =============================================================================

#KirkSource: {
	analysis_type: "quality" | "coverage" | "gaps" | "invert" | "effects" | "ears"
	finding_id:    string
	severity:      #Severity
	category:      string
	original_text: string
	suggestion:    string | null
}

#Severity: "low" | "medium" | "high" | "critical"

// =============================================================================
// EARS (EASY APPROACH TO REQUIREMENTS SYNTAX) PATTERNS
// =============================================================================

// EARS pattern types (documentary, not enforced):
// - "Ubiquitous": Simple requirement "THE SYSTEM SHALL <behavior>"
// - "EventDriven": "WHEN <event> THE SYSTEM SHALL <behavior>"
// - "StateDriven": "WHILE <state> THE SYSTEM SHALL <behavior>"
// - "Optional": "WHERE <condition> THE SYSTEM SHALL <behavior>"
// - "Unwanted": "IF <condition> THEN THE SYSTEM SHALL NOT <behavior>"

#EarsPatternInfo: {
	pattern_type: string
	trigger:      string | null
	state:        string | null
	condition:    string | null
	behavior:     string
}

// =============================================================================
// DBC (DESIGN BY CONTRACT) SPECIFICATIONS
// =============================================================================

#BeadContracts: {
	preconditions:  [...string]
	postconditions: [...#ContractCheck]
	invariants:     [...string]
}

#ContractCheck: {
	check_name: string
	rule:       string
	why:        string
}

// =============================================================================
// BDD (BEHAVIOR DRIVEN DEVELOPMENT) SCENARIOS
// =============================================================================

// BDD format: Given <context>, WHEN <action>, THEN <outcome>
// Used for test scenario documentation and automation

#BddScenario: {
	name:      string
	given:     [...string]
	when:      string
	then:      string
	assertion: string
}

// =============================================================================
// ATDD (ACCEPTANCE TEST DRIVEN DEVELOPMENT) CRITERIA
// =============================================================================

// ATDD criteria define when a bead is "done"
// verification_type: "automated" (run tests), "manual" (review output), "review" (code review)

#AcceptanceCriterion: {
	id:               string
	description:      string
	verification_type: "automated" | "manual" | "review"
	check_expression: string | null
	verified:         bool
}

// =============================================================================
// TYPE DEFINITIONS FOR AI IMPLEMENTATION
// =============================================================================

// TypeDefinition provides AI agents with function/type signatures needed
// to implement the bead. Each definition includes:
// - name: The type or function name
// - signature: Full type signature (e.g., "fn handle(request: Request) -> Response")
// - purpose: Human-readable description of what this type does

#TypeDefinition: {
	name:      string
	signature: string
	purpose:   string
}

// =============================================================================
// USAGE NOTES FOR AI AGENTS
// =============================================================================

// When generating EnhancedBead records, AI should:
//
// 1. Always provide unique id: use format "bead-{type}-{category}-{index}"
// 2. Map severity to priority: critical=1, high=2, medium=3, low=4
// 3. Map mental model to round: inversion=3, second-order=4, checklist=2, coverage=2, security=3, quality=2
// 4. Provide at least one AcceptanceCriterion per bead
// 5. Provide at least one BddScenario per bead
// 6. Include type definitions for all custom types needed
// 7. Fill kirk_sources array when analysis_type = "kirk"
// 8. Set effort based on severity: critical=30min, high=20min, medium=15min, low=10min
// 9. Add relevant labels: "security", "quality", "api", "contracts", etc.
