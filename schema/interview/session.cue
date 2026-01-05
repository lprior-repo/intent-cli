// Interview Session Schema
// Defines the structure of an interview session with two AI personas

package interview

import (
	"time"
	"github.com/intent-cli/schema/core:intent"
)

// ============================================================================
// INTERVIEW SESSION - The full interview state
// ============================================================================

#Session: {
	// Session identity
	id:      string & =~"^sess_[a-z0-9]+$"
	created: time.Format(time.RFC3339) | string

	// What are we building?
	profile:          intent.#Profile
	initial_prompt:   string
	working_title:    string

	// Current state
	stage:            #Stage
	rounds_completed: int & >=0 & <=5

	// The two personas
	architect: #PersonaState
	adversary: #PersonaState

	// Accumulated knowledge
	answers:    [...#Answer]
	extracted:  #ExtractedSpec
	gaps:       [...#Gap]
	conflicts:  [...#Conflict]

	// Pending questions
	pending_questions: [...#Question]

	// Completion
	completed_at?: time.Format(time.RFC3339) | string
	final_spec?:   string  // Path to generated spec file
}

#Stage: "discovery" | "refinement" | "validation" | "review" | "complete"

// ============================================================================
// PERSONA STATE - Track each persona's progress
// ============================================================================

#PersonaState: {
	name:       "architect" | "adversary"
	satisfied:  bool | *false
	concerns:   [...string]
	questions_asked: int | *0
	sign_off?: {
		at:     time.Format(time.RFC3339) | string
		reason: string
	}

	// What is this persona looking for?
	satisfaction_criteria: [...#SatisfactionCriterion]
}

#SatisfactionCriterion: {
	criterion: string
	met:       bool
	evidence?: string
}

// ============================================================================
// QUESTIONS - What we ask the human
// ============================================================================

#Question: {
	id:       string
	round:    int & >=1 & <=5
	persona:  "architect" | "adversary"

	// Question metadata
	perspective: #Perspective
	category:    #Category
	priority:    #Priority

	// The actual question
	question:   string
	why_asking: string
	example_answer?: string

	// What this question extracts
	extracts: [...string]

	// Follow-up questions based on answer
	follow_ups?: [...#Question]

	// Was it asked/answered?
	status: "pending" | "asked" | "answered" | "skipped"
	answer?: #Answer
}

#Perspective: "user" | "developer" | "security" | "ops" | "business"

#Category: "happy_path" | "error_case" | "edge_case" | "constraint" |
           "dependency" | "security" | "performance" | "data_model"

#Priority: "critical" | "important" | "nice_to_have"

// ============================================================================
// ANSWERS - Human responses to questions
// ============================================================================

#Answer: {
	question_id: string
	timestamp:   time.Format(time.RFC3339) | string
	raw_answer:  string

	// What we extracted from the answer
	extracted: [...#Extraction]

	// Any follow-up questions triggered
	triggered_follow_ups: [...string]
}

#Extraction: {
	field:      string
	value:      _
	confidence: "high" | "medium" | "low"
	needs_confirmation: bool | *false
}

// ============================================================================
// EXTRACTED SPEC - What we've learned so far
// ============================================================================

#ExtractedSpec: {
	// Core fields (may be partial)
	name?:        string
	intent?:      string
	description?: string
	audience?:    string

	// Success criteria
	success_criteria: [...string]

	// Features and behaviors (may be partial)
	features: [...#PartialFeature]

	// Constraints discovered
	constraints: [...intent.#Constraint]

	// Errors enumerated
	errors: [...intent.#ErrorDefinition]

	// Anti-patterns identified
	anti_patterns: [...intent.#AntiPattern]

	// Clarifications made
	clarifications: [...intent.#Clarification]

	// AI hints gathered
	ai_hints: intent.#AIHints
}

#PartialFeature: {
	name:        string
	description: string
	behaviors:   [...#PartialBehavior]
	complete:    bool | *false
}

#PartialBehavior: {
	name:     string
	intent:   string
	complete: bool | *false

	// Partial request/response
	request?: {
		method?: string
		path?:   string
		body?:   _
	}
	response?: {
		status?: int
		checks?: {
			[string]: _
		}
	}

	// What's still missing?
	missing_fields: [...string]
}

// ============================================================================
// GAPS - What we still need to know
// ============================================================================

#Gap: {
	id:          string
	field:       string
	description: string
	blocking:    bool

	// How to fill this gap
	suggested_question?: string
	suggested_default?:  _

	// Was it resolved?
	resolved: bool | *false
	resolved_by?: string  // answer_id or "default"
}

// ============================================================================
// CONFLICTS - Contradictions in answers
// ============================================================================

#Conflict: {
	id:          string
	description: string

	// The conflicting answers
	between: [string, string]  // answer_ids

	// Resolution
	resolution_options: [...string]
	resolved:           bool | *false
	resolution?:        string
	resolved_by?:       string  // "architect" | "adversary" | "human"
}

// ============================================================================
// ROUND DEFINITIONS - The 5-round structure
// ============================================================================

#RoundDefinition: {
	round:       int & >=1 & <=5
	name:        string
	perspective: #Perspective
	goal:        string

	// Questions for this round
	questions: [...#QuestionTemplate]
}

#QuestionTemplate: {
	id:          string
	persona:     "architect" | "adversary"
	question:    string
	why_asking:  string
	extracts:    [...string]
	priority:    #Priority

	// Conditional: only ask if...
	condition?: string

	// Profile-specific: only ask for these profiles
	profiles?: [...intent.#Profile]
}

// ============================================================================
// THE FIVE ROUNDS
// ============================================================================

#Rounds: [
	#RoundDefinition & {
		round:       1
		name:        "Core Intent"
		perspective: "user"
		goal:        "Understand what the human wants to build"
	},
	#RoundDefinition & {
		round:       2
		name:        "Error Cases"
		perspective: "developer"
		goal:        "Discover failure modes"
	},
	#RoundDefinition & {
		round:       3
		name:        "Edge Cases"
		perspective: "developer"
		goal:        "Find the boundaries"
	},
	#RoundDefinition & {
		round:       4
		name:        "Security & Compliance"
		perspective: "security"
		goal:        "Identify risks"
	},
	#RoundDefinition & {
		round:       5
		name:        "Operations"
		perspective: "ops"
		goal:        "Understand runtime needs"
	},
]
