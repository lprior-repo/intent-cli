// Intent Core Schema
// The universal schema that all profiles extend
// This is THE source of truth for what an Intent spec contains

package intent

import (
	"time"
)

// ============================================================================
// CORE INTENT - Universal across all profiles
// ============================================================================

#Intent: {
	// Identity
	name:    #Name
	version: #Version | *"1.0.0"

	// The most important field - natural language description
	intent: #IntentDescription

	// What type of thing is this?
	profile: #Profile

	// Interview metadata - how was this spec created?
	session?: #InterviewSession

	// Universal fields
	constraints:    [...#Constraint]
	examples:       #Examples
	clarifications: [...#Clarification]
	ai_hints:       #AIHints

	// Profile-specific fields are added by each profile schema
	...
}

// ============================================================================
// PRIMITIVES
// ============================================================================

// Name must be kebab-case, lowercase
#Name: string & =~"^[a-z][a-z0-9]*(-[a-z0-9]+)*$"

// Semantic version
#Version: string & =~"^[0-9]+\\.[0-9]+\\.[0-9]+$"

// Intent description - must be meaningful (at least 10 chars)
#IntentDescription: string & strings.MinRunes(10)

// Supported profiles
#Profile: "api" | "cli" | "event" | "data" | "workflow" | "ui"

// ============================================================================
// CONSTRAINTS - Universal validation rules
// ============================================================================

#Constraint: {
	field:    string
	rule:     #Rule
	why:      string
	severity: *"error" | "warning" | "info"

	// Optional: when does this constraint apply?
	when?: string

	// Optional: which profiles/features does this apply to?
	applies_to?: [...string]
}

// Rule expressions
#Rule: string & =~"^(required|optional|absent|equals|one_of|matches|format|min|max|length|min_length|max_length|is|valid|contains|not_contains|references|integer|number|string|boolean|array|object|null).*"

// ============================================================================
// EXAMPLES - Good and bad examples
// ============================================================================

#Examples: {
	valid:   [..._]
	invalid: [...#InvalidExample]
}

#InvalidExample: {
	value:  _
	reason: string
}

// ============================================================================
// CLARIFICATIONS - Decisions made during interview
// ============================================================================

#Clarification: {
	question:   string
	answer:     string
	decided_by: string
	date:       time.Format("2006-01-02") | string
	context?:   string
}

// ============================================================================
// AI HINTS - Guidance for AI implementation
// ============================================================================

#AIHints: {
	implementation: #ImplementationHints
	entities:       #Entities
	security:       #SecurityHints
	pitfalls:       [...string]

	// Additional freeform hints
	...
}

#ImplementationHints: {
	suggested_stack: [...string]
	architecture?:   string
	patterns?:       [...string]
	avoid?:          [...string]
}

#Entities: {
	[string]: #EntityDefinition
}

#EntityDefinition: {
	fields: {
		[string]: string | #FieldDefinition
	}
	relationships?: {
		[string]: #Relationship
	}
}

#FieldDefinition: {
	type:        string
	required:    bool | *true
	description: string
	constraints: [...string]
}

#Relationship: {
	type:        "belongs_to" | "has_many" | "has_one" | "many_to_many"
	entity:      string
	foreign_key: string
	on_delete?:  "cascade" | "nullify" | "restrict"
}

#SecurityHints: {
	authentication?: string
	authorization?:  string
	encryption?:     string
	audit?:          string

	// Common security fields
	password_hashing?: string
	jwt_algorithm?:    string
	jwt_expiry?:       string
	rate_limiting?:    string

	// Additional security hints
	...
}

// ============================================================================
// INTERVIEW SESSION - Metadata about spec creation
// ============================================================================

#InterviewSession: {
	id:        string
	created:   time.Format(time.RFC3339) | string
	completed: time.Format(time.RFC3339) | string | *null

	// Persona sign-off
	architect: #PersonaSignoff
	adversary: #PersonaSignoff

	// How many rounds of questions?
	rounds_completed: int & >=0 & <=5

	// Any unresolved gaps?
	unresolved_gaps: [...#Gap]
}

#PersonaSignoff: {
	satisfied:   bool
	concerns:    [...string]
	signed_at?:  time.Format(time.RFC3339) | string
	sign_off_reason?: string
}

#Gap: {
	field:       string
	description: string
	blocking:    bool
	suggested_default?: _
}

// ============================================================================
// ANTI-PATTERNS - What NOT to do
// ============================================================================

#AntiPattern: {
	name:        #Name
	description: string
	bad_example: _
	good_example: _
	why:         string
	severity:    *"error" | "warning"
}

// ============================================================================
// ERRORS - Typed error definitions
// ============================================================================

#ErrorDefinition: {
	code:        string & =~"^[A-Z][A-Z0-9_]*$"
	status?:     int & >=400 & <600
	message:     string
	retryable:   bool | *false
	retry_after?: string

	// What triggers this error?
	when: string

	// Structure of error details
	details?: {
		[string]: string
	}
}

// ============================================================================
// TRAITS - Reusable behaviors (stolen from Smithy)
// ============================================================================

#Traits: {
	idempotent?: #IdempotentTrait
	paginated?:  #PaginatedTrait
	retryable?:  #RetryableTrait
	cacheable?:  #CacheableTrait
	deprecated?: #DeprecatedTrait
}

#IdempotentTrait: {
	key_header: string | *"X-Idempotency-Key"
	window:     string | *"24h"
	scope:      "global" | "api_key" | "user" | *"api_key"
}

#PaginatedTrait: {
	style:         "cursor" | "offset" | "page"
	cursor_param:  string | *"cursor"
	limit_param:   string | *"limit"
	max_limit:     int | *100
	default_limit: int | *20
}

#RetryableTrait: {
	max_attempts:  int | *3
	backoff:       "exponential" | "linear" | "constant" | *"exponential"
	initial_delay: string | *"1s"
	max_delay:     string | *"30s"
}

#CacheableTrait: {
	ttl:       string
	vary_by:   [...string]
	invalidate_on: [...string]
}

#DeprecatedTrait: {
	since:      #Version
	removed_in?: #Version
	migration:  string
}
