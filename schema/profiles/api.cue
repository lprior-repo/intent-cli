// API Profile Schema
// Extends core intent with HTTP/REST API specific fields

package api

import (
	"github.com/intent-cli/schema/core:intent"
)

// ============================================================================
// API SPEC - Top level API specification
// ============================================================================

#Spec: intent.#Intent & {
	profile: "api"

	// API-specific metadata
	description: string
	audience:    string

	// What does success look like?
	success_criteria: [...string]

	// API configuration
	config: #Config

	// Features group related behaviors
	features: [...#Feature]

	// Global rules that apply to all responses
	rules: [...#Rule]

	// Anti-patterns to detect
	anti_patterns: [...intent.#AntiPattern]

	// Typed errors
	errors?: {
		[string]: intent.#ErrorDefinition
	}

	// Traits that can be applied to behaviors
	traits?: intent.#Traits
}

// ============================================================================
// CONFIG - API-wide configuration
// ============================================================================

#Config: {
	base_url:   string & =~"^https?://"
	timeout_ms: int & >0 | *5000
	headers: {
		[string]: string
	}
}

// ============================================================================
// FEATURE - Logical grouping of behaviors
// ============================================================================

#Feature: {
	name:        string
	description: string
	behaviors:   [...#Behavior]

	// Feature-level bead metadata
	bead?: #FeatureBeadMeta
}

#FeatureBeadMeta: {
	epic?:     string
	priority?: "critical" | "high" | "medium" | "low"
	labels?:   [...string]
}

// ============================================================================
// BEHAVIOR - A single API test case
// ============================================================================

#Behavior: {
	// Identity
	name:   intent.#Name
	intent: intent.#IntentDescription

	// Optional metadata
	notes?: string
	tags:   [...string] | *[]

	// Dependencies
	requires: [...string] | *[]

	// Apply traits
	traits?: [...string]

	// Given-When-Then structure (stolen from BDD)
	given?: [...string]

	// The request to make
	request: #Request

	// Expected response
	response: #Response

	// Values to capture for later behaviors
	captures: {
		[string]: string
	} | *{}

	// Behavior-level bead metadata
	bead?: #BehaviorBeadMeta
}

#BehaviorBeadMeta: {
	id?:       string
	estimate?: "tiny" | "small" | "medium" | "large" | "huge"
	labels?:   [...string]
}

// ============================================================================
// REQUEST - HTTP request definition
// ============================================================================

#Request: {
	method: #Method
	path:   string & =~"^/"

	headers: {
		[string]: string
	} | *{}

	query: {
		[string]: string | int | bool
	} | *{}

	body: _ | *null

	// Request body schema (for validation/documentation)
	body_schema?: #BodySchema
}

#Method: "GET" | "POST" | "PUT" | "PATCH" | "DELETE" | "HEAD" | "OPTIONS"

#BodySchema: {
	type: "object" | "array" | "string" | "number" | "boolean"
	required?: [...string]
	properties?: {
		[string]: #PropertySchema
	}
}

#PropertySchema: {
	type:        string
	description?: string
	format?:     string
	minimum?:    number
	maximum?:    number
	minLength?:  int
	maxLength?:  int
	pattern?:    string
	enum?:       [...]
	required?:   bool
	default?:    _
}

// ============================================================================
// RESPONSE - Expected HTTP response
// ============================================================================

#Response: {
	status: int & >=100 & <600

	// Example response body (for documentation)
	example: _ | *null

	// Validation checks
	checks: {
		[string]: #Check
	} | *{}

	// Expected headers
	headers: {
		[string]: string
	} | *{}
}

#Check: {
	rule: string
	why:  string | *""

	// AI context for this specific check
	ai_context?: {
		severity?:       "critical" | "high" | "medium" | "low"
		common_mistake?: string
		how_to_implement?: string
	}
}

// ============================================================================
// RULE - Global rules that apply to multiple responses
// ============================================================================

#Rule: {
	name:        intent.#Name
	description: string

	// When does this rule apply?
	when: #RuleWhen

	// What to check
	check: #RuleCheck

	// Example of a violation
	example?: _
}

#RuleWhen: {
	status?: string  // e.g., ">= 400", "200", "2xx"
	method?: #Method | "*"
	path?:   string  // Regex pattern
}

#RuleCheck: {
	body_must_not_contain: [...string] | *[]
	body_must_contain:     [...string] | *[]
	fields_must_exist:     [...string] | *[]
	fields_must_not_exist: [...string] | *[]
	header_must_exist:     string | *""
	header_must_not_exist: string | *""
}

// ============================================================================
// PROVIDER STATES - Preconditions (stolen from Pact)
// ============================================================================

#ProviderState: {
	name:        string
	description: string
	setup?:      string
	teardown?:   string
	requires?:   [...string]
}

// ============================================================================
// AUTHORIZATION - Who can do what
// ============================================================================

#Authorization: {
	model: "rbac" | "abac" | "ownership" | "none"

	roles?: [...string]

	rules?: [...#AuthRule]
}

#AuthRule: {
	action:   "create" | "read" | "update" | "delete" | "list" | "*"
	resource: string
	allowed:  [...string]
	filter?:  string
}

// ============================================================================
// TIMING - Performance expectations
// ============================================================================

#Timing: {
	expected_latency_ms?: int
	timeout_ms?:          int

	// For async operations
	async?: {
		pattern:          "polling" | "webhook" | "websocket"
		status_endpoint?: string
		poll_interval_ms?: int
		terminal_states:  [...string]
	}
}

// ============================================================================
// IDEMPOTENCY - Retry safety
// ============================================================================

#Idempotency: {
	safe:       bool        // No side effects
	idempotent: bool        // Same result on retry
	retry_safe: bool        // Safe to retry on failure
	key_header?: string     // Header for idempotency key
	window?:     string     // How long keys are stored
}

// ============================================================================
// PAGINATION - List handling
// ============================================================================

#Pagination: {
	style: "cursor" | "offset" | "page"

	cursor?: {
		param:          string | *"cursor"
		response_field: string | *"next_cursor"
		encoding:       "opaque" | "base64" | "json"
	}

	offset?: {
		limit_param:   string | *"limit"
		offset_param:  string | *"offset"
		default_limit: int | *20
		max_limit:     int | *100
	}

	response_envelope?: {
		items_field:    string
		total_field?:   string
		has_more_field?: string
	}
}
