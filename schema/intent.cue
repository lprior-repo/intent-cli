// Intent v2.0 Schema
// Human-writes, AI-verifies, AI-implements
package intent

// Main specification type
#Spec: {
	name:        string
	description: string
	audience:    string
	version:     string

	success_criteria: [...string]

	config: #Config

	features:      [...#Feature]
	rules:         [...#Rule]
	anti_patterns: [...#AntiPattern]

	// AI implementation hints (required)
	ai_hints: #AIHints
}

// Configuration for the spec
#Config: {
	base_url:   string
	timeout_ms: int
	headers:    #Headers
}

// HTTP headers map
#Headers: [string]: string

// Feature groups related behaviors
#Feature: {
	name:        string
	description: string
	behaviors:   [...#Behavior]
}

// A single behavior/test case
#Behavior: {
	name:   #Identifier
	intent: string // Plain English purpose

	// Additional context for humans/AI (optional)
	notes?: string

	// Dependencies - behaviors that must run first (optional)
	requires?: [...#Identifier]

	// Tags for filtering (optional)
	tags?: [...string]

	request: #Request

	response: #Response

	// Capture values for later use (optional)
	captures?: #Captures
}

// HTTP methods
#Method: "GET" | "POST" | "PUT" | "PATCH" | "DELETE" | "HEAD" | "OPTIONS"

// Valid identifier pattern
#Identifier: =~"^[a-z][a-z0-9_-]*$"

// HTTP request definition
#Request: {
	method:  #Method
	path:    string
	headers: #Headers
	query: {...}
	body?: _ // Optional (e.g., GET requests have no body)
}

// Expected response
#Response: {
	status: int & >=100 & <=599

	// Example of a valid response (for AI learning) (optional)
	example?: {...}

	// Structured checks
	checks: #Checks

	// Headers to check (optional)
	headers?: #Headers
}

// Checks map field paths to check definitions
#Checks: [string]: #Check

// A single check with rule and explanation
#Check: {
	rule: string    // Human-readable rule string
	why?:  string    // Explanation of why this matters (optional)
}

// Capture definitions
#Captures: [#Identifier]: string

// Global rules that apply to all responses
#Rule: {
	name:        string
	description: string

	// When to apply this rule (optional, null means always apply)
	when?: #When | *null

	// The check to perform
	check: #RuleCheck

	// Example of correct response
	example?: {...}
}

// Conditions for when a rule applies
#When: {
	status?: string // e.g., ">= 400"
	method?: #Method
	path?:   string // regex pattern
}

// Rule checks for global rules
#RuleCheck: {
	body_must_not_contain?: [...string]
	body_must_contain?: [...string]
	fields_must_exist?: [...string]
	fields_must_not_exist?: [...string]
	header_must_exist?:     string
	header_must_not_exist?: string
}

// Anti-patterns with good/bad examples
#AntiPattern: {
	name:        string
	description: string

	// What NOT to do
	bad_example: {...}

	// What TO do
	good_example: {...}

	// Explanation (optional)
	why?: string
}

// AI implementation hints
#AIHints: {
	implementation?: {
		suggested_stack?: [...string]
	}

	entities?: [string]: #EntityHint

	security?: {
		password_hashing?: string
		jwt_algorithm?:    string
		jwt_expiry?:       string
		rate_limiting?:    string
	}

	pitfalls?: [...string]
}

#EntityHint: {
	fields?: [string]: string | #FieldHint
}

#FieldHint: {
	description?: string
	type?:        string
	validation?:  string
	example?:     string
	sensitive?:   bool | *false
}
