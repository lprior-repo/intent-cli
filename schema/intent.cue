// Intent v2.0 Schema
// Human-writes, AI-verifies, AI-implements
package intent

// Main specification type
#Spec: {
	name:        string
	description: string
	audience:    string | *""
	version:     string | *"1.0.0"

	success_criteria: [...string]

	config: #Config | *#DefaultConfig

	features:      [...#Feature]
	rules:         [...#Rule]
	anti_patterns: [...#AntiPattern]

	// Optional AI implementation hints
	ai_hints?: #AIHints
}

// Configuration for the spec
#Config: {
	base_url:   string | *""
	timeout_ms: int | *5000
	headers:    #Headers | *{}
}

#DefaultConfig: #Config & {}

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

	// Additional context for humans/AI
	notes: string | *""

	// Dependencies - behaviors that must run first
	requires: [...#Identifier] | *[]

	// Tags for filtering
	tags: [...string] | *[]

	request: #Request

	response: #Response

	// Capture values for later use
	captures: #Captures | *{}
}

// HTTP methods
#Method: "GET" | "POST" | "PUT" | "PATCH" | "DELETE" | "HEAD" | "OPTIONS"

// Valid identifier pattern
#Identifier: =~"^[a-z][a-z0-9_-]*$"

// HTTP request definition
#Request: {
	method:  #Method
	path:    string
	headers: #Headers | *{}
	query: {...} | *{}
	body: _ | *null
}

// Expected response
#Response: {
	status: int & >=100 & <=599

	// Example of a valid response (for AI learning)
	example: {...} | *null

	// Structured checks
	checks: #Checks | *{}

	// Optional headers to check
	headers?: #Headers
}

// Checks map field paths to check definitions
#Checks: [string]: #Check

// A single check with rule and explanation
#Check: {
	rule: string    // Human-readable rule string
	why:  string | *"" // Explanation of why this matters
}

// Capture definitions
#Captures: [#Identifier]: string

// Global rules that apply to all responses
#Rule: {
	name:        string
	description: string

	// When to apply this rule
	when: #When | *null

	// The check to perform
	check: #RuleCheck

	// Example of correct response
	example: {...} | *null
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

	// Explanation
	why: string | *""
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

	// Codebase context for AI to understand existing patterns
	codebase?: #CodebaseContext
}

// Codebase context schema for existing project patterns
#CodebaseContext: {
	// Common patterns in the codebase
	patterns?: {
		error_handling?: string
		auth_middleware?: string
		validation?: string
		testing?: string
	}

	// Technology stack
	stack?: {
		language?: string
		framework?: string
		database?: string
		orm?: string
		testing?: string
	}

	// Entry points into the codebase
	entry_points?: [...#EntryPoint]

	// Architectural boundaries
	boundaries?: [...#Boundary]
}

// Entry point definition
#EntryPoint: {
	name: string
	path: string
	description?: string
}

// Architectural boundary
#Boundary: {
	name: string
	description?: string
	modules?: [...string]
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

// =============================================================================
// Light Spec - Minimal spec schema for simple tasks
// =============================================================================

// Simplified behavior for light specs (HTTP/API focused)
// No requires, tags, captures, or notes - just the essentials
#LightBehavior: {
	name:   #Identifier
	intent: string // Plain English purpose (the "why")

	request: {
		method: #Method
		path:   string
		body?:  _
	}

	response: {
		status:  int & >=100 & <=599
		checks?: #Checks
	}
}

// Minimal spec for simple tasks
// No config block required, no rules required
#LightSpec: {
	name:        string
	description: string
	behaviors: [#LightBehavior, ...#LightBehavior] // At least one behavior required

	// Optional fields
	anti_patterns?: [...#AntiPattern]
	ai_hints?:      #AIHints
}
