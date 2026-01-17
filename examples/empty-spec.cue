package empty

import "github.com/intent-cli/intent/schema:intent"

// Empty spec with no features for testing error handling
spec: intent.#Spec & {
	name:        "Empty Spec"
	description: "Completely empty spec for error testing"
	audience:    "Test users"
	version:     "0.1.0"

	success_criteria: []

	config: {
		base_url:   "http://localhost:8080"
		timeout_ms: 5000
		headers: {}
	}

	features: []
	rules: []
	anti_patterns: []

	ai_hints: {
		implementation: {
			suggested_stack: []
		}
		pitfalls: []
	}
}
