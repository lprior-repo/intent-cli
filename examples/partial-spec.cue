package partial

import "github.com/intent-cli/intent/schema:intent"

// Minimal partial spec with no features
spec: intent.#Spec & {
	name:        "Partial API Spec"
	description: "A partially defined spec for testing graceful degradation"
	audience:    "Test users"
	version:     "0.1.0"

	success_criteria: [
		"Test that analysis works with minimal spec",
	]

	config: {
		base_url:   "http://localhost:8080"
		timeout_ms: 5000
		headers: {}
	}

	// Empty features - this is the partial part
	features: []

	rules: []

	anti_patterns: []

	// ai_hints field is optional, omit it entirely
}
