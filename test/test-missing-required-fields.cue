import "github.com/intent-cli/intent/schema:intent"

// This spec is missing the required 'name' field
spec: intent.#Spec & {
	description: "Test spec missing name field"
	audience: "Test audience"
	version: "1.0.0"

	success_criteria: ["Test criterion"]

	config: {
		base_url: "http://localhost:8080"
		timeout_ms: 5000
	}

	features: []

	rules: []

	anti_patterns: []

	ai_hints: {}
}
