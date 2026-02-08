import "github.com/intent-cli/intent/schema:intent"

// Test: Valid spec with all required fields
spec: intent.#Spec & {
	name: "Valid Test Spec"
	description: "This is a valid test spec with all required fields"
	audience: "Test audience"
	version: "1.0.0"

	success_criteria: ["Test criterion 1", "Test criterion 2"]

	config: {
		base_url: "http://localhost:8080"
		timeout_ms: 5000
	}

	features: [
		{
			name: "Test Feature"
			description: "A test feature"
			behaviors: [
				{
					name: "test-behavior"
					intent: "To test something"
					request: {
						method: "GET"
						path: "/test"
					}
					response: {
						status: 200
						checks: {}
					}
				}
			]
		}
	]

	rules: []

	anti_patterns: []

	ai_hints: {}
}
