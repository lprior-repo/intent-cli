package test_invalid_json

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
	name: "Invalid JSON Test"

	description: "This spec has invalid JSON in response.example"

	audience: "Test"

	version: "1.0.0"

	success_criteria: ["Test case for validation"]

	config: {
		base_url:   "http://localhost:8080"
		timeout_ms: 5000
		headers: {}
	}

	features: [
		{
			name: "Test Feature"

			description: "Feature with invalid JSON example"

			behaviors: [
				{
					name:   "invalid-json-example"
					intent: "Should fail validation"

					request: {
						method: "GET"
						path:   "/test"
						headers: {}
						query: {}
					}

					response: {
						status: 200

						// This is invalid JSON - 'json' and 'invalid' are unquoted identifiers
						example: {message: "ok", invalid: json}

						checks: {}
					}

					captures: {}
				},
			]
		},
	]

	rules: []

	anti_patterns: []

	ai_hints: {
		implementation: {}
		entities: {}
		security: {}
		pitfalls: null
	}
}
