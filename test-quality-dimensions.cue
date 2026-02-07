package quality_test

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
	name: "Quality Test API"

	description: """
		This spec tests all quality dimensions
		"""

	audience: "Test audience"

	success_criteria: [
		"All features work",
		"API is secure"
	]

	config: {
		base_url:   "https://api.example.com"
		timeout_ms: 5000
		headers: {}
	}

	features: [
		{
			name: "Complete Feature"

			description: """
				A feature with all required fields
				"""

			behaviors: [
				{
					name:   "complete-behavior"
					intent: "Test complete behavior"

					request: {
						method: "GET"
						path:   "/test"
					}

					response: {
						status: 200
						example: {
							message: "success"
						}
						checks: [
							{
								rule: "status == 200"
								why: "Status should be 200"
							}
						]
					}

					notes: "This is a complete behavior"

					requires: []
					tags: []
					captures: {}
				}
			]
		}
	]

	rules: [
		{
			name: "test-rule"
			condition: "true"
			message: "Test rule passed"
		}
	]

	anti_patterns: [
		"avoid-sensitive-data"
	]

	ai_hints: "This is a test API"
}