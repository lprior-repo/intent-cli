import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
	name: "Test Spec"
	description: "Test description"
	audience: "Test audience"
	version: "1.0.0"

	config: {
		base_url: "http://localhost:8080"
		timeout_ms: 5000
	}

	features: []

	rules: []

	anti_patterns: []

	ai_hints: {}
}
