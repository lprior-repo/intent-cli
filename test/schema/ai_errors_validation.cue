package schema

import "github.com/intent-cli/intent/schema/ai"

// Test that complete AI error structure can be instantiated
testAIPromptError: ai.#PromptErrors.BEAD_LOAD_ERROR

// Test template error
testTemplateError: ai.#TemplateErrors.RENDER_FAILED

// Test model error
testModelError: ai.#ModelErrors.INFERENCE_FAILED

// Test context error
testContextError: ai.#ContextErrors.CONTEXT_OVERFLOW

// Test parsing error
testParsingError: ai.#ParsingErrors.DECODE_FAILED

// Test custom AI error
customAIError: ai.#AIErrorTaxonomy & {
	code:     "AI_CUSTOM_ERROR"
	category: "prompt"
	severity: "error"
	message:  "Custom AI error"
}

// Test AI error with recovery steps
aiErrorWithRecovery: ai.#AIErrorTaxonomy & {
	code:     "AI_BEAD_LOAD_ERROR"
	category: "prompt"
	severity: "error"
	message:  "Failed to load bead"
	recovery: [
		"Check session exists",
		"Verify bead ID",
		"Review CUE syntax",
	]
	retry_allowed: true
}
