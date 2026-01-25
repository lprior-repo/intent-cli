package ai

import "github.com/intent-cli/intent/schema/ai"

// Test that AI Request envelope is properly defined
testAIRequest: ai.#Request & {
	id:      "req-001"
	command: "vision.start"
	args: {
		profile: "api"
	}
}

// Test AI Request with optional options
testAIRequestWithOptions: ai.#Request & {
	id:      "req-002"
	command: "vision.answer"
	args: {
		session_id:  "sess-abc"
		question_id: "q1"
		answer:      "Building a REST API"
	}
	options: {
		timeout_ms: 5000
		dry_run:    true
	}
}

// Test that AI Response envelope is properly defined
testAIResponse: ai.#Response & {
	id:      "req-001"
	success: true
	command: "vision.start"
	data: {
		session_id: "sess-abc"
		phase:      "vision"
	}
	errors: []
	next_actions: []
	metadata: {
		timestamp:   "2026-01-25T10:00:00Z"
		version:     "4.0.0"
		duration_ms: 100
		exit_code:   0
	}
}

// Test AI Response with error
testAIResponseWithError: ai.#Response & {
	id:      "req-002"
	success: false
	command: "vision.answer"
	data: {}
	errors: [{
		code:        "SESSION_NOT_FOUND"
		message:     "Session sess-xyz does not exist"
		location:    "session_store"
		fix_hint:    "Start a new session with vision.start"
		fix_command: "{\"command\":\"vision.start\",\"args\":{\"profile\":\"api\"}}"
	}]
	next_actions: []
	metadata: {
		timestamp:   "2026-01-25T10:00:00Z"
		version:     "4.0.0"
		duration_ms: 50
		exit_code:   1
	}
}

// Test AI Error structure
testAIError: ai.#Error & {
	code:    "SPEC_NOT_FOUND"
	message: "Specification file not found"
}

// Test AI Error with all optional fields
testAIErrorComplete: ai.#Error & {
	code:        "VALIDATION_FAILED"
	message:     "Schema validation failed"
	location:    "spec.cue:42"
	fix_hint:    "Add missing required field 'version'"
	fix_command: "{\"command\":\"doctor\",\"args\":{\"spec\":\"spec.cue\"}}"
}

// Test NextAction structure
testNextAction: ai.#NextAction & {
	command:  "{\"id\":\"auto\",\"command\":\"vision.answer\",\"args\":{\"session_id\":\"sess-abc\",\"question_id\":\"q2\"}}"
	reason:   "Answer next required question"
	priority: 1
}

// Test NextAction with blocks
testNextActionWithBlocks: ai.#NextAction & {
	command:  "{\"id\":\"5\",\"command\":\"vision.critique\",\"args\":{\"session_id\":\"sess-abc\"}}"
	reason:   "Run critique after all questions answered"
	priority: 2
	blocks: ["req-001", "req-002", "req-003"]
}

// Test Metadata structure
testMetadata: ai.#Metadata & {
	timestamp:   "2026-01-25T10:00:00Z"
	version:     "4.0.0"
	duration_ms: 150
	exit_code:   0
}

// Test Response with next_actions
testResponseWithNextActions: ai.#Response & {
	id:      "req-003"
	success: true
	command: "vision.start"
	data: {
		session_id: "sess-123"
		questions: []
	}
	errors: []
	next_actions: [
		{
			command:  "{\"id\":\"auto\",\"command\":\"vision.answer\",\"args\":{\"session_id\":\"sess-123\",\"question_id\":\"q1\"}}"
			reason:   "Answer first question"
			priority: 1
		},
		{
			command:  "{\"id\":\"auto\",\"command\":\"vision.critique\",\"args\":{\"session_id\":\"sess-123\"}}"
			reason:   "Run critique after answering all questions"
			priority: 2
		},
	]
	metadata: {
		timestamp:   "2026-01-25T10:00:00Z"
		version:     "4.0.0"
		duration_ms: 120
		exit_code:   0
	}
}

// Test priority constraint (must be 1-5)
testPriorityMin: ai.#NextAction & {
	command:  "test"
	reason:   "test"
	priority: 1
}

testPriorityMax: ai.#NextAction & {
	command:  "test"
	reason:   "test"
	priority: 5
}

// Test timeout_ms constraint (must be >0)
testTimeoutValid: ai.#Request & {
	id:      "req-004"
	command: "test"
	args: {}
	options: {
		timeout_ms: 1
	}
}
