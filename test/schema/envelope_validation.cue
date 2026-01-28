package schema

import "github.com/intent-cli/intent/schema/common"

// Test that Request envelope is properly defined
testRequest: common.#Request & {
	command: "test.command"
	params: {
		test: "value"
	}
}

// Test that Request with optional fields works
testRequestOptional: common.#Request & {
	command: "test.command"
	params: {}
	session_id: "sess-123"
	simulate:   true
	select: ["status", "data.score"]
}

// Test that simulate defaults to false
testRequestSimulateDefault: common.#Request & {
	command: "test.command"
	params: {}
}

// Test that Response envelope is properly defined
testResponse: common.#Response & {
	status: "ok"
	data: {
		result: "success"
	}
	metadata: {
		timestamp:   "2026-01-25T10:00:00Z"
		duration_ms: 100
		version:     "4.0.0"
	}
	next_actions: []
}

// Test response with error
testResponseWithError: common.#Response & {
	status: "error"
	data: {}
	metadata: {
		timestamp:   "2026-01-25T10:00:00Z"
		duration_ms: 50
		version:     "4.0.0"
	}
	next_actions: []
	error: {
		code:    "VALIDATION_FAILED"
		message: "Invalid input"
	}
}

// Test response with session_id
testResponseWithSession: common.#Response & {
	status: "ok"
	data: {}
	metadata: {
		timestamp:   "2026-01-25T10:00:00Z"
		duration_ms: 75
		version:     "4.0.0"
	}
	next_actions: []
	session_id: "sess-abc123"
}

// Test response with next_actions
testResponseWithNextActions: common.#Response & {
	status: "ok"
	data: {}
	metadata: {
		timestamp:   "2026-01-25T10:00:00Z"
		duration_ms: 60
		version:     "4.0.0"
	}
	next_actions: [
		{
			command: "next.command"
			params: {action: "continue"}
		},
	]
}

// Test Error structure
testError: common.#Error & {
	code:    "SPEC_NOT_FOUND"
	message: "Specification file not found"
}

// Test Error with fix - replace_file
testErrorFixReplaceFile: common.#Error & {
	code:    "SPEC_INVALID"
	message: "Invalid specification syntax"
	path:    "spec.cue"
	fix: {
		type:    "replace_file"
		file:    "spec.cue"
		content: "corrected content"
	}
}

// Test Error with fix - replace_string
testErrorFixReplaceString: common.#Error & {
	code:    "TYPO"
	message: "Typo in code"
	fix: {
		type: "replace_string"
		old:  "worng"
		new:  "wrong"
	}
}

// Test Error with fix - run_command
testErrorFixRunCommand: common.#Error & {
	code:    "FORMAT_NEEDED"
	message: "File needs formatting"
	fix: {
		type:    "run_command"
		command: "gleam format"
	}
}

// =============================================================================
// CONSOLIDATED ENVELOPE TESTS (AI features merged into common)
// =============================================================================

// Test Request with correlation ID (AI feature)
testRequestWithID: common.#Request & {
	id:      "req-001"
	command: "vision.start"
	params: {profile: "api"}
}

// Test Request with options (AI feature)
testRequestWithOptions: common.#Request & {
	id:      "req-002"
	command: "vision.answer"
	params: {session_id: "sess-abc", question_id: "q1", answer: "Building a REST API"}
	options: {
		timeout_ms: 5000
		dry_run:    true
	}
}

// Test Request with minimal options
testRequestWithTimeoutOnly: common.#Request & {
	id:      "req-003"
	command: "test.command"
	params: {}
	options: {
		timeout_ms: 1000
	}
}

// Test Response with correlation ID and success (AI features)
testResponseWithIDAndSuccess: common.#Response & {
	id:      "req-001"
	status:  "ok"
	success: true
	command: "vision.start"
	data: {session_id: "sess-abc", phase: "vision"}
	metadata: {
		timestamp:   "2026-01-27T10:00:00Z"
		duration_ms: 100
		version:     "4.0.0"
		exit_code:   0
	}
	next_actions: []
}

// Test Response with errors array (AI feature)
testResponseWithErrorsArray: common.#Response & {
	id:      "req-002"
	status:  "error"
	success: false
	command: "vision.answer"
	data: {}
	errors: [{
		code:     "SESSION_NOT_FOUND"
		message:  "Session sess-xyz does not exist"
		location: "session_store"
		fix_hint: "Start a new session with vision.start"
		fix: {
			type:       "replace_string"
			suggestion: "Use correct session ID"
		}
	}]
	metadata: {
		timestamp:   "2026-01-27T10:00:00Z"
		duration_ms: 50
		version:     "4.0.0"
		exit_code:   1
	}
	next_actions: []
}

// Test Response with both error and errors (hybrid approach)
testResponseHybridError: common.#Response & {
	id:      "req-003"
	status:  "error"
	success: false
	command: "test.command"
	data: {}
	error: {
		code:    "VALIDATION_FAILED"
		message: "Schema validation failed"
		path:    "spec.cue:42"
	}
	errors: []
	metadata: {
		timestamp:   "2026-01-27T10:00:00Z"
		duration_ms: 25
		version:     "4.0.0"
		exit_code:   3
	}
	next_actions: []
}

// Test NextAction with reason and priority
testNextActionRich: common.#NextAction & {
	command:  "{\"id\":\"auto\",\"command\":\"vision.answer\",\"args\":{\"session_id\":\"sess-abc\",\"question_id\":\"q2\"}}"
	reason:   "Answer next required question"
	priority: 1
}

// Test NextAction with blocks
testNextActionWithBlocks: common.#NextAction & {
	command:  "{\"id\":\"5\",\"command\":\"vision.critique\",\"args\":{\"session_id\":\"sess-abc\"}}"
	reason:   "Run critique after all questions answered"
	priority: 2
	blocks: ["req-001", "req-002", "req-003"]
}

// Test Response with next_actions_rich
testResponseWithRichNextActions: common.#Response & {
	id:      "req-004"
	status:  "ok"
	success: true
	command: "vision.start"
	data: {session_id: "sess-123", questions: []}
	errors: []
	next_actions: []
	next_actions_rich: [
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
		timestamp:   "2026-01-27T10:00:00Z"
		duration_ms: 120
		version:     "4.0.0"
		exit_code:   0
	}
}

// Test Error with location (alias for path) and fix_hint
testErrorEnhanced: common.#Error & {
	code:     "VALIDATION_FAILED"
	message:  "Schema validation failed"
	location: "spec.cue:42"
	fix_hint: "Add missing required field 'version'"
	fix: {
		type:    "replace_file"
		file:    "spec.cue"
		content: "corrected content"
	}
}

// Test priority constraint (must be 1-5)
testPriorityMin: common.#NextAction & {
	command:  "test"
	reason:   "test"
	priority: 1
}

testPriorityMax: common.#NextAction & {
	command:  "test"
	reason:   "test"
	priority: 5
}

// Test timeout_ms constraint (must be >0)
testTimeoutValid: common.#Request & {
	id:      "req-005"
	command: "test"
	params: {}
	options: {
		timeout_ms: 1
	}
}
