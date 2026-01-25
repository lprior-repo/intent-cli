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
	simulate: true
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
		timestamp: "2026-01-25T10:00:00Z"
		duration_ms: 100
		version: "4.0.0"
	}
	next_actions: []
}

// Test response with error
testResponseWithError: common.#Response & {
	status: "error"
	data: {}
	metadata: {
		timestamp: "2026-01-25T10:00:00Z"
		duration_ms: 50
		version: "4.0.0"
	}
	next_actions: []
	error: {
		code: "VALIDATION_FAILED"
		message: "Invalid input"
	}
}

// Test response with session_id
testResponseWithSession: common.#Response & {
	status: "ok"
	data: {}
	metadata: {
		timestamp: "2026-01-25T10:00:00Z"
		duration_ms: 75
		version: "4.0.0"
	}
	next_actions: []
	session_id: "sess-abc123"
}

// Test response with next_actions
testResponseWithNextActions: common.#Response & {
	status: "ok"
	data: {}
	metadata: {
		timestamp: "2026-01-25T10:00:00Z"
		duration_ms: 60
		version: "4.0.0"
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
	code: "SPEC_NOT_FOUND"
	message: "Specification file not found"
}

// Test Error with fix - replace_file
testErrorFixReplaceFile: common.#Error & {
	code: "SPEC_INVALID"
	message: "Invalid specification syntax"
	path: "spec.cue"
	fix: {
		type: "replace_file"
		file: "spec.cue"
		content: "corrected content"
	}
}

// Test Error with fix - replace_string
testErrorFixReplaceString: common.#Error & {
	code: "TYPO"
	message: "Typo in code"
	fix: {
		type: "replace_string"
		old: "worng"
		new: "wrong"
	}
}

// Test Error with fix - run_command
testErrorFixRunCommand: common.#Error & {
	code: "FORMAT_NEEDED"
	message: "File needs formatting"
	fix: {
		type: "run_command"
		command: "gleam format"
	}
}
