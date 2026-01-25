// Test file for common types
package common

// Test #Severity enumeration
testSeverity: {
	info:     #Severity & "info"
	warning:  #Severity & "warning"
	error:    #Severity & "error"
	critical: #Severity & "critical"
	// This should fail if we try invalid values
	// invalid: #Severity & "invalid"
}

// Test #HTTPMethod enumeration
testHTTPMethod: {
	get:     #HTTPMethod & "GET"
	post:    #HTTPMethod & "POST"
	put:     #HTTPMethod & "PUT"
	patch:   #HTTPMethod & "PATCH"
	delete:  #HTTPMethod & "DELETE"
	head:    #HTTPMethod & "HEAD"
	options: #HTTPMethod & "OPTIONS"
	// invalid: #HTTPMethod & "INVALID"
}

// Test #Identifier pattern
testIdentifier: {
	valid1: #Identifier & "user-api"
	valid2: #Identifier & "create_user"
	valid3: #Identifier & "test123"
	// These should fail validation:
	// invalid1: #Identifier & "User-API"  // uppercase
	// invalid2: #Identifier & "123test"   // starts with number
	// invalid3: #Identifier & "test"     // starts with underscore
}

// Test #Headers type
testHeaders: {
	example: #Headers & {
		"Content-Type":  "application/json"
		"Authorization": "Bearer token"
		"X-Custom":      "value"
	}
}

// Test #Timestamp format (ISO 8601)
testTimestamp: {
	valid1: #Timestamp & "2026-01-25T09:51:06Z"
	valid2: #Timestamp & "2026-01-25T09:51:06.123456Z"
	valid3: #Timestamp & "2026-01-25T09:51:06-06:00"
	// invalid: #Timestamp & "2026-01-25"  // date only
}

// Test #URL format
testURL: {
	valid1: #URL & "https://api.example.com"
	valid2: #URL & "http://localhost:8080"
	valid3: #URL & "https://example.com/path?query=value"
	// invalid: #URL & "not-a-url"
}

// Test #Email format
testEmail: {
	valid1: #Email & "user@example.com"
	valid2: #Email & "test.user+tag@example.co.uk"
	// invalid: #Email & "not-an-email"
}

// Test #StatusCode range
testStatusCode: {
	ok:          #StatusCode & 200
	created:     #StatusCode & 201
	badRequest:  #StatusCode & 400
	notFound:    #StatusCode & 404
	serverError: #StatusCode & 500
	// invalid1: #StatusCode & 99   // too low
	// invalid2: #StatusCode & 600  // too high
}

// Test #NonEmptyString
testNonEmptyString: {
	valid: #NonEmptyString & "content"
	// invalid: #NonEmptyString & ""  // empty string
}

// Test #UUID format
testUUID: {
	valid1: #UUID & "550e8400-e29b-41d4-a716-446655440000"
	valid2: #UUID & "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
	// invalid: #UUID & "not-a-uuid"
}

// Test #Duration format
testDuration: {
	valid1: #Duration & "1h"
	valid2: #Duration & "30m"
	valid3: #Duration & "45s"
	valid4: #Duration & "1h30m45s"
	// invalid: #Duration & "invalid"
}

// Test #InterviewStage enumeration
testInterviewStage: {
	discovery:   #InterviewStage & "discovery"
	refinement:  #InterviewStage & "refinement"
	validation:  #InterviewStage & "validation"
	complete:    #InterviewStage & "complete"
	paused:      #InterviewStage & "paused"
}

// Test #ProfileType enumeration
testProfileType: {
	api:      #ProfileType & "api"
	cli:      #ProfileType & "cli"
	event:    #ProfileType & "event"
	data:     #ProfileType & "data"
	workflow: #ProfileType & "workflow"
	ui:       #ProfileType & "ui"
}

// Test #Perspective enumeration
testPerspective: {
	user:      #Perspective & "user"
	developer: #Perspective & "developer"
	ops:       #Perspective & "ops"
	security:  #Perspective & "security"
	business:  #Perspective & "business"
}

// Test #QuestionCategory enumeration
testQuestionCategory: {
	happy_path:    #QuestionCategory & "happy_path"
	error_case:    #QuestionCategory & "error_case"
	edge_case:     #QuestionCategory & "edge_case"
	constraint:    #QuestionCategory & "constraint"
	dependency:    #QuestionCategory & "dependency"
	nonfunctional: #QuestionCategory & "nonfunctional"
}

// Test #QuestionPriority enumeration
testQuestionPriority: {
	critical:     #QuestionPriority & "critical"
	important:    #QuestionPriority & "important"
	nice_to_have: #QuestionPriority & "nice_to_have"
}

// Test #GapType enumeration
testGapType: {
	inversion:    #GapType & "inversion"
	second_order: #GapType & "second_order"
	checklist:    #GapType & "checklist"
	coverage:     #GapType & "coverage"
	security:     #GapType & "security"
}

// Test #ConflictType enumeration
testConflictType: {
	cap_theorem:           #ConflictType & "cap_theorem"
	scope_paradox:         #ConflictType & "scope_paradox"
	security_usability:    #ConflictType & "security_usability"
	performance_consistency: #ConflictType & "performance_consistency"
}

// Test #ProtocolStatus enumeration
testProtocolStatus: {
	ok:             #ProtocolStatus & "ok"
	error:          #ProtocolStatus & "error"
	requires_input: #ProtocolStatus & "requires_input"
}
