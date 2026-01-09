// Invalid test cases for #LightSpec - these should FAIL cue vet -c
// Run each test individually to verify validation catches the error
package intent

// =============================================================================
// Invalid Test Cases (uncomment one at a time to test validation)
// =============================================================================

// TEST 1: Missing required 'name' field
// Uncomment to test:
// missing_name: #LightSpec & {
// 	description: "No name provided"
// 	behaviors: [{
// 		name:   "test"
// 		intent: "Test"
// 		request: {method: "GET", path: "/"}
// 		response: {status: 200}
// 	}]
// }

// TEST 2: Missing required 'description' field
// Uncomment to test:
// missing_description: #LightSpec & {
// 	name: "Test"
// 	behaviors: [{
// 		name:   "test"
// 		intent: "Test"
// 		request: {method: "GET", path: "/"}
// 		response: {status: 200}
// 	}]
// }

// TEST 3: Empty behaviors list (requires at least one)
// Uncomment to test:
// empty_behaviors: #LightSpec & {
// 	name:        "Test"
// 	description: "Test"
// 	behaviors:   []
// }

// TEST 4: Invalid HTTP method
// Uncomment to test:
// invalid_method: #LightSpec & {
// 	name:        "Test"
// 	description: "Test"
// 	behaviors: [{
// 		name:   "test"
// 		intent: "Test"
// 		request: {method: "INVALID", path: "/"}
// 		response: {status: 200}
// 	}]
// }

// TEST 5: Invalid status code (out of range)
// Uncomment to test:
// invalid_status: #LightSpec & {
// 	name:        "Test"
// 	description: "Test"
// 	behaviors: [{
// 		name:   "test"
// 		intent: "Test"
// 		request: {method: "GET", path: "/"}
// 		response: {status: 999}
// 	}]
// }

// TEST 6: Invalid behavior name (doesn't match identifier pattern)
// Uncomment to test:
// invalid_behavior_name: #LightSpec & {
// 	name:        "Test"
// 	description: "Test"
// 	behaviors: [{
// 		name:   "Invalid Name With Spaces"
// 		intent: "Test"
// 		request: {method: "GET", path: "/"}
// 		response: {status: 200}
// 	}]
// }

// TEST 7: Missing behavior intent
// Uncomment to test:
// missing_intent: #LightSpec & {
// 	name:        "Test"
// 	description: "Test"
// 	behaviors: [{
// 		name: "test"
// 		request: {method: "GET", path: "/"}
// 		response: {status: 200}
// 	}]
// }

// TEST 8: Missing request path
// Uncomment to test:
// missing_path: #LightSpec & {
// 	name:        "Test"
// 	description: "Test"
// 	behaviors: [{
// 		name:   "test"
// 		intent: "Test"
// 		request: {method: "GET"}
// 		response: {status: 200}
// 	}]
// }

// This file validates as-is (all invalid cases are commented out)
// The valid placeholder below makes this file pass cue vet
_placeholder: "This file contains commented invalid test cases"
