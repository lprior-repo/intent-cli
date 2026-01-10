// Intent Schema - Optimized for AI Implementation
// This is what I (the AI) want to read when implementing

package intent

// The spec I consume
#Spec: {
	name: string

	// What am I building? Natural language.
	intent: string

	// Show me, don't tell me
	behaviors: [...#Behavior]

	// What should I NEVER do?
	never: [...string]

	// How do I verify I'm done?
	check_command: string
}

#Behavior: {
	name:   string
	intent: string

	// THE EXAMPLE - This is what I implement to
	example: {
		request: {
			method: string
			path:   string
			headers?: {...}
			body?: _
		}
		response: {
			status: int
			body:   _
		}
	}

	// What can go wrong?
	errors: [...#Error]

	// Anything I should know?
	notes?: string
}

#Error: {
	status:  int
	code:    string
	when:    string    // "Email format is wrong"
	example?: _        // Show me the error response
}
