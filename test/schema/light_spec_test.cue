// Test cases for #LightSpec and #LightBehavior schemas
package intent

// =============================================================================
// Valid Test Cases
// =============================================================================

// Minimal valid light spec with single behavior
minimal_valid: #LightSpec & {
	name:        "Minimal API Test"
	description: "The simplest possible light spec"
	behaviors: [{
		name:   "health-check"
		intent: "Verify the service is running"
		request: {
			method: "GET"
			path:   "/health"
		}
		response: {
			status: 200
		}
	}]
}

// Light spec with multiple behaviors
multi_behavior: #LightSpec & {
	name:        "User API Test"
	description: "Test user CRUD operations"
	behaviors: [
		{
			name:   "list-users"
			intent: "Get all users in the system"
			request: {
				method: "GET"
				path:   "/users"
			}
			response: {
				status: 200
			}
		},
		{
			name:   "create-user"
			intent: "Create a new user account"
			request: {
				method: "POST"
				path:   "/users"
				body: {
					name:  "Test User"
					email: "test@example.com"
				}
			}
			response: {
				status: 201
				checks: {
					id: {
						rule: "exists"
						why:  "Created user must have an ID"
					}
				}
			}
		},
		{
			name:   "get-user"
			intent: "Retrieve a specific user by ID"
			request: {
				method: "GET"
				path:   "/users/1"
			}
			response: {
				status: 200
				checks: {
					name: {
						rule: "is_string"
						why:  "User must have a name"
					}
					email: {
						rule: "is_string"
						why:  "User must have an email"
					}
				}
			}
		},
	]
}

// Light spec with all optional fields populated
full_featured: #LightSpec & {
	name:        "Complete Light Spec"
	description: "Light spec demonstrating all optional features"
	behaviors: [{
		name:   "get-item"
		intent: "Retrieve an item from inventory"
		request: {
			method: "GET"
			path:   "/items/123"
		}
		response: {
			status: 200
			checks: {
				quantity: {
					rule: ">= 0"
					why:  "Quantity cannot be negative"
				}
			}
		}
	}]
	anti_patterns: [{
		name:        "No stack traces"
		description: "Never expose internal errors to clients"
		bad_example: {
			error:       "Internal Server Error"
			stack_trace: "at line 42..."
		}
		good_example: {
			error: "An unexpected error occurred"
			code:  "INTERNAL_ERROR"
		}
		why: "Stack traces leak implementation details"
	}]
	ai_hints: {
		implementation: {
			suggested_stack: ["Node.js", "Express", "PostgreSQL"]
		}
		pitfalls: [
			"Don't expose internal IDs",
			"Always validate input",
		]
	}
}

// Light spec with all HTTP methods
all_methods: #LightSpec & {
	name:        "HTTP Methods Test"
	description: "Test all supported HTTP methods"
	behaviors: [
		{
			name:   "get-resource"
			intent: "Read a resource"
			request: {method: "GET", path: "/resource"}
			response: {status: 200}
		},
		{
			name:   "create-resource"
			intent: "Create a resource"
			request: {method: "POST", path: "/resource", body: {data: "test"}}
			response: {status: 201}
		},
		{
			name:   "replace-resource"
			intent: "Replace a resource"
			request: {method: "PUT", path: "/resource/1", body: {data: "new"}}
			response: {status: 200}
		},
		{
			name:   "update-resource"
			intent: "Partially update a resource"
			request: {method: "PATCH", path: "/resource/1", body: {data: "patch"}}
			response: {status: 200}
		},
		{
			name:   "delete-resource"
			intent: "Delete a resource"
			request: {method: "DELETE", path: "/resource/1"}
			response: {status: 204}
		},
		{
			name:   "head-resource"
			intent: "Get resource headers only"
			request: {method: "HEAD", path: "/resource"}
			response: {status: 200}
		},
		{
			name:   "options-resource"
			intent: "Get allowed methods"
			request: {method: "OPTIONS", path: "/resource"}
			response: {status: 200}
		},
	]
}

// Light spec with request body
with_body: #LightSpec & {
	name:        "Request Body Test"
	description: "Test specs with various request bodies"
	behaviors: [{
		name:   "complex-body"
		intent: "Send a complex JSON body"
		request: {
			method: "POST"
			path:   "/api/orders"
			body: {
				customer_id: 42
				items: [
					{product_id: 1, quantity: 2},
					{product_id: 3, quantity: 1},
				]
				shipping: {
					address: "123 Main St"
					city:    "Anytown"
					zip:     "12345"
				}
			}
		}
		response: {
			status: 201
			checks: {
				"order_id": {
					rule: "exists"
					why:  "Order must return an ID"
				}
				"total": {
					rule: "> 0"
					why:  "Order total must be positive"
				}
			}
		}
	}]
}
