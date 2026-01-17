package error_handling_api

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
	name: "Error Handling API"

	description: """
		Comprehensive error handling patterns demonstrating EARS syntax
		for unwanted behaviors. Shows proper HTTP status codes, structured
		error responses, validation failures, rate limiting, and recovery.
		"""

	audience: "API developers and testing teams"

	version: "1.0.0"

	success_criteria: [
		"All error responses use structured format with code and message",
		"HTTP status codes match error types (4xx client, 5xx server)",
		"Validation errors include field-specific details",
		"Rate limiting returns proper 429 responses",
		"Server errors maintain consistency without exposing internals",
	]

	config: {
		base_url:   "http://localhost:8080"
		timeout_ms: 5000
		headers: {}
	}

	features: [
		{
			name: "Resource Not Found (404)"

			description: """
				IF a requested resource does not exist, THEN the system SHALL
				return a 404 status with a structured error containing a specific
				error code and human-readable message identifying what was not found.
				"""

			behaviors: [
				{
					name:   "get-nonexistent-resource"
					intent: "Requesting a resource that doesn't exist returns 404"

					request: {
						method: "GET"
						path:   "/api/users/usr_nonexistent"
						headers: {}
						query: {}
						body: null
					}

					response: {
						status: 404
						headers: {}

						example: {
							error: {
								code:    "RESOURCE_NOT_FOUND"
								message: "User with id 'usr_nonexistent' was not found"
								path:    "/api/users/usr_nonexistent"
							}
						}

						checks: {
							"error.code": {
								rule: "equals RESOURCE_NOT_FOUND"
								why:  "Specific error code enables client-side error handling"
							}
							"error.message": {
								rule: "non-empty string"
								why:  "Human-readable message for debugging"
							}
							"error.path": {
								rule: "equals /api/users/usr_nonexistent"
								why:  "Echo requested path for traceability"
							}
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <resource does not exist>, THEN THE SYSTEM SHALL <return 404>
						"""
					requires: []
					tags: ["error", "404", "unwanted"]
					captures: {}
				},
				{
					name:   "get-nonexistent-nested-resource"
					intent: "Nested resource not found returns specific error"

					request: {
						method: "GET"
						path:   "/api/organizations/org_123/teams/team_999"
						headers: {}
						query: {}
						body: null
					}

					response: {
						status: 404
						headers: {}

						example: {
							error: {
								code:    "TEAM_NOT_FOUND"
								message: "Team 'team_999' not found in organization 'org_123'"
								details: {
									organization_id: "org_123"
									team_id:         "team_999"
								}
							}
						}

						checks: {
							"error.code": {
								rule: "equals TEAM_NOT_FOUND"
								why:  "Specific code for nested resource type"
							}
							"error.details.organization_id": {
								rule: "equals org_123"
								why:  "Include context for debugging"
							}
						}
					}

					notes: "Nested resources provide context in error details"
					requires: []
					tags: ["error", "404", "nested"]
					captures: {}
				},
			]
		},
		{
			name: "Validation Errors (400)"

			description: """
				IF request validation fails, THEN the system SHALL return 400
				with field-specific error details showing which fields failed
				validation and why.
				"""

			behaviors: [
				{
					name:   "create-resource-missing-required-field"
					intent: "Missing required field returns validation error"

					request: {
						method: "POST"
						path:   "/api/users"
						headers: {}
						query: {}
						body: {
							name: "John Doe"
							// missing required 'email' field
						}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "VALIDATION_ERROR"
								message: "Request validation failed"
								fields: {
									email: "Field 'email' is required but was not provided"
								}
							}
						}

						checks: {
							"error.code": {
								rule: "equals VALIDATION_ERROR"
								why:  "Standard code for all validation failures"
							}
							"error.fields.email": {
								rule: "non-empty string"
								why:  "Field-specific error message"
							}
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <required field missing>, THEN THE SYSTEM SHALL <return 400>
						"""
					requires: []
					tags: ["error", "400", "validation"]
					captures: {}
				},
				{
					name:   "create-resource-invalid-email-format"
					intent: "Invalid email format returns validation error"

					request: {
						method: "POST"
						path:   "/api/users"
						headers: {}
						query: {}
						body: {
							name:  "Jane Doe"
							email: "not-an-email"
						}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "VALIDATION_ERROR"
								message: "Request validation failed"
								fields: {
									email: "Value 'not-an-email' is not a valid email address"
								}
							}
						}

						checks: {
							"error.code": {
								rule: "equals VALIDATION_ERROR"
								why:  "Consistent validation error code"
							}
							"error.fields.email": {
								rule: "contains not a valid email"
								why:  "Explains validation rule that failed"
							}
						}
					}

					notes: "Format validation failures include the invalid value"
					requires: []
					tags: ["error", "400", "validation", "format"]
					captures: {}
				},
				{
					name:   "create-resource-multiple-validation-errors"
					intent: "Multiple validation failures return all errors at once"

					request: {
						method: "POST"
						path:   "/api/products"
						headers: {}
						query: {}
						body: {
							name:     ""
							price:    -10
							quantity: 1000000
						}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "VALIDATION_ERROR"
								message: "Request validation failed on 3 fields"
								fields: {
									name:     "Field 'name' cannot be empty"
									price:    "Field 'price' must be greater than 0"
									quantity: "Field 'quantity' exceeds maximum allowed value of 10000"
								}
							}
						}

						checks: {
							"error.code": {
								rule: "equals VALIDATION_ERROR"
								why:  "Single error code for all validation failures"
							}
							"error.fields.name": {
								rule: "non-empty string"
								why:  "Each field gets specific error message"
							}
							"error.fields.price": {
								rule: "non-empty string"
								why:  "Price validation explained"
							}
							"error.fields.quantity": {
								rule: "non-empty string"
								why:  "Quantity limit communicated"
							}
						}
					}

					notes: "All validation errors returned in single response, not one at a time"
					requires: []
					tags: ["error", "400", "validation", "multiple"]
					captures: {}
				},
			]
		},
		{
			name: "Authorization Errors (401/403)"

			description: """
				IF authentication is missing or invalid, THEN THE SYSTEM SHALL
				return 401. IF user is authenticated but lacks permission, THEN
				THE SYSTEM SHALL return 403.
				"""

			behaviors: [
				{
					name:   "access-without-auth-token"
					intent: "Missing authentication returns 401"

					request: {
						method: "GET"
						path:   "/api/protected/resource"
						headers: {}
						query: {}
						body: null
					}

					response: {
						status: 401
						headers: {}

						example: {
							error: {
								code:    "UNAUTHORIZED"
								message: "Authentication required. Provide a valid token in Authorization header"
							}
						}

						checks: {
							"error.code": {
								rule: "equals UNAUTHORIZED"
								why:  "Standard code for missing/invalid auth"
							}
							"error.message": {
								rule: "contains Authorization header"
								why:  "Hints at how to fix the issue"
							}
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <no auth token>, THEN THE SYSTEM SHALL <return 401>
						"""
					requires: []
					tags: ["error", "401", "auth"]
					captures: {}
				},
				{
					name:   "access-with-expired-token"
					intent: "Expired authentication token returns 401"

					request: {
						method: "GET"
						path:   "/api/protected/resource"
						headers: {
							"Authorization": "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.expired"
						}
						query: {}
						body: null
					}

					response: {
						status: 401
						headers: {}

						example: {
							error: {
								code:    "TOKEN_EXPIRED"
								message: "Authentication token has expired"
								expires_at: "2024-01-15T10:30:00Z"
							}
						}

						checks: {
							"error.code": {
								rule: "equals TOKEN_EXPIRED"
								why:  "Specific code distinguishes expiration from other auth failures"
							}
						}
					}

					notes: "Expired tokens get specific error code to trigger token refresh"
					requires: []
					tags: ["error", "401", "token", "expired"]
					captures: {}
				},
				{
					name:   "access-forbidden-insufficient-permissions"
					intent: "Authenticated user without permission returns 403"

					request: {
						method: "DELETE"
						path:   "/api/admin/users/usr_123"
						headers: {
							"Authorization": "Bearer valid-token-but-not-admin"
						}
						query: {}
						body: null
					}

					response: {
						status: 403
						headers: {}

						example: {
							error: {
								code:    "FORBIDDEN"
								message: "Insufficient permissions to perform this action"
								required_permission: "admin:users:delete"
							}
						}

						checks: {
							"error.code": {
								rule: "equals FORBIDDEN"
								why:  "403 means authenticated but not authorized"
							}
							"error.required_permission": {
								rule: "non-empty string"
								why:  "Tell user what permission is needed"
							}
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <insufficient permissions>, THEN THE SYSTEM SHALL <return 403>
						401 = not authenticated, 403 = not authorized
						"""
					requires: []
					tags: ["error", "403", "permissions"]
					captures: {}
				},
			]
		},
		{
			name: "Conflict Errors (409)"

			description: """
				IF a resource already exists or state conflict occurs, THEN
				THE SYSTEM SHALL return 409 with details about the conflict.
				"""

			behaviors: [
				{
					name:   "create-duplicate-resource"
					intent: "Creating resource with duplicate unique field returns 409"

					request: {
						method: "POST"
						path:   "/api/users"
						headers: {}
						query: {}
						body: {
							email: "existing@example.com"
							name:  "Test User"
						}
					}

					response: {
						status: 409
						headers: {}

						example: {
							error: {
								code:    "RESOURCE_CONFLICT"
								message: "A user with email 'existing@example.com' already exists"
								conflict_field: "email"
								existing_id:    "usr_xyz789"
							}
						}

						checks: {
							"error.code": {
								rule: "equals RESOURCE_CONFLICT"
								why:  "Standard code for uniqueness violations"
							}
							"error.conflict_field": {
								rule: "equals email"
								why:  "Identify which field caused the conflict"
							}
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <unique constraint violated>, THEN THE SYSTEM SHALL <return 409>
						"""
					requires: []
					tags: ["error", "409", "conflict", "duplicate"]
					captures: {}
				},
				{
					name:   "update-stale-resource"
					intent: "Concurrent update conflict returns 409"

					request: {
						method: "PUT"
						path:   "/api/documents/doc_123"
						headers: {
							"If-Match": "old-etag-value"
						}
						query: {}
						body: {
							title: "Updated Title"
						}
					}

					response: {
						status: 409
						headers: {}

						example: {
							error: {
								code:           "CONCURRENT_MODIFICATION"
								message:        "Document was modified by another user"
								current_etag:   "new-etag-value"
								submitted_etag: "old-etag-value"
							}
						}

						checks: {
							"error.code": {
								rule: "equals CONCURRENT_MODIFICATION"
								why:  "Specific code for optimistic locking failures"
							}
							"error.current_etag": {
								rule: "non-empty string"
								why:  "Provide current ETag for retry"
							}
						}
					}

					notes: "Optimistic locking failures include current version for retry"
					requires: []
					tags: ["error", "409", "conflict", "concurrent"]
					captures: {}
				},
			]
		},
		{
			name: "Rate Limiting (429)"

			description: """
				IF request rate exceeds limit, THEN THE SYSTEM SHALL return 429
				with Retry-After header indicating when client can retry.
				"""

			behaviors: [
				{
					name:   "rate-limit-exceeded"
					intent: "Exceeding rate limit returns 429 with retry information"

					request: {
						method: "GET"
						path:   "/api/data"
						headers: {}
						query: {}
						body: null
					}

					response: {
						status: 429

						example: {
							error: {
								code:    "RATE_LIMIT_EXCEEDED"
								message: "Rate limit of 100 requests per minute exceeded"
								limit:   100
								window:  "60s"
								retry_after: 45
							}
						}

						checks: {
							"error.code": {
								rule: "equals RATE_LIMIT_EXCEEDED"
								why:  "Standard code for rate limiting"
							}
							"error.limit": {
								rule: "integer >= 1"
								why:  "Communicate the rate limit to client"
							}
							"error.retry_after": {
								rule: "integer >= 0"
								why:  "Seconds until rate limit resets"
							}
						}

						headers: {
							"Retry-After": "45"
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <rate limit exceeded>, THEN THE SYSTEM SHALL <return 429>
						Include Retry-After header per RFC 6585
						"""
					requires: []
					tags: ["error", "429", "rate-limit"]
					captures: {}
				},
			]
		},
		{
			name: "Server Errors (500)"

			description: """
				IF an unexpected server error occurs, THEN THE SYSTEM SHALL
				return 5xx status with generic message and correlation ID for
				support tracking, WITHOUT exposing internal implementation details.
				"""

			behaviors: [
				{
					name:   "internal-server-error"
					intent: "Server error returns 500 with correlation ID"

					request: {
						method: "GET"
						path:   "/api/unstable/endpoint"
						headers: {}
						query: {}
						body: null
					}

					response: {
						status: 500
						headers: {}

						example: {
							error: {
								code:           "INTERNAL_SERVER_ERROR"
								message:        "An unexpected error occurred. Please contact support with the correlation ID"
								correlation_id: "req_abc123xyz789"
							}
						}

						checks: {
							"error.code": {
								rule: "equals INTERNAL_SERVER_ERROR"
								why:  "Generic code for unexpected server errors"
							}
							"error.correlation_id": {
								rule: "string matching req_[a-z0-9]+"
								why:  "Correlation ID for support to trace logs"
							}
							"error.message": {
								rule: "does not contain stack trace"
								why:  "SECURITY: Never expose internal details"
							}
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <unexpected error>, THEN THE SYSTEM SHALL <return 500>
						Never expose stack traces, database errors, or file paths
						"""
					requires: []
					tags: ["error", "500", "server-error"]
					captures: {}
				},
				{
					name:   "service-unavailable"
					intent: "Temporary unavailability returns 503 with retry info"

					request: {
						method: "POST"
						path:   "/api/process"
						headers: {}
						query: {}
						body: {
							data: "test"
						}
					}

					response: {
						status: 503
						headers: {}

						example: {
							error: {
								code:    "SERVICE_UNAVAILABLE"
								message: "Service is temporarily unavailable due to maintenance"
								retry_after: 300
							}
						}

						checks: {
							"error.code": {
								rule: "equals SERVICE_UNAVAILABLE"
								why:  "Specific code for planned/unplanned downtime"
							}
							"error.retry_after": {
								rule: "integer >= 0"
								why:  "Seconds until service should be available"
							}
						}

						headers: {
							"Retry-After": "300"
						}
					}

					notes: "503 indicates temporary condition, client should retry"
					requires: []
					tags: ["error", "503", "unavailable"]
					captures: {}
				},
			]
		},
		{
			name: "Method Not Allowed (405)"

			description: """
				IF client uses unsupported HTTP method, THEN THE SYSTEM SHALL
				return 405 with Allow header listing supported methods.
				"""

			behaviors: [
				{
					name:   "unsupported-http-method"
					intent: "Unsupported method returns 405 with allowed methods"

					request: {
						method: "DELETE"
						path:   "/api/readonly/resource"
						headers: {}
						query: {}
						body: null
					}

					response: {
						status: 405

						example: {
							error: {
								code:    "METHOD_NOT_ALLOWED"
								message: "Method DELETE is not allowed for this resource"
								allowed_methods: ["GET", "HEAD", "OPTIONS"]
							}
						}

						checks: {
							"error.code": {
								rule: "equals METHOD_NOT_ALLOWED"
								why:  "Standard code for unsupported methods"
							}
							"error.allowed_methods": {
								rule: "non-empty array"
								why:  "Tell client which methods are supported"
							}
						}

						headers: {
							"Allow": "GET, HEAD, OPTIONS"
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <unsupported HTTP method>, THEN THE SYSTEM SHALL <return 405>
						Must include Allow header per RFC 7231
						"""
					requires: []
					tags: ["error", "405", "method"]
					captures: {}
				},
			]
		},
		{
			name: "Request Too Large (413)"

			description: """
				IF request exceeds size limits, THEN THE SYSTEM SHALL return 413
				with details about size limits.
				"""

			behaviors: [
				{
					name:   "payload-too-large"
					intent: "Request exceeding size limit returns 413"

					request: {
						method: "POST"
						path:   "/api/upload"
						headers: {}
						query: {}
						body: {
							data: "... very large payload ..."
						}
					}

					response: {
						status: 413
						headers: {}

						example: {
							error: {
								code:         "PAYLOAD_TOO_LARGE"
								message:      "Request body exceeds maximum allowed size"
								max_size:     "10MB"
								actual_size:  "25MB"
							}
						}

						checks: {
							"error.code": {
								rule: "equals PAYLOAD_TOO_LARGE"
								why:  "Standard code for size limit violations"
							}
							"error.max_size": {
								rule: "non-empty string"
								why:  "Communicate size limit to client"
							}
						}
					}

					notes: """
						EARS Pattern: UNWANTED
						IF <payload exceeds limit>, THEN THE SYSTEM SHALL <return 413>
						"""
					requires: []
					tags: ["error", "413", "size-limit"]
					captures: {}
				},
			]
		},
	]

	rules: [
		{
			name:        "structured-errors"
			description: "All error responses must have consistent structure"

			when: {status: ">= 400"}

			check: {
				fields_must_exist: ["error.code", "error.message"]
			}

			example: {
				error: {
					code:    "ERROR_CODE"
					message: "Human readable description"
				}
			}
		},
		{
			name:        "no-stack-traces"
			description: "Server errors must never expose stack traces"

			when: {status: ">= 500"}

			check: {
				body_must_not_contain: ["stack", "trace", "Exception", "Error at line"]
			}
		},
		{
			name:        "content-type-header"
			description: "All responses must declare content type"

			check: {
				header_must_exist: "Content-Type"
			}
		},
		{
			name:        "correlation-id-on-errors"
			description: "Server errors should include correlation ID"

			when: {status: ">= 500"}

			check: {
				fields_must_exist: ["error.correlation_id"]
			}
		},
	]

	anti_patterns: [
		{
			name:        "generic-error-messages"
			description: "Error messages should be specific, not generic"

			bad_example: {
				error: {
					code:    "ERROR"
					message: "Something went wrong"
				}
			}

			good_example: {
				error: {
					code:    "VALIDATION_ERROR"
					message: "Request validation failed"
					fields: {
						email: "Field 'email' is required but was not provided"
					}
				}
			}

			why: """
				Generic errors don't help developers debug. Provide specific
				error codes and actionable messages.
				"""
		},
		{
			name:        "exposing-internal-details"
			description: "Never expose internal implementation details in errors"

			bad_example: {
				error: {
					message: "SQLException: Column 'user_id' does not exist in table 'users'"
				}
			}

			good_example: {
				error: {
					code:           "INTERNAL_SERVER_ERROR"
					message:        "An unexpected error occurred"
					correlation_id: "req_abc123"
				}
			}

			why: """
				Stack traces, database errors, and file paths reveal internal
				architecture and create security vulnerabilities. Use correlation
				IDs for debugging instead.
				"""
		},
		{
			name:        "wrong-status-codes"
			description: "Use semantically correct HTTP status codes"

			bad_example: {
				status: 200
				body: {
					error: "User not found"
				}
			}

			good_example: {
				status: 404
				body: {
					error: {
						code:    "USER_NOT_FOUND"
						message: "User with id 'usr_123' was not found"
					}
				}
			}

			why: """
				Status code 200 means success. Errors should use appropriate
				4xx/5xx codes so clients can handle them correctly.
				"""
		},
		{
			name:        "missing-retry-information"
			description: "Rate limit and unavailable errors need retry guidance"

			bad_example: {
				status: 429
				body: {
					error: {
						message: "Too many requests"
					}
				}
			}

			good_example: {
				status: 429
				headers: {
					"Retry-After": "60"
				}
				body: {
					error: {
						code:        "RATE_LIMIT_EXCEEDED"
						message:     "Rate limit exceeded"
						retry_after: 60
					}
				}
			}

			why: """
				Clients need to know when they can retry. Include Retry-After
				header and retry timing in response body.
				"""
		},
	]

	ai_hints: {
		implementation: {
			suggested_stack: ["Node.js", "Express", "TypeScript"]
		}

		security: {
			password_hashing: "Not applicable (error handling patterns only)"
			jwt_algorithm:    "HS256 for token validation examples"
			jwt_expiry:       "1 hour for examples"
			rate_limiting:    "100 requests per minute per IP"
		}

		entities: {
			error_response: {
				fields: {
					code:           "string, SCREAMING_SNAKE_CASE, machine-readable"
					message:        "string, human-readable explanation"
					correlation_id: "string, prefixed 'req_', for 5xx errors"
					fields:         "object, field-specific errors for validation (400)"
					retry_after:    "integer, seconds until retry allowed (429, 503)"
				}
			}
		}

		pitfalls: [
			"Don't return 200 OK with error in body",
			"Don't expose stack traces or database errors in responses",
			"Don't use generic error messages like 'Something went wrong'",
			"Don't forget Retry-After header for 429 and 503",
			"Don't mix 401 (not authenticated) with 403 (not authorized)",
			"Don't return validation errors one at a time - return all at once",
			"Don't omit correlation IDs from server errors (5xx)",
			"Don't expose existence of resources in error messages (security)",
		]
	}
}
