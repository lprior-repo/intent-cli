// What I (Claude) want to receive
// This makes me maximally powerful as an implementer

// THE SPEC IS THE TEST
// I implement until this passes

spec: {
	// One sentence. What am I building?
	intent: "Users can register, login, and manage their profile"

	// The verification command - I run this constantly
	verify: "intent check spec.cue --target http://localhost:8080"

	// Each behavior is a test case I must pass
	tests: [
		// ====== HAPPY PATH ======
		{
			name: "register"
			it: "creates a new user"

			// Exactly what I send
			request: {
				method: "POST"
				path:   "/users"
				body: {
					email:    "test@example.com"
					password: "SecurePass123!"
				}
			}

			// Exactly what I must return
			response: {
				status: 201
				body: {
					id:         "usr_*"      // Wildcard: matches usr_anything
					email:      "test@example.com"
					created_at: "*ISO8601*"  // Wildcard: any valid timestamp
				}
			}

			// What I capture for later tests
			capture: {
				user_id: "body.id"
			}
		},

		// ====== ERROR CASES ======
		{
			name: "register-invalid-email"
			it: "rejects invalid email format"

			request: {
				method: "POST"
				path:   "/users"
				body: {
					email:    "not-an-email"
					password: "SecurePass123!"
				}
			}

			response: {
				status: 400
				body: {
					error: {
						code:    "INVALID_EMAIL"
						message: "*"  // Any message is fine
					}
				}
			}
		},
		{
			name: "register-weak-password"
			it: "rejects weak passwords"

			request: {
				method: "POST"
				path:   "/users"
				body: {
					email:    "test2@example.com"
					password: "weak"
				}
			}

			response: {
				status: 400
				body: {
					error: {
						code: "WEAK_PASSWORD"
					}
				}
			}
		},
		{
			name: "register-duplicate"
			it: "rejects duplicate email"
			after: ["register"]  // Runs after register test

			request: {
				method: "POST"
				path:   "/users"
				body: {
					email:    "test@example.com"  // Same as register
					password: "DifferentPass456!"
				}
			}

			response: {
				status: 409
				body: {
					error: {
						code: "EMAIL_EXISTS"
					}
				}
			}
		},

		// ====== AUTH ======
		{
			name: "login"
			it: "returns JWT for valid credentials"
			after: ["register"]

			request: {
				method: "POST"
				path:   "/auth/login"
				body: {
					email:    "test@example.com"
					password: "SecurePass123!"
				}
			}

			response: {
				status: 200
				body: {
					token:      "*JWT*"  // Wildcard: valid JWT
					expires_in: ">= 3600"  // At least 1 hour
				}
			}

			capture: {
				token: "body.token"
			}
		},
		{
			name: "login-wrong-password"
			it: "rejects wrong password with generic error"
			after: ["register"]

			request: {
				method: "POST"
				path:   "/auth/login"
				body: {
					email:    "test@example.com"
					password: "WrongPassword!"
				}
			}

			response: {
				status: 401
				body: {
					error: {
						code: "INVALID_CREDENTIALS"  // NOT "WRONG_PASSWORD"
					}
				}
			}
		},
		{
			name: "login-unknown-email"
			it: "rejects unknown email with SAME error as wrong password"

			request: {
				method: "POST"
				path:   "/auth/login"
				body: {
					email:    "unknown@example.com"
					password: "AnyPassword123!"
				}
			}

			response: {
				status: 401
				body: {
					error: {
						code: "INVALID_CREDENTIALS"  // SAME as wrong password
					}
				}
			}
		},
	]

	// GUARDRAILS - What I must NEVER do
	never: [
		{
			rule:   "password in response"
			why:    "Security: never expose passwords"
			detect: "body contains 'password'"
		},
		{
			rule:   "sequential IDs"
			why:    "Security: enumeration attacks"
			detect: "id is integer"
		},
		{
			rule:   "different auth errors"
			why:    "Security: user enumeration"
			detect: "401 response has code != INVALID_CREDENTIALS"
		},
	]

	// HINTS - Help me implement correctly
	hints: {
		stack: "Node.js + Express + PostgreSQL"

		patterns: {
			ids:       "Prefix with type: usr_, tok_, etc. Use ULID or UUID."
			passwords: "bcrypt with cost >= 10. NEVER store plain."
			tokens:    "JWT with HS256. Include user_id in payload."
		}

		pitfalls: [
			"Validate email format BEFORE checking database",
			"Hash password BEFORE storing",
			"Check password with constant-time comparison",
			"Always return INVALID_CREDENTIALS for auth failures",
		]
	}
}
