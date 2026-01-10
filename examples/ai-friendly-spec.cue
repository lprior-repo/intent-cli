// Example: What I (the AI) want to receive
// This is my desire path - the format that makes implementation easy

package user_api

spec: {
	name: "user-registration"

	intent: "Users can create an account with email and password"

	behaviors: [
		{
			name: "create-user"
			intent: "Register a new user"

			example: {
				request: {
					method: "POST"
					path: "/users"
					body: {
						email: "user@example.com"
						password: "SecurePass123!"
						name: "Test User"
					}
				}
				response: {
					status: 201
					body: {
						id: "usr_abc123xyz"
						email: "user@example.com"
						name: "Test User"
						created_at: "2024-01-15T10:30:00Z"
					}
				}
			}

			errors: [
				{ status: 400, code: "INVALID_EMAIL", when: "email format is wrong" },
				{ status: 400, code: "WEAK_PASSWORD", when: "password < 8 chars or missing uppercase/number" },
				{ status: 409, code: "EMAIL_EXISTS", when: "email already registered" },
			]
		},
		{
			name: "login"
			intent: "Authenticate and get a token"

			example: {
				request: {
					method: "POST"
					path: "/auth/login"
					body: {
						email: "user@example.com"
						password: "SecurePass123!"
					}
				}
				response: {
					status: 200
					body: {
						token: "eyJhbGciOiJIUzI1NiIs..."
						expires_in: 3600
					}
				}
			}

			errors: [
				// IMPORTANT: Same error for both cases (security)
				{ status: 401, code: "INVALID_CREDENTIALS", when: "wrong password OR email not found" },
			]

			notes: "NEVER return different errors for 'email not found' vs 'wrong password'"
		},
	]

	never: [
		"Return password in any response, even hashed",
		"Use sequential integer IDs (use prefixed random: usr_xxx)",
		"Return different errors for 'email not found' vs 'wrong password'",
		"Store passwords in plain text (use bcrypt)",
		"Put sensitive data in URLs",
	]

	check_command: "intent check user-api.cue --target http://localhost:8080"
}
