// Example: Main Session File
// This file contains interview state and generated beads
// Feedback is appended to a separate file (feedback-demo.cue)

package intent

session: #Session & {
	id:         "demo-001"
	created_at: "2026-01-08T10:00:00Z"
	updated_at: "2026-01-08T12:30:00Z"

	// Interview state
	interview: #InterviewSession & {
		id:               "demo-001"
		profile:          "api"
		created_at:       "2026-01-08T10:00:00Z"
		updated_at:       "2026-01-08T11:00:00Z"
		completed_at:     "2026-01-08T11:00:00Z"
		stage:            "complete"
		rounds_completed: 5
		answers: []
		extracted_spec: {
			name:        "User API"
			description: "User management endpoints"
			audience:    "Mobile apps"
		}
		gaps: []
		conflicts: []
	}

	// Generated beads from interview
	beads: [
		{
			id:     "AUTH-001"
			title:  "Implement JWT token validation middleware"
			what:   "Add validate_jwt/1 function to src/middleware/auth.gleam that extracts and validates JWT from Authorization header"
			why:    "Security requirement: all /api/* endpoints need authentication"
			test: {
				command: "curl -H 'Authorization: Bearer invalid' http://localhost:8080/api/users"
				expect: {
					status:        401
					body_contains: "unauthorized"
				}
			}
			done_when: [
				"File src/middleware/auth.gleam exists",
				"Function validate_jwt/1 is exported",
				"Invalid tokens return 401",
				"Missing Authorization header returns 401",
			]
			file:       "src/middleware/auth.gleam"
			edge_cases: ["expired token", "malformed token", "missing header", "wrong algorithm"]
			requires: []
			effort: "20min"
			status: "completed"
		},
		{
			id:     "AUTH-002"
			title:  "Add token refresh endpoint"
			what:   "Create POST /auth/refresh that accepts refresh token and returns new access token"
			why:    "UX: users shouldn't need to re-login when access token expires"
			test: {
				command: "curl -X POST -d '{\"refresh_token\":\"valid\"}' http://localhost:8080/auth/refresh"
				expect: {
					status:        200
					body_contains: "access_token"
				}
			}
			done_when: [
				"POST /auth/refresh endpoint exists",
				"Valid refresh token returns new access token",
				"Invalid refresh token returns 401",
			]
			file:       "src/routes/auth.gleam"
			edge_cases: ["expired refresh token", "revoked token", "reuse detection"]
			requires:   ["AUTH-001"]
			effort:     "15min"
			status:     "in_progress"
		},
		{
			id:     "AUTH-003"
			title:  "Implement token revocation"
			what:   "Add POST /auth/revoke and token blacklist check in validation middleware"
			why:    "Security: users must be able to invalidate tokens on logout or compromise"
			test: {
				command: "curl -X POST -H 'Authorization: Bearer valid' http://localhost:8080/auth/revoke"
				expect: {
					status: 204
				}
			}
			done_when: [
				"POST /auth/revoke endpoint exists",
				"Revoked tokens fail validation",
				"Blacklist persists across restarts",
			]
			file:       "src/routes/auth.gleam"
			edge_cases: ["already revoked", "bulk revocation", "blacklist cleanup"]
			requires:   ["AUTH-001"]
			effort:     "20min"
			status:     "pending"
		},
	]

	// Feedback starts empty - unified from feedback-demo.cue
	// Using open list to allow unification
	feedback: _feedback_entries
}

// Placeholder that feedback file will fill
_feedback_entries: [...#BeadFeedback]
