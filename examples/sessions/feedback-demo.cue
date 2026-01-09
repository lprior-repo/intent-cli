// Example: Feedback File (Append-Only)
// Each execution result is appended here
// CUE unifies this with session-demo.cue automatically

package intent

// Feedback entries - unifies with _feedback_entries in session file
_feedback_entries: [
	{
		bead_id:     "AUTH-001"
		result:      "success"
		reason:      "All tests passing, middleware integrated"
		executed_at: "2026-01-08T11:15:00Z"
		duration_ms: 847000 // ~14 minutes
	},
	{
		bead_id:     "AUTH-002"
		result:      "blocked"
		reason:      "Need clarification on refresh token storage"
		executed_at: "2026-01-08T12:30:00Z"
		duration_ms: 0
		blocked_by: {
			type:          "question"
			details:       "Should refresh tokens be stored in Redis or database?"
			unblocks_when: "Human provides storage preference"
		}
	},
]

// To append new feedback, CLI adds entries to the list above
// CUE automatically validates against #BeadFeedback schema
