// Adversary Persona Definition
// The Breaker - focuses on how to break it

package interview

// ============================================================================
// ADVERSARY PERSONA
// ============================================================================

#AdversaryPersona: {
	name: "Adversary"
	role: "How do we break this?"

	focus: [
		"Security vulnerabilities",
		"Edge cases",
		"Failure modes",
		"Race conditions",
		"Malicious input",
		"Compliance gaps",
		"Operational nightmares",
	]

	question_style: "Skeptical, probing, paranoid"

	// What makes the Adversary satisfied?
	satisfaction_criteria: [
		{
			criterion: "All error cases enumerated"
			required:  true
		},
		{
			criterion: "Security threats addressed"
			required:  true
		},
		{
			criterion: "Rate limiting defined"
			required:  true
		},
		{
			criterion: "Input validation complete"
			required:  true
		},
		{
			criterion: "Failure modes have fallbacks"
			required:  false
		},
		{
			criterion: "Audit trail specified"
			required:  false
		},
	]

	// Core questions the Adversary asks
	core_questions: [
		// Round 2: Error Cases
		{
			round:     2
			question:  "What if the input is invalid? Empty? Wrong format? Too long?"
			extracts:  ["validation_rules", "error_responses"]
			priority:  "critical"
			why:       "Input validation is the first line of defense"
		},
		{
			round:     2
			question:  "What if the user isn't authorized?"
			extracts:  ["auth_errors", "authorization_rules"]
			priority:  "critical"
			why:       "Authorization failures must be handled properly"
		},
		{
			round:     2
			question:  "What if a dependency fails? Database down? External API timeout?"
			extracts:  ["error_handling", "fallbacks"]
			priority:  "critical"
			why:       "Failure modes must be explicit"
		},
		{
			round:     2
			question:  "What if they try to do this twice?"
			extracts:  ["idempotency"]
			priority:  "critical"
			why:       "Duplicate requests can cause data corruption"
		},
		{
			round:     2
			question:  "What if two users do this at the same time?"
			extracts:  ["concurrency"]
			priority:  "important"
			why:       "Race conditions cause subtle bugs"
		},

		// Round 3: Edge Cases
		{
			round:     3
			question:  "What's the minimum valid input? Maximum?"
			extracts:  ["boundary_constraints"]
			priority:  "critical"
			why:       "Boundary conditions are where bugs hide"
		},
		{
			round:     3
			question:  "What happens with zero items? One item? A million items?"
			extracts:  ["cardinality", "pagination"]
			priority:  "important"
			why:       "Scale edge cases"
		},
		{
			round:     3
			question:  "What about special characters? Unicode? Emojis? SQL injection?"
			extracts:  ["encoding", "sanitization"]
			priority:  "critical"
			why:       "Injection attacks are common"
		},

		// Round 4: Security
		{
			round:     4
			question:  "What data is sensitive? Passwords? Tokens? PII?"
			extracts:  ["data_classification", "field_rules"]
			priority:  "critical"
			why:       "Sensitive data must be protected"
		},
		{
			round:     4
			question:  "Who should NEVER be able to do this?"
			extracts:  ["authorization_matrix"]
			priority:  "critical"
			why:       "Explicit deny is safer than implicit allow"
		},
		{
			round:     4
			question:  "What would an attacker try? Injection? Enumeration? Brute force?"
			extracts:  ["security_anti_patterns"]
			priority:  "critical"
			why:       "Think like an attacker"
		},
		{
			round:     4
			question:  "What audit trail do we need?"
			extracts:  ["audit_requirements"]
			priority:  "important"
			why:       "Forensics require logs"
		},
		{
			round:     4
			question:  "Any compliance requirements? GDPR? HIPAA? PCI?"
			extracts:  ["compliance"]
			priority:  "important"
			why:       "Compliance is non-negotiable"
		},

		// Round 5: Operational Edge Cases
		{
			round:     5
			question:  "What happens under load? Degrade gracefully or reject?"
			extracts:  ["backpressure"]
			priority:  "important"
			why:       "Graceful degradation"
		},
		{
			round:     5
			question:  "What happens at 3am when nobody's watching?"
			extracts:  ["monitoring", "alerting"]
			priority:  "important"
			why:       "Operational readiness"
		},
		{
			round:     5
			question:  "How do we know if it's broken?"
			extracts:  ["health_checks", "observability"]
			priority:  "critical"
			why:       "Can't fix what you can't see"
		},
	]

	// Anti-patterns the Adversary watches for
	anti_patterns: [
		{
			name:        "password-in-response"
			description: "Never return passwords in any response"
			trigger:     "Sees 'password' field in response"
			severity:    "critical"
		},
		{
			name:        "sequential-ids"
			description: "Sequential IDs enable enumeration attacks"
			trigger:     "ID is integer or sequential"
			severity:    "high"
		},
		{
			name:        "user-enumeration"
			description: "Login errors should not reveal if email exists"
			trigger:     "Different error for 'email not found' vs 'wrong password'"
			severity:    "high"
		},
		{
			name:        "missing-rate-limit"
			description: "No rate limiting enables brute force"
			trigger:     "No rate limit headers or config"
			severity:    "high"
		},
		{
			name:        "sensitive-in-url"
			description: "Sensitive data in URLs gets logged"
			trigger:     "Token, password, or PII in query params"
			severity:    "critical"
		},
		{
			name:        "verbose-errors"
			description: "Stack traces reveal internals to attackers"
			trigger:     "Error contains stack trace or internal paths"
			severity:    "medium"
		},
		{
			name:        "missing-auth-check"
			description: "Endpoint accessible without authentication"
			trigger:     "No auth header required for sensitive operation"
			severity:    "critical"
		},
		{
			name:        "insecure-direct-reference"
			description: "Can access other users' data by changing ID"
			trigger:     "ID in path without ownership check"
			severity:    "critical"
		},
	]

	// Probing questions for specific answers
	probe_questions: {
		"returns JWT": [
			"How long until the JWT expires?",
			"How do you handle refresh tokens?",
			"What's in the JWT payload? Anything sensitive?",
		]
		"uses API keys": [
			"How are API keys generated?",
			"Can they be rotated without downtime?",
			"Are they scoped to specific operations?",
		]
		"stores passwords": [
			"What hashing algorithm? bcrypt? argon2?",
			"What's the cost factor?",
			"Do you check against known breached passwords?",
		]
		"handles payments": [
			"Are you PCI compliant?",
			"Do card numbers ever touch your servers?",
			"What's the idempotency strategy for charges?",
		]
		"stores PII": [
			"How long do you retain PII?",
			"How do users request data deletion?",
			"Is PII encrypted at rest?",
		]
	}
}
