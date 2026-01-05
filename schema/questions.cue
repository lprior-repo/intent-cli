// Interview Questions Database
// 5 rounds × 5+ perspectives = comprehensive interrogation
// This is the question library AI uses to guide users
package intent

#QuestionDatabase: {
	// Round 1: Core Intent (What are we building?)
	round_1: #Round1

	// Round 2: Error Cases (What can go wrong?)
	round_2: #Round2

	// Round 3: Edge Cases (Where are the boundaries?)
	round_3: #Round3

	// Round 4: Security & Compliance (How do we keep this safe?)
	round_4: #Round4

	// Round 5: Operations (How does this run in production?)
	round_5: #Round5
}

// ROUND 1: CORE INTENT
#Round1: {
	user_1: {
		id:          "r1-user-1"
		round:       1
		perspective: "user"
		category:    "happy_path"
		priority:    "critical"
		question:    "In one sentence, what should this do?"
		context:     "We're starting with the core intent. Give us the simplest possible description."
		example:     "Allow users to log in with email and password"
		extract_into: "name"
	}

	user_2: {
		id:          "r1-user-2"
		round:       1
		perspective: "user"
		category:    "happy_path"
		priority:    "critical"
		question:    "Who will use this? What are they trying to accomplish?"
		context:     "Understanding your audience helps us design the right behavior."
		example:     "Mobile app users who want to create accounts and manage their profile"
		extract_into: "audience,success_criteria"
	}

	user_3: {
		id:          "r1-user-3"
		round:       1
		perspective: "user"
		category:    "happy_path"
		priority:    "critical"
		question:    "Walk me through the happy path. What happens step-by-step?"
		context:     "Describe the ideal flow from start to finish. Don't worry about errors yet."
		example:     "User enters email and password → we validate both → create account → return profile"
		extract_into: "behaviors"
	}

	user_4: {
		id:          "r1-user-4"
		round:       1
		perspective: "user"
		category:    "happy_path"
		priority:    "critical"
		question:    "What's the MOST important thing this MUST do correctly?"
		context:     "Every system has one thing that absolutely cannot fail. What's yours?"
		example:     "Never expose passwords. Ever."
		extract_into: "critical_constraint,anti_patterns"
	}

	user_5: {
		id:          "r1-user-5"
		round:       1
		perspective: "user"
		category:    "happy_path"
		priority:    "critical"
		question:    "What would make this a failure, even if it 'works'?"
		context:     "Sometimes technically correct is still wrong. What's the business failure mode?"
		example:     "If login errors reveal whether an email exists, that's a security hole."
		extract_into: "anti_patterns"
	}

	dev_1: {
		id:          "r1-dev-1"
		round:       1
		perspective: "developer"
		category:    "constraint"
		priority:    "important"
		question:    "What data model does this operate on? List the key entities."
		context:     "Understanding the domain helps us catch inconsistencies."
		example:     "Users (id, email, password_hash, profile), Tokens (token, user_id, expires_at)"
		extract_into: "entities"
	}

	dev_2: {
		id:          "r1-dev-2"
		round:       1
		perspective: "developer"
		category:    "dependency"
		priority:    "important"
		question:    "What external systems does this talk to? (databases, APIs, caches, etc)"
		context:     "Dependencies affect implementation complexity and failure modes."
		example:     "PostgreSQL (user data), Redis (session cache), SendGrid (emails)"
		extract_into: "dependencies"
	}

	dev_3: {
		id:          "r1-dev-3"
		round:       1
		perspective: "developer"
		category:    "constraint"
		priority:    "important"
		question:    "Do you already have code for this, or is it completely new?"
		context:     "Greenfield vs. integration changes the complexity."
		example:     "We have a User table and auth middleware, need to refactor for new flow"
		extract_into: "development_context"
	}

	ops_1: {
		id:          "r1-ops-1"
		round:       1
		perspective: "ops"
		category:    "nonfunctional"
		priority:    "important"
		question:    "Where will this run? (cloud provider, on-prem, edge, multiple regions?)"
		context:     "Deployment target affects how we think about scaling and resilience."
		example:     "AWS (us-east-1 and eu-west-1 for GDPR), RDS, Lambda or EC2?"
		extract_into: "deployment_target"
	}

	security_1: {
		id:          "r1-security-1"
		round:       1
		perspective: "security"
		category:    "constraint"
		priority:    "critical"
		question:    "What kind of authentication does this need?"
		context:     "Auth method cascades through the whole design."
		example:     "API key for server-to-server, JWT for mobile apps, session cookies for web"
		extract_into: "auth_method"
	}

	business_1: {
		id:          "r1-business-1"
		round:       1
		perspective: "business"
		category:    "constraint"
		priority:    "important"
		question:    "What's success? How do you measure if this worked?"
		context:     "Business metrics drive technical decisions."
		example:     "75% of trial users convert to paid accounts"
		extract_into: "success_criteria"
	}
}

// ROUND 2: ERROR CASES
#Round2: {
	dev_1: {
		id:          "r2-dev-1"
		round:       2
		perspective: "developer"
		category:    "error_case"
		priority:    "critical"
		question:    "What if the input is invalid? Give examples."
		context:     "Validation failures are the most common error case."
		example:     "Empty email, bad format, password too short, SQL injection attempt"
		extract_into: "validation_rules,error_cases"
	}

	security_1: {
		id:          "r2-security-1"
		round:       2
		perspective: "security"
		category:    "error_case"
		priority:    "critical"
		question:    "What if the user isn't authorized? What should happen?"
		context:     "Authorization failures need consistent responses."
		example:     "Missing/invalid token → 401, valid token but wrong permissions → 403"
		extract_into: "auth_rules,error_cases"
	}

	dev_2: {
		id:          "r2-dev-2"
		round:       2
		perspective: "developer"
		category:    "error_case"
		priority:    "critical"
		question:    "What if a dependency fails? (database timeout, external API down, cache miss)"
		context:     "Degradation strategies are critical for reliability."
		example:     "Database down → return cached data if available, else 503 with retry-after"
		extract_into: "error_handling,fallbacks"
	}

	dev_3: {
		id:          "r2-dev-3"
		round:       2
		perspective: "developer"
		category:    "error_case"
		priority:    "important"
		question:    "What if they try to do this twice in a row? (same operation, same user)"
		context:     "Idempotency prevents bugs and data corruption."
		example:     "Creating the same account twice should either succeed once or fail both times"
		extract_into: "idempotency_rules"
	}

	dev_4: {
		id:          "r2-dev-4"
		round:       2
		perspective: "developer"
		category:    "error_case"
		priority:    "important"
		question:    "What if two different users try to do this at the same time?"
		context:     "Concurrency bugs are subtle and dangerous."
		example:     "Both claim the last inventory item, or transfer same money twice"
		extract_into: "concurrency_rules"
	}

	ops_1: {
		id:          "r2-ops-1"
		round:       2
		perspective: "ops"
		category:    "error_case"
		priority:    "important"
		question:    "What's the acceptable error rate? When should alerts fire?"
		context:     "You need to know what's normal vs. what's broken."
		example:     "< 0.1% 4xx errors is normal, > 1% is alert, > 5% is critical"
		extract_into: "monitoring_thresholds"
	}
}

// ROUND 3: EDGE CASES
#Round3: {
	qa_1: {
		id:          "r3-qa-1"
		round:       3
		perspective: "developer"  // Often QA perspective, but dev can answer
		category:    "edge_case"
		priority:    "important"
		question:    "What's the minimum valid input? What's the maximum?"
		context:     "Boundaries reveal assumptions."
		example:     "Email: 1-254 chars, password: 8-128 chars, usernames: alphanumeric + underscore"
		extract_into: "field_constraints"
	}

	qa_2: {
		id:          "r3-qa-2"
		round:       3
		perspective: "developer"
		category:    "edge_case"
		priority:    "important"
		question:    "What happens with zero items? One item? A million items?"
		context:     "Collection handling is where bugs hide."
		example:     "Empty list returns 200 OK with empty array, paginate at 100 items"
		extract_into: "collection_rules"
	}

	qa_3: {
		id:          "r3-qa-3"
		round:       3
		perspective: "developer"
		category:    "edge_case"
		priority:    "nice_to_have"
		question:    "What about special characters? Unicode? Emojis? Null bytes?"
		context:     "Text encoding surprises cause production bugs."
		example:     "Accept UTF-8, reject null bytes, emoji in names OK, SQL special chars escaped"
		extract_into: "encoding_rules"
	}

	ops_1: {
		id:          "r3-ops-1"
		round:       3
		perspective: "ops"
		category:    "edge_case"
		priority:    "important"
		question:    "What about timezones? Daylight saving time? Date boundaries?"
		context:     "Temporal logic is a major pain point."
		example:     "Store everything UTC, convert for display, account for DST in scheduling"
		extract_into: "temporal_rules"
	}

	security_1: {
		id:          "r3-security-1"
		round:       3
		perspective: "security"
		category:    "edge_case"
		priority:    "critical"
		question:    "What about old/expired credentials? Revoked tokens? Compromised secrets?"
		context:     "Token lifecycle is critical."
		example:     "Expired JWT → 401, revoked token → 401, check revocation list per request"
		extract_into: "token_lifecycle"
	}
}

// ROUND 4: SECURITY & COMPLIANCE
#Round4: {
	security_1: {
		id:          "r4-security-1"
		round:       4
		perspective: "security"
		category:    "constraint"
		priority:    "critical"
		question:    "What data is sensitive? (PII, passwords, tokens, credit cards)"
		context:     "Sensitive data needs special handling."
		example:     "Passwords, email, API keys, credit card numbers"
		extract_into: "sensitive_fields,data_classification"
	}

	security_2: {
		id:          "r4-security-2"
		round:       4
		perspective: "security"
		category:    "constraint"
		priority:    "critical"
		question:    "Who should NEVER be able to do this? What are the deny rules?"
		context:     "Sometimes it's easier to define what's forbidden."
		example:     "Admins can't delete their own account, users can't access other profiles"
		extract_into: "authorization_matrix"
	}

	security_3: {
		id:          "r4-security-3"
		round:       4
		perspective: "security"
		category:    "constraint"
		priority:    "critical"
		question:    "What would an attacker try? (injection, enumeration, brute force, replay, etc)"
		context:     "Think like an attacker to design defenses."
		example:     "Email enumeration, password spraying, token reuse, SQL injection"
		extract_into: "threat_model,anti_patterns"
	}

	ops_1: {
		id:          "r4-ops-1"
		round:       4
		perspective: "ops"
		category:    "constraint"
		priority:    "important"
		question:    "What audit trail do we need? What should be logged?"
		context:     "Logging supports compliance, debugging, and forensics."
		example:     "Log: user_id, action, timestamp, ip, success/failure, reason"
		extract_into: "audit_requirements"
	}

	business_1: {
		id:          "r4-business-1"
		round:       4
		perspective: "business"
		category:    "constraint"
		priority:    "important"
		question:    "Are there compliance requirements? (GDPR, HIPAA, PCI, CCPA, SOC2, etc)"
		context:     "Compliance is non-negotiable."
		example:     "GDPR: right to be forgotten, data portability, PII protection"
		extract_into: "compliance_requirements"
	}
}

// ROUND 5: OPERATIONS
#Round5: {
	ops_1: {
		id:          "r5-ops-1"
		round:       5
		perspective: "ops"
		category:    "nonfunctional"
		priority:    "important"
		question:    "How fast should this respond? What's acceptable latency?"
		context:     "Performance is a feature."
		example:     "P95 < 100ms, P99 < 500ms, unacceptable if > 1s"
		extract_into: "performance"
	}

	ops_2: {
		id:          "r5-ops-2"
		round:       5
		perspective: "ops"
		category:    "nonfunctional"
		priority:    "important"
		question:    "How many requests per second? How many concurrent users?"
		context:     "Scale requirements drive architecture."
		example:     "1000 RPS, 50k concurrent users, peak 3x average"
		extract_into: "scale"
	}

	ops_3: {
		id:          "r5-ops-3"
		round:       5
		perspective: "ops"
		category:    "nonfunctional"
		priority:    "important"
		question:    "What happens under load? Degrade gracefully or reject requests?"
		context:     "Backpressure strategy matters."
		example:     "Queue requests, reject with 429 if queue > 10k, fail fast after 30s timeout"
		extract_into: "backpressure_strategy"
	}

	ops_4: {
		id:          "r5-ops-4"
		round:       5
		perspective: "ops"
		category:    "nonfunctional"
		priority:    "important"
		question:    "How do we know if it's broken? What should trigger alerts?"
		context:     "Observability is operational. Define signals, not just metrics."
		example:     "Alert: error_rate > 1%, latency_p95 > 500ms, all_instances_down"
		extract_into: "monitoring"
	}

	business_1: {
		id:          "r5-business-1"
		round:       5
		perspective: "business"
		category:    "nonfunctional"
		priority:    "important"
		question:    "What's the required availability / SLA?"
		context:     "SLA drives redundancy and backup strategies."
		example:     "99.9% uptime (8.76 hours/year downtime), 99.99% if payment-critical"
		extract_into: "availability"
	}

	ops_5: {
		id:          "r5-ops-5"
		round:       5
		perspective: "ops"
		category:    "constraint"
		priority:    "nice_to_have"
		question:    "How do we deploy changes without breaking clients?"
		context:     "Versioning strategy prevents breakage."
		example:     "Semantic versioning, backwards-compatible changes for N versions, deprecation warnings"
		extract_into: "versioning_strategy"
	}
}
