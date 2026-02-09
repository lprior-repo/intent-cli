# Intent Schema: #Invariant Type

## Overview

The `#Invariant` type defines global rules that must **always be true** across all behaviors in a specification. Invariants are universal truths about the system - conditions that must hold regardless of context, state, or specific behaviors being executed.

## Type Definition

```cue
#Invariant: {
	name!:        string
	description!: string
	criteria!:    [...string] // What must always be true
}
```

## Required Fields

### `name: string`
A unique identifier for the invariant.
- Should be descriptive and concise
- Used in documentation and validation reports
- Examples:
  - `"passwords-never-exposed"`
  - `"unique-email-addresses"`
  - `"consistent-error-format"`

### `description: string`
A human-readable explanation of the invariant.
- Should explain **why** this must always be true
- Helps humans and AI understand the importance
- Examples:
  - `"Passwords must never appear in any API response for security"`
  - `"Email addresses must be unique across all user accounts"`
  - `"All error responses must follow a consistent structure"`

### `criteria: [...string]`
A list of conditions that define the invariant.
- Each criterion is something that must **always** be true
- Applies to **all behaviors** in the specification
- Declarative - describes "what" not "how"
- Examples:
  - `"password field is absent from all responses"`
  - `"email addresses are case-insensitive and unique"`
  - `"error responses include code and message fields"`

## Invariants vs. Verifications

### Invariants (Global)
- Apply to **all behaviors** in the specification
- Define universal truths about the system
- Enforced across the entire API/system
- Defined at the spec level:

```cue
spec: {
	invariants: [
		{
			name: "passwords-never-exposed"
			description: "Passwords must never appear in any response"
			criteria: [
				"password field is absent from all responses",
				"password_hash field is absent from all responses",
				"plaintext password is never logged",
			]
		},
	]
}
```

### Verifications (Behavior-Specific)
- Apply to **individual behaviors**
- Define postconditions for that behavior
- Only checked when that behavior executes
- Defined at the behavior level:

```cue
behaviors: [
	{
		name: "create-user"
		verifications: [
			{
				description: "User is created without password"
				criteria: [
					"id matches usr_[a-z0-9]+",
					"password field is absent",
				]
			}
		]
	}
]
```

## When to Use Invariants

Use invariants for rules that must **always** be true:

### 1. Security Rules
Security invariants protect sensitive data:
```cue
invariants: [
	{
		name: "passwords-never-exposed"
		description: "Passwords must never appear in any response"
		criteria: [
			"password field is absent from all responses",
			"password_hash field is absent from all responses",
			"plaintext password is never logged",
		]
	},
	{
		name: "authentication-required"
		description: "Protected endpoints require authentication"
		criteria: [
			"Endpoints under /api/protected require valid JWT",
			"Expired or invalid tokens are rejected with 401",
		]
	},
]
```

### 2. Data Integrity Rules
Ensure data consistency:
```cue
invariants: [
	{
		name: "unique-email-addresses"
		description: "Email addresses must be unique across all users"
		criteria: [
			"No two users have the same email address",
			"Email comparisons are case-insensitive",
			"Email uniqueness is enforced at database level",
		]
	},
	{
		name: "immutable-user-id"
		description: "User IDs never change once assigned"
		criteria: [
			"user id is generated once at creation",
			"user id cannot be updated",
			"user id references are stable over time",
		]
	},
]
```

### 3. Response Format Rules
Ensure consistent API structure:
```cue
invariants: [
	{
		name: "consistent-error-format"
		description: "All error responses follow the same structure"
		criteria: [
			"Error responses include error.code field",
			"Error responses include error.message field",
			"Error codes are from a defined set",
		]
	},
	{
		name: "timestamp-format"
		description: "All timestamps use ISO8601 format"
		criteria: [
			"created_at is valid ISO8601 datetime",
			"updated_at is valid ISO8601 datetime",
			"All timestamps include timezone (Z or offset)",
		]
	},
]
```

### 4. Business Logic Rules
Enforce business constraints:
```cue
invariants: [
	{
		name: "non-negative-balance"
		description: "Account balances must never be negative"
		criteria: [
			"Account balance is always >= 0",
			"Transactions that would make balance negative are rejected",
		]
	},
	{
		name: "age-restriction"
		description: "Users must meet minimum age requirements"
		criteria: [
			"User age is >= 18 for account creation",
			"Age-restricted features check user age",
		]
	},
]
```

### 5. Audit Requirements
Ensure compliance and traceability:
```cue
invariants: [
	{
		name: "audit-logging"
		description: "All state changes are logged"
		criteria: [
			"Every state change creates an audit log entry",
			"Audit entries include timestamp, user, and action",
			"Audit logs are immutable",
		]
	},
	{
		name: "request-id-tracking"
		description: "All requests include tracking ID"
		criteria: [
			"Every request generates unique request_id",
			"request_id is included in response",
			"request_id is included in error responses",
		]
	},
]
```

## Complete Examples

### Security Invariants

```cue
spec: {
	invariants: [
		{
			name: "passwords-never-exposed"
			description: "Passwords must never appear in any API response"
			criteria: [
				"password field is absent from all responses",
				"password_hash field is absent from all responses",
				"plaintext password is never logged",
				"password confirmation field is never stored",
			]
		},
		{
			name: "no-sql-injection"
			description: "All user input is sanitized to prevent SQL injection"
			criteria: [
				"Database queries use parameterized statements",
				"User input is never concatenated into SQL",
				"Input validation rejects potential SQL patterns",
			]
		},
		{
			name: "rate-limiting-enforced"
			description: "API rate limiting prevents abuse"
			criteria: [
				"Rate limit is enforced per IP address",
				"Rate limit exceeded returns 429 status",
				"Rate limit headers are included in response",
			]
		},
	]
}
```

### Data Integrity Invariants

```cue
spec: {
	invariants: [
		{
			name: "unique-email-addresses"
			description: "Email addresses must be unique across all users"
			criteria: [
				"No two active users have the same email",
				"Email comparisons are case-insensitive",
				"Email uniqueness is enforced at database level",
				"Duplicate email attempts return clear error",
			]
		},
		{
			name: "immutable-user-id"
			description: "User IDs never change once assigned"
			criteria: [
				"user id is generated once at creation",
				"user id cannot be updated via API",
				"user id references are stable over time",
			]
		},
		{
			name: "consistent-timestamps"
			description: "All timestamps use ISO8601 format in UTC"
			criteria: [
				"created_at is valid ISO8601 datetime",
				"updated_at is valid ISO8601 datetime",
				"All timestamps include timezone (Z or offset)",
				"All timestamps are in UTC timezone",
			]
		},
	]
}
```

### Response Format Invariants

```cue
spec: {
	invariants: [
		{
			name: "consistent-error-format"
			description: "All error responses follow the same structure"
			criteria: [
				"Error responses include error.code field",
				"Error responses include error.message field",
				"Error codes are from a defined enum",
				"Error messages are human-readable",
			]
		},
		{
			name: "resource-ids-in-responses"
			description: "Resource responses always include id field"
			criteria: [
				"Resource objects include id field",
				"id field is non-null string",
				"id field matches expected pattern",
			]
		},
		{
			name: "pagination-metadata"
			description: "List responses include pagination metadata"
			criteria: [
				"List responses include total count",
				"List responses include page information",
				"List responses include page size",
			]
		},
	]
}
```

## Invariant Categories

### Security Invariants
Protect against security vulnerabilities:
- Passwords never exposed
- Authentication required for protected endpoints
- Rate limiting enforced
- SQL injection prevented
- XSS attacks prevented

### Data Integrity Invariants
Ensure data consistency:
- Unique constraints enforced
- Immutable fields never change
- Referential integrity maintained
- No orphaned records

### Business Logic Invariants
Enforce business rules:
- Account balances never negative
- Age restrictions enforced
- Quota limits respected
- Workflow rules followed

### Format Invariants
Ensure consistent structure:
- All timestamps ISO8601
- All errors have consistent format
- All IDs follow pattern
- All enums use defined values

### Audit Invariants
Ensure compliance:
- All changes logged
- All requests tracked
- All actions attributable
- Logs are immutable

## Best Practices

### ✅ Do

1. **Make invariants truly universal**
   ```cue
   // Good - applies to everything
   {
     name: "passwords-never-exposed"
     criteria: ["password field is absent from all responses"]
   }

   // Bad - should be a behavior verification
   {
     name: "create-user-returns-id"
     criteria: ["create-user returns id"]  // Not universal!
   }
   ```

2. **Be specific about what must be true**
   ```cue
   criteria: [
     "password field is absent from all responses",
     "password_hash field is absent from all responses",
     "plaintext password is never logged",
   ]
   ```

3. **Explain why the invariant matters**
   ```cue
   description: "Passwords must never appear in any response for security compliance"
   ```

4. **Use clear, descriptive names**
   ```cue
   name: "passwords-never-exposed"  // Good
   name: "security-rule-1"          // Bad - unclear
   ```

### ❌ Don't

1. **Don't put behavior-specific checks in invariants**
   ```cue
   // Bad - this is behavior-specific
   {
     name: "create-user-validation"
     criteria: ["create-user validates email"]
   }

   // Good - put in behavior verification instead
   {
     name: "email-validation"
     criteria: ["All email inputs are validated for format"]
   }
   ```

2. **Don't make invariants too broad**
   ```cue
   // Bad - too vague
   {
     name: "security"
     criteria: ["system is secure"]
   }

   // Good - specific
   {
     name: "passwords-never-exposed"
     criteria: ["password field is absent from all responses"]
   }
   ```

3. **Don't duplicate behavior verifications**
   ```cue
   // If a behavior already verifies something specific,
   // don't make it an invariant unless it's truly universal
   ```

4. **Don't include implementation details**
   ```cue
   // Bad - implementation-specific
   criteria: ["bcrypt hash is used"]

   // Good - behavior-focused
   criteria: ["password is hashed with secure algorithm"]
   ```

## Invariants in the Specification

Invariants are defined at the spec level:

```cue
#Spec: {
	name!:        string
	description!: string
	audience!:    string
	version!:     string

	success_criteria!: [...string]

	features!:      [...#Feature]
	invariants!:   [...#Invariant]  // Global invariants here
	anti_patterns!: [...#AntiPattern]

	ai_hints!: #AIHints
}
```

## Integration with Behaviors

Behaviors can reference invariants:

```cue
{
	name: "create-user"
	intent: "Create a new user account"

	preconditions: [
		"Email is not already registered",  // References invariant
	]

	postconditions: [
		"User account is created",
		"Password is hashed (never exposed)",  // References invariant
	]

	verifications: [
		{
			description: "User is created without exposing password"
			criteria: [
				"id matches usr_[a-z0-9]+",
				"password field is absent",  // Satisfies invariant
			]
		}
	]
}
```

## Validation Workflow

When Intent validates a specification:

1. **Check all behaviors** against invariants
2. **Verify** behavior verifications don't violate invariants
3. **Report** any conflicts between behaviors and invariants
4. **Ensure** invariants are satisfied by all behaviors

Example conflict:
```cue
// Invariant says:
invariants: [
	{ name: "passwords-never-exposed",
	  criteria: ["password field is absent from all responses"] }
]

// But behavior tries to verify:
verifications: [
	{ description: "Response includes password hash",
	  criteria: ["password_hash field is present"] }  // CONFLICT!
]
```

Intent would report this as an error.

## See Also

- [`#Verification`](./schema-verification-type.md) - Behavior-specific verifications
- [`#Behavior`](./schema-behavior-type.md) - Behavior definitions
- [`#Spec`](./schema-spec-type.md) - Complete specification
