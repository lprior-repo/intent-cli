# Intent Schema: #Verification Type

## Overview

The `#Verification` type defines how to verify that a behavior works correctly. Unlike traditional HTTP testing, verifications focus on **what should be true** after a behavior completes, rather than specific HTTP responses.

## Type Definition

```cue
#Verification: {
	description!: string
	criteria!:    [...string]
	examples?:    [...] // Generic JSON examples
}
```

## Required Fields

### `description: string`
A human-readable explanation of what this verification checks.
- Should clearly state the verification purpose
- Helps humans and AI understand what's being validated
- Examples:
  - `"User response contains valid data without password"`
  - `"Duplicate email returns structured error"`
  - `"Password is hashed before storage"`

### `criteria: [...string]`
A list of conditions that must be true for this verification to pass.
- Each criterion is a string describing a condition
- Criteria are declarative - they describe "what" not "how"
- Can reference fields, values, patterns, or conditions
- Examples:
  - `"id matches usr_[a-z0-9]+"`
  - `"email equals the provided email"`
  - `"password field is absent"`
  - `"created_at is valid ISO8601 datetime"`

## Optional Fields

### `examples: [...]` (optional)
Concrete examples showing the expected data structure.
- Can be valid JSON objects, arrays, or primitives
- Helps illustrate the criteria
- Useful for documentation and AI understanding
- Multiple examples can show different valid states

## When to Use Verifications

Verifications are used to define **postconditions** - what must be true after a behavior completes:

### 1. Data Structure Validation
Verify the shape and content of data:
```cue
verifications: [
	{
		description: "User response contains valid data without password"
		criteria: [
			"id matches usr_[a-z0-9]+",
			"email equals the provided email",
			"password field is absent",
			"created_at is valid ISO8601 datetime",
		]
		examples: [
			{
				id:         "usr_abc123xyz"
				email:      "newuser@example.com"
				name:       "New User"
				created_at: "2024-01-15T10:30:00Z"
			},
		]
	},
]
```

### 2. Error Condition Verification
Verify error responses are correct:
```cue
verifications: [
	{
		description: "Duplicate email returns structured error"
		criteria: [
			"error.code equals EMAIL_EXISTS",
			"error.message is non-empty string",
		]
		examples: [
			{
				error: {
					code:    "EMAIL_EXISTS"
					message: "An account with this email already exists"
				}
			},
		]
	},
]
```

### 3. State Change Verification
Verify that system state changed correctly:
```cue
verifications: [
	{
		description: "User account is created and persisted"
		criteria: [
			"User record exists in database",
			"User ID is assigned",
			"Password hash is stored",
		]
	},
]
```

### 4. Business Rule Verification
Verify business logic is applied correctly:
```cue
verifications: [
	{
		description: "Referral bonus is applied correctly"
		criteria: [
			"Account balance includes bonus amount",
			"Bonus equals 10% of initial deposit",
			"Referrer account is credited",
		]
	},
]
```

## Complete Examples

### Behavior with Single Verification

```cue
{
	name:   "successful-registration"
	intent: "A new user can create an account"

	preconditions: [
		"Email is not already registered",
		"Password meets security requirements",
	]

	postconditions: [
		"User account is created in the system",
		"Password is hashed and stored securely",
	]

	verifications: [
		{
			description: "User response contains valid data without password"
			criteria: [
				"id matches usr_[a-z0-9]+",
				"email equals the provided email",
				"password field is absent",
				"created_at is valid ISO8601 datetime",
			]
			examples: [
				{
					id:         "usr_abc123xyz"
					email:      "newuser@example.com"
					name:       "New User"
					created_at: "2024-01-15T10:30:00Z"
				},
			]
		},
	]
}
```

### Behavior with Multiple Verifications

```cue
{
	name:   "process-payment"
	intent: "Process a payment and update account balance"

	preconditions: [
		"User account exists",
		"Payment method is valid",
		"Sufficient funds available",
	]

	postconditions: [
		"Payment is processed",
		"Account balance is updated",
		"Transaction record is created",
	]

	verifications: [
		{
			description: "Payment transaction is recorded"
			criteria: [
				"transaction.id is non-empty string",
				"transaction.amount equals payment amount",
				"transaction.status equals 'completed'",
				"transaction.timestamp is valid ISO8601 datetime",
			]
		},
		{
			description: "Account balance is correctly updated"
			criteria: [
				"balance.previous_amount plus payment.amount equals balance.new_amount",
				"balance.currency matches payment currency",
			]
		},
		{
			description: "Payment method is charged correctly"
			criteria: [
				"payment_method.charge_id exists",
				"payment_method.amount matches transaction",
			]
		},
	]
}
```

### Error Case Behavior

```cue
{
	name:   "insufficient-funds-rejected"
	intent: "Payment is rejected when funds are insufficient"

	preconditions: [
		"User account exists",
		"Payment amount exceeds available balance",
	]

	postconditions: [
		"Payment is not processed",
		"Account balance is unchanged",
	]

	verifications: [
		{
			description: "Insufficient funds returns structured error"
			criteria: [
				"error.code equals INSUFFICIENT_FUNDS",
				"error.message explains the shortfall",
				"error.data.available_amount is provided",
			]
			examples: [
				{
					error: {
						code:    "INSUFFICIENT_FUNDS"
						message: "Insufficient funds. Available: $100.00, Required: $150.00"
						data: {
							available_amount: "100.00"
							required_amount:  "150.00"
							currency:        "USD"
						}
					}
				},
			]
		},
	]
}
```

## Verification vs. HTTP Testing

### Traditional HTTP Testing (Old)
```cue
// Old way - focused on HTTP details
request: {
	method: "POST"
	path: "/users"
	body: { email: "user@example.com" }
}
response: {
	status: 201
	example: { id: "usr_123", email: "user@example.com" }
	checks: {
		"id": { rule: "is uuid", why: "..." }
	}
}
```

### Verifications (New)
```cue
// New way - focused on behavior and outcomes
verifications: [
	{
		description: "User account is created successfully"
		criteria: [
			"id matches usr_[a-z0-9]+",
			"email equals the provided email",
			"account exists in database",
		]
	}
]
```

**Key Differences:**
- **Declarative**: Describes what should be true, not how to test it
- **Implementation-agnostic**: Works for APIs, CLIs, libraries, etc.
- **Human-readable**: Clear what's being verified
- **AI-friendly**: Easy to understand and implement

## Best Practices

### ✅ Do

1. **Be specific and clear**
   ```cue
   criteria: [
     "id matches usr_[a-z0-9]+",  // Good - specific pattern
     "id is valid string",          // Bad - too vague
   ]
   ```

2. **Include examples for complex structures**
   ```cue
   examples: [
     {
       error: {
         code: "EMAIL_EXISTS",
         message: "An account with this email already exists"
       }
     }
   ]
   ```

3. **Verify important business rules**
   ```cue
   criteria: [
     "account balance is updated atomically",
     "transaction is recorded in audit log",
   ]
   ```

4. **Group related criteria**
   ```cue
   // One verification per concern
   { description: "User data is valid", ... }
   { description: "Password is secure", ... }
   { description: "Account is created", ... }
   ```

### ❌ Don't

1. **Don't include implementation details**
   ```cue
   criteria: [
     "POST /users endpoint returns 201",     // Bad - implementation
     "user record is inserted into database", // Good - behavior
   ]
   ```

2. **Don't make criteria overly complex**
   ```cue
   criteria: [
     "id matches pattern AND email is valid AND password absent AND created_at valid",  // Bad
     "id matches usr_[a-z0-9]+",    // Good - separate criteria
     "email is valid format",
     "password field is absent",
   ]
   ```

3. **Don't verify trivial things**
   ```cue
   criteria: [
     "response exists",  // Too trivial
     "user has email",   // Better
   ]
   ```

4. **Don't mix concerns in one verification**
   ```cue
   // Bad - verifies user data AND payment
   { description: "User and payment are valid", ... }

   // Good - separate verifications
   { description: "User data is valid", ... }
   { description: "Payment is processed", ... }
   ```

## Verification Criteria Patterns

### Field Validation
```cue
criteria: [
  "id matches usr_[a-z0-9]+",
  "email equals provided email",
  "age is greater than or equal to 18",
]
```

### Presence/Absence
```cue
criteria: [
  "password field is absent",
  "error object is present",
  "metadata exists",
]
```

### Format Validation
```cue
criteria: [
  "created_at is valid ISO8601 datetime",
  "email matches RFC 5322 format",
  "phone number matches E.164 format",
]
```

### Business Rules
```cue
criteria: [
  "account balance is non-negative",
  "user age meets minimum requirement",
  "discount is applied correctly",
]
```

### State Changes
```cue
criteria: [
  "user status changes from pending to active",
  "inventory count is decremented",
  "audit log entry is created",
]
```

## Integration with Behaviors

Verifications are part of behaviors:
```cue
#Behavior: {
	name:   #Identifier
	intent: string

	preconditions?:  [...string]      // What must be true before
	postconditions?: [...string]      // What must be true after
	verifications?:  [...#Verification] // How to verify it worked

	notes:    string | *""
	requires?: [...#Identifier]
	tags?:    [...string]
}
```

**Flow:**
1. **Preconditions** are satisfied
2. **Behavior** executes
3. **Postconditions** become true
4. **Verifications** confirm the postconditions

## See Also

- [`#Behavior`](./schema-behavior-type.md) - Behavior definitions
- [`#Invariant`](./schema-invariant-type.md) - Global invariants
- [`#Spec`](./schema-spec-type.md) - Complete specification
