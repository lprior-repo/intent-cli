# Intent Specification Format Reference

Complete reference for the Intent CUE specification format (v3.0).

## Overview

Intent v3.0 uses **declarative specifications** focused on planning and verification rather than HTTP testing. Specifications define what a system should do through behaviors, preconditions, postconditions, and verifications.

## Top-Level Structure

```cue
package api

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
    // Basic metadata (all required)
    name: "User Management API"
    description: "API for user authentication and profile management"
    audience: "Mobile and web clients"
    version: "1.0.0"

    // Success criteria
    success_criteria: [
        "Users can register and login",
        "Passwords never exposed in responses",
    ]

    // Features (required, at least one)
    features: [...]

    // Invariants (required)
    invariants: [...]

    // Anti-patterns (required)
    anti_patterns: [...]

    // AI hints (required)
    ai_hints: {...}
}
```

### Required Fields

All fields in Intent specifications are **required**. No defaults are provided for backwards compatibility.

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Specification name |
| `description` | string | What this specification describes |
| `audience` | string | Target users of the system |
| `version` | string | Semantic version |
| `success_criteria` | [string] | List of acceptance criteria |
| `features` | [#Feature] | List of feature specifications |
| `invariants` | [#Invariant] | Global invariants |
| `anti_patterns` | [#AntiPattern] | Anti-patterns to avoid |
| `ai_hints` | #AIHints | Implementation guidance |

## Features

Features group related behaviors together.

```cue
features: [{
    name: "User Management"
    description: "User CRUD operations"

    // Required: at least one behavior
    behaviors: [...]
}]
```

### Feature Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Feature name (required) |
| `description` | string | Feature description (required) |
| `behaviors` | [#Behavior] | List of behaviors (required, cannot be empty) |

```cue
{
    name: "User Management"
    description: "User CRUD operations"
    behaviors: [Behavior]
}
```

## Behaviors

Behaviors describe what a system does, not how to test it. They are **declarative** specifications.

```cue
{
    name: "successful-user-registration"
    intent: "A new user can create an account with valid email and password"

    // Additional context (optional)
    notes: "User email must be unique"

    // Dependencies (optional)
    requires: ["setup-database"]

    // Tags for filtering (optional)
    tags: ["happy-path", "authentication"]

    // Preconditions (optional, defaults to empty)
    preconditions: [
        "User provides valid email address",
        "User provides strong password",
        "Email address not already registered",
    ]

    // Postconditions (optional, defaults to empty)
    postconditions: [
        "User account exists in database",
        "User can authenticate with credentials",
        "Password is hashed before storage",
    ]

    // Verifications (optional, defaults to empty)
    verifications: [{
        description: "User can log in"
        criteria: [
            "Authentication succeeds with valid credentials",
            "Authentication fails with invalid credentials",
        ]
        examples: [
            {
                input: { email: "user@example.com", password: "valid-pass" }
                expected: { success: true, token: "<jwt_token>" }
            }
        ]
    }]
}
```

### Behavior Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | #Identifier | Behavior name (required, must match `[a-z][a-z0-9_-]*`) |
| `intent` | string | What this behavior demonstrates (required) |
| `notes` | string | Additional context (optional, defaults to empty string) |
| `requires` | [#Identifier] | Behavior dependencies (optional, defaults to empty list) |
| `tags` | [string] | Classification tags (optional, defaults to empty list) |
| `preconditions` | [string] | What must be true first (optional, defaults to empty list) |
| `postconditions` | [string] | What must be true after (optional, defaults to empty list) |
| `verifications` | [#Verification] | How to verify it works (optional, defaults to empty list) |

## Verifications

Verifications describe how to confirm that a behavior works correctly.

```cue
verifications: [{
    description: "User can authenticate with credentials"
    criteria: [
        "Valid credentials return 200 with JWT token",
        "Invalid credentials return 401",
        "Account locks after 5 failed attempts",
    ]
    examples: [
        {
            input: { email: "user@example.com", password: "valid-pass" }
            expected: { success: true, token: "<jwt_token>" }
        }
        {
            input: { email: "user@example.com", password: "wrong-pass" }
            expected: { success: false, error: "Invalid credentials" }
        }
    ]
}]
```

### Verification Fields

| Field | Type | Description |
|-------|------|-------------|
| `description` | string | What is being verified (required) |
| `criteria` | [string] | Verification criteria (required) |
| `examples` | [...] | JSON examples demonstrating criteria (optional) |

## Invariants

Invariants are global rules that apply to all behaviors. They describe what must always be true.

```cue
invariants: [{
    name: "no-password-exposure"
    description: "Passwords never appear in any API response"
    criteria: [
        "Password field absent from all user responses",
        "Password hash never returned",
        "Password never logged or exposed in errors",
    ]
}]
```

### Invariant Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Invariant name (required) |
| `description` | string | What this invariant ensures (required) |
| `criteria` | [string] | What must always be true (required) |

## Anti-Patterns

Anti-patterns document common mistakes to avoid, with good and bad examples.

```cue
anti_patterns: [{
    name: "missing-timestamps"
    description: "All responses should include created_at and updated_at"
    bad_example: {
        id: "123"
        name: "Product"
    }
    good_example: {
        id: "123"
        name: "Product"
        created_at: "2024-01-04T12:00:00Z"
        updated_at: "2024-01-04T12:00:00Z"
    }
    why: "Timestamps are essential for auditing and debugging"
}]
```

### Anti-Pattern Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Pattern name (required) |
| `description` | string | Pattern description (required) |
| `bad_example` | {...} | Example showing the anti-pattern (required) |
| `good_example` | {...} | Example showing correct implementation (required) |
| `why` | string | Explanation of why it matters (optional) |

## AI Hints

AI hints provide implementation guidance for AI systems or developers.

```cue
ai_hints: {
    implementation: {
        suggested_stack: ["PostgreSQL", "Express.js", "Node.js"]
    }
    entities: {
        User: {
            fields: {
                id: "UUID primary key"
                name: "User full name (required)"
                email: "User email (required, unique)"
                created_at: "ISO8601 timestamp"
                updated_at: "ISO8601 timestamp"
            }
        }
        Item: {
            fields: {
                id: "UUID primary key"
                user_id: "Foreign key to User"
                title: "Item title (required)"
                created_at: "ISO8601 timestamp"
            }
        }
    }
    security: {
        password_hashing: "Use bcrypt with >= 10 rounds"
        jwt_algorithm: "HS256 or RS256"
        jwt_expiry: "15-30 minutes for access tokens"
        rate_limiting: "100 requests per minute per IP"
    }
    pitfalls: [
        "Never return passwords in responses",
        "Always validate input on server",
        "Use HTTPS in production",
        "Implement proper error handling",
        "Don't expose internal errors to clients"
    ]
}
```

### AI Hint Fields

| Field | Type | Description |
|-------|------|-------------|
| `implementation` | {...} | Stack recommendations (optional) |
| `entities` | {string: #EntityHint} | Entity/model definitions (optional) |
| `security` | {...} | Security best practices (optional) |
| `pitfalls` | [string] | Common mistakes to avoid (optional) |

## Complete Example

```cue
package api

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
    name: "User Management API"
    description: "API for user authentication and profile management"
    audience: "Mobile and web clients"
    version: "1.0.0"

    success_criteria: [
        "Users can register and login",
        "Passwords never exposed in responses",
        "All errors return structured error objects",
    ]

    features: [
        {
            name: "User Registration"
            description: "New user registration flow"
            behaviors: [
                {
                    name: "successful-registration"
                    intent: "A new user can create an account"
                    preconditions: [
                        "User provides valid email",
                        "User provides strong password",
                    ]
                    postconditions: [
                        "User account exists in system",
                        "Password is hashed",
                        "User can authenticate",
                    ]
                    verifications: [{
                        description: "User can log in"
                        criteria: [
                            "Authentication succeeds with valid credentials",
                            "Authentication fails with invalid credentials",
                        ]
                    }]
                }
            ]
        }
    ]

    invariants: [
        {
            name: "no-password-exposure"
            description: "Passwords never appear in responses"
            criteria: [
                "Password field absent from all user responses",
                "Password hash never returned",
            ]
        }
    ]

    anti_patterns: [
        {
            name: "missing-timestamps"
            description: "All responses should include timestamps"
            bad_example: { id: "123", name: "User" }
            good_example: {
                id: "123"
                name: "User"
                created_at: "2024-01-04T12:00:00Z"
                updated_at: "2024-01-04T12:00:00Z"
            }
            why: "Timestamps are essential for auditing"
        }
    ]

    ai_hints: {
        implementation: {
            suggested_stack: ["PostgreSQL", "Node.js", "Express"]
        }
        entities: {
            User: {
                fields: {
                    id: "UUID primary key"
                    email: "Unique email address"
                    password_hash: "Bcrypt hash"
                    created_at: "ISO8601 timestamp"
                }
            }
        }
        security: {
            password_hashing: "bcrypt with cost factor 12"
            jwt_algorithm: "HS256"
            jwt_expiry: "1 hour"
        }
        pitfalls: [
            "Never return passwords in responses",
            "Always validate input on server",
            "Use HTTPS in production",
        ]
    }
}
```

## Tips for Writing Specifications

1. **All fields are required** - Even optional-looking fields must be explicitly provided
2. **Use descriptive behavior names** - Names should clearly indicate what the behavior does
3. **Focus on intent** - Describe what the system does, not how to test it
4. **Document preconditions and postconditions** - What must be true before and after
5. **Provide clear verifications** - How to confirm the behavior works
6. **Use global invariants** - Document system-wide rules
7. **Include anti-patterns** - Help implementers avoid common mistakes
8. **Provide AI hints** - Guide implementation with suggestions

## Key Differences from v2.0

### Removed Fields
- `config` - No HTTP execution configuration
- `request` - No HTTP request definitions
- `response` - No HTTP response definitions
- `captures` - No variable capture
- `checks` - No field-level validation rules
- `when` - No conditional rule application

### New Fields
- `preconditions` - What must be true before
- `postconditions` - What must be true after
- `verifications` - How to verify correctness
- `invariants` - Global rules (renamed from `rules`)

### Conceptual Changes
- **v2.0**: "Define HTTP requests and validate responses"
- **v3.0**: "Describe behaviors declaratively and verify correctness"

## See Also

- [User Guide](USER_GUIDE.md) - Comprehensive usage guide
- [Schema Documentation](schema-spec-type.md) - Schema type reference
- [Migration Guide](../MIGRATION.md) - Migrating from v2.0
