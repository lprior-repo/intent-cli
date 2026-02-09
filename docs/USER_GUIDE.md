# Intent CLI User Guide

This comprehensive guide covers everything you need to know to use Intent effectively for requirement planning and specification generation.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Interactive Interviews](#interactive-interviews)
3. [Writing Specifications](#writing-specifications)
4. [Plan Generation](#plan-generation)
5. [Analysis Tools](#analysis-tools)
6. [Best Practices](#best-practices)
7. [Common Patterns](#common-patterns)
8. [Troubleshooting](#troubleshooting)

## Getting Started

### Basic Workflow

The typical Intent workflow is:

1. **Run an interview** - Use interactive interviews to capture requirements
2. **Generate specification** - Intent creates a structured CUE specification
3. **Generate plan** - Create a structured plan from the specification
4. **Emit beads** - Convert plan items into beads for br (beads_rust)
5. **Analyze quality** - Review specification for quality and security issues
6. **Iterate** - Refine specification and plan as needed

### Your First Interview

Start an interactive interview to capture requirements:

```bash
# Start an interview with the API profile
gleam run -- interview --profile api

# Resume a previous session
gleam run -- interview --resume <session-id>

# List all interview sessions
gleam run -- history
```

During the interview, you'll be prompted to describe:
- System overview and goals
- User roles and actors
- Features and behaviors
- Success criteria
- Constraints and anti-patterns

### Viewing Your Specification

After completing an interview, Intent generates a CUE specification:

```bash
# View the specification
gleam run -- show --session <session-id>

# Export to a file
gleam run -- show --session <session-id> --out spec.cue
```

## Interactive Interviews

### Starting an Interview

Intent uses interactive interviews to capture requirements:

```bash
# Start with a specific profile
gleam run -- interview --profile api

# Start with custom session notes
gleam run -- interview --profile api --notes "Building a REST API for e-commerce"

# Resume a previous session
gleam run -- interview --resume <session-id>
```

### Interview Profiles

Available profiles:
- `api` - REST API design and behavior specification
- `system` - System architecture and component design
- `cli` - Command-line interface design

### Answering Questions

During the interview, you'll see prompts like:

```
[1/15] What is the primary purpose of this system?

Enter your response (or 'skip' to defer):
```

Tips for good answers:
- Be specific and concise
- Use plain English, not technical jargon
- Provide examples when helpful
- Use "THE SYSTEM SHALL" format for requirements
- Can use "skip" to defer and come back later

### Session Management

```bash
# List all sessions
gleam run -- history

# Show diff between sessions
gleam run -- diff --session <session-id>

# List sessions by profile
gleam run -- sessions --profile api
```

## Writing Specifications

### Specification Structure

Every Intent specification has this structure:

```cue
spec: {
    name: String                    // Name of the system
    description: String             // What this system does
    audience: String                // Who uses it
    version: String                 // Version (semantic versioning)
    success_criteria: [String]      // What success looks like
    features: [Feature]             // Groups of related behaviors
    invariants: [Invariant]         // Global invariants
    anti_patterns: [AntiPattern]   // Common mistakes to detect
    ai_hints: AIHints              // Hints for AI implementation
}
```

### Features

Features group related behaviors:

```cue
features: [{
    name: "User Management"
    description: "User CRUD operations"
    behaviors: [
        // Behaviors for user management
    ]
}]
```

### Behaviors

A behavior describes a system capability:

```cue
{
    name: "create-user"                              // Unique identifier
    intent: "Create a new user with valid email"     // What this behavior demonstrates
    preconditions: [                                 // What must be true first
        "User provides valid email",
        "User provides valid password"
    ]
    postconditions: [                                // What must be true after
        "User account exists in system",
        "User can authenticate with credentials"
    ]
    verifications: [{                                // How to verify it works
        description: "User can log in"
        criteria: [
            "Authentication succeeds with valid credentials",
            "Authentication fails with invalid credentials"
        ]
    }]
    notes: "User email must be unique"               // Additional context
    requires: ["setup-database"]                     // Dependencies
    tags: ["happy-path", "create"]                   // Classification tags
}
```

### Invariants

Global invariants apply to all behaviors:

```cue
invariants: [{
    name: "data-consistency"
    description: "All data operations maintain consistency"
    criteria: [
        "Database transactions are atomic",
        "No partial updates on failure",
        "Rollback on error"
    ]
}]
```

### Anti-Patterns

Document common mistakes to avoid:

```cue
anti_patterns: [{
    name: "missing-timestamps"
    description: "Responses should include created_at and updated_at"
    bad_example: {
        id: "123"
        name: "Product"
        // Missing timestamps!
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

## Plan Generation

### Creating a Plan

Generate a structured plan from your specification:

```bash
# Generate a plan from current context
gleam run -- plan

# Generate with additional notes
gleam run -- plan --notes "Focus on authentication first"

# Generate from a specific session
gleam run -- plan --session <session-id>
```

### Managing Plans

```bash
# Get next task recommendation
gleam run -- plan-next

# Approve a plan
gleam run -- plan-approve <plan-id>

# Check plan status
gleam run -- plan-status

# Emit beads to br (beads_rust)
gleam run -- plan-emit-beads <session-id>

# Emit and execute immediately
gleam run -- plan-emit-beads <session-id> --execute
```

### Plan Workflows

```bash
# Start a new planning session
gleam run -- plan-work --profile cli

# Add vision statement
gleam run -- plan-work --vision "Build a developer-focused planning tool"

# Export beads for tracking
gleam run -- plan-emit-beads <session-id> --target br --json
```

## Analysis Tools

### Effects Analysis

Analyze second-order effects and system impacts:

```bash
# Analyze entire specification
gleam run -- effects examples/spec.cue

# Analyze specific behavior
gleam run -- effects examples/spec.cue --behavior <name>

# Output JSON for processing
gleam run -- effects examples/spec.cue --json
```

### Quality Analysis

Check specification quality and completeness:

```bash
# Analyze quality
gleam run -- quality examples/spec.cue

# Get detailed report
gleam run -- quality examples/spec.cue --verbose
```

### Semantic Validation

Validate semantics and detect issues:

```bash
# Validate semantics
gleam run -- validate examples/spec.cue

# Show specific validation checks
gleam run -- validate examples/spec.cue --checks
```

### Coverage Analysis

Check specification coverage:

```bash
# Analyze coverage
gleam run -- coverage examples/spec.cue

# Identify gaps
gleam run -- gaps examples/spec.cue
```

### Linting

Lint specifications for style and consistency:

```bash
# Lint a specification
gleam run -- lint examples/spec.cue

# Auto-fix issues
gleam run -- lint examples/spec.cue --fix
```

## Advanced Features

### Bead Generation

Convert specifications into beads for br (beads_rust):

```bash
# Generate beads from a session
gleam run -- beads --session <session-id>

# Generate in JSON format
gleam run -- beads --session <session-id> --format json

# Regenerate beads
gleam run -- beads-regenerate --session <session-id>

# Check bead status
gleam run -- bead-status --bead-id <id>
```

### Documentation Generation

Generate documentation from specifications:

```bash
# Generate vision document
gleam run -- vision --out ./docs/vision.md

# Generate ready document
gleam run -- ready --out ./docs/ready.md

# Export specification
gleam run -- export --session <session-id> --out spec.cue
```

### Answer Templates

Export and reuse interview answers:

```bash
# Export answer template
gleam run -- interview --profile api --export-answers-template template.json

# Import answers from file
gleam run -- interview --profile api --import-answers template.json
```

### Session Diffing

Compare changes between sessions:

```bash
# Show diff between sessions
gleam run -- diff --session <session-id>

# Show diff with specific base
gleam run -- diff --session <session-id> --base <base-session-id>
```

## Best Practices

### 1. Organize by Feature

Group related behaviors in features:

```cue
features: [
    {
        name: "User Management"
        behaviors: [
            // Create, read, update, delete behaviors
        ]
    }
    {
        name: "Authentication"
        behaviors: [
            // Login, logout, session management
        ]
    }
]
```

### 2. Clear Intent Statements

Write intent statements that describe the business value:

```cue
// Good
intent: "Enable users to authenticate securely with email and password"

// Bad
intent: "Login endpoint"
```

### 3. Comprehensive Preconditions and Postconditions

Document what must be true before and after:

```cue
{
    name: "create-user"
    intent: "Create a new user account"
    preconditions: [
        "User provides valid email address",
        "User provides strong password",
        "Email address not already registered"
    ]
    postconditions: [
        "User account exists in database",
        "User receives confirmation email",
        "User can authenticate with credentials"
    ]
}
```

### 4. Use Dependencies Wisely

Model behavior dependencies accurately:

```cue
{
    name: "update-user"
    requires: ["create-user"]      // Single dependency
    // ...
}

{
    name: "delete-user"
    requires: ["create-user", "update-user"]  // Multiple dependencies
    // ...
}
```

### 5. Document Verifications

Provide clear verification criteria:

```cue
verifications: [{
    description: "User can authenticate"
    criteria: [
        "Authentication succeeds with valid credentials",
        "Authentication fails with invalid credentials",
        "Account locks after 5 failed attempts"
    ]
    examples: [
        {
            input: { email: "user@example.com", password: "valid-pass" }
            expected: { success: true, token: "<jwt_token>" }
        }
    ]
}]
```

### 6. Use Global Invariants

Document system-wide invariants:

```cue
invariants: [
    {
        name: "data-consistency"
        description: "All data operations maintain ACID properties"
        criteria: [
            "Database transactions are atomic",
            "No partial updates on failure",
            "Rollback on error"
        ]
    }
    {
        name: "security"
        description: "Security is never compromised"
        criteria: [
            "Passwords are hashed before storage",
            "Sensitive data is encrypted at rest",
            "Authentication is required for protected resources"
        ]
    }
]
```

## Common Patterns

### Pattern: CRUD Operations

```cue
features: [{
    name: "User CRUD"
    behaviors: [
        {
            name: "create-user"
            intent: "Create a new user account"
            preconditions: [
                "User provides valid email",
                "User provides strong password"
            ]
            postconditions: [
                "User account exists",
                "User can authenticate"
            ]
        }
        {
            name: "read-user"
            intent: "Retrieve user details"
            requires: ["create-user"]
            preconditions: ["User account exists"]
            postconditions: ["User details are displayed"]
        }
        {
            name: "update-user"
            intent: "Update user information"
            requires: ["create-user"]
            preconditions: ["User account exists", "User provides updated data"]
            postconditions: ["User information is updated"]
        }
        {
            name: "delete-user"
            intent: "Delete user account"
            requires: ["create-user"]
            preconditions: ["User account exists", "User confirms deletion"]
            postconditions: ["User account is removed", "User cannot authenticate"]
        }
    ]
}]
```

### Pattern: Authentication Flow

```cue
features: [{
    name: "Authentication"
    behaviors: [
        {
            name: "register-user"
            intent: "Register a new user account"
            preconditions: [
                "User provides unique email",
                "User provides strong password"
            ]
            postconditions: [
                "Account is created",
                "Confirmation email is sent"
            ]
        }
        {
            name: "login-user"
            intent: "Authenticate user with credentials"
            requires: ["register-user"]
            preconditions: [
                "User account exists",
                "User provides valid credentials"
            ]
            postconditions: [
                "User is authenticated",
                "Session token is issued"
            ]
        }
        {
            name: "logout-user"
            intent: "End user session"
            requires: ["login-user"]
            preconditions: ["User is authenticated"]
            postconditions: [
                "Session is terminated",
                "Token is invalidated"
            ]
        }
    ]
}]
```

### Pattern: Data Validation

```cue
features: [{
    name: "Input Validation"
    behaviors: [
        {
            name: "validate-email-format"
            intent: "Ensure email addresses are valid"
            preconditions: ["User provides email address"]
            postconditions: [
                "Valid email is accepted",
                "Invalid email is rejected with error"
            ]
            verifications: [{
                description: "Email format validation"
                criteria: [
                    "Valid format: user@domain.com",
                    "Invalid format: rejected with 400",
                    "Missing @ symbol: rejected",
                    "Missing domain: rejected"
                ]
            }]
        }
        {
            name: "validate-password-strength"
            intent: "Ensure passwords meet security requirements"
            preconditions: ["User provides password"]
            postconditions: [
                "Strong password is accepted",
                "Weak password is rejected with requirements"
            ]
            verifications: [{
                description: "Password strength validation"
                criteria: [
                    "Minimum 8 characters",
                    "Contains uppercase letter",
                    "Contains lowercase letter",
                    "Contains number",
                    "Contains special character"
                ]
            }]
        }
    ]
}]
```

## Troubleshooting

### Issue: Interview Session Not Found

If you try to resume a session that doesn't exist:

```
Error: Session 'interview-123' not found
```

**Solution:**
1. List all sessions: `gleam run -- history`
2. Check the session ID is correct
3. Verify the session file exists in the interview storage directory

### Issue: Plan Generation Fails

If plan generation fails:

```
Error: No specification found for session
```

**Solution:**
1. Complete an interview first
2. Check that the session has a generated specification
3. Use `gleam run -- show --session <id>` to verify

### Issue: Bead Emission Fails

If bead emission to br fails:

```
Error: Failed to emit beads to br
```

**Solution:**
1. Ensure br (beads_rust) is installed
2. Check that br is properly configured
3. Verify the plan is approved: `gleam run -- plan-status`
4. Try emitting without --execute first to preview

### Issue: Quality Analysis Shows Warnings

If quality analysis reports issues:

```
Warning: 3 behaviors missing verifications
Warning: 5 behaviors missing postconditions
```

**Solution:**
1. Review each warning in the output
2. Add missing verifications to behaviors
3. Document preconditions and postconditions
4. Use `gleam run -- improve` for suggestions

### Issue: Effects Analysis Not Working

If effects analysis doesn't show results:

```
Error: No behaviors found for analysis
```

**Solution:**
1. Check that specification has behaviors defined
2. Verify behavior names are valid (use lowercase, hyphens, underscores)
3. Check for syntax errors in the specification

### Issue: Lint Shows Style Issues

If lint reports style problems:

```
Warning: Behavior name 'CreateUser' should use lowercase
```

**Solution:**
1. Follow naming conventions (lowercase, hyphens, underscores)
2. Use `gleam run -- lint --fix` to auto-fix
3. Review the lint output for specific issues

See [SPEC_FORMAT.md](SPEC_FORMAT.md) for more details on specification syntax.
