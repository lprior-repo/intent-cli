# Intent CLI - Contract-Driven API Testing

Intent is a powerful contract-driven API testing framework written in Gleam. It allows you to write comprehensive API specifications in CUE format and automatically execute them against your API, verifying that your implementation matches the specification.

## Features

- **Contract-Driven Testing**: Define API specifications in human-readable CUE format
- **Executable Specifications**: Automatically execute tests against your API
- **Type Safety**: Built entirely in Gleam for maximum type safety
- **Rich Validation**: Multiple validation rules, anti-pattern detection, and quality scoring
- **Variable Interpolation**: Support for capturing values from responses and interpolating them into subsequent requests
- **Dependency Resolution**: Automatic dependency order resolution between behaviors
- **Quality Analysis**: Automatic spec quality analysis and improvement suggestions
- **Linting**: Proactive detection of anti-patterns and quality issues
- **Flexible Output**: JSON and human-readable output formats

## Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/intent-cli
cd intent-cli

# Build the project
gleam build

# Run the CLI
gleam run -- check examples/user-api.cue --target http://localhost:8080
```

### Basic Example

Create a file `api.cue` with your API specification:

```cue
package api

spec: {
    name: "User API"
    description: "A simple user management API"
    audience: "Backend developers"
    version: "1.0.0"

    config: {
        base_url: "http://localhost:8080"
        timeout_ms: 5000
        headers: {
            "Content-Type": "application/json"
        }
    }

    features: [
        {
            name: "User Management"
            description: "Create, read, update, and delete users"
            behaviors: [
                {
                    name: "create-user"
                    intent: "Create a new user with valid data"
                    request: {
                        method: "POST"
                        path: "/users"
                        body: {
                            name: "John Doe"
                            email: "john@example.com"
                        }
                    }
                    response: {
                        status: 201
                        example: {
                            id: "user-123"
                            name: "John Doe"
                            email: "john@example.com"
                            created_at: "2024-01-04T12:00:00Z"
                        }
                        checks: {
                            "id": {
                                rule: "equals uuid"
                                why: "User IDs must be valid UUIDs"
                            }
                            "created_at": {
                                rule: "is iso8601 timestamp"
                                why: "Created at must be ISO8601 format"
                            }
                        }
                    }
                    captures: {
                        user_id: "id"
                    }
                }
                {
                    name: "get-user"
                    intent: "Retrieve a user by ID"
                    requires: ["create-user"]
                    request: {
                        method: "GET"
                        path: "/users/${user_id}"
                    }
                    response: {
                        status: 200
                        example: {
                            id: "user-123"
                            name: "John Doe"
                            email: "john@example.com"
                            created_at: "2024-01-04T12:00:00Z"
                        }
                        checks: {
                            "id": {
                                rule: "equals uuid"
                                why: "User IDs must be valid UUIDs"
                            }
                        }
                    }
                }
            ]
        }
    ]

    rules: [
        {
            name: "no-exposed-secrets"
            description: "Response should not contain exposed secrets"
            when: {
                status: ">= 200"
                method: "GET"
                path: "/users.*"
            }
            check: {
                body_must_not_contain: ["password", "secret", "token"]
            }
            example: {
                error: "Secrets exposed in response"
            }
        }
    ]

    anti_patterns: [
        {
            name: "missing-timestamps"
            description: "Responses should include timestamps"
            bad_example: {
                id: "user-123"
                name: "John Doe"
            }
            good_example: {
                id: "user-123"
                name: "John Doe"
                created_at: "2024-01-04T12:00:00Z"
                updated_at: "2024-01-04T12:00:00Z"
            }
            why: "Timestamps help with debugging and auditing"
        }
    ]

    ai_hints: {
        implementation: {
            suggested_stack: ["PostgreSQL", "Express.js", "Node.js"]
        }
        entities: {
            User: {
                fields: {
                    id: "UUID primary key"
                    name: "User full name (required)"
                    email: "User email address (required, unique)"
                    created_at: "ISO8601 timestamp"
                    updated_at: "ISO8601 timestamp"
                }
            }
        }
        security: {
            password_hashing: "Use bcrypt with salt rounds >= 10"
            jwt_algorithm: "HS256 or RS256"
            jwt_expiry: "15-30 minutes for access tokens"
            rate_limiting: "100 requests per minute per IP"
        }
        pitfalls: [
            "Don't store passwords in plain text"
            "Always validate input on the server"
            "Use HTTPS in production"
            "Implement proper error handling without exposing internals"
        ]
    }

    success_criteria: [
        "All behaviors pass"
        "No rule violations detected"
        "No anti-patterns in responses"
        "Response times under 500ms"
    ]
}
```

Then run Intent against your API:

```bash
gleam run -- check api.cue --target http://localhost:8080
```

## Documentation

- **[Installation Guide](docs/INSTALLATION.md)** - Detailed setup instructions
- **[User Guide](docs/USER_GUIDE.md)** - Complete usage documentation with examples
- **[CUE Specification Format](docs/SPEC_FORMAT.md)** - Detailed specification syntax and options
- **[API Documentation](docs/API.md)** - Architecture and module documentation
- **[Contributing](CONTRIBUTING.md)** - Guidelines for contributors

## Command Line Interface

### Check Command

Validate an API specification against a running server:

```bash
gleam run -- check <spec-file> --target <base-url> [options]
```

**Options:**
- `--target, -t <url>` - Base URL of the API to test (required)
- `--feature, -f <name>` - Filter to specific feature
- `--behavior, -b <name>` - Filter to specific behavior
- `--verbose, -v` - Enable verbose output
- `--output, -o json|text` - Output format (default: text)

**Examples:**

```bash
# Check against local development server
gleam run -- check api.cue --target http://localhost:8080

# Check with verbose output
gleam run -- check api.cue --target http://localhost:8080 --verbose

# Check only user management feature
gleam run -- check api.cue --target http://localhost:8080 --feature "User Management"

# Output results as JSON
gleam run -- check api.cue --target http://localhost:8080 --output json
```

## Understanding Results

Intent provides comprehensive test results with the following information:

### Pass/Fail Status
- **PASS**: All behaviors passed, no rule violations, no anti-patterns detected
- **FAIL**: One or more behaviors failed or rules were violated

### Behavior Results
For each behavior, Intent reports:
- **Status**: PASS, FAIL, or BLOCKED
- **Problems**: Detailed list of what failed
- **Request**: Method and URL that was sent
- **Response**: Status code received
- **Timing**: Milliseconds elapsed

### Rule Violations
Global rules that apply to multiple endpoints are reported with:
- **Rule Name**: The violated rule
- **Description**: What the rule checks
- **Violations**: Specific instances of the violation

### Anti-Patterns
Detected anti-patterns in responses with:
- **Pattern Name**: The detected pattern
- **Description**: Why it's problematic
- **Found**: Where it was found in the response
- **Examples**: Bad and good examples

### Quality Score
A comprehensive quality assessment including:
- **Coverage**: How many error cases are tested
- **Clarity**: How well documented the spec is
- **Testability**: How easy the spec is to test
- **AI Readiness**: How much guidance is provided for implementation

## Key Concepts

### Behaviors
A behavior represents a single test case - one request-response pair that verifies a specific aspect of the API.

```cue
{
    name: "get-user"
    intent: "Retrieve a user by ID"
    requires: ["create-user"]  // Depends on create-user behavior
    request: { ... }
    response: { ... }
    captures: { ... }
}
```

### Variables and Captures
You can capture values from responses and use them in subsequent requests:

```cue
// Behavior 1: Create user and capture the ID
captures: {
    user_id: "id"  // Capture response.id as ${user_id}
}

// Behavior 2: Use captured value
request: {
    path: "/users/${user_id}"  // Use the captured user_id
}
```

### Dependency Resolution
Intent automatically determines the correct execution order based on behavior dependencies:

```cue
{
    name: "update-user"
    requires: ["create-user"]  // Must run after create-user
}
```

### Rules
Global rules that apply across multiple endpoints:

```cue
{
    name: "no-secrets"
    description: "Responses should not expose secrets"
    when: {
        status: ">= 200"
        method: "GET"
        path: "/users.*"  // Regex support
    }
    check: {
        body_must_not_contain: ["password", "secret"]
        fields_must_exist: ["id", "name"]
    }
}
```

### Anti-Patterns
Detect common mistakes in API responses:

```cue
{
    name: "missing-timestamps"
    description: "Responses should include created_at and updated_at"
    bad_example: { id: "123" }
    good_example: {
        id: "123"
        created_at: "2024-01-04T12:00:00Z"
    }
    why: "Timestamps are essential for auditing"
}
```

## Performance

Intent is designed to be fast:
- Parallel rule checking
- Regex pattern caching using Erlang ETS
- Efficient JSON parsing with Erlang/OTP 27 native JSON support
- Minimal allocations and zero-copy where possible

## Architecture

Intent is built with:
- **Gleam** - A typed functional language for Erlang
- **Erlang/OTP 27** - Battle-tested distributed runtime
- **CUE** - Configuration language for specifications
- **Native JSON** - Erlang/OTP 27 native JSON parsing

See [API Documentation](docs/API.md) for detailed architecture information.

## Project Status

Intent is actively developed. The current version focuses on:
- Core API testing functionality
- Specification validation and linting
- Quality analysis and improvement suggestions
- Comprehensive documentation

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

Intent is licensed under the Apache License 2.0. See LICENSE for details.

## Support

For issues, questions, or suggestions:
- Open an [Issue](https://github.com/yourusername/intent-cli/issues)
- Check [Discussions](https://github.com/yourusername/intent-cli/discussions)
- See [Documentation](docs/)

---

**Built with ❤️ using Gleam**
