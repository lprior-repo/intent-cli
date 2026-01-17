# AI-Friendly Error Messages

This document demonstrates the new AI-friendly error system in Intent CLI. All errors now include:

- **Structured format**: Machine-readable CUE/JSON output
- **Actionable suggestions**: Clear guidance on what to do next
- **Recovery steps**: Step-by-step instructions for fixing the error
- **Context**: Relevant information to diagnose the issue

## Error Format

All AI-friendly errors follow this structure:

```cue
{
    action: "error_category"
    error: {
        type: "specific_error_type"
        message: "what went wrong"
        context: {
            field1: "value1"
            field2: "value2"
        }
    }
    suggestion: "what to do next"
    recovery: [
        "step 1",
        "step 2",
        "step 3"
    ]
}
```

## Example Errors

### 1. File Not Found

**Before:**
```
Error: File not found: examples/missing-api.cue
```

**After (CUE format):**
```cue
{
    action: "file_error"
    error: {
        type: "file_not_found"
        message: "File not found: examples/missing-api.cue"
        context: {
            path: "examples/missing-api.cue"
            expected_location: "CUE specification file"
        }
    }
    suggestion: "Create the missing file or directory"
    recovery: [
        "Check if the parent directory exists",
        "Create directory: mkdir -p examples",
        "Create the file with appropriate content",
        "Verify file permissions allow read/write access"
    ]
}
```

**After (Human-readable):**
```
Error: File not found: examples/missing-api.cue

Context:
  path: examples/missing-api.cue
  expected_location: CUE specification file

Suggestion: Create the missing file or directory

Recovery Steps:
  1. Check if the parent directory exists
  2. Create directory: mkdir -p examples
  3. Create the file with appropriate content
  4. Verify file permissions allow read/write access
```

### 2. CUE Validation Error

**Before:**
```
Error: CUE validation failed:
spec.name: cannot use value "user-api" (type string) as string:
    error in call to str.MaxRunes: invalid value "user-api" (out of bound =~"^[a-z][a-z0-9-]*$")
```

**After:**
```cue
{
    action: "cue_error"
    error: {
        type: "validation_error"
        message: "CUE validation failed: spec.name: cannot use value..."
        context: {
            file: "examples/user-api.cue"
            validation_output: "spec.name: cannot use value..."
        }
    }
    suggestion: "Fix CUE syntax errors in the specification file"
    recovery: [
        "Check if CUE is installed: cue version",
        "If not installed: Visit https://cuelang.org/docs/install/",
        "Validate CUE syntax: cue vet examples/user-api.cue",
        "Review error message for specific line numbers and fix syntax",
        "Ensure all required fields are present in the spec"
    ]
}
```

### 3. Session Not Found

**Before:**
```
Error: Session not found: interview-20240101-999999
```

**After:**
```cue
{
    action: "session_error"
    error: {
        type: "session_not_found"
        message: "Interview session not found: interview-20240101-999999"
        context: {
            session_id: "interview-20240101-999999"
            sessions_file: ".interview/sessions.jsonl"
        }
    }
    suggestion: "Use a valid session ID from an existing interview"
    recovery: [
        "List all sessions: intent interview --list",
        "Check if .interview directory exists: ls -la .interview",
        "If no sessions exist, start a new interview: intent interview --profile api",
        "Verify session ID format (should be like: interview-20240101-120000)"
    ]
}
```

### 4. HTTP Connection Error

**Before:**
```
Error: Connection timeout
```

**After:**
```cue
{
    action: "http_error"
    error: {
        type: "connection_failed"
        message: "HTTP connection failed: Connection timeout"
        context: {
            target_url: "http://localhost:8080"
            error: "Connection timeout"
        }
    }
    suggestion: "Verify the target API is running and accessible"
    recovery: [
        "Check if server is running: curl http://localhost:8080",
        "Verify base_url in your spec is correct",
        "Test network connectivity: ping localhost",
        "Check for firewall rules blocking the connection",
        "If using HTTPS, verify SSL certificate is valid"
    ]
}
```

### 5. Variable Not Found (Interpolation Error)

**Before:**
```
Error: Variable not found: user_id
```

**After:**
```cue
{
    action: "interpolation_error"
    error: {
        type: "variable_not_found"
        message: "Variable not found: user_id"
        context: {
            variable: "user_id"
            available: "token, refresh_token"
        }
    }
    suggestion: "Ensure the variable is captured in a previous behavior"
    recovery: [
        "Check that a previous behavior captures this variable using 'captures'",
        "Verify the behavior order - variables must be captured before use",
        "Review spec for typos in variable names",
        "Use 'intent validate' to check for undefined variables"
    ]
}
```

### 6. Invalid Bead ID Format

**Before:**
```
Error: Invalid bead ID format: AUTH-99
```

**After:**
```cue
{
    action: "validation_error"
    error: {
        type: "invalid_input"
        message: "Invalid bead ID format: AUTH-99"
        context: {
            validation_error: "Invalid bead ID format: AUTH-99"
        }
    }
    suggestion: "Fix the validation error"
    recovery: [
        "Check the error message for specific validation failures",
        "Ensure session IDs are alphanumeric with hyphens/underscores",
        "Ensure bead IDs follow format: PREFIX-NNN (e.g., AUTH-001)",
        "Verify all required fields are present"
    ]
}
```

### 7. SSRF Protection Blocked

**Before:**
```
Error: Blocked request to localhost
```

**After:**
```cue
{
    action: "security_error"
    error: {
        type: "ssrf_blocked"
        message: "Security: Blocked request to localhost (127.x). Private IP ranges are not allowed for security reasons."
        context: {
            security_check: "SSRF protection"
        }
    }
    suggestion: "Use a public-facing URL for API testing"
    recovery: [
        "Blocked URLs: localhost, 127.x, 10.x, 192.168.x, 172.16-31.x",
        "Use a publicly accessible test server instead",
        "For local testing, deploy API to a public endpoint",
        "Review SSRF protection documentation for allowed URLs"
    ]
}
```

### 8. Permission Denied Writing File

**Before:**
```
Error: Failed to write JSONL - permission denied
```

**After:**
```cue
{
    action: "file_error"
    error: {
        type: "permission_denied"
        message: "Permission denied writing to: .interview/sessions.jsonl"
        context: {
            path: ".interview/sessions.jsonl"
        }
    }
    suggestion: "Check file/directory permissions"
    recovery: [
        "Check permissions: ls -la .interview",
        "Ensure you have write access to the directory",
        "If needed, adjust permissions: chmod u+w .interview",
        "Check if disk is full: df -h"
    ]
}
```

## Using AI-Friendly Errors

### In Code

All error modules now expose both CUE and text formatters:

```gleam
import intent/loader
import intent/ai_errors

// Load a spec
case loader.load_spec("missing.cue") {
  Ok(spec) -> // use spec
  Error(e) -> {
    // For AI agents (CUE format)
    let cue_error = loader.format_error_ai(e)
    io.println(cue_error)

    // For humans (readable text)
    let text_error = loader.format_error_text(e)
    io.println(text_error)
  }
}
```

### Available Formatters

Each error module provides:

1. **`format_error_ai(error)`** - Returns CUE-structured output for AI agents
2. **`format_error_text(error)`** - Returns human-readable text with suggestions
3. **`format_error(error)`** - Returns legacy simple error message

### Modules with AI-Friendly Errors

- `intent/ai_errors` - Core error builders and formatters
- `intent/loader` - CUE loading and validation errors
- `intent/bead_feedback` - Bead execution feedback errors
- `intent/http_client` - HTTP request execution errors
- `intent/interview_storage` - Session storage errors (planned)

## Benefits for AI Agents

1. **Self-Recovery**: AI agents can parse recovery steps and attempt fixes automatically
2. **Context Awareness**: Structured context helps agents understand the root cause
3. **Deterministic**: Same error always produces same structured output
4. **Actionable**: Every error includes concrete next steps
5. **Machine-Readable**: CUE format is parseable and can drive automated workflows

## Example AI Agent Workflow

When an AI agent encounters an error:

1. **Parse the CUE structure** to extract:
   - `action` - What category of error occurred
   - `error.type` - Specific error type for pattern matching
   - `error.context` - Relevant data (file paths, IDs, etc.)
   - `recovery` - Step-by-step fix instructions

2. **Attempt automated recovery**:
   ```gleam
   case error.action {
     "file_error" -> {
       // Extract directory from context
       // Run: mkdir -p <directory>
       // Retry the operation
     }
     "session_error" -> {
       // Run: intent interview --list
       // Parse available sessions
       // Select correct session ID
     }
     "http_error" -> {
       // Check base_url in spec
       // Verify server is running
       // Suggest spec fix
     }
   }
   ```

3. **Report back to user** if recovery fails, with full context

## Future Enhancements

- [ ] Add machine-readable error codes (e.g., `E001`, `E002`)
- [ ] Include links to documentation for each error type
- [ ] Add telemetry to track most common errors
- [ ] Generate error recovery suggestions using LLM
- [ ] Add error severity levels (warning, error, critical)
