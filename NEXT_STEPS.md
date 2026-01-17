# Immediate Next Steps for Ralph Loop

## Current State

**Commits**:
- ✅ 90313a1 - P0 Issue 1: Flag syntax normalization (COMPLETE)
- ✅ 833e269 - P0 Issue 2: FFI and Config type added (PARTIAL)

**Compilation Status**: FAILING (Config arity errors in test files)

## Quick Fix Required

All Config constructions need `allow_localhost: False` added.

### Automated Fix Script

```bash
# Create a Python script to fix Config constructions
cat > fix_config.py << 'EOF'
import re
import sys

def fix_config_in_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Pattern to match Config( ... headers: dict.new(), )
    # and add allow_localhost: False before the closing paren
    pattern = r'(types\.Config\([^)]*headers: dict\.new\(\),)\s*\)'
    replacement = r'\1\n      allow_localhost: False,\n    )'

    content = re.sub(pattern, replacement, content, flags=re.MULTILINE | re.DOTALL)

    # Also handle Config without types. prefix
    pattern2 = r'(Config\([^)]*headers: dict\.new\(\),)\s*\)'
    replacement2 = r'\1\n      allow_localhost: False,\n    )'
    content = re.sub(pattern2, replacement2, content, flags=re.MULTILINE | re.DOTALL)

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Fixed {filepath}")

if __name__ == "__main__":
    files = [
        "test/intent_test.gleam",
        "test/intent/quality_analyzer_test.gleam",
    ]
    for f in files:
        fix_config.py(f)
EOF

python3 fix_config.py
gleam build --target erlang
```

### Manual Fix Alternative

For each file with Config errors:

**Pattern to find**:
```gleam
Config(
  base_url: "...",
  timeout_ms: 5000,
  headers: dict.new(),
)
```

**Replace with**:
```gleam
Config(
  base_url: "...",
  timeout_ms: 5000,
  headers: dict.new(),
  allow_localhost: False,
)
```

**Files to fix** (from gleam build output):
1. `test/intent_test.gleam` - 15 occurrences
2. `test/intent/quality_analyzer_test.gleam` - 2 occurrences

### After Fixes Compile

1. Run `gleam test` - should pass all tests
2. Commit: `git add -A && git commit -m "fix(P0): Add allow_localhost to all Config constructions"`
3. Continue with remaining P0 Issue 2 work

## Remaining P0 Issue 2 Tasks

Once compilation succeeds:

### 1. Add External Bindings
**File**: `src/intent.gleam`
```gleam
@external(erlang, "intent_ffi", "get_env")
fn get_env(name: String) -> Result(String, Nil)

fn is_localhost_allowed_by_env() -> Bool {
  case get_env("INTENT_ALLOW_LOCALHOST") {
    Ok("true") | Ok("1") | Ok("yes") -> True
    _ -> False
  }
}
```

### 2. Add Flag to check Command
**File**: `src/intent.gleam` - in `check_command()` function

Add flag:
```gleam
|> glint.flag(
  "allow-localhost",
  flag.bool()
    |> flag.default(False)
    |> flag.description("Allow localhost for development (bypasses SSRF protection)"),
)
```

Read and use flag:
```gleam
let allow_localhost =
  flag.get_bool(input.flags, "allow-localhost")
  |> result.unwrap(False)
  || is_localhost_allowed_by_env()

// Pass to config override when loading spec
```

### 3. Update Security Validation
**File**: `src/intent/security.gleam`

Make validate_url accept allow_localhost parameter or create new function:
```gleam
pub fn validate_url_with_config(url: String, allow_localhost: Bool) -> Result(Nil, SecurityError) {
  // ... existing setup ...

  case string.contains(url_lower, "localhost") || string.contains(url_lower, "127.0.") {
    True -> {
      case allow_localhost {
        True -> Ok(Nil)
        False -> Error(SSRFAttempt(url, "Localhost blocked. Use --allow-localhost for dev testing."))
      }
    }
    False -> // continue existing validation
  }
}
```

### 4. Thread Through HTTP Client
**File**: `src/intent/http_client.gleam`

Update validate_safe_url to accept and use config.

### 5. Create Tests
**File**: `test/localhost_support_test.gleam` (NEW)

Create comprehensive tests for localhost scenarios.

### 6. Run Full Test Suite
```bash
gleam test
```

All 1580+ tests should pass.

### 7. Manual Testing
```bash
# Should fail
intent check examples/pokemon-api.cue --target http://localhost:8080

# Should work
intent check examples/pokemon-api.cue --target http://localhost:8080 --allow-localhost

# Should work
INTENT_ALLOW_LOCALHOST=true intent check examples/pokemon-api.cue --target http://localhost:8080
```

### 8. Final Commit and Close Bead
```bash
git add -A
git commit -m "feat(P0): Add localhost bypass for development testing

- Add --allow-localhost flag to check command
- Add INTENT_ALLOW_LOCALHOST env var support
- Update security validation to conditionally allow localhost
- Thread config through HTTP client
- Add comprehensive tests

Closes: intent-cli-utkb"

bd close intent-cli-utkb --reason="Complete localhost bypass implementation. All tests passing."
```

## Then: Move to P1

Start implementing:
1. Consistent JSON output across all commands
2. Structured error recovery
3. Spinner suppression

Follow same TDD approach.

