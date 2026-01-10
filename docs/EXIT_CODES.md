# Exit Codes Reference

Intent CLI uses machine-readable exit codes to communicate test results and errors. This document provides comprehensive guidance on understanding, using, and troubleshooting exit codes in various scenarios.

## Exit Code Summary

| Exit Code | Name | Severity | CI Behavior |
|-----------|------|----------|------------|
| **0** | Success | None | Pipeline continues |
| **1** | Failed | High | Pipeline fails |
| **2** | Blocked | Medium | Pipeline fails |
| **3** | Invalid | High | Pipeline fails |
| **4** | Error | Critical | Pipeline fails |

## Detailed Exit Codes

### Exit Code 0: Success

All checks passed successfully. The specification is valid and all behaviors executed without failures.

**When it occurs:**
- All behaviors passed their validation checks
- No rule violations were detected
- All dependencies executed successfully
- The spec file is syntactically and semantically valid

**Examples:**

```bash
$ intent check api.cue --target http://localhost:8080
PASS
Passed: 5 / Failed: 0 / Blocked: 0 / Total: 5
All checks passed!
$ echo $?
0
```

**CI/CD Implications:**
```yaml
# GitHub Actions example
- name: Run Intent tests
  run: |
    intent check spec.cue --target ${{ secrets.API_URL }}
  # Only proceeds if exit code is 0
```

---

### Exit Code 1: Failed

One or more behaviors failed validation. The specification is valid and executable, but some behaviors did not meet their expected outcomes.

**When it occurs:**
- HTTP status code mismatch
- Response body validation failed
- Field-level checks failed (e.g., UUID format, regex pattern)
- Rule violations detected (global rules matched and failed)
- Anti-pattern violations detected
- Response headers validation failed

**Examples:**

```bash
$ intent check api.cue --target http://localhost:8080
FAIL
Passed: 3 / Failed: 2 / Blocked: 0 / Total: 5

FAILURES:

[User Management] create-user
Intent: Create a new user with valid email
Problems:
  - status: HTTP status code mismatch
    Expected: 201
    Actual: 200
  - id: Value mismatch
    Expected: uuid
    Actual: not-a-uuid
$ echo $?
1
```

**Troubleshooting Steps:**
1. Run with `--verbose` flag for detailed error information
2. Check if the API is running on the expected target URL
3. Verify the specification matches the actual API behavior
4. Review the Problems section for specific validation failures
5. Check field-level `checks` for overly strict validation rules

**CI/CD Implications:**
```yaml
# GitHub Actions with detailed failure reporting
- name: Run Intent tests
  id: intent
  continue-on-error: true
  run: |
    intent check spec.cue --target ${{ secrets.API_URL }} --json > results.json || true

- name: Display detailed results
  if: steps.intent.outcome == 'failure'
  run: |
    cat results.json | jq '.'
```

---

### Exit Code 2: Blocked

Dependencies failed, blocking execution of dependent behaviors. Some behaviors could not run because their required dependencies failed.

**When it occurs:**
- A required behavior failed validation
- A required behavior was invalid
- Circular dependency detected (should not happen with valid specs)
- Dependency chain was broken by a failure

**Examples:**

```bash
$ intent check api.cue --target http://localhost:8080
FAIL
Passed: 2 / Failed: 1 / Blocked: 2 / Total: 5

BLOCKED:

[Item Management] get-item
Blocked: Requires 'create-item' which failed

[Item Management] update-item
Blocked: Requires 'create-item' and 'get-item' which failed/blocked
$ echo $?
2
```

**Troubleshooting Steps:**
1. Identify the root cause failure in the FAILURES section
2. Fix the failed behavior first
3. Verify dependency chains are correct
4. Check for unintended dependencies
5. Use `--only` flag to test specific behaviors in isolation

**CI/CD Implications:**
```yaml
# Continue execution but mark as unstable
- name: Run Intent tests
  run: |
    intent check spec.cue --target ${{ secrets.API_URL }} || EXIT_CODE=$?
    if [ "$EXIT_CODE" = "2" ]; then
      echo "Tests blocked - marking as unstable"
      exit 0
    elif [ "$EXIT_CODE" != "0" ]; then
      exit 1
    fi
```

---

### Exit Code 3: Invalid

The specification file is invalid, not found, or has syntax errors. The spec could not be parsed or validated.

**When it occurs:**
- Spec file not found at the specified path
- Invalid CUE syntax (parsing errors)
- Missing required fields in the spec
- Type mismatches in the specification
- Invalid feature/behavior structure
- Circular dependencies between behaviors

**Examples:**

```bash
$ intent check api.cue --target http://localhost:8080
Error: failed to load spec: /home/user/api.cue: openfile: No such file or directory
$ echo $?
3
```

```bash
$ intent check api.cue --target http://localhost:8080
Error: spec validation failed: missing required field 'name' in spec
$ echo $?
3
```

**Troubleshooting Steps:**
1. Verify the spec file path is correct
2. Run `intent validate spec.cue` for detailed validation errors
3. Check CUE syntax: `cue fmt spec.cue` or `cue vet spec.cue`
4. Verify all required fields are present
5. Check for typos in field names
6. Review the specification format in `SPEC_FORMAT.md`

**CI/CD Implications:**
```yaml
# Early exit for invalid specs
- name: Validate specification
  run: |
    if ! intent validate spec.cue; then
      echo "Invalid specification - cannot proceed"
      exit 3
    fi
```

---

### Exit Code 4: Error

A general error occurred that prevented normal execution. This indicates a system-level error, misuse, or unexpected condition.

**When it occurs:**
- Invalid command-line arguments or flags
- Network connectivity issues (API unreachable)
- Timeout while waiting for API response
- Invalid JSON response from API
- Permission errors (file access issues)
- Out of memory or system resource limits
- Internal runtime errors

**Examples:**

```bash
$ intent check api.cue --target http://localhost:8080
Error: failed to connect to http://localhost:8080: Connection refused
$ echo $?
4
```

```bash
$ intent check api.cue --target http://localhost:8080
Error: request timeout after 5000ms
$ echo $?
4
```

```bash
$ intent check api.cue --invalid-flag
Error: unknown flag: --invalid-flag
$ echo $?
4
```

**Troubleshooting Steps:**
1. Check network connectivity to the target URL
2. Verify the API server is running and accessible
3. Check firewall/proxy settings
4. Increase timeout with `config.timeout_ms` in the spec
5. Verify command-line arguments with `intent --help`
6. Check file permissions for the spec file and working directory
7. Review system logs for resource issues

**CI/CD Implications:**
```yaml
# Retry mechanism for transient errors
- name: Run Intent tests with retry
  uses: nick-fields/retry-action@v2
  with:
    timeout_minutes: 10
    max_attempts: 3
    command: |
      intent check spec.cue --target ${{ secrets.API_URL }}
```

## Test Scenarios Matrix

| Scenario | Exit Code | Example Command | Expected Behavior |
|----------|-----------|-----------------|-------------------|
| Valid spec, all tests pass | 0 | `intent check spec.cue --target http://api:8080` | Success, continue |
| Valid spec, tests fail | 1 | `intent check spec.cue --target http://api:8080` | Failure, stop |
| Valid spec, dependencies fail | 2 | `intent check spec.cue --target http://api:8080` | Failure, stop |
| Invalid spec syntax | 3 | `intent check spec.cue --target http://api:8080` | Failure, stop |
| Spec file not found | 3 | `intent check missing.cue --target http://api:8080` | Failure, stop |
| API unreachable | 4 | `intent check spec.cue --target http://invalid:9999` | Failure, stop |
| Invalid command arguments | 4 | `intent check spec.cue --unknown-flag` | Failure, stop |
| Network timeout | 4 | `intent check spec.cue --target http://api:8080` | Failure, stop |

## CI/CD Usage Patterns

### GitHub Actions

#### Basic Integration

```yaml
name: API Tests

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    services:
      api:
        image: my-api:latest
        ports:
          - 8080:8080

    steps:
      - uses: actions/checkout@v3

      - name: Install Intent
        run: |
          # Install instructions from INSTALLATION.md
          curl -sSL https://install.intent.sh | sh

      - name: Run Intent tests
        run: |
          intent check spec.cue --target http://localhost:8080
```

#### With Detailed Reporting

```yaml
- name: Run Intent tests
  id: test
  continue-on-error: true
  run: |
    intent check spec.cue --target http://localhost:8080 --json > results.json || EXIT_CODE=$?
    echo "EXIT_CODE=$EXIT_CODE" >> $GITHUB_ENV

- name: Upload test results
  if: always()
  uses: actions/upload-artifact@v3
  with:
    name: intent-results
    path: results.json

- name: Fail on actual failures
  run: |
    if [ "$EXIT_CODE" != "0" ] && [ "$EXIT_CODE" != "2" ]; then
      echo "Tests failed or error occurred"
      exit 1
    fi

- name: Comment on PR
  if: github.event_name == 'pull_request' && always()
  uses: actions/github-script@v6
  with:
    script: |
      const fs = require('fs');
      const results = JSON.parse(fs.readFileSync('results.json', 'utf8'));
      const comment = `## Intent Test Results\n\n${JSON.stringify(results, null, 2)}`;
      github.rest.issues.createComment({
        issue_number: context.issue.number,
        owner: context.repo.owner,
        repo: context.repo.repo,
        body: comment
      });
```

### GitLab CI

```yaml
stages:
  - test

test-api:
  stage: test
  services:
    - name: my-api:latest
      alias: api
  script:
    - intent check spec.cue --target http://api:8080
  artifacts:
    reports:
      junit: intent-results.xml
    when: always
  allow_failure:
    exit_codes: 2  # Treat blocked as non-fatal
```

### Jenkins

```groovy
pipeline {
  agent any

  stages {
    stage('Test API') {
      steps {
        script {
          def exitCode = sh(
            script: 'intent check spec.cue --target http://api:8080',
            returnStatus: true
          )

          if (exitCode == 0) {
            echo 'All tests passed'
          } else if (exitCode == 1) {
            error('Tests failed')
          } else if (exitCode == 2) {
            echo 'Tests blocked - marking as unstable'
            currentBuild.result = 'UNSTABLE'
          } else if (exitCode == 3) {
            error('Invalid specification')
          } else {
            error('System error occurred')
          }
        }
      }
    }
  }
}
```

## Advanced Patterns

### Retry Logic

```bash
#!/bin/bash
# retry-intent.sh - Retry Intent tests with backoff

MAX_RETRIES=3
RETRY_DELAY=5

for i in $(seq 1 $MAX_RETRIES); do
  echo "Attempt $i of $MAX_RETRIES..."
  intent check spec.cue --target http://localhost:8080
  EXIT_CODE=$?

  # Don't retry on invalid spec or system errors
  if [ $EXIT_CODE -eq 3 ] || [ $EXIT_CODE -eq 4 ]; then
    echo "Fatal error - not retrying"
    exit $EXIT_CODE
  fi

  # Success or unrecoverable failure
  if [ $EXIT_CODE -ne 2 ]; then
    exit $EXIT_CODE
  fi

  # Blocked - retry after delay
  if [ $i -lt $MAX_RETRIES ]; then
    echo "Tests blocked, retrying in ${RETRY_DELAY}s..."
    sleep $RETRY_DELAY
  fi
done

echo "Max retries exceeded"
exit 2
```

### Parallel Testing

```bash
#!/bin/bash
# parallel-intent.sh - Run multiple specs in parallel

specs=("specs/users.cue" "specs/items.cue" "specs/orders.cue")
pids=()
exit_codes=()

for spec in "${specs[@]}"; do
  echo "Starting: $spec"
  intent check "$spec" --target http://localhost:8080 > "results/$(basename $spec).log" 2>&1 &
  pids+=($!)
done

# Wait for all jobs and collect exit codes
for i in "${!pids[@]}"; do
  wait ${pids[$i]}
  exit_codes+=($?)
  echo "Completed: ${specs[$i]} (exit code: ${exit_codes[$i]})"
done

# Aggregate results
failed=0
blocked=0
invalid=0
errors=0

for code in "${exit_codes[@]}"; do
  case $code in
    1) failed=$((failed + 1)) ;;
    2) blocked=$((blocked + 1)) ;;
    3) invalid=$((invalid + 1)) ;;
    4) errors=$((errors + 1)) ;;
  esac
done

echo ""
echo "Summary:"
echo "  Failed: $failed"
echo "  Blocked: $blocked"
echo "  Invalid: $invalid"
echo "  Errors: $errors"

# Exit with code 1 if any failures
if [ $((failed + blocked + invalid + errors)) -gt 0 ]; then
  exit 1
fi

exit 0
```

### Spec Validation Before Deployment

```bash
#!/bin/bash
# validate-and-deploy.sh

# Validate spec before deployment
echo "Validating specification..."
intent validate spec.cue
if [ $? -ne 0 ]; then
  echo "Invalid specification - cannot deploy"
  exit 3
fi

# Run smoke tests
echo "Running smoke tests..."
intent check spec.cue --target http://staging-api:8080
TEST_EXIT=$?

case $TEST_EXIT in
  0)
    echo "Smoke tests passed - deploying to production"
    # Deployment commands...
    exit 0
    ;;
  1)
    echo "Smoke tests failed - blocking deployment"
    exit 1
    ;;
  2)
    echo "Smoke tests blocked - blocking deployment"
    exit 2
    ;;
  *)
    echo "Unexpected error - blocking deployment"
    exit 4
    ;;
esac
```

## Troubleshooting Guides

### Issue: Exit Code 4 on Valid Spec

**Symptoms:** Spec validates successfully but `intent check` exits with code 4.

**Possible Causes:**
1. API server is not running
2. Network connectivity issues
3. Firewall or proxy blocking requests
4. Request timeout (API too slow)
5. Invalid JSON response from API

**Solutions:**
1. Verify API is running: `curl http://localhost:8080/health`
2. Test network connectivity: `ping localhost`
3. Check firewall rules: `sudo iptables -L`
4. Increase timeout in spec: `config.timeout_ms: 10000`
5. Validate API response format: `curl http://localhost:8080/api | jq .`

### Issue: Exit Code 2 With No Failures

**Symptoms:** Output shows "Failed: 0 / Blocked: N" but exit code is 2.

**Possible Causes:**
1. Dependency chain has failures that are being suppressed
2. Blocked behaviors are considered failures in CI
3. Expected behavior - blocked dependencies are considered failures

**Solutions:**
1. Run with `--verbose` to see dependency details
2. Use `--only` flag to test specific behaviors: `intent check spec.cue --only "behavior-name"`
3. Review dependency chains in the spec
4. In CI, treat exit code 2 differently if desired (see examples above)

### Issue: Exit Code 3 on Previously Valid Spec

**Symptoms:** Spec was valid before, now exits with code 3.

**Possible Causes:**
1. File was corrupted or accidentally modified
2. Dependencies updated with breaking changes
3. CUE language version mismatch
4. Spec file path is wrong or file doesn't exist

**Solutions:**
1. Check file permissions: `ls -la spec.cue`
2. Verify file exists: `ls spec.cue`
3. Restore from version control: `git checkout HEAD -- spec.cue`
4. Validate CUE syntax: `cue fmt spec.cue` or `cue vet spec.cue`
5. Check Intent CLI version: `intent --version`

### Issue: Flaky Exit Codes (Sometimes 0, Sometimes 1)

**Symptoms:** Same spec produces different exit codes on different runs.

**Possible Causes:**
1. API has race conditions or timing issues
2. Network instability
3. API state changes between runs
4. Resource contention in CI environment
5. Non-deterministic data (timestamps, random IDs)

**Solutions:**
1. Add retry logic (see examples above)
2. Increase timeout values in the spec
3. Use deterministic test data
4. Isolate test environments
5. Run tests sequentially instead of in parallel
6. Check API logs for timing-related issues

## Best Practices

### For CI/CD Pipelines

1. **Validate specs early**: Run `intent validate` before building or deploying
2. **Use JSON output**: `--json` flag for machine-readable results
3. **Handle all exit codes**: Don't just check for 0 vs non-zero
4. **Provide detailed logs**: Use `--verbose` in CI for debugging
5. **Upload test artifacts**: Save JSON results for analysis
6. **Set appropriate timeouts**: Prevent CI jobs from hanging

### For Shell Scripts

1. **Capture exit codes**: Always check `$?` after running Intent
2. **Provide helpful error messages**: Map exit codes to human-readable messages
3. **Clean up resources**: Ensure processes terminate even on errors
4. **Log all output**: Redirect stdout and stderr to log files
5. **Use `set -e` with caution**: It exits on any non-zero, including exit code 2

### For Development

1. **Use local testing**: Test against local API before pushing
2. **Incremental changes**: Test one behavior at a time with `--only`
3. **Understand dependencies**: Know which behaviors block others
4. **Read error messages**: The Problems section contains actionable information
5. **Keep specs simple**: Complex specs are harder to debug

## Additional Resources

- [User Guide](USER_GUIDE.md) - Comprehensive usage guide
- [Specification Format](SPEC_FORMAT.md) - Spec file reference
- [Installation Guide](INSTALLATION.md) - Installation instructions
- GitHub Issues - Report bugs or request features

## Quick Reference Card

```
0  Success
   → All checks passed

1  Failed
   → Behaviors failed or rules violated

2  Blocked
   → Dependencies failed, blocking execution

3  Invalid
   → Spec file errors (syntax, missing fields, not found)

4  Error
   → System errors (network, timeout, permissions)
```

Use `intent --help` to view exit codes in the CLI help output.
Use `intent exit-codes` or `intent --exit-codes` for dedicated exit codes documentation.
