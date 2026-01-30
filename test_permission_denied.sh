#!/usr/bin/env bash
# Test script for permission denied handling in sessions command
# Tests the fix for intent-cli-lvdn

set -e

echo "Testing permission denied handling..."

# Create test sessions file
TEST_FILE="/tmp/test-sessions-lvdn.jsonl"
echo '{"id":"test-session","profile":"api","created_at":"2024-01-01T00:00:00Z","updated_at":"2024-01-01T00:00:00Z","completed_at":"","stage":"discovery","rounds_completed":0,"answers":[],"gaps":[],"conflicts":[],"raw_notes":""}' > "$TEST_FILE"

# Test 1: Normal case - file exists and is readable
echo "Test 1: Normal case (file exists and readable)"
OUTPUT=$(gleam run -- sessions 2>&1 || echo "EXIT:$?")
echo "Output (truncated): ${OUTPUT:0:200}"

# Test 2: File doesn't exist - should return exit 0 with empty list
echo ""
echo "Test 2: File not found (should exit 0 with empty sessions)"
rm -f .intent/sessions.jsonl
OUTPUT=$(gleam run -- sessions 2>&1)
EXIT_CODE=$?
echo "Exit code: $EXIT_CODE"
if [ $EXIT_CODE -eq 0 ]; then
  echo "✓ Correct exit code (0 for file not found)"
else
  echo "✗ Wrong exit code (expected 0, got $EXIT_CODE)"
fi

# Test 3: Permission denied - should return exit 4 with error
echo ""
echo "Test 3: Permission denied (should exit 4 with error)"
echo '{"id":"test","profile":"api","created_at":"2024-01-01T00:00:00Z","updated_at":"2024-01-01T00:00:00Z","completed_at":"","stage":"discovery","rounds_completed":0}' > .intent/sessions.jsonl
chmod 000 .intent/sessions.jsonl
OUTPUT=$(gleam run -- sessions 2>&1 || true)
EXIT_CODE=$?
chmod 644 .intent/sessions.jsonl  # Restore permissions
echo "Exit code: $EXIT_CODE"
echo "Output (truncated): ${OUTPUT:0:300}"
if [ $EXIT_CODE -eq 4 ]; then
  echo "✓ Correct exit code (4 for permission denied)"
else
  echo "✗ Wrong exit code (expected 4, got $EXIT_CODE)"
fi

if echo "$OUTPUT" | grep -q "PERMISSION_DENIED"; then
  echo "✓ Output contains PERMISSION_DENIED error"
else
  echo "✗ Output missing PERMISSION_DENIED error"
fi

# Cleanup
rm -f .intent/sessions.jsonl "$TEST_FILE"

echo ""
echo "Test complete!"
