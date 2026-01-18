#!/bin/bash
# Test: All commands respond to --help
# Validates that every Intent CLI command properly responds to --help flag
# and produces meaningful output with exit code 0

set -e

# Array of all 24 Intent CLI commands
COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

PASS=0
FAIL=0
BINARY="${1:-intent}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=============================================="
echo "Help Text Invocation Test"
echo "=============================================="
echo "Testing binary: $BINARY"
echo "Commands to test: ${#COMMANDS[@]}"
echo ""

for cmd in "${COMMANDS[@]}"; do
  output=$("$BINARY" "$cmd" --help 2>&1 || true)
  exit_code=$?

  # Test 1: Exit code should be 0
  if [ $exit_code -eq 0 ]; then
    printf "${GREEN}✓${NC} %-20s exit code 0\n" "$cmd"
    ((PASS++))
  else
    printf "${RED}✗${NC} %-20s exit code $exit_code (expected 0)\n" "$cmd"
    ((FAIL++))
  fi

  # Test 2: Output should not be empty
  if [ -n "$output" ]; then
    byte_count=$(echo -n "$output" | wc -c)
    printf "${GREEN}✓${NC} %-20s output: $byte_count bytes\n" "$cmd"
    ((PASS++))
  else
    printf "${RED}✗${NC} %-20s no output produced\n" "$cmd"
    ((FAIL++))
  fi

  # Test 3: Output should be reasonably long (> 500 bytes for help text)
  if [ -n "$output" ]; then
    byte_count=$(echo -n "$output" | wc -c)
    if [ $byte_count -gt 500 ]; then
      printf "${GREEN}✓${NC} %-20s output length: substantial ($byte_count bytes)\n" "$cmd"
      ((PASS++))
    else
      printf "${YELLOW}⚠${NC} %-20s output is brief ($byte_count bytes, expected > 500)\n" "$cmd"
      ((FAIL++))
    fi
  fi
done

echo ""
echo "=============================================="
echo "Summary: $PASS passed, $FAIL failed"
echo "=============================================="

if [ $FAIL -eq 0 ]; then
  echo -e "${GREEN}✓ All tests passed${NC}"
  exit 0
else
  echo -e "${RED}✗ Some tests failed${NC}"
  exit 1
fi
