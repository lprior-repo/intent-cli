#!/bin/bash
# Master test runner for all help text validation tests
# Orchestrates execution of all test scripts and produces summary report

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS=(
  "test-help-invocation"
  "test-help-sections"
  "test-help-examples"
  "test-help-flags"
  "test-help-quality"
)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Get binary path from argument or use default
BINARY="${1:-intent}"

echo ""
echo "=============================================="
echo "Intent CLI Help Text Test Suite"
echo "=============================================="
echo ""
echo "Binary: $BINARY"
echo "Tests: ${#TESTS[@]}"
echo ""

TOTAL_PASSED=0
TOTAL_FAILED=0
TOTAL_WARNINGS=0
TESTS_PASSED=0
TESTS_FAILED=0

# Make scripts executable
for test in "${TESTS[@]}"; do
  chmod +x "$SCRIPT_DIR/$test.sh" 2>/dev/null || true
done

# Run each test
for test in "${TESTS[@]}"; do
  echo -e "${BLUE}Running: $test${NC}"
  echo "---"

  if bash "$SCRIPT_DIR/$test.sh" "$BINARY" 2>&1; then
    echo -e "${GREEN}✓ $test PASSED${NC}"
    ((TESTS_PASSED++))
  else
    echo -e "${RED}✗ $test FAILED${NC}"
    ((TESTS_FAILED++))
  fi

  echo ""
done

# Summary
echo "=============================================="
echo "Test Suite Summary"
echo "=============================================="
echo ""
echo "Tests Passed:  $TESTS_PASSED/${#TESTS[@]}"
echo "Tests Failed:  $TESTS_FAILED/${#TESTS[@]}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
  echo -e "${GREEN}✓ All tests passed!${NC}"
  echo ""
  echo "Help text quality status: ACCEPTABLE"
  exit 0
else
  echo -e "${RED}✗ Some tests failed. Review output above.${NC}"
  echo ""
  echo "Help text quality status: NEEDS FIXES"
  exit 1
fi
