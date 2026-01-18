#!/bin/bash
# Intent CLI Help Text Validation Script
# Validates that all 24 commands have proper help text

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "Intent CLI Help Text Validation"
echo "=========================================="
echo ""

commands=(
    "check" "validate" "show" "export"
    "lint" "analyze" "improve" "doctor"
    "interview" "beads" "bead-status" "history"
    "diff" "sessions"
    "quality" "invert" "coverage" "gaps" "effects" "ears" "parse"
    "plan" "plan-approve" "beads-regenerate"
)

passed=0
failed=0
warnings=0

for cmd in "${commands[@]}"; do
    output=$(gleam run -- "$cmd" --help 2>&1)

    # Check if help is present
    if echo "$output" | grep -q "USAGE"; then
        echo -e "${GREEN}✓${NC} $cmd"
        passed=$((passed + 1))
    else
        echo -e "${RED}✗${NC} $cmd - No help text"
        failed=$((failed + 1))
    fi
done

echo ""
echo "=========================================="
echo "Results:"
echo -e "  ${GREEN}Passed:${NC} $passed"
echo -e "  ${RED}Failed:${NC} $failed"
echo -e "  ${YELLOW}Total:${NC} ${#commands[@]}"
echo "=========================================="

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}✓ All commands have help text${NC}"
    exit 0
else
    echo -e "${RED}✗ Some commands missing help${NC}"
    exit 1
fi
