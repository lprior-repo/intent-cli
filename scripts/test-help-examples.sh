#!/bin/bash
# Test: Usage examples are present and have valid syntax
# Validates that each command has at least 2 examples and they use realistic paths

set -e

COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

PASS=0
FAIL=0
WARNINGS=0
BINARY="${1:-intent}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=============================================="
echo "Help Text Examples Validation Test"
echo "=============================================="
echo ""

for cmd in "${COMMANDS[@]}"; do
  help_text=$("$BINARY" "$cmd" --help 2>&1 || true)

  echo "Checking: $cmd"
  echo "---"

  # Count examples (lines starting with command name or 'intent' in examples section)
  # Extract from USAGE EXAMPLES section
  examples_section=$(echo "$help_text" | \
    awk '/USAGE|EXAMPLES/,/^[A-Z]/' | \
    grep -E "^\s+(intent|$cmd)" || true)

  example_count=$(echo "$examples_section" | grep -c "intent" || echo 0)

  if [ "$example_count" -ge 2 ]; then
    printf "${GREEN}✓${NC} %-20s has %d examples\n" "$cmd" "$example_count"
    ((PASS++))
  elif [ "$example_count" -eq 1 ]; then
    printf "${YELLOW}⚠${NC} %-20s has only 1 example (need 2+)\n" "$cmd"
    ((WARNINGS++))
  else
    printf "${RED}✗${NC} %-20s has no examples\n" "$cmd"
    ((FAIL++))
  fi

  # Check for hardcoded absolute paths (bad practice)
  # Look for /tmp, /home, /var, /opt, /usr patterns in examples
  bad_paths=$(echo "$examples_section" | grep -E "/(tmp|home|var|opt|usr)/" || true)

  if [ -z "$bad_paths" ]; then
    printf "${GREEN}✓${NC} %-20s examples avoid absolute paths\n" "$cmd"
    ((PASS++))
  else
    printf "${RED}✗${NC} %-20s examples contain absolute paths:\n" "$cmd"
    echo "$bad_paths" | sed 's/^/      /'
    ((FAIL++))
  fi

  # Check for common bad patterns in examples
  # Missing required flags, unrealistic scenarios, etc.
  if echo "$examples_section" | grep -qE "TODO|FIXME|placeholder|YOUR_"; then
    printf "${YELLOW}⚠${NC} %-20s examples contain placeholder text\n" "$cmd"
    ((WARNINGS++))
  else
    printf "${GREEN}✓${NC} %-20s examples are concrete\n" "$cmd"
    ((PASS++))
  fi

  # Validate basic command structure
  # Check that intent commands at least start with 'intent <cmd>'
  invalid_examples=$(echo "$examples_section" | grep -v "^intent" | grep "intent" || true)
  if [ -n "$invalid_examples" ]; then
    printf "${YELLOW}⚠${NC} %-20s some examples may have odd formatting\n" "$cmd"
    ((WARNINGS++))
  else
    printf "${GREEN}✓${NC} %-20s examples follow standard format\n" "$cmd"
    ((PASS++))
  fi

  echo ""
done

echo "=============================================="
echo "Summary: $PASS passed, $FAIL failed, $WARNINGS warnings"
echo "=============================================="

if [ $FAIL -eq 0 ]; then
  echo -e "${GREEN}✓ All examples valid${NC}"
  exit 0
else
  echo -e "${RED}✗ Some examples have issues${NC}"
  exit 1
fi
