#!/bin/bash
# Test: All flags are documented
# Validates that flags used in examples are documented in FLAG section

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
echo "Help Text Flag Coverage Test"
echo "=============================================="
echo ""

for cmd in "${COMMANDS[@]}"; do
  help_text=$("$BINARY" "$cmd" --help 2>&1 || true)

  echo "Checking: $cmd"
  echo "---"

  # Extract flags from examples section
  flags_in_examples=$(echo "$help_text" | \
    awk '/USAGE|EXAMPLES/,/^[A-Z]/' | \
    grep -oE '\-\-[a-z0-9][a-z0-9\-]*' | sort -u || true)

  # Extract documented flags from FLAG/OPTIONS section
  flags_documented=$(echo "$help_text" | \
    awk '/^FLAG|^OPTIONS|^OPTION/,/^[A-Z]/' | \
    grep -oE '\-\-[a-z0-9][a-z0-9\-]*' | sort -u || true)

  # Check coverage
  if [ -z "$flags_in_examples" ]; then
    printf "${GREEN}✓${NC} %-20s no flags in examples\n" "$cmd"
    ((PASS++))
  else
    all_documented=true
    for flag in $flags_in_examples; do
      if echo "$flags_documented" | grep -q "^$flag\$"; then
        printf "${GREEN}✓${NC} %-20s flag %s is documented\n" "$cmd" "$flag"
        ((PASS++))
      else
        printf "${RED}✗${NC} %-20s flag %s used but not documented\n" "$cmd" "$flag"
        ((FAIL++))
        all_documented=false
      fi
    done
  fi

  # Also check that documented flags are reasonable number
  flag_count=$(echo "$flags_documented" | wc -l)
  if [ "$flag_count" -gt 0 ]; then
    printf "${GREEN}✓${NC} %-20s %d flags documented\n" "$cmd" "$flag_count"
    ((PASS++))
  else
    printf "${YELLOW}⚠${NC} %-20s no flags documented\n" "$cmd"
    ((WARNINGS++))
  fi

  # Check for common undocumented flags
  common_flags="--json --verbose --quiet --help --output"
  for cflag in $common_flags; do
    if echo "$help_text" | grep -q "$cflag" && ! echo "$flags_documented" | grep -q "^$cflag\$"; then
      printf "${YELLOW}⚠${NC} %-20s common flag %s may not be documented\n" "$cmd" "$cflag"
      ((WARNINGS++))
    fi
  done

  echo ""
done

echo "=============================================="
echo "Summary: $PASS passed, $FAIL failed, $WARNINGS warnings"
echo "=============================================="

if [ $FAIL -eq 0 ]; then
  echo -e "${GREEN}✓ All flags documented${NC}"
  exit 0
else
  echo -e "${RED}✗ Some flags not documented${NC}"
  exit 1
fi
