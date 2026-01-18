#!/bin/bash
# Test: Help text contains all required sections
# Validates that each command has WHAT, WHY, WHEN, PREREQUISITES (optional),
# EXAMPLES, FLAGS, CODES, and SEE ALSO sections

set -e

COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

# Commands that are known to not require PREREQUISITES
COMMANDS_NO_PREREQS=(
  "sessions" "history" "diff" "plan" "plan-approve"
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
echo "Help Text Sections Validation Test"
echo "=============================================="
echo ""

check_section() {
  local cmd=$1
  local section=$2
  local help_text=$3
  local required=${4:-true}

  if echo "$help_text" | grep -q "^$section"; then
    printf "${GREEN}✓${NC} %-20s has %s\n" "$cmd" "$section"
    ((PASS++))
    return 0
  else
    if [ "$required" = "true" ]; then
      printf "${RED}✗${NC} %-20s missing %s\n" "$cmd" "$section"
      ((FAIL++))
    else
      printf "${YELLOW}⚠${NC} %-20s no %s (optional)\n" "$cmd" "$section"
      ((WARNINGS++))
    fi
    return 1
  fi
}

for cmd in "${COMMANDS[@]}"; do
  help_text=$("$BINARY" "$cmd" --help 2>&1 || true)

  echo "Checking: $cmd"
  echo "---"

  # Required sections
  check_section "$cmd" "WHAT IT DOES" "$help_text" "true"
  check_section "$cmd" "WHY YOU'D USE IT" "$help_text" "true"
  check_section "$cmd" "WHEN TO USE IT" "$help_text" "true"

  # Conditional PREREQUISITES
  if [[ " ${COMMANDS_NO_PREREQS[@]} " =~ " $cmd " ]]; then
    check_section "$cmd" "PREREQUISITES" "$help_text" "false"
  else
    check_section "$cmd" "PREREQUISITES" "$help_text" "true"
  fi

  # Usage examples (look for USAGE EXAMPLES or similar)
  if echo "$help_text" | grep -qE "^USAGE|^EXAMPLES"; then
    printf "${GREEN}✓${NC} %-20s has usage examples section\n" "$cmd"
    ((PASS++))
  else
    printf "${RED}✗${NC} %-20s missing usage examples section\n" "$cmd"
    ((FAIL++))
  fi

  # Flag details
  if echo "$help_text" | grep -qE "^FLAG|^OPTIONS"; then
    printf "${GREEN}✓${NC} %-20s has flag documentation\n" "$cmd"
    ((PASS++))
  else
    printf "${RED}✗${NC} %-20s missing flag documentation\n" "$cmd"
    ((FAIL++))
  fi

  # Exit codes
  check_section "$cmd" "EXIT CODES" "$help_text" "true"

  # See also
  if echo "$help_text" | grep -qE "^SEE ALSO|^RELATED|^RELATED COMMANDS"; then
    printf "${GREEN}✓${NC} %-20s has related commands\n" "$cmd"
    ((PASS++))
  else
    printf "${YELLOW}⚠${NC} %-20s no related commands section\n" "$cmd"
    ((WARNINGS++))
  fi

  echo ""
done

echo "=============================================="
echo "Summary: $PASS passed, $FAIL failed, $WARNINGS warnings"
echo "=============================================="

if [ $FAIL -eq 0 ]; then
  echo -e "${GREEN}✓ All required sections present${NC}"
  exit 0
else
  echo -e "${RED}✗ Some required sections missing${NC}"
  exit 1
fi
