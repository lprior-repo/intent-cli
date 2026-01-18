#!/bin/bash
# Test: Help text content quality
# Validates spelling, grammar, length, and formatting of help text

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
echo "Help Text Content Quality Test"
echo "=============================================="
echo ""

# Common misspellings
declare -A TYPOS=(
  ["teh"]="the"
  ["taht"]="that"
  ["becuase"]="because"
  ["recieve"]="receive"
  ["occured"]="occurred"
  ["seperate"]="separate"
  ["wiht"]="with"
  ["writting"]="writing"
)

for cmd in "${COMMANDS[@]}"; do
  help_text=$("$BINARY" "$cmd" --help 2>&1 || true)

  echo "Checking: $cmd"
  echo "---"

  # Check for common typos
  typo_found=false
  for typo in "${!TYPOS[@]}"; do
    if echo "$help_text" | grep -iq "\b$typo\b"; then
      printf "${RED}✗${NC} %-20s contains typo: '$typo' → '${TYPOS[$typo]}'\n" "$cmd"
      ((FAIL++))
      typo_found=true
    fi
  done

  if [ "$typo_found" = false ]; then
    printf "${GREEN}✓${NC} %-20s no common typos detected\n" "$cmd"
    ((PASS++))
  fi

  # Check for incomplete sentences (text ending without punctuation)
  # Look for lines that don't end with period, question mark, or colon
  incomplete=$(echo "$help_text" | \
    grep -vE "^[[:space:]]*$" | \
    grep -vE "[.:?!]$" | \
    grep -vE "^(WHAT|WHY|WHEN|PREREQUISITES|USAGE|FLAG|EXIT|SEE)" | \
    head -1 || true)

  if [ -z "$incomplete" ]; then
    printf "${GREEN}✓${NC} %-20s all sentences properly punctuated\n" "$cmd"
    ((PASS++))
  else
    printf "${YELLOW}⚠${NC} %-20s possible incomplete sentence\n" "$cmd"
    ((WARNINGS++))
  fi

  # Check line length - help text should wrap nicely at 80-100 chars
  long_lines=$(echo "$help_text" | awk 'length > 100' | wc -l)
  if [ "$long_lines" -eq 0 ]; then
    printf "${GREEN}✓${NC} %-20s all lines reasonable length\n" "$cmd"
    ((PASS++))
  else
    printf "${YELLOW}⚠${NC} %-20s $long_lines lines exceed 100 characters\n" "$cmd"
    ((WARNINGS++))
  fi

  # Check for consistent capitalization of "Intent"
  if echo "$help_text" | grep -qE "[Ii]ntent" | grep -qv "Intent"; then
    printf "${YELLOW}⚠${NC} %-20s inconsistent 'Intent' capitalization\n" "$cmd"
    ((WARNINGS++))
  else
    printf "${GREEN}✓${NC} %-20s consistent 'Intent' capitalization\n" "$cmd"
    ((PASS++))
  fi

  # Check for proper spacing in lists
  # Flag descriptions should be indented
  if echo "$help_text" | grep -q "^  --"; then
    printf "${GREEN}✓${NC} %-20s proper flag indentation\n" "$cmd"
    ((PASS++))
  else
    printf "${YELLOW}⚠${NC} %-20s flag indentation may be inconsistent\n" "$cmd"
    ((WARNINGS++))
  fi

  # Check that description doesn't just repeat the command name
  first_line=$(echo "$help_text" | head -1)
  if echo "$first_line" | grep -qi "^$cmd"; then
    printf "${YELLOW}⚠${NC} %-20s description may repeat command name\n" "$cmd"
    ((WARNINGS++))
  else
    printf "${GREEN}✓${NC} %-20s description is distinct from command name\n" "$cmd"
    ((PASS++))
  fi

  echo ""
done

echo "=============================================="
echo "Summary: $PASS passed, $FAIL failed, $WARNINGS warnings"
echo "=============================================="

if [ $FAIL -eq 0 ]; then
  echo -e "${GREEN}✓ Content quality acceptable${NC}"
  exit 0
else
  echo -e "${RED}✗ Content quality issues found${NC}"
  exit 1
fi
