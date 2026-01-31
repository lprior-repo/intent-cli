#!/usr/bin/env bash
# Convert a validated CUE bead specification into a bd issue
# Usage: cue-to-bd.sh <bead.cue>
#
# This script:
# 1. Validates the CUE file
# 2. Exports to JSON
# 3. Creates a bd issue with the full specification as description

set -euo pipefail

BEAD_FILE="${1:-}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ -z "$BEAD_FILE" ]]; then
    echo "Usage: cue-to-bd.sh <bead.cue>"
    exit 2
fi

# Validate first
echo "Step 1: Validating CUE schema..."
if ! "$SCRIPT_DIR/validate-bead-cue.sh" "$BEAD_FILE"; then
    echo "ERROR: Bead failed validation. Fix errors before importing."
    exit 1
fi

echo ""
echo "Step 2: Exporting to JSON..."
BEAD_JSON=$(cue export "$BEAD_FILE" --out json 2>/dev/null)

# Extract fields
TITLE=$(echo "$BEAD_JSON" | jq -r '.bead.title')
TYPE=$(echo "$BEAD_JSON" | jq -r '.bead.type')
PRIORITY=$(echo "$BEAD_JSON" | jq -r '.bead.priority')
LABELS=$(echo "$BEAD_JSON" | jq -r '.bead.labels | join(",")')

# Generate markdown description from the CUE file
echo ""
echo "Step 3: Generating markdown description..."
DESCRIPTION=$(cat "$BEAD_FILE")

echo ""
echo "Step 4: Creating bd issue..."
echo "  Title: $TITLE"
echo "  Type: $TYPE"
echo "  Priority: P$PRIORITY"
echo "  Labels: $LABELS"
echo ""

# Create the issue
RESULT=$(bd create \
    --type "$TYPE" \
    --priority "$PRIORITY" \
    --title "$TITLE" \
    --labels "$LABELS" \
    --description "$DESCRIPTION" \
    2>&1)

echo "$RESULT"

# Extract the issue ID from the output
ISSUE_ID=$(echo "$RESULT" | grep -oP 'intent-cli-[a-z0-9]+' | head -1)

if [[ -n "$ISSUE_ID" ]]; then
    echo ""
    echo "✅ Successfully created bd issue: $ISSUE_ID"
    echo ""
    echo "View with: bd show $ISSUE_ID"
    echo "Validate with: .beads/validate-bead.sh $ISSUE_ID"
else
    echo "❌ Failed to create bd issue"
    exit 1
fi
