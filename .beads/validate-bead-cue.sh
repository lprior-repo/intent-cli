#!/usr/bin/env bash
# Validate bead against CUE schema
# Requires: cue command-line tool
# Usage: validate-bead-cue.sh <bead-id>

set -euo pipefail

BEAD_ID="${1:-}"
SCHEMA_PATH="/home/lewis/src/intent-cli/schema/enhanced-bead.cue"

if [[ -z "$BEAD_ID" ]]; then
    echo "Usage: validate-bead-cue.sh <bead-id>"
    exit 2
fi

# Check if cue is available
if ! command -v cue &> /dev/null; then
    echo "WARNING: cue command not found, falling back to text validation"
    exec /home/lewis/src/intent-cli/.beads/validate-bead.sh "$BEAD_ID"
fi

# Export bead to temp JSON
TEMP_FILE=$(mktemp /tmp/bead-XXXXXX.json)
trap "rm -f $TEMP_FILE" EXIT

bd show "$BEAD_ID" --json > "$TEMP_FILE" 2>/dev/null || {
    echo "ERROR: Bead $BEAD_ID not found"
    exit 2
}

# Validate against schema
if cue vet "$SCHEMA_PATH" "$TEMP_FILE" 2>&1; then
    echo "✅ Bead $BEAD_ID passes CUE schema validation"
    exit 0
else
    echo "❌ Bead $BEAD_ID fails CUE schema validation"
    exit 1
fi
