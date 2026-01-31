#!/usr/bin/env bash
# Enhanced bead creation that validates against template
# Usage: bd-create-enhanced.sh --title "Component: Description" --description-file <path>
#
# This wrapper:
# 1. Validates the description file has all 10 sections
# 2. Creates the bead via bd create
# 3. Validates the created bead

set -euo pipefail

TITLE=""
DESC_FILE=""
TYPE="feature"
PRIORITY="2"
LABELS=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --title)
            TITLE="$2"
            shift 2
            ;;
        --description-file)
            DESC_FILE="$2"
            shift 2
            ;;
        --type)
            TYPE="$2"
            shift 2
            ;;
        --priority)
            PRIORITY="$2"
            shift 2
            ;;
        --labels)
            LABELS="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Validate required arguments
if [[ -z "$TITLE" || -z "$DESC_FILE" ]]; then
    echo "Usage: bd-create-enhanced.sh --title 'Component: Description' --description-file <path>"
    echo ""
    echo "Options:"
    echo "  --title           Bead title (required, format: 'Component: Description')"
    echo "  --description-file  Path to description file (required)"
    echo "  --type            Issue type: feature|bug|task|epic|chore (default: feature)"
    echo "  --priority        Priority: 0-4 (default: 2)"
    echo "  --labels          Comma-separated labels"
    exit 1
fi

# Validate title format
if ! echo "$TITLE" | grep -qE '^[A-Za-z0-9_-]+: .+'; then
    echo "ERROR: Title must be in format 'Component: Description'"
    echo "       Got: $TITLE"
    exit 1
fi

# Validate description file exists
if [[ ! -f "$DESC_FILE" ]]; then
    echo "ERROR: Description file not found: $DESC_FILE"
    exit 1
fi

DESC=$(cat "$DESC_FILE")

# Validate all 10 sections are present
MISSING_SECTIONS=()

check_section() {
    local pattern="$1"
    local name="$2"
    if ! echo "$DESC" | grep -qi "$pattern"; then
        MISSING_SECTIONS+=("$name")
    fi
}

check_section "ears_requirements\|THE SYSTEM SHALL" "EARS Requirements (Section 1)"
check_section "contracts\|preconditions\|postconditions" "KIRK Contracts (Section 2)"
check_section "inversions\|THE SYSTEM SHALL NOT" "Inversion Analysis (Section 3)"
check_section "acceptance_tests\|happy_paths" "ATDD Tests (Section 4)"
check_section "e2e_tests\|pipeline_test" "E2E Tests (Section 5)"
check_section "implementation_tasks\|phase_1" "Implementation Tasks (Section 6)"
check_section "failure_modes\|symptom" "Failure Modes (Section 7)"
check_section "completion_checklist\|\[ \]" "Completion Checklist (Section 8)"
check_section "context\|related_files" "Context (Section 9)"
check_section "ai_hints\|do:" "AI Hints (Section 10)"

if [[ ${#MISSING_SECTIONS[@]} -gt 0 ]]; then
    echo "❌ REJECTED: Description is missing ${#MISSING_SECTIONS[@]} required sections:"
    for section in "${MISSING_SECTIONS[@]}"; do
        echo "   - $section"
    done
    echo ""
    echo "See .beads/BEAD_TEMPLATE.md for required format"
    exit 1
fi

echo "✅ Description validates - all 10 sections present"

# Create the bead
CMD="bd create --type $TYPE --priority $PRIORITY --title \"$TITLE\" --description \"\$(cat \"$DESC_FILE\")\""
if [[ -n "$LABELS" ]]; then
    CMD="$CMD --labels \"$LABELS\""
fi

echo "Creating bead..."
eval "$CMD"

echo "✅ Enhanced bead created successfully"
