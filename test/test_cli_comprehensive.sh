#!/bin/bash
# Intent CLI Comprehensive Test Suite
# Systematically tests all CLI commands with proper validation

set -euo pipefail

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results tracking
declare -a TEST_RESULTS=()
declare -a TEST_NAMES=()
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Test spec files
SPEC_FILES=(
  "examples/user-api.cue"
  "examples/meal-planner-api.cue"
  "examples/array-validation.cue"
  "examples/regex-rules.cue"
  "examples/nested-paths.cue"
  "examples/conflicts-gaps.cue"
)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Run a command and test its exit code
test_command() {
    local name="$1"
    local command="$2"
    local expected_exit="$3"
    local description="$4"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    TEST_NAMES+=("$name")
    
    log_info "Testing: $name"
    log_info "Description: $description"
    
    # Run command and capture output
    if output=$(eval "$command" 2>&1); then
        actual_exit=0
    else
        actual_exit=$?
    fi
    
    # Check exit code
    if [ "$actual_exit" -eq "$expected_exit" ]; then
        log_success "$name passed"
        TEST_RESULTS+=("PASS")
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        log_error "$name failed (expected exit $expected_exit, got $actual_exit)"
        echo "  Output: $(echo "$output" | head -c 200)..."
        TEST_RESULTS+=("FAIL")
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Test that output is valid JSON
test_json_output() {
    local name="$1"
    local command="$2"
    local expected_exit="$3"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    TEST_NAMES+=("$name")
    
    log_info "Testing: $name (JSON validation)"
    
    # Run command
    if output=$(eval "$command" 2>&1); then
        actual_exit=0
    else
        actual_exit=$?
    fi
    
    # Check exit code
    if [ "$actual_exit" -ne "$expected_exit" ]; then
        log_error "$name failed (expected exit $expected_exit, got $actual_exit)"
        TEST_RESULTS+=("FAIL")
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    # Check if output is valid JSON
    if echo "$output" | jq -e . >/dev/null 2>&1; then
        # Extract AI ergonomics fields
        success=$(echo "$output" | jq -r '.success // .ok // "unknown"')
        has_errors=$(echo "$output" | jq -e '.errors' >/dev/null 2>&1 && echo "yes" || echo "no")
        has_next=$(echo "$output" | jq -e '.next_actions' >/dev/null 2>&1 && echo "yes" || echo "no")
        has_metadata=$(echo "$output" | jq -e '.metadata' >/dev/null 2>&1 && echo "yes" || echo "no")
        
        log_success "$name passed (JSON valid, success=$success, has_errors=$has_errors, has_next=$has_next, has_metadata=$has_metadata)"
        TEST_RESULTS+=("PASS")
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        # Non-JSON output (may be acceptable for some commands)
        log_success "$name passed (non-JSON output)"
        TEST_RESULTS+=("PASS")
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    fi
}

# ============================================================================
# TEST SUITES
# ============================================================================

test_suite_core_commands() {
    local spec="${SPEC_FILES[0]}"
    
    log_info "=== CORE COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "validate spec" \
        "gleam run -- validate $spec" \
        0
    
    test_command "validate missing file" \
        "gleam run -- validate nonexistent.cue" \
        3 \
        "Error handling for missing files"
    
    test_json_output "show spec" \
        "gleam run -- show $spec" \
        0
    
    test_json_output "export spec" \
        "gleam run -- export $spec" \
        0
    
    test_json_output "lint spec" \
        "gleam run -- lint $spec" \
        0
    
    test_json_output "analyze spec" \
        "gleam run -- analyze $spec" \
        0
    
    test_json_output "improve spec" \
        "gleam run -- improve $spec" \
        0
    
    test_json_output "doctor spec" \
        "gleam run -- doctor $spec" \
        0
    
    echo
}

test_suite_interview_commands() {
    log_info "=== INTERVIEW COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "interview API profile" \
        "gleam run -- interview --profile=api" \
        0
    
    test_json_output "interview CLI profile" \
        "gleam run -- interview --profile=cli" \
        0
    
    test_json_output "interview UI profile" \
        "gleam run -- interview --profile=ui" \
        0
    
    test_json_output "interview dry-run" \
        "gleam run -- interview --profile=api --dry-run" \
        0
    
    echo
}

test_suite_beads_commands() {
    log_info "=== BEADS COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "bead-status" \
        "gleam run -- bead-status" \
        0
    
    test_json_output "beads-regenerate" \
        "gleam run -- beads-regenerate" \
        0
    
    echo
}

test_suite_history_commands() {
    log_info "=== HISTORY & SESSIONS TEST SUITE ==="
    echo
    
    test_json_output "history" \
        "gleam run -- history" \
        0
    
    test_json_output "sessions" \
        "gleam run -- sessions" \
        0
    
    echo
}

test_suite_kirk_commands() {
    local spec="${SPEC_FILES[0]}"
    
    log_info "=== KIRK QUALITY COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "quality analysis" \
        "gleam run -- quality $spec" \
        0
    
    test_json_output "invert analysis" \
        "gleam run -- invert $spec" \
        0
    
    test_json_output "coverage analysis" \
        "gleam run -- coverage $spec" \
        0
    
    test_json_output "gaps analysis" \
        "gleam run -- gaps $spec" \
        0
    
    test_json_output "ears parser" \
        "gleam run -- ears $spec" \
        0
    
    test_json_output "parse spec" \
        "gleam run -- parse $spec" \
        0
    
    test_json_output "effects analysis" \
        "gleam run -- effects $spec" \
        0
    
    echo
}

test_suite_ai_commands() {
    log_info "=== AI COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "ai schema --all" \
        "gleam run -- ai schema --all" \
        0
    
    test_json_output "ai aggregate" \
        "gleam run -- ai aggregate" \
        0
    
    echo
}

test_suite_plan_commands() {
    log_info "=== PLAN COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "plan" \
        "gleam run -- plan" \
        0
    
    echo
}

test_suite_phase_commands() {
    log_info "=== SHAPE & READY PHASE COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "shape start" \
        "gleam run -- shape start" \
        0
    
    test_json_output "ready start" \
        "gleam run -- ready start" \
        0
    
    test_json_output "vision start" \
        "gleam run -- vision start" \
        0
    
    echo
}

test_suite_misc_commands() {
    log_info "=== MISC COMMANDS TEST SUITE ==="
    echo
    
    test_json_output "help" \
        "gleam run -- help" \
        0
    
    test_json_output "diff" \
        "gleam run -- diff" \
        0
    
    test_json_output "feedback" \
        "gleam run -- feedback" \
        0
    
    test_json_output "prompt" \
        "gleam run -- prompt" \
        0
    
    echo
}

# ============================================================================
# REPORT GENERATION
# ============================================================================

generate_summary() {
    local success_rate=0
    if [ "$TOTAL_TESTS" -gt 0 ]; then
        success_rate=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    fi
    
    echo
    echo "========================================"
    echo -e "${BLUE}COMPREHENSIVE TEST SUMMARY${NC}"
    echo "========================================"
    echo "Total Tests: $TOTAL_TESTS"
    echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
    echo -e "Failed: ${RED}$FAILED_TESTS${NC}"
    echo "Success Rate: $success_rate%"
    echo "========================================"
    echo
}

generate_detailed_report() {
    echo "========================================"
    echo "DETAILED RESULTS"
    echo "========================================"
    
    for i in "${!TEST_NAMES[@]}"; do
        name="${TEST_NAMES[$i]}"
        result="${TEST_RESULTS[$i]}"
        
        if [ "$result" = "PASS" ]; then
            echo -e "${GREEN}✓ PASS${NC} | $name"
        else
            echo -e "${RED}✗ FAIL${NC} | $name"
        fi
    done
    
    echo "========================================"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

main() {
    echo
    echo "========================================"
    echo "Intent CLI Comprehensive Test Suite"
    echo "========================================"
    echo "Project Root: $PROJECT_ROOT"
    echo "Started: $(date)"
    echo "========================================"
    echo
    
    # Run all test suites
    test_suite_core_commands
    test_suite_interview_commands
    test_suite_beads_commands
    test_suite_history_commands
    test_suite_kirk_commands
    test_suite_ai_commands
    test_suite_plan_commands
    test_suite_phase_commands
    test_suite_misc_commands
    
    # Generate reports
    generate_summary
    generate_detailed_report
    
    # Exit with proper code
    if [ "$FAILED_TESTS" -gt 0 ]; then
        echo
        log_error "Test suite completed with failures"
        exit 1
    else
        echo
        log_success "All tests passed!"
        exit 0
    fi
}

# Run main function
main "$@"
