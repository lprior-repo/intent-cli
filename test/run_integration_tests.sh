#!/bin/bash
set -euo pipefail

# Intent CLI Integration Test Suite
# Tests all 33 CLI commands systematically
#
# Usage: ./run_integration_tests.sh [options]
#   --spec-file <path>    Use specific spec file for tests (default: examples/user-api.cue)
#   --category <name>     Run only specific category of tests
#   --verbose             Show detailed output
#   --no-color            Disable colored output
#   --help                Show this help message

# ============================================================================
# CONFIGURATION
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Default spec file for testing
DEFAULT_SPEC="${PROJECT_DIR}/examples/user-api.cue"
SPEC_FILE="${DEFAULT_SPEC}"

# Test categories
declare -a TEST_CATEGORIES=(
	"core_spec"
	"interview"
	"beads"
	"history_sessions"
	"kirk"
	"ai"
	"plan"
	"phase"
	"misc"
)

# Exit codes per AGENTS.md
EXIT_PASS=0
EXIT_FAIL=1
EXIT_INVALID=3
EXIT_ERROR=4

# Test statistics
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# JSON validation checks
declare -a FAILED_JSON_TESTS=()

# Colors
if [[ -t 1 ]] && [[ "${NO_COLOR:-}" != "1" ]]; then
	RED='\033[0;31m'
	GREEN='\033[0;32m'
	YELLOW='\033[1;33m'
	BLUE='\033[0;34m'
	MAGENTA='\033[0;35m'
	CYAN='\033[0;36m'
	BOLD='\033[1m'
	NC='\033[0m'
else
	RED=''
	GREEN=''
	YELLOW=''
	BLUE=''
	MAGENTA=''
	CYAN=''
	BOLD=''
	NC=''
fi

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

log_info() {
	echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
	echo -e "${GREEN}[PASS]${NC} $*"
}

log_error() {
	echo -e "${RED}[FAIL]${NC} $*" >&2
}

log_warning() {
	echo -e "${YELLOW}[WARN]${NC} $*"
}

log_test() {
	echo -e "${CYAN}[TEST]${NC} $*"
}

log_section() {
	echo ""
	echo -e "${BOLD}${MAGENTA}═══ $* ═══${NC}"
	echo ""
}

# Run a command and capture exit code and output
run_command() {
	local cmd="$1"
	local expected_exit="${2:-0}"
	local description="${3:-Command execution}"
	local json_mode="${4:-false}"

	TOTAL_TESTS=$((TOTAL_TESTS + 1))

	log_test "Running: $cmd"

	# Run command and capture output
	local output
	local exit_code
	local raw_output

	raw_output=$(eval "$cmd" 2>&1)
	exit_code=$?

	# Filter out gleam run artifacts and extract JSON (if present)
	if echo "$raw_output" | grep -q '^{'; then
		# Extract JSON from output (get last occurrence of lines starting with {)
		output=$(echo "$raw_output" | grep -E '^\{' | tail -1)
	else
		output="$raw_output"
	fi

	# Check exit code
	if [[ "$exit_code" -eq "$expected_exit" ]]; then
		log_success "Exit code: $exit_code (expected: $expected_exit)"
	else
		log_error "Exit code: $exit_code (expected: $expected_exit)"
		FAILED_TESTS=$((FAILED_TESTS + 1))
		echo -e "${YELLOW}Output:${NC}\n$output"
		return 1
	fi

	# Validate JSON structure if in JSON mode
	if [[ "$json_mode" == "true" ]]; then
		if validate_json_structure "$output"; then
			log_success "JSON structure valid"
		else
			log_error "JSON structure invalid"
			FAILED_TESTS=$((FAILED_TESTS + 1))
			FAILED_JSON_TESTS+=("$cmd")
			echo -e "${YELLOW}Output:${NC}\n$output"
			return 1
		fi
	fi

	PASSED_TESTS=$((PASSED_TESTS + 1))
	echo ""

	return 0
}

# Validate JSON structure follows AI CLI Ergonomics v1.1
validate_json_structure() {
	local json="$1"

	# Check if output is valid JSON
	if ! echo "$json" | jq empty >/dev/null 2>&1; then
		log_error "Output is not valid JSON"
		return 1
	fi

	# Check for required fields based on AI CLI Ergonomics v1.1
	local has_status
	local has_data
	local has_command

	has_status=$(echo "$json" | jq -e '.status' >/dev/null 2>&1 && echo "true" || echo "false")
	has_data=$(echo "$json" | jq -e '.data' >/dev/null 2>&1 && echo "true" || echo "false")
	has_command=$(echo "$json" | jq -e '.command' >/dev/null 2>&1 && echo "true" || echo "false")

	# For success responses, check for success structure
	if echo "$json" | jq -e '.status == "success"' >/dev/null 2>&1; then
		if [[ "$has_data" == "false" ]] && [[ "$has_command" == "false" ]]; then
			log_error "Success response missing required fields (data or command)"
			return 1
		fi
	fi

	# For failure responses, check for error array
	if echo "$json" | jq -e '.status == "failure"' >/dev/null 2>&1; then
		local has_errors
		has_errors=$(echo "$json" | jq -e '.errors' >/dev/null 2>&1 && echo "true" || echo "false")
		if [[ "$has_errors" == "false" ]]; then
			log_error "Failure response missing errors array"
			return 1
		fi
	fi

	return 0
}

# Check if command exists in PATH
command_exists() {
	command -v "$1" >/dev/null 2>&1
}

# ============================================================================
# SETUP AND TEARDOWN
# ============================================================================

setup() {
	log_section "SETUP"

	# Check if we're in the right directory
	if [[ ! -f "${PROJECT_DIR}/gleam.toml" ]]; then
		log_error "Not in valid Intent CLI project directory"
		log_error "Expected: ${PROJECT_DIR}/gleam.toml"
		exit 1
	fi

	# Check if gleam is installed
	if ! command_exists gleam; then
		log_error "Gleam is required to run the Intent CLI"
		log_error "Install from: https://gleam.run/getting-started/install/"
		exit 1
	fi

	# Check if intent command exists in PATH or use gleam run
	if command_exists intent; then
		log_success "Intent CLI found in PATH"
		INTENT_CMD="intent"
	else
		log_warning "Intent CLI not in PATH, using 'gleam run' instead"
		cd "$PROJECT_DIR"
		gleam build >/dev/null 2>&1 || {
			log_error "Failed to build Intent CLI"
			exit 1
		}
		log_success "Intent CLI built successfully"
		INTENT_CMD="gleam run --"
	fi

	# Check if spec file exists
	if [[ ! -f "$SPEC_FILE" ]]; then
		log_error "Spec file not found: $SPEC_FILE"
		exit 1
	fi

	# Check for jq (required for JSON validation)
	if ! command_exists jq; then
		log_error "jq is required for JSON validation"
		log_error "Install with: apt-get install jq (Ubuntu) or brew install jq (macOS)"
		exit 1
	fi

	# Export INTENT_CMD for use in test functions
	export INTENT_CMD

	log_info "Using command: $INTENT_CMD"
	log_info "Using spec file: $SPEC_FILE"
	log_info "Project directory: $PROJECT_DIR"
}

teardown() {
	log_section "TEARDOWN"

	log_info "Test execution completed"
}

# ============================================================================
# TEST CATEGORIES
# ============================================================================

test_core_spec_commands() {
	log_section "CORE SPEC COMMANDS"

	# Test validate
	run_command \
		"$INTENT_CMD validate '$SPEC_FILE'" \
		$EXIT_PASS \
		"Validate spec file syntax and structure" \
		true

	# Test show
	run_command \
		"$INTENT_CMD show '$SPEC_FILE'" \
		$EXIT_PASS \
		"Show parsed spec" \
		true

	# Test export
	run_command \
		"$INTENT_CMD export '$SPEC_FILE'" \
		$EXIT_PASS \
		"Export spec to JSON"

	# Test lint
	run_command \
		"$INTENT_CMD lint '$SPEC_FILE'" \
		$EXIT_PASS \
		"Check for anti-patterns and quality issues" \
		true

	# Test analyze
	run_command \
		"$INTENT_CMD analyze '$SPEC_FILE'" \
		$EXIT_PASS \
		"Analyze spec quality" \
		true

	# Test improve
	run_command \
		"$INTENT_CMD improve '$SPEC_FILE'" \
		$EXIT_PASS \
		"Get improvement suggestions" \
		true

	# Test doctor
	run_command \
		"$INTENT_CMD doctor '$SPEC_FILE'" \
		$EXIT_PASS \
		"Run health check with prioritized improvements" \
		true
}

test_interview_commands() {
	log_section "INTERVIEW COMMANDS"

	# Test interview with different profiles
	local profiles=("api" "cli" "event" "data" "workflow" "ui")

	for profile in "${profiles[@]}"; do
		# Test dry-run mode for each profile
		run_command \
			"$INTENT_CMD interview --profile=$profile --dry-run" \
			$EXIT_PASS \
			"Interview with profile: $profile (dry-run)"
	done

	# Test interview without profile (should use default)
	run_command \
		"$INTENT_CMD interview --dry-run" \
		$EXIT_PASS \
		"Interview with default profile (dry-run)"
}

test_beads_commands() {
	log_section "BEADS COMMANDS"

	# Test bead-status (no session ID should fail with exit_error)
	run_command \
		"$INTENT_CMD bead-status" \
		$EXIT_ERROR \
		"Bead-status without arguments should fail"

	# Test bead-status with missing required flags
	run_command \
		"$INTENT_CMD bead-status --status success" \
		$EXIT_ERROR \
		"Bead-status without bead-id should fail"
}

test_history_sessions_commands() {
	log_section "HISTORY & SESSIONS COMMANDS"

	# Test sessions
	run_command \
		"$INTENT_CMD sessions" \
		$EXIT_PASS \
		"List all interview sessions" \
		true

	# Test history (no session ID should fail)
	run_command \
		"$INTENT_CMD history" \
		$EXIT_ERROR \
		"History without session ID should fail"
}

test_kirk_commands() {
	log_section "KIRK COMMANDS"

	# Test quality
	run_command \
		"$INTENT_CMD quality '$SPEC_FILE'" \
		$EXIT_PASS \
		"KIRK quality analysis" \
		true

	# Test invert
	run_command \
		"$INTENT_CMD invert '$SPEC_FILE'" \
		$EXIT_PASS \
		"KIRK inversion analysis" \
		true

	# Test coverage
	run_command \
		"$INTENT_CMD coverage '$SPEC_FILE'" \
		$EXIT_PASS \
		"KIRK coverage analysis" \
		true

	# Test gaps
	run_command \
		"$INTENT_CMD gaps '$SPEC_FILE'" \
		$EXIT_PASS \
		"KIRK gap detection" \
		true

	# Test ears (needs a requirements file, skip if not exists)
	local req_file="${PROJECT_DIR}/examples/interview-workflow.cue"
	if [[ -f "$req_file" ]]; then
		run_command \
			"$INTENT_CMD ears '$req_file' --output json" \
			$EXIT_PASS \
			"KIRK EARS parser" \
			true
	else
		log_warning "Skipping ears test (no requirements file found)"
		SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
	fi

	# Test parse (needs a requirements file)
	if [[ -f "$req_file" ]]; then
		run_command \
			"$INTENT_CMD parse '$req_file'" \
			$EXIT_PASS \
			"Parse EARS requirements" \
			true
	else
		log_warning "Skipping parse test (no requirements file found)"
		SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
	fi

	# Test effects
	run_command \
		"$INTENT_CMD effects '$SPEC_FILE'" \
		$EXIT_PASS \
		"KIRK second-order effects analysis" \
		true
}

test_ai_commands() {
	log_section "AI COMMANDS"

	# Test ai schema --all
	run_command \
		"$INTENT_CMD ai schema --all" \
		$EXIT_PASS \
		"AI schema introspection (all schemas)" \
		true

	# Test ai schema --list
	run_command \
		"$INTENT_CMD ai schema --list" \
		$EXIT_PASS \
		"AI schema introspection (list commands)" \
		true

	# Test ai aggregate
	local spec2="${PROJECT_DIR}/examples/meal-planner-api.cue"
	if [[ -f "$spec2" ]]; then
		run_command \
			"$INTENT_CMD ai aggregate '$SPEC_FILE' '$spec2'" \
			$EXIT_PASS \
			"AI aggregate multiple specs" \
			true
	else
		log_warning "Skipping ai aggregate (need multiple spec files)"
		SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
	fi
}

test_plan_commands() {
	log_section "PLAN COMMANDS"

	# Test plan (no session ID should fail)
	run_command \
		"$INTENT_CMD plan" \
		$EXIT_ERROR \
		"Plan without session ID should fail"

	# Test plan-approve (no session ID should fail)
	run_command \
		"$INTENT_CMD plan-approve" \
		$EXIT_ERROR \
		"Plan-approve without session ID should fail"
}

test_phase_commands() {
	log_section "PHASE COMMANDS"

	# Test shape start
	run_command \
		"$INTENT_CMD shape start" \
		$EXIT_PASS \
		"Shape phase: start new session" \
		true

	# Test shape check (no session should fail)
	run_command \
		"$INTENT_CMD shape check" \
		$EXIT_ERROR \
		"Shape check without session ID should fail"

	# Test shape critique (no session should fail)
	run_command \
		"$INTENT_CMD shape critique" \
		$EXIT_ERROR \
		"Shape critique without session ID should fail"

	# Test shape respond (no session should fail)
	run_command \
		"$INTENT_CMD shape respond" \
		$EXIT_ERROR \
		"Shape respond without session ID should fail"

	# Test shape agree (no session should fail)
	run_command \
		"$INTENT_CMD shape agree" \
		$EXIT_ERROR \
		"Shape agree without session ID should fail"

	# Test ready start
	run_command \
		"$INTENT_CMD ready start" \
		$EXIT_PASS \
		"Ready phase: start new session" \
		true

	# Test ready check (no session should fail)
	run_command \
		"$INTENT_CMD ready check" \
		$EXIT_ERROR \
		"Ready check without session ID should fail"

	# Test ready critique (no session should fail)
	run_command \
		"$INTENT_CMD ready critique" \
		$EXIT_ERROR \
		"Ready critique without session ID should fail"

	# Test ready respond (no session should fail)
	run_command \
		"$INTENT_CMD ready respond" \
		$EXIT_ERROR \
		"Ready respond without session ID should fail"

	# Test ready agree (no session should fail)
	run_command \
		"$INTENT_CMD ready agree" \
		$EXIT_ERROR \
		"Ready agree without session ID should fail"
}

test_misc_commands() {
	log_section "MISCELLANEOUS COMMANDS"

	# Test help (no args)
	run_command \
		"$INTENT_CMD help" \
		$EXIT_PASS \
		"Help command (no arguments)"

	# Test help (with command)
	run_command \
		"$INTENT_CMD help validate" \
		$EXIT_PASS \
		"Help command (with specific command)"

	# Test diff (needs two specs)
	local spec2="${PROJECT_DIR}/examples/meal-planner-api.cue"
	if [[ -f "$spec2" ]]; then
		run_command \
			"$INTENT_CMD diff '$SPEC_FILE' '$spec2' --json" \
			$EXIT_PASS \
			"Diff two specs" \
			true
	else
		log_warning "Skipping diff test (need two spec files)"
		SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
	fi

	# Test feedback (no results file should fail)
	run_command \
		"$INTENT_CMD feedback" \
		$EXIT_ERROR \
		"Feedback without results file should fail"

	# Test prompt (no session ID should fail)
	run_command \
		"$INTENT_CMD prompt" \
		$EXIT_ERROR \
		"Prompt without session ID should fail"
}

# ============================================================================
# REPORTING
# ============================================================================

print_summary() {
	log_section "TEST SUMMARY"

	echo -e "${BOLD}Total Tests:${NC}     $TOTAL_TESTS"
	echo -e "${GREEN}Passed:${NC}         $PASSED_TESTS"
	echo -e "${RED}Failed:${NC}         $FAILED_TESTS"
	echo -e "${YELLOW}Skipped:${NC}       $SKIPPED_TESTS"
	echo ""

	# Calculate pass rate
	if [[ $TOTAL_TESTS -gt 0 ]]; then
		local pass_rate=$((PASSED_TESTS * 100 / TOTAL_TESTS))
		echo -e "${BOLD}Pass Rate:${NC}      ${pass_rate}%"
		echo ""

		if [[ $FAILED_TESTS -gt 0 ]]; then
			echo -e "${RED}${FAILED_TESTS} test(s) failed${NC}"
			if [[ ${#FAILED_JSON_TESTS[@]} -gt 0 ]]; then
				echo ""
				echo -e "${YELLOW}Tests with JSON validation failures:${NC}"
				for test_cmd in "${FAILED_JSON_TESTS[@]}"; do
					echo "  • $test_cmd"
				done
			fi
		fi
	fi

	echo ""
}

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

show_help() {
	cat <<EOF
Intent CLI Integration Test Suite

Tests all 33 CLI commands systematically with:
  • Exit code validation (per AGENTS.md)
  • JSON structure validation (AI CLI Ergonomics v1.1)
  • Colored output and detailed reports

Usage: $0 [options]

Options:
  --spec-file <path>    Use specific spec file for tests
                        (default: examples/user-api.cue)
  --category <name>     Run only specific category of tests:
                        core_spec, interview, beads, history_sessions,
                        kirk, ai, plan, phase, misc
  --verbose             Show detailed command output
  --no-color            Disable colored output
  --help                Show this help message

Examples:
  $0                                    # Run all tests
  $0 --category kirk                    # Test only KIRK commands
  $0 --spec-file examples/api.cue       # Use specific spec file
  $0 --verbose --no-color               # Verbose, no colors

Exit Codes:
  0 - All tests passed
  1 - One or more tests failed
  2 - Setup or teardown error
EOF
}

main() {
	# Parse arguments
	local run_all_categories=true
	local verbose=false

	while [[ $# -gt 0 ]]; do
		case $1 in
		--spec-file)
			SPEC_FILE="$2"
			shift 2
			;;
		--category)
			run_all_categories=false
			TEST_CATEGORIES=("$2")
			shift 2
			;;
		--verbose)
			verbose=true
			shift
			;;
		--no-color)
			NO_COLOR=1
			export NO_COLOR
			shift
			;;
		--help)
			show_help
			exit 0
			;;
		*)
			log_error "Unknown option: $1"
			show_help
			exit 1
			;;
		esac
	done

	# Print header
	echo -e "${BOLD}${CYAN}"
	echo "╔═══════════════════════════════════════════════════════════════╗"
	echo "║         Intent CLI Integration Test Suite                      ║"
	echo "║         Production-Ready Command Validation                    ║"
	echo "╚═══════════════════════════════════════════════════════════════╝"
	echo -e "${NC}"
	echo ""

	# Setup
	setup

	# Run tests based on category
	if [[ "$run_all_categories" == "true" ]]; then
		for category in "${TEST_CATEGORIES[@]}"; do
			case $category in
			core_spec) test_core_spec_commands ;;
			interview) test_interview_commands ;;
			beads) test_beads_commands ;;
			history_sessions) test_history_sessions_commands ;;
			kirk) test_kirk_commands ;;
			ai) test_ai_commands ;;
			plan) test_plan_commands ;;
			phase) test_phase_commands ;;
			misc) test_misc_commands ;;
			esac
		done
	else
		for category in "${TEST_CATEGORIES[@]}"; do
			case $category in
			core_spec) test_core_spec_commands ;;
			interview) test_interview_commands ;;
			beads) test_beads_commands ;;
			history_sessions) test_history_sessions_commands ;;
			kirk) test_kirk_commands ;;
			ai) test_ai_commands ;;
			plan) test_plan_commands ;;
			phase) test_phase_commands ;;
			misc) test_misc_commands ;;
			*)
				log_error "Unknown category: $category"
				exit 1
				;;
			esac
		done
	fi

	# Teardown
	teardown

	# Print summary
	print_summary

	# Exit with appropriate code
	if [[ $FAILED_TESTS -eq 0 ]]; then
		echo -e "${GREEN}${BOLD}✓ All tests passed!${NC}"
		exit 0
	else
		echo -e "${RED}${BOLD}✗ Some tests failed${NC}"
		exit 1
	fi
}

# Run main
main "$@"
