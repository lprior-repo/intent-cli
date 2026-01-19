#!/usr/bin/env nu

# ═══════════════════════════════════════════════════════════════════════════
# Bead Queue Manager - AI-Driven Interface
# ═══════════════════════════════════════════════════════════════════════════
# This script is designed to be called by Claude (AI orchestrator).
# It manages:
#   • Work queue enumeration from bd (beads daemon)
#   • Workspace lifecycle (creation/cleanup) via zjj
#   • Idempotency tracking
#   • Result recording
#
# Claude will:
#   1. Call this script to get the work queue
#   2. For each bead: invoke /tdd15 skill
#   3. Call this script to record results
#
# Usage:
#   nu scripts/process-beads.nu list [--json]           # Get pending beads
#   nu scripts/process-beads.nu create-workspace <id>   # Create isolated workspace
#   nu scripts/process-beads.nu record <id> <status>    # Record completion
#   nu scripts/process-beads.nu cleanup <id>            # Remove workspace
#   nu scripts/process-beads.nu validate-integration    # Run validation suite
# ═══════════════════════════════════════════════════════════════════════════

const PROCESSED_LOG = "./.beads_processed.jsonl"
const SESSION_LOG = "./.bead_session.jsonl"
const LOG_DIR = "./.bead_logs"
const PROJECT_ROOT = "/home/lewis/src/intent-cli"

def main [command?: string, arg1?: string, arg2?: string] {
    if ($command == null or ($command | is-empty)) {
        print_usage
        exit 1
    }

    match $command {
        "list" => { cmd_list_beads }
        "create-workspace" => { cmd_create_workspace $arg1 }
        "record" => { cmd_record_result $arg1 $arg2 }
        "cleanup" => { cmd_cleanup_workspace $arg1 }
        "validate-integration" => { cmd_validate_integration }
        "status" => { cmd_status }
        _ => {
            print $"Unknown command: ($command)"
            print_usage
            exit 1
        }
    }
}

# ═══════════════════════════════════════════════════════════════════════════
# COMMAND: list - Get pending beads
# ═══════════════════════════════════════════════════════════════════════════

def cmd_list_beads [] {
    let all_beads = (load_all_beads)
    let processed = (load_processed_beads)
    let processed_ids = ($processed | each {|p| $p.id})
    let pending = ($all_beads | where {|b| $b.id not-in $processed_ids})

    # Output as JSON for AI parsing
    $pending | to json
}

# ═══════════════════════════════════════════════════════════════════════════
# COMMAND: create-workspace - Create isolated JJ workspace
# ═══════════════════════════════════════════════════════════════════════════

def cmd_create_workspace [bead_id: string] {
    if ($bead_id | is-empty) {
        print "❌ bead_id required"
        exit 1
    }

    let space_name = $"bead-($bead_id)"

    # Create workspace via zjj
    let result = (
        do {
            ^zjj add $space_name --bead $bead_id --no-open | complete
        }
    )

    if $result.exit_code != 0 {
        print $"{{\"success\": false, \"error\": ($result.stdout | to json)}}"
        exit 1
    }

    print $"{{\"success\": true, \"bead_id\": \"($bead_id)\", \"workspace\": \"($space_name)\"}}"
}

# ═══════════════════════════════════════════════════════════════════════════
# COMMAND: record - Record bead completion
# ═══════════════════════════════════════════════════════════════════════════

def cmd_record_result [bead_id: string, status: string] {
    if ($bead_id | is-empty) or ($status | is-empty) {
        print "❌ bead_id and status required"
        exit 1
    }

    let timestamp = (date now | format date "%Y-%m-%dT%H:%M:%SZ")
    let entry = {
        id: $bead_id
        status: $status
        timestamp: $timestamp
    }

    # Append to processed log
    let json_entry = ($entry | to json)
    $json_entry | save --append $PROCESSED_LOG

    print $"{\"success\": true, \"recorded\": ($entry | to json)}"
}

# ═══════════════════════════════════════════════════════════════════════════
# COMMAND: cleanup - Remove workspace
# ═══════════════════════════════════════════════════════════════════════════

def cmd_cleanup_workspace [bead_id: string] {
    if ($bead_id | is-empty) {
        print "❌ bead_id required"
        exit 1
    }

    let space_name = $"bead-($bead_id)"

    # Remove via zjj
    let result = (
        do {
            ^zjj remove $space_name --force | complete
        }
    )

    if $result.exit_code != 0 {
        print $"{{\"success\": false, \"error\": ($result.stdout | to json)}}"
        exit 1
    }

    print $"{{\"success\": true, \"cleaned\": \"($bead_id)\"}}"
}

# ═══════════════════════════════════════════════════════════════════════════
# COMMAND: validate-integration - Run 4 parallel validators
# ═══════════════════════════════════════════════════════════════════════════

def cmd_validate_integration [] {
    cd $PROJECT_ROOT

    let validators = [
        {name: "gleam build", cmd: "gleam", args: ["build"]}
        {name: "gleam test", cmd: "gleam", args: ["test"]}
        {name: "gleam format", cmd: "gleam", args: ["format", "--check"]}
        {name: "moon check", cmd: "moon", args: ["check"]}
    ]

    let results = ($validators | par-each --threads 4 {|v|
        let result = (
            do {
                timeout 600000 {
                    ^$v.cmd ...$v.args | complete
                }
            }
        )

        {
            validator: $v.name
            success: ($result.exit_code == 0)
            exit_code: $result.exit_code
        }
    })

    let all_passed = ($results | all {|r| $r.success})

    # Output JSON for AI parsing
    {
        all_passed: $all_passed
        results: $results
    } | to json
}

# ═══════════════════════════════════════════════════════════════════════════
# COMMAND: status - Show current state
# ═══════════════════════════════════════════════════════════════════════════

def cmd_status [] {
    let all_beads = (load_all_beads)
    let processed = (load_processed_beads)
    let pending_count = (($all_beads | length) - ($processed | length))

    {
        total_beads: ($all_beads | length)
        processed_beads: ($processed | length)
        pending_beads: $pending_count
    } | to json
}

# ═══════════════════════════════════════════════════════════════════════════
# HELPERS
# ═══════════════════════════════════════════════════════════════════════════

def load_all_beads [] {
    let ready = (
        do {
            let result = (^bd ready --json 2>/dev/null | complete)
            if $result.exit_code == 0 {
                $result.stdout | from json
            } else {
                []
            }
        }
    )

    let in_progress = (
        do {
            let result = (^bd list --status=in_progress --json 2>/dev/null | complete)
            if $result.exit_code == 0 {
                $result.stdout | from json
            } else {
                []
            }
        }
    )

    let combined = ($ready ++ $in_progress)
    let unique_ids = ($combined | each {|b| $b.id} | uniq)
    ($combined | where {|b| $b.id in $unique_ids})
}

def load_processed_beads [] {
    if (($PROCESSED_LOG | path exists)) {
        do {
            try {
                let raw = (open --raw $PROCESSED_LOG)
                let wrapped = if ($raw | str starts-with "[") {
                    $raw
                } else {
                    "[" + ($raw | str replace "}\n{" "},\n{") + "]"
                }
                $wrapped | from json
            } catch {
                []
            }
        }
    } else {
        []
    }
}

def print_usage [] {
    print "Bead Queue Manager - AI Interface"
    print ""
    print "Commands:"
    print "  list                          Get pending beads (JSON)"
    print "  create-workspace <id>         Create isolated workspace"
    print "  record <id> <status>          Record bead completion"
    print "  cleanup <id>                  Remove workspace"
    print "  validate-integration          Run all validators in parallel"
    print "  status                        Show queue status"
}

# ═══════════════════════════════════════════════════════════════════════════

main
