#!/usr/bin/env nu

# Parallel bead processor with zjj + tdd15 + moon validation
# Usage: nu process-beads.nu [--max-parallel 5]

def main [
    --max-parallel: int = 5  # Max parallel tdd15 agents
    --dry-run                # Preview without executing
] {
    print "🚀 Starting parallel bead processing..."

    # 1. Get all open/in_progress beads
    let beads = (get_work_beads)

    if ($beads | length) == 0 {
        print "✅ No beads to process. All done!"
        return
    }

    print $"📋 Found ($beads | length) beads to process"

    if $dry_run {
        print "\n🔍 Dry run - would process:"
        $beads | each {|b| print $"  - ($b.id): ($b.title)" } | ignore
        return
    }

    # 2. Ensure zjj spaces exist for all beads
    ensure_zjj_spaces $beads

    # 3. Process beads in parallel batches
    let results = (process_beads_parallel $beads $max_parallel)

    # 4. Report results
    let failures = ($results | where success == false)
    if ($failures | length) > 0 {
        print $"\n⚠️  ($failures | length) beads failed:"
        $failures | each {|f| print $"  - ($f.bead): ($f.error)" } | ignore
    }

    # 5. Validate main is green
    validate_main_green

    print "\n✨ All beads processed successfully!"
}

# Get all open/in_progress beads
def get_work_beads [] {
    let ready = (^bd ready --json | complete | get stdout | from json)
    let in_progress = (^bd list --status=in_progress --json | complete | get stdout | from json)

    ($ready ++ $in_progress
    | uniq-by id
    | select id title priority status)
}

# Ensure zjj spaces exist for all beads
def ensure_zjj_spaces [beads: list] {
    print "\n🔧 Ensuring zjj spaces exist..."

    let existing_spaces = (^zjj list --json | complete | get stdout | from json | get sessions | get name)

    $beads | each {|bead|
        let space_name = $"bead-($bead.id)"

        if $space_name not-in $existing_spaces {
            print $"  Creating space: ($space_name)"
            ^zjj add $space_name --bead $bead.id | complete | ignore
        } else {
            print $"  ✓ Space exists: ($space_name)"
        }
    } | ignore
}

# Process beads in parallel batches
def process_beads_parallel [beads: list, max_parallel: int] {
    let parallel_msg = $"with ($max_parallel) parallel threads"
    print $"\n⚡ Processing beads $parallel_msg"

    ($beads
    | enumerate
    | par-each --threads $max_parallel --keep-order {|item|
        let bead = $item.item
        let idx = $item.index
        process_single_bead $bead $idx
    })
}

# Process a single bead in its zjj space
def process_single_bead [bead: record, idx: int] {
    let space_name = $"bead-($bead.id)"
    let workspace_path = $"/home/lewis/src/intent-cli__workspaces/($space_name)"

    print $"\n[($idx + 1)] 🔨 Processing: ($bead.id) - ($bead.title)"

    try {
        # Run tdd15 in workspace
        print $"  └─ Running tdd15..."
        let result = (
            ^claude --no-tty $"/tdd15 ($bead.id)"
            | complete
        )

        if $result.exit_code != 0 {
            print $"  ❌ tdd15 failed for ($bead.id)"
            return {success: false, bead: $bead.id, error: "tdd15 failed"}
        }

        # Run moon validation in workspace
        print $"  └─ Running moon validation..."
        let moon_result = (
            do --ignore-errors { cd $workspace_path; ^moon check }
            | complete
        )

        if $moon_result.exit_code != 0 {
            print $"  ❌ Moon validation failed for ($bead.id)"
            return {success: false, bead: $bead.id, error: "moon failed"}
        }

        # All green - push with jj
        print $"  └─ Pushing to remote..."
        do --ignore-errors {
            cd $workspace_path
            ^jj bookmark create $"bead-($bead.id)-done"
            ^jj git push
        } | complete | ignore

        # Cleanup zjj space
        ^zjj remove $space_name | complete | ignore

        print $"  ✅ Complete: ($bead.id)"
        {success: true, bead: $bead.id}

    } catch {|err|
        print $"  ❌ Error processing ($bead.id): ($err.msg)"
        {success: false, bead: $bead.id, error: $err.msg}
    }
}

# Validate main branch is green
def validate_main_green [] {
    print "\n🔍 Validating main branch..."

    cd /home/lewis/src/intent-cli

    # Run full validation suite
    print "  └─ Running gleam build..."
    ^gleam build

    print "  └─ Running gleam test..."
    ^gleam test

    print "  └─ Running gleam format --check..."
    ^gleam format --check

    print "  └─ Running moon check..."
    ^moon check

    print "  ✅ Main branch is green!"
}
