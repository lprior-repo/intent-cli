#!/usr/bin/env bash
set -euo pipefail

cd /home/lewis/src/intent-cli

echo "=== PHASE 1: Fix Open Beads (TDD15) ==="
ralph --prompt-file .ralph/phase1-fix-bugs.md --no-commit

echo ""
echo "=== PHASE 2: Red Queen + Product Owner Review ==="
ralph --prompt-file .ralph/phase2-review.md --no-commit

echo ""
echo "=== PHASE 3: Close Review Beads (TDD15) ==="
ralph --prompt-file .ralph/phase3-close-review-beads.md --no-commit

echo ""
echo "=== ALL PHASES COMPLETE ==="
echo "Running final sync..."
bd sync
git push
echo "Done."
