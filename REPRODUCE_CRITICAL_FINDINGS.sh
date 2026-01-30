#!/usr/bin/env bash
# Red Queen Critical Findings - Reproduction Script

cd "$(dirname "$0")"
echo "=== Red Queen Critical Findings ==="
echo ""

echo "[TEST 1] Symlink Attack"
cp .intent/sessions.jsonl .intent/sessions.jsonl.bak 2>/dev/null || true
rm -f .intent/sessions.jsonl
ln -s /etc/passwd .intent/sessions.jsonl
if gleam run -m intent sessions 2>&1 | grep -q '"success":true'; then
    echo "✗ VULNERABLE: Followed symlink"
else
    echo "✓ FIXED: Rejected symlink"
fi
rm .intent/sessions.jsonl
mv .intent/sessions.jsonl.bak .intent/sessions.jsonl 2>/dev/null || true

echo ""
echo "[TEST 2] Silent Data Loss"
cp .intent/sessions.jsonl .intent/sessions.jsonl.bak
head -c 500 .intent/sessions.jsonl > .intent/sessions.jsonl.tmp
mv .intent/sessions.jsonl.tmp .intent/sessions.jsonl
if gleam run -m intent sessions 2>&1 | grep -q '"total":0'; then
    echo "✗ VULNERABLE: Silent data loss (returned 0 sessions)"
else
    echo "✓ FIXED: Detected corruption"
fi
mv .intent/sessions.jsonl.bak .intent/sessions.jsonl

echo ""
echo "Issues: intent-cli-83rb, intent-cli-4c5t, intent-cli-pn1w"
