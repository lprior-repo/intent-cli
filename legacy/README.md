# Deprecated HTTP Testing Format

These files use the v2.0 HTTP testing format that has been deprecated.
See MIGRATION.md for v3.0 migration guide.

**DO NOT** use these for new specifications.

Last updated: 2026-02-09
Status: Legacy - Preserved for reference only

## Files in This Directory

### kirk.cue
KIRK (Knowledge-Informed Requirements & Kontract) schema extensions for v2.0.
Included mental models, quality metrics, coverage analysis, and gap detection.

### kirk.proto
Protocol Buffers definition for KIRK specification format.

### intent-kirk.cue
Self-validation spec for the KIRK system (dogfooding).
Used KIRK to validate its own implementation.

### intent-self.cue
Meta-specification file - Intent CLI's own spec in the old HTTP format.

## Migration Notes

The v3.0 format has removed all HTTP-specific fields:
- `request` (method, path, headers, query, body)
- `response` (status, example, checks)
- `config` (base_url, timeout_ms, headers)
- `captures` (variable extraction)
- `rules` (global validation rules)

Behaviors in v3.0 are now declarative and focus on:
- `preconditions` - What must be true before
- `postconditions` - What must be true after
- `verifications` - How to verify the behavior works

See MIGRATION.md for complete migration guide.
