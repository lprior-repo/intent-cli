# Changelog

All notable changes to Intent CLI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Planned
- Additional interview profiles
- Enhanced validation rules
- Export to other issue trackers
- Web UI for interviews

## [0.1.0] - 2025-02-09

### Major Changes from v2.0

This release represents a complete architectural shift from **contract-driven HTTP API testing** to **planning and work item generation**.

### Breaking Changes

- **Removed**: All HTTP execution infrastructure (client, runner, request/response handling)
- **Removed**: Response validation and checking system
- **Removed**: API quality analysis (KIRK modules for HTTP testing)
- **Removed**: Variable interpolation and capture system
- **Removed**: Real-time dependency resolution between behaviors
- **Changed**: Spec format from HTTP-focused to declarative behaviors
- **Changed**: `rules` renamed to `invariants` with different semantics
- **Changed**: Behaviors now use `preconditions`/`postconditions` instead of `request`/`response`

### Added

#### Round 1: Core Infrastructure
- Interactive interview system for requirement gathering
- Interview session management and storage
- Bead (work item) template generation
- Integration with br (beads_rust) issue tracker
- Semantic validation of specifications
- Declarative behavior specification format

#### Round 2: Plan Commands
- `plan` command - Generate plans from context
- `plan-next` command - Suggest next task with multiple strategies
- `vision` command - Generate vision documents
- `ready` command - Generate ready documents

#### Round 3: Plan Management
- `plan-approve` command - Approve generated plans
- `bead-status` command - Check bead status in br
- `beads-regenerate` command - Regenerate beads with updated templates

#### Round 4: Quality and History
- `history` command - List all interview sessions
- `sessions` command - Filter sessions by profile
- `diff` command - Show changes between session snapshots
- Quality analyzer for specification completeness
- Improved error handling throughout

#### Round 5: Documentation and Cleanup
- Comprehensive spec format documentation
- Schema documentation for all types
- Migration guide from v2.0 to v3.0
- User guide with examples and best practices
- Removed deprecated `check` command

#### Round 6: Final Polish
- Effects analysis for second-order impacts
- Anti-patterns detection
- AI hints for implementation guidance
- Test suite restoration (543 tests passing)
- Documentation updates for current state

### Changed

- **Specification Format**: Behaviors are now declarative with preconditions, postconditions, and verifications
- **Invariants**: Global rules renamed to invariants with different semantics
- **Validation**: Semantic validation instead of HTTP response validation
- **Workflow**: Interview-driven instead of spec-file-driven

### Fixed

- All compiler warnings resolved
- Pre-existing test failures fixed
- Semantic validation bugs resolved
- Memory issues in large test suites fixed

### Performance

- Optimized interview session storage
- Improved bead generation performance
- Enhanced effects analysis speed

### Documentation

- Complete user guide (USER_GUIDE.md)
- Specification format reference (SPEC_FORMAT.md)
- Schema documentation for all types
- Migration guide from v2.0 (MIGRATION.md)
- Updated README with current functionality
- This CHANGELOG

### Deprecated

- HTTP testing functionality (use dedicated API testing tools instead)
- CUE spec file parsing for HTTP validation
- Response checking and validation rules

### Removed

- `intent check` command - HTTP testing
- `intent coverage` command - HTTP coverage analysis
- `intent gaps` command - HTTP gap detection
- `intent analyze` command - API quality analysis
- All HTTP client and runner code
- All response validation infrastructure

### Security

- Password hashing guidance in AI hints
- Security best practices in anti-patterns
- Input validation examples

### Testing

- 543 tests passing
- Test suite migrated from v2.0 HTTP to v3.0 declarative
- Comprehensive semantic validation tests
- Interview system tests
- Bead generation tests

## [2.0.0] - 2024-XX-XX (Previous HTTP Testing Version)

### Added
- Contract-driven HTTP API testing
- Request/response validation
- Variable interpolation
- Coverage analysis
- Gap detection
- Quality metrics

### Removed (in v3.0)
See v3.0 breaking changes above

---

## Migration Notes

If you're migrating from v2.0, see [MIGRATION.md](MIGRATION.md) for detailed migration instructions.

Key changes:
- HTTP testing → Planning and specification
- Request/Response → Preconditions/Postconditions
- Response checks → Verifications
- Global rules → Invariants
- Test execution → Bead generation

---

[Unreleased]: https://github.com/your-org/intent-cli/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/your-org/intent-cli/releases/tag/v0.1.0
