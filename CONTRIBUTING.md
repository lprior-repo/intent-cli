# Contributing to Intent CLI

Thank you for your interest in contributing! This guide covers development setup, coding standards, and the contribution process.

## Quick Start

```bash
# Clone the repository
git clone https://github.com/lprior-repo/intent-cli.git
cd intent-cli

# Build and test
gleam build
gleam test

# Or use just
just build
just test
```

## Development Tools

- **Gleam 1.14.0+** - Primary language
- **Erlang/OTP 27+** - Runtime
- **git-cliff** - Changelog generation (optional)
- **just** - Task runner (optional)
- **bd** - Issue tracking

Install git-cliff and just:
```bash
cargo install git-cliff just
# or: brew install git-cliff just
```

## Coding Standards

Intent CLI follows strict functional programming principles. See the 7 Gleam Commandments:

1. **Explicitness** - No implicit conversions
2. **Immutability** - All values are immutable
3. **Type-First** - Define types before logic
4. **Pattern Matching** - Prefer `case` over `if`
5. **Pipelines** - Use `|>` operator
6. **Railway Errors** - Never panic, always return `Result`
7. **Naming** - snake_case for functions, PascalCase for types

## Development Workflow

### Using Beads for Issues

```bash
# Find work
bd ready

# Claim an issue
bd update <id> --status in_progress

# When done
bd close <id> --reason "Implemented feature X"
```

### Making Changes

```bash
# 1. Create a branch
git checkout -b feature/your-feature

# 2. Write code + tests
vim src/intent/your_module.gleam
vim test/your_module_test.gleam

# 3. Format and test
gleam format
gleam test

# 4. Commit with conventional format
git commit -m "feat(module): add new feature"
```

## Commit Message Format

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <description>

Types:
  feat     - New feature
  fix      - Bug fix
  docs     - Documentation
  test     - Tests
  refactor - Code refactoring
  chore    - Maintenance
  perf     - Performance
  build    - Build system
```

Examples:
```bash
git commit -m "feat(interview): add session resumption"
git commit -m "fix(parser): handle empty CUE blocks"
git commit -m "docs(readme): update installation instructions"
```

## Testing

```bash
# Run all tests
gleam test

# Or with just
just test
```

Tests mirror the source structure:
```
src/intent/interview.gleam → test/interview_test.gleam
```

Guidelines:
- Keep tests fast (< 5 seconds total)
- One assertion per test
- Test error cases
- Use descriptive names

## Changelog Management

We use automated changelog generation with git-cliff.

### Preview Changes

```bash
just changelog-preview
```

### Generate Changelog

```bash
# For unreleased changes
just changelog

# For a specific version
just changelog v0.2.0
```

### Release Process

```bash
just release 0.2.0
```

This will:
1. Generate changelog
2. Show next steps for tagging and pushing

For full release documentation, see [docs/RELEASE_PROCESS.md](docs/RELEASE_PROCESS.md).

## Pull Request Process

1. **Ensure code quality:**
   ```bash
   gleam format --check
   gleam build
   gleam test
   ```

2. **Push your branch:**
   ```bash
   git push origin your-branch
   ```

3. **Create PR with:**
   - Clear title describing the change
   - Reference to bead ID (e.g., "Closes intent-cli-abc123")
   - Description of changes and why
   - Note any breaking changes

## Project Structure

```
src/intent/
├── interview.gleam           # Interview engine
├── kirk/                     # KIRK analysis
│   ├── quality_analyzer.gleam
│   ├── inversion_checker.gleam
│   ├── coverage_analyzer.gleam
│   ├── gap_detector.gleam
│   └── effects_analyzer.gleam
├── checker.gleam             # API testing
├── runner.gleam              # Test execution
└── types.gleam               # Core types
```

## Documentation

- Module docs: `////` at top of file
- Function docs: `///` before `pub fn`
- Inline comments: `//` for explanations

Always document:
- All public APIs
- Complex algorithms
- Why, not what

## Getting Help

- Check `docs/` directory
- Review `examples/` directory
- Use `bd list` for issues
- Open GitHub discussions

## License

By contributing, you agree that your contributions will be licensed under Apache 2.0.

---

Thank you for contributing to Intent CLI!
