# Justfile for Intent CLI development tasks
# https://github.com/casey/just

# Default recipe to display help
default:
    @just --list

# Run full CI pipeline (format, build, test)
ci:
    gleam format --check
    gleam build
    gleam test

# Format code
format:
    gleam format

# Build the project
build:
    gleam build

# Run tests
test:
    gleam test

# Build and install binary to ~/.local/bin
install:
    gleam build
    mkdir -p ~/.local/bin
    cp -f build/dev/erlang/*/ebin/intent ~/.local/bin/ 2>/dev/null || \
    gleam run -m gleescript && cp -f dist/intent/intent ~/.local/bin/
    chmod +x ~/.local/bin/intent
    @echo "✓ Installed to ~/.local/bin/intent"

# Generate changelog using git-cliff
changelog tag="":
    #!/usr/bin/env bash
    set -euo pipefail
    if ! command -v git-cliff &> /dev/null; then
        echo "Error: git-cliff is not installed"
        echo "Install with: cargo install git-cliff"
        echo "Or: brew install git-cliff"
        echo "Or: https://git-cliff.org/docs/installation"
        exit 1
    fi
    if [ -z "{{ tag }}" ]; then
        git-cliff --unreleased --tag "Unreleased" -o CHANGELOG.md
    else
        git-cliff --tag "{{ tag }}" -o CHANGELOG.md
    fi
    echo "✓ Updated CHANGELOG.md"

# Generate changelog for a specific version range
changelog-range from="" to="HEAD":
    #!/usr/bin/env bash
    set -euo pipefail
    if ! command -v git-cliff &> /dev/null; then
        echo "Error: git-cliff is not installed"
        exit 1
    fi
    if [ -z "{{ from }}" ]; then
        git-cliff -o CHANGELOG.md
    else
        git-cliff {{ from }}..{{ to }} -o CHANGELOG.md
    fi
    echo "✓ Updated CHANGELOG.md"

# Preview changelog without writing to file
changelog-preview tag="":
    #!/usr/bin/env bash
    set -euo pipefail
    if ! command -v git-cliff &> /dev/null; then
        echo "Error: git-cliff is not installed"
        exit 1
    fi
    if [ -z "{{ tag }}" ]; then
        git-cliff --unreleased --tag "Unreleased"
    else
        git-cliff --tag "{{ tag }}"
    fi

# Update changelog for a new release
release version:
    @echo "Generating changelog for version {{ version }}..."
    @just changelog "v{{ version }}"
    @echo "Changelog updated. Review CHANGELOG.md before committing."
    @echo ""
    @echo "Next steps:"
    @echo "  1. Review CHANGELOG.md"
    @echo "  2. Update version in gleam.toml to {{ version }}"
    @echo "  3. git add CHANGELOG.md gleam.toml"
    @echo "  4. git commit -m 'chore(release): prepare for v{{ version }}'"
    @echo "  5. git tag -a v{{ version }} -m 'Release v{{ version }}'"
    @echo "  6. git push && git push --tags"

# Clean build artifacts
clean:
    rm -rf build/
    @echo "✓ Cleaned build artifacts"

# Run the CLI with arguments
run *args:
    gleam run -- {{ args }}

# Check for common issues
check:
    @echo "Checking format..."
    gleam format --check
    @echo "Building..."
    gleam build
    @echo "Running tests..."
    gleam test
    @echo "✓ All checks passed"
