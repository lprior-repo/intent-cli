# Release Process

This document describes how to create and publish a new release of Intent CLI.

## Prerequisites

Ensure you have the following tools installed:

- **git-cliff** - Automated changelog generation
  ```bash
  cargo install git-cliff
  # or: brew install git-cliff
  ```

- **just** - Command runner
  ```bash
  cargo install just
  # or: brew install just
  ```

- **gleam** - Gleam compiler (1.14.0+)
  ```bash
  # Already installed for development
  gleam --version
  ```

## Release Checklist

### 1. Prepare the Release

**Verify clean state:**
```bash
# Ensure working directory is clean
git status

# Ensure all tests pass
gleam test

# Ensure code is formatted
gleam format --check

# Build successfully
gleam build
```

**Check current version:**
```bash
# Current version is in gleam.toml
grep '^version' gleam.toml
```

### 2. Update Version Number

Determine the new version using [Semantic Versioning](https://semver.org/):

- **MAJOR** (X.0.0) - Breaking changes
- **MINOR** (0.X.0) - New features, backward compatible
- **PATCH** (0.0.X) - Bug fixes, backward compatible

**Update gleam.toml:**
```bash
# Edit gleam.toml manually or use sed
sed -i 's/version = ".*"/version = "0.2.0"/' gleam.toml
```

### 3. Generate Changelog

**Preview changes:**
```bash
# See what will be added to changelog
just changelog-preview v0.2.0
```

**Generate changelog:**
```bash
# Use the release command for guided process
just release 0.2.0

# Or manually generate
just changelog v0.2.0
```

**Review CHANGELOG.md:**
- Check that all notable changes are included
- Verify categorization (Features, Bug Fixes, etc.)
- Edit manually if needed
- Ensure dates are correct
- Add any missing context or notes

### 4. Commit Release Changes

```bash
# Stage the changed files
git add gleam.toml CHANGELOG.md

# Commit with conventional format
git commit -m "chore(release): prepare for v0.2.0"
```

### 5. Create Git Tag

```bash
# Create annotated tag
git tag -a v0.2.0 -m "Release v0.2.0"

# Verify tag
git tag -l -n1 v0.2.0
```

### 6. Push Changes

```bash
# Push commits
git push origin main

# Push tags
git push origin --tags
```

### 7. Create GitHub Release

1. Go to [GitHub Releases](https://github.com/lprior-repo/intent-cli/releases)
2. Click "Draft a new release"
3. Choose the tag you just created (v0.2.0)
4. Use the version as the release title (v0.2.0)
5. Copy the relevant section from CHANGELOG.md as the description
6. Attach any release artifacts if applicable
7. Click "Publish release"

### 8. Verify Release

```bash
# Verify tag is visible
git tag -l

# Verify GitHub release exists
gh release view v0.2.0  # If gh CLI is installed

# Test installation from tag
git clone --branch v0.2.0 https://github.com/lprior-repo/intent-cli.git /tmp/intent-test
cd /tmp/intent-test
gleam build
gleam test
```

### 9. Announce Release

- Update project README.md if needed
- Post in relevant channels/communities
- Update documentation site if applicable

## Release Workflow (Quick Reference)

```bash
# 1. Ensure clean state
git status
gleam test
gleam format --check

# 2. Update version in gleam.toml
vim gleam.toml  # or your editor

# 3. Generate and review changelog
just release 0.2.0

# 4. Review changes
git diff gleam.toml CHANGELOG.md

# 5. Commit and tag
git add gleam.toml CHANGELOG.md
git commit -m "chore(release): prepare for v0.2.0"
git tag -a v0.2.0 -m "Release v0.2.0"

# 6. Push everything
git push origin main
git push origin --tags

# 7. Create GitHub release (via web UI or gh CLI)
```

## Hotfix Release Process

For urgent bug fixes:

1. **Create hotfix branch from tag:**
   ```bash
   git checkout -b hotfix/0.1.1 v0.1.0
   ```

2. **Make the fix:**
   ```bash
   # Fix the bug
   # Add tests
   # Update version in gleam.toml to 0.1.1
   ```

3. **Update changelog:**
   ```bash
   just changelog v0.1.1
   ```

4. **Commit, tag, and push:**
   ```bash
   git commit -am "fix: critical bug in X"
   git tag -a v0.1.1 -m "Hotfix v0.1.1"
   git push origin hotfix/0.1.1
   git push origin --tags
   ```

5. **Merge back to main:**
   ```bash
   git checkout main
   git merge hotfix/0.1.1
   git push origin main
   ```

## Changelog Tips

### Good Commit Messages

```bash
# Good - clear and descriptive
feat(interview): add support for resuming sessions across restarts
fix(parser): handle empty CUE blocks without crashing
docs(api): add examples for KIRK analysis commands

# Bad - too vague
feat: improvements
fix: bug
docs: updates
```

### Manual Changelog Editing

If git-cliff doesn't categorize something correctly:

1. Edit CHANGELOG.md directly
2. Move entries between sections
3. Reword descriptions for clarity
4. Add context or breaking change notes
5. Ensure links to issues/PRs are correct

### Breaking Changes

Always call out breaking changes explicitly:

```markdown
## [2.0.0] - 2026-02-01

### ⚠️ BREAKING CHANGES

- **interview:** Session format changed - old sessions must be migrated
- **api:** Removed deprecated `--legacy` flag
- **config:** Default timeout increased from 5s to 30s

### Features
...
```

## Common Issues

### Changelog Missing Commits

**Problem:** Some commits don't appear in changelog

**Solution:**
- Check `cliff.toml` commit_parsers configuration
- Ensure commits follow conventional format
- Use `just changelog-preview` to see what's included
- Manually add missing entries to CHANGELOG.md

### Wrong Version Order

**Problem:** Versions appear in wrong order in changelog

**Solution:**
- Check that `topo_order = false` in cliff.toml
- Ensure tags follow semantic versioning (vX.Y.Z)
- Regenerate changelog: `just changelog`

### Merge Conflicts in CHANGELOG.md

**Problem:** Conflicts when merging branches

**Solution:**
- Keep both versions during merge
- Regenerate changelog: `just changelog`
- Review and manually adjust if needed

## Post-Release Tasks

After each release:

1. **Update documentation** - Ensure docs reflect new version
2. **Close related beads** - Mark completed issues as done
3. **Plan next version** - Review open beads for next milestone
4. **Monitor issues** - Watch for bugs in new release
5. **Update dependencies** - Check for outdated packages

## Version Support Policy

- **Latest version:** Fully supported, receives all updates
- **Previous minor:** Bug fixes only for 6 months
- **Older versions:** Community support only

## Emergency Rollback

If a release has critical issues:

1. **Warn users immediately** - Create GitHub issue
2. **Fix in hotfix branch** - Follow hotfix process above
3. **Delete problematic release** - GitHub UI or `gh release delete`
4. **Release fixed version ASAP**
5. **Post-mortem** - Document what went wrong

## Resources

- [Semantic Versioning](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [Conventional Commits](https://www.conventionalcommits.org/)
- [git-cliff Documentation](https://git-cliff.org/docs/)
- [just Documentation](https://just.systems/)

---

**Remember:** A good release process prevents bugs, a great release process makes rollbacks easy.
