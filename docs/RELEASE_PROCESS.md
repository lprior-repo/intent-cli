# Release Process

Complete guide for creating and publishing Intent CLI releases.

## Prerequisites

- **git-cliff** - Changelog generation
  ```bash
  cargo install git-cliff
  # or: brew install git-cliff
  ```

- **just** - Task runner
  ```bash
  cargo install just
  # or: brew install just
  ```

## Quick Release Checklist

- [ ] All tests pass (`gleam test`)
- [ ] Code is formatted (`gleam format --check`)
- [ ] Update version in `gleam.toml`
- [ ] Generate changelog (`just release X.Y.Z`)
- [ ] Review CHANGELOG.md
- [ ] Commit changes
- [ ] Create git tag
- [ ] Push to remote
- [ ] Create GitHub release

## Semantic Versioning

Follow [SemVer](https://semver.org/):

- **MAJOR (X.0.0)** - Breaking changes
- **MINOR (0.X.0)** - New features, backward compatible
- **PATCH (0.0.X)** - Bug fixes, backward compatible

## Step-by-Step Release

### 1. Verify Clean State

```bash
# Ensure working directory is clean
git status

# Run full test suite
gleam test

# Check formatting
gleam format --check

# Build successfully
gleam build
```

### 2. Update Version

Edit `gleam.toml`:
```toml
version = "0.2.0"
```

### 3. Generate Changelog

```bash
# Preview first
just changelog-preview v0.2.0

# Generate changelog
just release 0.2.0
```

### 4. Review Changes

Check `CHANGELOG.md`:
- All notable changes included
- Proper categorization
- Correct dates
- Clear descriptions

Edit manually if needed.

### 5. Commit Release

```bash
git add gleam.toml CHANGELOG.md
git commit -m "chore(release): prepare for v0.2.0"
```

### 6. Create Tag

```bash
git tag -a v0.2.0 -m "Release v0.2.0"
```

### 7. Push Everything

```bash
git push origin main
git push origin --tags
```

### 8. Create GitHub Release

1. Go to GitHub Releases page
2. Click "Draft a new release"
3. Select tag v0.2.0
4. Copy relevant CHANGELOG.md section
5. Publish release

## Hotfix Releases

For urgent bug fixes:

```bash
# 1. Branch from tag
git checkout -b hotfix/0.1.1 v0.1.0

# 2. Make fixes
# ... fix code, add tests ...

# 3. Update version
vim gleam.toml  # Set to 0.1.1

# 4. Generate changelog
just changelog v0.1.1

# 5. Commit and tag
git commit -am "fix: critical bug in X"
git tag -a v0.1.1 -m "Hotfix v0.1.1"

# 6. Push
git push origin hotfix/0.1.1
git push origin --tags

# 7. Merge back to main
git checkout main
git merge hotfix/0.1.1
git push origin main
```

## Commit Message Tips

Good commit messages make better changelogs:

### Good Examples
```
feat(interview): add support for resuming sessions across restarts
fix(parser): handle empty CUE blocks without crashing
docs(api): add examples for KIRK analysis commands
```

### Bad Examples
```
feat: improvements       # Too vague
fix: bug                 # What bug?
docs: updates           # What updates?
```

## Breaking Changes

Always call out breaking changes:

```markdown
## [2.0.0] - 2026-02-01

### ⚠️ BREAKING CHANGES

- **interview:** Session format changed - migrate old sessions
- **api:** Removed deprecated `--legacy` flag
- **config:** Default timeout increased from 5s to 30s
```

## Troubleshooting

### Changelog Missing Commits

**Problem:** Some commits don't appear

**Solution:**
- Check `cliff.toml` commit_parsers
- Ensure conventional commit format
- Use `just changelog-preview` to debug
- Add missing entries manually

### Wrong Version Order

**Problem:** Versions out of order

**Solution:**
- Check `topo_order = false` in cliff.toml
- Use semantic versioning (vX.Y.Z)
- Regenerate: `just changelog`

## Post-Release

After releasing:

1. Monitor issues for bugs
2. Update documentation
3. Close related beads
4. Plan next version
5. Check dependencies

## Resources

- [Semantic Versioning](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [Conventional Commits](https://www.conventionalcommits.org/)
- [git-cliff Documentation](https://git-cliff.org/docs/)

---

**Remember:** Good releases prevent bugs, great releases make rollbacks easy.
