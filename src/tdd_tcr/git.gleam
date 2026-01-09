//// Git command wrapper for TDD-TCR TCR (Test-Commit-Revert) enforcement.
////
//// Provides safe, type-safe git operations using shellout.
//// All operations return Result types - no exceptions.
//// Works with git for state management (stash/reset for TCR).

import gleam/result
import gleam/string
import shellout

/// Run a raw git command with arguments
///
/// Returns stdout on success, error message on failure.
pub fn git_command(args: List(String)) -> Result(String, String) {
  shellout.command("git", args, ".", [])
  |> result.map_error(fn(err) {
    "Git command failed: " <> shellout.exit_code_to_string(err)
  })
}

/// Stage all changes and commit with message
///
/// If allow_empty is True, commit even if nothing changed.
/// Returns Ok(Nil) on success, Error(reason) on failure.
pub fn commit(message: String, allow_empty: Bool) -> Result(Nil, String) {
  let args = case allow_empty {
    True -> ["commit", "-m", message, "--allow-empty", "--no-verify"]
    False -> ["commit", "-m", message, "--no-verify"]
  }
  git_command(args)
  |> result.map(fn(_) { Nil })
}

/// Add all files to staging area
///
/// Equivalent to `git add -A`
pub fn add_all() -> Result(Nil, String) {
  git_command(["add", "-A"])
  |> result.map(fn(_) { Nil })
}

/// Stash current changes and return the stash identifier
///
/// The stash ID can be used with reset_hard_to_stash to restore state.
pub fn stash() -> Result(String, String) {
  git_command(["stash", "push", "-m", "tdd-tcr-stash"])
  |> result.try(fn(_) {
    // Get the stash ID (usually stash@{0} for latest)
    git_command(["stash", "list", "--format=%H"])
    |> result.map(fn(output) {
      let lines = string.split(string.trim(output), "\n")
      case lines {
        [first, ..rest] -> first
        [] -> "stash@{0}"
      }
    })
  })
}

/// Hard reset to a specific stash
///
/// Restores the repository to the state saved in the given stash ID.
/// This is used for TCR reverts - if tests fail, restore the pre-implementation state.
pub fn reset_hard_to_stash(stash_id: String) -> Result(Nil, String) {
  // First, reset hard to HEAD to clear any uncommitted changes
  use _ <- result.try(git_command(["reset", "--hard", "HEAD"]))

  // Then apply and drop the stash to restore the saved state
  use _ <- result.try(git_command(["stash", "apply", stash_id]))

  Ok(Nil)
}

/// Get the current branch name
///
/// Returns branch name without "refs/heads/" prefix.
pub fn current_branch() -> Result(String, String) {
  git_command(["rev-parse", "--abbrev-ref", "HEAD"])
  |> result.map(string.trim)
}

/// Get git status output (human-readable)
///
/// Returns the output of `git status`
pub fn status() -> Result(String, String) {
  git_command(["status", "--porcelain"])
  |> result.map(string.trim)
}
