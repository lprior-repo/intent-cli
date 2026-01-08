//// Shell command execution wrapper for TDD-TCR operations.
////
//// Provides type-safe wrappers around shellout for running external commands.
//// Used for: gleam build, gleam test, git commands, etc.

import gleam/result
import shellout

/// Run a command and return stdout
///
/// Executes command with args in current directory.
/// Returns Ok(stdout) on success (exit code 0).
/// Returns Error(stderr) on failure (exit code != 0).
pub fn run_command(cmd: String, args: List(String)) -> Result(String, String) {
  shellout.command(cmd, args, ".", [])
  |> result.map_error(fn(err) {
    shellout.exit_code_to_string(err)
  })
}

/// Run a command silently, only checking exit code
///
/// Useful for commands where we only care about success/failure, not output.
/// Returns Ok(Nil) if exit code is 0, Error(reason) otherwise.
pub fn run_command_silent(cmd: String, args: List(String)) -> Result(Nil, String) {
  run_command(cmd, args)
  |> result.map(fn(_) { Nil })
}
