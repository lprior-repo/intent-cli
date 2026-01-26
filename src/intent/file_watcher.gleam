/// File watcher module - poll-based file change detection
import gleam/erlang/process
import gleam/io
import repeatedly
import simplifile

/// Error types for file watching
pub type WatchError {
  FileNotFound(path: String)
  PermissionDenied(path: String)
  UnknownError(path: String, reason: String)
}

/// File state for tracking changes
pub type FileState {
  FileState(path: String, mtime_seconds: Int)
}

/// Get the current modification time of a file
pub fn get_file_mtime(path: String) -> Result(Int, WatchError) {
  case simplifile.file_info(path) {
    Ok(info) -> Ok(info.mtime_seconds)
    Error(simplifile.Enoent) -> Error(FileNotFound(path))
    Error(simplifile.Eacces) -> Error(PermissionDenied(path))
    Error(_) -> Error(UnknownError(path, "Unknown file system error"))
  }
}

/// Initialize file state by reading current mtime
pub fn init_file_state(path: String) -> Result(FileState, WatchError) {
  case get_file_mtime(path) {
    Ok(mtime) -> Ok(FileState(path: path, mtime_seconds: mtime))
    Error(e) -> Error(e)
  }
}

/// Check if file has changed since last state
pub fn has_changed(state: FileState) -> Result(Bool, WatchError) {
  case get_file_mtime(state.path) {
    Ok(current_mtime) -> Ok(current_mtime != state.mtime_seconds)
    Error(e) -> Error(e)
  }
}

/// Update file state with current mtime
pub fn update_state(state: FileState) -> Result(FileState, WatchError) {
  case get_file_mtime(state.path) {
    Ok(mtime) -> Ok(FileState(..state, mtime_seconds: mtime))
    Error(e) -> Error(e)
  }
}

/// Watch a file and call callback when it changes
pub fn watch(path: String, interval_ms: Int, on_change: fn() -> Nil) -> Nil {
  case init_file_state(path) {
    Ok(initial_state) -> {
      on_change()
      let _repeater =
        repeatedly.call(interval_ms, initial_state, fn(state, _count) {
          case has_changed(state) {
            Ok(True) -> {
              case update_state(state) {
                Ok(new_state) -> {
                  on_change()
                  new_state
                }
                Error(_) -> state
              }
            }
            Ok(False) -> state
            Error(_) -> state
          }
        })
      process.sleep_forever()
    }
    Error(FileNotFound(_)) -> {
      io.println_error("Error: File not found: " <> path)
      Nil
    }
    Error(PermissionDenied(_)) -> {
      io.println_error("Error: Permission denied: " <> path)
      Nil
    }
    Error(UnknownError(_, reason)) -> {
      io.println_error("Error: " <> reason)
      Nil
    }
  }
}
