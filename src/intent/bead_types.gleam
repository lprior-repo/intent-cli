//// Bead type definitions and helper functions for work item management.
////
//// This module defines the core types for beads (atomic work units) and provides
//// type-safe conversions and status checking helpers.

import gleam/list
import gleam/result

/// Represents the current state of a bead in its lifecycle.
pub type BeadStatus {
  Open
  InProgress
  Closed
}

/// Categorizes the type of work a bead represents.
pub type BeadKind {
  Task
  Bug
  Feature
}

/// An atomic work unit (5-30 minutes) derived from specifications.
///
/// Beads are the fundamental unit of work in the Intent CLI system,
/// typically generated from CUE specifications or interview sessions.
pub type Bead {
  Bead(
    id: String,
    title: String,
    status: BeadStatus,
    priority: Int,
    issue_type: BeadKind,
    created_at: String,
    created_by: String,
    updated_at: String,
    labels: List(String),
  )
}

/// Converts a BeadStatus to its string representation.
///
/// ## Examples
///
/// ```gleam
/// status_to_string(Open)
/// // -> "open"
///
/// status_to_string(InProgress)
/// // -> "in_progress"
/// ```
pub fn status_to_string(status: BeadStatus) -> String {
  case status {
    Open -> "open"
    InProgress -> "in_progress"
    Closed -> "closed"
  }
}

/// Converts a string to a BeadStatus.
///
/// Returns Error if the string does not match a valid status.
///
/// ## Examples
///
/// ```gleam
/// status_from_string("open")
/// // -> Ok(Open)
///
/// status_from_string("invalid")
/// // -> Error("Unknown status: invalid")
/// ```
pub fn status_from_string(status: String) -> Result(BeadStatus, String) {
  case status {
    "open" -> Ok(Open)
    "in_progress" -> Ok(InProgress)
    "closed" -> Ok(Closed)
    _ -> Error("Unknown status: " <> status)
  }
}

/// Converts a BeadKind to its string representation.
///
/// ## Examples
///
/// ```gleam
/// kind_to_string(Task)
/// // -> "task"
///
/// kind_to_string(Feature)
/// // -> "feature"
/// ```
pub fn kind_to_string(kind: BeadKind) -> String {
  case kind {
    Task -> "task"
    Bug -> "bug"
    Feature -> "feature"
  }
}

/// Converts a string to a BeadKind.
///
/// Returns Error if the string does not match a valid kind.
///
/// ## Examples
///
/// ```gleam
/// kind_from_string("task")
/// // -> Ok(Task)
///
/// kind_from_string("invalid")
/// // -> Error("Unknown kind: invalid")
/// ```
pub fn kind_from_string(kind: String) -> Result(BeadKind, String) {
  case kind {
    "task" -> Ok(Task)
    "bug" -> Ok(Bug)
    "feature" -> Ok(Feature)
    _ -> Error("Unknown kind: " <> kind)
  }
}

/// Checks if a bead is in Open status.
///
/// ## Examples
///
/// ```gleam
/// is_open(Bead(..bead, status: Open))
/// // -> True
///
/// is_open(Bead(..bead, status: Closed))
/// // -> False
/// ```
pub fn is_open(bead: Bead) -> Bool {
  case bead.status {
    Open -> True
    _ -> False
  }
}

/// Checks if a bead is in InProgress status.
///
/// ## Examples
///
/// ```gleam
/// is_in_progress(Bead(..bead, status: InProgress))
/// // -> True
///
/// is_in_progress(Bead(..bead, status: Open))
/// // -> False
/// ```
pub fn is_in_progress(bead: Bead) -> Bool {
  case bead.status {
    InProgress -> True
    _ -> False
  }
}

/// Checks if a bead is in Closed status.
///
/// ## Examples
///
/// ```gleam
/// is_closed(Bead(..bead, status: Closed))
/// // -> True
///
/// is_closed(Bead(..bead, status: Open))
/// // -> False
/// ```
pub fn is_closed(bead: Bead) -> Bool {
  case bead.status {
    Closed -> True
    _ -> False
  }
}

/// Checks if a bead has a specific label.
///
/// ## Examples
///
/// ```gleam
/// has_label(Bead(..bead, labels: ["urgent", "api"]), "urgent")
/// // -> True
///
/// has_label(Bead(..bead, labels: ["api"]), "urgent")
/// // -> False
/// ```
pub fn has_label(bead: Bead, label: String) -> Bool {
  list.contains(bead.labels, label)
}
