// Common types used across Intent CLI schemas
// These types are shared across intent.cue, kirk.cue, interview.cue, and protocol.cue
//
// This file provides foundational type definitions including:
// - Severity and status enumerations
// - HTTP types (methods, headers, status codes)
// - String patterns (identifiers, emails, URLs, UUIDs)
// - Time and duration formats
// - Interview and workflow enumerations
// - KIRK analysis type categories
// - Protocol status types
//
// Usage:
//   Import this package in other schema files:
//   import "schema/common"
//
//   Use types with the common prefix:
//   severity: common.#Severity
//   method: common.#HTTPMethod
package common

// =============================================================================
// SEVERITY AND STATUS TYPES
// =============================================================================

// Severity levels for issues, errors, and quality metrics
#Severity: "info" | "warning" | "error" | "critical"

// HTTP status code range
#StatusCode: int & >=100 & <=599

// =============================================================================
// HTTP TYPES
// =============================================================================

// HTTP methods
#HTTPMethod: "GET" | "POST" | "PUT" | "PATCH" | "DELETE" | "HEAD" | "OPTIONS"

// HTTP headers map
#Headers: [string]: string

// =============================================================================
// IDENTIFIER AND STRING PATTERNS
// =============================================================================

// Valid identifier pattern (lowercase, alphanumeric, hyphens, underscores)
// Must start with lowercase letter
#Identifier: =~"^[a-z][a-z0-9_-]*$"

// Non-empty string
#NonEmptyString: string & !=""

// Email address pattern (basic validation)
#Email: =~"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

// URL pattern (http/https)
#URL: =~"^https?://[^\\s]+$"

// UUID v4 pattern
#UUID: =~"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"

// =============================================================================
// TIME AND DURATION TYPES
// =============================================================================

// ISO 8601 timestamp with timezone
// Examples: 2026-01-25T09:51:06Z, 2026-01-25T09:51:06.123456Z, 2026-01-25T09:51:06-06:00
#Timestamp: =~"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}:\\d{2})$"

// Duration in Go format (e.g., "1h", "30m", "45s", "1h30m45s")
#Duration: =~"^(\\d+h)?(\\d+m)?(\\d+s)?$"

// =============================================================================
// INTERVIEW AND WORKFLOW TYPES
// =============================================================================

// Interview stages
#InterviewStage: "discovery" | "refinement" | "validation" | "complete" | "paused"

// System profile types
#ProfileType: "api" | "cli" | "event" | "data" | "workflow" | "ui"

// Perspective/role for answering questions
#Perspective: "user" | "developer" | "ops" | "security" | "business"

// Question categories
#QuestionCategory: "happy_path" | "error_case" | "edge_case" | "constraint" | "dependency" | "nonfunctional"

// Question priorities
#QuestionPriority: "critical" | "important" | "nice_to_have"

// =============================================================================
// KIRK ANALYSIS TYPES
// =============================================================================

// Gap types for mental model gap detection
#GapType: "inversion" | "second_order" | "checklist" | "coverage" | "security"

// Conflict types
#ConflictType: "cap_theorem" | "scope_paradox" | "security_usability" | "performance_consistency"

// =============================================================================
// PROTOCOL AND STATUS TYPES
// =============================================================================

// Protocol status for AI agent responses
#ProtocolStatus: "ok" | "error" | "requires_input"
