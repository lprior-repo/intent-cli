/// Standardized emoji and Unicode symbols for consistent formatting across Intent CLI
///
/// This module provides constants for all emoji and symbols used in output formatting,
/// ensuring consistency across Kirk analysis modules and other command output.
// =============================================================================
// STATUS INDICATORS
// =============================================================================

/// Success indicator - used for passed checks, completed tasks
pub const success = "✅"

/// Failure indicator - used for failed checks, errors
pub const failure = "❌"

/// Warning indicator - used for warnings, medium severity issues
pub const warning = "⚠️"

/// Info indicator - used for informational messages, low severity
pub const info = "ℹ️"

/// Critical indicator - used for critical issues, security problems
pub const critical = "🚨"

// =============================================================================
// SEVERITY ICONS (matches severity levels across modules)
// =============================================================================

/// Critical severity icon
pub const severity_critical = "🚨"

/// High severity icon
pub const severity_high = "❌"

/// Medium severity icon
pub const severity_medium = "⚠️"

/// Low severity icon
pub const severity_low = "ℹ️"

// =============================================================================
// CATEGORY ICONS (for different analysis types)
// =============================================================================

/// Security-related icon
pub const security = "🔐"

/// Coverage/testing icon
pub const coverage = "📊"

/// Quality/analysis icon
pub const quality = "💎"

/// Gap/missing items icon
pub const gap = "🕳️"

/// Inversion/failure mode icon
pub const inversion = "🔄"

/// Effects/consequences icon
pub const effects = "🎯"

/// Lock/concurrency icon
pub const lock = "🔒"

/// User/person icon
pub const user = "👤"

/// Integration/connection icon
pub const integration = "🔌"

/// Light bulb for suggestions
pub const suggestion = "💡"

/// Package/module icon
pub const package = "📦"

/// Path/route icon
pub const path = "🛤️"

/// Tools/methods icon
pub const tools = "🔧"

/// Target/goal icon
pub const target = "🎯"

/// Edge cases icon
pub const edge = "🔺"

// =============================================================================
// PROGRESS AND STATUS
// =============================================================================

/// Checkmark (simple, no color box)
pub const check = "✓"

/// Cross/X mark (simple, no color box)
pub const cross = "✗"

/// Green checkmark in box
pub const check_box = "✅"

/// Red X in box
pub const cross_box = "❌"

/// Circle indicator
pub const circle = "●"

/// Empty circle
pub const circle_empty = "○"

/// Filled square (for progress bars)
pub const block_filled = "█"

/// Empty/light square (for progress bars)
pub const block_empty = "░"

/// Half block
pub const block_half = "▌"

// =============================================================================
// BOX DRAWING CHARACTERS
// =============================================================================

/// Box top-left corner
pub const box_tl = "╔"

/// Box top-right corner
pub const box_tr = "╗"

/// Box bottom-left corner
pub const box_bl = "╚"

/// Box bottom-right corner
pub const box_br = "╝"

/// Box horizontal line
pub const box_h = "═"

/// Box vertical line
pub const box_v = "║"

/// Box left T-junction
pub const box_lt = "╠"

/// Box right T-junction
pub const box_rt = "╣"

/// Box top T-junction
pub const box_tt = "╦"

/// Box bottom T-junction
pub const box_bt = "╩"

/// Box cross/plus junction
pub const box_cross = "╬"

// =============================================================================
// ARROWS AND POINTERS
// =============================================================================

/// Right arrow
pub const arrow_right = "→"

/// Left arrow
pub const arrow_left = "←"

/// Up arrow
pub const arrow_up = "↑"

/// Down arrow
pub const arrow_down = "↓"

/// Redirect/return arrow
pub const arrow_return = "↪️"

/// Bullet point
pub const bullet = "•"

/// Dash bullet
pub const dash = "–"

// =============================================================================
// NUMERIC/SCORE INDICATORS
// =============================================================================

/// Trophy/achievement
pub const trophy = "🏆"

/// Star
pub const star = "⭐"

/// Fire (for performance/hot)
pub const fire = "🔥"

/// Thumbs up
pub const thumbs_up = "👍"

/// Thumbs down
pub const thumbs_down = "👎"

// =============================================================================
// HTTP STATUS CODE ICONS
// =============================================================================

/// 2xx Success responses
pub const status_2xx = "✅"

/// 3xx Redirect responses
pub const status_3xx = "↪️"

/// 4xx Client error responses
pub const status_4xx = "⚠️"

/// 5xx Server error responses
pub const status_5xx = "❌"

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Get severity icon by severity level string
pub fn severity_icon(severity: String) -> String {
  case severity {
    "critical" -> severity_critical
    "high" -> severity_high
    "medium" -> severity_medium
    "low" -> severity_low
    _ -> info
  }
}

/// Get status code category icon
pub fn status_icon(status: Int) -> String {
  case status {
    s if s >= 200 && s < 300 -> status_2xx
    s if s >= 300 && s < 400 -> status_3xx
    s if s >= 400 && s < 500 -> status_4xx
    s if s >= 500 && s < 600 -> status_5xx
    _ -> circle
  }
}

/// Get boolean status icon (success/failure)
pub fn bool_icon(value: Bool) -> String {
  case value {
    True -> success
    False -> failure
  }
}
