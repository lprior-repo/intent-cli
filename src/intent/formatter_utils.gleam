/// Formatting utilities for consistent output across Intent CLI
/// Provides reusable functions for box headers, progress bars, indentation, and more.
import gleam/float
import gleam/int
import gleam/string
import intent/emoji_constants as emoji

// =============================================================================
// BOX HEADERS
// =============================================================================

/// Standard box header width (internal content width)
pub const box_width = 60

/// Create a box header with centered title
pub fn box_header(title: String) -> String {
  let top =
    emoji.box_tl <> string.repeat(emoji.box_h, box_width) <> emoji.box_tr
  let middle = center_in_box(title, box_width)
  let bottom =
    emoji.box_bl <> string.repeat(emoji.box_h, box_width) <> emoji.box_br

  top <> "\n" <> middle <> "\n" <> bottom
}

/// Create a box header with title and subtitle
pub fn box_header_with_subtitle(title: String, subtitle: String) -> String {
  let top =
    emoji.box_tl <> string.repeat(emoji.box_h, box_width) <> emoji.box_tr
  let title_line = center_in_box(title, box_width)
  let subtitle_line = center_in_box(subtitle, box_width)
  let bottom =
    emoji.box_bl <> string.repeat(emoji.box_h, box_width) <> emoji.box_br

  top <> "\n" <> title_line <> "\n" <> subtitle_line <> "\n" <> bottom
}

/// Center text within box borders
fn center_in_box(text: String, width: Int) -> String {
  let text_len = string.length(text)
  let padding = width - text_len
  let left_pad = padding / 2
  let right_pad = padding - left_pad

  emoji.box_v
  <> string.repeat(" ", left_pad)
  <> text
  <> string.repeat(" ", right_pad)
  <> emoji.box_v
}

// =============================================================================
// PROGRESS BARS
// =============================================================================

/// Create a progress bar from a percentage (0-100)
pub fn progress_bar(percentage: Float) -> String {
  progress_bar_with_width(percentage, 10)
}

/// Create a progress bar with custom width
pub fn progress_bar_with_width(percentage: Float, width: Int) -> String {
  let clamped = float.clamp(percentage, 0.0, 100.0)
  let filled_count = float.round(clamped /. 100.0 *. int.to_float(width))
  let empty_count = width - filled_count

  "["
  <> string.repeat(emoji.block_filled, filled_count)
  <> string.repeat(emoji.block_empty, empty_count)
  <> "]"
}

/// Format a score with progress bar and percentage
pub fn score_with_bar(score: Float) -> String {
  progress_bar(score) <> " " <> format_percentage(score)
}

/// Format a score with progress bar and color-coded status
pub fn score_with_status(score: Float) -> String {
  let status_icon = case True {
    _ if score >=. 90.0 -> emoji.success
    _ if score >=. 70.0 -> emoji.warning
    _ -> emoji.failure
  }

  score_with_bar(score) <> " " <> status_icon
}

/// Format a float as percentage with 1 decimal place
pub fn format_percentage(value: Float) -> String {
  float_to_string_1dp(value) <> "%"
}

// =============================================================================
// INDENTATION
// =============================================================================

/// No indentation (root level)
pub fn indent_0() -> String {
  ""
}

/// Level 1 indentation (2 spaces)
pub fn indent_1() -> String {
  "  "
}

/// Level 2 indentation (4 spaces)
pub fn indent_2() -> String {
  "    "
}

/// Level 3 indentation (6 spaces)
pub fn indent_3() -> String {
  "      "
}

/// Level 4 indentation (8 spaces) - maximum depth
pub fn indent_4() -> String {
  "        "
}

/// Create custom indentation with N levels (2 spaces per level)
pub fn indent_n(level: Int) -> String {
  string.repeat("  ", level)
}

// =============================================================================
// SCORE FORMATTING
// =============================================================================

/// Format an integer score out of 100 with visual indicator
pub fn format_score_int(score: Int) -> String {
  let percentage = int.to_float(score)
  int.to_string(score) <> "/100 " <> status_icon_for_score(percentage)
}

/// Format a float score as percentage with visual indicator
pub fn format_score_float(score: Float) -> String {
  format_percentage(score) <> " " <> status_icon_for_score(score)
}

fn status_icon_for_score(score: Float) -> String {
  case True {
    _ if score >=. 90.0 -> emoji.success
    _ if score >=. 70.0 -> emoji.warning
    _ if score >=. 50.0 -> emoji.info
    _ -> emoji.failure
  }
}

// =============================================================================
// SECTION FORMATTING
// =============================================================================

/// Format a section header with icon and title
pub fn section_header(icon: String, title: String) -> String {
  icon <> " " <> title
}

/// Format a section header with icon, title, and count
pub fn section_header_with_count(
  icon: String,
  title: String,
  count: Int,
) -> String {
  icon <> " " <> title <> " (" <> int.to_string(count) <> ")"
}

/// Format a section separator line
pub fn section_separator() -> String {
  string.repeat("─", box_width)
}

/// Format an empty section message
pub fn empty_section_message(message: String) -> String {
  indent_1() <> "(" <> message <> ")"
}

// =============================================================================
// LIST FORMATTING
// =============================================================================

/// Format a bulleted list item at level 1
pub fn bullet_item(text: String) -> String {
  indent_1() <> emoji.bullet <> " " <> text
}

/// Format a bulleted list item at level 2
pub fn bullet_item_2(text: String) -> String {
  indent_2() <> emoji.bullet <> " " <> text
}

/// Format a numbered list item
pub fn numbered_item(number: Int, text: String) -> String {
  indent_1() <> int.to_string(number) <> ". " <> text
}

/// Format a check/cross list item
pub fn status_item(passed: Bool, text: String) -> String {
  let icon = case passed {
    True -> emoji.success
    False -> emoji.failure
  }
  indent_1() <> icon <> " " <> text
}

// =============================================================================
// KEY-VALUE FORMATTING
// =============================================================================

/// Format a key-value pair with alignment
pub fn kv_pair(key: String, value: String) -> String {
  indent_1() <> key <> ": " <> value
}

/// Format a key-value pair at level 2
pub fn kv_pair_2(key: String, value: String) -> String {
  indent_2() <> key <> ": " <> value
}

// =============================================================================
// FLOAT UTILITIES
// =============================================================================

/// Convert float to string with 1 decimal place
pub fn float_to_string_1dp(f: Float) -> String {
  let rounded = float.round(f *. 10.0) |> int.to_float
  let divided = rounded /. 10.0

  let int_part = float.floor(divided) |> float.round
  let decimal_part = float.round({ divided -. int.to_float(int_part) } *. 10.0)

  int.to_string(int_part) <> "." <> int.to_string(decimal_part)
}

/// Round float to nearest integer
pub fn round_to_int(f: Float) -> Int {
  float.round(f)
}
