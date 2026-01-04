/// Calendar utilities for date/time operations
/// Leverages Erlang calendar module for efficiency

import gleam/int
import gleam/order
import gleam/string

/// Gregorian date representation
pub type Date {
  Date(year: Int, month: Int, day: Int)
}

/// Time representation
pub type Time {
  Time(hour: Int, minute: Int, second: Int)
}

/// DateTime combination
pub type DateTime {
  DateTime(date: Date, time: Time)
}

/// Parse ISO 8601 date (YYYY-MM-DD)
pub fn parse_date(date_str: String) -> Result(Date, String) {
  let parts = string.split(date_str, "-")
  case parts {
    [year_str, month_str, day_str] -> {
      case int.parse(year_str) {
        Ok(year) ->
          case int.parse(month_str) {
            Ok(month) ->
              case int.parse(day_str) {
                Ok(day) ->
                  case is_valid_date(year, month, day) {
                    True -> Ok(Date(year, month, day))
                    False -> Error("Invalid date: " <> date_str)
                  }
                Error(_) -> Error("Invalid date format: " <> date_str)
              }
            Error(_) -> Error("Invalid date format: " <> date_str)
          }
        Error(_) -> Error("Invalid date format: " <> date_str)
      }
    }
    _ -> Error("Date must be YYYY-MM-DD format")
  }
}

/// Parse ISO 8601 time (HH:MM:SS)
pub fn parse_time(time_str: String) -> Result(Time, String) {
  let parts = string.split(time_str, ":")
  case parts {
    [hour_str, minute_str, second_str] -> {
      case int.parse(hour_str) {
        Ok(hour) ->
          case int.parse(minute_str) {
            Ok(minute) ->
              case int.parse(second_str) {
                Ok(second) ->
                  case is_valid_time(hour, minute, second) {
                    True -> Ok(Time(hour, minute, second))
                    False -> Error("Invalid time: " <> time_str)
                  }
                Error(_) -> Error("Invalid time format: " <> time_str)
              }
            Error(_) -> Error("Invalid time format: " <> time_str)
          }
        Error(_) -> Error("Invalid time format: " <> time_str)
      }
    }
    _ -> Error("Time must be HH:MM:SS format")
  }
}

/// Check if date is valid (accounting for leap years and month boundaries)
fn is_valid_date(year: Int, month: Int, day: Int) -> Bool {
  case month >= 1 && month <= 12 {
    False -> False
    True -> {
      let max_day = days_in_month(year, month)
      day >= 1 && day <= max_day
    }
  }
}

/// Check if time is valid
fn is_valid_time(hour: Int, minute: Int, second: Int) -> Bool {
  hour >= 0 && hour < 24 && minute >= 0 && minute < 60 && second >= 0
  && second < 60
}

/// Get number of days in a given month (accounting for leap years)
pub fn days_in_month(year: Int, month: Int) -> Int {
  case month {
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    4 | 6 | 9 | 11 -> 30
    2 -> case is_leap_year(year) {
      True -> 29
      False -> 28
    }
    _ -> 0
  }
}

/// Check if a year is a leap year
pub fn is_leap_year(year: Int) -> Bool {
  case year % 400 == 0 {
    True -> True
    False ->
      case year % 100 == 0 {
        True -> False
        False -> year % 4 == 0
      }
  }
}

/// Format date as ISO 8601 (YYYY-MM-DD)
pub fn format_date(date: Date) -> String {
  int.to_string(date.year)
  <> "-"
  <> pad_int(date.month, 2)
  <> "-"
  <> pad_int(date.day, 2)
}

/// Format time as ISO 8601 (HH:MM:SS)
pub fn format_time(time: Time) -> String {
  pad_int(time.hour, 2)
  <> ":"
  <> pad_int(time.minute, 2)
  <> ":"
  <> pad_int(time.second, 2)
}

/// Format datetime as ISO 8601 (YYYY-MM-DDTHH:MM:SS)
pub fn format_datetime(dt: DateTime) -> String {
  format_date(dt.date) <> "T" <> format_time(dt.time)
}

/// Pad integer with zeros to specified width
fn pad_int(num: Int, width: Int) -> String {
  let str = int.to_string(num)
  let padding = width - string.length(str)
  case padding > 0 {
    True -> string.repeat("0", padding) <> str
    False -> str
  }
}

/// Compare two dates
pub fn compare_dates(a: Date, b: Date) -> order.Order {
  case int.compare(a.year, b.year) {
    order.Lt -> order.Lt
    order.Gt -> order.Gt
    order.Eq ->
      case int.compare(a.month, b.month) {
        order.Lt -> order.Lt
        order.Gt -> order.Gt
        order.Eq -> int.compare(a.day, b.day)
      }
  }
}

/// Compare two times
pub fn compare_times(a: Time, b: Time) -> order.Order {
  case int.compare(a.hour, b.hour) {
    order.Lt -> order.Lt
    order.Gt -> order.Gt
    order.Eq ->
      case int.compare(a.minute, b.minute) {
        order.Lt -> order.Lt
        order.Gt -> order.Gt
        order.Eq -> int.compare(a.second, b.second)
      }
  }
}

/// Get day of week (0=Sunday, 6=Saturday)
pub fn day_of_week(date: Date) -> Int {
  // Zeller's congruence algorithm
  let year = case date.month < 3 {
    True -> date.year - 1
    False -> date.year
  }
  let month = case date.month < 3 {
    True -> date.month + 12
    False -> date.month
  }
  let k = year % 100
  let j = year / 100

  let month_calc = { 13 * { month + 1 } } / 5
  let k_calc = k / 4
  let j_calc = j / 4
  let h =
    date.day + month_calc + k + k_calc + j_calc - { 2 * j }

  let day = h % 7
  case day {
    -6 -> 1
    -5 -> 2
    -4 -> 3
    -3 -> 4
    -2 -> 5
    -1 -> 6
    0 -> 0
    _ -> day % 7
  }
}

/// Get day name
pub fn day_name(day_num: Int) -> String {
  case day_num % 7 {
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    _ -> "Unknown"
  }
}

/// Get month name
pub fn month_name(month_num: Int) -> String {
  case month_num {
    1 -> "January"
    2 -> "February"
    3 -> "March"
    4 -> "April"
    5 -> "May"
    6 -> "June"
    7 -> "July"
    8 -> "August"
    9 -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"
    _ -> "Unknown"
  }
}
