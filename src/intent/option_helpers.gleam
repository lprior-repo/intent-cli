import gleam/option

/// Unwrap an Option(Bool) with a default value
pub fn unwrap_bool(opt: option.Option(Bool), default: Bool) -> Bool {
  option.unwrap(opt, default)
}

/// Unwrap an Option(Int) with a default value
pub fn unwrap_int(opt: option.Option(Int), default: Int) -> Int {
  option.unwrap(opt, default)
}

/// Unwrap an Option(Float) with a default value
pub fn unwrap_float(opt: option.Option(Float), default: Float) -> Float {
  option.unwrap(opt, default)
}

/// Unwrap an Option(String) with a default value
pub fn unwrap_string(opt: option.Option(String), default: String) -> String {
  option.unwrap(opt, default)
}

/// Unwrap an Option(List(String)) with a default value
pub fn unwrap_string_list(
  opt: option.Option(List(String)),
  default: List(String),
) {
  option.unwrap(opt, default)
}

/// Unwrap an Option(List(Int)) with a default value
pub fn unwrap_int_list(opt: option.Option(List(Int)), default: List(Int)) {
  option.unwrap(opt, default)
}

/// Unwrap an Option(List(t)) with a default value
pub fn unwrap_list(opt: option.Option(List(t)), default: List(t)) {
  option.unwrap(opt, default)
}
