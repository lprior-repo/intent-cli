import gleam/io
import gleam/string
import intent/plan_mode

pub fn main() {
  io.println("=== Testing AI-Friendly Error Formatting for plan_mode ===\n")

  // Test SessionNotFound
  io.println("1. SessionNotFound - AI Format (CUE):")
  let err1 = plan_mode.SessionNotFound("test-123")
  io.println(plan_mode.format_error_ai(err1))
  io.println("\n")

  io.println("2. SessionNotFound - Text Format:")
  io.println(plan_mode.format_error_text(err1))
  io.println("\n" <> string.repeat("=", 80) <> "\n")

  // Test ParseError
  io.println("3. ParseError - AI Format (CUE):")
  let err2 = plan_mode.ParseError("Invalid CUE syntax at line 42")
  io.println(plan_mode.format_error_ai(err2))
  io.println("\n")

  io.println("4. ParseError - Text Format:")
  io.println(plan_mode.format_error_text(err2))
  io.println("\n" <> string.repeat("=", 80) <> "\n")

  // Test CyclicDependency
  io.println("5. CyclicDependency - AI Format (CUE):")
  let err3 = plan_mode.CyclicDependency(["bead-A", "bead-B", "bead-C"])
  io.println(plan_mode.format_error_ai(err3))
  io.println("\n")

  io.println("6. CyclicDependency - Text Format:")
  io.println(plan_mode.format_error_text(err3))
  io.println("\n" <> string.repeat("=", 80) <> "\n")

  // Test MissingDependency
  io.println("7. MissingDependency - AI Format (CUE):")
  let err4 = plan_mode.MissingDependency("api-create-user", "auth-login")
  io.println(plan_mode.format_error_ai(err4))
  io.println("\n")

  io.println("8. MissingDependency - Text Format:")
  io.println(plan_mode.format_error_text(err4))
  io.println("\n")

  io.println("=== All Error Formats Tested Successfully ===")
}
