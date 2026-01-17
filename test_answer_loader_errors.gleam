import gleam/io
import gleam/string
import intent/answer_loader

pub fn main() {
  io.println("=== Testing AI-Friendly Error Formatting for answer_loader ===\n")

  // Test 1: FileNotFound
  io.println("1. FileNotFound - AI Format (CUE):")
  let err1 = answer_loader.FileNotFound("/tmp/missing.json")
  io.println(answer_loader.format_error_ai(err1))
  io.println("\n")

  io.println("2. FileNotFound - Text Format:")
  io.println(answer_loader.format_error_text(err1))
  io.println("\n" <> string.repeat("=", 80) <> "\n")

  // Test 2: PermissionDenied
  io.println("3. PermissionDenied - AI Format (CUE):")
  let err2 = answer_loader.PermissionDenied("/tmp/protected.json")
  io.println(answer_loader.format_error_ai(err2))
  io.println("\n")

  io.println("4. PermissionDenied - Text Format:")
  io.println(answer_loader.format_error_text(err2))
  io.println("\n" <> string.repeat("=", 80) <> "\n")

  // Test 3: ParseError
  io.println("5. ParseError - AI Format (CUE):")
  let err3 =
    answer_loader.ParseError(
      "/tmp/invalid.json",
      "Invalid JSON format. Expected object with string keys and values.",
    )
  io.println(answer_loader.format_error_ai(err3))
  io.println("\n")

  io.println("6. ParseError - Text Format:")
  io.println(answer_loader.format_error_text(err3))
  io.println("\n" <> string.repeat("=", 80) <> "\n")

  // Test 4: SchemaError
  io.println("7. SchemaError - AI Format (CUE):")
  let err4 =
    answer_loader.SchemaError(
      "Found empty answers for questions: [question-2, question-5]",
    )
  io.println(answer_loader.format_error_ai(err4))
  io.println("\n")

  io.println("8. SchemaError - Text Format:")
  io.println(answer_loader.format_error_text(err4))
  io.println("\n" <> string.repeat("=", 80) <> "\n")

  // Test 5: IoError
  io.println("9. IoError - AI Format (CUE):")
  let err5 = answer_loader.IoError("Failed to read file: /tmp/corrupt.json")
  io.println(answer_loader.format_error_ai(err5))
  io.println("\n")

  io.println("10. IoError - Text Format:")
  io.println(answer_loader.format_error_text(err5))
  io.println("\n")

  io.println("=== All Error Formats Tested Successfully ===")
}
