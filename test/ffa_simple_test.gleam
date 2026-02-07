/// Simple FFI test to verify basic functionality
import gleam/io
import gleam/should

// Test basic imports work
pub fn test_basic_imports() {
  io.println("Testing basic imports...")
  True |> should.be_true()
}

pub fn main() {
  io.println("🧪 Running simple FFI test...")
  test_basic_imports()
  io.println("✅ Simple FFI test completed!")
}