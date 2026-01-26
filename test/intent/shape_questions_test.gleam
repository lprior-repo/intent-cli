import gleam/string
import gleeunit/should
import intent/question_loader

pub fn shape_questions_round_1_test() {
  let assert Ok(db) = question_loader.load_default_questions()
  let questions = question_loader.get_questions(db, "shape", 1)

  // Should have exactly 6 questions for shape profile, round 1
  should.equal(questions |> list_length, 6)
}

pub fn shape_questions_structure_test() {
  let assert Ok(db) = question_loader.load_default_questions()
  let questions = question_loader.get_questions(db, "shape", 1)

  // All questions should have valid IDs starting with "r1-shape-"
  questions
  |> list_all(fn(q) { string_starts_with(q.id, "r1-shape-") })
  |> should.be_true

  // All questions should be round 1
  questions
  |> list_all(fn(q) { q.round == 1 })
  |> should.be_true

  // All questions should have non-empty question text
  questions
  |> list_all(fn(q) { q.question != "" })
  |> should.be_true
}

pub fn shape_questions_extract_fields_test() {
  let assert Ok(db) = question_loader.load_default_questions()
  let questions = question_loader.get_questions(db, "shape", 1)

  // Verify questions extract into ShapeSection fields
  let extract_fields =
    questions
    |> list_flat_map(fn(q) { q.extract_into })

  // Should include key ShapeSection fields
  should.be_true(list_contains(extract_fields, "features"))
  should.be_true(list_contains(extract_fields, "critical_path"))
  should.be_true(list_contains(extract_fields, "mvp_slice"))
  should.be_true(list_contains(extract_fields, "shortcuts"))
  should.be_true(list_contains(extract_fields, "post_mvp"))
  should.be_true(list_contains(extract_fields, "validation_moment"))
}

// Helper functions
fn list_length(list: List(a)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}

fn list_all(list: List(a), predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> True
    [head, ..rest] ->
      case predicate(head) {
        False -> False
        True -> list_all(rest, predicate)
      }
  }
}

fn list_contains(list: List(a), item: a) -> Bool {
  case list {
    [] -> False
    [head, ..rest] ->
      case head == item {
        True -> True
        False -> list_contains(rest, item)
      }
  }
}

fn string_starts_with(string: String, prefix: String) -> Bool {
  string.starts_with(string, prefix)
}

fn list_flat_map(list: List(a), f: fn(a) -> List(b)) -> List(b) {
  case list {
    [] -> []
    [head, ..rest] -> list_append(f(head), list_flat_map(rest, f))
  }
}

fn list_append(a: List(a), b: List(a)) -> List(a) {
  case a {
    [] -> b
    [head, ..rest] -> [head, ..list_append(rest, b)]
  }
}
