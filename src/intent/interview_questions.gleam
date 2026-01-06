/// Interview Questions Library
/// Types and loader for interview questions
/// Questions are defined in schema/questions.cue

import gleam/option
import intent/question_loader
import intent/question_types.{type Question, HappyPath, Critical, Question, User}

/// Get all questions for a specific profile and round
/// Loads from CUE file, falls back to minimal questions if loading fails
pub fn get_questions_for_round(profile: String, round: Int) -> List(Question) {
  case question_loader.load_default_questions() {
    Ok(db) -> question_loader.get_questions(db, profile, round)
    Error(_) -> fallback_questions(profile, round)
  }
}

/// Get the next unasked question in the current round
pub fn get_next_question(
  profile: String,
  round: Int,
  answered_ids: List(String),
) -> option.Option(Question) {
  let questions = get_questions_for_round(profile, round)
  case find_first_unanswered(questions, answered_ids) {
    Ok(q) -> option.Some(q)
    Error(_) -> option.None
  }
}

fn find_first_unanswered(
  questions: List(Question),
  answered: List(String),
) -> Result(Question, Nil) {
  case questions {
    [] -> Error(Nil)
    [q, ..rest] -> {
      case list_contains(answered, q.id) {
        True -> find_first_unanswered(rest, answered)
        False -> Ok(q)
      }
    }
  }
}

fn list_contains(list: List(String), item: String) -> Bool {
  case list {
    [] -> False
    [head, ..tail] -> {
      case head == item {
        True -> True
        False -> list_contains(tail, item)
      }
    }
  }
}

/// Fallback questions if CUE loading fails
/// Returns a minimal set to allow the interview to function
fn fallback_questions(profile: String, round: Int) -> List(Question) {
  case round {
    1 -> [
      Question(
        id: "fallback-1",
        round: 1,
        perspective: User,
        category: HappyPath,
        priority: Critical,
        question: "In one sentence, what should this " <> profile <> " do?",
        context: "Questions could not be loaded from CUE. Using fallback.",
        example: "Describe the core purpose",
        expected_type: "text",
        extract_into: ["name"],
        depends_on: [],
        blocks: [],
      ),
    ]
    _ -> []
  }
}
