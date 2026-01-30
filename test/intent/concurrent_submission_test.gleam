import gleam/dict
import gleam/list
import gleeunit/should
import intent/interview.{Answer}
import intent/interview_storage
import intent/question_types.{Developer}
import simplifile

const test_file = ".beads/test_concurrent_sessions.jsonl"

pub fn concurrent_answer_submissions_data_loss_test() {
  let _ = setup_test_file()

  let session1 = create_session("session-1")
  let session2 = create_session("session-2")
  let session3 = create_session("session-3")

  let result1 = interview_storage.append_session_to_jsonl(session1, test_file)
  let result2 = interview_storage.append_session_to_jsonl(session2, test_file)
  let result3 = interview_storage.append_session_to_jsonl(session3, test_file)

  result1 |> should.be_ok
  result2 |> should.be_ok
  result3 |> should.be_ok

  let sessions_result = interview_storage.list_sessions_from_jsonl(test_file)
  case sessions_result {
    Ok(sessions) -> {
      let session_ids = list.map(sessions, fn(s) { s.id })
      should.equal(list.length(sessions), 3)
      should.be_true(list.contains(session_ids, "session-1"))
      should.be_true(list.contains(session_ids, "session-2"))
      should.be_true(list.contains(session_ids, "session-3"))
    }
    Error(_) -> should.fail()
  }

  cleanup_test_file()
}

pub fn concurrent_same_session_updates_last_write_wins_test() {
  let _ = setup_test_file()

  let session1 = create_session("session-a")
  let _ = interview_storage.append_session_to_jsonl(session1, test_file)

  let answer1 =
    Answer(
      question_id: "q1",
      question_text: "Question 1",
      perspective: Developer,
      round: 1,
      response: "First answer",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2026-01-15T10:00:00Z",
    )

  let answer2 =
    Answer(
      question_id: "q1",
      question_text: "Question 1",
      perspective: Developer,
      round: 1,
      response: "Second answer",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2026-01-15T10:01:00Z",
    )

  let session_updated1 = interview.add_answer(session1, answer1)
  let session_updated2 = interview.add_answer(session1, answer2)

  let result1 =
    interview_storage.append_session_to_jsonl(session_updated1, test_file)
  let result2 =
    interview_storage.append_session_to_jsonl(session_updated2, test_file)

  result1 |> should.be_ok
  result2 |> should.be_ok

  let sessions_result = interview_storage.list_sessions_from_jsonl(test_file)
  case sessions_result {
    Ok(sessions) -> {
      should.equal(list.length(sessions), 1)
      let session = list.first(sessions)
      case session {
        Ok(s) -> {
          should.equal(list.length(s.answers), 1)
          let answer = list.first(s.answers)
          case answer {
            Ok(a) -> should.equal(a.response, "Second answer")
            Error(_) -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  cleanup_test_file()
}

pub fn concurrent_updates_different_sessions_race_test() {
  let _ = setup_test_file()

  let session1 = create_session("concurrent-1")
  let session2 = create_session("concurrent-2")

  let _ = interview_storage.append_session_to_jsonl(session1, test_file)

  let answer =
    Answer(
      question_id: "q1",
      question_text: "Question 1",
      perspective: Developer,
      round: 1,
      response: "Answer",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2026-01-15T10:00:00Z",
    )

  let session2_updated = interview.add_answer(session2, answer)

  let result1 = interview_storage.append_session_to_jsonl(session1, test_file)
  let result2 =
    interview_storage.append_session_to_jsonl(session2_updated, test_file)

  result1 |> should.be_ok
  result2 |> should.be_ok

  let sessions_result = interview_storage.list_sessions_from_jsonl(test_file)
  case sessions_result {
    Ok(sessions) -> {
      let session_ids = list.map(sessions, fn(s) { s.id })
      should.equal(list.length(sessions), 2)
      should.be_true(list.contains(session_ids, "concurrent-1"))
      should.be_true(list.contains(session_ids, "concurrent-2"))
    }
    Error(_) -> should.fail()
  }

  cleanup_test_file()
}

fn setup_test_file() {
  let _ = simplifile.delete(test_file)
  let _ = simplifile.create_directory(".beads")
}

fn cleanup_test_file() {
  let _ = simplifile.delete(test_file)
}

fn create_session(id: String) -> interview.InterviewSession {
  interview.create_session(id, interview.Api, "2026-01-15T00:00:00Z")
}
