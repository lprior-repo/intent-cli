//// Tests for interview engine

import gleam/list
import gleeunit
import gleeunit/should
import intent/interview.{Api, Discovery, create_session}

pub fn main() {
  gleeunit.main()
}

pub fn create_session_initializes_correctly_test() {
  let id = "test-session-1"
  let profile = Api
  let timestamp = "2024-01-01T00:00:00Z"
  let session = create_session(id, profile, timestamp)

  session.id
  |> should.equal(id)

  session.profile
  |> should.equal(Api)

  session.stage
  |> should.equal(Discovery)

  session.rounds_completed
  |> should.equal(0)

  session.answers
  |> list.length
  |> should.equal(0)
}
