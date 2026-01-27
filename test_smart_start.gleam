import intent/interview_storage
import intent/smart_start

fn main() {
  let action =
    smart_start.determine_start_action(".intent/sessions.jsonl", fn(_) {
      Ok("")
    })
  io.debug(action)
}
