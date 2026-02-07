import intent/interview
import intent/interview_storage
import intent/plan_mode

/// The `plan-next` command - determine next action for session execution
pub fn plan_next_command(session_id: String) -> Result(String, String) {
  let jsonl_path = ".intent/session-" <> session_id <> ".cue"
  case interview_storage.get_session_from_jsonl(jsonl_path, session_id) {
    Error(err) -> Error("Session not found: " <> err)
    Ok(session) -> {
      case plan_mode.compute_plan(session_id) {
        Error(err) -> Error(plan_mode.format_error(err))
        Ok(plan) -> {
          let gated_plan = interview.apply_phase_gating(session, plan)

          case gated_plan.blockers {
            [blocker, ..] -> Ok("blocked: " <> blocker)
            [] -> {
              case gated_plan.phases {
                [] -> Ok("done")
                [first_phase, ..] -> {
                  case first_phase.beads {
                    [] -> Ok("done")
                    [first_bead, ..] -> {
                      Ok(
                        "execute_bead: "
                        <> first_bead.id
                        <> " ("
                        <> first_bead.title
                        <> ")",
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
