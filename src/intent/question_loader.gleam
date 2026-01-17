/// Question Loader
/// Loads interview questions from CUE files at runtime
/// Supports custom questions from .intent/custom-questions.cue
import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/question_types.{
  type Perspective, type Question, type QuestionCategory, type QuestionPriority,
  Business, Constraint, Critical, Dependency, Developer, EdgeCase, ErrorCase,
  HappyPath, Important, NiceTohave, NonFunctional, Ops, Question, Security, User,
}
import intent/security
import shellout

/// Error types for question loading
pub type QuestionLoadError {
  FileNotFound(path: String)
  CueExportError(message: String)
  JsonParseError(message: String)
  QuestionParseError(message: String)
  SecurityError(message: String)
}

/// Loaded questions database
pub type QuestionsDatabase {
  QuestionsDatabase(
    api: ProfileQuestions,
    cli: ProfileQuestions,
    event: ProfileQuestions,
    data: ProfileQuestions,
    workflow: ProfileQuestions,
    ui: ProfileQuestions,
    common: CommonQuestions,
  )
}

pub type ProfileQuestions {
  ProfileQuestions(round_1: List(Question), round_2: List(Question))
}

pub type CommonQuestions {
  CommonQuestions(
    round_3: List(Question),
    round_4: List(Question),
    round_5: List(Question),
  )
}

/// Custom questions - optional overrides/additions
pub type CustomQuestions {
  CustomQuestions(
    api: Option(CustomProfileQuestions),
    cli: Option(CustomProfileQuestions),
    event: Option(CustomProfileQuestions),
    data: Option(CustomProfileQuestions),
    workflow: Option(CustomProfileQuestions),
    ui: Option(CustomProfileQuestions),
    common: Option(CustomCommonQuestions),
  )
}

pub type CustomProfileQuestions {
  CustomProfileQuestions(
    round_1: Option(List(Question)),
    round_2: Option(List(Question)),
  )
}

pub type CustomCommonQuestions {
  CustomCommonQuestions(
    round_3: Option(List(Question)),
    round_4: Option(List(Question)),
    round_5: Option(List(Question)),
  )
}

/// Default path for custom questions
const custom_questions_path = ".intent/custom-questions.cue"

/// Load questions from a CUE file
pub fn load_questions(
  path: String,
) -> Result(QuestionsDatabase, QuestionLoadError) {
  // Validate path for security
  case security.validate_file_path(path) {
    Ok(validated_path) -> export_and_parse(validated_path)
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Load questions from the default schema path, merging with custom questions
pub fn load_default_questions() -> Result(QuestionsDatabase, QuestionLoadError) {
  // Load built-in questions first
  case load_questions("schema/questions.cue") {
    Ok(db) -> {
      // Try to load custom questions and merge them
      case load_custom_questions(custom_questions_path) {
        Ok(custom) -> Ok(merge_custom_questions(db, custom))
        Error(_) -> Ok(db)
        // No custom questions or error loading - use defaults
      }
    }
    Error(e) -> Error(e)
  }
}

/// Load custom questions from a path
pub fn load_custom_questions(
  path: String,
) -> Result(CustomQuestions, QuestionLoadError) {
  // Validate path for security
  case security.validate_file_path(path) {
    Ok(validated_path) -> export_and_parse_custom(validated_path)
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

fn export_and_parse_custom(
  path: String,
) -> Result(CustomQuestions, QuestionLoadError) {
  case
    shellout.command("cue", ["export", path, "-e", "custom_questions"], ".", [])
  {
    Ok(json_str) -> parse_custom_questions_json(json_str)
    Error(#(_, stderr)) -> Error(CueExportError(stderr))
  }
}

fn parse_custom_questions_json(
  json_str: String,
) -> Result(CustomQuestions, QuestionLoadError) {
  case json.decode(json_str, dynamic.dynamic) {
    Ok(data) -> parse_custom_database(data)
    Error(_) -> Error(JsonParseError("Failed to decode custom questions JSON"))
  }
}

fn parse_custom_database(
  data: Dynamic,
) -> Result(CustomQuestions, QuestionLoadError) {
  let decoder =
    dynamic.decode7(
      CustomQuestions,
      dynamic.optional_field("api", parse_custom_profile_questions),
      dynamic.optional_field("cli", parse_custom_profile_questions),
      dynamic.optional_field("event", parse_custom_profile_questions),
      dynamic.optional_field("data", parse_custom_profile_questions),
      dynamic.optional_field("workflow", parse_custom_profile_questions),
      dynamic.optional_field("ui", parse_custom_profile_questions),
      dynamic.optional_field("common", parse_custom_common_questions),
    )

  case decoder(data) {
    Ok(custom) -> Ok(custom)
    Error(errs) ->
      Error(QuestionParseError(
        "Failed to parse custom questions: " <> format_decode_errors(errs),
      ))
  }
}

fn parse_custom_profile_questions(
  data: Dynamic,
) -> Result(CustomProfileQuestions, List(dynamic.DecodeError)) {
  dynamic.decode2(
    CustomProfileQuestions,
    dynamic.optional_field("round_1", dynamic.list(parse_question)),
    dynamic.optional_field("round_2", dynamic.list(parse_question)),
  )(data)
}

fn parse_custom_common_questions(
  data: Dynamic,
) -> Result(CustomCommonQuestions, List(dynamic.DecodeError)) {
  dynamic.decode3(
    CustomCommonQuestions,
    dynamic.optional_field("round_3", dynamic.list(parse_question)),
    dynamic.optional_field("round_4", dynamic.list(parse_question)),
    dynamic.optional_field("round_5", dynamic.list(parse_question)),
  )(data)
}

/// Merge custom questions with built-in questions
/// Custom questions with same ID override built-ins; new IDs are added
fn merge_custom_questions(
  db: QuestionsDatabase,
  custom: CustomQuestions,
) -> QuestionsDatabase {
  QuestionsDatabase(
    api: merge_profile(db.api, custom.api),
    cli: merge_profile(db.cli, custom.cli),
    event: merge_profile(db.event, custom.event),
    data: merge_profile(db.data, custom.data),
    workflow: merge_profile(db.workflow, custom.workflow),
    ui: merge_profile(db.ui, custom.ui),
    common: merge_common(db.common, custom.common),
  )
}

fn merge_profile(
  base: ProfileQuestions,
  custom: Option(CustomProfileQuestions),
) -> ProfileQuestions {
  case custom {
    None -> base
    Some(c) ->
      ProfileQuestions(
        round_1: merge_question_list(base.round_1, c.round_1),
        round_2: merge_question_list(base.round_2, c.round_2),
      )
  }
}

fn merge_common(
  base: CommonQuestions,
  custom: Option(CustomCommonQuestions),
) -> CommonQuestions {
  case custom {
    None -> base
    Some(c) ->
      CommonQuestions(
        round_3: merge_question_list(base.round_3, c.round_3),
        round_4: merge_question_list(base.round_4, c.round_4),
        round_5: merge_question_list(base.round_5, c.round_5),
      )
  }
}

fn merge_question_list(
  base: List(Question),
  custom: Option(List(Question)),
) -> List(Question) {
  case custom {
    None -> base
    Some(custom_questions) -> {
      // Get IDs of custom questions for override detection
      let custom_ids = list.map(custom_questions, fn(q) { q.id })

      // Keep base questions that aren't overridden
      let filtered_base =
        list.filter(base, fn(q) { !list.contains(custom_ids, q.id) })

      // Append custom questions (overrides + new)
      list.append(filtered_base, custom_questions)
    }
  }
}

fn export_and_parse(
  path: String,
) -> Result(QuestionsDatabase, QuestionLoadError) {
  case shellout.command("cue", ["export", path, "-e", "questions"], ".", []) {
    Ok(json_str) -> parse_questions_json(json_str)
    Error(#(_, stderr)) -> Error(CueExportError(stderr))
  }
}

fn parse_questions_json(
  json_str: String,
) -> Result(QuestionsDatabase, QuestionLoadError) {
  case json.decode(json_str, dynamic.dynamic) {
    Ok(data) -> parse_database(data)
    Error(_) -> Error(JsonParseError("Failed to decode JSON"))
  }
}

fn parse_database(data: Dynamic) -> Result(QuestionsDatabase, QuestionLoadError) {
  let decoder =
    dynamic.decode7(
      QuestionsDatabase,
      dynamic.field("api", parse_profile_questions),
      dynamic.field("cli", parse_profile_questions),
      dynamic.field("event", parse_profile_questions),
      dynamic.field("data", parse_profile_questions),
      dynamic.field("workflow", parse_profile_questions),
      dynamic.field("ui", parse_profile_questions),
      dynamic.field("common", parse_common_questions),
    )

  case decoder(data) {
    Ok(db) -> Ok(db)
    Error(errs) ->
      Error(QuestionParseError(
        "Failed to parse questions: " <> format_decode_errors(errs),
      ))
  }
}

fn parse_profile_questions(
  data: Dynamic,
) -> Result(ProfileQuestions, List(dynamic.DecodeError)) {
  dynamic.decode2(
    ProfileQuestions,
    dynamic.field("round_1", dynamic.list(parse_question)),
    dynamic.field("round_2", dynamic.list(parse_question)),
  )(data)
}

fn parse_common_questions(
  data: Dynamic,
) -> Result(CommonQuestions, List(dynamic.DecodeError)) {
  dynamic.decode3(
    CommonQuestions,
    dynamic.field("round_3", dynamic.list(parse_question)),
    dynamic.field("round_4", dynamic.list(parse_question)),
    dynamic.field("round_5", dynamic.list(parse_question)),
  )(data)
}

fn parse_question(data: Dynamic) -> Result(Question, List(dynamic.DecodeError)) {
  // Use decode8 + additional fields
  let base_decoder =
    dynamic.decode8(
      fn(id, round, perspective, category, priority, question, context, example) {
        #(
          id,
          round,
          perspective,
          category,
          priority,
          question,
          context,
          example,
        )
      },
      dynamic.field("id", dynamic.string),
      dynamic.field("round", dynamic.int),
      dynamic.field("perspective", dynamic.string),
      dynamic.field("category", dynamic.string),
      dynamic.field("priority", dynamic.string),
      dynamic.field("question", dynamic.string),
      dynamic.field("context", dynamic.string),
      dynamic.field("example", dynamic.string),
    )

  let extra_decoder =
    dynamic.decode4(
      fn(expected_type, extract_into, depends_on, blocks) {
        #(expected_type, extract_into, depends_on, blocks)
      },
      dynamic.optional_field("expected_type", dynamic.string),
      dynamic.optional_field("extract_into", dynamic.list(dynamic.string)),
      dynamic.optional_field("depends_on", dynamic.list(dynamic.string)),
      dynamic.optional_field("blocks", dynamic.list(dynamic.string)),
    )

  case base_decoder(data), extra_decoder(data) {
    Ok(#(
      id,
      round,
      perspective_str,
      category_str,
      priority_str,
      question,
      context,
      example,
    )),
      Ok(#(expected_type_opt, extract_into_opt, depends_on_opt, blocks_opt))
    -> {
      let perspective = parse_perspective(perspective_str)
      let category = parse_category(category_str)
      let priority = parse_priority(priority_str)

      Ok(Question(
        id: id,
        round: round,
        perspective: perspective,
        category: category,
        priority: priority,
        question: question,
        context: context,
        example: example,
        expected_type: option.unwrap(expected_type_opt, "text"),
        extract_into: option.unwrap(extract_into_opt, []),
        depends_on: option.unwrap(depends_on_opt, []),
        blocks: option.unwrap(blocks_opt, []),
      ))
    }
    Error(errs), _ -> Error(errs)
    _, Error(errs) -> Error(errs)
  }
}

fn parse_perspective(s: String) -> Perspective {
  case string.lowercase(s) {
    "user" -> User
    "developer" -> Developer
    "ops" -> Ops
    "security" -> Security
    "business" -> Business
    _ -> User
  }
}

fn parse_category(s: String) -> QuestionCategory {
  case string.lowercase(s) {
    "happy_path" -> HappyPath
    "error_case" -> ErrorCase
    "edge_case" -> EdgeCase
    "constraint" -> Constraint
    "dependency" -> Dependency
    "nonfunctional" -> NonFunctional
    _ -> HappyPath
  }
}

fn parse_priority(s: String) -> QuestionPriority {
  case string.lowercase(s) {
    "critical" -> Critical
    "important" -> Important
    "nice_to_have" -> NiceTohave
    _ -> Important
  }
}

fn format_decode_errors(errors: List(dynamic.DecodeError)) -> String {
  errors
  |> list.map(fn(e) {
    "Expected " <> e.expected <> " at " <> string.join(e.path, ".")
  })
  |> string.join(", ")
}

/// Get questions for a specific profile and round from a loaded database
pub fn get_questions(
  db: QuestionsDatabase,
  profile: String,
  round: Int,
) -> List(Question) {
  case profile, round {
    "api", 1 -> db.api.round_1
    "api", 2 -> db.api.round_2
    "cli", 1 -> db.cli.round_1
    "cli", 2 -> db.cli.round_2
    "event", 1 -> db.event.round_1
    "event", 2 -> db.event.round_2
    "data", 1 -> db.data.round_1
    "data", 2 -> db.data.round_2
    "workflow", 1 -> db.workflow.round_1
    "workflow", 2 -> db.workflow.round_2
    "ui", 1 -> db.ui.round_1
    "ui", 2 -> db.ui.round_2
    _, 3 -> db.common.round_3
    _, 4 -> db.common.round_4
    _, 5 -> db.common.round_5
    _, _ -> []
  }
}

/// Format a QuestionLoadError as a human-readable string (legacy)
pub fn format_error(error: QuestionLoadError) -> String {
  case error {
    FileNotFound(path) -> "Questions file not found: " <> path
    CueExportError(msg) -> "CUE export failed:\n" <> msg
    JsonParseError(msg) -> "JSON parse error: " <> msg
    QuestionParseError(msg) -> "Question parse error: " <> msg
    SecurityError(msg) -> msg
  }
}

// =============================================================================
// AI-FRIENDLY ERROR FORMATTING
// =============================================================================

/// Format error as AI-friendly CUE structure
/// Returns structured error with action, context, suggestion, and recovery steps
pub fn format_error_ai(error: QuestionLoadError) -> String {
  case error {
    FileNotFound(path) -> format_file_not_found_ai(path)
    CueExportError(msg) -> format_cue_export_error_ai(msg)
    JsonParseError(msg) -> format_json_parse_error_ai(msg)
    QuestionParseError(msg) -> format_question_parse_error_ai(msg)
    SecurityError(msg) -> format_security_error_ai(msg)
  }
}

/// Format error as human-readable text with context and recovery steps
pub fn format_error_text(error: QuestionLoadError) -> String {
  case error {
    FileNotFound(path) -> format_file_not_found_text(path)
    CueExportError(msg) -> format_cue_export_error_text(msg)
    JsonParseError(msg) -> format_json_parse_error_text(msg)
    QuestionParseError(msg) -> format_question_parse_error_text(msg)
    SecurityError(msg) -> format_security_error_text(msg)
  }
}

/// Format error as JSON for programmatic use
pub fn format_error_json(error: QuestionLoadError) -> String {
  // JSON format is similar to CUE but with valid JSON syntax
  case error {
    FileNotFound(path) -> format_file_not_found_json(path)
    CueExportError(msg) -> format_cue_export_error_json(msg)
    JsonParseError(msg) -> format_json_parse_error_json(msg)
    QuestionParseError(msg) -> format_question_parse_error_json(msg)
    SecurityError(msg) -> format_security_error_json(msg)
  }
}

// -----------------------------------------------------------------------------
// FileNotFound Error Formatters
// -----------------------------------------------------------------------------

fn format_file_not_found_ai(path: String) -> String {
  let is_custom = string.contains(path, "custom-questions")
  let error_type = case is_custom {
    True -> "custom_questions_not_found"
    False -> "schema_file_not_found"
  }

  let message = case is_custom {
    True -> "Custom questions file not found: " <> path
    False -> "Questions schema file not found: " <> path
  }

  let suggestion = case is_custom {
    True ->
      "Custom questions are optional. Either create the file or remove the --custom flag"
    False -> "Install the questions schema or check the installation path"
  }

  let recovery_steps = case is_custom {
    True ->
      "        \"Custom questions are optional - Intent will use built-in questions\",\n"
      <> "        \"Create .intent/custom-questions.cue if you want custom questions\",\n"
      <> "        \"Format: custom_questions: { api: { round_1: [...] } }\",\n"
      <> "        \"See schema/questions.cue for question structure examples\""
    False ->
      "        \"Verify Intent is properly installed with: moon run :install\",\n"
      <> "        \"Check that schema/questions.cue exists in the Intent directory\",\n"
      <> "        \"Re-install Intent if the schema file is missing\",\n"
      <> "        \"Ensure INTENT_HOME or installation path is correct\""
  }

  "{\n"
  <> "    action: \"file_error\"\n"
  <> "    error: {\n"
  <> "        type: \""
  <> error_type
  <> "\"\n"
  <> "        message: \""
  <> message
  <> "\"\n"
  <> "        context: {\n"
  <> "            path: \""
  <> path
  <> "\"\n"
  <> "            file_type: \"CUE questions schema\"\n"
  <> "            is_optional: "
  <> case is_custom {
    True -> "true"
    False -> "false"
  }
  <> "\n"
  <> "        }\n"
  <> "    }\n"
  <> "    suggestion: \""
  <> suggestion
  <> "\"\n"
  <> "    recovery: [\n"
  <> recovery_steps
  <> "\n"
  <> "    ]\n"
  <> "}"
}

fn format_file_not_found_text(path: String) -> String {
  let is_custom = string.contains(path, "custom-questions")

  let error_msg = case is_custom {
    True -> "Custom questions file not found: " <> path
    False -> "Questions schema file not found: " <> path
  }

  let context = case is_custom {
    True ->
      "Context:\n"
      <> "  path: "
      <> path
      <> "\n"
      <> "  file_type: CUE questions schema\n"
      <> "  is_optional: true\n"
    False ->
      "Context:\n"
      <> "  path: "
      <> path
      <> "\n"
      <> "  file_type: CUE questions schema\n"
      <> "  is_optional: false\n"
  }

  let suggestion = case is_custom {
    True ->
      "Suggestion: Custom questions are optional. Either create the file or remove the --custom flag"
    False ->
      "Suggestion: Install the questions schema or check the installation path"
  }

  let recovery = case is_custom {
    True ->
      "Recovery Steps:\n"
      <> "  1. Custom questions are optional - Intent will use built-in questions\n"
      <> "  2. Create .intent/custom-questions.cue if you want custom questions\n"
      <> "  3. Format: custom_questions: { api: { round_1: [...] } }\n"
      <> "  4. See schema/questions.cue for question structure examples"
    False ->
      "Recovery Steps:\n"
      <> "  1. Verify Intent is properly installed with: moon run :install\n"
      <> "  2. Check that schema/questions.cue exists in the Intent directory\n"
      <> "  3. Re-install Intent if the schema file is missing\n"
      <> "  4. Ensure INTENT_HOME or installation path is correct"
  }

  "Error: "
  <> error_msg
  <> "\n\n"
  <> context
  <> "\n"
  <> suggestion
  <> "\n\n"
  <> recovery
}

fn format_file_not_found_json(path: String) -> String {
  let is_custom = string.contains(path, "custom-questions")
  let error_type = case is_custom {
    True -> "custom_questions_not_found"
    False -> "schema_file_not_found"
  }

  let message = case is_custom {
    True -> "Custom questions file not found: " <> path
    False -> "Questions schema file not found: " <> path
  }

  "{\n"
  <> "    \"action\": \"file_error\",\n"
  <> "    \"error\": {\n"
  <> "        \"type\": \""
  <> error_type
  <> "\",\n"
  <> "        \"message\": \""
  <> escape_json_string(message)
  <> "\",\n"
  <> "        \"context\": {\n"
  <> "            \"path\": \""
  <> escape_json_string(path)
  <> "\",\n"
  <> "            \"file_type\": \"CUE questions schema\",\n"
  <> "            \"is_optional\": "
  <> case is_custom {
    True -> "true"
    False -> "false"
  }
  <> "\n"
  <> "        }\n"
  <> "    }\n"
  <> "}"
}

// -----------------------------------------------------------------------------
// CueExportError Error Formatters
// -----------------------------------------------------------------------------

fn format_cue_export_error_ai(msg: String) -> String {
  let error_type = detect_cue_error_type(msg)
  let suggestion = get_cue_error_suggestion(error_type)
  let recovery_steps = get_cue_error_recovery(error_type)

  "{\n"
  <> "    action: \"cue_error\"\n"
  <> "    error: {\n"
  <> "        type: \""
  <> error_type
  <> "\"\n"
  <> "        message: \"CUE export failed\"\n"
  <> "        context: {\n"
  <> "            cue_error: \""
  <> escape_cue_string(truncate_message(msg, 200))
  <> "\"\n"
  <> "            format: \"CUE\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "    suggestion: \""
  <> suggestion
  <> "\"\n"
  <> "    recovery: [\n"
  <> recovery_steps
  <> "\n"
  <> "    ]\n"
  <> "}"
}

fn format_cue_export_error_text(msg: String) -> String {
  let error_type = detect_cue_error_type(msg)
  let suggestion = get_cue_error_suggestion(error_type)
  let recovery_text = get_cue_error_recovery_text(error_type)

  "Error: CUE export failed\n\n"
  <> "Context:\n"
  <> "  cue_error: "
  <> truncate_message(msg, 200)
  <> "\n"
  <> "  format: CUE\n\n"
  <> "Suggestion: "
  <> suggestion
  <> "\n\n"
  <> recovery_text
}

fn format_cue_export_error_json(msg: String) -> String {
  let error_type = detect_cue_error_type(msg)

  "{\n"
  <> "    \"action\": \"cue_error\",\n"
  <> "    \"error\": {\n"
  <> "        \"type\": \""
  <> error_type
  <> "\",\n"
  <> "        \"message\": \"CUE export failed\",\n"
  <> "        \"context\": {\n"
  <> "            \"cue_error\": \""
  <> escape_json_string(truncate_message(msg, 200))
  <> "\",\n"
  <> "            \"format\": \"CUE\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "}"
}

fn detect_cue_error_type(msg: String) -> String {
  let lower = string.lowercase(msg)
  case
    string.contains(lower, "undefined field")
    || string.contains(lower, "not found")
  {
    True -> "cue_missing_field"
    False ->
      case string.contains(lower, "conflicting values") {
        True -> "cue_type_conflict"
        False -> "cue_export_failed"
      }
  }
}

fn get_cue_error_suggestion(error_type: String) -> String {
  case error_type {
    "cue_missing_field" ->
      "Add the missing field to your questions schema or check for typos"
    "cue_type_conflict" ->
      "Fix type conflicts in your CUE schema - ensure values match their types"
    _ -> "Fix CUE syntax errors in your questions schema"
  }
}

fn get_cue_error_recovery(error_type: String) -> String {
  case error_type {
    "cue_missing_field" ->
      "        \"Check that all required fields are present in the schema\",\n"
      <> "        \"Required profiles: api, cli, event, data, workflow, ui, common\",\n"
      <> "        \"Each profile needs: round_1, round_2 (round_3/4/5 for common)\",\n"
      <> "        \"Test with: cue export schema/questions.cue -e questions\""
    "cue_type_conflict" ->
      "        \"Check that 'round' is an integer (1-5), not a string\",\n"
      <> "        \"Verify all string fields are quoted\",\n"
      <> "        \"Ensure arrays use [...] syntax\",\n"
      <> "        \"Test with: cue vet schema/questions.cue\""
    _ ->
      "        \"Run: cue export schema/questions.cue -e questions\",\n"
      <> "        \"Check CUE syntax with: cue vet schema/questions.cue\",\n"
      <> "        \"Review schema/questions.cue for examples\",\n"
      <> "        \"Ensure all required fields match the #Question schema\""
  }
}

fn get_cue_error_recovery_text(error_type: String) -> String {
  case error_type {
    "cue_missing_field" ->
      "Recovery Steps:\n"
      <> "  1. Check that all required fields are present in the schema\n"
      <> "  2. Required profiles: api, cli, event, data, workflow, ui, common\n"
      <> "  3. Each profile needs: round_1, round_2 (round_3/4/5 for common)\n"
      <> "  4. Test with: cue export schema/questions.cue -e questions"
    "cue_type_conflict" ->
      "Recovery Steps:\n"
      <> "  1. Check that 'round' is an integer (1-5), not a string\n"
      <> "  2. Verify all string fields are quoted\n"
      <> "  3. Ensure arrays use [...] syntax\n"
      <> "  4. Test with: cue vet schema/questions.cue"
    _ ->
      "Recovery Steps:\n"
      <> "  1. Run: cue export schema/questions.cue -e questions\n"
      <> "  2. Check CUE syntax with: cue vet schema/questions.cue\n"
      <> "  3. Review schema/questions.cue for examples\n"
      <> "  4. Ensure all required fields match the #Question schema"
  }
}

// -----------------------------------------------------------------------------
// JsonParseError Error Formatters
// -----------------------------------------------------------------------------

fn format_json_parse_error_ai(msg: String) -> String {
  "{\n"
  <> "    action: \"parse_error\"\n"
  <> "    error: {\n"
  <> "        type: \"json_decode_failed\"\n"
  <> "        message: \"Failed to decode JSON from CUE export\"\n"
  <> "        context: {\n"
  <> "            parse_error: \""
  <> escape_cue_string(msg)
  <> "\"\n"
  <> "            format: \"JSON\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "    suggestion: \"Test CUE export manually to debug JSON generation\"\n"
  <> "    recovery: [\n"
  <> "        \"Run: cue export schema/questions.cue -e questions > /tmp/questions.json\",\n"
  <> "        \"Validate JSON with: cat /tmp/questions.json | jq .\",\n"
  <> "        \"Check that CUE exports valid JSON without syntax errors\",\n"
  <> "        \"Ensure the 'questions' field exists in the CUE schema\"\n"
  <> "    ]\n"
  <> "}"
}

fn format_json_parse_error_text(msg: String) -> String {
  "Error: Failed to decode JSON from CUE export\n\n"
  <> "Context:\n"
  <> "  parse_error: "
  <> msg
  <> "\n"
  <> "  format: JSON\n\n"
  <> "Suggestion: Test CUE export manually to debug JSON generation\n\n"
  <> "Recovery Steps:\n"
  <> "  1. Run: cue export schema/questions.cue -e questions > /tmp/questions.json\n"
  <> "  2. Validate JSON with: cat /tmp/questions.json | jq .\n"
  <> "  3. Check that CUE exports valid JSON without syntax errors\n"
  <> "  4. Ensure the 'questions' field exists in the CUE schema"
}

fn format_json_parse_error_json(msg: String) -> String {
  "{\n"
  <> "    \"action\": \"parse_error\",\n"
  <> "    \"error\": {\n"
  <> "        \"type\": \"json_decode_failed\",\n"
  <> "        \"message\": \"Failed to decode JSON from CUE export\",\n"
  <> "        \"context\": {\n"
  <> "            \"parse_error\": \""
  <> escape_json_string(msg)
  <> "\",\n"
  <> "            \"format\": \"JSON\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "}"
}

// -----------------------------------------------------------------------------
// QuestionParseError Error Formatters
// -----------------------------------------------------------------------------

fn format_question_parse_error_ai(msg: String) -> String {
  let error_type = detect_question_parse_error_type(msg)
  let suggestion = get_question_parse_suggestion(error_type)
  let recovery_steps = get_question_parse_recovery(error_type)

  "{\n"
  <> "    action: \"schema_error\"\n"
  <> "    error: {\n"
  <> "        type: \""
  <> error_type
  <> "\"\n"
  <> "        message: \"Question schema validation failed\"\n"
  <> "        context: {\n"
  <> "            validation_error: \""
  <> escape_cue_string(truncate_message(msg, 150))
  <> "\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "    suggestion: \""
  <> suggestion
  <> "\"\n"
  <> "    recovery: [\n"
  <> recovery_steps
  <> "\n"
  <> "    ]\n"
  <> "}"
}

fn format_question_parse_error_text(msg: String) -> String {
  let error_type = detect_question_parse_error_type(msg)
  let suggestion = get_question_parse_suggestion(error_type)
  let recovery_text = get_question_parse_recovery_text(error_type)

  "Error: Question schema validation failed\n\n"
  <> "Context:\n"
  <> "  validation_error: "
  <> truncate_message(msg, 150)
  <> "\n\n"
  <> "Suggestion: "
  <> suggestion
  <> "\n\n"
  <> recovery_text
}

fn format_question_parse_error_json(msg: String) -> String {
  let error_type = detect_question_parse_error_type(msg)

  "{\n"
  <> "    \"action\": \"schema_error\",\n"
  <> "    \"error\": {\n"
  <> "        \"type\": \""
  <> error_type
  <> "\",\n"
  <> "        \"message\": \"Question schema validation failed\",\n"
  <> "        \"context\": {\n"
  <> "            \"validation_error\": \""
  <> escape_json_string(truncate_message(msg, 150))
  <> "\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "}"
}

fn detect_question_parse_error_type(msg: String) -> String {
  let lower = string.lowercase(msg)
  case
    string.contains(lower, "api")
    || string.contains(lower, "cli")
    || string.contains(lower, "event")
    || string.contains(lower, "data")
    || string.contains(lower, "workflow")
    || string.contains(lower, "ui")
  {
    True -> "invalid_profile_structure"
    False ->
      case
        string.contains(lower, "round_3")
        || string.contains(lower, "round_4")
        || string.contains(lower, "round_5")
      {
        True -> "invalid_round_structure"
        False -> "invalid_question_format"
      }
  }
}

fn get_question_parse_suggestion(error_type: String) -> String {
  case error_type {
    "invalid_profile_structure" ->
      "Fix the profile structure in your questions schema"
    "invalid_round_structure" ->
      "Fix the round structure in the common questions section"
    _ -> "Fix the question format to match the required schema"
  }
}

fn get_question_parse_recovery(error_type: String) -> String {
  case error_type {
    "invalid_profile_structure" ->
      "        \"Required profiles: api, cli, event, data, workflow, ui\",\n"
      <> "        \"Each profile must have: round_1 and round_2\",\n"
      <> "        \"Each round is a list of questions: [...#Question]\",\n"
      <> "        \"See schema/questions.cue for complete examples\""
    "invalid_round_structure" ->
      "        \"The 'common' section must have: round_3, round_4, round_5\",\n"
      <> "        \"Each round is a list of questions: [...#Question]\",\n"
      <> "        \"Common questions are asked across all profiles\",\n"
      <> "        \"See schema/questions.cue for structure examples\""
    _ ->
      "        \"Required fields: id, round, perspective, category, priority, question, context, example\",\n"
      <> "        \"Valid perspectives: user, developer, ops, security, business\",\n"
      <> "        \"Valid categories: happy_path, error_case, edge_case, constraint, dependency, nonfunctional\",\n"
      <> "        \"Valid priorities: critical, important, nice_to_have\""
  }
}

fn get_question_parse_recovery_text(error_type: String) -> String {
  case error_type {
    "invalid_profile_structure" ->
      "Recovery Steps:\n"
      <> "  1. Required profiles: api, cli, event, data, workflow, ui\n"
      <> "  2. Each profile must have: round_1 and round_2\n"
      <> "  3. Each round is a list of questions: [...#Question]\n"
      <> "  4. See schema/questions.cue for complete examples"
    "invalid_round_structure" ->
      "Recovery Steps:\n"
      <> "  1. The 'common' section must have: round_3, round_4, round_5\n"
      <> "  2. Each round is a list of questions: [...#Question]\n"
      <> "  3. Common questions are asked across all profiles\n"
      <> "  4. See schema/questions.cue for structure examples"
    _ ->
      "Recovery Steps:\n"
      <> "  1. Required fields: id, round, perspective, category, priority, question, context, example\n"
      <> "  2. Valid perspectives: user, developer, ops, security, business\n"
      <> "  3. Valid categories: happy_path, error_case, edge_case, constraint, dependency, nonfunctional\n"
      <> "  4. Valid priorities: critical, important, nice_to_have"
  }
}

// -----------------------------------------------------------------------------
// SecurityError Error Formatters
// -----------------------------------------------------------------------------

fn format_security_error_ai(msg: String) -> String {
  "{\n"
  <> "    action: \"security_error\"\n"
  <> "    error: {\n"
  <> "        type: \"security_validation_failed\"\n"
  <> "        message: \"Security validation failed for file path\"\n"
  <> "        context: {\n"
  <> "            security_error: \""
  <> escape_cue_string(msg)
  <> "\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "    suggestion: \"Use safe file paths without path traversal or suspicious characters\"\n"
  <> "    recovery: [\n"
  <> "        \"Avoid path traversal sequences like ../ or ..\\\\\",\n"
  <> "        \"Use relative or absolute paths without suspicious patterns\",\n"
  <> "        \"Ensure the file path is within the project directory\",\n"
  <> "        \"Check for null bytes or other invalid characters in the path\"\n"
  <> "    ]\n"
  <> "}"
}

fn format_security_error_text(msg: String) -> String {
  "Error: Security validation failed for file path\n\n"
  <> "Context:\n"
  <> "  security_error: "
  <> msg
  <> "\n\n"
  <> "Suggestion: Use safe file paths without path traversal or suspicious characters\n\n"
  <> "Recovery Steps:\n"
  <> "  1. Avoid path traversal sequences like ../ or ..\\\n"
  <> "  2. Use relative or absolute paths without suspicious patterns\n"
  <> "  3. Ensure the file path is within the project directory\n"
  <> "  4. Check for null bytes or other invalid characters in the path"
}

fn format_security_error_json(msg: String) -> String {
  "{\n"
  <> "    \"action\": \"security_error\",\n"
  <> "    \"error\": {\n"
  <> "        \"type\": \"security_validation_failed\",\n"
  <> "        \"message\": \"Security validation failed for file path\",\n"
  <> "        \"context\": {\n"
  <> "            \"security_error\": \""
  <> escape_json_string(msg)
  <> "\"\n"
  <> "        }\n"
  <> "    }\n"
  <> "}"
}

// -----------------------------------------------------------------------------
// Helper Functions
// -----------------------------------------------------------------------------

/// Escape special characters for JSON strings
fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
  |> string.replace("\r", "\\r")
}

/// Escape special characters for CUE strings (similar to JSON but used in CUE output)
fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

/// Truncate long messages to a maximum length
fn truncate_message(msg: String, max_length: Int) -> String {
  case string.length(msg) > max_length {
    True -> string.slice(msg, 0, max_length) <> "..."
    False -> msg
  }
}
