/// Interview Questions Library
/// Hardcoded questions for each round and perspective
/// In production, this would load from questions.cue

import gleam/option.{type Option}

// Question types duplicated here to avoid circular imports
pub type Perspective {
  User
  Developer
  Ops
  Security
  Business
}

pub type QuestionCategory {
  HappyPath
  ErrorCase
  EdgeCase
  Constraint
  Dependency
  NonFunctional
}

pub type QuestionPriority {
  Critical
  Important
  NiceTohave
}

pub type Question {
  Question(
    id: String,
    round: Int,
    perspective: Perspective,
    category: QuestionCategory,
    priority: QuestionPriority,
    question: String,
    context: String,
    example: String,
    expected_type: String,
    extract_into: List(String),
    depends_on: List(String),
    blocks: List(String),
  )
}

/// Get all questions for a specific profile and round
pub fn get_questions_for_round(
  profile: String,
  round: Int,
) -> List(Question) {
  case #(profile, round) {
    #("api", 1) -> round_1_api()
    #("cli", 1) -> round_1_cli()
    #("event", 1) -> round_1_event()
    #("data", 1) -> round_1_data()
    #("workflow", 1) -> round_1_workflow()
    #("ui", 1) -> round_1_ui()
    #("api", 2) -> round_2_api()
    #("cli", 2) -> round_2_cli()
    #("event", 2) -> round_2_event()
    #("data", 2) -> round_2_data()
    #("workflow", 2) -> round_2_workflow()
    #("ui", 2) -> round_2_ui()
    #(_, 3) -> round_3_common()
    #(_, 4) -> round_4_common()
    #(_, 5) -> round_5_common()
    _ -> []
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

// ============================================================================
// ROUND 1: CORE INTENT (What are we building?)
// ============================================================================

fn round_1_api() -> List(Question) {
  [
    Question(
      id: "r1-user-api-1",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "In one sentence, what should this API do?",
      context: "We're starting with the core intent. Give us the simplest possible description.",
      example: "Allow users to log in with email and password",
      expected_type: "text",
      extract_into: ["name"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-user-api-2",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "Who will use this API? What are they trying to accomplish?",
      context: "Understanding your audience helps us design the right behavior.",
      example: "Mobile app users, web frontend, and third-party integrations",
      expected_type: "text",
      extract_into: ["audience", "success_criteria"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-user-api-3",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "Walk me through the happy path. What happens step-by-step?",
      context: "Describe the ideal flow from start to finish. Don't worry about errors yet.",
      example: "Client sends POST /login with email/password → validates → returns JWT token",
      expected_type: "text",
      extract_into: ["behaviors"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-dev-api-1",
      round: 1,
      perspective: Developer,
      category: Constraint,
      priority: Important,
      question: "What data model does this operate on? List the key entities.",
      context: "Understanding the domain helps us catch inconsistencies.",
      example: "Users (id, email, password_hash), Tokens (token, user_id, expires_at)",
      expected_type: "text",
      extract_into: ["entities"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-security-api-1",
      round: 1,
      perspective: Security,
      category: Constraint,
      priority: Critical,
      question: "What kind of authentication does this need?",
      context: "Auth method cascades through the whole design.",
      example: "JWT for mobile, session cookies for web, API keys for server-to-server",
      expected_type: "text",
      extract_into: ["auth_method"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_1_cli() -> List(Question) {
  [
    Question(
      id: "r1-user-cli-1",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "What's the main command users will run?",
      context: "Start with the primary use case.",
      example: "intent check --file=spec.cue --target=http://api.example.com",
      expected_type: "text",
      extract_into: ["name", "command_name"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-user-cli-2",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "Who are the users of this CLI?",
      context: "Developers? DevOps? QA engineers?",
      example: "API test engineers and DevOps teams testing HTTP endpoints",
      expected_type: "text",
      extract_into: ["audience"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-dev-cli-1",
      round: 1,
      perspective: Developer,
      category: Constraint,
      priority: Important,
      question: "What are the main sub-commands or flags?",
      context: "List the key operations users will perform.",
      example: "check, validate, generate, run, report, export",
      expected_type: "text",
      extract_into: ["behaviors"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_1_event() -> List(Question) {
  [
    Question(
      id: "r1-user-event-1",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "What events will this system emit?",
      context: "Start with the main event types.",
      example: "user.created, user.deleted, order.placed, payment.confirmed",
      expected_type: "text",
      extract_into: ["name", "event_types"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-user-event-2",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "Who will consume these events?",
      context: "What systems care about these events?",
      example: "Email service, analytics pipeline, notification system",
      expected_type: "text",
      extract_into: ["audience"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-dev-event-1",
      round: 1,
      perspective: Developer,
      category: Constraint,
      priority: Important,
      question: "What fields must every event have?",
      context: "The common schema across all events.",
      example: "id, timestamp, type, version, source, payload",
      expected_type: "text",
      extract_into: ["entities"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_1_data() -> List(Question) {
  [
    Question(
      id: "r1-user-data-1",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "What's the primary entity this system manages?",
      context: "The main thing users care about.",
      example: "Users, Products, Orders, Documents",
      expected_type: "text",
      extract_into: ["name", "entities"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-ops-data-1",
      round: 1,
      perspective: Ops,
      category: Constraint,
      priority: Important,
      question: "How long must data be kept? Any retention policies?",
      context: "Affects storage, compliance, archival strategy.",
      example: "Keep indefinitely, delete after 90 days, archive after 1 year",
      expected_type: "text",
      extract_into: ["retention"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_1_workflow() -> List(Question) {
  [
    Question(
      id: "r1-user-workflow-1",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "What are the main workflow states?",
      context: "How does something move from start to finish?",
      example: "Draft → Submitted → Approved → Completed",
      expected_type: "text",
      extract_into: ["states"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-user-workflow-2",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "What transitions between states are allowed?",
      context: "Not all state changes should be valid.",
      example: "Can't go from Approved back to Draft; Draft can skip to Completed if auto-approved",
      expected_type: "text",
      extract_into: ["transitions"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_1_ui() -> List(Question) {
  [
    Question(
      id: "r1-user-ui-1",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "What's the main screen users see first?",
      context: "The entry point to your application.",
      example: "Dashboard showing recent activity, login screen, or home page",
      expected_type: "text",
      extract_into: ["name", "screens"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r1-user-ui-2",
      round: 1,
      perspective: User,
      category: HappyPath,
      priority: Critical,
      question: "What's the core user flow?",
      context: "The happy path through your interface.",
      example: "Log in → View dashboard → Create new item → Confirm → See results",
      expected_type: "text",
      extract_into: ["user_flows"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

// ============================================================================
// ROUND 2: ERROR CASES (What can go wrong?)
// ============================================================================

fn round_2_api() -> List(Question) {
  [
    Question(
      id: "r2-user-api-1",
      round: 2,
      perspective: User,
      category: ErrorCase,
      priority: Critical,
      question: "What's the most common error users will hit?",
      context: "The error that happens 80% of the time.",
      example: "Wrong password, email already exists, invalid token",
      expected_type: "text",
      extract_into: ["error_cases"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r2-security-api-1",
      round: 2,
      perspective: Security,
      category: ErrorCase,
      priority: Critical,
      question: "What information should NEVER leak in error messages?",
      context: "Error responses can expose sensitive information.",
      example: "Don't say 'email exists' - just say 'cannot create account'",
      expected_type: "text",
      extract_into: ["anti_patterns"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r2-dev-api-1",
      round: 2,
      perspective: Developer,
      category: ErrorCase,
      priority: Important,
      question: "What are the HTTP status codes you'll return?",
      context: "200, 400, 401, 403, 404, 409, 500, 503, etc.",
      example: "200 OK, 400 Bad Request, 401 Unauthorized, 409 Conflict",
      expected_type: "text",
      extract_into: ["status_codes"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_2_cli() -> List(Question) {
  [
    Question(
      id: "r2-user-cli-1",
      round: 2,
      perspective: User,
      category: ErrorCase,
      priority: Critical,
      question: "What exit codes should indicate failure?",
      context: "Scripts need to know if a command succeeded or failed.",
      example: "0=success, 1=generic error, 2=usage error, 3=validation failed",
      expected_type: "text",
      extract_into: ["exit_codes"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r2-dev-cli-1",
      round: 2,
      perspective: Developer,
      category: ErrorCase,
      priority: Important,
      question: "How should errors be displayed?",
      context: "stderr vs stdout, verbosity levels, JSON output, etc.",
      example: "Error message to stderr, JSON output to stdout, optional --debug flag",
      expected_type: "text",
      extract_into: ["error_handling"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_2_event() -> List(Question) {
  [
    Question(
      id: "r2-ops-event-1",
      round: 2,
      perspective: Ops,
      category: ErrorCase,
      priority: Critical,
      question: "What happens if an event fails to deliver?",
      context: "Delivery guarantees: at-most-once, at-least-once, exactly-once?",
      example: "Retry up to 3 times with exponential backoff, then dead-letter queue",
      expected_type: "text",
      extract_into: ["delivery_guarantees"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_2_data() -> List(Question) {
  [
    Question(
      id: "r2-user-data-1",
      round: 2,
      perspective: User,
      category: ErrorCase,
      priority: Important,
      question: "Can data be deleted? What happens to related data?",
      context: "Cascading deletes, soft deletes, audit trails.",
      example: "Delete user → archive their orders, keep for 7 years for tax compliance",
      expected_type: "text",
      extract_into: ["deletion_policy"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_2_workflow() -> List(Question) {
  [
    Question(
      id: "r2-user-workflow-1",
      round: 2,
      perspective: User,
      category: ErrorCase,
      priority: Critical,
      question: "What happens if a step fails? How does it recover?",
      context: "Retry, rollback, manual intervention?",
      example: "Payment fails → send email → user can retry → auto-retry after 1 hour",
      expected_type: "text",
      extract_into: ["error_recovery"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

fn round_2_ui() -> List(Question) {
  [
    Question(
      id: "r2-user-ui-1",
      round: 2,
      perspective: User,
      category: ErrorCase,
      priority: Important,
      question: "What error messages will users see?",
      context: "Form validation, API errors, permission denied, etc.",
      example: "Email is required, Password must be 8+ characters, Access denied",
      expected_type: "text",
      extract_into: ["error_messages"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

// ============================================================================
// ROUND 3: EDGE CASES (Where are the boundaries?)
// ============================================================================

fn round_3_common() -> List(Question) {
  [
    Question(
      id: "r3-dev-1",
      round: 3,
      perspective: Developer,
      category: EdgeCase,
      priority: Important,
      question: "What's the maximum size of inputs/payloads?",
      context: "File uploads, API request bodies, database entries.",
      example: "Max file: 100MB, max request body: 10MB, max field length: 255 chars",
      expected_type: "text",
      extract_into: ["size_limits"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r3-ops-1",
      round: 3,
      perspective: Ops,
      category: EdgeCase,
      priority: Important,
      question: "What happens under extreme load?",
      context: "Rate limiting, queuing, graceful degradation?",
      example: "Queue requests, return 429 Too Many Requests, fail fast at 10k req/sec",
      expected_type: "text",
      extract_into: ["load_handling"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r3-security-1",
      round: 3,
      perspective: Security,
      category: EdgeCase,
      priority: Critical,
      question: "What if someone tries to do something they shouldn't?",
      context: "Authorization, privilege escalation, race conditions.",
      example: "User A can't see User B's data, can't modify other users' profiles",
      expected_type: "text",
      extract_into: ["security_rules"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

// ============================================================================
// ROUND 4: SECURITY & COMPLIANCE
// ============================================================================

fn round_4_common() -> List(Question) {
  [
    Question(
      id: "r4-security-1",
      round: 4,
      perspective: Security,
      category: Constraint,
      priority: Critical,
      question: "What data is sensitive and needs encryption?",
      context: "Passwords, tokens, PII, payment info.",
      example: "Passwords (bcrypt), tokens (in-transit), SSN/credit cards (at-rest)",
      expected_type: "text",
      extract_into: ["encryption_requirements"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r4-business-1",
      round: 4,
      perspective: Business,
      category: Constraint,
      priority: Important,
      question: "Are there compliance requirements? (GDPR, HIPAA, PCI, SOC2?)",
      context: "Legal, regulatory, industry standards.",
      example: "GDPR (EU users), PCI DSS (payments), SOC2 (enterprise customers)",
      expected_type: "text",
      extract_into: ["compliance"],
      depends_on: [],
      blocks: [],
    ),
  ]
}

// ============================================================================
// ROUND 5: OPERATIONS (How does it run in production?)
// ============================================================================

fn round_5_common() -> List(Question) {
  [
    Question(
      id: "r5-ops-1",
      round: 5,
      perspective: Ops,
      category: Constraint,
      priority: Important,
      question: "Where will this run? (cloud, on-prem, edge, regions?)",
      context: "Deployment topology affects everything.",
      example: "AWS (us-east-1, eu-west-1), multi-region for GDPR, CDN for static assets",
      expected_type: "text",
      extract_into: ["deployment_target"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r5-ops-2",
      round: 5,
      perspective: Ops,
      category: NonFunctional,
      priority: Critical,
      question: "What's your uptime requirement? (SLA?)",
      context: "99%, 99.9%, 99.99% availability.",
      example: "99.9% (8.76 hours downtime/year), acceptable during maintenance windows",
      expected_type: "text",
      extract_into: ["sla"],
      depends_on: [],
      blocks: [],
    ),
    Question(
      id: "r5-ops-3",
      round: 5,
      perspective: Ops,
      category: NonFunctional,
      priority: Important,
      question: "How will you monitor this? (metrics, logs, alerts?)",
      context: "Observability strategy.",
      example: "Prometheus metrics, ELK logs, PagerDuty alerts on p95 latency > 500ms",
      expected_type: "text",
      extract_into: ["monitoring"],
      depends_on: [],
      blocks: [],
    ),
  ]
}
