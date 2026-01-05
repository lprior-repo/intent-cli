-module(intent@interview_questions).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/interview_questions.gleam").
-export([get_questions_for_round/2, get_next_question/3]).
-export_type([perspective/0, question_category/0, question_priority/0, question/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type perspective() :: user | developer | ops | security | business.

-type question_category() :: happy_path |
    error_case |
    edge_case |
    constraint |
    dependency |
    non_functional.

-type question_priority() :: critical | important | nice_tohave.

-type question() :: {question,
        binary(),
        integer(),
        perspective(),
        question_category(),
        question_priority(),
        binary(),
        binary(),
        binary(),
        binary(),
        list(binary()),
        list(binary()),
        list(binary())}.

-file("src/intent/interview_questions.gleam", 101).
-spec list_contains(list(binary()), binary()) -> boolean().
list_contains(List, Item) ->
    case List of
        [] ->
            false;

        [Head | Tail] ->
            case Head =:= Item of
                true ->
                    true;

                false ->
                    list_contains(Tail, Item)
            end
    end.

-file("src/intent/interview_questions.gleam", 86).
-spec find_first_unanswered(list(question()), list(binary())) -> {ok,
        question()} |
    {error, nil}.
find_first_unanswered(Questions, Answered) ->
    case Questions of
        [] ->
            {error, nil};

        [Q | Rest] ->
            case list_contains(Answered, erlang:element(2, Q)) of
                true ->
                    find_first_unanswered(Rest, Answered);

                false ->
                    {ok, Q}
            end
    end.

-file("src/intent/interview_questions.gleam", 117).
-spec round_1_api() -> list(question()).
round_1_api() ->
    [{question,
            <<"r1-user-api-1"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"In one sentence, what should this API do?"/utf8>>,
            <<"We're starting with the core intent. Give us the simplest possible description."/utf8>>,
            <<"Allow users to log in with email and password"/utf8>>,
            <<"text"/utf8>>,
            [<<"name"/utf8>>],
            [],
            []},
        {question,
            <<"r1-user-api-2"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"Who will use this API? What are they trying to accomplish?"/utf8>>,
            <<"Understanding your audience helps us design the right behavior."/utf8>>,
            <<"Mobile app users, web frontend, and third-party integrations"/utf8>>,
            <<"text"/utf8>>,
            [<<"audience"/utf8>>, <<"success_criteria"/utf8>>],
            [],
            []},
        {question,
            <<"r1-user-api-3"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"Walk me through the happy path. What happens step-by-step?"/utf8>>,
            <<"Describe the ideal flow from start to finish. Don't worry about errors yet."/utf8>>,
            <<"Client sends POST /login with email/password → validates → returns JWT token"/utf8>>,
            <<"text"/utf8>>,
            [<<"behaviors"/utf8>>],
            [],
            []},
        {question,
            <<"r1-dev-api-1"/utf8>>,
            1,
            developer,
            constraint,
            important,
            <<"What data model does this operate on? List the key entities."/utf8>>,
            <<"Understanding the domain helps us catch inconsistencies."/utf8>>,
            <<"Users (id, email, password_hash), Tokens (token, user_id, expires_at)"/utf8>>,
            <<"text"/utf8>>,
            [<<"entities"/utf8>>],
            [],
            []},
        {question,
            <<"r1-security-api-1"/utf8>>,
            1,
            security,
            constraint,
            critical,
            <<"What kind of authentication does this need?"/utf8>>,
            <<"Auth method cascades through the whole design."/utf8>>,
            <<"JWT for mobile, session cookies for web, API keys for server-to-server"/utf8>>,
            <<"text"/utf8>>,
            [<<"auth_method"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 192).
-spec round_1_cli() -> list(question()).
round_1_cli() ->
    [{question,
            <<"r1-user-cli-1"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"What's the main command users will run?"/utf8>>,
            <<"Start with the primary use case."/utf8>>,
            <<"intent check --file=spec.cue --target=http://api.example.com"/utf8>>,
            <<"text"/utf8>>,
            [<<"name"/utf8>>, <<"command_name"/utf8>>],
            [],
            []},
        {question,
            <<"r1-user-cli-2"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"Who are the users of this CLI?"/utf8>>,
            <<"Developers? DevOps? QA engineers?"/utf8>>,
            <<"API test engineers and DevOps teams testing HTTP endpoints"/utf8>>,
            <<"text"/utf8>>,
            [<<"audience"/utf8>>],
            [],
            []},
        {question,
            <<"r1-dev-cli-1"/utf8>>,
            1,
            developer,
            constraint,
            important,
            <<"What are the main sub-commands or flags?"/utf8>>,
            <<"List the key operations users will perform."/utf8>>,
            <<"check, validate, generate, run, report, export"/utf8>>,
            <<"text"/utf8>>,
            [<<"behaviors"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 239).
-spec round_1_event() -> list(question()).
round_1_event() ->
    [{question,
            <<"r1-user-event-1"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"What events will this system emit?"/utf8>>,
            <<"Start with the main event types."/utf8>>,
            <<"user.created, user.deleted, order.placed, payment.confirmed"/utf8>>,
            <<"text"/utf8>>,
            [<<"name"/utf8>>, <<"event_types"/utf8>>],
            [],
            []},
        {question,
            <<"r1-user-event-2"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"Who will consume these events?"/utf8>>,
            <<"What systems care about these events?"/utf8>>,
            <<"Email service, analytics pipeline, notification system"/utf8>>,
            <<"text"/utf8>>,
            [<<"audience"/utf8>>],
            [],
            []},
        {question,
            <<"r1-dev-event-1"/utf8>>,
            1,
            developer,
            constraint,
            important,
            <<"What fields must every event have?"/utf8>>,
            <<"The common schema across all events."/utf8>>,
            <<"id, timestamp, type, version, source, payload"/utf8>>,
            <<"text"/utf8>>,
            [<<"entities"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 286).
-spec round_1_data() -> list(question()).
round_1_data() ->
    [{question,
            <<"r1-user-data-1"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"What's the primary entity this system manages?"/utf8>>,
            <<"The main thing users care about."/utf8>>,
            <<"Users, Products, Orders, Documents"/utf8>>,
            <<"text"/utf8>>,
            [<<"name"/utf8>>, <<"entities"/utf8>>],
            [],
            []},
        {question,
            <<"r1-ops-data-1"/utf8>>,
            1,
            ops,
            constraint,
            important,
            <<"How long must data be kept? Any retention policies?"/utf8>>,
            <<"Affects storage, compliance, archival strategy."/utf8>>,
            <<"Keep indefinitely, delete after 90 days, archive after 1 year"/utf8>>,
            <<"text"/utf8>>,
            [<<"retention"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 319).
-spec round_1_workflow() -> list(question()).
round_1_workflow() ->
    [{question,
            <<"r1-user-workflow-1"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"What are the main workflow states?"/utf8>>,
            <<"How does something move from start to finish?"/utf8>>,
            <<"Draft → Submitted → Approved → Completed"/utf8>>,
            <<"text"/utf8>>,
            [<<"states"/utf8>>],
            [],
            []},
        {question,
            <<"r1-user-workflow-2"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"What transitions between states are allowed?"/utf8>>,
            <<"Not all state changes should be valid."/utf8>>,
            <<"Can't go from Approved back to Draft; Draft can skip to Completed if auto-approved"/utf8>>,
            <<"text"/utf8>>,
            [<<"transitions"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 352).
-spec round_1_ui() -> list(question()).
round_1_ui() ->
    [{question,
            <<"r1-user-ui-1"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"What's the main screen users see first?"/utf8>>,
            <<"The entry point to your application."/utf8>>,
            <<"Dashboard showing recent activity, login screen, or home page"/utf8>>,
            <<"text"/utf8>>,
            [<<"name"/utf8>>, <<"screens"/utf8>>],
            [],
            []},
        {question,
            <<"r1-user-ui-2"/utf8>>,
            1,
            user,
            happy_path,
            critical,
            <<"What's the core user flow?"/utf8>>,
            <<"The happy path through your interface."/utf8>>,
            <<"Log in → View dashboard → Create new item → Confirm → See results"/utf8>>,
            <<"text"/utf8>>,
            [<<"user_flows"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 389).
-spec round_2_api() -> list(question()).
round_2_api() ->
    [{question,
            <<"r2-user-api-1"/utf8>>,
            2,
            user,
            error_case,
            critical,
            <<"What's the most common error users will hit?"/utf8>>,
            <<"The error that happens 80% of the time."/utf8>>,
            <<"Wrong password, email already exists, invalid token"/utf8>>,
            <<"text"/utf8>>,
            [<<"error_cases"/utf8>>],
            [],
            []},
        {question,
            <<"r2-security-api-1"/utf8>>,
            2,
            security,
            error_case,
            critical,
            <<"What information should NEVER leak in error messages?"/utf8>>,
            <<"Error responses can expose sensitive information."/utf8>>,
            <<"Don't say 'email exists' - just say 'cannot create account'"/utf8>>,
            <<"text"/utf8>>,
            [<<"anti_patterns"/utf8>>],
            [],
            []},
        {question,
            <<"r2-dev-api-1"/utf8>>,
            2,
            developer,
            error_case,
            important,
            <<"What are the HTTP status codes you'll return?"/utf8>>,
            <<"200, 400, 401, 403, 404, 409, 500, 503, etc."/utf8>>,
            <<"200 OK, 400 Bad Request, 401 Unauthorized, 409 Conflict"/utf8>>,
            <<"text"/utf8>>,
            [<<"status_codes"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 436).
-spec round_2_cli() -> list(question()).
round_2_cli() ->
    [{question,
            <<"r2-user-cli-1"/utf8>>,
            2,
            user,
            error_case,
            critical,
            <<"What exit codes should indicate failure?"/utf8>>,
            <<"Scripts need to know if a command succeeded or failed."/utf8>>,
            <<"0=success, 1=generic error, 2=usage error, 3=validation failed"/utf8>>,
            <<"text"/utf8>>,
            [<<"exit_codes"/utf8>>],
            [],
            []},
        {question,
            <<"r2-dev-cli-1"/utf8>>,
            2,
            developer,
            error_case,
            important,
            <<"How should errors be displayed?"/utf8>>,
            <<"stderr vs stdout, verbosity levels, JSON output, etc."/utf8>>,
            <<"Error message to stderr, JSON output to stdout, optional --debug flag"/utf8>>,
            <<"text"/utf8>>,
            [<<"error_handling"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 469).
-spec round_2_event() -> list(question()).
round_2_event() ->
    [{question,
            <<"r2-ops-event-1"/utf8>>,
            2,
            ops,
            error_case,
            critical,
            <<"What happens if an event fails to deliver?"/utf8>>,
            <<"Delivery guarantees: at-most-once, at-least-once, exactly-once?"/utf8>>,
            <<"Retry up to 3 times with exponential backoff, then dead-letter queue"/utf8>>,
            <<"text"/utf8>>,
            [<<"delivery_guarantees"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 488).
-spec round_2_data() -> list(question()).
round_2_data() ->
    [{question,
            <<"r2-user-data-1"/utf8>>,
            2,
            user,
            error_case,
            important,
            <<"Can data be deleted? What happens to related data?"/utf8>>,
            <<"Cascading deletes, soft deletes, audit trails."/utf8>>,
            <<"Delete user → archive their orders, keep for 7 years for tax compliance"/utf8>>,
            <<"text"/utf8>>,
            [<<"deletion_policy"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 507).
-spec round_2_workflow() -> list(question()).
round_2_workflow() ->
    [{question,
            <<"r2-user-workflow-1"/utf8>>,
            2,
            user,
            error_case,
            critical,
            <<"What happens if a step fails? How does it recover?"/utf8>>,
            <<"Retry, rollback, manual intervention?"/utf8>>,
            <<"Payment fails → send email → user can retry → auto-retry after 1 hour"/utf8>>,
            <<"text"/utf8>>,
            [<<"error_recovery"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 526).
-spec round_2_ui() -> list(question()).
round_2_ui() ->
    [{question,
            <<"r2-user-ui-1"/utf8>>,
            2,
            user,
            error_case,
            important,
            <<"What error messages will users see?"/utf8>>,
            <<"Form validation, API errors, permission denied, etc."/utf8>>,
            <<"Email is required, Password must be 8+ characters, Access denied"/utf8>>,
            <<"text"/utf8>>,
            [<<"error_messages"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 549).
-spec round_3_common() -> list(question()).
round_3_common() ->
    [{question,
            <<"r3-dev-1"/utf8>>,
            3,
            developer,
            edge_case,
            important,
            <<"What's the maximum size of inputs/payloads?"/utf8>>,
            <<"File uploads, API request bodies, database entries."/utf8>>,
            <<"Max file: 100MB, max request body: 10MB, max field length: 255 chars"/utf8>>,
            <<"text"/utf8>>,
            [<<"size_limits"/utf8>>],
            [],
            []},
        {question,
            <<"r3-ops-1"/utf8>>,
            3,
            ops,
            edge_case,
            important,
            <<"What happens under extreme load?"/utf8>>,
            <<"Rate limiting, queuing, graceful degradation?"/utf8>>,
            <<"Queue requests, return 429 Too Many Requests, fail fast at 10k req/sec"/utf8>>,
            <<"text"/utf8>>,
            [<<"load_handling"/utf8>>],
            [],
            []},
        {question,
            <<"r3-security-1"/utf8>>,
            3,
            security,
            edge_case,
            critical,
            <<"What if someone tries to do something they shouldn't?"/utf8>>,
            <<"Authorization, privilege escalation, race conditions."/utf8>>,
            <<"User A can't see User B's data, can't modify other users' profiles"/utf8>>,
            <<"text"/utf8>>,
            [<<"security_rules"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 600).
-spec round_4_common() -> list(question()).
round_4_common() ->
    [{question,
            <<"r4-security-1"/utf8>>,
            4,
            security,
            constraint,
            critical,
            <<"What data is sensitive and needs encryption?"/utf8>>,
            <<"Passwords, tokens, PII, payment info."/utf8>>,
            <<"Passwords (bcrypt), tokens (in-transit), SSN/credit cards (at-rest)"/utf8>>,
            <<"text"/utf8>>,
            [<<"encryption_requirements"/utf8>>],
            [],
            []},
        {question,
            <<"r4-business-1"/utf8>>,
            4,
            business,
            constraint,
            important,
            <<"Are there compliance requirements? (GDPR, HIPAA, PCI, SOC2?)"/utf8>>,
            <<"Legal, regulatory, industry standards."/utf8>>,
            <<"GDPR (EU users), PCI DSS (payments), SOC2 (enterprise customers)"/utf8>>,
            <<"text"/utf8>>,
            [<<"compliance"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 637).
-spec round_5_common() -> list(question()).
round_5_common() ->
    [{question,
            <<"r5-ops-1"/utf8>>,
            5,
            ops,
            constraint,
            important,
            <<"Where will this run? (cloud, on-prem, edge, regions?)"/utf8>>,
            <<"Deployment topology affects everything."/utf8>>,
            <<"AWS (us-east-1, eu-west-1), multi-region for GDPR, CDN for static assets"/utf8>>,
            <<"text"/utf8>>,
            [<<"deployment_target"/utf8>>],
            [],
            []},
        {question,
            <<"r5-ops-2"/utf8>>,
            5,
            ops,
            non_functional,
            critical,
            <<"What's your uptime requirement? (SLA?)"/utf8>>,
            <<"99%, 99.9%, 99.99% availability."/utf8>>,
            <<"99.9% (8.76 hours downtime/year), acceptable during maintenance windows"/utf8>>,
            <<"text"/utf8>>,
            [<<"sla"/utf8>>],
            [],
            []},
        {question,
            <<"r5-ops-3"/utf8>>,
            5,
            ops,
            non_functional,
            important,
            <<"How will you monitor this? (metrics, logs, alerts?)"/utf8>>,
            <<"Observability strategy."/utf8>>,
            <<"Prometheus metrics, ELK logs, PagerDuty alerts on p95 latency > 500ms"/utf8>>,
            <<"text"/utf8>>,
            [<<"monitoring"/utf8>>],
            [],
            []}].

-file("src/intent/interview_questions.gleam", 49).
?DOC(" Get all questions for a specific profile and round\n").
-spec get_questions_for_round(binary(), integer()) -> list(question()).
get_questions_for_round(Profile, Round) ->
    case {Profile, Round} of
        {<<"api"/utf8>>, 1} ->
            round_1_api();

        {<<"cli"/utf8>>, 1} ->
            round_1_cli();

        {<<"event"/utf8>>, 1} ->
            round_1_event();

        {<<"data"/utf8>>, 1} ->
            round_1_data();

        {<<"workflow"/utf8>>, 1} ->
            round_1_workflow();

        {<<"ui"/utf8>>, 1} ->
            round_1_ui();

        {<<"api"/utf8>>, 2} ->
            round_2_api();

        {<<"cli"/utf8>>, 2} ->
            round_2_cli();

        {<<"event"/utf8>>, 2} ->
            round_2_event();

        {<<"data"/utf8>>, 2} ->
            round_2_data();

        {<<"workflow"/utf8>>, 2} ->
            round_2_workflow();

        {<<"ui"/utf8>>, 2} ->
            round_2_ui();

        {_, 3} ->
            round_3_common();

        {_, 4} ->
            round_4_common();

        {_, 5} ->
            round_5_common();

        _ ->
            []
    end.

-file("src/intent/interview_questions.gleam", 74).
?DOC(" Get the next unasked question in the current round\n").
-spec get_next_question(binary(), integer(), list(binary())) -> gleam@option:option(question()).
get_next_question(Profile, Round, Answered_ids) ->
    Questions = get_questions_for_round(Profile, Round),
    case find_first_unanswered(Questions, Answered_ids) of
        {ok, Q} ->
            {some, Q};

        {error, _} ->
            none
    end.
