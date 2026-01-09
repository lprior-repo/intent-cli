-module(intent@bead_templates).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/bead_templates.gleam").
-export([new_bead/9, bead_to_jsonl_line/1, beads_to_jsonl/1, filter_beads_by_type/2, sort_beads_by_priority/1, add_dependency/3, generate_beads_from_session/1, bead_stats/1, format_bead_preview/1, format_progressive_preview/2]).
-export_type([bead_record/0, bead_stats/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type bead_record() :: {bead_record,
        binary(),
        binary(),
        binary(),
        integer(),
        binary(),
        list(binary()),
        binary(),
        list(binary()),
        list(binary()),
        binary(),
        binary(),
        list(binary()),
        list(binary()),
        list(binary())}.

-type bead_stats() :: {bead_stats,
        integer(),
        gleam@dict:dict(binary(), integer()),
        gleam@dict:dict(integer(), integer())}.

-file("src/intent/bead_templates.gleam", 37).
?DOC(
    " Create a basic BeadRecord with default empty AI-friendly fields\n"
    " Used for backward compatibility and tests\n"
).
-spec new_bead(
    binary(),
    binary(),
    binary(),
    integer(),
    binary(),
    list(binary()),
    binary(),
    list(binary()),
    list(binary())
) -> bead_record().
new_bead(
    Title,
    Description,
    Profile_type,
    Priority,
    Issue_type,
    Labels,
    Ai_hints,
    Acceptance_criteria,
    Dependencies
) ->
    {bead_record,
        Title,
        Description,
        Profile_type,
        Priority,
        Issue_type,
        Labels,
        Ai_hints,
        Acceptance_criteria,
        Dependencies,
        <<""/utf8>>,
        <<""/utf8>>,
        [],
        [],
        []}.

-file("src/intent/bead_templates.gleam", 81).
?DOC(" Generate API endpoint beads\n").
-spec generate_api_beads(intent@interview:interview_session(), binary()) -> list(bead_record()).
generate_api_beads(Session, Profile) ->
    Endpoint_answers = gleam@list:filter(
        erlang:element(9, Session),
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"endpoint"/utf8>>, <<"path"/utf8>>]
            )
        end
    ),
    gleam@list:map(
        Endpoint_answers,
        fun(Answer@1) ->
            {bead_record,
                <<"Implement API endpoint"/utf8>>,
                erlang:element(6, Answer@1),
                Profile,
                3,
                <<"api_endpoint"/utf8>>,
                [<<"api"/utf8>>, <<"endpoint"/utf8>>, <<"implementation"/utf8>>],
                <<"Use interview response to build OpenAPI spec and implementation"/utf8>>,
                [<<"Endpoint responds with correct status code"/utf8>>,
                    <<"Response schema matches spec"/utf8>>,
                    <<"Error handling implemented"/utf8>>,
                    <<"Documentation added"/utf8>>],
                [],
                <<"GET /resource/123 with Authorization: Bearer token"/utf8>>,
                <<"{\"id\": 123, \"name\": \"example\", \"status\": \"active\"}"/utf8>>,
                [<<"200 OK for valid requests"/utf8>>,
                    <<"JSON body with resource data"/utf8>>],
                [<<"Return 500 for validation errors"/utf8>>,
                    <<"Expose internal error details"/utf8>>],
                [<<"Invalid ID format"/utf8>>,
                    <<"Missing auth header"/utf8>>,
                    <<"Expired token"/utf8>>,
                    <<"Resource not found"/utf8>>]}
        end
    ).

-file("src/intent/bead_templates.gleam", 113).
?DOC(" Generate CLI command beads\n").
-spec generate_cli_beads(intent@interview:interview_session(), binary()) -> list(bead_record()).
generate_cli_beads(Session, Profile) ->
    Command_answers = gleam@list:filter(
        erlang:element(9, Session),
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"command"/utf8>>, <<"subcommand"/utf8>>]
            )
        end
    ),
    gleam@list:map(
        Command_answers,
        fun(Answer@1) ->
            {bead_record,
                <<"Implement CLI command"/utf8>>,
                erlang:element(6, Answer@1),
                Profile,
                3,
                <<"cli_command"/utf8>>,
                [<<"cli"/utf8>>, <<"command"/utf8>>, <<"implementation"/utf8>>],
                <<"Review interview response for command syntax, options, and behavior"/utf8>>,
                [<<"Command parses arguments correctly"/utf8>>,
                    <<"Output format matches spec"/utf8>>,
                    <<"Help text is clear"/utf8>>,
                    <<"Error messages are helpful"/utf8>>],
                [],
                <<"mycli process --input data.json --output result.txt"/utf8>>,
                <<"Processing complete. Wrote 42 records to result.txt"/utf8>>,
                [<<"Exit code 0 on success"/utf8>>,
                    <<"Clear success message"/utf8>>],
                [<<"Exit 0 on failure"/utf8>>,
                    <<"Print stack traces in production"/utf8>>],
                [<<"Missing required args"/utf8>>,
                    <<"Invalid file path"/utf8>>,
                    <<"Permission denied"/utf8>>,
                    <<"Empty input"/utf8>>]}
        end
    ).

-file("src/intent/bead_templates.gleam", 145).
?DOC(" Generate event beads\n").
-spec generate_event_beads(intent@interview:interview_session(), binary()) -> list(bead_record()).
generate_event_beads(Session, Profile) ->
    Event_answers = gleam@list:filter(
        erlang:element(9, Session),
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"event"/utf8>>, <<"message"/utf8>>]
            )
        end
    ),
    gleam@list:map(
        Event_answers,
        fun(Answer@1) ->
            {bead_record,
                <<"Define and emit event"/utf8>>,
                erlang:element(6, Answer@1),
                Profile,
                2,
                <<"event"/utf8>>,
                [<<"event"/utf8>>, <<"messaging"/utf8>>, <<"integration"/utf8>>],
                <<"Create event schema and producer/consumer implementation"/utf8>>,
                [<<"Event schema defined"/utf8>>,
                    <<"Producer implementation complete"/utf8>>,
                    <<"Consumer can subscribe"/utf8>>,
                    <<"Event routing working"/utf8>>],
                [],
                <<"{\"user_id\": \"u123\", \"action\": \"created\", \"timestamp\": \"2026-01-09T12:00:00Z\"}"/utf8>>,
                <<"Event published to topic: user.created with correlation_id: abc-123"/utf8>>,
                [<<"Event with valid schema"/utf8>>,
                    <<"Unique correlation_id"/utf8>>],
                [<<"Emit event without required fields"/utf8>>,
                    <<"Block on publish failure"/utf8>>],
                [<<"Duplicate event detection"/utf8>>,
                    <<"Consumer offline"/utf8>>,
                    <<"Schema version mismatch"/utf8>>]}
        end
    ).

-file("src/intent/bead_templates.gleam", 177).
?DOC(" Generate data model beads\n").
-spec generate_data_beads(intent@interview:interview_session(), binary()) -> list(bead_record()).
generate_data_beads(Session, Profile) ->
    Entity_answers = gleam@list:filter(
        erlang:element(9, Session),
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"entity"/utf8>>, <<"data model"/utf8>>, <<"schema"/utf8>>]
            )
        end
    ),
    gleam@list:map(
        Entity_answers,
        fun(Answer@1) ->
            {bead_record,
                <<"Implement data model"/utf8>>,
                erlang:element(6, Answer@1),
                Profile,
                4,
                <<"data_model"/utf8>>,
                [<<"data"/utf8>>, <<"schema"/utf8>>, <<"storage"/utf8>>],
                <<"Generate database schema and ORM/repository layer"/utf8>>,
                [<<"Schema migrations ready"/utf8>>,
                    <<"Validation rules implemented"/utf8>>,
                    <<"Indexes optimized"/utf8>>,
                    <<"Tests cover all fields"/utf8>>],
                [],
                <<"{\"name\": \"John\", \"email\": \"john@example.com\", \"age\": 30}"/utf8>>,
                <<"User{id: 1, name: \"John\", email: \"john@example.com\", age: 30, created_at: ...}"/utf8>>,
                [<<"All required fields populated"/utf8>>,
                    <<"Auto-generated ID and timestamps"/utf8>>],
                [<<"Allow null for required fields"/utf8>>,
                    <<"Store unvalidated data"/utf8>>],
                [<<"Duplicate unique key"/utf8>>,
                    <<"Max field length exceeded"/utf8>>,
                    <<"Invalid foreign key"/utf8>>]}
        end
    ).

-file("src/intent/bead_templates.gleam", 209).
?DOC(" Generate workflow beads\n").
-spec generate_workflow_beads(intent@interview:interview_session(), binary()) -> list(bead_record()).
generate_workflow_beads(Session, Profile) ->
    Workflow_answers = gleam@list:filter(
        erlang:element(9, Session),
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"workflow"/utf8>>, <<"process"/utf8>>, <<"step"/utf8>>]
            )
        end
    ),
    gleam@list:map(
        Workflow_answers,
        fun(Answer@1) ->
            {bead_record,
                <<"Implement workflow step"/utf8>>,
                erlang:element(6, Answer@1),
                Profile,
                2,
                <<"workflow"/utf8>>,
                [<<"workflow"/utf8>>,
                    <<"orchestration"/utf8>>,
                    <<"automation"/utf8>>],
                <<"Design state machine and implement step logic"/utf8>>,
                [<<"State transitions working"/utf8>>,
                    <<"Error handling and retries"/utf8>>,
                    <<"Step completion detection"/utf8>>,
                    <<"Monitoring/logging implemented"/utf8>>],
                [],
                <<"{\"order_id\": \"ord-123\", \"action\": \"approve\", \"approver\": \"user-456\"}"/utf8>>,
                <<"{\"order_id\": \"ord-123\", \"status\": \"approved\", \"next_step\": \"ship\"}"/utf8>>,
                [<<"Updated workflow state"/utf8>>,
                    <<"Next step indication"/utf8>>],
                [<<"Skip required approval steps"/utf8>>,
                    <<"Allow invalid state transitions"/utf8>>],
                [<<"Timeout waiting for approval"/utf8>>,
                    <<"Concurrent modifications"/utf8>>,
                    <<"Rollback on failure"/utf8>>]}
        end
    ).

-file("src/intent/bead_templates.gleam", 241).
?DOC(" Generate UI screen beads\n").
-spec generate_ui_beads(intent@interview:interview_session(), binary()) -> list(bead_record()).
generate_ui_beads(Session, Profile) ->
    Screen_answers = gleam@list:filter(
        erlang:element(9, Session),
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"screen"/utf8>>, <<"view"/utf8>>, <<"interface"/utf8>>]
            )
        end
    ),
    gleam@list:map(
        Screen_answers,
        fun(Answer@1) ->
            {bead_record,
                <<"Build UI screen"/utf8>>,
                erlang:element(6, Answer@1),
                Profile,
                2,
                <<"ui_screen"/utf8>>,
                [<<"ui"/utf8>>, <<"frontend"/utf8>>, <<"component"/utf8>>],
                <<"Create mockup, component hierarchy, and responsive design"/utf8>>,
                [<<"All required fields present"/utf8>>,
                    <<"Responsive on mobile/tablet/desktop"/utf8>>,
                    <<"Accessibility standards met"/utf8>>,
                    <<"User testing completed"/utf8>>],
                [],
                <<"User clicks 'Add to Cart' button on product page"/utf8>>,
                <<"Cart counter increments, toast shows 'Added to cart', button state changes"/utf8>>,
                [<<"Visual feedback within 100ms"/utf8>>,
                    <<"Updated cart state"/utf8>>],
                [<<"Block UI during API call"/utf8>>,
                    <<"Allow double-click duplicate adds"/utf8>>],
                [<<"Slow network"/utf8>>,
                    <<"Item out of stock"/utf8>>,
                    <<"Session expired"/utf8>>,
                    <<"Mobile landscape"/utf8>>]}
        end
    ).

-file("src/intent/bead_templates.gleam", 273).
?DOC(" Convert bead record to JSONL line format (for .beads/issues.jsonl)\n").
-spec bead_to_jsonl_line(bead_record()) -> binary().
bead_to_jsonl_line(Bead) ->
    Json_list = [{<<"title"/utf8>>, gleam@json:string(erlang:element(2, Bead))},
        {<<"description"/utf8>>, gleam@json:string(erlang:element(3, Bead))},
        {<<"profile_type"/utf8>>, gleam@json:string(erlang:element(4, Bead))},
        {<<"priority"/utf8>>, gleam@json:int(erlang:element(5, Bead))},
        {<<"issue_type"/utf8>>, gleam@json:string(erlang:element(6, Bead))},
        {<<"labels"/utf8>>,
            gleam@json:array(erlang:element(7, Bead), fun gleam@json:string/1)},
        {<<"ai_hints"/utf8>>, gleam@json:string(erlang:element(8, Bead))},
        {<<"acceptance_criteria"/utf8>>,
            gleam@json:array(erlang:element(9, Bead), fun gleam@json:string/1)},
        {<<"dependencies"/utf8>>,
            gleam@json:array(erlang:element(10, Bead), fun gleam@json:string/1)},
        {<<"input_example"/utf8>>, gleam@json:string(erlang:element(11, Bead))},
        {<<"output_example"/utf8>>, gleam@json:string(erlang:element(12, Bead))},
        {<<"must_return"/utf8>>,
            gleam@json:array(erlang:element(13, Bead), fun gleam@json:string/1)},
        {<<"must_not"/utf8>>,
            gleam@json:array(erlang:element(14, Bead), fun gleam@json:string/1)},
        {<<"edge_cases"/utf8>>,
            gleam@json:array(erlang:element(15, Bead), fun gleam@json:string/1)}],
    _pipe = gleam@json:object(Json_list),
    gleam@json:to_string(_pipe).

-file("src/intent/bead_templates.gleam", 306).
?DOC(" Format beads for output as JSONL (newline-delimited JSON)\n").
-spec beads_to_jsonl(list(bead_record())) -> binary().
beads_to_jsonl(Beads) ->
    _pipe = Beads,
    _pipe@1 = gleam@list:map(_pipe, fun bead_to_jsonl_line/1),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-file("src/intent/bead_templates.gleam", 313).
?DOC(" Extract beads with specific issue type\n").
-spec filter_beads_by_type(list(bead_record()), binary()) -> list(bead_record()).
filter_beads_by_type(Beads, Issue_type) ->
    gleam@list:filter(
        Beads,
        fun(Bead) -> erlang:element(6, Bead) =:= Issue_type end
    ).

-file("src/intent/bead_templates.gleam", 321).
?DOC(" Sort beads by priority (higher number = higher priority)\n").
-spec sort_beads_by_priority(list(bead_record())) -> list(bead_record()).
sort_beads_by_priority(Beads) ->
    gleam@list:sort(
        Beads,
        fun(A, B) ->
            gleam@int:compare(erlang:element(5, B), erlang:element(5, A))
        end
    ).

-file("src/intent/bead_templates.gleam", 328).
?DOC(" Add dependency between beads (updates beads in place)\n").
-spec add_dependency(list(bead_record()), binary(), binary()) -> list(bead_record()).
add_dependency(Beads, From_title, To_title) ->
    gleam@list:map(
        Beads,
        fun(Bead) -> case erlang:element(2, Bead) =:= From_title of
                true ->
                    {bead_record,
                        erlang:element(2, Bead),
                        erlang:element(3, Bead),
                        erlang:element(4, Bead),
                        erlang:element(5, Bead),
                        erlang:element(6, Bead),
                        erlang:element(7, Bead),
                        erlang:element(8, Bead),
                        erlang:element(9, Bead),
                        lists:append(erlang:element(10, Bead), [To_title]),
                        erlang:element(11, Bead),
                        erlang:element(12, Bead),
                        erlang:element(13, Bead),
                        erlang:element(14, Bead),
                        erlang:element(15, Bead)};

                false ->
                    Bead
            end end
    ).

-file("src/intent/bead_templates.gleam", 346).
?DOC(" Helper: convert Profile to string\n").
-spec profile_to_string(intent@interview:profile()) -> binary().
profile_to_string(Profile) ->
    case Profile of
        api ->
            <<"api"/utf8>>;

        cli ->
            <<"cli"/utf8>>;

        event ->
            <<"event"/utf8>>;

        data ->
            <<"data"/utf8>>;

        workflow ->
            <<"workflow"/utf8>>;

        u_i ->
            <<"ui"/utf8>>
    end.

-file("src/intent/bead_templates.gleam", 67).
?DOC(" Generate beads from a completed interview session\n").
-spec generate_beads_from_session(intent@interview:interview_session()) -> list(bead_record()).
generate_beads_from_session(Session) ->
    Profile_str = profile_to_string(erlang:element(3, Session)),
    case erlang:element(3, Session) of
        api ->
            generate_api_beads(Session, Profile_str);

        cli ->
            generate_cli_beads(Session, Profile_str);

        event ->
            generate_event_beads(Session, Profile_str);

        data ->
            generate_data_beads(Session, Profile_str);

        workflow ->
            generate_workflow_beads(Session, Profile_str);

        u_i ->
            generate_ui_beads(Session, Profile_str)
    end.

-file("src/intent/bead_templates.gleam", 367).
?DOC(" Calculate stats for a list of beads\n").
-spec bead_stats(list(bead_record())) -> bead_stats().
bead_stats(Beads) ->
    Total = erlang:length(Beads),
    By_type = gleam@list:fold(
        Beads,
        gleam@dict:new(),
        fun(Acc, Bead) ->
            Current = begin
                _pipe = gleam@dict:get(Acc, erlang:element(6, Bead)),
                gleam@result:unwrap(_pipe, 0)
            end,
            gleam@dict:insert(Acc, erlang:element(6, Bead), Current + 1)
        end
    ),
    By_priority = gleam@list:fold(
        Beads,
        gleam@dict:new(),
        fun(Acc@1, Bead@1) ->
            Current@1 = begin
                _pipe@1 = gleam@dict:get(Acc@1, erlang:element(5, Bead@1)),
                gleam@result:unwrap(_pipe@1, 0)
            end,
            gleam@dict:insert(Acc@1, erlang:element(5, Bead@1), Current@1 + 1)
        end
    ),
    {bead_stats, Total, By_type, By_priority}.

-file("src/intent/bead_templates.gleam", 385).
?DOC(
    " Format a single bead for progressive preview display\n"
    " Shows a condensed single-line view of the bead\n"
).
-spec format_bead_preview(bead_record()) -> binary().
format_bead_preview(Bead) ->
    Type_badge = case erlang:element(6, Bead) of
        <<"api_endpoint"/utf8>> ->
            <<"[API]"/utf8>>;

        <<"cli_command"/utf8>> ->
            <<"[CLI]"/utf8>>;

        <<"event"/utf8>> ->
            <<"[EVT]"/utf8>>;

        <<"data_model"/utf8>> ->
            <<"[DAT]"/utf8>>;

        <<"workflow"/utf8>> ->
            <<"[WFL]"/utf8>>;

        <<"ui_screen"/utf8>> ->
            <<"[UI]"/utf8>>;

        _ ->
            <<"[???]"/utf8>>
    end,
    Desc = case gleam@string:length(erlang:element(3, Bead)) > 50 of
        true ->
            <<(gleam@string:slice(erlang:element(3, Bead), 0, 47))/binary,
                "..."/utf8>>;

        false ->
            erlang:element(3, Bead)
    end,
    <<<<<<<<Type_badge/binary, " "/utf8>>/binary,
                (erlang:element(2, Bead))/binary>>/binary,
            ": "/utf8>>/binary,
        Desc/binary>>.

-file("src/intent/bead_templates.gleam", 407).
?DOC(
    " Format beads for progressive preview during interview\n"
    " Shows what beads would be generated based on current answers\n"
).
-spec format_progressive_preview(list(bead_record()), integer()) -> binary().
format_progressive_preview(Beads, Round) ->
    case Beads of
        [] ->
            <<""/utf8>>;

        _ ->
            Count = erlang:length(Beads),
            Header = case Round of
                1 ->
                    <<"BEAD PREVIEW (rough outline based on Round 1):"/utf8>>;

                2 ->
                    <<"BEAD PREVIEW (refined with scope from Round 2):"/utf8>>;

                3 ->
                    <<"BEAD PREVIEW (error cases added from Round 3):"/utf8>>;

                4 ->
                    <<"BEAD PREVIEW (security hardened from Round 4):"/utf8>>;

                5 ->
                    <<"BEAD PREVIEW (production-ready from Round 5):"/utf8>>;

                _ ->
                    <<"BEAD PREVIEW:"/utf8>>
            end,
            Bead_lines = begin
                _pipe = Beads,
                _pipe@1 = gleam@list:take(_pipe, 5),
                _pipe@2 = gleam@list:map(
                    _pipe@1,
                    fun(B) ->
                        <<"  • "/utf8, (format_bead_preview(B))/binary>>
                    end
                ),
                gleam@string:join(_pipe@2, <<"\n"/utf8>>)
            end,
            More_indicator = case Count > 5 of
                true ->
                    <<<<"\n  ... and "/utf8,
                            (gleam@int:to_string(Count - 5))/binary>>/binary,
                        " more"/utf8>>;

                false ->
                    <<""/utf8>>
            end,
            <<<<<<<<<<<<<<<<"\n┌─────────────────────────────────────────────────────────────────┐\n"/utf8,
                                            "│ "/utf8>>/binary,
                                        Header/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                "├─────────────────────────────────────────────────────────────────┤\n"/utf8>>/binary,
                            Bead_lines/binary>>/binary,
                        More_indicator/binary>>/binary,
                    "\n"/utf8>>/binary,
                "└─────────────────────────────────────────────────────────────────┘\n"/utf8>>
    end.
