-module(intent).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent.gleam").
-export([main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/intent.gleam", 224).
-spec print_spec_summary(intent@types:spec()) -> nil.
print_spec_summary(Spec) ->
    gleam@io:println(<<"Spec: "/utf8, (erlang:element(2, Spec))/binary>>),
    gleam@io:println(<<"Version: "/utf8, (erlang:element(5, Spec))/binary>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Description:"/utf8>>),
    gleam@io:println(erlang:element(3, Spec)),
    gleam@io:println(<<""/utf8>>),
    case erlang:element(4, Spec) of
        <<""/utf8>> ->
            nil;

        Audience ->
            gleam@io:println(<<"Audience: "/utf8, Audience/binary>>),
            gleam@io:println(<<""/utf8>>)
    end,
    case erlang:element(6, Spec) of
        [] ->
            nil;

        Criteria ->
            gleam@io:println(<<"Success Criteria:"/utf8>>),
            gleam@list:each(
                Criteria,
                fun(C) -> gleam@io:println(<<"  - "/utf8, C/binary>>) end
            ),
            gleam@io:println(<<""/utf8>>)
    end,
    gleam@io:println(<<"Features:"/utf8>>),
    gleam@list:each(
        erlang:element(8, Spec),
        fun(Feature) ->
            gleam@io:println(<<"  "/utf8, (erlang:element(2, Feature))/binary>>),
            gleam@io:println(
                <<"    "/utf8, (erlang:element(3, Feature))/binary>>
            ),
            gleam@io:println(
                <<"    Behaviors: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(4, Feature))
                    ))/binary>>
            ),
            gleam@list:each(
                erlang:element(4, Feature),
                fun(B) ->
                    gleam@io:println(
                        <<<<<<"      - "/utf8, (erlang:element(2, B))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, B))/binary>>
                    )
                end
            )
        end
    ),
    case erlang:element(9, Spec) of
        [] ->
            nil;

        Rules ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Global Rules:"/utf8>>),
            gleam@list:each(
                Rules,
                fun(Rule) ->
                    gleam@io:println(
                        <<<<<<"  - "/utf8, (erlang:element(2, Rule))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, Rule))/binary>>
                    )
                end
            )
    end,
    case erlang:element(10, Spec) of
        [] ->
            nil;

        Patterns ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Anti-Patterns:"/utf8>>),
            gleam@list:each(
                Patterns,
                fun(P) ->
                    gleam@io:println(
                        <<<<<<"  - "/utf8, (erlang:element(2, P))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, P))/binary>>
                    )
                end
            )
    end,
    nil.

-file("src/intent.gleam", 539).
-spec profile_to_display_string(intent@interview:profile()) -> binary().
profile_to_display_string(Profile) ->
    case Profile of
        api ->
            <<"API"/utf8>>;

        cli ->
            <<"CLI"/utf8>>;

        event ->
            <<"Event System"/utf8>>;

        data ->
            <<"Data System"/utf8>>;

        workflow ->
            <<"Workflow"/utf8>>;

        u_i ->
            <<"User Interface"/utf8>>
    end.

-file("src/intent.gleam", 553).
-spec generate_uuid() -> binary().
generate_uuid() ->
    <<"interview-abc123def456"/utf8>>.

-file("src/intent.gleam", 559).
-spec current_timestamp() -> binary().
current_timestamp() ->
    <<"2026-01-04T00:00:00Z"/utf8>>.

-file("src/intent.gleam", 486).
-spec run_interview(intent@interview:profile(), binary(), binary()) -> nil.
run_interview(Profile, Json_input, Export_to) ->
    Session_id = <<"interview-"/utf8, (generate_uuid())/binary>>,
    Timestamp = current_timestamp(),
    Session = intent@interview:create_session(Session_id, Profile, Timestamp),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<"                    INTENT INTERVIEW"/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"Profile: "/utf8, (profile_to_display_string(Profile))/binary>>
    ),
    gleam@io:println(<<"Session: "/utf8, Session_id/binary>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"This guided interview will help us discover and refine your"/utf8>>
    ),
    gleam@io:println(<<"specification through structured questioning."/utf8>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"We'll ask 25 questions across 5 rounds × 5 perspectives:"/utf8>>
    ),
    gleam@io:println(
        <<"  • Round 1: Core Intent (what are you building?)"/utf8>>
    ),
    gleam@io:println(<<"  • Round 2: Error Cases (what can go wrong?)"/utf8>>),
    gleam@io:println(
        <<"  • Round 3: Edge Cases (where are the boundaries?)"/utf8>>
    ),
    gleam@io:println(
        <<"  • Round 4: Security & Compliance (how do we keep it safe?)"/utf8>>
    ),
    gleam@io:println(
        <<"  • Round 5: Operations (how does it run in production?)"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Press Ctrl+C to save and exit at any time."/utf8>>),
    gleam@io:println(
        <<<<"Session will be saved to: .interview/"/utf8, Session_id/binary>>/binary,
            ".jsonl"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Ready? Let's begin."/utf8>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"⚠️ Interactive TUI not yet implemented"/utf8>>),
    gleam@io:println(<<"Stub: waiting for question/answer loop"/utf8>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"When implemented, interview will:"/utf8>>),
    gleam@io:println(<<"  1. Load questions from schema/questions.cue"/utf8>>),
    gleam@io:println(<<"  2. Show one question at a time"/utf8>>),
    gleam@io:println(
        <<"  3. Extract key fields from answers (AI-driven)"/utf8>>
    ),
    gleam@io:println(<<"  4. Detect gaps and conflicts"/utf8>>),
    gleam@io:println(<<"  5. Pause for critical gaps"/utf8>>),
    gleam@io:println(<<"  6. Offer conflict resolution options"/utf8>>),
    gleam@io:println(<<"  7. Export final spec as CUE"/utf8>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Session saved (empty): "/utf8, Session_id/binary>>),
    intent_ffi:halt(0).

-file("src/intent.gleam", 98).
-spec run_check(binary(), binary(), boolean(), binary(), binary(), boolean()) -> nil.
run_check(Spec_path, Target_url, Is_json, Feature_filter, Only_filter, Verbose) ->
    case intent@loader:load_spec(Spec_path) of
        {error, E} ->
            gleam@io:println_error(
                <<"Error: "/utf8, (intent@loader:format_error(E))/binary>>
            ),
            intent_ffi:halt(3);

        {ok, Spec} ->
            Options = {run_options, case Feature_filter of
                    <<""/utf8>> ->
                        none;

                    F ->
                        {some, F}
                end, case Only_filter of
                    <<""/utf8>> ->
                        none;

                    B ->
                        {some, B}
                end, Verbose},
            Result = intent@runner:run_spec(Spec, Target_url, Options),
            case Is_json of
                true ->
                    Json_result = intent@output:spec_result_to_json(Result),
                    gleam@io:println(gleam@json:to_string(Json_result));

                false ->
                    gleam@io:println(intent@output:spec_result_to_text(Result))
            end,
            Exit_code = case Result of
                {spec_result, true, _, _, _, _, _, _, _, _, _} ->
                    0;

                {spec_result, _, _, _, Blocked, _, _, _, _, _, _} when Blocked > 0 ->
                    2;

                _ ->
                    1
            end,
            intent_ffi:halt(Exit_code)
    end.

-file("src/intent.gleam", 50).
?DOC(" The `check` command - run spec against a target\n").
-spec check_command() -> glint:command(nil).
check_command() ->
    _pipe@5 = glint:command(
        fun(Input) ->
            Target_url = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"target"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<""/utf8>>)
            end,
            Is_json = begin
                _pipe@1 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, false)
            end,
            Feature_filter = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"feature"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Only_filter = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"only"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            Is_verbose = begin
                _pipe@4 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"verbose"/utf8>>
                ),
                gleam@result:unwrap(_pipe@4, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    run_check(
                        Spec_path,
                        Target_url,
                        Is_json,
                        Feature_filter,
                        Only_filter,
                        Is_verbose
                    );

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent check <spec.cue> --target <url>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@6 = glint:description(
        _pipe@5,
        <<"Run spec against a target URL and verify behaviors"/utf8>>
    ),
    _pipe@9 = glint:flag(
        _pipe@6,
        <<"target"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(
                _pipe@8,
                <<"Target base URL to test against"/utf8>>
            )
        end
    ),
    _pipe@12 = glint:flag(
        _pipe@9,
        <<"json"/utf8>>,
        begin
            _pipe@10 = glint@flag:bool(),
            _pipe@11 = glint@flag:default(_pipe@10, false),
            glint@flag:description(_pipe@11, <<"Output results as JSON"/utf8>>)
        end
    ),
    _pipe@15 = glint:flag(
        _pipe@12,
        <<"feature"/utf8>>,
        begin
            _pipe@13 = glint@flag:string(),
            _pipe@14 = glint@flag:default(_pipe@13, <<""/utf8>>),
            glint@flag:description(
                _pipe@14,
                <<"Filter to a specific feature"/utf8>>
            )
        end
    ),
    _pipe@18 = glint:flag(
        _pipe@15,
        <<"only"/utf8>>,
        begin
            _pipe@16 = glint@flag:string(),
            _pipe@17 = glint@flag:default(_pipe@16, <<""/utf8>>),
            glint@flag:description(
                _pipe@17,
                <<"Run only a specific behavior"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@18,
        <<"verbose"/utf8>>,
        begin
            _pipe@19 = glint@flag:bool(),
            _pipe@20 = glint@flag:default(_pipe@19, false),
            glint@flag:description(_pipe@20, <<"Verbose output"/utf8>>)
        end
    ).

-file("src/intent.gleam", 153).
?DOC(" The `validate` command - validate CUE spec without running\n").
-spec validate_command() -> glint:command(nil).
validate_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:validate_cue(Spec_path) of
                        {ok, _} ->
                            gleam@io:println(
                                <<"Valid: "/utf8, Spec_path/binary>>
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Invalid: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent validate <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Validate a CUE spec file without running tests"/utf8>>
    ).

-file("src/intent.gleam", 179).
?DOC(" The `show` command - pretty print a parsed spec\n").
-spec show_command() -> glint:command(nil).
show_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case Is_json of
                        true ->
                            case intent@loader:export_spec_json(Spec_path) of
                                {ok, Json_str} ->
                                    gleam@io:println(Json_str),
                                    intent_ffi:halt(0);

                                {error, E} ->
                                    gleam@io:println_error(
                                        <<"Error: "/utf8,
                                            (intent@loader:format_error(E))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        false ->
                            case intent@loader:load_spec(Spec_path) of
                                {ok, Spec} ->
                                    print_spec_summary(Spec),
                                    intent_ffi:halt(0);

                                {error, E@1} ->
                                    gleam@io:println_error(
                                        <<"Error: "/utf8,
                                            (intent@loader:format_error(E@1))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent show <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(_pipe@1, <<"Pretty print a parsed spec"/utf8>>),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-file("src/intent.gleam", 287).
?DOC(" The `export` command - export spec to JSON\n").
-spec export_command() -> glint:command(nil).
export_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:export_spec_json(Spec_path) of
                        {ok, Json_str} ->
                            gleam@io:println(Json_str),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(4)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent export <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(_pipe, <<"Export spec to JSON format"/utf8>>).

-file("src/intent.gleam", 313).
?DOC(" The `lint` command - check for specification anti-patterns\n").
-spec lint_command() -> glint:command(nil).
lint_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Lint_result = intent@spec_linter:lint_spec(Spec),
                            case Lint_result of
                                lint_valid ->
                                    gleam@io:println(
                                        <<"✓ Spec is well-formed - no linting issues found"/utf8>>
                                    ),
                                    intent_ffi:halt(0);

                                {lint_warnings, Warnings} ->
                                    gleam@io:println(
                                        intent@spec_linter:format_warnings(
                                            Warnings
                                        )
                                    ),
                                    intent_ffi:halt(1)
                            end;

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent lint <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Check spec for anti-patterns and quality issues"/utf8>>
    ).

-file("src/intent.gleam", 348).
?DOC(" The `analyze` command - analyze spec quality\n").
-spec analyze_command() -> glint:command(nil).
analyze_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@quality_analyzer:analyze_spec(Spec),
                            gleam@io:println(
                                intent@quality_analyzer:format_report(Report)
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent analyze <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Analyze spec quality and provide improvement suggestions"/utf8>>
    ).

-file("src/intent.gleam", 375).
?DOC(" The `improve` command - suggest improvements\n").
-spec improve_command() -> glint:command(nil).
improve_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Quality_report = intent@quality_analyzer:analyze_spec(
                                Spec
                            ),
                            Lint_result = intent@spec_linter:lint_spec(Spec),
                            Context = {improvement_context,
                                Quality_report,
                                Lint_result,
                                Spec},
                            Suggestions = intent@improver:suggest_improvements(
                                Context
                            ),
                            gleam@io:println(
                                intent@improver:format_improvements(Suggestions)
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent improve <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Suggest improvements based on quality analysis and linting"/utf8>>
    ).

-file("src/intent.gleam", 410).
?DOC(" The `interview` command - guided specification discovery\n").
-spec interview_command() -> glint:command(nil).
interview_command() ->
    _pipe@4 = glint:command(
        fun(Input) ->
            Profile_str = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"profile"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"api"/utf8>>)
            end,
            Resume_id = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"resume"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            Export_to = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"export"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Json_input = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"answers"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            case Resume_id of
                <<""/utf8>> ->
                    case gleam@string:lowercase(Profile_str) of
                        <<"api"/utf8>> ->
                            run_interview(api, Json_input, Export_to);

                        <<"cli"/utf8>> ->
                            run_interview(cli, Json_input, Export_to);

                        <<"event"/utf8>> ->
                            run_interview(event, Json_input, Export_to);

                        <<"data"/utf8>> ->
                            run_interview(data, Json_input, Export_to);

                        <<"workflow"/utf8>> ->
                            run_interview(workflow, Json_input, Export_to);

                        <<"ui"/utf8>> ->
                            run_interview(u_i, Json_input, Export_to);

                        _ ->
                            gleam@io:println_error(
                                <<<<"Error: unknown profile '"/utf8,
                                        Profile_str/binary>>/binary,
                                    "'"/utf8>>
                            ),
                            gleam@io:println_error(
                                <<"Valid profiles: api, cli, event, data, workflow, ui"/utf8>>
                            ),
                            intent_ffi:halt(4)
                    end;

                Id ->
                    gleam@io:println(
                        <<"Resuming interview session: "/utf8, Id/binary>>
                    ),
                    gleam@io:println_error(
                        <<"Session resume not yet implemented"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@5 = glint:description(
        _pipe@4,
        <<"Guided specification discovery through structured interview"/utf8>>
    ),
    _pipe@8 = glint:flag(
        _pipe@5,
        <<"profile"/utf8>>,
        begin
            _pipe@6 = glint@flag:string(),
            _pipe@7 = glint@flag:default(_pipe@6, <<"api"/utf8>>),
            glint@flag:description(
                _pipe@7,
                <<"System profile: api, cli, event, data, workflow, or ui"/utf8>>
            )
        end
    ),
    _pipe@11 = glint:flag(
        _pipe@8,
        <<"resume"/utf8>>,
        begin
            _pipe@9 = glint@flag:string(),
            _pipe@10 = glint@flag:default(_pipe@9, <<""/utf8>>),
            glint@flag:description(
                _pipe@10,
                <<"Resume existing interview session by ID"/utf8>>
            )
        end
    ),
    _pipe@14 = glint:flag(
        _pipe@11,
        <<"answers"/utf8>>,
        begin
            _pipe@12 = glint@flag:string(),
            _pipe@13 = glint@flag:default(_pipe@12, <<""/utf8>>),
            glint@flag:description(
                _pipe@13,
                <<"Path to YAML/JSON file with pre-filled answers"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@14,
        <<"export"/utf8>>,
        begin
            _pipe@15 = glint@flag:string(),
            _pipe@16 = glint@flag:default(_pipe@15, <<""/utf8>>),
            glint@flag:description(
                _pipe@16,
                <<"Export completed interview to spec file"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 34).
-spec main() -> nil.
main() ->
    _pipe = glint:new(),
    _pipe@1 = glint:with_name(_pipe, <<"intent"/utf8>>),
    _pipe@2 = glint:with_pretty_help(_pipe@1, glint:default_pretty_help()),
    _pipe@3 = glint:add(_pipe@2, [<<"check"/utf8>>], check_command()),
    _pipe@4 = glint:add(_pipe@3, [<<"validate"/utf8>>], validate_command()),
    _pipe@5 = glint:add(_pipe@4, [<<"show"/utf8>>], show_command()),
    _pipe@6 = glint:add(_pipe@5, [<<"export"/utf8>>], export_command()),
    _pipe@7 = glint:add(_pipe@6, [<<"lint"/utf8>>], lint_command()),
    _pipe@8 = glint:add(_pipe@7, [<<"analyze"/utf8>>], analyze_command()),
    _pipe@9 = glint:add(_pipe@8, [<<"improve"/utf8>>], improve_command()),
    _pipe@10 = glint:add(_pipe@9, [<<"interview"/utf8>>], interview_command()),
    glint:run(_pipe@10, erlang:element(4, argv:load())).
