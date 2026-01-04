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

-file("src/intent.gleam", 221).
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

-file("src/intent.gleam", 95).
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

-file("src/intent.gleam", 47).
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

-file("src/intent.gleam", 150).
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

-file("src/intent.gleam", 176).
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

-file("src/intent.gleam", 284).
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

-file("src/intent.gleam", 310).
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

-file("src/intent.gleam", 345).
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

-file("src/intent.gleam", 372).
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

-file("src/intent.gleam", 32).
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
    glint:run(_pipe@9, erlang:element(4, argv:load())).
