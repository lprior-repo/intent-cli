-module(intent@validator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/validator.gleam").
-export([validate_spec/1, format_issues/1]).
-export_type([validation_result/0, validation_issue/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type validation_result() :: validation_valid |
    {validation_invalid, list(validation_issue())}.

-type validation_issue() :: {rule_syntax_error,
        binary(),
        binary(),
        binary(),
        binary()} |
    {undefined_variable, binary(), binary(), binary(), binary()} |
    {invalid_path, binary(), binary(), binary()} |
    {missing_dependency, binary(), binary()} |
    {circular_dependency, list(binary())} |
    {missing_capture, binary(), binary(), binary(), list(binary())}.

-file("src/intent/validator.gleam", 97).
?DOC(" Validate rule syntax by attempting to parse it\n").
-spec validate_rule_syntax(binary(), binary(), binary()) -> list(validation_issue()).
validate_rule_syntax(_, _, _) ->
    [].

-file("src/intent/validator.gleam", 158).
?DOC(" Get list of variables that are available before a behavior runs\n").
-spec get_available_captures(
    intent@types:behavior(),
    list(intent@types:behavior())
) -> list(binary()).
get_available_captures(Behavior, All_behaviors) ->
    Behavior_index = begin
        _pipe = All_behaviors,
        gleam@list:find_map(
            _pipe,
            fun(B) ->
                case erlang:element(2, B) =:= erlang:element(2, Behavior) of
                    true ->
                        {ok,
                            erlang:length(
                                gleam@list:take_while(
                                    All_behaviors,
                                    fun(X) ->
                                        erlang:element(2, X) /= erlang:element(
                                            2,
                                            B
                                        )
                                    end
                                )
                            )};

                    false ->
                        {error, nil}
                end
            end
        )
    end,
    case Behavior_index of
        {error, _} ->
            [];

        {ok, Idx} ->
            _pipe@1 = All_behaviors,
            _pipe@2 = gleam@list:take(_pipe@1, Idx),
            _pipe@3 = gleam@list:flat_map(
                _pipe@2,
                fun(B@1) -> gleam@dict:keys(erlang:element(9, B@1)) end
            ),
            gleam@list:unique(_pipe@3)
    end.

-file("src/intent/validator.gleam", 187).
?DOC(" Find which behaviors capture a given variable\n").
-spec find_behaviors_capturing(binary(), list(intent@types:behavior())) -> list(binary()).
find_behaviors_capturing(Var_name, Behaviors) ->
    _pipe = Behaviors,
    gleam@list:filter_map(
        _pipe,
        fun(B) -> case gleam@dict:get(erlang:element(9, B), Var_name) of
                {ok, _} ->
                    {ok, erlang:element(2, B)};

                {error, _} ->
                    {error, nil}
            end end
    ).

-file("src/intent/validator.gleam", 201).
?DOC(" Extract variable names from a string (${var_name} syntax)\n").
-spec extract_variables(binary()) -> list(binary()).
extract_variables(S) ->
    Parts = gleam@string:split(S, <<"${"/utf8>>),
    _pipe = Parts,
    _pipe@1 = gleam@list:drop(_pipe, 1),
    gleam@list:filter_map(
        _pipe@1,
        fun(Part) -> case gleam@string:split_once(Part, <<"}"/utf8>>) of
                {ok, {Var_name, _}} ->
                    {ok, Var_name};

                {error, _} ->
                    {error, nil}
            end end
    ).

-file("src/intent/validator.gleam", 108).
?DOC(" Validate variable references in behavior\n").
-spec validate_variable_references(
    intent@types:behavior(),
    list(intent@types:behavior())
) -> list(validation_issue()).
validate_variable_references(Behavior, All_behaviors) ->
    Mut_issues = [],
    Available_captures = get_available_captures(Behavior, All_behaviors),
    Path_vars = extract_variables(
        erlang:element(3, erlang:element(7, Behavior))
    ),
    Path_issues = begin
        _pipe = Path_vars,
        gleam@list:filter_map(
            _pipe,
            fun(Var_name) ->
                case gleam@list:contains(Available_captures, Var_name) of
                    true ->
                        {error, nil};

                    false ->
                        Captured_by = find_behaviors_capturing(
                            Var_name,
                            All_behaviors
                        ),
                        {ok,
                            {missing_capture,
                                erlang:element(2, Behavior),
                                <<"request.path"/utf8>>,
                                Var_name,
                                Captured_by}}
                end
            end
        )
    end,
    Mut_issues@1 = lists:append(Mut_issues, Path_issues),
    Header_vars = begin
        _pipe@1 = erlang:element(4, erlang:element(7, Behavior)),
        _pipe@2 = gleam@dict:values(_pipe@1),
        _pipe@3 = gleam@list:flat_map(_pipe@2, fun extract_variables/1),
        gleam@list:unique(_pipe@3)
    end,
    Header_issues = begin
        _pipe@4 = Header_vars,
        gleam@list:filter_map(
            _pipe@4,
            fun(Var_name@1) ->
                case gleam@list:contains(Available_captures, Var_name@1) of
                    true ->
                        {error, nil};

                    false ->
                        Captured_by@1 = find_behaviors_capturing(
                            Var_name@1,
                            All_behaviors
                        ),
                        {ok,
                            {missing_capture,
                                erlang:element(2, Behavior),
                                <<"request.headers"/utf8>>,
                                Var_name@1,
                                Captured_by@1}}
                end
            end
        )
    end,
    Mut_issues@2 = lists:append(Mut_issues@1, Header_issues),
    Mut_issues@2.

-file("src/intent/validator.gleam", 59).
?DOC(" Validate a single behavior\n").
-spec validate_behavior(
    intent@types:behavior(),
    list(binary()),
    list(intent@types:behavior())
) -> list(validation_issue()).
validate_behavior(Behavior, All_behavior_names, All_behaviors) ->
    Mut_issues = [],
    Rule_issues = begin
        _pipe = erlang:element(4, erlang:element(8, Behavior)),
        _pipe@1 = maps:to_list(_pipe),
        gleam@list:flat_map(
            _pipe@1,
            fun(Pair) ->
                {Field, Check} = Pair,
                validate_rule_syntax(
                    erlang:element(2, Behavior),
                    Field,
                    erlang:element(2, Check)
                )
            end
        )
    end,
    Mut_issues@1 = lists:append(Mut_issues, Rule_issues),
    Dep_issues = begin
        _pipe@2 = erlang:element(5, Behavior),
        gleam@list:filter_map(
            _pipe@2,
            fun(Dep_name) ->
                case gleam@list:contains(All_behavior_names, Dep_name) of
                    true ->
                        {error, nil};

                    false ->
                        {ok,
                            {missing_dependency,
                                erlang:element(2, Behavior),
                                Dep_name}}
                end
            end
        )
    end,
    Mut_issues@2 = lists:append(Mut_issues@1, Dep_issues),
    Var_issues = validate_variable_references(Behavior, All_behaviors),
    Mut_issues@3 = lists:append(Mut_issues@2, Var_issues),
    Mut_issues@3.

-file("src/intent/validator.gleam", 229).
?DOC(" Check if a behavior has circular dependency\n").
-spec has_circular_dependency(
    binary(),
    list(binary()),
    list(intent@types:behavior())
) -> boolean().
has_circular_dependency(Behavior_name, Visited, All_behaviors) ->
    case gleam@list:contains(Visited, Behavior_name) of
        true ->
            true;

        false ->
            case gleam@list:find(
                All_behaviors,
                fun(B) -> erlang:element(2, B) =:= Behavior_name end
            ) of
                {error, _} ->
                    false;

                {ok, Behavior} ->
                    gleam@list:any(
                        erlang:element(5, Behavior),
                        fun(Dep) ->
                            has_circular_dependency(
                                Dep,
                                lists:append(Visited, [Behavior_name]),
                                All_behaviors
                            )
                        end
                    )
            end
    end.

-file("src/intent/validator.gleam", 216).
?DOC(" Check for circular dependencies\n").
-spec check_circular_dependencies(list(intent@types:behavior())) -> list(validation_issue()).
check_circular_dependencies(Behaviors) ->
    _pipe = Behaviors,
    gleam@list:filter_map(
        _pipe,
        fun(Behavior) ->
            case has_circular_dependency(
                erlang:element(2, Behavior),
                [],
                Behaviors
            ) of
                true ->
                    {ok, {circular_dependency, [erlang:element(2, Behavior)]}};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/intent/validator.gleam", 27).
?DOC(" Validate a complete spec before execution\n").
-spec validate_spec(intent@types:spec()) -> validation_result().
validate_spec(Spec) ->
    Mut_issues = [],
    All_behaviors = begin
        _pipe = erlang:element(8, Spec),
        gleam@list:flat_map(
            _pipe,
            fun(Feature) -> erlang:element(4, Feature) end
        )
    end,
    Behavior_names = begin
        _pipe@1 = All_behaviors,
        gleam@list:map(_pipe@1, fun(B) -> erlang:element(2, B) end)
    end,
    Behavior_issues = begin
        _pipe@2 = All_behaviors,
        gleam@list:flat_map(
            _pipe@2,
            fun(Behavior) ->
                validate_behavior(Behavior, Behavior_names, All_behaviors)
            end
        )
    end,
    Mut_issues@1 = lists:append(Mut_issues, Behavior_issues),
    Circular_issues = check_circular_dependencies(All_behaviors),
    Mut_issues@2 = lists:append(Mut_issues@1, Circular_issues),
    case gleam@list:is_empty(Mut_issues@2) of
        true ->
            validation_valid;

        false ->
            {validation_invalid, Mut_issues@2}
    end.

-file("src/intent/validator.gleam", 262).
?DOC(" Format a single validation issue\n").
-spec format_issue(validation_issue()) -> binary().
format_issue(Issue) ->
    case Issue of
        {rule_syntax_error, Behavior, Field, Rule, Error} ->
            <<<<<<<<<<<<<<<<<<"Behavior '"/utf8, Behavior/binary>>/binary,
                                            "', field '"/utf8>>/binary,
                                        Field/binary>>/binary,
                                    "':\n"/utf8>>/binary,
                                "  Invalid rule syntax: "/utf8>>/binary,
                            Rule/binary>>/binary,
                        "\n"/utf8>>/binary,
                    "  Error: "/utf8>>/binary,
                Error/binary>>;

        {undefined_variable, Behavior@1, Field@1, Var_name, Suggestion} ->
            <<<<<<<<<<<<<<<<<<"Behavior '"/utf8, Behavior@1/binary>>/binary,
                                            "', field '"/utf8>>/binary,
                                        Field@1/binary>>/binary,
                                    "':\n"/utf8>>/binary,
                                "  Variable '"/utf8>>/binary,
                            Var_name/binary>>/binary,
                        "' is not defined\n"/utf8>>/binary,
                    "  Suggestion: "/utf8>>/binary,
                Suggestion/binary>>;

        {invalid_path, Behavior@2, Path, Error@1} ->
            <<<<<<<<<<<<<<"Behavior '"/utf8, Behavior@2/binary>>/binary,
                                    "':\n"/utf8>>/binary,
                                "  Invalid path: "/utf8>>/binary,
                            Path/binary>>/binary,
                        "\n"/utf8>>/binary,
                    "  Error: "/utf8>>/binary,
                Error@1/binary>>;

        {missing_dependency, Behavior@3, Depends_on} ->
            <<<<<<<<<<"Behavior '"/utf8, Behavior@3/binary>>/binary,
                            "':\n"/utf8>>/binary,
                        "  Depends on behavior '"/utf8>>/binary,
                    Depends_on/binary>>/binary,
                "' which does not exist"/utf8>>;

        {circular_dependency, Behaviors} ->
            <<<<"Circular dependency detected:\n"/utf8, "  Behaviors: "/utf8>>/binary,
                (gleam@string:join(Behaviors, <<" -> "/utf8>>))/binary>>;

        {missing_capture, Behavior@4, Location, Var_name@1, Captured_by} ->
            <<<<<<<<<<<<<<<<"Behavior '"/utf8, Behavior@4/binary>>/binary,
                                        "', "/utf8>>/binary,
                                    Location/binary>>/binary,
                                ":\n"/utf8>>/binary,
                            "  Variable '"/utf8>>/binary,
                        Var_name@1/binary>>/binary,
                    "' is not available\n"/utf8>>/binary,
                (case Captured_by of
                    [] ->
                        <<"  Hint: No behavior captures this variable. Check spelling."/utf8>>;

                    _ ->
                        <<<<<<<<<<"  Hint: This variable is captured by: "/utf8,
                                            (gleam@string:join(
                                                Captured_by,
                                                <<", "/utf8>>
                                            ))/binary>>/binary,
                                        "\n"/utf8>>/binary,
                                    "  Ensure these behaviors run before '"/utf8>>/binary,
                                Behavior@4/binary>>/binary,
                            "'"/utf8>>
                end)/binary>>
    end.

-file("src/intent/validator.gleam", 252).
?DOC(" Format validation issues for display\n").
-spec format_issues(list(validation_issue())) -> binary().
format_issues(Issues) ->
    Issue_lines = begin
        _pipe = Issues,
        _pipe@1 = gleam@list:map(_pipe, fun format_issue/1),
        gleam@string:join(_pipe@1, <<"\n\n"/utf8>>)
    end,
    <<<<<<"Validation failed with "/utf8,
                (gleam@int:to_string(erlang:length(Issues)))/binary>>/binary,
            " issue(s):\n\n"/utf8>>/binary,
        Issue_lines/binary>>.
