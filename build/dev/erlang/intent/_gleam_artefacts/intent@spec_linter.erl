-module(intent@spec_linter).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/spec_linter.gleam").
-export([lint_spec/1, format_warnings/1]).
-export_type([lint_result/0, lint_warning/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type lint_result() :: lint_valid | {lint_warnings, list(lint_warning())}.

-type lint_warning() :: {anti_pattern_detected, binary(), binary(), binary()} |
    {vague_rule, binary(), binary(), binary()} |
    {missing_example, binary()} |
    {unused_anti_pattern, binary()} |
    {naming_convention, binary(), binary()}.

-file("src/intent/spec_linter.gleam", 135).
?DOC(" Extract all keys from a JSON object (recursively)\n").
-spec extract_all_keys(gleam@json:json()) -> list(binary()).
extract_all_keys(Json) ->
    Json_str = gleam@json:to_string(Json),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:dict(
            fun gleam@dynamic:string/1,
            fun gleam@dynamic:dynamic/1
        )
    ) of
        {ok, Obj} ->
            gleam@dict:keys(Obj);

        {error, _} ->
            []
    end.

-file("src/intent/spec_linter.gleam", 126).
?DOC(" Check if a JSON example contains the bad pattern keys\n").
-spec contains_anti_pattern_keys(gleam@json:json(), intent@types:anti_pattern()) -> boolean().
contains_anti_pattern_keys(Example, Pattern) ->
    Bad_keys = extract_all_keys(erlang:element(4, Pattern)),
    Example_keys = extract_all_keys(Example),
    gleam@list:any(
        Bad_keys,
        fun(Key) -> gleam@list:contains(Example_keys, Key) end
    ).

-file("src/intent/spec_linter.gleam", 101).
?DOC(" Check for anti-patterns in a behavior's response example\n").
-spec check_anti_patterns(
    intent@types:behavior(),
    list(intent@types:anti_pattern())
) -> list(lint_warning()).
check_anti_patterns(Behavior, Patterns) ->
    case erlang:element(3, erlang:element(8, Behavior)) =:= gleam@json:null() of
        true ->
            [];

        false ->
            _pipe = Patterns,
            gleam@list:filter_map(
                _pipe,
                fun(Pattern) ->
                    case contains_anti_pattern_keys(
                        erlang:element(3, erlang:element(8, Behavior)),
                        Pattern
                    ) of
                        false ->
                            {error, nil};

                        true ->
                            {ok,
                                {anti_pattern_detected,
                                    erlang:element(2, Behavior),
                                    erlang:element(2, Pattern),
                                    <<"Response example contains keys from anti-pattern: "/utf8,
                                        (erlang:element(3, Pattern))/binary>>}}
                    end
                end
            )
    end.

-file("src/intent/spec_linter.gleam", 147).
?DOC(" Check for vague rules in a behavior\n").
-spec check_for_vague_rules(intent@types:behavior()) -> list(lint_warning()).
check_for_vague_rules(Behavior) ->
    _pipe = erlang:element(4, erlang:element(8, Behavior)),
    _pipe@1 = maps:to_list(_pipe),
    gleam@list:filter_map(
        _pipe@1,
        fun(Pair) ->
            {Field, Check} = Pair,
            Rule_lower = gleam@string:lowercase(erlang:element(2, Check)),
            Has_valid_keyword = gleam_stdlib:contains_string(
                Rule_lower,
                <<"valid"/utf8>>
            ),
            Has_email_keyword = gleam_stdlib:contains_string(
                Rule_lower,
                <<"email"/utf8>>
            ),
            Has_uuid_keyword = gleam_stdlib:contains_string(
                Rule_lower,
                <<"uuid"/utf8>>
            ),
            Has_iso_keyword = gleam_stdlib:contains_string(
                Rule_lower,
                <<"iso"/utf8>>
            ),
            Has_jwt_keyword = gleam_stdlib:contains_string(
                Rule_lower,
                <<"jwt"/utf8>>
            ),
            Has_uri_keyword = gleam_stdlib:contains_string(
                Rule_lower,
                <<"uri"/utf8>>
            ),
            Has_correct_format = gleam_stdlib:contains_string(
                Rule_lower,
                <<"correct format"/utf8>>
            ),
            Has_proper_format = gleam_stdlib:contains_string(
                Rule_lower,
                <<"proper format"/utf8>>
            ),
            Is_vague = ((((((Has_valid_keyword andalso not Has_email_keyword)
            andalso not Has_uuid_keyword)
            andalso not Has_iso_keyword)
            andalso not Has_jwt_keyword)
            andalso not Has_uri_keyword)
            orelse Has_correct_format)
            orelse Has_proper_format,
            case Is_vague of
                false ->
                    {error, nil};

                true ->
                    {ok,
                        {vague_rule,
                            erlang:element(2, Behavior),
                            Field,
                            <<(erlang:element(2, Check))/binary,
                                " (too vague - be specific)"/utf8>>}}
            end
        end
    ).

-file("src/intent/spec_linter.gleam", 205).
?DOC(" Check if a name has invalid characters (not alphanumeric, hyphen, underscore)\n").
-spec has_invalid_name_chars(binary()) -> boolean().
has_invalid_name_chars(Name) ->
    _pipe = gleam@string:to_graphemes(Name),
    gleam@list:any(
        _pipe,
        fun(C) ->
            case gleam_stdlib:contains_string(
                <<"abcdefghijklmnopqrstuvwxyz0123456789-_"/utf8>>,
                C
            ) of
                true ->
                    false;

                false ->
                    true
            end
        end
    ).

-file("src/intent/spec_linter.gleam", 190).
?DOC(" Check naming conventions for behaviors\n").
-spec check_naming_convention(intent@types:behavior()) -> {ok, lint_warning()} |
    {error, nil}.
check_naming_convention(Behavior) ->
    case has_invalid_name_chars(erlang:element(2, Behavior)) of
        false ->
            {error, nil};

        true ->
            {ok,
                {naming_convention,
                    erlang:element(2, Behavior),
                    <<"Use kebab-case for behavior names (e.g., 'get-user-by-id')"/utf8>>}}
    end.

-file("src/intent/spec_linter.gleam", 28).
?DOC(" Lint a complete spec\n").
-spec lint_spec(intent@types:spec()) -> lint_result().
lint_spec(Spec) ->
    Mut_warnings = [],
    Behaviors = begin
        _pipe = erlang:element(8, Spec),
        gleam@list:flat_map(_pipe, fun(F) -> erlang:element(4, F) end)
    end,
    Antipattern_warnings = begin
        _pipe@1 = Behaviors,
        gleam@list:flat_map(
            _pipe@1,
            fun(Behavior) ->
                check_anti_patterns(Behavior, erlang:element(10, Spec))
            end
        )
    end,
    Mut_warnings@1 = lists:append(Mut_warnings, Antipattern_warnings),
    Vague_warnings = begin
        _pipe@2 = Behaviors,
        gleam@list:flat_map(_pipe@2, fun check_for_vague_rules/1)
    end,
    Mut_warnings@2 = lists:append(Mut_warnings@1, Vague_warnings),
    Example_warnings = begin
        _pipe@3 = Behaviors,
        gleam@list:filter_map(
            _pipe@3,
            fun(B) ->
                case erlang:element(3, erlang:element(8, B)) =:= gleam@json:null(
                    
                ) of
                    true ->
                        {ok, {missing_example, erlang:element(2, B)}};

                    false ->
                        {error, nil}
                end
            end
        )
    end,
    Mut_warnings@3 = lists:append(Mut_warnings@2, Example_warnings),
    Naming_warnings = begin
        _pipe@4 = Behaviors,
        gleam@list:filter_map(_pipe@4, fun check_naming_convention/1)
    end,
    Mut_warnings@4 = lists:append(Mut_warnings@3, Naming_warnings),
    Used_patterns = begin
        _pipe@5 = Behaviors,
        _pipe@8 = gleam@list:flat_map(
            _pipe@5,
            fun(B@1) -> _pipe@6 = erlang:element(10, Spec),
                _pipe@7 = gleam@list:filter(
                    _pipe@6,
                    fun(Ap) ->
                        (erlang:element(3, erlang:element(8, B@1)) /= gleam@json:null(
                            
                        ))
                        andalso contains_anti_pattern_keys(
                            erlang:element(3, erlang:element(8, B@1)),
                            Ap
                        )
                    end
                ),
                gleam@list:map(
                    _pipe@7,
                    fun(Ap@1) -> erlang:element(2, Ap@1) end
                ) end
        ),
        gleam@list:unique(_pipe@8)
    end,
    Unused_warnings = begin
        _pipe@9 = erlang:element(10, Spec),
        gleam@list:filter_map(
            _pipe@9,
            fun(Ap@2) ->
                case gleam@list:contains(Used_patterns, erlang:element(2, Ap@2)) of
                    true ->
                        {error, nil};

                    false ->
                        {ok, {unused_anti_pattern, erlang:element(2, Ap@2)}}
                end
            end
        )
    end,
    Mut_warnings@5 = lists:append(Mut_warnings@4, Unused_warnings),
    case gleam@list:is_empty(Mut_warnings@5) of
        true ->
            lint_valid;

        false ->
            {lint_warnings, Mut_warnings@5}
    end.

-file("src/intent/spec_linter.gleam", 226).
?DOC(" Format a single lint warning\n").
-spec format_warning(lint_warning()) -> binary().
format_warning(Warning) ->
    case Warning of
        {anti_pattern_detected, Behavior, Pattern, Details} ->
            <<<<<<<<<<<<<<"Behavior '"/utf8, Behavior/binary>>/binary,
                                    "':\n"/utf8>>/binary,
                                "  Anti-pattern: "/utf8>>/binary,
                            Pattern/binary>>/binary,
                        "\n"/utf8>>/binary,
                    "  "/utf8>>/binary,
                Details/binary>>;

        {vague_rule, Behavior@1, Field, Rule} ->
            <<<<<<<<<<<<"Behavior '"/utf8, Behavior@1/binary>>/binary,
                                "', field '"/utf8>>/binary,
                            Field/binary>>/binary,
                        "':\n"/utf8>>/binary,
                    "  "/utf8>>/binary,
                Rule/binary>>;

        {missing_example, Behavior@2} ->
            <<<<<<"Behavior '"/utf8, Behavior@2/binary>>/binary, "':\n"/utf8>>/binary,
                "  Missing response example (helps AI understand intent)"/utf8>>;

        {unused_anti_pattern, Pattern@1} ->
            <<<<"Anti-pattern '"/utf8, Pattern@1/binary>>/binary,
                "' is not tested by any behavior"/utf8>>;

        {naming_convention, Behavior@3, Suggestion} ->
            <<<<<<<<"Behavior '"/utf8, Behavior@3/binary>>/binary, "':\n"/utf8>>/binary,
                    "  "/utf8>>/binary,
                Suggestion/binary>>
    end.

-file("src/intent/spec_linter.gleam", 216).
?DOC(" Format lint warnings for display\n").
-spec format_warnings(list(lint_warning())) -> binary().
format_warnings(Warnings) ->
    Warning_lines = begin
        _pipe = Warnings,
        _pipe@1 = gleam@list:map(_pipe, fun format_warning/1),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    <<<<<<"Linting found "/utf8,
                (gleam@int:to_string(erlang:length(Warnings)))/binary>>/binary,
            " warning(s):\n\n"/utf8>>/binary,
        Warning_lines/binary>>.
