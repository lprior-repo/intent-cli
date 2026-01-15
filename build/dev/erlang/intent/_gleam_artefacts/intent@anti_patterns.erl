-module(intent@anti_patterns).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([check_anti_patterns/3, format_anti_pattern/1]).
-export_type([anti_pattern_result/0]).

-type anti_pattern_result() :: no_anti_patterns |
    {anti_pattern_detected,
        binary(),
        binary(),
        binary(),
        gleam@json:json(),
        gleam@json:json()}.

-spec get_all_keys_from_dynamic(gleam@dynamic:dynamic_()) -> gleam@set:set(binary()).
get_all_keys_from_dynamic(Data) ->
    case (gleam@dynamic:dict(
        fun gleam@dynamic:string/1,
        fun gleam@dynamic:dynamic/1
    ))(Data) of
        {ok, Obj} ->
            _pipe = Obj,
            _pipe@1 = maps:to_list(_pipe),
            _pipe@2 = gleam@list:flat_map(
                _pipe@1,
                fun(Pair) ->
                    {Key, Val} = Pair,
                    Nested = get_all_keys_from_dynamic(Val),
                    [Key | gleam@set:to_list(Nested)]
                end
            ),
            gleam@set:from_list(_pipe@2);

        {error, _} ->
            gleam@set:new()
    end.

-spec get_all_keys(gleam@json:json()) -> gleam@set:set(binary()).
get_all_keys(Value) ->
    Json_str = gleam@json:to_string(Value),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:dict(
            fun gleam@dynamic:string/1,
            fun gleam@dynamic:dynamic/1
        )
    ) of
        {ok, Obj} ->
            _pipe = Obj,
            _pipe@1 = maps:to_list(_pipe),
            _pipe@2 = gleam@list:flat_map(
                _pipe@1,
                fun(Pair) ->
                    {Key, Val} = Pair,
                    Nested = get_all_keys_from_dynamic(Val),
                    [Key | gleam@set:to_list(Nested)]
                end
            ),
            gleam@set:from_list(_pipe@2);

        {error, _} ->
            gleam@set:new()
    end.

-spec extract_problem_keys(intent@types:anti_pattern()) -> gleam@set:set(binary()).
extract_problem_keys(Pattern) ->
    Bad_keys = get_all_keys(erlang:element(4, Pattern)),
    Good_keys = get_all_keys(erlang:element(5, Pattern)),
    gleam@set:filter(
        Bad_keys,
        fun(Key) -> not gleam@set:contains(Good_keys, Key) end
    ).

-spec check_for_bad_keys(gleam@json:json(), gleam@set:set(binary())) -> list(binary()).
check_for_bad_keys(Body, Bad_keys) ->
    Response_keys = get_all_keys(Body),
    _pipe = Bad_keys,
    _pipe@1 = gleam@set:to_list(_pipe),
    gleam@list:filter(
        _pipe@1,
        fun(Key) -> gleam@set:contains(Response_keys, Key) end
    ).

-spec detect_pattern(
    intent@types:anti_pattern(),
    intent@http_client:execution_result()
) -> gleam@option:option(anti_pattern_result()).
detect_pattern(Pattern, Response) ->
    Bad_keys = extract_problem_keys(Pattern),
    Found_problems = check_for_bad_keys(erlang:element(4, Response), Bad_keys),
    case gleam@list:is_empty(Found_problems) of
        true ->
            none;

        false ->
            {some,
                {anti_pattern_detected,
                    erlang:element(2, Pattern),
                    erlang:element(3, Pattern),
                    <<"Response contains: "/utf8,
                        (gleam@string:join(Found_problems, <<", "/utf8>>))/binary>>,
                    erlang:element(4, Pattern),
                    erlang:element(5, Pattern)}}
    end.

-spec check_anti_patterns(
    list(intent@types:anti_pattern()),
    intent@http_client:execution_result(),
    binary()
) -> list(anti_pattern_result()).
check_anti_patterns(Patterns, Response, _) ->
    _pipe = Patterns,
    gleam@list:filter_map(
        _pipe,
        fun(Pattern) -> case detect_pattern(Pattern, Response) of
                {some, Result} ->
                    {ok, Result};

                none ->
                    {error, nil}
            end end
    ).

-spec format_anti_pattern(anti_pattern_result()) -> binary().
format_anti_pattern(Result) ->
    case Result of
        no_anti_patterns ->
            <<"No anti-patterns detected"/utf8>>;

        {anti_pattern_detected, Name, Description, Found, Bad, Good} ->
            <<<<<<<<<<<<<<<<<<<<<<<<<<"Anti-pattern detected: "/utf8,
                                                                Name/binary>>/binary,
                                                            "\n"/utf8>>/binary,
                                                        "Description: "/utf8>>/binary,
                                                    Description/binary>>/binary,
                                                "\n"/utf8>>/binary,
                                            "Found: "/utf8>>/binary,
                                        Found/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                "Bad example: "/utf8>>/binary,
                            (gleam@json:to_string(Bad))/binary>>/binary,
                        "\n"/utf8>>/binary,
                    "Good example: "/utf8>>/binary,
                (gleam@json:to_string(Good))/binary>>
    end.
