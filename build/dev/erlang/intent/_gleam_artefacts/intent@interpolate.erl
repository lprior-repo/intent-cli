-module(intent@interpolate).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new_context/0, set_variable/3, set_request_body/2, set_response_body/2, get_variable/2, json_to_string/1, extract_capture/2, interpolate_string/2, interpolate_headers/2]).
-export_type([context/0]).

-type context() :: {context,
        gleam@dict:dict(binary(), gleam@json:json()),
        gleam@option:option(gleam@json:json()),
        gleam@option:option(gleam@json:json())}.

-spec new_context() -> context().
new_context() ->
    {context, gleam@dict:new(), none, none}.

-spec set_variable(context(), binary(), gleam@json:json()) -> context().
set_variable(Ctx, Name, Value) ->
    erlang:setelement(
        2,
        Ctx,
        gleam@dict:insert(erlang:element(2, Ctx), Name, Value)
    ).

-spec set_request_body(context(), gleam@json:json()) -> context().
set_request_body(Ctx, Body) ->
    erlang:setelement(3, Ctx, {some, Body}).

-spec set_response_body(context(), gleam@json:json()) -> context().
set_response_body(Ctx, Body) ->
    erlang:setelement(4, Ctx, {some, Body}).

-spec get_variable(context(), binary()) -> gleam@option:option(gleam@json:json()).
get_variable(Ctx, Name) ->
    _pipe = gleam@dict:get(erlang:element(2, Ctx), Name),
    gleam@option:from_result(_pipe).

-spec get_array_element_from_json(gleam@json:json(), integer()) -> {ok,
        gleam@json:json()} |
    {error, binary()}.
get_array_element_from_json(Json, Index) ->
    Json_str = gleam@json:to_string(Json),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Lst} ->
            case begin
                _pipe = gleam@list:drop(Lst, Index),
                gleam@list:first(_pipe)
            end of
                {ok, Elem} ->
                    Json_val = intent@parser:dynamic_to_json(Elem),
                    {ok, Json_val};

                {error, _} ->
                    {error,
                        <<<<<<<<"Array index "/utf8,
                                        (gleam@int:to_string(Index))/binary>>/binary,
                                    " out of bounds (length: "/utf8>>/binary,
                                (gleam@int:to_string(erlang:length(Lst)))/binary>>/binary,
                            ")"/utf8>>}
            end;

        {error, _} ->
            {error,
                <<<<"Cannot index non-array with ["/utf8,
                        (gleam@int:to_string(Index))/binary>>/binary,
                    "]"/utf8>>}
    end.

-spec get_array_element_last_from_json(gleam@json:json(), integer()) -> {ok,
        gleam@json:json()} |
    {error, binary()}.
get_array_element_last_from_json(Json, From_end) ->
    Json_str = gleam@json:to_string(Json),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Lst} ->
            Length = erlang:length(Lst),
            Actual_index = Length - From_end,
            case (Actual_index >= 0) andalso (Actual_index < Length) of
                false ->
                    {error,
                        <<<<<<<<"Array index -"/utf8,
                                        (gleam@int:to_string(From_end))/binary>>/binary,
                                    " out of bounds (length: "/utf8>>/binary,
                                (gleam@int:to_string(Length))/binary>>/binary,
                            ")"/utf8>>};

                true ->
                    case begin
                        _pipe = gleam@list:drop(Lst, Actual_index),
                        gleam@list:first(_pipe)
                    end of
                        {ok, Elem} ->
                            Json_val = intent@parser:dynamic_to_json(Elem),
                            {ok, Json_val};

                        {error, _} ->
                            {error, <<"Failed to access array element"/utf8>>}
                    end
            end;

        {error, _} ->
            {error, <<"Cannot index non-array with negative index"/utf8>>}
    end.

-spec navigate_json(gleam@json:json(), list(binary())) -> {ok,
        gleam@json:json()} |
    {error, binary()}.
navigate_json(Value, Path) ->
    case Path of
        [] ->
            {ok, Value};

        Components ->
            intent@array_indexing:navigate_path(Value, Components)
    end.

-spec resolve_path(context(), binary()) -> {ok, gleam@json:json()} |
    {error, binary()}.
resolve_path(Ctx, Path) ->
    Parts = gleam@string:split(Path, <<"."/utf8>>),
    case Parts of
        [<<"request"/utf8>>, <<"body"/utf8>> | Rest] ->
            case erlang:element(3, Ctx) of
                {some, Body} ->
                    navigate_json(Body, Rest);

                none ->
                    {error, <<"No request body in context"/utf8>>}
            end;

        [<<"response"/utf8>>, <<"body"/utf8>> | Rest@1] ->
            case erlang:element(4, Ctx) of
                {some, Body@1} ->
                    navigate_json(Body@1, Rest@1);

                none ->
                    {error, <<"No response body in context"/utf8>>}
            end;

        [First_part | Rest@2] ->
            case intent@array_indexing:parse_path_component(First_part) of
                {ok, {Var_name, Array_spec}} ->
                    case get_variable(Ctx, Var_name) of
                        {some, Value} ->
                            case Array_spec of
                                no_array ->
                                    case Rest@2 of
                                        [] ->
                                            {ok, Value};

                                        _ ->
                                            navigate_json(Value, Rest@2)
                                    end;

                                {index, Idx} ->
                                    case get_array_element_from_json(Value, Idx) of
                                        {ok, Elem} ->
                                            case Rest@2 of
                                                [] ->
                                                    {ok, Elem};

                                                _ ->
                                                    navigate_json(Elem, Rest@2)
                                            end;

                                        {error, E} ->
                                            {error, E}
                                    end;

                                {last_n, N} ->
                                    case get_array_element_last_from_json(
                                        Value,
                                        N
                                    ) of
                                        {ok, Elem@1} ->
                                            case Rest@2 of
                                                [] ->
                                                    {ok, Elem@1};

                                                _ ->
                                                    navigate_json(
                                                        Elem@1,
                                                        Rest@2
                                                    )
                                            end;

                                        {error, E@1} ->
                                            {error, E@1}
                                    end;

                                all ->
                                    {error,
                                        <<"Array wildcard [*] not supported in variable paths"/utf8>>}
                            end;

                        none ->
                            {error,
                                <<"Variable not found: "/utf8, Var_name/binary>>}
                    end;

                {error, E@2} ->
                    {error, E@2}
            end;

        [] ->
            {error, <<"Empty variable path"/utf8>>}
    end.

-spec json_to_string(gleam@json:json()) -> binary().
json_to_string(Value) ->
    Encoded = gleam@json:to_string(Value),
    case gleam@string:starts_with(Encoded, <<"\""/utf8>>) andalso gleam@string:ends_with(
        Encoded,
        <<"\""/utf8>>
    ) of
        true ->
            _pipe = Encoded,
            _pipe@1 = gleam@string:drop_left(_pipe, 1),
            gleam@string:drop_right(_pipe@1, 1);

        false ->
            Encoded
    end.

-spec extract_capture(context(), binary()) -> {ok, gleam@json:json()} |
    {error, binary()}.
extract_capture(Ctx, Capture_path) ->
    resolve_path(Ctx, Capture_path).

-spec resolve_path_with_depth(context(), binary(), integer(), list(binary())) -> {ok,
        gleam@json:json()} |
    {error, binary()}.
resolve_path_with_depth(Ctx, Path, Depth, Visited) ->
    case resolve_path(Ctx, Path) of
        {ok, Value} ->
            Value_str = gleam@json:to_string(Value),
            case gleam@string:starts_with(Value_str, <<"\""/utf8>>) andalso gleam_stdlib:contains_string(
                Value_str,
                <<"${"/utf8>>
            ) of
                true ->
                    Unquoted = begin
                        _pipe = Value_str,
                        _pipe@1 = gleam@string:drop_left(_pipe, 1),
                        gleam@string:drop_right(_pipe@1, 1)
                    end,
                    case interpolate_string_with_depth(
                        Ctx,
                        Unquoted,
                        Depth,
                        Visited
                    ) of
                        {ok, Interpolated} ->
                            {ok, gleam@json:string(Interpolated)};

                        {error, E} ->
                            {error, E}
                    end;

                false ->
                    {ok, Value}
            end;

        {error, E@1} ->
            {error, E@1}
    end.

-spec interpolate_string_with_depth(
    context(),
    binary(),
    integer(),
    list(binary())
) -> {ok, binary()} | {error, binary()}.
interpolate_string_with_depth(Ctx, S, Depth, Visited) ->
    case Depth > 10 of
        true ->
            {error, <<"Variable interpolation depth limit exceeded"/utf8>>};

        false ->
            Pattern = <<"\\$\\{([^}]+)\\}"/utf8>>,
            case gleam@regexp:from_string(Pattern) of
                {ok, Re} ->
                    Matches = gleam@regexp:scan(Re, S),
                    interpolate_matches_with_depth(
                        Ctx,
                        S,
                        Matches,
                        Depth,
                        Visited
                    );

                {error, _} ->
                    {ok, S}
            end
    end.

-spec interpolate_matches_with_depth(
    context(),
    binary(),
    list(gleam@regexp:match()),
    integer(),
    list(binary())
) -> {ok, binary()} | {error, binary()}.
interpolate_matches_with_depth(Ctx, S, Matches, Depth, Visited) ->
    case Matches of
        [] ->
            {ok, S};

        [Match | Rest] ->
            case erlang:element(3, Match) of
                [{some, Var_path}] ->
                    case gleam@list:contains(Visited, Var_path) of
                        true ->
                            {error,
                                <<"Circular variable reference detected: "/utf8,
                                    Var_path/binary>>};

                        false ->
                            case resolve_path_with_depth(
                                Ctx,
                                Var_path,
                                Depth + 1,
                                [Var_path | Visited]
                            ) of
                                {ok, Value} ->
                                    Value_str = json_to_string(Value),
                                    New_s = gleam@string:replace(
                                        S,
                                        erlang:element(2, Match),
                                        Value_str
                                    ),
                                    interpolate_matches_with_depth(
                                        Ctx,
                                        New_s,
                                        Rest,
                                        Depth,
                                        Visited
                                    );

                                {error, E} ->
                                    {error, E}
                            end
                    end;

                _ ->
                    interpolate_matches_with_depth(Ctx, S, Rest, Depth, Visited)
            end
    end.

-spec interpolate_string(context(), binary()) -> {ok, binary()} |
    {error, binary()}.
interpolate_string(Ctx, S) ->
    interpolate_string_with_depth(Ctx, S, 0, []).

-spec interpolate_headers(context(), gleam@dict:dict(binary(), binary())) -> {ok,
        gleam@dict:dict(binary(), binary())} |
    {error, binary()}.
interpolate_headers(Ctx, Headers) ->
    _pipe = Headers,
    _pipe@1 = maps:to_list(_pipe),
    _pipe@2 = gleam@list:try_map(
        _pipe@1,
        fun(Pair) ->
            {Key, Value} = Pair,
            case interpolate_string(Ctx, Value) of
                {ok, New_value} ->
                    {ok, {Key, New_value}};

                {error, E} ->
                    {error, E}
            end
        end
    ),
    gleam@result:map(_pipe@2, fun maps:from_list/1).
