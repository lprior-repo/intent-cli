-module(intent@array_indexing).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse_path_component/1, get_all_array_elements/1, navigate_path/2, split_path/1, validate_path/1]).
-export_type([array_spec/0]).

-type array_spec() :: no_array | {index, integer()} | {last_n, integer()} | all.

-spec parse_index(binary()) -> {ok, integer()} | {error, binary()}.
parse_index(S) ->
    case gleam@int:parse(S) of
        {ok, N} ->
            case N >= 0 of
                true ->
                    {ok, N};

                false ->
                    {error,
                        <<"Array index must be non-negative: "/utf8, S/binary>>}
            end;

        {error, _} ->
            {error, <<"Array index must be a number: "/utf8, S/binary>>}
    end.

-spec parse_path_component(binary()) -> {ok, {binary(), array_spec()}} |
    {error, binary()}.
parse_path_component(Component) ->
    case gleam_stdlib:contains_string(Component, <<"["/utf8>>) of
        false ->
            {ok, {Component, no_array}};

        true ->
            case gleam@string:split_once(Component, <<"["/utf8>>) of
                {error, _} ->
                    {error, <<"Invalid array syntax: "/utf8, Component/binary>>};

                {ok, {Field_name, Rest}} ->
                    case gleam@string:split_once(Rest, <<"]"/utf8>>) of
                        {error, _} ->
                            {error,
                                <<"Missing closing ] in array index: "/utf8,
                                    Component/binary>>};

                        {ok, {Index_str, <<""/utf8>>}} ->
                            case Index_str of
                                <<"*"/utf8>> ->
                                    {ok, {Field_name, all}};

                                <<"-1"/utf8>> ->
                                    {ok, {Field_name, {last_n, 1}}};

                                _ ->
                                    case gleam@string:starts_with(
                                        Index_str,
                                        <<"-"/utf8>>
                                    ) of
                                        true ->
                                            case parse_index(
                                                gleam@string:slice(
                                                    Index_str,
                                                    1,
                                                    gleam@string:length(
                                                        Index_str
                                                    )
                                                )
                                            ) of
                                                {error, E} ->
                                                    {error, E};

                                                {ok, N} ->
                                                    {ok,
                                                        {Field_name,
                                                            {last_n, N}}}
                                            end;

                                        false ->
                                            case parse_index(Index_str) of
                                                {error, E@1} ->
                                                    {error, E@1};

                                                {ok, N@1} ->
                                                    {ok,
                                                        {Field_name,
                                                            {index, N@1}}}
                                            end
                                    end
                            end;

                        {ok, {_, _}} ->
                            {error,
                                <<"Invalid array syntax: only one ] expected: "/utf8,
                                    Component/binary>>}
                    end
            end
    end.

-spec get_all_array_elements(gleam@json:json()) -> {ok, list(gleam@json:json())} |
    {error, binary()}.
get_all_array_elements(Json) ->
    Json_str = gleam@json:to_string(Json),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Lst} ->
            Elements = begin
                _pipe = Lst,
                gleam@list:map(_pipe, fun intent@parser:dynamic_to_json/1)
            end,
            {ok, Elements};

        {error, _} ->
            {error, <<"Cannot get array elements from non-array JSON"/utf8>>}
    end.

-spec dynamic_to_json(gleam@dynamic:dynamic_()) -> {ok, gleam@json:json()} |
    {error, binary()}.
dynamic_to_json(Dyn) ->
    Json_val = intent@parser:dynamic_to_json(Dyn),
    {ok, Json_val}.

-spec navigate_field(gleam@json:json(), binary()) -> {ok,
        gleam@option:option(gleam@json:json())} |
    {error, binary()}.
navigate_field(Json, Field) ->
    Json_str = gleam@json:to_string(Json),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:dict(
            fun gleam@dynamic:string/1,
            fun gleam@dynamic:dynamic/1
        )
    ) of
        {ok, Obj_dict} ->
            case gleam@dict:get(Obj_dict, Field) of
                {ok, Dyn} ->
                    case dynamic_to_json(Dyn) of
                        {ok, J} ->
                            {ok, {some, J}};

                        {error, _} ->
                            {error,
                                <<"Cannot convert field value to JSON"/utf8>>}
                    end;

                {error, _} ->
                    {ok, none}
            end;

        {error, _} ->
            {error,
                <<<<"Cannot navigate field '"/utf8, Field/binary>>/binary,
                    "' in non-object JSON"/utf8>>}
    end.

-spec get_array_element(gleam@json:json(), integer()) -> {ok, gleam@json:json()} |
    {error, binary()}.
get_array_element(Json, Index) ->
    Json_str = gleam@json:to_string(Json),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Lst} ->
            Maybe_elem = begin
                _pipe = Lst,
                _pipe@1 = gleam@list:drop(_pipe, Index),
                gleam@list:first(_pipe@1)
            end,
            case Maybe_elem of
                {ok, Elem} ->
                    case dynamic_to_json(Elem) of
                        {ok, J} ->
                            {ok, J};

                        {error, _} ->
                            {error,
                                <<"Cannot convert array element to JSON at index "/utf8,
                                    (gleam@int:to_string(Index))/binary>>}
                    end;

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
                <<<<"Cannot index non-array JSON with ["/utf8,
                        (gleam@int:to_string(Index))/binary>>/binary,
                    "]"/utf8>>}
    end.

-spec get_array_element_last(gleam@json:json(), integer()) -> {ok,
        gleam@json:json()} |
    {error, binary()}.
get_array_element_last(Json, From_end) ->
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
                    Maybe_elem = begin
                        _pipe = Lst,
                        _pipe@1 = gleam@list:drop(_pipe, Actual_index),
                        gleam@list:first(_pipe@1)
                    end,
                    case Maybe_elem of
                        {ok, Elem} ->
                            case dynamic_to_json(Elem) of
                                {ok, J} ->
                                    {ok, J};

                                {error, _} ->
                                    {error,
                                        <<"Cannot convert array element to JSON at index -"/utf8,
                                            (gleam@int:to_string(From_end))/binary>>}
                            end;

                        {error, _} ->
                            {error, <<"Failed to access array element"/utf8>>}
                    end
            end;

        {error, _} ->
            {error, <<"Cannot index non-array JSON with negative index"/utf8>>}
    end.

-spec navigate_path(gleam@json:json(), list(binary())) -> {ok,
        gleam@json:json()} |
    {error, binary()}.
navigate_path(Json, Path_components) ->
    case Path_components of
        [] ->
            {ok, Json};

        [First | Rest] ->
            case parse_path_component(First) of
                {error, E} ->
                    {error, E};

                {ok, {Field_name, Array_spec}} ->
                    case navigate_field(Json, Field_name) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, none} ->
                            {error,
                                <<<<"Field '"/utf8, Field_name/binary>>/binary,
                                    "' not found"/utf8>>};

                        {ok, {some, Next_json}} ->
                            case Array_spec of
                                no_array ->
                                    navigate_path(Next_json, Rest);

                                {index, Idx} ->
                                    case get_array_element(Next_json, Idx) of
                                        {error, E@2} ->
                                            {error, E@2};

                                        {ok, Elem} ->
                                            navigate_path(Elem, Rest)
                                    end;

                                {last_n, N} ->
                                    case get_array_element_last(Next_json, N) of
                                        {error, E@3} ->
                                            {error, E@3};

                                        {ok, Elem@1} ->
                                            navigate_path(Elem@1, Rest)
                                    end;

                                all ->
                                    {error,
                                        <<"Array wildcard [*] requires special handling in rules"/utf8>>}
                            end
                    end
            end
    end.

-spec split_path(binary()) -> list(binary()).
split_path(Path) ->
    _pipe = gleam@string:split(Path, <<"."/utf8>>),
    _pipe@1 = gleam@list:map(_pipe, fun gleam@string:trim/1),
    gleam@list:filter(_pipe@1, fun(S) -> not gleam@string:is_empty(S) end).

-spec validate_path(binary()) -> {ok, nil} | {error, binary()}.
validate_path(Path) ->
    case gleam@string:is_empty(Path) of
        true ->
            {error, <<"Path cannot be empty"/utf8>>};

        false ->
            Components = split_path(Path),
            _pipe@1 = gleam@result:all(
                begin
                    _pipe = Components,
                    gleam@list:map(
                        _pipe,
                        fun(Component) ->
                            case parse_path_component(Component) of
                                {ok, _} ->
                                    {ok, nil};

                                {error, E} ->
                                    {error, E}
                            end
                        end
                    )
                end
            ),
            gleam@result:map(_pipe@1, fun(_) -> nil end)
    end.
