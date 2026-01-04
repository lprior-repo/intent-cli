-module(intent@interpolate).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/interpolate.gleam").
-export([new_context/0, set_variable/3, set_request_body/2, set_response_body/2, get_variable/2, json_to_string/1, interpolate_string/2, interpolate_headers/2, extract_capture/2]).
-export_type([context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type context() :: {context,
        gleam@dict:dict(binary(), gleam@json:json()),
        gleam@option:option(gleam@json:json()),
        gleam@option:option(gleam@json:json())}.

-file("src/intent/interpolate.gleam", 24).
?DOC(" Create a new empty context\n").
-spec new_context() -> context().
new_context() ->
    {context, gleam@dict:new(), none, none}.

-file("src/intent/interpolate.gleam", 29).
?DOC(" Add a captured value to the context\n").
-spec set_variable(context(), binary(), gleam@json:json()) -> context().
set_variable(Ctx, Name, Value) ->
    {context,
        gleam@dict:insert(erlang:element(2, Ctx), Name, Value),
        erlang:element(3, Ctx),
        erlang:element(4, Ctx)}.

-file("src/intent/interpolate.gleam", 34).
?DOC(" Set the request body in context\n").
-spec set_request_body(context(), gleam@json:json()) -> context().
set_request_body(Ctx, Body) ->
    {context, erlang:element(2, Ctx), {some, Body}, erlang:element(4, Ctx)}.

-file("src/intent/interpolate.gleam", 39).
?DOC(" Set the response body in context\n").
-spec set_response_body(context(), gleam@json:json()) -> context().
set_response_body(Ctx, Body) ->
    {context, erlang:element(2, Ctx), erlang:element(3, Ctx), {some, Body}}.

-file("src/intent/interpolate.gleam", 44).
?DOC(" Get a variable value from context\n").
-spec get_variable(context(), binary()) -> gleam@option:option(gleam@json:json()).
get_variable(Ctx, Name) ->
    _pipe = gleam@dict:get(erlang:element(2, Ctx), Name),
    gleam@option:from_result(_pipe).

-file("src/intent/interpolate.gleam", 118).
?DOC(
    " Navigate into a JSON value using a path with array indexing support\n"
    " Supports: field, field[0], field[-1], field.nested[0].value\n"
).
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

-file("src/intent/interpolate.gleam", 88).
?DOC(" Resolve a variable path like \"response.body.id\" or \"user_id\"\n").
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

        [Var_name] ->
            case get_variable(Ctx, Var_name) of
                {some, Value} ->
                    {ok, Value};

                none ->
                    {error, <<"Variable not found: "/utf8, Var_name/binary>>}
            end;

        [Var_name@1 | Rest@2] ->
            case get_variable(Ctx, Var_name@1) of
                {some, Value@1} ->
                    navigate_json(Value@1, Rest@2);

                none ->
                    {error, <<"Variable not found: "/utf8, Var_name@1/binary>>}
            end;

        [] ->
            {error, <<"Empty variable path"/utf8>>}
    end.

-file("src/intent/interpolate.gleam", 129).
?DOC(" Convert a JSON value to a string representation\n").
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

-file("src/intent/interpolate.gleam", 62).
-spec interpolate_matches(context(), binary(), list(gleam@regexp:match())) -> {ok,
        binary()} |
    {error, binary()}.
interpolate_matches(Ctx, S, Matches) ->
    case Matches of
        [] ->
            {ok, S};

        [Match | Rest] ->
            case erlang:element(3, Match) of
                [{some, Var_path}] ->
                    case resolve_path(Ctx, Var_path) of
                        {ok, Value} ->
                            Value_str = json_to_string(Value),
                            New_s = gleam@string:replace(
                                S,
                                erlang:element(2, Match),
                                Value_str
                            ),
                            interpolate_matches(Ctx, New_s, Rest);

                        {error, E} ->
                            {error, E}
                    end;

                _ ->
                    interpolate_matches(Ctx, S, Rest)
            end
    end.

-file("src/intent/interpolate.gleam", 51).
?DOC(
    " Interpolate variables in a string\n"
    " Replaces ${var_name} with the stringified value of the variable\n"
).
-spec interpolate_string(context(), binary()) -> {ok, binary()} |
    {error, binary()}.
interpolate_string(Ctx, S) ->
    Pattern = <<"\\$\\{([^}]+)\\}"/utf8>>,
    case gleam@regexp:from_string(Pattern) of
        {ok, Re} ->
            Matches = gleam@regexp:scan(Re, S),
            interpolate_matches(Ctx, S, Matches);

        {error, _} ->
            {ok, S}
    end.

-file("src/intent/interpolate.gleam", 143).
?DOC(" Interpolate variables in headers dict\n").
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

-file("src/intent/interpolate.gleam", 160).
?DOC(" Extract a value from JSON using a capture path like \"response.body.id\"\n").
-spec extract_capture(context(), binary()) -> {ok, gleam@json:json()} |
    {error, binary()}.
extract_capture(Ctx, Capture_path) ->
    resolve_path(Ctx, Capture_path).
