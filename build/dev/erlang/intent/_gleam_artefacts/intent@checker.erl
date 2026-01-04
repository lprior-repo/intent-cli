-module(intent@checker).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/checker.gleam").
-export([check_response/3]).
-export_type([check_result/0, response_check_result/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type check_result() :: {check_passed, binary(), binary()} |
    {check_failed, binary(), binary(), binary(), binary(), binary()}.

-type response_check_result() :: {response_check_result,
        list(check_result()),
        list(check_result()),
        boolean(),
        integer(),
        integer()}.

-file("src/intent/checker.gleam", 94).
?DOC(" Check a response header against expected value\n").
-spec check_header(binary(), binary(), gleam@dict:dict(binary(), binary())) -> check_result().
check_header(Header_name, Expected_value, Actual_headers) ->
    Lower_name = gleam@string:lowercase(Header_name),
    Actual_value = begin
        _pipe = Actual_headers,
        _pipe@1 = maps:to_list(_pipe),
        gleam@list:find(
            _pipe@1,
            fun(Pair) ->
                gleam@string:lowercase(erlang:element(1, Pair)) =:= Lower_name
            end
        )
    end,
    case Actual_value of
        {ok, {_, Value}} ->
            case Value =:= Expected_value of
                true ->
                    {check_passed,
                        <<"header:"/utf8, Header_name/binary>>,
                        <<"equals "/utf8, Expected_value/binary>>};

                false ->
                    {check_failed,
                        <<"header:"/utf8, Header_name/binary>>,
                        <<"equals "/utf8, Expected_value/binary>>,
                        Expected_value,
                        Value,
                        <<<<<<<<<<<<"Header '"/utf8, Header_name/binary>>/binary,
                                            "' expected '"/utf8>>/binary,
                                        Expected_value/binary>>/binary,
                                    "' but got '"/utf8>>/binary,
                                Value/binary>>/binary,
                            "'"/utf8>>}
            end;

        {error, _} ->
            {check_failed,
                <<"header:"/utf8, Header_name/binary>>,
                <<"present"/utf8>>,
                <<"header to be present"/utf8>>,
                <<"header missing"/utf8>>,
                <<<<"Expected header '"/utf8, Header_name/binary>>/binary,
                    "' not found in response"/utf8>>}
    end.

-file("src/intent/checker.gleam", 155).
-spec interpolate_rule(binary(), intent@interpolate:context()) -> binary().
interpolate_rule(Rule_str, Ctx) ->
    case intent@interpolate:interpolate_string(Ctx, Rule_str) of
        {ok, Interpolated} ->
            Interpolated;

        {error, _} ->
            Rule_str
    end.

-file("src/intent/checker.gleam", 313).
-spec navigate_json_path(gleam@json:json(), list(binary())) -> gleam@option:option(gleam@json:json()).
navigate_json_path(Value, Path) ->
    case Path of
        [] ->
            {some, Value};

        [Key | Rest] ->
            Json_str = gleam@json:to_string(Value),
            case gleam@json:decode(
                Json_str,
                gleam@dynamic:dict(
                    fun gleam@dynamic:string/1,
                    fun gleam@dynamic:dynamic/1
                )
            ) of
                {ok, Obj} ->
                    case gleam@dict:get(Obj, Key) of
                        {ok, Next} ->
                            Next_json = intent@parser:dynamic_to_json(Next),
                            navigate_json_path(Next_json, Rest);

                        {error, _} ->
                            none
                    end;

                {error, _} ->
                    none
            end
    end.

-file("src/intent/checker.gleam", 306).
?DOC(" Get a field value from JSON using dot notation\n").
-spec get_field_value(gleam@option:option(gleam@json:json()), binary()) -> gleam@option:option(gleam@json:json()).
get_field_value(Body, Field) ->
    case Body of
        none ->
            none;

        {some, Json_val} ->
            navigate_json_path(
                Json_val,
                gleam@string:split(Field, <<"."/utf8>>)
            )
    end.

-file("src/intent/checker.gleam", 162).
-spec check_absent(binary(), gleam@option:option(gleam@json:json())) -> check_result().
check_absent(Field, Body) ->
    case get_field_value(Body, Field) of
        none ->
            {check_passed, Field, <<"absent"/utf8>>};

        {some, _} ->
            {check_failed,
                Field,
                <<"absent"/utf8>>,
                <<"field to be absent"/utf8>>,
                <<"field exists"/utf8>>,
                <<<<"Field '"/utf8, Field/binary>>/binary,
                    "' should not be present in response"/utf8>>}
    end.

-file("src/intent/checker.gleam", 176).
-spec check_present(binary(), gleam@option:option(gleam@json:json())) -> check_result().
check_present(Field, Body) ->
    case get_field_value(Body, Field) of
        {some, _} ->
            {check_passed, Field, <<"present"/utf8>>};

        none ->
            {check_failed,
                Field,
                <<"present"/utf8>>,
                <<"field to be present"/utf8>>,
                <<"field missing"/utf8>>,
                <<<<"Field '"/utf8, Field/binary>>/binary,
                    "' must be present in response"/utf8>>}
    end.

-file("src/intent/checker.gleam", 344).
-spec check_equals_json(gleam@json:json(), gleam@json:json()) -> {ok, nil} |
    {error, binary()}.
check_equals_json(Value, Expected) ->
    Actual_str = gleam@json:to_string(Value),
    Expected_str = gleam@json:to_string(Expected),
    case Actual_str =:= Expected_str of
        true ->
            {ok, nil};

        false ->
            {error,
                <<<<<<"Expected "/utf8, Expected_str/binary>>/binary,
                        " but got "/utf8>>/binary,
                    Actual_str/binary>>}
    end.

-file("src/intent/checker.gleam", 354).
-spec check_equals_int(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_equals_int(Value, Expected) ->
    case gleam@json:decode(gleam@json:to_string(Value), fun gleam@dynamic:int/1) of
        {ok, Actual} ->
            case Actual =:= Expected of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected "/utf8,
                                    (gleam@int:to_string(Expected))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected integer but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 371).
-spec check_equals_float(gleam@json:json(), float()) -> {ok, nil} |
    {error, binary()}.
check_equals_float(Value, Expected) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:float/1
    ) of
        {ok, Actual} ->
            case Actual =:= Expected of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected "/utf8,
                                    (gleam@float:to_string(Expected))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@float:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected float but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 388).
-spec check_equals_bool(gleam@json:json(), boolean()) -> {ok, nil} |
    {error, binary()}.
check_equals_bool(Value, Expected) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:bool/1
    ) of
        {ok, Actual} ->
            case Actual =:= Expected of
                true ->
                    {ok, nil};

                false ->
                    Expected_str = case Expected of
                        true ->
                            <<"true"/utf8>>;

                        false ->
                            <<"false"/utf8>>
                    end,
                    Actual_str = case Actual of
                        true ->
                            <<"true"/utf8>>;

                        false ->
                            <<"false"/utf8>>
                    end,
                    {error,
                        <<<<<<"Expected "/utf8, Expected_str/binary>>/binary,
                                " but got "/utf8>>/binary,
                            Actual_str/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected boolean but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 409).
-spec check_is_string(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_string(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, _} ->
            {ok, nil};

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 416).
-spec check_is_integer(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_integer(Value) ->
    case gleam@json:decode(gleam@json:to_string(Value), fun gleam@dynamic:int/1) of
        {ok, _} ->
            {ok, nil};

        {error, _} ->
            {error,
                <<"Expected integer but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 423).
-spec check_is_number(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_number(Value) ->
    Json_str = gleam@json:to_string(Value),
    case gleam@json:decode(Json_str, fun gleam@dynamic:int/1) of
        {ok, _} ->
            {ok, nil};

        {error, _} ->
            case gleam@json:decode(Json_str, fun gleam@dynamic:float/1) of
                {ok, _} ->
                    {ok, nil};

                {error, _} ->
                    {error,
                        <<"Expected number but got "/utf8, Json_str/binary>>}
            end
    end.

-file("src/intent/checker.gleam", 435).
-spec check_is_boolean(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_boolean(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:bool/1
    ) of
        {ok, _} ->
            {ok, nil};

        {error, _} ->
            {error,
                <<"Expected boolean but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 442).
-spec check_is_array(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_array(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, _} ->
            {ok, nil};

        {error, _} ->
            {error,
                <<"Expected array but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 449).
-spec check_is_object(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_object(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        gleam@dynamic:dict(
            fun gleam@dynamic:string/1,
            fun gleam@dynamic:dynamic/1
        )
    ) of
        {ok, _} ->
            {ok, nil};

        {error, _} ->
            {error,
                <<"Expected object but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 458).
-spec check_is_null(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_null(Value) ->
    case gleam@json:to_string(Value) of
        <<"null"/utf8>> ->
            {ok, nil};

        Other ->
            {error, <<"Expected null but got "/utf8, Other/binary>>}
    end.

-file("src/intent/checker.gleam", 465).
-spec check_non_empty_string(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_non_empty_string(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            case gleam@string:is_empty(S) of
                true ->
                    {error,
                        <<"Expected non-empty string but got empty string"/utf8>>};

                false ->
                    {ok, nil}
            end;

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 492).
-spec check_string_starting_with(gleam@json:json(), binary()) -> {ok, nil} |
    {error, binary()}.
check_string_starting_with(Value, Prefix) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            case gleam@string:starts_with(S, Prefix) of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<<<"String '"/utf8, S/binary>>/binary,
                                    "' does not start with '"/utf8>>/binary,
                                Prefix/binary>>/binary,
                            "'"/utf8>>}
            end;

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 507).
-spec check_string_ending_with(gleam@json:json(), binary()) -> {ok, nil} |
    {error, binary()}.
check_string_ending_with(Value, Suffix) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            case gleam@string:ends_with(S, Suffix) of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<<<"String '"/utf8, S/binary>>/binary,
                                    "' does not end with '"/utf8>>/binary,
                                Suffix/binary>>/binary,
                            "'"/utf8>>}
            end;

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 519).
-spec check_string_containing(gleam@json:json(), binary()) -> {ok, nil} |
    {error, binary()}.
check_string_containing(Value, Substring) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            case gleam_stdlib:contains_string(S, Substring) of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<<<"String '"/utf8, S/binary>>/binary,
                                    "' does not contain '"/utf8>>/binary,
                                Substring/binary>>/binary,
                            "'"/utf8>>}
            end;

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 534).
-spec check_is_email(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_email(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            intent@formats:validate_email(S);

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 541).
-spec check_is_uuid(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_uuid(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            intent@formats:validate_uuid(S);

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 548).
-spec check_is_uri(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_uri(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            intent@formats:validate_uri(S);

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 621).
-spec base64_url_decode(binary()) -> {ok, binary()} | {error, binary()}.
base64_url_decode(Input) ->
    intent_ffi:base64_url_decode(Input).

-file("src/intent/checker.gleam", 590).
-spec validate_jwt_part(binary(), binary()) -> {ok, nil} | {error, binary()}.
validate_jwt_part(Part, Name) ->
    case base64_url_decode(Part) of
        {ok, Decoded} ->
            case gleam@json:decode(Decoded, fun gleam@dynamic:dynamic/1) of
                {ok, _} ->
                    {ok, nil};

                {error, _} ->
                    {error,
                        <<<<"JWT "/utf8, Name/binary>>/binary,
                            " is not valid JSON after Base64 decoding"/utf8>>}
            end;

        {error, _} ->
            {error,
                <<<<"JWT "/utf8, Name/binary>>/binary,
                    " is not valid Base64URL encoding"/utf8>>}
    end.

-file("src/intent/checker.gleam", 603).
-spec validate_jwt_header_has_alg(binary()) -> {ok, nil} | {error, binary()}.
validate_jwt_header_has_alg(Header) ->
    case base64_url_decode(Header) of
        {ok, Decoded} ->
            case gleam@json:decode(
                Decoded,
                gleam@dynamic:dict(
                    fun gleam@dynamic:string/1,
                    fun gleam@dynamic:dynamic/1
                )
            ) of
                {ok, Obj} ->
                    case gleam@dict:has_key(Obj, <<"alg"/utf8>>) of
                        true ->
                            {ok, nil};

                        false ->
                            {error,
                                <<"JWT header missing required 'alg' field"/utf8>>}
                    end;

                {error, _} ->
                    {error, <<"JWT header is not a valid JSON object"/utf8>>}
            end;

        {error, _} ->
            {error, <<"JWT header is not valid Base64URL encoding"/utf8>>}
    end.

-file("src/intent/checker.gleam", 559).
-spec check_valid_jwt(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_valid_jwt(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            Parts = gleam@string:split(S, <<"."/utf8>>),
            case Parts of
                [Header, Payload, _] ->
                    case validate_jwt_part(Header, <<"header"/utf8>>) of
                        {ok, _} ->
                            case validate_jwt_header_has_alg(Header) of
                                {ok, _} ->
                                    validate_jwt_part(
                                        Payload,
                                        <<"payload"/utf8>>
                                    );

                                {error, E} ->
                                    {error, E}
                            end;

                        {error, E@1} ->
                            {error, E@1}
                    end;

                _ ->
                    {error,
                        <<<<"'"/utf8, S/binary>>/binary,
                            "' is not a valid JWT (expected 3 dot-separated parts)"/utf8>>}
            end;

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 555).
-spec check_is_jwt(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_jwt(Value) ->
    check_valid_jwt(Value).

-file("src/intent/checker.gleam", 625).
-spec check_is_iso8601(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_is_iso8601(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            intent@formats:validate_iso8601(S);

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 632).
-spec check_integer_gte(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_integer_gte(Value, N) ->
    case gleam@json:decode(gleam@json:to_string(Value), fun gleam@dynamic:int/1) of
        {ok, Actual} ->
            case Actual >= N of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected integer >= "/utf8,
                                    (gleam@int:to_string(N))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected integer but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 649).
-spec check_integer_gt(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_integer_gt(Value, N) ->
    case gleam@json:decode(gleam@json:to_string(Value), fun gleam@dynamic:int/1) of
        {ok, Actual} ->
            case Actual > N of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected integer > "/utf8,
                                    (gleam@int:to_string(N))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected integer but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 666).
-spec check_integer_lte(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_integer_lte(Value, N) ->
    case gleam@json:decode(gleam@json:to_string(Value), fun gleam@dynamic:int/1) of
        {ok, Actual} ->
            case Actual =< N of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected integer <= "/utf8,
                                    (gleam@int:to_string(N))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected integer but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 683).
-spec check_integer_lt(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_integer_lt(Value, N) ->
    case gleam@json:decode(gleam@json:to_string(Value), fun gleam@dynamic:int/1) of
        {ok, Actual} ->
            case Actual < N of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected integer < "/utf8,
                                    (gleam@int:to_string(N))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected integer but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 700).
-spec check_integer_between(gleam@json:json(), integer(), integer()) -> {ok,
        nil} |
    {error, binary()}.
check_integer_between(Value, Low, High) ->
    case gleam@json:decode(gleam@json:to_string(Value), fun gleam@dynamic:int/1) of
        {ok, Actual} ->
            case (Actual >= Low) andalso (Actual =< High) of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<<<<<"Expected integer between "/utf8,
                                            (gleam@int:to_string(Low))/binary>>/binary,
                                        " and "/utf8>>/binary,
                                    (gleam@int:to_string(High))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected integer but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 719).
-spec check_number_between(gleam@json:json(), float(), float()) -> {ok, nil} |
    {error, binary()}.
check_number_between(Value, Low, High) ->
    Json_str = gleam@json:to_string(Value),
    case gleam@json:decode(Json_str, fun gleam@dynamic:float/1) of
        {ok, Actual} ->
            case (Actual >= Low) andalso (Actual =< High) of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<<<<<"Expected number between "/utf8,
                                            (gleam@float:to_string(Low))/binary>>/binary,
                                        " and "/utf8>>/binary,
                                    (gleam@float:to_string(High))/binary>>/binary,
                                " but got "/utf8>>/binary,
                            (gleam@float:to_string(Actual))/binary>>}
            end;

        {error, _} ->
            case gleam@json:decode(Json_str, fun gleam@dynamic:int/1) of
                {ok, I} ->
                    Actual@1 = gleam@int:to_float(I),
                    case (Actual@1 >= Low) andalso (Actual@1 =< High) of
                        true ->
                            {ok, nil};

                        false ->
                            {error,
                                <<<<<<<<<<"Expected number between "/utf8,
                                                    (gleam@float:to_string(Low))/binary>>/binary,
                                                " and "/utf8>>/binary,
                                            (gleam@float:to_string(High))/binary>>/binary,
                                        " but got "/utf8>>/binary,
                                    (gleam@float:to_string(Actual@1))/binary>>}
                    end;

                {error, _} ->
                    {error,
                        <<"Expected number but got "/utf8, Json_str/binary>>}
            end
    end.

-file("src/intent/checker.gleam", 761).
-spec check_not_null(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_not_null(Value) ->
    case gleam@json:to_string(Value) of
        <<"null"/utf8>> ->
            {error, <<"Expected non-null value but got null"/utf8>>};

        _ ->
            {ok, nil}
    end.

-file("src/intent/checker.gleam", 768).
-spec check_non_empty_array(gleam@json:json()) -> {ok, nil} | {error, binary()}.
check_non_empty_array(Value) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Arr} ->
            case gleam@list:is_empty(Arr) of
                true ->
                    {error,
                        <<"Expected non-empty array but got empty array"/utf8>>};

                false ->
                    {ok, nil}
            end;

        {error, _} ->
            {error,
                <<"Expected array but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 779).
-spec check_array_of_length(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_array_of_length(Value, N) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Arr} ->
            Actual_len = erlang:length(Arr),
            case Actual_len =:= N of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected array of length "/utf8,
                                    (gleam@int:to_string(N))/binary>>/binary,
                                " but got length "/utf8>>/binary,
                            (gleam@int:to_string(Actual_len))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected array but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 798).
-spec check_array_min_items(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_array_min_items(Value, N) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Arr} ->
            Actual_len = erlang:length(Arr),
            case Actual_len >= N of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected array with at least "/utf8,
                                    (gleam@int:to_string(N))/binary>>/binary,
                                " items but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual_len))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected array but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 817).
-spec check_array_max_items(gleam@json:json(), integer()) -> {ok, nil} |
    {error, binary()}.
check_array_max_items(Value, N) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Arr} ->
            Actual_len = erlang:length(Arr),
            case Actual_len =< N of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<"Expected array with at most "/utf8,
                                    (gleam@int:to_string(N))/binary>>/binary,
                                " items but got "/utf8>>/binary,
                            (gleam@int:to_string(Actual_len))/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected array but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 940).
-spec json_to_raw_string(gleam@json:json()) -> binary().
json_to_raw_string(Value) ->
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

-file("src/intent/checker.gleam", 336).
-spec check_equals_string(gleam@json:json(), binary()) -> {ok, nil} |
    {error, binary()}.
check_equals_string(Value, Expected) ->
    Actual = json_to_raw_string(Value),
    case Actual =:= Expected of
        true ->
            {ok, nil};

        false ->
            {error,
                <<<<<<<<"Expected '"/utf8, Expected/binary>>/binary,
                            "' but got '"/utf8>>/binary,
                        Actual/binary>>/binary,
                    "'"/utf8>>}
    end.

-file("src/intent/checker.gleam", 836).
-spec check_one_of(gleam@json:json(), list(binary())) -> {ok, nil} |
    {error, binary()}.
check_one_of(Value, Options) ->
    Actual = json_to_raw_string(Value),
    case gleam@list:contains(Options, Actual) of
        true ->
            {ok, nil};

        false ->
            {error,
                <<<<<<<<"Expected one of ["/utf8,
                                (gleam@string:join(Options, <<", "/utf8>>))/binary>>/binary,
                            "] but got '"/utf8>>/binary,
                        Actual/binary>>/binary,
                    "'"/utf8>>}
    end.

-file("src/intent/checker.gleam", 851).
-spec check_string_contains_json(gleam@json:json(), gleam@json:json()) -> {ok,
        nil} |
    {error, binary()}.
check_string_contains_json(Value, Expected) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            Expected_str = json_to_raw_string(Expected),
            case gleam_stdlib:contains_string(S, Expected_str) of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        <<<<<<<<"String '"/utf8, S/binary>>/binary,
                                    "' does not contain '"/utf8>>/binary,
                                Expected_str/binary>>/binary,
                            "'"/utf8>>}
            end;

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 952).
-spec json_to_display_string(gleam@json:json()) -> binary().
json_to_display_string(Value) ->
    gleam@json:to_string(Value).

-file("src/intent/checker.gleam", 476).
-spec check_string_matching(gleam@json:json(), binary()) -> {ok, nil} |
    {error, binary()}.
check_string_matching(Value, Pattern) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        fun gleam@dynamic:string/1
    ) of
        {ok, S} ->
            case intent_checker:get_or_compile_regex(Pattern) of
                {ok, Re} ->
                    case gleam@regexp:check(Re, S) of
                        true ->
                            {ok, nil};

                        false ->
                            {error,
                                <<<<<<<<"String '"/utf8, S/binary>>/binary,
                                            "' does not match pattern /"/utf8>>/binary,
                                        Pattern/binary>>/binary,
                                    "/"/utf8>>}
                    end;

                {error, _} ->
                    {error, <<"Invalid regex pattern: "/utf8, Pattern/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected string but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 874).
-spec check_array_where_each(
    gleam@json:json(),
    intent@rule:rule_expr(),
    intent@interpolate:context()
) -> {ok, nil} | {error, binary()}.
check_array_where_each(Value, Inner_rule, Ctx) ->
    case gleam@json:decode(
        gleam@json:to_string(Value),
        gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
    ) of
        {ok, Items} ->
            Failures = begin
                _pipe = Items,
                _pipe@1 = gleam@list:index_map(
                    _pipe,
                    fun(Item, Idx) ->
                        Item_json = intent@parser:dynamic_to_json(Item),
                        Result = case Inner_rule of
                            is_string ->
                                check_is_string(Item_json);

                            is_integer ->
                                check_is_integer(Item_json);

                            is_number ->
                                check_is_number(Item_json);

                            is_boolean ->
                                check_is_boolean(Item_json);

                            is_object ->
                                check_is_object(Item_json);

                            {string_matching, Pattern} ->
                                check_string_matching(Item_json, Pattern);

                            is_email ->
                                check_is_email(Item_json);

                            is_uuid ->
                                check_is_uuid(Item_json);

                            _ ->
                                case evaluate_rule(
                                    <<<<"item["/utf8,
                                            (gleam@int:to_string(Idx))/binary>>/binary,
                                        "]"/utf8>>,
                                    intent@rule:to_string(Inner_rule),
                                    Inner_rule,
                                    Item_json,
                                    Ctx
                                ) of
                                    {check_passed, _, _} ->
                                        {ok, nil};

                                    {check_failed, _, _, _, _, Explanation} ->
                                        {error, Explanation}
                                end
                        end,
                        case Result of
                            {ok, _} ->
                                none;

                            {error, Msg} ->
                                {some, {Idx, Msg}}
                        end
                    end
                ),
                gleam@list:filter_map(_pipe@1, fun(Opt) -> case Opt of
                            {some, Pair} ->
                                {ok, Pair};

                            none ->
                                {error, nil}
                        end end)
            end,
            case Failures of
                [] ->
                    {ok, nil};

                [{Idx@1, Msg@1} | _] ->
                    {error,
                        <<<<<<"Array item at index "/utf8,
                                    (gleam@int:to_string(Idx@1))/binary>>/binary,
                                " failed: "/utf8>>/binary,
                            Msg@1/binary>>}
            end;

        {error, _} ->
            {error,
                <<"Expected array but got "/utf8,
                    (gleam@json:to_string(Value))/binary>>}
    end.

-file("src/intent/checker.gleam", 211).
-spec evaluate_rule(
    binary(),
    binary(),
    intent@rule:rule_expr(),
    gleam@json:json(),
    intent@interpolate:context()
) -> check_result().
evaluate_rule(Field, Rule_str, Parsed, Value, Ctx) ->
    Result = case Parsed of
        {equals, Expected} ->
            check_equals_string(Value, Expected);

        {equals_variable, Var_name} ->
            case intent@interpolate:get_variable(Ctx, Var_name) of
                {some, Expected@1} ->
                    check_equals_json(Value, Expected@1);

                none ->
                    {error,
                        <<<<"Variable '"/utf8, Var_name/binary>>/binary,
                            "' not found"/utf8>>}
            end;

        {equals_int, Expected@2} ->
            check_equals_int(Value, Expected@2);

        {equals_float, Expected@3} ->
            check_equals_float(Value, Expected@3);

        {equals_bool, Expected@4} ->
            check_equals_bool(Value, Expected@4);

        is_string ->
            check_is_string(Value);

        is_integer ->
            check_is_integer(Value);

        is_number ->
            check_is_number(Value);

        is_boolean ->
            check_is_boolean(Value);

        is_array ->
            check_is_array(Value);

        is_object ->
            check_is_object(Value);

        is_null ->
            check_is_null(Value);

        non_empty_string ->
            check_non_empty_string(Value);

        {string_matching, Pattern} ->
            check_string_matching(Value, Pattern);

        {string_starting_with, Prefix} ->
            check_string_starting_with(Value, Prefix);

        {string_ending_with, Suffix} ->
            check_string_ending_with(Value, Suffix);

        {string_containing, Substring} ->
            check_string_containing(Value, Substring);

        is_email ->
            check_is_email(Value);

        is_uuid ->
            check_is_uuid(Value);

        is_uri ->
            check_is_uri(Value);

        is_jwt ->
            check_is_jwt(Value);

        is_iso8601 ->
            check_is_iso8601(Value);

        valid_jwt ->
            check_valid_jwt(Value);

        valid_iso8601 ->
            check_is_iso8601(Value);

        {integer_gte, N} ->
            check_integer_gte(Value, N);

        {integer_gt, N@1} ->
            check_integer_gt(Value, N@1);

        {integer_lte, N@2} ->
            check_integer_lte(Value, N@2);

        {integer_lt, N@3} ->
            check_integer_lt(Value, N@3);

        {integer_between, Low, High} ->
            check_integer_between(Value, Low, High);

        {number_between, Low@1, High@1} ->
            check_number_between(Value, Low@1, High@1);

        not_null ->
            check_not_null(Value);

        non_empty_array ->
            check_non_empty_array(Value);

        {array_of_length, N@4} ->
            check_array_of_length(Value, N@4);

        {array_with_min_items, N@5} ->
            check_array_min_items(Value, N@5);

        {array_with_max_items, N@6} ->
            check_array_max_items(Value, N@6);

        {one_of, Options} ->
            check_one_of(Value, Options);

        {contains_variable, Var_name@1} ->
            case intent@interpolate:get_variable(Ctx, Var_name@1) of
                {some, Expected@5} ->
                    check_string_contains_json(Value, Expected@5);

                none ->
                    {error,
                        <<<<"Variable '"/utf8, Var_name@1/binary>>/binary,
                            "' not found"/utf8>>}
            end;

        {array_where_each, Inner_rule} ->
            check_array_where_each(Value, Inner_rule, Ctx);

        {raw, Raw} ->
            case gleam@string:starts_with(Raw, <<"equals "/utf8>>) of
                true ->
                    Expected@6 = gleam@string:drop_left(Raw, 7),
                    check_equals_string(Value, Expected@6);

                false ->
                    {error, <<"Unknown rule: "/utf8, Raw/binary>>}
            end;

        _ ->
            {error,
                <<"Rule not implemented: "/utf8,
                    (intent@rule:to_string(Parsed))/binary>>}
    end,
    case Result of
        {ok, _} ->
            {check_passed, Field, Rule_str};

        {error, Explanation} ->
            {check_failed,
                Field,
                Rule_str,
                Rule_str,
                json_to_display_string(Value),
                Explanation}
    end.

-file("src/intent/checker.gleam", 190).
-spec check_rule(
    binary(),
    binary(),
    gleam@option:option(gleam@json:json()),
    intent@interpolate:context()
) -> check_result().
check_rule(Field, Rule_str, Body, Ctx) ->
    Parsed = intent@rule:parse(Rule_str),
    case get_field_value(Body, Field) of
        none ->
            {check_failed,
                Field,
                Rule_str,
                Rule_str,
                <<"field missing"/utf8>>,
                <<<<"Field '"/utf8, Field/binary>>/binary,
                    "' not found in response"/utf8>>};

        {some, Value} ->
            evaluate_rule(Field, Rule_str, Parsed, Value, Ctx)
    end.

-file("src/intent/checker.gleam", 137).
?DOC(" Check a single field against its rule\n").
-spec check_field(
    binary(),
    intent@types:check(),
    gleam@option:option(gleam@json:json()),
    intent@interpolate:context()
) -> check_result().
check_field(Field, Check, Body, Ctx) ->
    Parsed_rule = intent@rule:parse(erlang:element(2, Check)),
    Interpolated_rule = interpolate_rule(erlang:element(2, Check), Ctx),
    case Parsed_rule of
        absent ->
            check_absent(Field, Body);

        present ->
            check_present(Field, Body);

        _ ->
            check_rule(Field, Interpolated_rule, Body, Ctx)
    end.

-file("src/intent/checker.gleam", 43).
?DOC(" Check an execution result against expected response\n").
-spec check_response(
    intent@types:response(),
    intent@http_client:execution_result(),
    intent@interpolate:context()
) -> response_check_result().
check_response(Expected, Actual, Ctx) ->
    Status_ok = erlang:element(2, Actual) =:= erlang:element(2, Expected),
    {Body_passed, Body_failed} = begin
        _pipe = erlang:element(4, Expected),
        _pipe@1 = maps:to_list(_pipe),
        _pipe@2 = gleam@list:map(
            _pipe@1,
            fun(Pair) ->
                {Field, Check} = Pair,
                check_field(Field, Check, erlang:element(4, Actual), Ctx)
            end
        ),
        gleam@list:partition(_pipe@2, fun(Result) -> case Result of
                    {check_passed, _, _} ->
                        true;

                    {check_failed, _, _, _, _, _} ->
                        false
                end end)
    end,
    {Header_passed, Header_failed} = case erlang:element(5, Expected) of
        none ->
            {[], []};

        {some, Expected_headers} ->
            _pipe@3 = Expected_headers,
            _pipe@4 = maps:to_list(_pipe@3),
            _pipe@5 = gleam@list:map(
                _pipe@4,
                fun(Pair@1) ->
                    {Header_name, Expected_value} = Pair@1,
                    check_header(
                        Header_name,
                        Expected_value,
                        erlang:element(3, Actual)
                    )
                end
            ),
            gleam@list:partition(_pipe@5, fun(Result@1) -> case Result@1 of
                        {check_passed, _, _} ->
                            true;

                        {check_failed, _, _, _, _, _} ->
                            false
                    end end)
    end,
    {response_check_result,
        lists:append(Body_passed, Header_passed),
        lists:append(Body_failed, Header_failed),
        Status_ok,
        erlang:element(2, Expected),
        erlang:element(2, Actual)}.
