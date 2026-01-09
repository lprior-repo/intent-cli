-module(intent@http_client).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/http_client.gleam").
-export([execute_request/3]).
-export_type([execution_result/0, execution_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type execution_result() :: {execution_result,
        integer(),
        gleam@dict:dict(binary(), binary()),
        gleam@json:json(),
        binary(),
        integer(),
        intent@types:method(),
        binary()}.

-type execution_error() :: {url_parse_error, binary()} |
    {interpolation_error, binary()} |
    {request_error, binary()} |
    {response_parse_error, binary()}.

-file("src/intent/http_client.gleam", 80).
-spec interpolate_path(binary(), intent@interpolate:context()) -> {ok, binary()} |
    {error, execution_error()}.
interpolate_path(Path, Ctx) ->
    _pipe = intent@interpolate:interpolate_string(Ctx, Path),
    gleam@result:map_error(
        _pipe,
        fun(Field@0) -> {interpolation_error, Field@0} end
    ).

-file("src/intent/http_client.gleam", 85).
-spec interpolate_headers(
    gleam@dict:dict(binary(), binary()),
    intent@interpolate:context()
) -> {ok, gleam@dict:dict(binary(), binary())} | {error, execution_error()}.
interpolate_headers(Headers, Ctx) ->
    _pipe = intent@interpolate:interpolate_headers(Ctx, Headers),
    gleam@result:map_error(
        _pipe,
        fun(Field@0) -> {interpolation_error, Field@0} end
    ).

-file("src/intent/http_client.gleam", 93).
-spec interpolate_body(gleam@json:json(), intent@interpolate:context()) -> {ok,
        gleam@json:json()} |
    {error, execution_error()}.
interpolate_body(Body, Ctx) ->
    Body_str = gleam@json:to_string(Body),
    case intent@interpolate:interpolate_string(Ctx, Body_str) of
        {ok, Interpolated_str} ->
            case gleam@json:decode(
                Interpolated_str,
                fun gleam@dynamic:dynamic/1
            ) of
                {ok, Data} ->
                    {ok, intent@parser:dynamic_to_json(Data)};

                {error, _} ->
                    {error,
                        {interpolation_error,
                            <<"Failed to parse interpolated body as JSON"/utf8>>}}
            end;

        {error, E} ->
            {error, {interpolation_error, E}}
    end.

-file("src/intent/http_client.gleam", 109).
-spec merge_headers(
    gleam@dict:dict(binary(), binary()),
    gleam@dict:dict(binary(), binary())
) -> gleam@dict:dict(binary(), binary()).
merge_headers(Config_headers, Request_headers) ->
    gleam@dict:merge(Config_headers, Request_headers).

-file("src/intent/http_client.gleam", 117).
-spec convert_method(intent@types:method()) -> gleam@http:method().
convert_method(Method) ->
    case Method of
        get ->
            get;

        post ->
            post;

        put ->
            put;

        patch ->
            patch;

        delete ->
            delete;

        head ->
            head;

        options ->
            options
    end.

-file("src/intent/http_client.gleam", 174).
-spec ensure_leading_slash(binary()) -> binary().
ensure_leading_slash(Path) ->
    case gleam@string:starts_with(Path, <<"/"/utf8>>) of
        true ->
            Path;

        false ->
            <<"/"/utf8, Path/binary>>
    end.

-file("src/intent/http_client.gleam", 129).
-spec build_http_request(
    gleam@http:method(),
    gleam@uri:uri(),
    gleam@dict:dict(binary(), binary()),
    gleam@json:json()
) -> {ok, gleam@http@request:request(binary())} | {error, execution_error()}.
build_http_request(Method, Parsed_uri, Headers, Body) ->
    Host = gleam@option:unwrap(
        erlang:element(4, Parsed_uri),
        <<"localhost"/utf8>>
    ),
    Path = ensure_leading_slash(erlang:element(6, Parsed_uri)),
    Port = erlang:element(5, Parsed_uri),
    Scheme = case erlang:element(2, Parsed_uri) of
        {some, <<"https"/utf8>>} ->
            https;

        _ ->
            http
    end,
    Req = begin
        _pipe = gleam@http@request:new(),
        _pipe@1 = gleam@http@request:set_method(_pipe, Method),
        _pipe@2 = gleam@http@request:set_host(_pipe@1, Host),
        _pipe@3 = gleam@http@request:set_path(_pipe@2, Path),
        gleam@http@request:set_scheme(_pipe@3, Scheme)
    end,
    Req@1 = case Port of
        {some, P} ->
            gleam@http@request:set_port(Req, P);

        none ->
            Req
    end,
    Req@2 = gleam@dict:fold(
        Headers,
        Req@1,
        fun(Acc, Key, Value) ->
            gleam@http@request:set_header(
                Acc,
                gleam@string:lowercase(Key),
                Value
            )
        end
    ),
    Body_str = gleam@json:to_string(Body),
    Req@3 = begin
        _pipe@4 = Req@2,
        _pipe@5 = gleam@http@request:set_body(_pipe@4, Body_str),
        gleam@http@request:set_header(
            _pipe@5,
            <<"content-type"/utf8>>,
            <<"application/json"/utf8>>
        )
    end,
    {ok, Req@3}.

-file("src/intent/http_client.gleam", 197).
-spec parse_response(
    gleam@http@response:response(binary()),
    integer(),
    intent@types:method(),
    binary()
) -> execution_result().
parse_response(Resp, Elapsed_ms, Method, Path) ->
    Headers = begin
        _pipe = erlang:element(3, Resp),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Pair) -> {erlang:element(1, Pair), erlang:element(2, Pair)} end
        ),
        maps:from_list(_pipe@1)
    end,
    Body = case gleam@string:is_empty(erlang:element(4, Resp)) of
        true ->
            gleam@json:null();

        false ->
            case gleam@json:decode(
                erlang:element(4, Resp),
                fun gleam@dynamic:dynamic/1
            ) of
                {ok, Data} ->
                    intent@parser:dynamic_to_json(Data);

                {error, _} ->
                    gleam@json:null()
            end
    end,
    {execution_result,
        erlang:element(2, Resp),
        Headers,
        Body,
        erlang:element(4, Resp),
        Elapsed_ms,
        Method,
        Path}.

-file("src/intent/http_client.gleam", 228).
-spec format_httpc_error(gleam@dynamic:dynamic_()) -> binary().
format_httpc_error(Error) ->
    Error_str = begin
        _pipe = gleam@string:inspect(Error),
        gleam@string:lowercase(_pipe)
    end,
    Check_patterns = fun(Patterns) ->
        gleam@list:any(
            Patterns,
            fun(P) -> gleam_stdlib:contains_string(Error_str, P) end
        )
    end,
    Message = case Check_patterns([<<"timeout"/utf8>>]) of
        true ->
            <<<<<<"Connection timeout: The request took too long to complete.\n"/utf8,
                        "  • Check if the target API is responding slowly\n"/utf8>>/binary,
                    "  • Try increasing the timeout_ms in your config\n"/utf8>>/binary,
                "  • Verify the base_url is correct and accessible"/utf8>>;

        false ->
            case Check_patterns(
                [<<"econnrefused"/utf8>>, <<"connection_refused"/utf8>>]
            ) of
                true ->
                    <<<<<<"Connection refused: Cannot connect to the target server.\n"/utf8,
                                "  • Check if the base_url is correct\n"/utf8>>/binary,
                            "  • Verify the server is running and listening on the specified port\n"/utf8>>/binary,
                        "  • Ensure your network firewall allows connections to this server"/utf8>>;

                false ->
                    case Check_patterns(
                        [<<"nxdomain"/utf8>>, <<"enotfound"/utf8>>]
                    ) of
                        true ->
                            <<<<<<"DNS resolution failed: Cannot find the hostname.\n"/utf8,
                                        "  • Check if the base_url hostname is spelled correctly\n"/utf8>>/binary,
                                    "  • Verify your network connection\n"/utf8>>/binary,
                                "  • Try pinging the hostname to test DNS resolution"/utf8>>;

                        false ->
                            case Check_patterns(
                                [<<"ssl"/utf8>>, <<"certificate"/utf8>>]
                            ) of
                                true ->
                                    <<<<<<"SSL/TLS certificate error: Cannot verify the server's certificate.\n"/utf8,
                                                "  • The server may have an invalid or expired certificate\n"/utf8>>/binary,
                                            "  • Check if your system's certificate store is up to date\n"/utf8>>/binary,
                                        "  • For development, ensure you're using the correct base_url scheme (http vs https)"/utf8>>;

                                false ->
                                    case Check_patterns([<<"eacces"/utf8>>]) of
                                        true ->
                                            <<<<"Permission denied: No access to the specified resource.\n"/utf8,
                                                    "  • Check if you have permission to access the target URL\n"/utf8>>/binary,
                                                "  • Verify the base_url and path are correct"/utf8>>;

                                        false ->
                                            case Check_patterns(
                                                [<<"ehostunreach"/utf8>>,
                                                    <<"enetunreach"/utf8>>]
                                            ) of
                                                true ->
                                                    <<<<<<"Network unreachable: Cannot reach the target host.\n"/utf8,
                                                                "  • Check your network connection\n"/utf8>>/binary,
                                                            "  • Verify the host is accessible from your location\n"/utf8>>/binary,
                                                        "  • Check for firewall or VPN restrictions"/utf8>>;

                                                false ->
                                                    <<<<<<<<<<"HTTP request failed: "/utf8,
                                                                        (gleam@string:inspect(
                                                                            Error
                                                                        ))/binary>>/binary,
                                                                    "\n"/utf8>>/binary,
                                                                "  • Check the base_url and ensure the target server is reachable\n"/utf8>>/binary,
                                                            "  • Verify the request path and headers are correct\n"/utf8>>/binary,
                                                        "  • Try running with a simpler request to isolate the issue"/utf8>>
                                            end
                                    end
                            end
                    end
            end
    end,
    Message.

-file("src/intent/http_client.gleam", 181).
-spec execute_with_timing(
    gleam@http@request:request(binary()),
    intent@types:method(),
    binary()
) -> {ok, execution_result()} | {error, execution_error()}.
execute_with_timing(Req, Method, Path) ->
    Start = intent_ffi:now_ms(),
    case gleam@httpc:send(Req) of
        {ok, Resp} ->
            Elapsed = intent_ffi:now_ms() - Start,
            {ok, parse_response(Resp, Elapsed, Method, Path)};

        {error, E} ->
            {error, {request_error, format_httpc_error(E)}}
    end.

-file("src/intent/http_client.gleam", 41).
?DOC(" Execute a behavior request against the target\n").
-spec execute_request(
    intent@types:config(),
    intent@types:request(),
    intent@interpolate:context()
) -> {ok, execution_result()} | {error, execution_error()}.
execute_request(Config, Req, Ctx) ->
    Base_url = erlang:element(2, Config),
    gleam@result:'try'(
        interpolate_path(erlang:element(3, Req), Ctx),
        fun(Path) ->
            Full_url = <<Base_url/binary, Path/binary>>,
            gleam@result:'try'(
                begin
                    _pipe = gleam@uri:parse(Full_url),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) ->
                            {url_parse_error,
                                <<"Invalid URL: "/utf8, Full_url/binary>>}
                        end
                    )
                end,
                fun(Parsed_uri) ->
                    gleam@result:'try'(
                        interpolate_headers(erlang:element(4, Req), Ctx),
                        fun(Request_headers) ->
                            Merged_headers = merge_headers(
                                erlang:element(4, Config),
                                Request_headers
                            ),
                            gleam@result:'try'(
                                interpolate_body(erlang:element(6, Req), Ctx),
                                fun(Interpolated_body) ->
                                    Method = convert_method(
                                        erlang:element(2, Req)
                                    ),
                                    gleam@result:'try'(
                                        build_http_request(
                                            Method,
                                            Parsed_uri,
                                            Merged_headers,
                                            Interpolated_body
                                        ),
                                        fun(Http_req) ->
                                            execute_with_timing(
                                                Http_req,
                                                erlang:element(2, Req),
                                                Path
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
