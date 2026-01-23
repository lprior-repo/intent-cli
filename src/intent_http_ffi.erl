-module(intent_http_ffi).
-export([request_no_body/4, request_with_body/5]).

%% HTTP request without body (GET, HEAD, OPTIONS)
request_no_body(Method, Url, Headers, TimeoutMs) when is_binary(Method), is_binary(Url), is_integer(TimeoutMs) ->
    ensure_inets_started(),
    MethodAtom = binary_to_atom(Method, utf8),
    UrlList = binary_to_list(Url),
    HeaderList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    HttpOptions = [
        {timeout, TimeoutMs},
        {connect_timeout, TimeoutMs},
        {ssl, [{verify, verify_none}]}
    ],
    Options = [
        {body_format, binary},
        {socket_opts, [{ipfamily, inet6fb4}]}
    ],
    Request = {UrlList, HeaderList},
    do_request(MethodAtom, Request, HttpOptions, Options).

%% HTTP request with body (POST, PUT, PATCH, DELETE)
request_with_body(Method, Url, Headers, Body, TimeoutMs) when is_binary(Method), is_binary(Url), is_binary(Body), is_integer(TimeoutMs) ->
    ensure_inets_started(),
    MethodAtom = binary_to_atom(Method, utf8),
    UrlList = binary_to_list(Url),
    HeaderList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    HttpOptions = [
        {timeout, TimeoutMs},
        {connect_timeout, TimeoutMs},
        {ssl, [{verify, verify_none}]}
    ],
    Options = [
        {body_format, binary},
        {socket_opts, [{ipfamily, inet6fb4}]}
    ],
    ContentType = proplists:get_value("content-type", HeaderList, "application/json"),
    Request = {UrlList, HeaderList, ContentType, Body},
    do_request(MethodAtom, Request, HttpOptions, Options).

do_request(Method, Request, HttpOptions, Options) ->
    case httpc:request(Method, Request, HttpOptions, Options) of
        {ok, {{_, Status, _}, RespHeaders, Body}} ->
            BinHeaders = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- RespHeaders],
            BinBody = case is_binary(Body) of
                true -> Body;
                false -> list_to_binary(Body)
            end,
            {ok, {Status, BinHeaders, BinBody}};
        {error, timeout} ->
            {error, <<"timeout">>};
        {error, {failed_connect, [{to_address, _}, {inet, _, econnrefused}]}} ->
            {error, <<"econnrefused">>};
        {error, {failed_connect, _}} ->
            {error, <<"connection_failed">>};
        {error, nxdomain} ->
            {error, <<"nxdomain">>};
        {error, Reason} ->
            ReasonBin = list_to_binary(io_lib:format("~p", [Reason])),
            {error, ReasonBin}
    end.

ensure_inets_started() ->
    case inets:start() of
        ok -> ok;
        {error, {already_started, inets}} -> ok
    end.
