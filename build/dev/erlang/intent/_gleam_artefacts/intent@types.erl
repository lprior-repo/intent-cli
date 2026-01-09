-module(intent@types).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/types.gleam").
-export([method_to_string/1, method_from_string/1]).
-export_type([spec/0, config/0, feature/0, behavior/0, method/0, request/0, response/0, check/0, rule/0, 'when'/0, rule_check/0, anti_pattern/0, a_i_hints/0, implementation_hints/0, entity_hint/0, security_hints/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type spec() :: {spec,
        binary(),
        binary(),
        binary(),
        binary(),
        list(binary()),
        config(),
        list(feature()),
        list(rule()),
        list(anti_pattern()),
        a_i_hints()}.

-type config() :: {config,
        binary(),
        integer(),
        gleam@dict:dict(binary(), binary())}.

-type feature() :: {feature, binary(), binary(), list(behavior())}.

-type behavior() :: {behavior,
        binary(),
        binary(),
        binary(),
        list(binary()),
        list(binary()),
        request(),
        response(),
        gleam@dict:dict(binary(), binary())}.

-type method() :: get | post | put | patch | delete | head | options.

-type request() :: {request,
        method(),
        binary(),
        gleam@dict:dict(binary(), binary()),
        gleam@dict:dict(binary(), gleam@json:json()),
        gleam@json:json()}.

-type response() :: {response,
        integer(),
        gleam@json:json(),
        gleam@dict:dict(binary(), check()),
        gleam@dict:dict(binary(), binary())}.

-type check() :: {check, binary(), binary()}.

-type rule() :: {rule,
        binary(),
        binary(),
        'when'(),
        rule_check(),
        gleam@json:json()}.

-type 'when'() :: {'when', binary(), method(), binary()}.

-type rule_check() :: {rule_check,
        list(binary()),
        list(binary()),
        list(binary()),
        list(binary()),
        binary(),
        binary()}.

-type anti_pattern() :: {anti_pattern,
        binary(),
        binary(),
        gleam@json:json(),
        gleam@json:json(),
        binary()}.

-type a_i_hints() :: {a_i_hints,
        implementation_hints(),
        gleam@dict:dict(binary(), entity_hint()),
        security_hints(),
        list(binary())}.

-type implementation_hints() :: {implementation_hints, list(binary())}.

-type entity_hint() :: {entity_hint, gleam@dict:dict(binary(), binary())}.

-type security_hints() :: {security_hints,
        binary(),
        binary(),
        binary(),
        binary()}.

-file("src/intent/types.gleam", 59).
?DOC(" Convert method to string\n").
-spec method_to_string(method()) -> binary().
method_to_string(Method) ->
    case Method of
        get ->
            <<"GET"/utf8>>;

        post ->
            <<"POST"/utf8>>;

        put ->
            <<"PUT"/utf8>>;

        patch ->
            <<"PATCH"/utf8>>;

        delete ->
            <<"DELETE"/utf8>>;

        head ->
            <<"HEAD"/utf8>>;

        options ->
            <<"OPTIONS"/utf8>>
    end.

-file("src/intent/types.gleam", 72).
?DOC(" Parse method from string\n").
-spec method_from_string(binary()) -> {ok, method()} | {error, binary()}.
method_from_string(S) ->
    case S of
        <<"GET"/utf8>> ->
            {ok, get};

        <<"POST"/utf8>> ->
            {ok, post};

        <<"PUT"/utf8>> ->
            {ok, put};

        <<"PATCH"/utf8>> ->
            {ok, patch};

        <<"DELETE"/utf8>> ->
            {ok, delete};

        <<"HEAD"/utf8>> ->
            {ok, head};

        <<"OPTIONS"/utf8>> ->
            {ok, options};

        _ ->
            {error, <<"Unknown HTTP method: "/utf8, S/binary>>}
    end.
