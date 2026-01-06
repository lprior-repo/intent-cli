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

-file("src/intent/checker.gleam", 105).
?DOC(" Convert internal CheckResult to public CheckResult\n").
-spec convert_check_result(intent@checker@types:check_result()) -> check_result().
convert_check_result(Result) ->
    case Result of
        {check_passed, Field, Rule} ->
            {check_passed, Field, Rule};

        {check_failed, Field@1, Rule@1, Expected, Actual, Explanation} ->
            {check_failed, Field@1, Rule@1, Expected, Actual, Explanation}
    end.

-file("src/intent/checker.gleam", 132).
-spec interpolate_rule(binary(), intent@interpolate:context()) -> binary().
interpolate_rule(Rule_str, Ctx) ->
    case intent@interpolate:interpolate_string(Ctx, Rule_str) of
        {ok, Interpolated} ->
            Interpolated;

        {error, _} ->
            Rule_str
    end.

-file("src/intent/checker.gleam", 139).
-spec check_absent(binary(), gleam@json:json()) -> intent@checker@types:check_result().
check_absent(Field, Body) ->
    case intent@checker@json:get_field_value(Body, Field) of
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

-file("src/intent/checker.gleam", 153).
-spec check_present(binary(), gleam@json:json()) -> intent@checker@types:check_result().
check_present(Field, Body) ->
    case intent@checker@json:get_field_value(Body, Field) of
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

-file("src/intent/checker.gleam", 167).
-spec check_rule(
    binary(),
    binary(),
    gleam@json:json(),
    intent@interpolate:context()
) -> intent@checker@types:check_result().
check_rule(Field, Rule_str, Body, Ctx) ->
    Parsed = intent@rule:parse(Rule_str),
    case intent@checker@json:get_field_value(Body, Field) of
        none ->
            {check_failed,
                Field,
                Rule_str,
                Rule_str,
                <<"field missing"/utf8>>,
                <<<<"Field '"/utf8, Field/binary>>/binary,
                    "' not found in response"/utf8>>};

        {some, Value} ->
            intent@checker@rules:evaluate_rule(
                Field,
                Rule_str,
                Parsed,
                Value,
                Ctx
            )
    end.

-file("src/intent/checker.gleam", 114).
?DOC(" Check a single field against its rule\n").
-spec check_field(
    binary(),
    intent@types:check(),
    gleam@json:json(),
    intent@interpolate:context()
) -> intent@checker@types:check_result().
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

-file("src/intent/checker.gleam", 49).
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
    {Header_passed, Header_failed} = begin
        _pipe@3 = erlang:element(5, Expected),
        _pipe@4 = maps:to_list(_pipe@3),
        _pipe@5 = gleam@list:map(
            _pipe@4,
            fun(Pair@1) ->
                {Header_name, Expected_value} = Pair@1,
                intent@checker@headers:check_header(
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
    Passed = begin
        _pipe@6 = lists:append(Body_passed, Header_passed),
        gleam@list:map(_pipe@6, fun convert_check_result/1)
    end,
    Failed = begin
        _pipe@7 = lists:append(Body_failed, Header_failed),
        gleam@list:map(_pipe@7, fun convert_check_result/1)
    end,
    {response_check_result,
        Passed,
        Failed,
        Status_ok,
        erlang:element(2, Expected),
        erlang:element(2, Actual)}.
