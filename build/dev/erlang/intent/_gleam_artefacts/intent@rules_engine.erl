-module(intent@rules_engine).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/rules_engine.gleam").
-export([check_rules/3, format_violation/1]).
-export_type([rule_result/0, rule_violation/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type rule_result() :: {rule_passed, binary()} |
    {rule_failed, binary(), binary(), list(rule_violation())}.

-type rule_violation() :: {body_contains, binary(), binary()} |
    {body_missing, binary()} |
    {field_missing, binary()} |
    {field_present, binary()} |
    {header_missing, binary()} |
    {header_present, binary()}.

-file("src/intent/rules_engine.gleam", 77).
-spec check_path_pattern(binary(), binary()) -> boolean().
check_path_pattern(Pattern, Path) ->
    case Pattern =:= Path of
        true ->
            true;

        false ->
            case gleam@regexp:from_string(Pattern) of
                {ok, Re} ->
                    gleam@regexp:check(Re, Path);

                {error, _} ->
                    false
            end
    end.

-file("src/intent/rules_engine.gleam", 91).
-spec check_status_condition(binary(), integer()) -> boolean().
check_status_condition(Expr, Status) ->
    Expr@1 = gleam@string:trim(Expr),
    case gleam@string:starts_with(Expr@1, <<">= "/utf8>>) of
        true ->
            N_str = gleam@string:drop_left(Expr@1, 3),
            case gleam@int:parse(N_str) of
                {ok, N} ->
                    Status >= N;

                {error, _} ->
                    false
            end;

        false ->
            case gleam@string:starts_with(Expr@1, <<"> "/utf8>>) of
                true ->
                    N_str@1 = gleam@string:drop_left(Expr@1, 2),
                    case gleam@int:parse(N_str@1) of
                        {ok, N@1} ->
                            Status > N@1;

                        {error, _} ->
                            false
                    end;

                false ->
                    case gleam@string:starts_with(Expr@1, <<"<= "/utf8>>) of
                        true ->
                            N_str@2 = gleam@string:drop_left(Expr@1, 3),
                            case gleam@int:parse(N_str@2) of
                                {ok, N@2} ->
                                    Status =< N@2;

                                {error, _} ->
                                    false
                            end;

                        false ->
                            case gleam@string:starts_with(Expr@1, <<"< "/utf8>>) of
                                true ->
                                    N_str@3 = gleam@string:drop_left(Expr@1, 2),
                                    case gleam@int:parse(N_str@3) of
                                        {ok, N@3} ->
                                            Status < N@3;

                                        {error, _} ->
                                            false
                                    end;

                                false ->
                                    case gleam@string:starts_with(
                                        Expr@1,
                                        <<"== "/utf8>>
                                    ) of
                                        true ->
                                            N_str@4 = gleam@string:drop_left(
                                                Expr@1,
                                                3
                                            ),
                                            case gleam@int:parse(N_str@4) of
                                                {ok, N@4} ->
                                                    Status =:= N@4;

                                                {error, _} ->
                                                    false
                                            end;

                                        false ->
                                            case gleam@int:parse(Expr@1) of
                                                {ok, N@5} ->
                                                    Status =:= N@5;

                                                {error, _} ->
                                                    false
                                            end
                                    end
                            end
                    end
            end
    end.

-file("src/intent/rules_engine.gleam", 54).
-spec check_when_conditions(
    intent@types:'when'(),
    intent@http_client:execution_result()
) -> boolean().
check_when_conditions(When, Response) ->
    Status_ok = case erlang:element(2, When) of
        none ->
            true;

        {some, Status_expr} ->
            check_status_condition(Status_expr, erlang:element(2, Response))
    end,
    Method_ok = case erlang:element(3, When) of
        none ->
            true;

        {some, Expected_method} ->
            erlang:element(7, Response) =:= Expected_method
    end,
    Path_ok = case erlang:element(4, When) of
        none ->
            true;

        {some, Path_pattern} ->
            check_path_pattern(Path_pattern, erlang:element(8, Response))
    end,
    (Status_ok andalso Method_ok) andalso Path_ok.

-file("src/intent/rules_engine.gleam", 47).
?DOC(" Check if a rule applies based on its `when` conditions\n").
-spec rule_applies(intent@types:rule(), intent@http_client:execution_result()) -> boolean().
rule_applies(Rule, Response) ->
    case erlang:element(4, Rule) of
        none ->
            true;

        {some, When} ->
            check_when_conditions(When, Response)
    end.

-file("src/intent/rules_engine.gleam", 241).
-spec contains_string(binary(), binary()) -> boolean().
contains_string(Body, Needle) ->
    gleam_stdlib:contains_string(
        gleam@string:lowercase(Body),
        gleam@string:lowercase(Needle)
    ).

-file("src/intent/rules_engine.gleam", 255).
-spec navigate_and_check(gleam@json:json(), list(binary())) -> boolean().
navigate_and_check(Value, Path) ->
    case Path of
        [] ->
            true;

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
                            navigate_and_check(Next_json, Rest);

                        {error, _} ->
                            false
                    end;

                {error, _} ->
                    false
            end
    end.

-file("src/intent/rules_engine.gleam", 245).
-spec field_exists(gleam@option:option(gleam@json:json()), binary()) -> boolean().
field_exists(Body, Field_path) ->
    case Body of
        none ->
            false;

        {some, Json_val} ->
            Parts = gleam@string:split(Field_path, <<"."/utf8>>),
            navigate_and_check(Json_val, Parts)
    end.

-file("src/intent/rules_engine.gleam", 277).
-spec header_exists(gleam@dict:dict(binary(), binary()), binary()) -> boolean().
header_exists(Headers, Header_name) ->
    Lower_name = gleam@string:lowercase(Header_name),
    _pipe = Headers,
    _pipe@1 = maps:to_list(_pipe),
    gleam@list:any(
        _pipe@1,
        fun(Pair) ->
            gleam@string:lowercase(erlang:element(1, Pair)) =:= Lower_name
        end
    ).

-file("src/intent/rules_engine.gleam", 164).
-spec collect_violations(
    intent@types:rule_check(),
    intent@http_client:execution_result()
) -> list(rule_violation()).
collect_violations(Check, Response) ->
    Violations = [],
    Violations@1 = case erlang:element(2, Check) of
        none ->
            Violations;

        {some, Forbidden_strings} ->
            gleam@list:fold(
                Forbidden_strings,
                Violations,
                fun(Acc, Forbidden) ->
                    case contains_string(erlang:element(5, Response), Forbidden) of
                        true ->
                            [{body_contains,
                                    Forbidden,
                                    <<"response body"/utf8>>} |
                                Acc];

                        false ->
                            Acc
                    end
                end
            )
    end,
    Violations@2 = case erlang:element(3, Check) of
        none ->
            Violations@1;

        {some, Required_strings} ->
            gleam@list:fold(
                Required_strings,
                Violations@1,
                fun(Acc@1, Required) ->
                    case contains_string(erlang:element(5, Response), Required) of
                        true ->
                            Acc@1;

                        false ->
                            [{body_missing, Required} | Acc@1]
                    end
                end
            )
    end,
    Violations@3 = case erlang:element(4, Check) of
        none ->
            Violations@2;

        {some, Required_fields} ->
            gleam@list:fold(
                Required_fields,
                Violations@2,
                fun(Acc@2, Field) ->
                    case field_exists(erlang:element(4, Response), Field) of
                        true ->
                            Acc@2;

                        false ->
                            [{field_missing, Field} | Acc@2]
                    end
                end
            )
    end,
    Violations@4 = case erlang:element(5, Check) of
        none ->
            Violations@3;

        {some, Forbidden_fields} ->
            gleam@list:fold(
                Forbidden_fields,
                Violations@3,
                fun(Acc@3, Field@1) ->
                    case field_exists(erlang:element(4, Response), Field@1) of
                        true ->
                            [{field_present, Field@1} | Acc@3];

                        false ->
                            Acc@3
                    end
                end
            )
    end,
    Violations@5 = case erlang:element(6, Check) of
        none ->
            Violations@4;

        {some, Required_header} ->
            case header_exists(erlang:element(3, Response), Required_header) of
                true ->
                    Violations@4;

                false ->
                    [{header_missing, Required_header} | Violations@4]
            end
    end,
    Violations@6 = case erlang:element(7, Check) of
        none ->
            Violations@5;

        {some, Forbidden_header} ->
            case header_exists(erlang:element(3, Response), Forbidden_header) of
                true ->
                    [{header_present, Forbidden_header} | Violations@5];

                false ->
                    Violations@5
            end
    end,
    Violations@6.

-file("src/intent/rules_engine.gleam", 151).
?DOC(" Check a single rule against a response\n").
-spec check_rule(
    intent@types:rule(),
    intent@http_client:execution_result(),
    binary()
) -> rule_result().
check_rule(Rule, Response, _) ->
    Violations = collect_violations(erlang:element(5, Rule), Response),
    case gleam@list:is_empty(Violations) of
        true ->
            {rule_passed, erlang:element(2, Rule)};

        false ->
            {rule_failed,
                erlang:element(2, Rule),
                erlang:element(3, Rule),
                Violations}
    end.

-file("src/intent/rules_engine.gleam", 36).
?DOC(" Check all global rules against a response\n").
-spec check_rules(
    list(intent@types:rule()),
    intent@http_client:execution_result(),
    binary()
) -> list(rule_result()).
check_rules(Rules, Response, Behavior_name) ->
    _pipe = Rules,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Rule) -> rule_applies(Rule, Response) end
    ),
    gleam@list:map(
        _pipe@1,
        fun(Rule@1) -> check_rule(Rule@1, Response, Behavior_name) end
    ).

-file("src/intent/rules_engine.gleam", 285).
?DOC(" Format a rule violation as a human-readable string\n").
-spec format_violation(rule_violation()) -> binary().
format_violation(Violation) ->
    case Violation of
        {body_contains, Forbidden, Location} ->
            <<<<<<"Found forbidden string '"/utf8, Forbidden/binary>>/binary,
                    "' in "/utf8>>/binary,
                Location/binary>>;

        {body_missing, Required} ->
            <<<<"Required string '"/utf8, Required/binary>>/binary,
                "' not found in response body"/utf8>>;

        {field_missing, Field} ->
            <<<<"Required field '"/utf8, Field/binary>>/binary,
                "' not found"/utf8>>;

        {field_present, Field@1} ->
            <<<<"Forbidden field '"/utf8, Field@1/binary>>/binary,
                "' is present in response"/utf8>>;

        {header_missing, Header} ->
            <<<<"Required header '"/utf8, Header/binary>>/binary,
                "' not found"/utf8>>;

        {header_present, Header@1} ->
            <<<<"Forbidden header '"/utf8, Header@1/binary>>/binary,
                "' is present in response"/utf8>>
    end.
