-module(intent@rule).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/rule.gleam").
-export([to_string/1, parse/1]).
-export_type([rule_expr/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type rule_expr() :: {equals, binary()} |
    {equals_variable, binary()} |
    {equals_int, integer()} |
    {equals_float, float()} |
    {equals_bool, boolean()} |
    is_string |
    is_integer |
    is_number |
    is_boolean |
    is_array |
    is_object |
    is_null |
    {string_matching, binary()} |
    {string_starting_with, binary()} |
    {string_ending_with, binary()} |
    {string_containing, binary()} |
    non_empty_string |
    is_email |
    is_uuid |
    is_uri |
    is_jwt |
    is_iso8601 |
    {integer_gte, integer()} |
    {integer_gt, integer()} |
    {integer_lte, integer()} |
    {integer_lt, integer()} |
    {integer_between, integer(), integer()} |
    {number_between, float(), float()} |
    present |
    absent |
    not_null |
    non_empty_array |
    {array_of_length, integer()} |
    {array_with_min_items, integer()} |
    {array_with_max_items, integer()} |
    {array_where_each, rule_expr()} |
    valid_jwt |
    valid_iso8601 |
    {one_of, list(binary())} |
    {contains_variable, binary()} |
    {raw, binary()}.

-file("src/intent/rule.gleam", 107).
-spec try_parse_equals(binary()) -> gleam@option:option(rule_expr()).
try_parse_equals(Rule) ->
    case gleam@string:starts_with(Rule, <<"equals "/utf8>>) of
        true ->
            Value = gleam@string:drop_left(Rule, 7),
            case gleam@string:starts_with(Value, <<"${"/utf8>>) andalso gleam@string:ends_with(
                Value,
                <<"}"/utf8>>
            ) of
                true ->
                    Var_name = begin
                        _pipe = Value,
                        _pipe@1 = gleam@string:drop_left(_pipe, 2),
                        gleam@string:drop_right(_pipe@1, 1)
                    end,
                    {some, {equals_variable, Var_name}};

                false ->
                    case Value of
                        <<"true"/utf8>> ->
                            {some, {equals_bool, true}};

                        <<"false"/utf8>> ->
                            {some, {equals_bool, false}};

                        _ ->
                            case gleam@int:parse(Value) of
                                {ok, N} ->
                                    {some, {equals_int, N}};

                                {error, _} ->
                                    case gleam@float:parse(Value) of
                                        {ok, F} ->
                                            {some, {equals_float, F}};

                                        {error, _} ->
                                            {some, {equals, Value}}
                                    end
                            end
                    end
            end;

        false ->
            none
    end.

-file("src/intent/rule.gleam", 139).
-spec try_parse_type(binary()) -> gleam@option:option(rule_expr()).
try_parse_type(Rule) ->
    case Rule of
        <<"string"/utf8>> ->
            {some, is_string};

        <<"integer"/utf8>> ->
            {some, is_integer};

        <<"number"/utf8>> ->
            {some, is_number};

        <<"boolean"/utf8>> ->
            {some, is_boolean};

        <<"array"/utf8>> ->
            {some, is_array};

        <<"object"/utf8>> ->
            {some, is_object};

        <<"null"/utf8>> ->
            {some, is_null};

        _ ->
            none
    end.

-file("src/intent/rule.gleam", 152).
-spec try_parse_string_pattern(binary()) -> gleam@option:option(rule_expr()).
try_parse_string_pattern(Rule) ->
    case Rule of
        <<"non-empty string"/utf8>> ->
            {some, non_empty_string};

        <<"email"/utf8>> ->
            {some, is_email};

        <<"uuid"/utf8>> ->
            {some, is_uuid};

        <<"uri"/utf8>> ->
            {some, is_uri};

        <<"jwt"/utf8>> ->
            {some, is_jwt};

        <<"iso8601 datetime"/utf8>> ->
            {some, is_iso8601};

        _ ->
            case gleam@string:starts_with(Rule, <<"string matching "/utf8>>) of
                true ->
                    {some, {string_matching, gleam@string:drop_left(Rule, 16)}};

                false ->
                    case gleam@string:starts_with(
                        Rule,
                        <<"string starting with "/utf8>>
                    ) of
                        true ->
                            {some,
                                {string_starting_with,
                                    gleam@string:drop_left(Rule, 21)}};

                        false ->
                            case gleam@string:starts_with(
                                Rule,
                                <<"string ending with "/utf8>>
                            ) of
                                true ->
                                    {some,
                                        {string_ending_with,
                                            gleam@string:drop_left(Rule, 19)}};

                                false ->
                                    case gleam@string:starts_with(
                                        Rule,
                                        <<"string containing "/utf8>>
                                    ) of
                                        true ->
                                            {some,
                                                {string_containing,
                                                    gleam@string:drop_left(
                                                        Rule,
                                                        18
                                                    )}};

                                        false ->
                                            case gleam@string:starts_with(
                                                Rule,
                                                <<"contains ${"/utf8>>
                                            ) of
                                                true ->
                                                    Var = begin
                                                        _pipe = Rule,
                                                        _pipe@1 = gleam@string:drop_left(
                                                            _pipe,
                                                            11
                                                        ),
                                                        gleam@string:drop_right(
                                                            _pipe@1,
                                                            1
                                                        )
                                                    end,
                                                    {some,
                                                        {contains_variable, Var}};

                                                false ->
                                                    none
                                            end
                                    end
                            end
                    end
            end
    end.

-file("src/intent/rule.gleam", 250).
-spec parse_range(binary(), binary()) -> gleam@option:option({integer(),
    integer()}).
parse_range(S, Sep) ->
    case gleam@string:split(S, Sep) of
        [Low_str, High_str] ->
            case {gleam@int:parse(Low_str), gleam@int:parse(High_str)} of
                {{ok, Low}, {ok, High}} ->
                    {some, {Low, High}};

                {_, _} ->
                    none
            end;

        _ ->
            none
    end.

-file("src/intent/rule.gleam", 261).
-spec parse_float_range(binary()) -> gleam@option:option({float(), float()}).
parse_float_range(S) ->
    case gleam@string:split(S, <<" and "/utf8>>) of
        [Low_str, High_str] ->
            case {gleam@float:parse(Low_str), gleam@float:parse(High_str)} of
                {{ok, Low}, {ok, High}} ->
                    {some, {Low, High}};

                {_, _} ->
                    none
            end;

        _ ->
            none
    end.

-file("src/intent/rule.gleam", 191).
-spec try_parse_number(binary()) -> gleam@option:option(rule_expr()).
try_parse_number(Rule) ->
    case gleam@string:starts_with(Rule, <<"integer >= "/utf8>>) of
        true ->
            Num_str = gleam@string:drop_left(Rule, 11),
            case gleam@int:parse(Num_str) of
                {ok, N} ->
                    {some, {integer_gte, N}};

                {error, _} ->
                    none
            end;

        false ->
            case gleam@string:starts_with(Rule, <<"integer > "/utf8>>) of
                true ->
                    Rest = gleam@string:drop_left(Rule, 10),
                    case parse_range(Rest, <<" and < "/utf8>>) of
                        {some, {Low, High}} ->
                            {some, {integer_between, Low + 1, High - 1}};

                        none ->
                            case gleam@int:parse(Rest) of
                                {ok, N@1} ->
                                    {some, {integer_gt, N@1}};

                                {error, _} ->
                                    none
                            end
                    end;

                false ->
                    case gleam@string:starts_with(Rule, <<"integer <= "/utf8>>) of
                        true ->
                            Num_str@1 = gleam@string:drop_left(Rule, 11),
                            case gleam@int:parse(Num_str@1) of
                                {ok, N@2} ->
                                    {some, {integer_lte, N@2}};

                                {error, _} ->
                                    none
                            end;

                        false ->
                            case gleam@string:starts_with(
                                Rule,
                                <<"integer < "/utf8>>
                            ) of
                                true ->
                                    Num_str@2 = gleam@string:drop_left(Rule, 10),
                                    case gleam@int:parse(Num_str@2) of
                                        {ok, N@3} ->
                                            {some, {integer_lt, N@3}};

                                        {error, _} ->
                                            none
                                    end;

                                false ->
                                    case gleam@string:starts_with(
                                        Rule,
                                        <<"number between "/utf8>>
                                    ) of
                                        true ->
                                            Rest@1 = gleam@string:drop_left(
                                                Rule,
                                                15
                                            ),
                                            case parse_float_range(Rest@1) of
                                                {some, {Low@1, High@1}} ->
                                                    {some,
                                                        {number_between,
                                                            Low@1,
                                                            High@1}};

                                                none ->
                                                    none
                                            end;

                                        false ->
                                            none
                                    end
                            end
                    end
            end
    end.

-file("src/intent/rule.gleam", 272).
-spec try_parse_presence(binary()) -> gleam@option:option(rule_expr()).
try_parse_presence(Rule) ->
    case Rule of
        <<"present"/utf8>> ->
            {some, present};

        <<"absent"/utf8>> ->
            {some, absent};

        <<"not null"/utf8>> ->
            {some, not_null};

        _ ->
            none
    end.

-file("src/intent/rule.gleam", 361).
?DOC(" Parse a list like [\"a\", \"b\", \"c\"]\n").
-spec parse_string_list(binary()) -> {ok, list(binary())} | {error, nil}.
parse_string_list(S) ->
    S@1 = gleam@string:trim(S),
    case gleam@string:starts_with(S@1, <<"["/utf8>>) andalso gleam@string:ends_with(
        S@1,
        <<"]"/utf8>>
    ) of
        true ->
            Inner = begin
                _pipe = S@1,
                _pipe@1 = gleam@string:drop_left(_pipe, 1),
                _pipe@2 = gleam@string:drop_right(_pipe@1, 1),
                gleam@string:trim(_pipe@2)
            end,
            _pipe@3 = Inner,
            _pipe@4 = gleam@string:split(_pipe@3, <<","/utf8>>),
            _pipe@7 = gleam@list:map(
                _pipe@4,
                fun(Item) ->
                    Item@1 = gleam@string:trim(Item),
                    case gleam@string:starts_with(Item@1, <<"\""/utf8>>) andalso gleam@string:ends_with(
                        Item@1,
                        <<"\""/utf8>>
                    ) of
                        true ->
                            _pipe@5 = Item@1,
                            _pipe@6 = gleam@string:drop_left(_pipe@5, 1),
                            gleam@string:drop_right(_pipe@6, 1);

                        false ->
                            Item@1
                    end
                end
            ),
            {ok, _pipe@7};

        false ->
            {error, nil}
    end.

-file("src/intent/rule.gleam", 342).
-spec try_parse_compound(binary()) -> gleam@option:option(rule_expr()).
try_parse_compound(Rule) ->
    case Rule of
        <<"valid JWT"/utf8>> ->
            {some, valid_jwt};

        <<"valid ISO8601 datetime"/utf8>> ->
            {some, valid_iso8601};

        _ ->
            case gleam@string:starts_with(Rule, <<"one of "/utf8>>) of
                true ->
                    List_str = gleam@string:drop_left(Rule, 7),
                    case parse_string_list(List_str) of
                        {ok, Items} ->
                            {some, {one_of, Items}};

                        {error, _} ->
                            none
                    end;

                false ->
                    none
            end
    end.

-file("src/intent/rule.gleam", 391).
?DOC(" Format a rule expression back to a human-readable string\n").
-spec to_string(rule_expr()) -> binary().
to_string(Expr) ->
    case Expr of
        {equals, S} ->
            <<"equals "/utf8, S/binary>>;

        {equals_variable, V} ->
            <<<<"equals ${"/utf8, V/binary>>/binary, "}"/utf8>>;

        {equals_int, N} ->
            <<"equals "/utf8, (gleam@int:to_string(N))/binary>>;

        {equals_float, F} ->
            <<"equals "/utf8, (gleam@float:to_string(F))/binary>>;

        {equals_bool, true} ->
            <<"equals true"/utf8>>;

        {equals_bool, false} ->
            <<"equals false"/utf8>>;

        is_string ->
            <<"string"/utf8>>;

        is_integer ->
            <<"integer"/utf8>>;

        is_number ->
            <<"number"/utf8>>;

        is_boolean ->
            <<"boolean"/utf8>>;

        is_array ->
            <<"array"/utf8>>;

        is_object ->
            <<"object"/utf8>>;

        is_null ->
            <<"null"/utf8>>;

        {string_matching, P} ->
            <<"string matching "/utf8, P/binary>>;

        {string_starting_with, P@1} ->
            <<"string starting with "/utf8, P@1/binary>>;

        {string_ending_with, P@2} ->
            <<"string ending with "/utf8, P@2/binary>>;

        {string_containing, P@3} ->
            <<"string containing "/utf8, P@3/binary>>;

        non_empty_string ->
            <<"non-empty string"/utf8>>;

        is_email ->
            <<"email"/utf8>>;

        is_uuid ->
            <<"uuid"/utf8>>;

        is_uri ->
            <<"uri"/utf8>>;

        is_jwt ->
            <<"jwt"/utf8>>;

        is_iso8601 ->
            <<"iso8601 datetime"/utf8>>;

        {integer_gte, N@1} ->
            <<"integer >= "/utf8, (gleam@int:to_string(N@1))/binary>>;

        {integer_gt, N@2} ->
            <<"integer > "/utf8, (gleam@int:to_string(N@2))/binary>>;

        {integer_lte, N@3} ->
            <<"integer <= "/utf8, (gleam@int:to_string(N@3))/binary>>;

        {integer_lt, N@4} ->
            <<"integer < "/utf8, (gleam@int:to_string(N@4))/binary>>;

        {integer_between, Low, High} ->
            <<<<<<"integer > "/utf8, (gleam@int:to_string(Low - 1))/binary>>/binary,
                    " and < "/utf8>>/binary,
                (gleam@int:to_string(High + 1))/binary>>;

        {number_between, Low@1, High@1} ->
            <<<<<<"number between "/utf8,
                        (gleam@float:to_string(Low@1))/binary>>/binary,
                    " and "/utf8>>/binary,
                (gleam@float:to_string(High@1))/binary>>;

        present ->
            <<"present"/utf8>>;

        absent ->
            <<"absent"/utf8>>;

        not_null ->
            <<"not null"/utf8>>;

        non_empty_array ->
            <<"non-empty array"/utf8>>;

        {array_of_length, N@5} ->
            <<"array of length "/utf8, (gleam@int:to_string(N@5))/binary>>;

        {array_with_min_items, N@6} ->
            <<<<"array with min "/utf8, (gleam@int:to_string(N@6))/binary>>/binary,
                " items"/utf8>>;

        {array_with_max_items, N@7} ->
            <<<<"array with max "/utf8, (gleam@int:to_string(N@7))/binary>>/binary,
                " items"/utf8>>;

        {array_where_each, Inner} ->
            <<"array where each "/utf8, (to_string(Inner))/binary>>;

        valid_jwt ->
            <<"valid JWT"/utf8>>;

        valid_iso8601 ->
            <<"valid ISO8601 datetime"/utf8>>;

        {one_of, Items} ->
            <<<<"one of ["/utf8,
                    (gleam@string:join(
                        gleam@list:map(
                            Items,
                            fun(I) ->
                                <<<<"\""/utf8, I/binary>>/binary, "\""/utf8>>
                            end
                        ),
                        <<", "/utf8>>
                    ))/binary>>/binary,
                "]"/utf8>>;

        {contains_variable, V@1} ->
            <<<<"contains ${"/utf8, V@1/binary>>/binary, "}"/utf8>>;

        {raw, S@1} ->
            S@1
    end.

-file("src/intent/rule.gleam", 281).
-spec try_parse_array(binary()) -> gleam@option:option(rule_expr()).
try_parse_array(Rule) ->
    case Rule of
        <<"non-empty array"/utf8>> ->
            {some, non_empty_array};

        _ ->
            case gleam@string:starts_with(Rule, <<"array of length "/utf8>>) of
                true ->
                    Num_str = gleam@string:drop_left(Rule, 16),
                    case gleam@int:parse(Num_str) of
                        {ok, N} ->
                            {some, {array_of_length, N}};

                        {error, _} ->
                            none
                    end;

                false ->
                    case gleam@string:starts_with(
                        Rule,
                        <<"array with min "/utf8>>
                    ) of
                        true ->
                            Rest = gleam@string:drop_left(Rule, 15),
                            case gleam@string:split(Rest, <<" item"/utf8>>) of
                                [Num_str@1 | _] ->
                                    case gleam@int:parse(Num_str@1) of
                                        {ok, N@1} ->
                                            {some, {array_with_min_items, N@1}};

                                        {error, _} ->
                                            none
                                    end;

                                _ ->
                                    none
                            end;

                        false ->
                            case gleam@string:starts_with(
                                Rule,
                                <<"array with max "/utf8>>
                            ) of
                                true ->
                                    Rest@1 = gleam@string:drop_left(Rule, 15),
                                    case gleam@string:split(
                                        Rest@1,
                                        <<" item"/utf8>>
                                    ) of
                                        [Num_str@2 | _] ->
                                            case gleam@int:parse(Num_str@2) of
                                                {ok, N@2} ->
                                                    {some,
                                                        {array_with_max_items,
                                                            N@2}};

                                                {error, _} ->
                                                    none
                                            end;

                                        _ ->
                                            none
                                    end;

                                false ->
                                    case gleam@string:starts_with(
                                        Rule,
                                        <<"array where each "/utf8>>
                                    ) of
                                        true ->
                                            Inner = gleam@string:drop_left(
                                                Rule,
                                                17
                                            ),
                                            Inner_rule = case gleam@string:starts_with(
                                                Inner,
                                                <<"is "/utf8>>
                                            ) of
                                                true ->
                                                    gleam@string:drop_left(
                                                        Inner,
                                                        3
                                                    );

                                                false ->
                                                    case gleam@string:starts_with(
                                                        Inner,
                                                        <<"matches "/utf8>>
                                                    ) of
                                                        true ->
                                                            <<"string matching "/utf8,
                                                                (gleam@string:drop_left(
                                                                    Inner,
                                                                    8
                                                                ))/binary>>;

                                                        false ->
                                                            Inner
                                                    end
                                            end,
                                            {some,
                                                {array_where_each,
                                                    parse(Inner_rule)}};

                                        false ->
                                            none
                                    end
                            end
                    end
            end
    end.

-file("src/intent/rule.gleam", 73).
?DOC(" Parse a rule string into a RuleExpr\n").
-spec parse(binary()) -> rule_expr().
parse(Rule) ->
    Rule@1 = gleam@string:trim(Rule),
    case try_parse_equals(Rule@1) of
        {some, Expr} ->
            Expr;

        none ->
            case try_parse_type(Rule@1) of
                {some, Expr@1} ->
                    Expr@1;

                none ->
                    case try_parse_string_pattern(Rule@1) of
                        {some, Expr@2} ->
                            Expr@2;

                        none ->
                            case try_parse_number(Rule@1) of
                                {some, Expr@3} ->
                                    Expr@3;

                                none ->
                                    case try_parse_presence(Rule@1) of
                                        {some, Expr@4} ->
                                            Expr@4;

                                        none ->
                                            case try_parse_array(Rule@1) of
                                                {some, Expr@5} ->
                                                    Expr@5;

                                                none ->
                                                    case try_parse_compound(
                                                        Rule@1
                                                    ) of
                                                        {some, Expr@6} ->
                                                            Expr@6;

                                                        none ->
                                                            {raw, Rule@1}
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.
