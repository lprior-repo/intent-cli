-module(intent@parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([dynamic_to_json/1, parse_spec/1, validate_json_safety/1]).
-export_type([json_validation_error/0]).

-type json_validation_error() :: {payload_too_large, integer(), integer()} |
    {nesting_too_deep, integer(), integer()}.

-spec count_max_nesting_depth(binary()) -> integer().
count_max_nesting_depth(Json_str) ->
    _pipe = Json_str,
    _pipe@1 = gleam@string:to_utf_codepoints(_pipe),
    _pipe@2 = gleam@list:fold(
        _pipe@1,
        {0, 0},
        fun(Acc, Codepoint) ->
            {Current, Max} = Acc,
            case gleam@string:utf_codepoint_to_int(Codepoint) of
                123 ->
                    {Current + 1, gleam@int:max(Max, Current + 1)};

                91 ->
                    {Current + 1, gleam@int:max(Max, Current + 1)};

                125 ->
                    {gleam@int:max(Current - 1, 0), Max};

                93 ->
                    {gleam@int:max(Current - 1, 0), Max};

                _ ->
                    Acc
            end
        end
    ),
    gleam@pair:second(_pipe@2).

-spec parse_string_dict(gleam@dynamic:dynamic_()) -> {ok,
        gleam@dict:dict(binary(), binary())} |
    {error, list(gleam@dynamic:decode_error())}.
parse_string_dict(Data) ->
    (gleam@dynamic:dict(fun gleam@dynamic:string/1, fun gleam@dynamic:string/1))(
        Data
    ).

-spec parse_config(gleam@dynamic:dynamic_()) -> {ok, intent@types:config()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_config(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"base_url"/utf8>>, fun gleam@dynamic:string/1))(
            Data
        ),
        fun(Base_url) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"timeout_ms"/utf8>>,
                    fun gleam@dynamic:int/1
                ))(Data),
                fun(Timeout_ms) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"headers"/utf8>>,
                            fun parse_string_dict/1
                        ))(Data),
                        fun(Headers) ->
                            {ok, {config, Base_url, Timeout_ms, Headers}}
                        end
                    )
                end
            )
        end
    ).

-spec parse_method(gleam@dynamic:dynamic_()) -> {ok, intent@types:method()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_method(Data) ->
    _pipe = Data,
    _pipe@1 = gleam@dynamic:string(_pipe),
    gleam@result:then(_pipe@1, fun(S) -> case S of
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
                    {error, [{decode_error, <<"HTTP method"/utf8>>, S, []}]}
            end end).

-spec dynamic_to_json(gleam@dynamic:dynamic_()) -> gleam@json:json().
dynamic_to_json(Data) ->
    case gleam@dynamic:classify(Data) of
        <<"Nil"/utf8>> ->
            gleam@json:null();

        <<"Bool"/utf8>> ->
            case gleam@dynamic:bool(Data) of
                {ok, B} ->
                    gleam@json:bool(B);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"Int"/utf8>> ->
            case gleam@dynamic:int(Data) of
                {ok, I} ->
                    gleam@json:int(I);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"Float"/utf8>> ->
            case gleam@dynamic:float(Data) of
                {ok, F} ->
                    gleam@json:float(F);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"String"/utf8>> ->
            case gleam@dynamic:string(Data) of
                {ok, S} ->
                    gleam@json:string(S);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"BitArray"/utf8>> ->
            case gleam@dynamic:string(Data) of
                {ok, S} ->
                    gleam@json:string(S);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"List"/utf8>> ->
            case (gleam@dynamic:list(fun gleam@dynamic:dynamic/1))(Data) of
                {ok, Items} ->
                    gleam@json:array(Items, fun dynamic_to_json/1);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"Tuple"/utf8>> ->
            case (gleam@dynamic:list(fun gleam@dynamic:dynamic/1))(Data) of
                {ok, Items} ->
                    gleam@json:array(Items, fun dynamic_to_json/1);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"Dict"/utf8>> ->
            case (gleam@dynamic:dict(
                fun gleam@dynamic:string/1,
                fun gleam@dynamic:dynamic/1
            ))(Data) of
                {ok, D} ->
                    _pipe = D,
                    _pipe@1 = maps:to_list(_pipe),
                    _pipe@2 = gleam@list:map(
                        _pipe@1,
                        fun(Pair) ->
                            {erlang:element(1, Pair),
                                dynamic_to_json(erlang:element(2, Pair))}
                        end
                    ),
                    gleam@json:object(_pipe@2);

                {error, _} ->
                    gleam@json:null()
            end;

        <<"Map"/utf8>> ->
            case (gleam@dynamic:dict(
                fun gleam@dynamic:string/1,
                fun gleam@dynamic:dynamic/1
            ))(Data) of
                {ok, D} ->
                    _pipe = D,
                    _pipe@1 = maps:to_list(_pipe),
                    _pipe@2 = gleam@list:map(
                        _pipe@1,
                        fun(Pair) ->
                            {erlang:element(1, Pair),
                                dynamic_to_json(erlang:element(2, Pair))}
                        end
                    ),
                    gleam@json:object(_pipe@2);

                {error, _} ->
                    gleam@json:null()
            end;

        _ ->
            gleam@json:null()
    end.

-spec parse_json_dict(gleam@dynamic:dynamic_()) -> {ok,
        gleam@dict:dict(binary(), gleam@json:json())} |
    {error, list(gleam@dynamic:decode_error())}.
parse_json_dict(Data) ->
    _pipe = Data,
    _pipe@1 = (gleam@dynamic:dict(
        fun gleam@dynamic:string/1,
        fun gleam@dynamic:dynamic/1
    ))(_pipe),
    gleam@result:map(
        _pipe@1,
        fun(D) ->
            gleam@dict:map_values(D, fun(_, V) -> dynamic_to_json(V) end)
        end
    ).

-spec parse_json_value(gleam@dynamic:dynamic_()) -> {ok, gleam@json:json()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_json_value(Data) ->
    {ok, dynamic_to_json(Data)}.

-spec parse_request(gleam@dynamic:dynamic_()) -> {ok, intent@types:request()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_request(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"method"/utf8>>, fun parse_method/1))(Data),
        fun(Method) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"path"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Data),
                fun(Path) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"headers"/utf8>>,
                            fun parse_string_dict/1
                        ))(Data),
                        fun(Headers) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"query"/utf8>>,
                                    fun parse_json_dict/1
                                ))(Data),
                                fun(Query) ->
                                    gleam@result:'try'(
                                        (gleam@dynamic:field(
                                            <<"body"/utf8>>,
                                            fun parse_json_value/1
                                        ))(Data),
                                        fun(Body) ->
                                            {ok,
                                                {request,
                                                    Method,
                                                    Path,
                                                    Headers,
                                                    Query,
                                                    Body}}
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

-spec parse_check(gleam@dynamic:dynamic_()) -> {ok, intent@types:check()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_check(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"rule"/utf8>>, fun gleam@dynamic:string/1))(Data),
        fun(Rule) ->
            gleam@result:'try'(
                (gleam@dynamic:field(<<"why"/utf8>>, fun gleam@dynamic:string/1))(
                    Data
                ),
                fun(Why) -> {ok, {check, Rule, Why}} end
            )
        end
    ).

-spec parse_checks(gleam@dynamic:dynamic_()) -> {ok,
        gleam@dict:dict(binary(), intent@types:check())} |
    {error, list(gleam@dynamic:decode_error())}.
parse_checks(Data) ->
    _pipe = Data,
    (gleam@dynamic:dict(fun gleam@dynamic:string/1, fun parse_check/1))(_pipe).

-spec parse_response(gleam@dynamic:dynamic_()) -> {ok, intent@types:response()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_response(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"status"/utf8>>, fun gleam@dynamic:int/1))(Data),
        fun(Status) ->
            gleam@result:'try'(
                (gleam@dynamic:field(<<"example"/utf8>>, fun parse_json_value/1))(
                    Data
                ),
                fun(Example) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"checks"/utf8>>,
                            fun parse_checks/1
                        ))(Data),
                        fun(Checks) ->
                            Headers = begin
                                _pipe = (gleam@dynamic:field(
                                    <<"headers"/utf8>>,
                                    fun parse_string_dict/1
                                ))(Data),
                                gleam@result:unwrap(_pipe, gleam@dict:new())
                            end,
                            {ok, {response, Status, Example, Checks, Headers}}
                        end
                    )
                end
            )
        end
    ).

-spec parse_behavior(gleam@dynamic:dynamic_()) -> {ok, intent@types:behavior()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_behavior(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1))(Data),
        fun(Name) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"intent"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Data),
                fun(Intent) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"notes"/utf8>>,
                            fun gleam@dynamic:string/1
                        ))(Data),
                        fun(Notes) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"requires"/utf8>>,
                                    gleam@dynamic:list(
                                        fun gleam@dynamic:string/1
                                    )
                                ))(Data),
                                fun(Requires) ->
                                    gleam@result:'try'(
                                        (gleam@dynamic:field(
                                            <<"tags"/utf8>>,
                                            gleam@dynamic:list(
                                                fun gleam@dynamic:string/1
                                            )
                                        ))(Data),
                                        fun(Tags) ->
                                            gleam@result:'try'(
                                                (gleam@dynamic:field(
                                                    <<"request"/utf8>>,
                                                    fun parse_request/1
                                                ))(Data),
                                                fun(Request) ->
                                                    gleam@result:'try'(
                                                        (gleam@dynamic:field(
                                                            <<"response"/utf8>>,
                                                            fun parse_response/1
                                                        ))(Data),
                                                        fun(Response) ->
                                                            gleam@result:'try'(
                                                                (gleam@dynamic:field(
                                                                    <<"captures"/utf8>>,
                                                                    fun parse_string_dict/1
                                                                ))(Data),
                                                                fun(Captures) ->
                                                                    {ok,
                                                                        {behavior,
                                                                            Name,
                                                                            Intent,
                                                                            Notes,
                                                                            Requires,
                                                                            Tags,
                                                                            Request,
                                                                            Response,
                                                                            Captures}}
                                                                end
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
                    )
                end
            )
        end
    ).

-spec parse_feature(gleam@dynamic:dynamic_()) -> {ok, intent@types:feature()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_feature(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1))(Data),
        fun(Name) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"description"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Data),
                fun(Description) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"behaviors"/utf8>>,
                            gleam@dynamic:list(fun parse_behavior/1)
                        ))(Data),
                        fun(Behaviors) ->
                            {ok, {feature, Name, Description, Behaviors}}
                        end
                    )
                end
            )
        end
    ).

-spec parse_when(gleam@dynamic:dynamic_()) -> {ok, intent@types:'when'()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_when(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"status"/utf8>>, fun gleam@dynamic:string/1))(
            Data
        ),
        fun(Status) ->
            Method = case (gleam@dynamic:field(
                <<"method"/utf8>>,
                fun parse_method/1
            ))(Data) of
                {ok, M} ->
                    {some, M};

                {error, _} ->
                    none
            end,
            Path = case (gleam@dynamic:field(
                <<"path"/utf8>>,
                fun gleam@dynamic:string/1
            ))(Data) of
                {ok, P} ->
                    {some, P};

                {error, _} ->
                    none
            end,
            {ok, {'when', Status, Method, Path}}
        end
    ).

-spec parse_rule_check(gleam@dynamic:dynamic_()) -> {ok,
        intent@types:rule_check()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_rule_check(Data) ->
    Body_must_not_contain = case (gleam@dynamic:field(
        <<"body_must_not_contain"/utf8>>,
        gleam@dynamic:list(fun gleam@dynamic:string/1)
    ))(Data) of
        {ok, V} ->
            V;

        {error, _} ->
            []
    end,
    Body_must_contain = case (gleam@dynamic:field(
        <<"body_must_contain"/utf8>>,
        gleam@dynamic:list(fun gleam@dynamic:string/1)
    ))(Data) of
        {ok, V@1} ->
            V@1;

        {error, _} ->
            []
    end,
    Fields_must_exist = case (gleam@dynamic:field(
        <<"fields_must_exist"/utf8>>,
        gleam@dynamic:list(fun gleam@dynamic:string/1)
    ))(Data) of
        {ok, V@2} ->
            V@2;

        {error, _} ->
            []
    end,
    Fields_must_not_exist = case (gleam@dynamic:field(
        <<"fields_must_not_exist"/utf8>>,
        gleam@dynamic:list(fun gleam@dynamic:string/1)
    ))(Data) of
        {ok, V@3} ->
            V@3;

        {error, _} ->
            []
    end,
    Header_must_exist = case (gleam@dynamic:field(
        <<"header_must_exist"/utf8>>,
        fun gleam@dynamic:string/1
    ))(Data) of
        {ok, V@4} ->
            V@4;

        {error, _} ->
            <<""/utf8>>
    end,
    Header_must_not_exist = case (gleam@dynamic:field(
        <<"header_must_not_exist"/utf8>>,
        fun gleam@dynamic:string/1
    ))(Data) of
        {ok, V@5} ->
            V@5;

        {error, _} ->
            <<""/utf8>>
    end,
    {ok,
        {rule_check,
            Body_must_not_contain,
            Body_must_contain,
            Fields_must_exist,
            Fields_must_not_exist,
            Header_must_exist,
            Header_must_not_exist}}.

-spec parse_rule(gleam@dynamic:dynamic_()) -> {ok, intent@types:rule()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_rule(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1))(Data),
        fun(Name) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"description"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Data),
                fun(Description) ->
                    When = case (gleam@dynamic:field(
                        <<"when"/utf8>>,
                        fun parse_when/1
                    ))(Data) of
                        {ok, W} ->
                            {some, W};

                        {error, _} ->
                            none
                    end,
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"check"/utf8>>,
                            fun parse_rule_check/1
                        ))(Data),
                        fun(Check) ->
                            Example = case (gleam@dynamic:field(
                                <<"example"/utf8>>,
                                fun parse_json_value/1
                            ))(Data) of
                                {ok, E} ->
                                    E;

                                {error, _} ->
                                    gleam@json:null()
                            end,
                            {ok,
                                {rule, Name, Description, When, Check, Example}}
                        end
                    )
                end
            )
        end
    ).

-spec parse_anti_pattern(gleam@dynamic:dynamic_()) -> {ok,
        intent@types:anti_pattern()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_anti_pattern(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1))(Data),
        fun(Name) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"description"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Data),
                fun(Description) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"bad_example"/utf8>>,
                            fun parse_json_value/1
                        ))(Data),
                        fun(Bad_example) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"good_example"/utf8>>,
                                    fun parse_json_value/1
                                ))(Data),
                                fun(Good_example) ->
                                    gleam@result:'try'(
                                        (gleam@dynamic:field(
                                            <<"why"/utf8>>,
                                            fun gleam@dynamic:string/1
                                        ))(Data),
                                        fun(Why) ->
                                            {ok,
                                                {anti_pattern,
                                                    Name,
                                                    Description,
                                                    Bad_example,
                                                    Good_example,
                                                    Why}}
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

-spec parse_implementation_hints(gleam@dynamic:dynamic_()) -> {ok,
        intent@types:implementation_hints()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_implementation_hints(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(
            <<"suggested_stack"/utf8>>,
            gleam@dynamic:list(fun gleam@dynamic:string/1)
        ))(Data),
        fun(Suggested_stack) ->
            {ok, {implementation_hints, Suggested_stack}}
        end
    ).

-spec parse_entity_hint(gleam@dynamic:dynamic_()) -> {ok,
        intent@types:entity_hint()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_entity_hint(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"fields"/utf8>>, fun parse_string_dict/1))(Data),
        fun(Fields) -> {ok, {entity_hint, Fields}} end
    ).

-spec parse_entities(gleam@dynamic:dynamic_()) -> {ok,
        gleam@dict:dict(binary(), intent@types:entity_hint())} |
    {error, list(gleam@dynamic:decode_error())}.
parse_entities(Data) ->
    (gleam@dynamic:dict(fun gleam@dynamic:string/1, fun parse_entity_hint/1))(
        Data
    ).

-spec parse_security_hints(gleam@dynamic:dynamic_()) -> {ok,
        intent@types:security_hints()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_security_hints(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(
            <<"password_hashing"/utf8>>,
            fun gleam@dynamic:string/1
        ))(Data),
        fun(Password_hashing) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"jwt_algorithm"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Data),
                fun(Jwt_algorithm) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"jwt_expiry"/utf8>>,
                            fun gleam@dynamic:string/1
                        ))(Data),
                        fun(Jwt_expiry) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"rate_limiting"/utf8>>,
                                    fun gleam@dynamic:string/1
                                ))(Data),
                                fun(Rate_limiting) ->
                                    {ok,
                                        {security_hints,
                                            Password_hashing,
                                            Jwt_algorithm,
                                            Jwt_expiry,
                                            Rate_limiting}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-spec parse_ai_hints(gleam@dynamic:dynamic_()) -> {ok, intent@types:ai_hints()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_ai_hints(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(
            <<"implementation"/utf8>>,
            fun parse_implementation_hints/1
        ))(Data),
        fun(Implementation) ->
            gleam@result:'try'(
                (gleam@dynamic:field(<<"entities"/utf8>>, fun parse_entities/1))(
                    Data
                ),
                fun(Entities) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"security"/utf8>>,
                            fun parse_security_hints/1
                        ))(Data),
                        fun(Security) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"pitfalls"/utf8>>,
                                    gleam@dynamic:list(
                                        fun gleam@dynamic:string/1
                                    )
                                ))(Data),
                                fun(Pitfalls) ->
                                    {ok,
                                        {ai_hints,
                                            Implementation,
                                            Entities,
                                            Security,
                                            Pitfalls}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-spec parse_spec(gleam@dynamic:dynamic_()) -> {ok, intent@types:spec()} |
    {error, list(gleam@dynamic:decode_error())}.
parse_spec(Data) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1))(Data),
        fun(Name) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"description"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Data),
                fun(Description) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"audience"/utf8>>,
                            fun gleam@dynamic:string/1
                        ))(Data),
                        fun(Audience) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"version"/utf8>>,
                                    fun gleam@dynamic:string/1
                                ))(Data),
                                fun(Version) ->
                                    gleam@result:'try'(
                                        (gleam@dynamic:field(
                                            <<"success_criteria"/utf8>>,
                                            gleam@dynamic:list(
                                                fun gleam@dynamic:string/1
                                            )
                                        ))(Data),
                                        fun(Success_criteria) ->
                                            gleam@result:'try'(
                                                (gleam@dynamic:field(
                                                    <<"config"/utf8>>,
                                                    fun parse_config/1
                                                ))(Data),
                                                fun(Config) ->
                                                    gleam@result:'try'(
                                                        (gleam@dynamic:field(
                                                            <<"features"/utf8>>,
                                                            gleam@dynamic:list(
                                                                fun parse_feature/1
                                                            )
                                                        ))(Data),
                                                        fun(Features) ->
                                                            gleam@result:'try'(
                                                                (gleam@dynamic:field(
                                                                    <<"rules"/utf8>>,
                                                                    gleam@dynamic:list(
                                                                        fun parse_rule/1
                                                                    )
                                                                ))(Data),
                                                                fun(Rules) ->
                                                                    gleam@result:'try'(
                                                                        (gleam@dynamic:field(
                                                                            <<"anti_patterns"/utf8>>,
                                                                            gleam@dynamic:list(
                                                                                fun parse_anti_pattern/1
                                                                            )
                                                                        ))(Data),
                                                                        fun(
                                                                            Anti_patterns
                                                                        ) ->
                                                                            gleam@result:'try'(
                                                                                (gleam@dynamic:field(
                                                                                    <<"ai_hints"/utf8>>,
                                                                                    fun parse_ai_hints/1
                                                                                ))(
                                                                                    Data
                                                                                ),
                                                                                fun(
                                                                                    Ai_hints
                                                                                ) ->
                                                                                    {ok,
                                                                                        {spec,
                                                                                            Name,
                                                                                            Description,
                                                                                            Audience,
                                                                                            Version,
                                                                                            Success_criteria,
                                                                                            Config,
                                                                                            Features,
                                                                                            Rules,
                                                                                            Anti_patterns,
                                                                                            Ai_hints}}
                                                                                end
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
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-spec validate_json_safety(binary()) -> {ok, nil} |
    {error, json_validation_error()}.
validate_json_safety(Json_str) ->
    Size = erlang:byte_size(Json_str),
    case Size > 10485760 of
        true ->
            {error, {payload_too_large, Size, 10485760}};

        false ->
            Depth = count_max_nesting_depth(Json_str),
            case Depth > 1000 of
                true ->
                    {error, {nesting_too_deep, Depth, 1000}};

                false ->
                    {ok, nil}
            end
    end.
