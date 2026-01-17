-module(glint@flag).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/glint/flag.gleam").
-export([string/0, string_list/0, build/1, constraint/2, description/2, default/2, build_map/1, int/0, int_list/0, float/0, float_list/0, bool/0, get_int_value/1, get_int/2, get_ints_value/1, get_ints/2, get_bool_value/1, get_bool/2, get_string_value/1, get_string/2, get_strings_value/1, get_strings/2, get_float_value/1, get_float/2, get_floats_value/1, get_floats/2, update_flags/2]).
-export_type([value/0, flag_builder/1, internal/1, flag/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type value() :: {b, internal(boolean())} |
    {i, internal(integer())} |
    {l_i, internal(list(integer()))} |
    {f, internal(float())} |
    {l_f, internal(list(float()))} |
    {s, internal(binary())} |
    {l_s, internal(list(binary()))}.

-opaque flag_builder(KEV) :: {flag_builder,
        binary(),
        fun((binary()) -> {ok, KEV} | {error, snag:snag()}),
        fun((internal(KEV)) -> value()),
        gleam@option:option(KEV)}.

-opaque internal(KEW) :: {internal,
        gleam@option:option(KEW),
        fun((binary()) -> {ok, KEW} | {error, snag:snag()})}.

-type flag() :: {flag, value(), binary()}.

-file("src/glint/flag.gleam", 141).
?DOC(" initialize custom builders using a Value constructor and a parsing function\n").
-spec new(
    fun((internal(KFN)) -> value()),
    fun((binary()) -> {ok, KFN} | {error, snag:snag()})
) -> flag_builder(KFN).
new(Valuer, P) ->
    {flag_builder, <<""/utf8>>, P, Valuer, none}.

-file("src/glint/flag.gleam", 115).
?DOC(" initialise a string flag builder\n").
-spec string() -> flag_builder(binary()).
string() ->
    new(fun(Field@0) -> {s, Field@0} end, fun(S) -> {ok, S} end).

-file("src/glint/flag.gleam", 121).
?DOC(" intitialise a string list flag builder\n").
-spec string_list() -> flag_builder(list(binary())).
string_list() ->
    new(fun(Field@0) -> {l_s, Field@0} end, fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            {ok, _pipe@1} end).

-file("src/glint/flag.gleam", 147).
?DOC(" convert a FlagBuilder(a) into its corresponding Flag representation\n").
-spec build(flag_builder(any())) -> flag().
build(Fb) ->
    {flag,
        (erlang:element(4, Fb))(
            {internal, erlang:element(5, Fb), erlang:element(3, Fb)}
        ),
        erlang:element(2, Fb)}.

-file("src/glint/flag.gleam", 175).
-spec attempt(
    {ok, KGE} | {error, KGF},
    fun((KGE) -> {ok, any()} | {error, KGF})
) -> {ok, KGE} | {error, KGF}.
attempt(Val, F) ->
    gleam@result:'try'(Val, fun(A) -> gleam@result:replace(F(A), A) end).

-file("src/glint/flag.gleam", 168).
?DOC(
    " attach a Constraint(a) to a Parser(a,Snag)\n"
    " this function should not be used directly unless\n"
).
-spec wrap_with_constraint(
    fun((binary()) -> {ok, KFY} | {error, snag:snag()}),
    fun((KFY) -> {ok, nil} | {error, snag:snag()})
) -> fun((binary()) -> {ok, KFY} | {error, snag:snag()}).
wrap_with_constraint(P, Constraint) ->
    fun(Input) -> attempt(P(Input), Constraint) end.

-file("src/glint/flag.gleam", 156).
?DOC(" attach a constraint to a `Flag`\n").
-spec constraint(
    flag_builder(KFU),
    fun((KFU) -> {ok, nil} | {error, snag:snag()})
) -> flag_builder(KFU).
constraint(Builder, Constraint) ->
    {flag_builder,
        erlang:element(2, Builder),
        wrap_with_constraint(erlang:element(3, Builder), Constraint),
        erlang:element(4, Builder),
        erlang:element(5, Builder)}.

-file("src/glint/flag.gleam", 196).
?DOC(" attach a description to a `Flag`\n").
-spec description(flag_builder(KGN), binary()) -> flag_builder(KGN).
description(Builder, Description) ->
    {flag_builder,
        Description,
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder)}.

-file("src/glint/flag.gleam", 205).
?DOC(" Set the default value for a flag `Value`\n").
-spec default(flag_builder(KGQ), KGQ) -> flag_builder(KGQ).
default(Builder, Default) ->
    {flag_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        {some, Default}}.

-file("src/glint/flag.gleam", 216).
?DOC(" Convert a list of flags to a Map.\n").
-spec build_map(list({binary(), flag()})) -> gleam@dict:dict(binary(), flag()).
build_map(Flags) ->
    maps:from_list(Flags).

-file("src/glint/flag.gleam", 262).
-spec access_type_error(binary()) -> {ok, any()} | {error, snag:snag()}.
access_type_error(Flag_type) ->
    snag:error(<<"cannot access flag as "/utf8, Flag_type/binary>>).

-file("src/glint/flag.gleam", 266).
-spec flag_not_provided_error() -> {ok, any()} | {error, snag:snag()}.
flag_not_provided_error() ->
    snag:error(<<"no value provided"/utf8>>).

-file("src/glint/flag.gleam", 270).
-spec construct_value(binary(), internal(KHA), fun((internal(KHA)) -> value())) -> {ok,
        value()} |
    {error, snag:snag()}.
construct_value(Input, Internal, Constructor) ->
    gleam@result:map(
        (erlang:element(3, Internal))(Input),
        fun(Val) ->
            Constructor({internal, {some, Val}, erlang:element(3, Internal)})
        end
    ).

-file("src/glint/flag.gleam", 281).
?DOC(" Computes the new flag value given the input and the expected flag type\n").
-spec compute_flag(binary(), value()) -> {ok, value()} | {error, snag:snag()}.
compute_flag(Input, Current) ->
    _pipe = Input,
    _pipe@1 = case Current of
        {i, Internal} ->
            fun(_capture) ->
                construct_value(
                    _capture,
                    Internal,
                    fun(Field@0) -> {i, Field@0} end
                )
            end;

        {l_i, Internal@1} ->
            fun(_capture@1) ->
                construct_value(
                    _capture@1,
                    Internal@1,
                    fun(Field@0) -> {l_i, Field@0} end
                )
            end;

        {f, Internal@2} ->
            fun(_capture@2) ->
                construct_value(
                    _capture@2,
                    Internal@2,
                    fun(Field@0) -> {f, Field@0} end
                )
            end;

        {l_f, Internal@3} ->
            fun(_capture@3) ->
                construct_value(
                    _capture@3,
                    Internal@3,
                    fun(Field@0) -> {l_f, Field@0} end
                )
            end;

        {s, Internal@4} ->
            fun(_capture@4) ->
                construct_value(
                    _capture@4,
                    Internal@4,
                    fun(Field@0) -> {s, Field@0} end
                )
            end;

        {l_s, Internal@5} ->
            fun(_capture@5) ->
                construct_value(
                    _capture@5,
                    Internal@5,
                    fun(Field@0) -> {l_s, Field@0} end
                )
            end;

        {b, Internal@6} ->
            fun(_capture@6) ->
                construct_value(
                    _capture@6,
                    Internal@6,
                    fun(Field@0) -> {b, Field@0} end
                )
            end
    end(_pipe),
    snag:context(_pipe@1, <<"failed to compute value for flag"/utf8>>).

-file("src/glint/flag.gleam", 296).
-spec layer_invalid_flag(snag:snag(), binary()) -> snag:snag().
layer_invalid_flag(Err, Flag) ->
    snag:layer(Err, <<<<"invalid flag '"/utf8, Flag/binary>>/binary, "'"/utf8>>).

-file("src/glint/flag.gleam", 300).
-spec no_value_flag_err(binary()) -> snag:snag().
no_value_flag_err(Flag_input) ->
    _pipe = (<<<<"flag '"/utf8, Flag_input/binary>>/binary,
        "' has no assigned value"/utf8>>),
    _pipe@1 = snag:new(_pipe),
    layer_invalid_flag(_pipe@1, Flag_input).

-file("src/glint/flag.gleam", 306).
-spec undefined_flag_err(binary()) -> snag:snag().
undefined_flag_err(Key) ->
    _pipe = <<"flag provided but not defined"/utf8>>,
    _pipe@1 = snag:new(_pipe),
    layer_invalid_flag(_pipe@1, Key).

-file("src/glint/flag.gleam", 312).
-spec cannot_parse(binary(), binary()) -> snag:snag().
cannot_parse(Value, Kind) ->
    _pipe = (<<<<<<"cannot parse value '"/utf8, Value/binary>>/binary,
            "' as "/utf8>>/binary,
        Kind/binary>>),
    snag:new(_pipe).

-file("src/glint/flag.gleam", 77).
?DOC(" initialise an int flag builder\n").
-spec int() -> flag_builder(integer()).
int() ->
    new(fun(Field@0) -> {i, Field@0} end, fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@int:parse(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                cannot_parse(Input, <<"int"/utf8>>)
            ) end).

-file("src/glint/flag.gleam", 86).
?DOC(" initialise an int list flag builder\n").
-spec int_list() -> flag_builder(list(integer())).
int_list() ->
    new(fun(Field@0) -> {l_i, Field@0} end, fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            _pipe@2 = gleam@list:try_map(_pipe@1, fun gleam@int:parse/1),
            gleam@result:replace_error(
                _pipe@2,
                cannot_parse(Input, <<"int list"/utf8>>)
            ) end).

-file("src/glint/flag.gleam", 96).
?DOC(" initialise a float flag builder\n").
-spec float() -> flag_builder(float()).
float() ->
    new(fun(Field@0) -> {f, Field@0} end, fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@float:parse(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                cannot_parse(Input, <<"float"/utf8>>)
            ) end).

-file("src/glint/flag.gleam", 105).
?DOC(" initialise a float list flag builder\n").
-spec float_list() -> flag_builder(list(float())).
float_list() ->
    new(fun(Field@0) -> {l_f, Field@0} end, fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            _pipe@2 = gleam@list:try_map(_pipe@1, fun gleam@float:parse/1),
            gleam@result:replace_error(
                _pipe@2,
                cannot_parse(Input, <<"float list"/utf8>>)
            ) end).

-file("src/glint/flag.gleam", 130).
?DOC(" initialise a bool flag builder\n").
-spec bool() -> flag_builder(boolean()).
bool() ->
    new(
        fun(Field@0) -> {b, Field@0} end,
        fun(Input) -> case gleam@string:lowercase(Input) of
                <<"true"/utf8>> ->
                    {ok, true};

                <<"t"/utf8>> ->
                    {ok, true};

                <<"false"/utf8>> ->
                    {ok, false};

                <<"f"/utf8>> ->
                    {ok, false};

                _ ->
                    {error, cannot_parse(Input, <<"bool"/utf8>>)}
            end end
    ).

-file("src/glint/flag.gleam", 321).
?DOC(" Access the contents for the associated flag\n").
-spec access(gleam@dict:dict(binary(), flag()), binary()) -> {ok, flag()} |
    {error, snag:snag()}.
access(Flags, Name) ->
    _pipe = gleam@dict:get(Flags, Name),
    gleam@result:replace_error(_pipe, undefined_flag_err(Name)).

-file("src/glint/flag.gleam", 233).
-spec update_flag_value(gleam@dict:dict(binary(), flag()), {binary(), binary()}) -> {ok,
        gleam@dict:dict(binary(), flag())} |
    {error, snag:snag()}.
update_flag_value(Flags, Data) ->
    {Key, Input} = Data,
    gleam@result:'try'(
        access(Flags, Key),
        fun(Contents) ->
            gleam@result:map(
                begin
                    _pipe = compute_flag(Input, erlang:element(2, Contents)),
                    gleam@result:map_error(
                        _pipe,
                        fun(_capture) -> layer_invalid_flag(_capture, Key) end
                    )
                end,
                fun(Value) ->
                    gleam@dict:insert(
                        Flags,
                        Key,
                        {flag, Value, erlang:element(3, Contents)}
                    )
                end
            )
        end
    ).

-file("src/glint/flag.gleam", 243).
-spec attempt_toggle_flag(gleam@dict:dict(binary(), flag()), binary()) -> {ok,
        gleam@dict:dict(binary(), flag())} |
    {error, snag:snag()}.
attempt_toggle_flag(Flags, Key) ->
    gleam@result:'try'(
        access(Flags, Key),
        fun(Contents) -> case erlang:element(2, Contents) of
                {b, {internal, none, _} = Internal} ->
                    _pipe = {internal,
                        {some, true},
                        erlang:element(3, Internal)},
                    _pipe@1 = {b, _pipe},
                    _pipe@2 = (fun(Val) ->
                        {flag, Val, erlang:element(3, Contents)}
                    end)(_pipe@1),
                    _pipe@3 = gleam@dict:insert(Flags, Key, _pipe@2),
                    {ok, _pipe@3};

                {b, {internal, {some, Val@1}, _} = Internal@1} ->
                    _pipe@4 = {internal,
                        {some, not Val@1},
                        erlang:element(3, Internal@1)},
                    _pipe@5 = {b, _pipe@4},
                    _pipe@6 = (fun(Val@2) ->
                        {flag, Val@2, erlang:element(3, Contents)}
                    end)(_pipe@5),
                    _pipe@7 = gleam@dict:insert(Flags, Key, _pipe@6),
                    {ok, _pipe@7};

                _ ->
                    {error, no_value_flag_err(Key)}
            end end
    ).

-file("src/glint/flag.gleam", 326).
-spec get_value(
    gleam@dict:dict(binary(), flag()),
    binary(),
    fun((flag()) -> {ok, KHG} | {error, snag:snag()})
) -> {ok, KHG} | {error, snag:snag()}.
get_value(Flags, Key, Kind) ->
    _pipe = access(Flags, Key),
    _pipe@1 = gleam@result:'try'(_pipe, Kind),
    snag:context(
        _pipe@1,
        <<<<"failed to retrieve value for flag '"/utf8, Key/binary>>/binary,
            "'"/utf8>>
    ).

-file("src/glint/flag.gleam", 338).
?DOC(" Gets the current value for the provided int flag\n").
-spec get_int_value(flag()) -> {ok, integer()} | {error, snag:snag()}.
get_int_value(Flag) ->
    case erlang:element(2, Flag) of
        {i, {internal, {some, Val}, _}} ->
            {ok, Val};

        {i, {internal, none, _}} ->
            flag_not_provided_error();

        _ ->
            access_type_error(<<"int"/utf8>>)
    end.

-file("src/glint/flag.gleam", 348).
?DOC(" Gets the current value for the associated int flag\n").
-spec get_int(gleam@dict:dict(binary(), flag()), binary()) -> {ok, integer()} |
    {error, snag:snag()}.
get_int(Flags, Name) ->
    get_value(Flags, Name, fun get_int_value/1).

-file("src/glint/flag.gleam", 354).
?DOC(" Gets the current value for the provided ints flag\n").
-spec get_ints_value(flag()) -> {ok, list(integer())} | {error, snag:snag()}.
get_ints_value(Flag) ->
    case erlang:element(2, Flag) of
        {l_i, {internal, {some, Val}, _}} ->
            {ok, Val};

        {l_i, {internal, none, _}} ->
            flag_not_provided_error();

        _ ->
            access_type_error(<<"int list"/utf8>>)
    end.

-file("src/glint/flag.gleam", 364).
?DOC(" Gets the current value for the associated ints flag\n").
-spec get_ints(gleam@dict:dict(binary(), flag()), binary()) -> {ok,
        list(integer())} |
    {error, snag:snag()}.
get_ints(Flags, Name) ->
    get_value(Flags, Name, fun get_ints_value/1).

-file("src/glint/flag.gleam", 370).
?DOC(" Gets the current value for the provided bool flag\n").
-spec get_bool_value(flag()) -> {ok, boolean()} | {error, snag:snag()}.
get_bool_value(Flag) ->
    case erlang:element(2, Flag) of
        {b, {internal, {some, Val}, _}} ->
            {ok, Val};

        {b, {internal, none, _}} ->
            flag_not_provided_error();

        _ ->
            access_type_error(<<"bool"/utf8>>)
    end.

-file("src/glint/flag.gleam", 380).
?DOC(" Gets the current value for the associated bool flag\n").
-spec get_bool(gleam@dict:dict(binary(), flag()), binary()) -> {ok, boolean()} |
    {error, snag:snag()}.
get_bool(Flags, Name) ->
    get_value(Flags, Name, fun get_bool_value/1).

-file("src/glint/flag.gleam", 386).
?DOC(" Gets the current value for the provided string flag\n").
-spec get_string_value(flag()) -> {ok, binary()} | {error, snag:snag()}.
get_string_value(Flag) ->
    case erlang:element(2, Flag) of
        {s, {internal, {some, Val}, _}} ->
            {ok, Val};

        {s, {internal, none, _}} ->
            flag_not_provided_error();

        _ ->
            access_type_error(<<"string"/utf8>>)
    end.

-file("src/glint/flag.gleam", 396).
?DOC(" Gets the current value for the associated string flag\n").
-spec get_string(gleam@dict:dict(binary(), flag()), binary()) -> {ok, binary()} |
    {error, snag:snag()}.
get_string(Flags, Name) ->
    get_value(Flags, Name, fun get_string_value/1).

-file("src/glint/flag.gleam", 402).
?DOC(" Gets the current value for the provided strings flag\n").
-spec get_strings_value(flag()) -> {ok, list(binary())} | {error, snag:snag()}.
get_strings_value(Flag) ->
    case erlang:element(2, Flag) of
        {l_s, {internal, {some, Val}, _}} ->
            {ok, Val};

        {l_s, {internal, none, _}} ->
            flag_not_provided_error();

        _ ->
            access_type_error(<<"string list"/utf8>>)
    end.

-file("src/glint/flag.gleam", 412).
?DOC(" Gets the current value for the associated strings flag\n").
-spec get_strings(gleam@dict:dict(binary(), flag()), binary()) -> {ok,
        list(binary())} |
    {error, snag:snag()}.
get_strings(Flags, Name) ->
    get_value(Flags, Name, fun get_strings_value/1).

-file("src/glint/flag.gleam", 418).
?DOC(" Gets the current value for the provided float flag\n").
-spec get_float_value(flag()) -> {ok, float()} | {error, snag:snag()}.
get_float_value(Flag) ->
    case erlang:element(2, Flag) of
        {f, {internal, {some, Val}, _}} ->
            {ok, Val};

        {f, {internal, none, _}} ->
            flag_not_provided_error();

        _ ->
            access_type_error(<<"float"/utf8>>)
    end.

-file("src/glint/flag.gleam", 428).
?DOC(" Gets the current value for the associated float flag\n").
-spec get_float(gleam@dict:dict(binary(), flag()), binary()) -> {ok, float()} |
    {error, snag:snag()}.
get_float(Flags, Name) ->
    get_value(Flags, Name, fun get_float_value/1).

-file("src/glint/flag.gleam", 434).
?DOC(" Gets the current value for the provided floats flag\n").
-spec get_floats_value(flag()) -> {ok, list(float())} | {error, snag:snag()}.
get_floats_value(Flag) ->
    case erlang:element(2, Flag) of
        {l_f, {internal, {some, Val}, _}} ->
            {ok, Val};

        {l_f, {internal, none, _}} ->
            flag_not_provided_error();

        _ ->
            access_type_error(<<"float list"/utf8>>)
    end.

-file("src/glint/flag.gleam", 444).
?DOC(" Gets the current value for the associated floats flag\n").
-spec get_floats(gleam@dict:dict(binary(), flag()), binary()) -> {ok,
        list(float())} |
    {error, snag:snag()}.
get_floats(Flags, Name) ->
    get_value(Flags, Name, fun get_floats_value/1).

-file("src/glint/flag.gleam", 224).
?DOC(
    " Updates a flag value, ensuring that the new value can satisfy the required type.\n"
    " Assumes that all flag inputs passed in start with --\n"
    " This function is only intended to be used from glint.execute_root\n"
).
-spec update_flags(gleam@dict:dict(binary(), flag()), binary()) -> {ok,
        gleam@dict:dict(binary(), flag())} |
    {error, snag:snag()}.
update_flags(Flags, Flag_input) ->
    Flag_input@1 = gleam@string:drop_left(
        Flag_input,
        gleam@string:length(<<"--"/utf8>>)
    ),
    case gleam@string:split_once(Flag_input@1, <<"="/utf8>>) of
        {ok, Data} ->
            update_flag_value(Flags, Data);

        {error, _} ->
            attempt_toggle_flag(Flags, Flag_input@1)
    end.
