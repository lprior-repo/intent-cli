-module(glint@flag@constraint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/glint/flag/constraint.gleam").
-export([one_of/1, none_of/1, each/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/glint/flag/constraint.gleam", 15).
?DOC(
    " one_of returns a Constraint that ensures the parsed flag value is\n"
    " one of the allowed values.\n"
).
-spec one_of(list(IIB)) -> fun((IIB) -> {ok, nil} | {error, snag:snag()}).
one_of(Allowed) ->
    Allowed_set = gleam@set:from_list(Allowed),
    fun(Val) -> case gleam@set:contains(Allowed_set, Val) of
            true ->
                {ok, nil};

            false ->
                snag:error(
                    <<<<<<<<"invalid value '"/utf8,
                                    (gleam@string:inspect(Val))/binary>>/binary,
                                "', must be one of: ["/utf8>>/binary,
                            (begin
                                _pipe = Allowed,
                                _pipe@1 = gleam@list:map(
                                    _pipe,
                                    fun(A) ->
                                        <<<<"'"/utf8,
                                                (gleam@string:inspect(A))/binary>>/binary,
                                            "'"/utf8>>
                                    end
                                ),
                                gleam@string:join(_pipe@1, <<", "/utf8>>)
                            end)/binary>>/binary,
                        "]"/utf8>>
                )
        end end.

-file("src/glint/flag/constraint.gleam", 38).
?DOC(" none_of returns a Constraint that ensures the parsed flag value is not one of the disallowed values.\n").
-spec none_of(list(IIE)) -> fun((IIE) -> {ok, nil} | {error, snag:snag()}).
none_of(Disallowed) ->
    Disallowed_set = gleam@set:from_list(Disallowed),
    fun(Val) -> case gleam@set:contains(Disallowed_set, Val) of
            false ->
                {ok, nil};

            true ->
                snag:error(
                    <<<<<<"invalid value '"/utf8,
                                (gleam@string:inspect(Val))/binary>>/binary,
                            "', must not be one of: ["/utf8>>/binary,
                        (((<<(begin
                                _pipe = Disallowed,
                                _pipe@1 = gleam@list:map(
                                    _pipe,
                                    fun(A) ->
                                        <<<<"'"/utf8,
                                                (gleam@string:inspect(A))/binary>>/binary,
                                            "'"/utf8>>
                                    end
                                ),
                                gleam@string:join(_pipe@1, <<", "/utf8>>)
                            end)/binary,
                            "]"/utf8>>)))/binary>>
                )
        end end.

-file("src/glint/flag/constraint.gleam", 68).
?DOC(
    " each is a convenience function for applying a Constraint(a) to a List(a).\n"
    " This is useful because the default behaviour for constraints on lists is that they will apply to the list as a whole.\n"
    " \n"
    " For example, to apply one_of to all items in a `List(Int)`:\n"
    " ```gleam\n"
    " [1, 2, 3, 4] |> one_of |> each\n"
    " ```\n"
).
-spec each(fun((IIH) -> {ok, nil} | {error, snag:snag()})) -> fun((list(IIH)) -> {ok,
        nil} |
    {error, snag:snag()}).
each(Constraint) ->
    fun(L) -> _pipe = L,
        _pipe@1 = gleam@list:try_map(_pipe, Constraint),
        gleam@result:replace(_pipe@1, nil) end.
