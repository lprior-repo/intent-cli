-module(intent@errors).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/errors.gleam").
-export([field_not_found/3, format_error/1, format_validation_error/1, extract_available_fields/1, format_format_error/4, suggest_next_steps/1]).
-export_type([contextual_error/0, validation_error/0, field_failure/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type contextual_error() :: {contextual_error,
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        list(binary()),
        list(binary()),
        binary()}.

-type validation_error() :: {validation_error, binary(), list(field_failure())}.

-type field_failure() :: {field_failure,
        binary(),
        binary(),
        binary(),
        binary(),
        binary()}.

-file("src/intent/errors.gleam", 70).
?DOC(
    " Calculate Levenshtein distance between two strings\n"
    " Used for suggesting similar field names\n"
).
-spec levenshtein_distance(binary(), binary()) -> integer().
levenshtein_distance(S1, S2) ->
    Len1 = gleam@string:length(S1),
    Len2 = gleam@string:length(S2),
    case {Len1, Len2} of
        {0, _} ->
            Len2;

        {_, 0} ->
            Len1;

        {_, _} ->
            Chars1 = gleam@string:to_graphemes(S1),
            Chars2 = gleam@string:to_graphemes(S2),
            Common = begin
                _pipe = gleam@list:filter(
                    Chars1,
                    fun(C1) -> gleam@list:contains(Chars2, C1) end
                ),
                erlang:length(_pipe)
            end,
            (Len1 + Len2) - (2 * Common)
    end.

-file("src/intent/errors.gleam", 46).
?DOC(
    " Suggest similar field names based on Levenshtein distance\n"
    " Helps users catch typos in field paths\n"
).
-spec suggest_field_names(binary(), list(binary())) -> list(binary()).
suggest_field_names(Target, Available) ->
    _pipe = Available,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(Field) -> {Field, levenshtein_distance(Target, Field)} end
    ),
    _pipe@2 = gleam@list:filter(
        _pipe@1,
        fun(Pair) ->
            {_, Dist} = Pair,
            Dist =< 2
        end
    ),
    _pipe@3 = gleam@list:sort(
        _pipe@2,
        fun(A, B) ->
            {_, Ad} = A,
            {_, Bd} = B,
            gleam@int:compare(Ad, Bd)
        end
    ),
    _pipe@4 = gleam@list:map(
        _pipe@3,
        fun(Pair@1) ->
            {Field@1, _} = Pair@1,
            Field@1
        end
    ),
    gleam@list:take(_pipe@4, 3).

-file("src/intent/errors.gleam", 26).
?DOC(" Create a contextual error with field suggestions\n").
-spec field_not_found(binary(), binary(), list(binary())) -> contextual_error().
field_not_found(Behavior, Field_path, Available_fields) ->
    Suggestions = suggest_field_names(Field_path, Available_fields),
    {contextual_error,
        Behavior,
        Field_path,
        <<"present"/utf8>>,
        <<"field to exist"/utf8>>,
        <<"field missing"/utf8>>,
        Available_fields,
        Suggestions,
        <<<<"Field '"/utf8, Field_path/binary>>/binary,
            "' not found in response"/utf8>>}.

-file("src/intent/errors.gleam", 93).
?DOC(" Format contextual error for display\n").
-spec format_error(contextual_error()) -> binary().
format_error(Error) ->
    Field_info = <<<<"Field: '"/utf8, (erlang:element(3, Error))/binary>>/binary,
        "'"/utf8>>,
    Rule_info = <<<<<<<<<<"Rule: "/utf8, (erlang:element(4, Error))/binary>>/binary,
                    "\n  Expected: "/utf8>>/binary,
                (erlang:element(5, Error))/binary>>/binary,
            "\n  Actual: "/utf8>>/binary,
        (erlang:element(6, Error))/binary>>,
    Available_info = case erlang:element(7, Error) of
        [] ->
            <<""/utf8>>;

        Fields ->
            <<"\n\nAvailable fields in response:\n  "/utf8,
                (gleam@string:join(Fields, <<"\n  "/utf8>>))/binary>>
    end,
    Suggestions_info = case erlang:element(8, Error) of
        [] ->
            <<""/utf8>>;

        Sugg ->
            <<"\n\nDid you mean:\n  "/utf8,
                (gleam@string:join(Sugg, <<"\n  "/utf8>>))/binary>>
    end,
    <<<<<<<<<<<<<<<<<<"Behavior '"/utf8, (erlang:element(2, Error))/binary>>/binary,
                                    "' validation failed:\n  "/utf8>>/binary,
                                Field_info/binary>>/binary,
                            "\n\n  "/utf8>>/binary,
                        Rule_info/binary>>/binary,
                    Available_info/binary>>/binary,
                Suggestions_info/binary>>/binary,
            "\n\n"/utf8>>/binary,
        (erlang:element(9, Error))/binary>>.

-file("src/intent/errors.gleam", 136).
?DOC(" Format validation error showing all failures at once\n").
-spec format_validation_error(validation_error()) -> binary().
format_validation_error(Error) ->
    Count = erlang:length(erlang:element(3, Error)),
    Plural = case Count of
        1 ->
            <<"failure"/utf8>>;

        _ ->
            <<"failures"/utf8>>
    end,
    Failure_lines = begin
        _pipe = erlang:element(3, Error),
        _pipe@1 = gleam@list:index_map(
            _pipe,
            fun(Failure, I) ->
                Idx = I + 1,
                <<<<<<<<<<<<<<<<<<<<<<<<"  "/utf8,
                                                                (gleam@int:to_string(
                                                                    Idx
                                                                ))/binary>>/binary,
                                                            ". Field '"/utf8>>/binary,
                                                        (erlang:element(
                                                            2,
                                                            Failure
                                                        ))/binary>>/binary,
                                                    "':\n"/utf8>>/binary,
                                                "     Rule: "/utf8>>/binary,
                                            (erlang:element(3, Failure))/binary>>/binary,
                                        "\n"/utf8>>/binary,
                                    "     Expected: "/utf8>>/binary,
                                (erlang:element(4, Failure))/binary>>/binary,
                            "\n"/utf8>>/binary,
                        "     Actual: "/utf8>>/binary,
                    (erlang:element(5, Failure))/binary>>
            end
        ),
        gleam@string:join(_pipe@1, <<"\n\n"/utf8>>)
    end,
    <<<<<<<<<<<<<<"Behavior '"/utf8, (erlang:element(2, Error))/binary>>/binary,
                            "' failed with "/utf8>>/binary,
                        (gleam@int:to_string(Count))/binary>>/binary,
                    " "/utf8>>/binary,
                Plural/binary>>/binary,
            ":\n\n"/utf8>>/binary,
        Failure_lines/binary>>.

-file("src/intent/errors.gleam", 160).
?DOC(" Extract available fields from JSON object for suggestions\n").
-spec extract_available_fields(gleam@json:json()) -> list(binary()).
extract_available_fields(Json) ->
    Json_str = gleam@json:to_string(Json),
    case gleam@json:decode(
        Json_str,
        gleam@dynamic:dict(
            fun gleam@dynamic:string/1,
            fun gleam@dynamic:dynamic/1
        )
    ) of
        {ok, Obj} ->
            _pipe = gleam@dict:keys(Obj),
            gleam@list:sort(_pipe, fun gleam@string:compare/2);

        {error, _} ->
            []
    end.

-file("src/intent/errors.gleam", 172).
?DOC(" Format error message for format validation failures\n").
-spec format_format_error(binary(), binary(), binary(), binary()) -> binary().
format_format_error(Field, Format_name, Value, Reason) ->
    <<<<<<<<<<<<<<"Field '"/utf8, Field/binary>>/binary,
                            "':\n  Expected valid "/utf8>>/binary,
                        Format_name/binary>>/binary,
                    "\n  Got: "/utf8>>/binary,
                Value/binary>>/binary,
            "\n  Problem: "/utf8>>/binary,
        Reason/binary>>.

-file("src/intent/errors.gleam", 182).
?DOC(" Suggest next validation steps based on error pattern\n").
-spec suggest_next_steps(binary()) -> list(binary()).
suggest_next_steps(Error_type) ->
    case Error_type of
        <<"format"/utf8>> ->
            [<<"Check that format validators parse correctly, not just regex match"/utf8>>,
                <<"Add test cases with edge cases (leap years, invalid dates, etc.)"/utf8>>];

        <<"missing_field"/utf8>> ->
            [<<"Verify the response structure matches the spec example"/utf8>>,
                <<"Check if the field is nested deeper than expected"/utf8>>,
                <<"Use 'array indexing' syntax if field is in an array"/utf8>>];

        <<"interpolation"/utf8>> ->
            [<<"Ensure the variable is captured in a previous behavior"/utf8>>,
                <<"Check variable name for typos"/utf8>>,
                <<"Use 'intent validate' to catch undefined variables before execution"/utf8>>];

        <<"circular_dependency"/utf8>> ->
            [<<"Review behavior 'requires' declarations for cycles"/utf8>>,
                <<"Break circular dependencies by removing one requires link"/utf8>>];

        _ ->
            [<<"Check the specification for ambiguities"/utf8>>,
                <<"Run 'intent analyze' to improve spec quality"/utf8>>]
    end.
