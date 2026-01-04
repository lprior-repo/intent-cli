-module(intent@pattern_library).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/pattern_library.gleam").
-export([new_library/0, analyze_and_add_spec/2, search_patterns/2, get_patterns_by_category/2, get_top_patterns/2, library_stats/1, format_patterns/1, suggest_patterns_for_spec/2]).
-export_type([pattern/0, pattern_library/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type pattern() :: {pattern,
        binary(),
        binary(),
        binary(),
        integer(),
        list(intent@types:behavior())}.

-type pattern_library() :: {pattern_library,
        list(pattern()),
        integer(),
        list(binary())}.

-file("src/intent/pattern_library.gleam", 31).
?DOC(" Create an empty pattern library\n").
-spec new_library() -> pattern_library().
new_library() ->
    {pattern_library, [], 0, []}.

-file("src/intent/pattern_library.gleam", 70).
?DOC(" Extract error handling patterns\n").
-spec append_error_handling_patterns(
    list(pattern()),
    list(intent@types:behavior())
) -> list(pattern()).
append_error_handling_patterns(Patterns, Behaviors) ->
    Error_behaviors = begin
        _pipe = Behaviors,
        gleam@list:filter(
            _pipe,
            fun(B) -> erlang:element(2, erlang:element(8, B)) >= 400 end
        )
    end,
    case erlang:length(Error_behaviors) of
        0 ->
            Patterns;

        Count ->
            lists:append(
                Patterns,
                [{pattern,
                        <<"Error Handling"/utf8>>,
                        <<"API returns appropriate error status codes and messages"/utf8>>,
                        <<"error-handling"/utf8>>,
                        Count,
                        Error_behaviors}]
            )
    end.

-file("src/intent/pattern_library.gleam", 98).
?DOC(" Extract validation patterns\n").
-spec append_validation_patterns(list(pattern()), list(intent@types:behavior())) -> list(pattern()).
append_validation_patterns(Patterns, Behaviors) ->
    Behaviors_with_checks = begin
        _pipe = Behaviors,
        gleam@list:filter(
            _pipe,
            fun(B) ->
                not gleam@dict:is_empty(erlang:element(4, erlang:element(8, B)))
            end
        )
    end,
    case erlang:length(Behaviors_with_checks) of
        0 ->
            Patterns;

        Count ->
            lists:append(
                Patterns,
                [{pattern,
                        <<"Response Validation"/utf8>>,
                        <<"Validates response fields against specific formats and rules"/utf8>>,
                        <<"validation"/utf8>>,
                        Count,
                        Behaviors_with_checks}]
            )
    end.

-file("src/intent/pattern_library.gleam", 126).
?DOC(" Extract authentication patterns\n").
-spec append_authentication_patterns(
    list(pattern()),
    list(intent@types:behavior())
) -> list(pattern()).
append_authentication_patterns(Patterns, Behaviors) ->
    Auth_behaviors = begin
        _pipe = Behaviors,
        gleam@list:filter(
            _pipe,
            fun(B) ->
                gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(2, B)),
                    <<"auth"/utf8>>
                )
                orelse gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(3, B)),
                    <<"auth"/utf8>>
                )
            end
        )
    end,
    case erlang:length(Auth_behaviors) of
        0 ->
            Patterns;

        Count ->
            lists:append(
                Patterns,
                [{pattern,
                        <<"Authentication"/utf8>>,
                        <<"Tests authentication and authorization scenarios"/utf8>>,
                        <<"authentication"/utf8>>,
                        Count,
                        Auth_behaviors}]
            )
    end.

-file("src/intent/pattern_library.gleam", 157).
?DOC(" Extract workflow/sequencing patterns\n").
-spec append_workflow_patterns(list(pattern()), list(intent@types:behavior())) -> list(pattern()).
append_workflow_patterns(Patterns, Behaviors) ->
    Workflow_behaviors = begin
        _pipe = Behaviors,
        gleam@list:filter(
            _pipe,
            fun(B) -> not gleam@list:is_empty(erlang:element(5, B)) end
        )
    end,
    case erlang:length(Workflow_behaviors) of
        0 ->
            Patterns;

        Count ->
            lists:append(
                Patterns,
                [{pattern,
                        <<"Workflow Sequencing"/utf8>>,
                        <<"Tests behaviors with dependencies and ordering constraints"/utf8>>,
                        <<"workflow"/utf8>>,
                        Count,
                        Workflow_behaviors}]
            )
    end.

-file("src/intent/pattern_library.gleam", 61).
?DOC(" Extract patterns from a list of behaviors\n").
-spec extract_patterns_from_behaviors(list(intent@types:behavior())) -> list(pattern()).
extract_patterns_from_behaviors(Behaviors) ->
    _pipe = [],
    _pipe@1 = append_error_handling_patterns(_pipe, Behaviors),
    _pipe@2 = append_validation_patterns(_pipe@1, Behaviors),
    _pipe@3 = append_authentication_patterns(_pipe@2, Behaviors),
    append_workflow_patterns(_pipe@3, Behaviors).

-file("src/intent/pattern_library.gleam", 185).
?DOC(" Merge newly extracted patterns with existing ones, updating frequency\n").
-spec merge_patterns(list(pattern()), list(pattern())) -> list(pattern()).
merge_patterns(Existing, New_patterns) ->
    _pipe = New_patterns,
    gleam@list:fold(
        _pipe,
        Existing,
        fun(Acc, New_pattern) ->
            case begin
                _pipe@1 = Acc,
                gleam@list:find(
                    _pipe@1,
                    fun(P) ->
                        erlang:element(2, P) =:= erlang:element(2, New_pattern)
                    end
                )
            end of
                {error, _} ->
                    lists:append(Acc, [New_pattern]);

                {ok, Found} ->
                    Updated = {pattern,
                        erlang:element(2, Found),
                        erlang:element(3, Found),
                        erlang:element(4, Found),
                        erlang:element(5, Found) + erlang:element(
                            5,
                            New_pattern
                        ),
                        lists:append(
                            erlang:element(6, Found),
                            erlang:element(6, New_pattern)
                        )},
                    _pipe@2 = Acc,
                    _pipe@3 = gleam@list:filter(
                        _pipe@2,
                        fun(P@1) ->
                            erlang:element(2, P@1) /= erlang:element(2, Found)
                        end
                    ),
                    lists:append(_pipe@3, [Updated])
            end
        end
    ).

-file("src/intent/pattern_library.gleam", 36).
?DOC(" Extract patterns from a single spec and add to library\n").
-spec analyze_and_add_spec(pattern_library(), intent@types:spec()) -> pattern_library().
analyze_and_add_spec(Library, Spec) ->
    Behaviors = begin
        _pipe = erlang:element(8, Spec),
        gleam@list:flat_map(_pipe, fun(F) -> erlang:element(4, F) end)
    end,
    Extracted_patterns = extract_patterns_from_behaviors(Behaviors),
    Updated_patterns = merge_patterns(
        erlang:element(2, Library),
        Extracted_patterns
    ),
    Categories = begin
        _pipe@1 = Updated_patterns,
        _pipe@2 = gleam@list:map(_pipe@1, fun(P) -> erlang:element(4, P) end),
        gleam@list:unique(_pipe@2)
    end,
    {pattern_library,
        Updated_patterns,
        erlang:element(3, Library) + 1,
        Categories}.

-file("src/intent/pattern_library.gleam", 212).
?DOC(" Find patterns matching a query string\n").
-spec search_patterns(pattern_library(), binary()) -> list(pattern()).
search_patterns(Library, Query) ->
    Query_lower = gleam@string:lowercase(Query),
    _pipe = erlang:element(2, Library),
    gleam@list:filter(
        _pipe,
        fun(P) ->
            (gleam_stdlib:contains_string(
                gleam@string:lowercase(erlang:element(2, P)),
                Query_lower
            )
            orelse gleam_stdlib:contains_string(
                gleam@string:lowercase(erlang:element(3, P)),
                Query_lower
            ))
            orelse gleam_stdlib:contains_string(
                gleam@string:lowercase(erlang:element(4, P)),
                Query_lower
            )
        end
    ).

-file("src/intent/pattern_library.gleam", 226).
?DOC(" Get patterns by category\n").
-spec get_patterns_by_category(pattern_library(), binary()) -> list(pattern()).
get_patterns_by_category(Library, Category) ->
    _pipe = erlang:element(2, Library),
    gleam@list:filter(_pipe, fun(P) -> erlang:element(4, P) =:= Category end).

-file("src/intent/pattern_library.gleam", 235).
?DOC(" Get most frequent patterns\n").
-spec get_top_patterns(pattern_library(), integer()) -> list(pattern()).
get_top_patterns(Library, Limit) ->
    _pipe = erlang:element(2, Library),
    _pipe@1 = gleam@list:sort(
        _pipe,
        fun(A, B) ->
            gleam@int:compare(erlang:element(5, B), erlang:element(5, A))
        end
    ),
    gleam@list:take(_pipe@1, Limit).

-file("src/intent/pattern_library.gleam", 245).
?DOC(" Get pattern library statistics\n").
-spec library_stats(pattern_library()) -> binary().
library_stats(Library) ->
    Pattern_count = erlang:length(erlang:element(2, Library)),
    Total_behaviors = begin
        _pipe = erlang:element(2, Library),
        _pipe@1 = gleam@list:flat_map(_pipe, fun(P) -> erlang:element(6, P) end),
        erlang:length(_pipe@1)
    end,
    <<<<<<<<<<<<<<<<"Pattern Library Statistics:
  Total patterns: "/utf8,
                                    (gleam@int:to_string(Pattern_count))/binary>>/binary,
                                "
  Categories: "/utf8>>/binary,
                            (gleam@int:to_string(
                                erlang:length(erlang:element(4, Library))
                            ))/binary>>/binary,
                        "
  Specs analyzed: "/utf8>>/binary,
                    (gleam@int:to_string(erlang:element(3, Library)))/binary>>/binary,
                "
  Total behaviors indexed: "/utf8>>/binary,
            (gleam@int:to_string(Total_behaviors))/binary>>/binary,
        "
"/utf8>>.

-file("src/intent/pattern_library.gleam", 280).
?DOC(" Format a single pattern\n").
-spec format_pattern(pattern()) -> binary().
format_pattern(Pattern) ->
    <<<<<<<<<<<<<<<<"Pattern: "/utf8, (erlang:element(2, Pattern))/binary>>/binary,
                                "
Category: "/utf8>>/binary,
                            (erlang:element(4, Pattern))/binary>>/binary,
                        "
Description: "/utf8>>/binary,
                    (erlang:element(3, Pattern))/binary>>/binary,
                "
Found in: "/utf8>>/binary,
            (gleam@int:to_string(erlang:element(5, Pattern)))/binary>>/binary,
        " behavior(s)
"/utf8>>.

-file("src/intent/pattern_library.gleam", 261).
?DOC(" Format patterns for display\n").
-spec format_patterns(list(pattern())) -> binary().
format_patterns(Patterns) ->
    case erlang:length(Patterns) of
        0 ->
            <<"No patterns found."/utf8>>;

        Count ->
            Header = <<<<"Found "/utf8, (gleam@int:to_string(Count))/binary>>/binary,
                " pattern(s):\\n\\n"/utf8>>,
            Pattern_lines = begin
                _pipe = Patterns,
                _pipe@1 = gleam@list:sort(
                    _pipe,
                    fun(A, B) ->
                        gleam@int:compare(
                            erlang:element(5, B),
                            erlang:element(5, A)
                        )
                    end
                ),
                _pipe@2 = gleam@list:map(_pipe@1, fun format_pattern/1),
                gleam@string:join(_pipe@2, <<"\\n\\n"/utf8>>)
            end,
            <<Header/binary, Pattern_lines/binary>>
    end.

-file("src/intent/pattern_library.gleam", 316).
?DOC(" Helper to conditionally append patterns\n").
-spec append_if(list(pattern()), boolean(), list(pattern())) -> list(pattern()).
append_if(Patterns, Condition, To_append) ->
    case Condition of
        true ->
            lists:append(Patterns, To_append);

        false ->
            Patterns
    end.

-file("src/intent/pattern_library.gleam", 289).
?DOC(" Suggest patterns for a new spec based on its content\n").
-spec suggest_patterns_for_spec(pattern_library(), intent@types:spec()) -> list(pattern()).
suggest_patterns_for_spec(Library, Spec) ->
    Behaviors = begin
        _pipe = erlang:element(8, Spec),
        gleam@list:flat_map(_pipe, fun(F) -> erlang:element(4, F) end)
    end,
    Has_errors = gleam@list:any(
        Behaviors,
        fun(B) -> erlang:element(2, erlang:element(8, B)) >= 400 end
    ),
    Has_auth = gleam@list:any(
        Behaviors,
        fun(B@1) ->
            gleam_stdlib:contains_string(
                gleam@string:lowercase(erlang:element(2, B@1)),
                <<"auth"/utf8>>
            )
        end
    ),
    Has_workflow = gleam@list:any(
        Behaviors,
        fun(B@2) -> not gleam@list:is_empty(erlang:element(5, B@2)) end
    ),
    _pipe@1 = [],
    _pipe@2 = append_if(
        _pipe@1,
        Has_errors,
        get_patterns_by_category(Library, <<"error-handling"/utf8>>)
    ),
    _pipe@3 = append_if(
        _pipe@2,
        Has_auth,
        get_patterns_by_category(Library, <<"authentication"/utf8>>)
    ),
    append_if(
        _pipe@3,
        Has_workflow,
        get_patterns_by_category(Library, <<"workflow"/utf8>>)
    ).
