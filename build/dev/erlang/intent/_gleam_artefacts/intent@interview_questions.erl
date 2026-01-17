-module(intent@interview_questions).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get_questions_for_round_with_db/3, get_questions_for_round/2, get_next_question/3]).

-spec get_questions_for_round_with_db(
    binary(),
    integer(),
    intent@question_loader:questions_database()
) -> list(intent@question_types:question()).
get_questions_for_round_with_db(Profile, Round, Db) ->
    intent@question_loader:get_questions(Db, Profile, Round).

-spec list_contains(list(binary()), binary()) -> boolean().
list_contains(List, Item) ->
    case List of
        [] ->
            false;

        [Head | Tail] ->
            case Head =:= Item of
                true ->
                    true;

                false ->
                    list_contains(Tail, Item)
            end
    end.

-spec find_first_unanswered(
    list(intent@question_types:question()),
    list(binary())
) -> {ok, intent@question_types:question()} | {error, nil}.
find_first_unanswered(Questions, Answered) ->
    case Questions of
        [] ->
            {error, nil};

        [Q | Rest] ->
            case list_contains(Answered, erlang:element(2, Q)) of
                true ->
                    find_first_unanswered(Rest, Answered);

                false ->
                    {ok, Q}
            end
    end.

-spec fallback_questions(binary(), integer()) -> list(intent@question_types:question()).
fallback_questions(Profile, Round) ->
    case Round of
        1 ->
            [{question,
                    <<"fallback-1"/utf8>>,
                    1,
                    user,
                    happy_path,
                    critical,
                    <<<<"In one sentence, what should this "/utf8,
                            Profile/binary>>/binary,
                        " do?"/utf8>>,
                    <<"Questions could not be loaded from CUE. Using fallback."/utf8>>,
                    <<"Describe the core purpose"/utf8>>,
                    <<"text"/utf8>>,
                    [<<"name"/utf8>>],
                    [],
                    []}];

        _ ->
            []
    end.

-spec get_questions_for_round(binary(), integer()) -> list(intent@question_types:question()).
get_questions_for_round(Profile, Round) ->
    case intent@question_loader:load_default_questions() of
        {ok, Db} ->
            intent@question_loader:get_questions(Db, Profile, Round);

        {error, _} ->
            fallback_questions(Profile, Round)
    end.

-spec get_next_question(binary(), integer(), list(binary())) -> gleam@option:option(intent@question_types:question()).
get_next_question(Profile, Round, Answered_ids) ->
    Questions = get_questions_for_round(Profile, Round),
    case find_first_unanswered(Questions, Answered_ids) of
        {ok, Q} ->
            {some, Q};

        {error, _} ->
            none
    end.
