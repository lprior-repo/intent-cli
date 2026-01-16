-module(pokemon_api).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/pokemon_api.gleam").
-export([init_store/0, main/0, handle_request/4]).
-export_type([pokemon/0, trainer/0, store/0]).

-type pokemon() :: {pokemon,
        binary(),
        binary(),
        binary(),
        integer(),
        integer(),
        binary()}.

-type trainer() :: {trainer, binary(), binary(), list(binary())}.

-type store() :: {store,
        gleam@dict:dict(binary(), pokemon()),
        gleam@dict:dict(binary(), trainer()),
        integer()}.

-file("src/pokemon_api.gleam", 63).
-spec init_store() -> store().
init_store() ->
    Pikachu = {pokemon,
        <<"pkmn_001"/utf8>>,
        <<"Pikachu"/utf8>>,
        <<"Electric"/utf8>>,
        25,
        100,
        <<"trainer_ash"/utf8>>},
    Charizard = {pokemon,
        <<"pkmn_002"/utf8>>,
        <<"Charizard"/utf8>>,
        <<"Fire"/utf8>>,
        50,
        200,
        <<"trainer_ash"/utf8>>},
    Bulbasaur = {pokemon,
        <<"pkmn_003"/utf8>>,
        <<"Bulbasaur"/utf8>>,
        <<"Grass"/utf8>>,
        15,
        80,
        <<"trainer_misty"/utf8>>},
    Ash = {trainer,
        <<"trainer_ash"/utf8>>,
        <<"Ash Ketchum"/utf8>>,
        [<<"pkmn_001"/utf8>>, <<"pkmn_002"/utf8>>]},
    Misty = {trainer,
        <<"trainer_misty"/utf8>>,
        <<"Misty"/utf8>>,
        [<<"pkmn_003"/utf8>>]},
    {store,
        maps:from_list(
            [{<<"pkmn_001"/utf8>>, Pikachu},
                {<<"pkmn_002"/utf8>>, Charizard},
                {<<"pkmn_003"/utf8>>, Bulbasaur}]
        ),
        maps:from_list(
            [{<<"trainer_ash"/utf8>>, Ash}, {<<"trainer_misty"/utf8>>, Misty}]
        ),
        4}.

-file("src/pokemon_api.gleam", 45).
-spec main() -> nil.
main() ->
    Store = init_store(),
    gleam@io:println(<<"Starting Pokemon API on http://localhost:8080"/utf8>>),
    _ = pokemon_server:start(8080, Store),
    gleam_erlang_ffi:sleep_forever().

-file("src/pokemon_api.gleam", 203).
-spec parse_pokemon_create(binary()) -> {ok,
        {binary(), binary(), integer(), binary()}} |
    {error, binary()}.
parse_pokemon_create(Body_str) ->
    case gleam@json:decode(Body_str, fun gleam@dynamic:dynamic/1) of
        {ok, Data} ->
            Decoder = gleam@dynamic:decode4(
                fun(Name, Pokemon_type, Level, Trainer_id) ->
                    {Name, Pokemon_type, Level, Trainer_id}
                end,
                gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1),
                gleam@dynamic:field(<<"type"/utf8>>, fun gleam@dynamic:string/1),
                gleam@dynamic:field(<<"level"/utf8>>, fun gleam@dynamic:int/1),
                gleam@dynamic:field(
                    <<"trainer_id"/utf8>>,
                    fun gleam@dynamic:string/1
                )
            ),
            case Decoder(Data) of
                {ok, Res} ->
                    {ok, Res};

                {error, _} ->
                    {error,
                        <<"Missing required fields: name, type, level, trainer_id"/utf8>>}
            end;

        {error, _} ->
            {error, <<"Invalid JSON"/utf8>>}
    end.

-file("src/pokemon_api.gleam", 287).
-spec error_response(integer(), binary(), binary()) -> {integer(), binary()}.
error_response(Status, Code, Message) ->
    Response = gleam@json:object(
        [{<<"error"/utf8>>,
                gleam@json:object(
                    [{<<"code"/utf8>>, gleam@json:string(Code)},
                        {<<"message"/utf8>>, gleam@json:string(Message)}]
                )}]
    ),
    {Status, gleam@json:to_string(Response)}.

-file("src/pokemon_api.gleam", 283).
-spec not_found() -> {integer(), binary()}.
not_found() ->
    error_response(404, <<"NOT_FOUND"/utf8>>, <<"Endpoint not found"/utf8>>).

-file("src/pokemon_api.gleam", 305).
-spec pokemon_to_json(pokemon()) -> gleam@json:json().
pokemon_to_json(Pokemon) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Pokemon))},
            {<<"name"/utf8>>, gleam@json:string(erlang:element(3, Pokemon))},
            {<<"type"/utf8>>, gleam@json:string(erlang:element(4, Pokemon))},
            {<<"level"/utf8>>, gleam@json:int(erlang:element(5, Pokemon))},
            {<<"hp"/utf8>>, gleam@json:int(erlang:element(6, Pokemon))},
            {<<"trainer_id"/utf8>>,
                gleam@json:string(erlang:element(7, Pokemon))}]
    ).

-file("src/pokemon_api.gleam", 145).
-spec list_pokemon(store()) -> {integer(), binary()}.
list_pokemon(Store) ->
    Pokemon_list = begin
        _pipe = erlang:element(2, Store),
        _pipe@1 = gleam@dict:values(_pipe),
        gleam@list:map(_pipe@1, fun pokemon_to_json/1)
    end,
    Response = gleam@json:object(
        [{<<"pokemon"/utf8>>, gleam@json:array(Pokemon_list, fun(X) -> X end)}]
    ),
    {200, gleam@json:to_string(Response)}.

-file("src/pokemon_api.gleam", 156).
-spec get_pokemon(store(), binary()) -> {integer(), binary()}.
get_pokemon(Store, Id) ->
    case gleam@dict:get(erlang:element(2, Store), Id) of
        {ok, Pokemon} ->
            {200, gleam@json:to_string(pokemon_to_json(Pokemon))};

        {error, _} ->
            error_response(
                404,
                <<"POKEMON_NOT_FOUND"/utf8>>,
                <<<<"Pokemon with id '"/utf8, Id/binary>>/binary,
                    "' not found"/utf8>>
            )
    end.

-file("src/pokemon_api.gleam", 255).
-spec get_trainer_pokemon(store(), binary()) -> {integer(), binary()}.
get_trainer_pokemon(Store, Id) ->
    case gleam@dict:get(erlang:element(3, Store), Id) of
        {ok, Trainer} ->
            Pokemon_list = begin
                _pipe = erlang:element(4, Trainer),
                gleam@list:filter_map(
                    _pipe,
                    fun(Pid) ->
                        case gleam@dict:get(erlang:element(2, Store), Pid) of
                            {ok, P} ->
                                {ok, pokemon_to_json(P)};

                            {error, _} ->
                                {error, nil}
                        end
                    end
                )
            end,
            Response = gleam@json:object(
                [{<<"pokemon"/utf8>>,
                        gleam@json:array(Pokemon_list, fun(X) -> X end)}]
            ),
            {200, gleam@json:to_string(Response)};

        {error, _} ->
            error_response(
                404,
                <<"TRAINER_NOT_FOUND"/utf8>>,
                <<<<"Trainer with id '"/utf8, Id/binary>>/binary,
                    "' not found"/utf8>>
            )
    end.

-file("src/pokemon_api.gleam", 316).
-spec trainer_to_json(trainer()) -> gleam@json:json().
trainer_to_json(Trainer) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Trainer))},
            {<<"name"/utf8>>, gleam@json:string(erlang:element(3, Trainer))},
            {<<"pokemon_count"/utf8>>,
                gleam@json:int(erlang:length(erlang:element(4, Trainer)))}]
    ).

-file("src/pokemon_api.gleam", 232).
-spec list_trainers(store()) -> {integer(), binary()}.
list_trainers(Store) ->
    Trainer_list = begin
        _pipe = erlang:element(3, Store),
        _pipe@1 = gleam@dict:values(_pipe),
        gleam@list:map(_pipe@1, fun trainer_to_json/1)
    end,
    Response = gleam@json:object(
        [{<<"trainers"/utf8>>, gleam@json:array(Trainer_list, fun(X) -> X end)}]
    ),
    {200, gleam@json:to_string(Response)}.

-file("src/pokemon_api.gleam", 243).
-spec get_trainer(store(), binary()) -> {integer(), binary()}.
get_trainer(Store, Id) ->
    case gleam@dict:get(erlang:element(3, Store), Id) of
        {ok, Trainer} ->
            {200, gleam@json:to_string(trainer_to_json(Trainer))};

        {error, _} ->
            error_response(
                404,
                <<"TRAINER_NOT_FOUND"/utf8>>,
                <<<<"Trainer with id '"/utf8, Id/binary>>/binary,
                    "' not found"/utf8>>
            )
    end.

-file("src/pokemon_api.gleam", 328).
-spec pad_id(integer()) -> binary().
pad_id(N) ->
    S = gleam@int:to_string(N),
    case gleam@string:length(S) of
        1 ->
            <<"00"/utf8, S/binary>>;

        2 ->
            <<"0"/utf8, S/binary>>;

        _ ->
            S
    end.

-file("src/pokemon_api.gleam", 168).
-spec create_pokemon(binary(), store()) -> {integer(), binary()}.
create_pokemon(Body, Store) ->
    case parse_pokemon_create(Body) of
        {ok, {Name, Pokemon_type, Level, Trainer_id}} ->
            case gleam@dict:get(erlang:element(3, Store), Trainer_id) of
                {ok, _} ->
                    case (Level >= 1) andalso (Level =< 100) of
                        true ->
                            Id = <<"pkmn_"/utf8,
                                (pad_id(erlang:element(4, Store)))/binary>>,
                            Pokemon = {pokemon,
                                Id,
                                Name,
                                Pokemon_type,
                                Level,
                                Level * 4,
                                Trainer_id},
                            {201,
                                gleam@json:to_string(pokemon_to_json(Pokemon))};

                        false ->
                            error_response(
                                400,
                                <<"INVALID_LEVEL"/utf8>>,
                                <<"Level must be between 1 and 100"/utf8>>
                            )
                    end;

                {error, _} ->
                    error_response(
                        400,
                        <<"TRAINER_NOT_FOUND"/utf8>>,
                        <<"Trainer not found"/utf8>>
                    )
            end;

        {error, Msg} ->
            error_response(400, <<"INVALID_REQUEST"/utf8>>, Msg)
    end.

-file("src/pokemon_api.gleam", 119).
-spec handle_request(binary(), binary(), binary(), store()) -> {integer(),
    binary()}.
handle_request(Method, Path, Body, Store) ->
    Path_parts = begin
        _pipe = Path,
        _pipe@1 = gleam@string:split(_pipe, <<"/"/utf8>>),
        gleam@list:filter(_pipe@1, fun(S) -> S /= <<""/utf8>> end)
    end,
    case {Method, Path_parts} of
        {<<"GET"/utf8>>, [<<"pokemon"/utf8>>]} ->
            list_pokemon(Store);

        {<<"GET"/utf8>>, [<<"pokemon"/utf8>>, Id]} ->
            get_pokemon(Store, Id);

        {<<"POST"/utf8>>, [<<"pokemon"/utf8>>]} ->
            create_pokemon(Body, Store);

        {<<"GET"/utf8>>, [<<"trainers"/utf8>>]} ->
            list_trainers(Store);

        {<<"GET"/utf8>>, [<<"trainers"/utf8>>, Id@1]} ->
            get_trainer(Store, Id@1);

        {<<"GET"/utf8>>, [<<"trainers"/utf8>>, Id@2, <<"pokemon"/utf8>>]} ->
            get_trainer_pokemon(Store, Id@2);

        {_, _} ->
            not_found()
    end.
