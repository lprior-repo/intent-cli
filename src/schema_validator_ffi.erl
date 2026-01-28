-module(schema_validator_ffi).
-export([validate_json/2, load_schema_file/1]).

%% Validate a JSON string against a JSON Schema string
%% Both inputs are binary strings containing JSON
%% Returns: {ok, nil} | {error, ErrorBinary}
validate_json(SchemaJson, DataJson) ->
    try
        Schema = decode_json(SchemaJson),
        Data = decode_json(DataJson),
        %% Strip $schema and $id to avoid jesse rejecting draft-07 syntax
        Schema1 = strip_draft7_fields(Schema),
        %% Resolve $ref to base-response.json by inlining
        Schema2 = resolve_base_ref(Schema1),
        case jesse:validate_with_schema(Schema2, Data, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]) of
            {ok, _} -> {ok, nil};
            {error, Errors} ->
                ErrorMsg = format_errors(Errors),
                {error, ErrorMsg}
        end
    catch
        error:{invalid_json, Reason} ->
            {error, <<"JSON parse error: ", (iolist_to_binary(io_lib:format("~p", [Reason])))/binary>>};
        Class:Reason:_Stack ->
            {error, <<"Validation error: ", (iolist_to_binary(io_lib:format("~p:~p", [Class, Reason])))/binary>>}
    end.

%% Load a schema file from disk, returns the raw binary content
%% Returns: {ok, Binary} | {error, Binary}
load_schema_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, Content};
        {error, Reason} ->
            {error, <<"Failed to read schema file: ", (atom_to_binary(Reason))/binary>>}
    end.

%% Decode JSON binary to Erlang maps (OTP 27+ json module)
decode_json(Bin) ->
    json:decode(Bin).

%% Strip draft-07 specific fields that jesse doesn't understand
strip_draft7_fields(Schema) when is_map(Schema) ->
    maps:without([<<"$schema">>, <<"$id">>], Schema);
strip_draft7_fields(Schema) ->
    Schema.

%% Resolve allOf $ref to base-response.json by loading and inlining
resolve_base_ref(Schema) when is_map(Schema) ->
    case maps:get(<<"allOf">>, Schema, undefined) of
        undefined -> Schema;
        AllOf when is_list(AllOf) ->
            %% Find and replace $ref entries pointing to base-response.json
            NewAllOf = lists:map(fun resolve_ref_entry/1, AllOf),
            maps:put(<<"allOf">>, NewAllOf, Schema)
    end;
resolve_base_ref(Schema) ->
    Schema.

resolve_ref_entry(#{<<"$ref">> := <<"base-response.json">>}) ->
    %% Load and inline the base schema, resolving internal $refs
    case file:read_file(<<"schema/json-schema/base-response.json">>) of
        {ok, Content} ->
            Base = decode_json(Content),
            Base1 = strip_draft7_fields(Base),
            inline_local_refs(Base1);
        {error, _} ->
            #{}
    end;
resolve_ref_entry(Entry) when is_map(Entry) ->
    inline_local_refs(Entry);
resolve_ref_entry(Entry) ->
    Entry.

%% Recursively resolve all local $ref (e.g. #/definitions/Foo) by inlining definitions
inline_local_refs(Schema) when is_map(Schema) ->
    Defs = maps:get(<<"definitions">>, Schema, #{}),
    resolve_refs_in(Schema, Defs);
inline_local_refs(Other) ->
    Other.

resolve_refs_in(#{<<"$ref">> := Ref} = _Node, Defs) when is_binary(Ref) ->
    case Ref of
        <<"#/definitions/", Name/binary>> ->
            case maps:get(Name, Defs, undefined) of
                undefined -> #{};
                Def -> resolve_refs_in(Def, Defs)
            end;
        _ -> #{}
    end;
resolve_refs_in(Map, Defs) when is_map(Map) ->
    maps:map(fun(_K, V) -> resolve_refs_in(V, Defs) end, Map);
resolve_refs_in(List, Defs) when is_list(List) ->
    lists:map(fun(Item) -> resolve_refs_in(Item, Defs) end, List);
resolve_refs_in(Other, _Defs) ->
    Other.

%% Format jesse error list into a human-readable binary string
format_errors({all_schemas_not_valid, Errors}) when is_list(Errors) ->
    format_errors(Errors);
format_errors({schema_invalid, _Schema, ErrorType}) ->
    iolist_to_binary(io_lib:format("invalid schema: ~p", [ErrorType]));
format_errors(Errors) when is_list(Errors) ->
    Parts = lists:map(fun format_error/1, Errors),
    iolist_to_binary(lists:join(<<"; ">>, Parts));
format_errors(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

format_error({data_invalid, _Schema, ErrorType, Value, Path}) ->
    iolist_to_binary(io_lib:format("~p at ~s (got ~p)", [ErrorType, format_path(Path), Value]));
format_error({schema_invalid, _Schema, ErrorType}) ->
    iolist_to_binary(io_lib:format("schema error: ~p", [ErrorType]));
format_error(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

format_path([]) -> <<"root">>;
format_path(Path) when is_list(Path) ->
    Parts = lists:map(fun
        (P) when is_binary(P) -> P;
        (P) when is_integer(P) -> integer_to_binary(P);
        (P) -> iolist_to_binary(io_lib:format("~p", [P]))
    end, Path),
    iolist_to_binary(lists:join(<<".">>, Parts));
format_path(Path) ->
    iolist_to_binary(io_lib:format("~p", [Path])).
