-module(glearray).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, from_list/1, to_list/1, length/1, get/2, get_or_default/3, copy_set/3, copy_push/2, copy_insert/3]).
-export_type([array/1]).

-type array(KEG) :: any() | {gleam_phantom, KEG}.

-spec new() -> array(any()).
new() ->
    glearray_ffi:new().

-spec from_list(list(KEJ)) -> array(KEJ).
from_list(List) ->
    erlang:list_to_tuple(List).

-spec to_list(array(KEM)) -> list(KEM).
to_list(Array) ->
    erlang:tuple_to_list(Array).

-spec length(array(any())) -> integer().
length(Array) ->
    erlang:tuple_size(Array).

-spec is_valid_index(array(any()), integer()) -> boolean().
is_valid_index(Array, Index) ->
    (Index >= 0) andalso (Index < erlang:tuple_size(Array)).

-spec get(array(KER), integer()) -> {ok, KER} | {error, nil}.
get(Array, Index) ->
    glearray_ffi:get(Array, Index).

-spec get_or_default(array(KEV), integer(), KEV) -> KEV.
get_or_default(Array, Index, Default) ->
    glearray_ffi:get_or_default(Array, Index, Default).

-spec copy_set(array(KEZ), integer(), KEZ) -> {ok, array(KEZ)} | {error, nil}.
copy_set(Array, Index, Value) ->
    glearray_ffi:set(Array, Index, Value).

-spec copy_push(array(KFJ), KFJ) -> array(KFJ).
copy_push(Array, Value) ->
    erlang:append_element(Array, Value).

-spec copy_insert(array(KFM), integer(), KFM) -> {ok, array(KFM)} | {error, nil}.
copy_insert(Array, Index, Value) ->
    glearray_ffi:insert(Array, Index, Value).
