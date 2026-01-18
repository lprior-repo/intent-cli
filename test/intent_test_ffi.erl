-module(intent_test_ffi).
-export([ref_set/2, ref_get/1, ref_new_list/0, ref_append/2, ref_get_list/1]).

%% Simple process dictionary-based mutable state for testing
%% Uses the ref itself as the key

ref_set(Ref, Value) ->
    put(Ref, Value),
    nil.

ref_get(Ref) ->
    get(Ref).

ref_new_list() ->
    Ref = make_ref(),
    put(Ref, []),
    Ref.

ref_append(Ref, Value) ->
    Current = get(Ref),
    put(Ref, Current ++ [Value]),
    nil.

ref_get_list(Ref) ->
    get(Ref).
