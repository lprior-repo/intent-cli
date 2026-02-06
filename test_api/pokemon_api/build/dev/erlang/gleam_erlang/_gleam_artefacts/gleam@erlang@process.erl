-module(gleam@erlang@process).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([self/0, start/2, new_subject/0, subject_owner/1, send/2, 'receive'/2, receive_forever/1, new_selector/0, select/2, select_forever/1, map_selector/2, merge_selector/2, flush_messages/0, selecting_trapped_exits/2, selecting/3, selecting_record2/3, selecting_record3/3, selecting_record4/3, selecting_record5/3, selecting_record6/3, selecting_record7/3, selecting_record8/3, selecting_anything/2, deselecting/2, sleep/1, sleep_forever/0, is_alive/1, monitor_process/1, selecting_process_down/3, demonitor_process/1, deselecting_process_down/2, try_call/3, call/3, try_call_forever/2, call_forever/2, link/1, unlink/1, send_after/3, cancel_timer/1, kill/1, send_exit/1, send_abnormal_exit/2, trap_exits/1, register/2, unregister/1, named/1, pid_from_dynamic/1]).
-export_type([pid_/0, subject/1, do_not_leak/0, selector/1, exit_message/0, exit_reason/0, anything_selector_tag/0, process_monitor_flag/0, process_monitor/0, process_down/0, call_error/1, timer/0, cancelled/0, kill_flag/0]).

-type pid_() :: any().

-opaque subject(FWS) :: {subject, pid_(), gleam@erlang:reference_()} |
    {gleam_phantom, FWS}.

-type do_not_leak() :: any().

-type selector(FWT) :: any() | {gleam_phantom, FWT}.

-type exit_message() :: {exit_message, pid_(), exit_reason()}.

-type exit_reason() :: normal | killed | {abnormal, binary()}.

-type anything_selector_tag() :: anything.

-type process_monitor_flag() :: process.

-opaque process_monitor() :: {process_monitor, gleam@erlang:reference_()}.

-type process_down() :: {process_down, pid_(), gleam@dynamic:dynamic_()}.

-type call_error(FWU) :: {callee_down, gleam@dynamic:dynamic_()} |
    call_timeout |
    {gleam_phantom, FWU}.

-type timer() :: any().

-type cancelled() :: timer_not_found | {cancelled, integer()}.

-type kill_flag() :: kill.

-spec self() -> pid_().
self() ->
    erlang:self().

-spec start(fun(() -> any()), boolean()) -> pid_().
start(Implementation, Link) ->
    case Link of
        true ->
            erlang:spawn_link(Implementation);

        false ->
            erlang:spawn(Implementation)
    end.

-spec new_subject() -> subject(any()).
new_subject() ->
    {subject, erlang:self(), erlang:make_ref()}.

-spec subject_owner(subject(any())) -> pid_().
subject_owner(Subject) ->
    erlang:element(2, Subject).

-spec send(subject(FXD), FXD) -> nil.
send(Subject, Message) ->
    erlang:send(
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ),
    nil.

-spec 'receive'(subject(FXF), integer()) -> {ok, FXF} | {error, nil}.
'receive'(Subject, Timeout) ->
    gleam_erlang_ffi:'receive'(Subject, Timeout).

-spec receive_forever(subject(FXJ)) -> FXJ.
receive_forever(Subject) ->
    gleam_erlang_ffi:'receive'(Subject).

-spec new_selector() -> selector(any()).
new_selector() ->
    gleam_erlang_ffi:new_selector().

-spec select(selector(FXN), integer()) -> {ok, FXN} | {error, nil}.
select(From, Within) ->
    gleam_erlang_ffi:select(From, Within).

-spec select_forever(selector(FXR)) -> FXR.
select_forever(From) ->
    gleam_erlang_ffi:select(From).

-spec map_selector(selector(FXT), fun((FXT) -> FXV)) -> selector(FXV).
map_selector(A, B) ->
    gleam_erlang_ffi:map_selector(A, B).

-spec merge_selector(selector(FXX), selector(FXX)) -> selector(FXX).
merge_selector(A, B) ->
    gleam_erlang_ffi:merge_selector(A, B).

-spec flush_messages() -> nil.
flush_messages() ->
    gleam_erlang_ffi:flush_messages().

-spec selecting_trapped_exits(selector(FYB), fun((exit_message()) -> FYB)) -> selector(FYB).
selecting_trapped_exits(Selector, Handler) ->
    Tag = erlang:binary_to_atom(<<"EXIT"/utf8>>),
    Handler@1 = fun(Message) ->
        Reason = erlang:element(3, Message),
        Normal = gleam@dynamic:from(normal),
        Killed = gleam@dynamic:from(killed),
        Reason@2 = case gleam@dynamic:string(Reason) of
            _ when Reason =:= Normal ->
                normal;

            _ when Reason =:= Killed ->
                killed;

            {ok, Reason@1} ->
                {abnormal, Reason@1};

            {error, _} ->
                {abnormal, gleam@string:inspect(Reason)}
        end,
        Handler({exit_message, erlang:element(2, Message), Reason@2})
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler@1).

-spec selecting(selector(FYE), subject(FYG), fun((FYG) -> FYE)) -> selector(FYE).
selecting(Selector, Subject, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2},
        Handler
    ).

-spec selecting_record2(
    selector(FYO),
    any(),
    fun((gleam@dynamic:dynamic_()) -> FYO)
) -> selector(FYO).
selecting_record2(Selector, Tag, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 2}, Handler).

-spec selecting_record3(
    selector(FYS),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FYS)
) -> selector(FYS).
selecting_record3(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(erlang:element(2, Message), erlang:element(3, Message))
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler).

-spec selecting_record4(
    selector(FYW),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FYW)
) -> selector(FYW).
selecting_record4(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 4}, Handler).

-spec selecting_record5(
    selector(FZA),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FZA)
) -> selector(FZA).
selecting_record5(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 5}, Handler).

-spec selecting_record6(
    selector(FZE),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FZE)
) -> selector(FZE).
selecting_record6(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 6}, Handler).

-spec selecting_record7(
    selector(FZI),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FZI)
) -> selector(FZI).
selecting_record7(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 7}, Handler).

-spec selecting_record8(
    selector(FZM),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FZM)
) -> selector(FZM).
selecting_record8(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message),
            erlang:element(8, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 8}, Handler).

-spec selecting_anything(selector(FZQ), fun((gleam@dynamic:dynamic_()) -> FZQ)) -> selector(FZQ).
selecting_anything(Selector, Handler) ->
    gleam_erlang_ffi:insert_selector_handler(Selector, anything, Handler).

-spec deselecting(selector(FYJ), subject(any())) -> selector(FYJ).
deselecting(Selector, Subject) ->
    gleam_erlang_ffi:remove_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2}
    ).

-spec sleep(integer()) -> nil.
sleep(A) ->
    gleam_erlang_ffi:sleep(A).

-spec sleep_forever() -> nil.
sleep_forever() ->
    gleam_erlang_ffi:sleep_forever().

-spec is_alive(pid_()) -> boolean().
is_alive(A) ->
    erlang:is_process_alive(A).

-spec monitor_process(pid_()) -> process_monitor().
monitor_process(Pid) ->
    _pipe = process,
    _pipe@1 = erlang:monitor(_pipe, Pid),
    {process_monitor, _pipe@1}.

-spec selecting_process_down(
    selector(GAC),
    process_monitor(),
    fun((process_down()) -> GAC)
) -> selector(GAC).
selecting_process_down(Selector, Monitor, Mapping) ->
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        erlang:element(2, Monitor),
        Mapping
    ).

-spec demonitor_process(process_monitor()) -> nil.
demonitor_process(Monitor) ->
    gleam_erlang_ffi:demonitor(Monitor).

-spec deselecting_process_down(selector(GAF), process_monitor()) -> selector(GAF).
deselecting_process_down(Selector, Monitor) ->
    gleam_erlang_ffi:remove_selector_handler(
        Selector,
        erlang:element(2, Monitor)
    ).

-spec try_call(subject(GAI), fun((subject(GAK)) -> GAI), integer()) -> {ok, GAK} |
    {error, call_error(GAK)}.
try_call(Subject, Make_request, Timeout) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2, Timeout)
    end,
    gleam_erlang_ffi:demonitor(Monitor),
    case Result of
        {error, nil} ->
            {error, call_timeout};

        {ok, Res} ->
            Res
    end.

-spec call(subject(GAP), fun((subject(GAR)) -> GAP), integer()) -> GAR.
call(Subject, Make_request, Timeout) ->
    _assert_subject = try_call(Subject, Make_request, Timeout),
    {ok, Resp} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call"/utf8>>,
                        line => 635})
    end,
    Resp.

-spec try_call_forever(subject(GAX), fun((subject(GAZ)) -> GAX)) -> {ok, GAZ} |
    {error, call_error(any())}.
try_call_forever(Subject, Make_request) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2)
    end,
    gleam_erlang_ffi:demonitor(Monitor),
    Result.

-spec call_forever(subject(GAT), fun((subject(GAV)) -> GAT)) -> GAV.
call_forever(Subject, Make_request) ->
    _assert_subject = try_call_forever(Subject, Make_request),
    {ok, Response} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call_forever"/utf8>>,
                        line => 650})
    end,
    Response.

-spec link(pid_()) -> boolean().
link(Pid) ->
    gleam_erlang_ffi:link(Pid).

-spec unlink(pid_()) -> nil.
unlink(Pid) ->
    erlang:unlink(Pid),
    nil.

-spec send_after(subject(GBG), integer(), GBG) -> timer().
send_after(Subject, Delay, Message) ->
    erlang:send_after(
        Delay,
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ).

-spec cancel_timer(timer()) -> cancelled().
cancel_timer(Timer) ->
    case gleam@dynamic:int(erlang:cancel_timer(Timer)) of
        {ok, I} ->
            {cancelled, I};

        {error, _} ->
            timer_not_found
    end.

-spec kill(pid_()) -> nil.
kill(Pid) ->
    erlang:exit(Pid, kill),
    nil.

-spec send_exit(pid_()) -> nil.
send_exit(Pid) ->
    erlang:exit(Pid, normal),
    nil.

-spec send_abnormal_exit(pid_(), binary()) -> nil.
send_abnormal_exit(Pid, Reason) ->
    erlang:exit(Pid, {abnormal, Reason}),
    nil.

-spec trap_exits(boolean()) -> nil.
trap_exits(A) ->
    gleam_erlang_ffi:trap_exits(A).

-spec register(pid_(), gleam@erlang@atom:atom_()) -> {ok, nil} | {error, nil}.
register(Pid, Name) ->
    gleam_erlang_ffi:register_process(Pid, Name).

-spec unregister(gleam@erlang@atom:atom_()) -> {ok, nil} | {error, nil}.
unregister(Name) ->
    gleam_erlang_ffi:unregister_process(Name).

-spec named(gleam@erlang@atom:atom_()) -> {ok, pid_()} | {error, nil}.
named(Name) ->
    gleam_erlang_ffi:process_named(Name).

-spec pid_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, pid_()} |
    {error, list(gleam@dynamic:decode_error())}.
pid_from_dynamic(From) ->
    gleam_erlang_ffi:pid_from_dynamic(From).
