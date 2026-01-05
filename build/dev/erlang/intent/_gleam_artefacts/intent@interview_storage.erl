-module(intent@interview_storage).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/interview_storage.gleam").
-export([session_to_json/1, session_to_jsonl_line/1, append_session_to_jsonl/2, list_sessions_from_jsonl/1, get_session_from_jsonl/2, init_database/1, save_session_to_db/2, query_sessions_by_profile/2, query_ready_sessions/1, sync_to_jsonl/3, sync_from_jsonl/2]).
-export_type([session_record/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type session_record() :: {session_record,
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        integer(),
        binary()}.

-file("src/intent/interview_storage.gleam", 50).
-spec profile_to_string(intent@interview:profile()) -> binary().
profile_to_string(Profile) ->
    case Profile of
        api ->
            <<"api"/utf8>>;

        cli ->
            <<"cli"/utf8>>;

        event ->
            <<"event"/utf8>>;

        data ->
            <<"data"/utf8>>;

        workflow ->
            <<"workflow"/utf8>>;

        u_i ->
            <<"ui"/utf8>>
    end.

-file("src/intent/interview_storage.gleam", 61).
-spec stage_to_string(intent@interview:interview_stage()) -> binary().
stage_to_string(Stage) ->
    case Stage of
        discovery ->
            <<"discovery"/utf8>>;

        refinement ->
            <<"refinement"/utf8>>;

        validation ->
            <<"validation"/utf8>>;

        complete ->
            <<"complete"/utf8>>;

        paused ->
            <<"paused"/utf8>>
    end.

-file("src/intent/interview_storage.gleam", 88).
-spec perspective_to_string(intent@interview_questions:perspective()) -> binary().
perspective_to_string(Perspective) ->
    case Perspective of
        user ->
            <<"user"/utf8>>;

        developer ->
            <<"developer"/utf8>>;

        ops ->
            <<"ops"/utf8>>;

        security ->
            <<"security"/utf8>>;

        business ->
            <<"business"/utf8>>
    end.

-file("src/intent/interview_storage.gleam", 71).
-spec answer_to_json(intent@interview:answer()) -> gleam@json:json().
answer_to_json(Answer) ->
    gleam@json:object(
        [{<<"question_id"/utf8>>, gleam@json:string(erlang:element(2, Answer))},
            {<<"question_text"/utf8>>,
                gleam@json:string(erlang:element(3, Answer))},
            {<<"perspective"/utf8>>,
                gleam@json:string(
                    perspective_to_string(erlang:element(4, Answer))
                )},
            {<<"round"/utf8>>, gleam@json:int(erlang:element(5, Answer))},
            {<<"response"/utf8>>, gleam@json:string(erlang:element(6, Answer))},
            {<<"extracted"/utf8>>,
                gleam@json:object(
                    begin
                        _pipe = maps:to_list(erlang:element(7, Answer)),
                        gleam@list:map(
                            _pipe,
                            fun(Pair) ->
                                {erlang:element(1, Pair),
                                    gleam@json:string(erlang:element(2, Pair))}
                            end
                        )
                    end
                )},
            {<<"confidence"/utf8>>, gleam@json:float(erlang:element(8, Answer))},
            {<<"notes"/utf8>>, gleam@json:string(erlang:element(9, Answer))},
            {<<"timestamp"/utf8>>,
                gleam@json:string(erlang:element(10, Answer))}]
    ).

-file("src/intent/interview_storage.gleam", 98).
-spec gap_to_json(intent@interview:gap()) -> gleam@json:json().
gap_to_json(Gap) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Gap))},
            {<<"field"/utf8>>, gleam@json:string(erlang:element(3, Gap))},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(4, Gap))},
            {<<"blocking"/utf8>>, gleam@json:bool(erlang:element(5, Gap))},
            {<<"suggested_default"/utf8>>,
                gleam@json:string(erlang:element(6, Gap))},
            {<<"why_needed"/utf8>>, gleam@json:string(erlang:element(7, Gap))},
            {<<"round"/utf8>>, gleam@json:int(erlang:element(8, Gap))},
            {<<"resolved"/utf8>>, gleam@json:bool(erlang:element(9, Gap))},
            {<<"resolution"/utf8>>, gleam@json:string(erlang:element(10, Gap))}]
    ).

-file("src/intent/interview_storage.gleam", 124).
-spec conflict_resolution_to_json(intent@interview:conflict_resolution()) -> gleam@json:json().
conflict_resolution_to_json(Res) ->
    gleam@json:object(
        [{<<"option"/utf8>>, gleam@json:string(erlang:element(2, Res))},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(3, Res))},
            {<<"tradeoffs"/utf8>>, gleam@json:string(erlang:element(4, Res))},
            {<<"recommendation"/utf8>>,
                gleam@json:string(erlang:element(5, Res))}]
    ).

-file("src/intent/interview_storage.gleam", 112).
-spec conflict_to_json(intent@interview:conflict()) -> gleam@json:json().
conflict_to_json(Conflict) ->
    {Between_1, Between_2} = erlang:element(3, Conflict),
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Conflict))},
            {<<"between"/utf8>>,
                gleam@json:array(
                    [Between_1, Between_2],
                    fun gleam@json:string/1
                )},
            {<<"description"/utf8>>,
                gleam@json:string(erlang:element(4, Conflict))},
            {<<"impact"/utf8>>, gleam@json:string(erlang:element(5, Conflict))},
            {<<"options"/utf8>>,
                gleam@json:array(
                    erlang:element(6, Conflict),
                    fun conflict_resolution_to_json/1
                )},
            {<<"chosen"/utf8>>, gleam@json:int(erlang:element(7, Conflict))}]
    ).

-file("src/intent/interview_storage.gleam", 34).
?DOC(
    " JSONL operations - git-friendly line-delimited JSON\n"
    " Each line is a complete session snapshot\n"
    " Stored at: .interview/sessions.jsonl\n"
).
-spec session_to_json(intent@interview:interview_session()) -> gleam@json:json().
session_to_json(Session) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Session))},
            {<<"profile"/utf8>>,
                gleam@json:string(profile_to_string(erlang:element(3, Session)))},
            {<<"created_at"/utf8>>,
                gleam@json:string(erlang:element(4, Session))},
            {<<"updated_at"/utf8>>,
                gleam@json:string(erlang:element(5, Session))},
            {<<"completed_at"/utf8>>,
                gleam@json:string(erlang:element(6, Session))},
            {<<"stage"/utf8>>,
                gleam@json:string(stage_to_string(erlang:element(7, Session)))},
            {<<"rounds_completed"/utf8>>,
                gleam@json:int(erlang:element(8, Session))},
            {<<"answers"/utf8>>,
                gleam@json:array(
                    erlang:element(9, Session),
                    fun answer_to_json/1
                )},
            {<<"gaps"/utf8>>,
                gleam@json:array(erlang:element(10, Session), fun gap_to_json/1)},
            {<<"conflicts"/utf8>>,
                gleam@json:array(
                    erlang:element(11, Session),
                    fun conflict_to_json/1
                )},
            {<<"raw_notes"/utf8>>,
                gleam@json:string(erlang:element(12, Session))}]
    ).

-file("src/intent/interview_storage.gleam", 134).
?DOC(" Encode session to JSONL line (for git storage)\n").
-spec session_to_jsonl_line(intent@interview:interview_session()) -> binary().
session_to_jsonl_line(Session) ->
    _pipe = Session,
    _pipe@1 = session_to_json(_pipe),
    gleam@json:to_string(_pipe@1).

-file("src/intent/interview_storage.gleam", 142).
?DOC(
    " Write session to .interview/sessions.jsonl\n"
    " Each session ID appears once, most recent last (for efficient updates)\n"
).
-spec append_session_to_jsonl(intent@interview:interview_session(), binary()) -> {ok,
        nil} |
    {error, binary()}.
append_session_to_jsonl(Session, Jsonl_path) ->
    {ok, nil}.

-file("src/intent/interview_storage.gleam", 156).
?DOC(" List all sessions from JSONL file\n").
-spec list_sessions_from_jsonl(binary()) -> {ok,
        list(intent@interview:interview_session())} |
    {error, binary()}.
list_sessions_from_jsonl(Jsonl_path) ->
    {ok, []}.

-file("src/intent/interview_storage.gleam", 164).
?DOC(" Get session by ID from JSONL\n").
-spec get_session_from_jsonl(binary(), binary()) -> {ok,
        intent@interview:interview_session()} |
    {error, binary()}.
get_session_from_jsonl(Jsonl_path, Session_id) ->
    _pipe = list_sessions_from_jsonl(Jsonl_path),
    gleam@result:'try'(
        _pipe,
        fun(Sessions) ->
            _pipe@1 = gleam@list:find(
                Sessions,
                fun(S) -> erlang:element(2, S) =:= Session_id end
            ),
            gleam@result:map_error(
                _pipe@1,
                fun(_) -> <<"Session not found: "/utf8, Session_id/binary>> end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 217).
?DOC(
    " SQLite operations - local database for queries and performance\n"
    " Database schema:\n"
    "\n"
    " CREATE TABLE sessions (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   profile TEXT NOT NULL,\n"
    "   created_at TEXT NOT NULL,\n"
    "   updated_at TEXT NOT NULL,\n"
    "   completed_at TEXT,\n"
    "   stage TEXT NOT NULL,\n"
    "   rounds_completed INTEGER NOT NULL,\n"
    "   raw_notes TEXT,\n"
    "   data JSONB  -- Full session data\n"
    " );\n"
    "\n"
    " CREATE TABLE answers (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   session_id TEXT NOT NULL REFERENCES sessions(id),\n"
    "   question_id TEXT NOT NULL,\n"
    "   round INTEGER NOT NULL,\n"
    "   perspective TEXT NOT NULL,\n"
    "   response TEXT NOT NULL,\n"
    "   confidence REAL NOT NULL,\n"
    "   timestamp TEXT NOT NULL\n"
    " );\n"
    "\n"
    " CREATE TABLE gaps (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   session_id TEXT NOT NULL REFERENCES sessions(id),\n"
    "   field TEXT NOT NULL,\n"
    "   blocking BOOLEAN NOT NULL,\n"
    "   resolved BOOLEAN NOT NULL\n"
    " );\n"
    "\n"
    " CREATE TABLE conflicts (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   session_id TEXT NOT NULL REFERENCES sessions(id),\n"
    "   description TEXT NOT NULL,\n"
    "   chosen INTEGER\n"
    " );\n"
    " Initialize SQLite database (create tables if not exist)\n"
).
-spec init_database(binary()) -> {ok, nil} | {error, binary()}.
init_database(Db_path) ->
    {ok, nil}.

-file("src/intent/interview_storage.gleam", 226).
?DOC(" Save session to SQLite\n").
-spec save_session_to_db(binary(), intent@interview:interview_session()) -> {ok,
        nil} |
    {error, binary()}.
save_session_to_db(Db_path, Session) ->
    {ok, nil}.

-file("src/intent/interview_storage.gleam", 236).
?DOC(" Query sessions by profile\n").
-spec query_sessions_by_profile(binary(), binary()) -> {ok,
        list(session_record())} |
    {error, binary()}.
query_sessions_by_profile(Db_path, Profile) ->
    {ok, []}.

-file("src/intent/interview_storage.gleam", 244).
?DOC(" Query ready sessions (active, not complete, has gaps)\n").
-spec query_ready_sessions(binary()) -> {ok, list(session_record())} |
    {error, binary()}.
query_ready_sessions(Db_path) ->
    {ok, []}.

-file("src/intent/interview_storage.gleam", 254).
?DOC(
    " Sync operations - keep SQLite and JSONL in sync\n"
    " Strategy: JSONL is source of truth for git\n"
    " 1. On read: load from JSONL, check SQLite is consistent\n"
    " 2. On write: write to both\n"
    " 3. Conflict resolution: JSONL wins (it's in git)\n"
).
-spec sync_to_jsonl(intent@interview:interview_session(), binary(), binary()) -> {ok,
        nil} |
    {error, binary()}.
sync_to_jsonl(Session, Db_path, Jsonl_path) ->
    gleam@result:'try'(
        save_session_to_db(Db_path, Session),
        fun(_) ->
            gleam@result:'try'(
                append_session_to_jsonl(Session, Jsonl_path),
                fun(_) -> {ok, nil} end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 266).
-spec sync_from_jsonl(binary(), binary()) -> {ok,
        list(intent@interview:interview_session())} |
    {error, binary()}.
sync_from_jsonl(Jsonl_path, Db_path) ->
    gleam@result:'try'(
        list_sessions_from_jsonl(Jsonl_path),
        fun(Sessions) ->
            _pipe = gleam@list:fold(
                Sessions,
                {ok, nil},
                fun(Acc, Session) ->
                    gleam@result:'try'(
                        Acc,
                        fun(_) -> save_session_to_db(Db_path, Session) end
                    )
                end
            ),
            gleam@result:map(_pipe, fun(_) -> Sessions end)
        end
    ).
