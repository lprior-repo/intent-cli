-module(intent@formats).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/formats.gleam").
-export([validate_email/1, validate_uuid/1, validate_uri/1, validate_iso8601/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/intent/formats.gleam", 67).
?DOC(" Check if email local part contains only valid characters\n").
-spec is_valid_email_local_chars(binary()) -> boolean().
is_valid_email_local_chars(S) ->
    _pipe = gleam@string:to_graphemes(S),
    gleam@list:all(_pipe, fun(C) -> case C of
                <<"."/utf8>> ->
                    true;

                <<"-"/utf8>> ->
                    true;

                <<"_"/utf8>> ->
                    true;

                <<"+"/utf8>> ->
                    true;

                _ ->
                    case gleam_stdlib:contains_string(
                        <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"/utf8>>,
                        C
                    ) of
                        true ->
                            true;

                        false ->
                            false
                    end
            end end).

-file("src/intent/formats.gleam", 32).
?DOC(" Validate email local part (before @)\n").
-spec validate_email_local(binary()) -> {ok, nil} | {error, binary()}.
validate_email_local(Local) ->
    case gleam@string:is_empty(Local) of
        true ->
            {error, <<"Email local part cannot be empty"/utf8>>};

        false ->
            case gleam_stdlib:contains_string(Local, <<".."/utf8>>) of
                true ->
                    {error,
                        <<"Email local part cannot contain consecutive dots"/utf8>>};

                false ->
                    case gleam@string:starts_with(Local, <<"."/utf8>>) orelse gleam@string:ends_with(
                        Local,
                        <<"."/utf8>>
                    ) of
                        true ->
                            {error,
                                <<"Email local part cannot start or end with a dot"/utf8>>};

                        false ->
                            Valid = is_valid_email_local_chars(Local),
                            case Valid of
                                true ->
                                    {ok, nil};

                                false ->
                                    {error,
                                        <<"Email local part contains invalid characters: "/utf8,
                                            Local/binary>>}
                            end
                    end
            end
    end.

-file("src/intent/formats.gleam", 109).
?DOC(" Check if a domain label is valid (alphanumeric and hyphens, not starting/ending with hyphen)\n").
-spec is_valid_domain_label(binary()) -> boolean().
is_valid_domain_label(Label) ->
    case gleam@string:length(Label) of
        0 ->
            false;

        _ ->
            Starts_with_hyphen = gleam@string:starts_with(Label, <<"-"/utf8>>),
            Ends_with_hyphen = gleam@string:ends_with(Label, <<"-"/utf8>>),
            All_chars_valid = begin
                _pipe = gleam@string:to_graphemes(Label),
                gleam@list:all(
                    _pipe,
                    fun(C) ->
                        case gleam_stdlib:contains_string(
                            <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"/utf8>>,
                            C
                        ) of
                            true ->
                                true;

                            false ->
                                false
                        end
                    end
                )
            end,
            (not Starts_with_hyphen andalso not Ends_with_hyphen) andalso All_chars_valid
    end.

-file("src/intent/formats.gleam", 82).
?DOC(" Validate email domain part (after @)\n").
-spec validate_email_domain(binary()) -> {ok, nil} | {error, binary()}.
validate_email_domain(Domain) ->
    case gleam@string:is_empty(Domain) of
        true ->
            {error, <<"Email domain cannot be empty"/utf8>>};

        false ->
            case gleam_stdlib:contains_string(Domain, <<"."/utf8>>) of
                false ->
                    {error,
                        <<"Email domain must contain at least one dot"/utf8>>};

                true ->
                    Labels = gleam@string:split(Domain, <<"."/utf8>>),
                    case gleam@list:any(Labels, fun gleam@string:is_empty/1) of
                        true ->
                            {error,
                                <<"Email domain contains empty label (consecutive or trailing dots)"/utf8>>};

                        false ->
                            case gleam@list:all(
                                Labels,
                                fun is_valid_domain_label/1
                            ) of
                                true ->
                                    {ok, nil};

                                false ->
                                    {error,
                                        <<"Email domain contains invalid labels: "/utf8,
                                            Domain/binary>>}
                            end
                    end
            end
    end.

-file("src/intent/formats.gleam", 10).
?DOC(
    " Validate email using RFC 5322 compliant parsing\n"
    " This is stricter than simple regex - validates local and domain parts\n"
).
-spec validate_email(binary()) -> {ok, nil} | {error, binary()}.
validate_email(Email) ->
    Parts = gleam@string:split(Email, <<"@"/utf8>>),
    case Parts of
        [Local, Domain] ->
            case validate_email_local(Local) of
                {error, E} ->
                    {error, E};

                {ok, _} ->
                    case validate_email_domain(Domain) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, _} ->
                            {ok, nil}
                    end
            end;

        _ ->
            {error,
                <<<<"'"/utf8, Email/binary>>/binary,
                    "' is not a valid email address (invalid @ format)"/utf8>>}
    end.

-file("src/intent/formats.gleam", 197).
?DOC(" Check if a string contains only valid hexadecimal characters\n").
-spec is_valid_hex(binary()) -> boolean().
is_valid_hex(S) ->
    _pipe = gleam@string:to_graphemes(S),
    gleam@list:all(
        _pipe,
        fun(C) ->
            case gleam_stdlib:contains_string(
                <<"0123456789abcdefABCDEF"/utf8>>,
                C
            ) of
                true ->
                    true;

                false ->
                    false
            end
        end
    ).

-file("src/intent/formats.gleam", 131).
?DOC(
    " Validate UUID format and structure\n"
    " Validates version (1-5) and variant (RFC 4122) bits\n"
).
-spec validate_uuid(binary()) -> {ok, nil} | {error, binary()}.
validate_uuid(Uuid) ->
    Parts = gleam@string:split(Uuid, <<"-"/utf8>>),
    case Parts of
        [Time_low, Time_mid, Time_high_version, Clock_seq_hi_variant, Node] ->
            case ((((gleam@string:length(Time_low) =:= 8) andalso (gleam@string:length(
                Time_mid
            )
            =:= 4))
            andalso (gleam@string:length(Time_high_version) =:= 4))
            andalso (gleam@string:length(Clock_seq_hi_variant) =:= 4))
            andalso (gleam@string:length(Node) =:= 12) of
                false ->
                    {error,
                        <<<<"'"/utf8, Uuid/binary>>/binary,
                            "' has invalid UUID segment lengths (expected 8-4-4-4-12)"/utf8>>};

                true ->
                    case (((is_valid_hex(Time_low) andalso is_valid_hex(
                        Time_mid
                    ))
                    andalso is_valid_hex(Time_high_version))
                    andalso is_valid_hex(Clock_seq_hi_variant))
                    andalso is_valid_hex(Node) of
                        false ->
                            {error,
                                <<<<"'"/utf8, Uuid/binary>>/binary,
                                    "' contains non-hexadecimal characters"/utf8>>};

                        true ->
                            Version_char = gleam@string:slice(
                                Time_high_version,
                                0,
                                1
                            ),
                            case gleam_stdlib:contains_string(
                                <<"12345"/utf8>>,
                                Version_char
                            ) of
                                false ->
                                    {error,
                                        <<<<<<<<"'"/utf8, Uuid/binary>>/binary,
                                                    "' has invalid UUID version (expected 1-5, got "/utf8>>/binary,
                                                Version_char/binary>>/binary,
                                            ")"/utf8>>};

                                true ->
                                    Variant_char = gleam@string:slice(
                                        Clock_seq_hi_variant,
                                        0,
                                        1
                                    ),
                                    case gleam_stdlib:contains_string(
                                        <<"89abAB"/utf8>>,
                                        Variant_char
                                    ) of
                                        false ->
                                            {error,
                                                <<<<"'"/utf8, Uuid/binary>>/binary,
                                                    "' has invalid RFC 4122 variant (expected 8,9,a,b variant bits)"/utf8>>};

                                        true ->
                                            {ok, nil}
                                    end
                            end
                    end
            end;

        _ ->
            {error,
                <<<<"'"/utf8, Uuid/binary>>/binary,
                    "' is not a valid UUID (invalid segment count)"/utf8>>}
    end.

-file("src/intent/formats.gleam", 246).
?DOC(" Validate URI scheme (must start with letter, contain only alphanumeric, +, -, .)\n").
-spec validate_uri_scheme(binary()) -> {ok, nil} | {error, binary()}.
validate_uri_scheme(Scheme) ->
    case gleam@string:is_empty(Scheme) of
        true ->
            {error, <<"URI scheme cannot be empty"/utf8>>};

        false ->
            First_char = gleam@string:slice(Scheme, 0, 1),
            Is_letter = case First_char of
                <<"a"/utf8>> ->
                    true;

                <<"b"/utf8>> ->
                    true;

                <<"c"/utf8>> ->
                    true;

                <<"d"/utf8>> ->
                    true;

                <<"e"/utf8>> ->
                    true;

                <<"f"/utf8>> ->
                    true;

                <<"g"/utf8>> ->
                    true;

                <<"h"/utf8>> ->
                    true;

                <<"i"/utf8>> ->
                    true;

                <<"j"/utf8>> ->
                    true;

                <<"k"/utf8>> ->
                    true;

                <<"l"/utf8>> ->
                    true;

                <<"m"/utf8>> ->
                    true;

                <<"n"/utf8>> ->
                    true;

                <<"o"/utf8>> ->
                    true;

                <<"p"/utf8>> ->
                    true;

                <<"q"/utf8>> ->
                    true;

                <<"r"/utf8>> ->
                    true;

                <<"s"/utf8>> ->
                    true;

                <<"t"/utf8>> ->
                    true;

                <<"u"/utf8>> ->
                    true;

                <<"v"/utf8>> ->
                    true;

                <<"w"/utf8>> ->
                    true;

                <<"x"/utf8>> ->
                    true;

                <<"y"/utf8>> ->
                    true;

                <<"z"/utf8>> ->
                    true;

                <<"A"/utf8>> ->
                    true;

                <<"B"/utf8>> ->
                    true;

                <<"C"/utf8>> ->
                    true;

                <<"D"/utf8>> ->
                    true;

                <<"E"/utf8>> ->
                    true;

                <<"F"/utf8>> ->
                    true;

                <<"G"/utf8>> ->
                    true;

                <<"H"/utf8>> ->
                    true;

                <<"I"/utf8>> ->
                    true;

                <<"J"/utf8>> ->
                    true;

                <<"K"/utf8>> ->
                    true;

                <<"L"/utf8>> ->
                    true;

                <<"M"/utf8>> ->
                    true;

                <<"N"/utf8>> ->
                    true;

                <<"O"/utf8>> ->
                    true;

                <<"P"/utf8>> ->
                    true;

                <<"Q"/utf8>> ->
                    true;

                <<"R"/utf8>> ->
                    true;

                <<"S"/utf8>> ->
                    true;

                <<"T"/utf8>> ->
                    true;

                <<"U"/utf8>> ->
                    true;

                <<"V"/utf8>> ->
                    true;

                <<"W"/utf8>> ->
                    true;

                <<"X"/utf8>> ->
                    true;

                <<"Y"/utf8>> ->
                    true;

                <<"Z"/utf8>> ->
                    true;

                _ ->
                    false
            end,
            case Is_letter of
                false ->
                    {error, <<"URI scheme must start with a letter"/utf8>>};

                true ->
                    Rest = gleam@string:slice(
                        Scheme,
                        1,
                        gleam@string:length(Scheme)
                    ),
                    Valid_chars = begin
                        _pipe = gleam@string:to_graphemes(Rest),
                        gleam@list:all(
                            _pipe,
                            fun(C) ->
                                case gleam_stdlib:contains_string(
                                    <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-.
"/utf8>>,
                                    C
                                ) of
                                    true ->
                                        true;

                                    false ->
                                        false
                                end
                            end
                        )
                    end,
                    case Valid_chars of
                        false ->
                            {error,
                                <<"URI scheme contains invalid characters: "/utf8,
                                    Scheme/binary>>};

                        true ->
                            {ok, nil}
                    end
            end
    end.

-file("src/intent/formats.gleam", 209).
?DOC(
    " Validate URI following RFC 3986\n"
    " Checks for valid scheme, authority, path, query, and fragment\n"
).
-spec validate_uri(binary()) -> {ok, nil} | {error, binary()}.
validate_uri(Uri) ->
    case gleam@string:is_empty(Uri) of
        true ->
            {error, <<"URI cannot be empty"/utf8>>};

        false ->
            Has_scheme_sep = gleam_stdlib:contains_string(Uri, <<"://"/utf8>>),
            case Has_scheme_sep of
                false ->
                    {error,
                        <<<<"'"/utf8, Uri/binary>>/binary,
                            "' is not a valid URI (missing scheme)"/utf8>>};

                true ->
                    case gleam@string:split_once(Uri, <<"://"/utf8>>) of
                        {error, _} ->
                            {error,
                                <<<<"'"/utf8, Uri/binary>>/binary,
                                    "' is not a valid URI (malformed scheme)"/utf8>>};

                        {ok, {Scheme, Rest}} ->
                            case validate_uri_scheme(Scheme) of
                                {error, E} ->
                                    {error, E};

                                {ok, _} ->
                                    case gleam@string:is_empty(Rest) of
                                        true ->
                                            {error,
                                                <<<<"'"/utf8, Uri/binary>>/binary,
                                                    "' has no authority after scheme"/utf8>>};

                                        false ->
                                            {ok, nil}
                                    end
                            end
                    end
            end
    end.

-file("src/intent/formats.gleam", 477).
?DOC(" Parse a string to integer\n").
-spec parse_int(binary()) -> {ok, integer()} | {error, nil}.
parse_int(S) ->
    case gleam@int:parse(S) of
        {ok, N} ->
            {ok, N};

        {error, _} ->
            {error, nil}
    end.

-file("src/intent/formats.gleam", 386).
?DOC(" Validate ISO8601 time part (HH:MM:SS with optional fractional seconds and timezone)\n").
-spec validate_iso8601_time(binary()) -> {ok, nil} | {error, binary()}.
validate_iso8601_time(Time_str) ->
    Time_without_tz = case gleam@string:split_once(Time_str, <<"Z"/utf8>>) of
        {ok, {T, _}} ->
            T;

        {error, _} ->
            case gleam@string:split_once(Time_str, <<"+"/utf8>>) of
                {ok, {T@1, _}} ->
                    T@1;

                {error, _} ->
                    case gleam@string:split_once(Time_str, <<"-"/utf8>>) of
                        {ok, {T@2, _}} ->
                            T@2;

                        {error, _} ->
                            Time_str
                    end
            end
    end,
    Parts = gleam@string:split(Time_without_tz, <<":"/utf8>>),
    case Parts of
        [Hour_str, Minute_str, Second_and_frac] ->
            case {parse_int(Hour_str), parse_int(Minute_str)} of
                {{error, _}, _} ->
                    {error,
                        <<"Invalid ISO8601 time: hour must be a number, got "/utf8,
                            Hour_str/binary>>};

                {_, {error, _}} ->
                    {error,
                        <<"Invalid ISO8601 time: minute must be a number, got "/utf8,
                            Minute_str/binary>>};

                {{ok, Hour}, {ok, Minute}} ->
                    case Hour of
                        H when (H < 0) orelse (H > 23) ->
                            {error,
                                <<"Invalid ISO8601 time: hour must be 00-23, got "/utf8,
                                    Hour_str/binary>>};

                        _ ->
                            case Minute of
                                M when (M < 0) orelse (M > 59) ->
                                    {error,
                                        <<"Invalid ISO8601 time: minute must be 00-59, got "/utf8,
                                            Minute_str/binary>>};

                                _ ->
                                    Sec_parts = gleam@string:split(
                                        Second_and_frac,
                                        <<"."/utf8>>
                                    ),
                                    case Sec_parts of
                                        [Sec_str] ->
                                            case parse_int(Sec_str) of
                                                {error, _} ->
                                                    {error,
                                                        <<"Invalid ISO8601 time: second must be a number, got "/utf8,
                                                            Sec_str/binary>>};

                                                {ok, Sec} ->
                                                    case Sec of
                                                        S when (S < 0) orelse (S > 59) ->
                                                            {error,
                                                                <<"Invalid ISO8601 time: second must be 00-59, got "/utf8,
                                                                    Sec_str/binary>>};

                                                        _ ->
                                                            {ok, nil}
                                                    end
                                            end;

                                        [Sec_str@1, _] ->
                                            case parse_int(Sec_str@1) of
                                                {error, _} ->
                                                    {error,
                                                        <<"Invalid ISO8601 time: second must be a number, got "/utf8,
                                                            Sec_str@1/binary>>};

                                                {ok, Sec@1} ->
                                                    case Sec@1 of
                                                        S@1 when (S@1 < 0) orelse (S@1 > 59) ->
                                                            {error,
                                                                <<"Invalid ISO8601 time: second must be 00-59, got "/utf8,
                                                                    Sec_str@1/binary>>};

                                                        _ ->
                                                            {ok, nil}
                                                    end
                                            end;

                                        _ ->
                                            {error,
                                                <<"Invalid ISO8601 time format"/utf8>>}
                                    end
                            end
                    end
            end;

        _ ->
            {error, <<"Invalid ISO8601 time format (expected HH:MM:SS)"/utf8>>}
    end.

-file("src/intent/formats.gleam", 485).
?DOC(" Check if a year is a leap year\n").
-spec is_leap_year(integer()) -> boolean().
is_leap_year(Year) ->
    case Year rem 4 of
        0 ->
            case Year rem 100 of
                0 ->
                    case Year rem 400 of
                        0 ->
                            true;

                        _ ->
                            false
                    end;

                _ ->
                    true
            end;

        _ ->
            false
    end.

-file("src/intent/formats.gleam", 501).
?DOC(" Get the number of days in a month\n").
-spec get_days_in_month(integer(), boolean()) -> integer().
get_days_in_month(Month, Is_leap) ->
    case Month of
        1 ->
            31;

        3 ->
            31;

        5 ->
            31;

        7 ->
            31;

        8 ->
            31;

        10 ->
            31;

        12 ->
            31;

        4 ->
            30;

        6 ->
            30;

        9 ->
            30;

        11 ->
            30;

        2 ->
            case Is_leap of
                true ->
                    29;

                false ->
                    28
            end;

        _ ->
            0
    end.

-file("src/intent/formats.gleam", 515).
?DOC(" Convert integer to string (for error messages)\n").
-spec int_to_string(integer()) -> binary().
int_to_string(N) ->
    gleam@int:to_string(N).

-file("src/intent/formats.gleam", 330).
?DOC(" Validate ISO8601 date (YYYY-MM-DD) with proper calendar validation\n").
-spec validate_iso8601_date(binary()) -> {ok, nil} | {error, binary()}.
validate_iso8601_date(Date_str) ->
    case gleam@string:length(Date_str) of
        10 ->
            Parts = gleam@string:split(Date_str, <<"-"/utf8>>),
            case Parts of
                [Year_str, Month_str, Day_str] ->
                    case {parse_int(Year_str),
                        parse_int(Month_str),
                        parse_int(Day_str)} of
                        {{error, _}, _, _} ->
                            {error,
                                <<<<"'"/utf8, Date_str/binary>>/binary,
                                    "' has invalid year (not a number)"/utf8>>};

                        {_, {error, _}, _} ->
                            {error,
                                <<<<"'"/utf8, Date_str/binary>>/binary,
                                    "' has invalid month (not a number)"/utf8>>};

                        {_, _, {error, _}} ->
                            {error,
                                <<<<"'"/utf8, Date_str/binary>>/binary,
                                    "' has invalid day (not a number)"/utf8>>};

                        {{ok, Year}, {ok, Month}, {ok, Day}} ->
                            case Month of
                                M when (M < 1) orelse (M > 12) ->
                                    {error,
                                        <<<<<<<<"'"/utf8, Date_str/binary>>/binary,
                                                    "' has invalid month: "/utf8>>/binary,
                                                Month_str/binary>>/binary,
                                            " (must be 01-12)"/utf8>>};

                                _ ->
                                    Max_day = get_days_in_month(
                                        Month,
                                        is_leap_year(Year)
                                    ),
                                    case Day of
                                        D when (D < 1) orelse (D > Max_day) ->
                                            {error,
                                                <<<<<<<<<<<<<<<<"'"/utf8,
                                                                                Date_str/binary>>/binary,
                                                                            "' has invalid day: "/utf8>>/binary,
                                                                        Day_str/binary>>/binary,
                                                                    " (month "/utf8>>/binary,
                                                                Month_str/binary>>/binary,
                                                            " has max "/utf8>>/binary,
                                                        (int_to_string(Max_day))/binary>>/binary,
                                                    " days)"/utf8>>};

                                        _ ->
                                            {ok, nil}
                                    end
                            end
                    end;

                _ ->
                    {error,
                        <<<<"'"/utf8, Date_str/binary>>/binary,
                            "' is not valid ISO8601 date format"/utf8>>}
            end;

        _ ->
            {error,
                <<<<"'"/utf8, Date_str/binary>>/binary,
                    "' is not valid ISO8601 date format (expected YYYY-MM-DD)"/utf8>>}
    end.

-file("src/intent/formats.gleam", 291).
?DOC(
    " Validate ISO 8601 datetime format with actual calendar validation\n"
    " Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, with optional timezone\n"
).
-spec validate_iso8601(binary()) -> {ok, nil} | {error, binary()}.
validate_iso8601(Datetime) ->
    case gleam@string:length(Datetime) of
        Len when Len < 10 ->
            {error,
                <<<<"'"/utf8, Datetime/binary>>/binary,
                    "' is not a valid ISO8601 datetime (too short)"/utf8>>};

        _ ->
            Date_part = gleam@string:slice(Datetime, 0, 10),
            case validate_iso8601_date(Date_part) of
                {error, E} ->
                    {error, E};

                {ok, _} ->
                    case gleam@string:length(Datetime) of
                        10 ->
                            {ok, nil};

                        _ ->
                            Time_sep = gleam@string:slice(Datetime, 10, 1),
                            case Time_sep of
                                <<"T"/utf8>> ->
                                    Time_part = gleam@string:slice(
                                        Datetime,
                                        11,
                                        gleam@string:length(Datetime)
                                    ),
                                    validate_iso8601_time(Time_part);

                                <<" "/utf8>> ->
                                    Time_part = gleam@string:slice(
                                        Datetime,
                                        11,
                                        gleam@string:length(Datetime)
                                    ),
                                    validate_iso8601_time(Time_part);

                                _ ->
                                    {error,
                                        <<<<"'"/utf8, Datetime/binary>>/binary,
                                            "' is not a valid ISO8601 datetime (invalid separator, expected T or space)"/utf8>>}
                            end
                    end
            end
    end.
