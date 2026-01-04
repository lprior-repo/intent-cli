-module(intent@calendar).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/calendar.gleam").
-export([parse_time/1, is_leap_year/1, days_in_month/2, parse_date/1, format_date/1, format_time/1, format_datetime/1, compare_dates/2, compare_times/2, day_of_week/1, day_name/1, month_name/1]).
-export_type([date/0, time/0, date_time/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type date() :: {date, integer(), integer(), integer()}.

-type time() :: {time, integer(), integer(), integer()}.

-type date_time() :: {date_time, date(), time()}.

-file("src/intent/calendar.gleam", 87).
?DOC(" Check if time is valid\n").
-spec is_valid_time(integer(), integer(), integer()) -> boolean().
is_valid_time(Hour, Minute, Second) ->
    (((((Hour >= 0) andalso (Hour < 24)) andalso (Minute >= 0)) andalso (Minute
    < 60))
    andalso (Second >= 0))
    andalso (Second < 60).

-file("src/intent/calendar.gleam", 50).
?DOC(" Parse ISO 8601 time (HH:MM:SS)\n").
-spec parse_time(binary()) -> {ok, time()} | {error, binary()}.
parse_time(Time_str) ->
    Parts = gleam@string:split(Time_str, <<":"/utf8>>),
    case Parts of
        [Hour_str, Minute_str, Second_str] ->
            case gleam@int:parse(Hour_str) of
                {ok, Hour} ->
                    case gleam@int:parse(Minute_str) of
                        {ok, Minute} ->
                            case gleam@int:parse(Second_str) of
                                {ok, Second} ->
                                    case is_valid_time(Hour, Minute, Second) of
                                        true ->
                                            {ok, {time, Hour, Minute, Second}};

                                        false ->
                                            {error,
                                                <<"Invalid time: "/utf8,
                                                    Time_str/binary>>}
                                    end;

                                {error, _} ->
                                    {error,
                                        <<"Invalid time format: "/utf8,
                                            Time_str/binary>>}
                            end;

                        {error, _} ->
                            {error,
                                <<"Invalid time format: "/utf8,
                                    Time_str/binary>>}
                    end;

                {error, _} ->
                    {error, <<"Invalid time format: "/utf8, Time_str/binary>>}
            end;

        _ ->
            {error, <<"Time must be HH:MM:SS format"/utf8>>}
    end.

-file("src/intent/calendar.gleam", 106).
?DOC(" Check if a year is a leap year\n").
-spec is_leap_year(integer()) -> boolean().
is_leap_year(Year) ->
    case (Year rem 400) =:= 0 of
        true ->
            true;

        false ->
            case (Year rem 100) =:= 0 of
                true ->
                    false;

                false ->
                    (Year rem 4) =:= 0
            end
    end.

-file("src/intent/calendar.gleam", 93).
?DOC(" Get number of days in a given month (accounting for leap years)\n").
-spec days_in_month(integer(), integer()) -> integer().
days_in_month(Year, Month) ->
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
            case is_leap_year(Year) of
                true ->
                    29;

                false ->
                    28
            end;

        _ ->
            0
    end.

-file("src/intent/calendar.gleam", 76).
?DOC(" Check if date is valid (accounting for leap years and month boundaries)\n").
-spec is_valid_date(integer(), integer(), integer()) -> boolean().
is_valid_date(Year, Month, Day) ->
    case (Month >= 1) andalso (Month =< 12) of
        false ->
            false;

        true ->
            Max_day = days_in_month(Year, Month),
            (Day >= 1) andalso (Day =< Max_day)
    end.

-file("src/intent/calendar.gleam", 24).
?DOC(" Parse ISO 8601 date (YYYY-MM-DD)\n").
-spec parse_date(binary()) -> {ok, date()} | {error, binary()}.
parse_date(Date_str) ->
    Parts = gleam@string:split(Date_str, <<"-"/utf8>>),
    case Parts of
        [Year_str, Month_str, Day_str] ->
            case gleam@int:parse(Year_str) of
                {ok, Year} ->
                    case gleam@int:parse(Month_str) of
                        {ok, Month} ->
                            case gleam@int:parse(Day_str) of
                                {ok, Day} ->
                                    case is_valid_date(Year, Month, Day) of
                                        true ->
                                            {ok, {date, Year, Month, Day}};

                                        false ->
                                            {error,
                                                <<"Invalid date: "/utf8,
                                                    Date_str/binary>>}
                                    end;

                                {error, _} ->
                                    {error,
                                        <<"Invalid date format: "/utf8,
                                            Date_str/binary>>}
                            end;

                        {error, _} ->
                            {error,
                                <<"Invalid date format: "/utf8,
                                    Date_str/binary>>}
                    end;

                {error, _} ->
                    {error, <<"Invalid date format: "/utf8, Date_str/binary>>}
            end;

        _ ->
            {error, <<"Date must be YYYY-MM-DD format"/utf8>>}
    end.

-file("src/intent/calendar.gleam", 141).
?DOC(" Pad integer with zeros to specified width\n").
-spec pad_int(integer(), integer()) -> binary().
pad_int(Num, Width) ->
    Str = gleam@int:to_string(Num),
    Padding = Width - gleam@string:length(Str),
    case Padding > 0 of
        true ->
            <<(gleam@string:repeat(<<"0"/utf8>>, Padding))/binary, Str/binary>>;

        false ->
            Str
    end.

-file("src/intent/calendar.gleam", 118).
?DOC(" Format date as ISO 8601 (YYYY-MM-DD)\n").
-spec format_date(date()) -> binary().
format_date(Date) ->
    <<<<<<<<(gleam@int:to_string(erlang:element(2, Date)))/binary, "-"/utf8>>/binary,
                (pad_int(erlang:element(3, Date), 2))/binary>>/binary,
            "-"/utf8>>/binary,
        (pad_int(erlang:element(4, Date), 2))/binary>>.

-file("src/intent/calendar.gleam", 127).
?DOC(" Format time as ISO 8601 (HH:MM:SS)\n").
-spec format_time(time()) -> binary().
format_time(Time) ->
    <<<<<<<<(pad_int(erlang:element(2, Time), 2))/binary, ":"/utf8>>/binary,
                (pad_int(erlang:element(3, Time), 2))/binary>>/binary,
            ":"/utf8>>/binary,
        (pad_int(erlang:element(4, Time), 2))/binary>>.

-file("src/intent/calendar.gleam", 136).
?DOC(" Format datetime as ISO 8601 (YYYY-MM-DDTHH:MM:SS)\n").
-spec format_datetime(date_time()) -> binary().
format_datetime(Dt) ->
    <<<<(format_date(erlang:element(2, Dt)))/binary, "T"/utf8>>/binary,
        (format_time(erlang:element(3, Dt)))/binary>>.

-file("src/intent/calendar.gleam", 151).
?DOC(" Compare two dates\n").
-spec compare_dates(date(), date()) -> gleam@order:order().
compare_dates(A, B) ->
    case gleam@int:compare(erlang:element(2, A), erlang:element(2, B)) of
        lt ->
            lt;

        gt ->
            gt;

        eq ->
            case gleam@int:compare(erlang:element(3, A), erlang:element(3, B)) of
                lt ->
                    lt;

                gt ->
                    gt;

                eq ->
                    gleam@int:compare(
                        erlang:element(4, A),
                        erlang:element(4, B)
                    )
            end
    end.

-file("src/intent/calendar.gleam", 165).
?DOC(" Compare two times\n").
-spec compare_times(time(), time()) -> gleam@order:order().
compare_times(A, B) ->
    case gleam@int:compare(erlang:element(2, A), erlang:element(2, B)) of
        lt ->
            lt;

        gt ->
            gt;

        eq ->
            case gleam@int:compare(erlang:element(3, A), erlang:element(3, B)) of
                lt ->
                    lt;

                gt ->
                    gt;

                eq ->
                    gleam@int:compare(
                        erlang:element(4, A),
                        erlang:element(4, B)
                    )
            end
    end.

-file("src/intent/calendar.gleam", 179).
?DOC(" Get day of week (0=Sunday, 6=Saturday)\n").
-spec day_of_week(date()) -> integer().
day_of_week(Date) ->
    Year = case erlang:element(3, Date) < 3 of
        true ->
            erlang:element(2, Date) - 1;

        false ->
            erlang:element(2, Date)
    end,
    Month = case erlang:element(3, Date) < 3 of
        true ->
            erlang:element(3, Date) + 12;

        false ->
            erlang:element(3, Date)
    end,
    K = Year rem 100,
    J = Year div 100,
    Month_calc = (13 * (Month + 1)) div 5,
    K_calc = K div 4,
    J_calc = J div 4,
    H = ((((erlang:element(4, Date) + Month_calc) + K) + K_calc) + J_calc) - (2
    * J),
    Day = H rem 7,
    case Day of
        -6 ->
            1;

        -5 ->
            2;

        -4 ->
            3;

        -3 ->
            4;

        -2 ->
            5;

        -1 ->
            6;

        0 ->
            0;

        _ ->
            Day rem 7
    end.

-file("src/intent/calendar.gleam", 212).
?DOC(" Get day name\n").
-spec day_name(integer()) -> binary().
day_name(Day_num) ->
    case Day_num rem 7 of
        0 ->
            <<"Sunday"/utf8>>;

        1 ->
            <<"Monday"/utf8>>;

        2 ->
            <<"Tuesday"/utf8>>;

        3 ->
            <<"Wednesday"/utf8>>;

        4 ->
            <<"Thursday"/utf8>>;

        5 ->
            <<"Friday"/utf8>>;

        6 ->
            <<"Saturday"/utf8>>;

        _ ->
            <<"Unknown"/utf8>>
    end.

-file("src/intent/calendar.gleam", 226).
?DOC(" Get month name\n").
-spec month_name(integer()) -> binary().
month_name(Month_num) ->
    case Month_num of
        1 ->
            <<"January"/utf8>>;

        2 ->
            <<"February"/utf8>>;

        3 ->
            <<"March"/utf8>>;

        4 ->
            <<"April"/utf8>>;

        5 ->
            <<"May"/utf8>>;

        6 ->
            <<"June"/utf8>>;

        7 ->
            <<"July"/utf8>>;

        8 ->
            <<"August"/utf8>>;

        9 ->
            <<"September"/utf8>>;

        10 ->
            <<"October"/utf8>>;

        11 ->
            <<"November"/utf8>>;

        12 ->
            <<"December"/utf8>>;

        _ ->
            <<"Unknown"/utf8>>
    end.
