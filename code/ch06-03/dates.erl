%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for splitting a date into a list of
%% year-month-day and finding Julian date.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(dates).
-export([date_parts/1, julian/1, is_leap_year/1]).

%% @doc Takes a string in ISO date format (yyyy-mm-dd) and
%% returns a list of integers in form [year, month, day].

-spec(date_parts(string()) -> list(integer())).

date_parts(DateStr) ->
  [YStr, MStr, DStr] = re:split(DateStr, "-", [{return, list}]),
  [element(1, string:to_integer(YStr)),
    element(1, string:to_integer(MStr)),
    element(1, string:to_integer(DStr))].

%% @doc Takes a string in ISO date format (yyyy-mm-dd) and
%% returns the day of the year (Julian date).

-spec(julian(string()) -> pos_integer()).

julian(IsoDate) ->
  [Y, M, D] = date_parts(IsoDate),
  DaysPerMonth = days_per_month(is_leap_year(Y)),
  julian(M, D, DaysPerMonth, 0).

%% @doc Helper function that recursively accumulates the number of days
%% up to the specified date.

-spec(julian(integer(), integer(), [integer()], integer()) -> integer()).

julian(1, Day, [H|_T], _Tally) when H < Day ->
  throw(illegal_day_in_month);
julian(1, Day, _MonthsList, Tally) ->
  Tally + Day;
julian(Month, Day, [H|T], Tally) ->
  julian(Month - 1, Day, T, Tally + H).

%% @doc Given a year, return true or false depending on whether
%% the year is a leap year.

-spec(is_leap_year(pos_integer()) -> boolean()).

is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
    orelse (Year rem 400 == 0).

%% @doc Given a boolean value indicating leap year, return a list of
%% day counts for each month in the year.

-spec(days_per_month(boolean()) -> list(integer())).

days_per_month(false) ->
  [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
days_per_month(true) ->
  [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31].
