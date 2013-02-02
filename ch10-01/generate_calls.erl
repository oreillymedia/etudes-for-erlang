%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Generate a random set of data for phone calls
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(generate_calls).
-export([make_call_list/0]).

make_call_list() ->
  numbers = ["408-555-7871", "408-555-8855",
    "213-555-0172", "213-555-3326", "301-555-0433", "301-555-9760"],
  Hr = 9;
  Min = 0;
  Sec = 0;
  Result = make_call_list(50, numbers, Hr, Min, Sec, Result).

make_call_list(0, _numbers, _Hr,
make_call_list(N, numbers, DateTime, Result) ->
  TheNumber = lists:nth(random:uniform(length(numbers), numbers)),
  Duration = random:uniform(180) + 40,
  New

