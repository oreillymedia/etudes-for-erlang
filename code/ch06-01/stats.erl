%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for calculating basic statistics on a list of numbers.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(stats).
-export([minimum/1]).

%% @doc Returns the minimum item in a list of numbers. Fails when given
%% an empty list, as there's nothing reasonable to return.

-spec(minimum(list(number())) -> number()).

minimum(NumberList) ->
  [Result | Rest] = Numbers,
  minimum(Rest, Result).

minimum([], Result) -> Result;

minimum([Head|Tail], Result) ->
  case Head < Result of
    true -> minimum(Tail, Head);
    false -> minimum(Tail, Result)
  end.

