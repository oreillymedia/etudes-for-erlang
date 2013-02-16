%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Recursive function for calculating cube root
%% of a number using Newton's method.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(roots).
-export([cuberoot/1, cube2/1]).

%% @doc Calculates the cube root of a number.`
%% Uses Newton's method to make approximated guesses until
%% the difference between current and previous guess
%% is less than a limit (in this case, .000001).

-spec(cuberoot(number()) -> number()).

cuberoot(X) -> cuberoot(X, X / 2.0).

cuberoot(X, G) ->
	io:format("Current guess is ~p~n", [G]),
	Next = G - ((G * G * G) - X) / (3 * (G * G)),
	case abs(G - Next) < 1.0e-8 of
	  true -> Next;
	  false -> cuberoot(X, Next)
	end.

cube2(N) -> cube2(N, N / 2.0).

cube2(N, X) ->
	io:format("Current guess is ~p~n", [X]),
	NextX = X - ((X * X * X) - N) / (3 * (X * X)),
	if
	  (X - NextX) < 0.000001 -> nextX;
	  true -> cube2(N, NextX)
	end.

