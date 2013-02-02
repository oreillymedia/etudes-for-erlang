-module(testor).
-export([test/1]).

test(X) ->
  case X of
    _ when X < 3 -> io:format("less than 3~n");
    _ when X == 3 -> io:format("exactly 3~n");
    _ -> io:format("Something else")
  end.
     

