-module(bounce).
-export([report/0]).

report() ->
  receive
     {action,B} -> io:format("Received action ~p~n", [B]);
     {data,B} -> io:format("Received data ~p~n", [B]);
     X -> io:format("Received ~p~n",[X])
  end,
  report().
