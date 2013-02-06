%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Read in a database of phone calls
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(phone_mnesia).
-export([setup/2, summary/0, summary/1, invoice/1]).
-include("phone_records.hrl").

-spec(setup(string(), string()) -> atom()).
setup(PhoneFileName, CustomerFileName) ->

  mnesia:create_schema([node()]),
  mnesia:start(),
  
  %% Fill table only when actually created
  {Result, _Message} = mnesia:create_table(phone_call,
    [{attributes, record_info(fields, phone_call)}, {type, bag}]),
  case Result of
    atomic ->
      io:format("Created table. about to fill~n"),
      fill_table(PhoneFileName, fun read_phone_call/1);
    aborted -> error
  end,
  ok.
%%  {Result2, Message2} = mnesia:create_table(customer,
%%    [{attributes, record_info(fields, customer)}]),
%%  case Result2 of
%%    atomic ->
%%      fill_table(customers, CustomerFileName, fun read_customer/1),
%%    aborted -> error
%%  end.
   
fill_table(InputFileName, ReadFn) ->
  {Result, InputFile} = file:open(InputFileName, [read]),
  io:format("Result of opening ~s is ~p~n", [InputFileName, Result]),
  case Result of
    ok -> TransResult = mnesia:transaction(fun() -> ReadFn(InputFile) end),
      io:format("Transaction result ~p~n", [TransResult]);
    _ -> io:format("Error opening file: ~p~n", [InputFileName])
  end.


-spec(read_phone_call(file:io_device()) -> atom()).
%% Read a line from the input file, and insert its contents into
%% the call_table. This function is called recursively until end of file
read_phone_call(InputFile) ->
  RawData = io:get_line(InputFile, ""),
  if
    is_list(RawData) ->
      Data = string:strip(RawData, right, $\n),
      [Number, SDate, STime, EDate, ETime] =
        re:split(Data, ",", [{return, list}]),
      io:format("Data: ~s~n", [Data]),
      Result = mnesia:write(#phone_call{phone_number = Number,
        start_date = to_date(SDate), start_time = to_time(STime),
        end_date = to_date(EDate), end_time= to_time(ETime)}),
      io:format("Result of writing ~s is ~p~n", [RawData, Result]),
      read_phone_call(InputFile);
    RawData == eof -> ok
  end.

to_date(Date) ->
  [Year, Month, Day] = re:split(Date, "-", [{return, list}]),
  [{Y, _}, {M, _}, {D, _}] = lists:map(fun string:to_integer/1,
    [Year, Month, Day]),
  {Y, M, D}.

to_time(Time) ->
  [Hour, Minute, Second] = re:split(Time, ":", [{return, list}]),
  [{H, _}, {M, _}, {S, _}] = lists:map(fun string:to_integer/1,
    [Hour, Minute, Second]),
  {H, M, S}.

summary() ->
  FirstKey = ets:first(call_table),
  summary(FirstKey, []).

summary(Key, Result) ->
  NextKey = ets:next(call_table, Key),
  case NextKey of
    '$end_of_table' -> Result;
    _ -> summary(NextKey, [summary(Key) | Result])
  end.

summary(PhoneNumber) ->
  Calls = ets:lookup(call_table, PhoneNumber),
  Total = lists:foldl(fun subtotal/2, 0, Calls),
  [PhoneNumber, (Total + 59) div 60].

subtotal(Item, Accumulator) ->
  StartSeconds = calendar:datetime_to_gregorian_seconds(
    {Item#phone_call.start_date, Item#phone_call.start_time}),
  EndSeconds = calendar:datetime_to_gregorian_seconds(
    {Item#phone_call.end_date, Item#phone_call.end_time}),
  Accumulator + (EndSeconds - StartSeconds).

invoice(X) -> error.

