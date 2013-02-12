%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Read in a database of people, their cats, and their veterinarian
%% appointments.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(cat_db).
-export([setup/2, list_appointments/3]).
-include("cat_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @doc Set up Mnesia tables for owners, cats, and appointments
%% given their file names.

-spec(setup(string(), string()) -> atom()).

setup(CatFileName, ApptFileName) ->

  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:delete_table(cat),
  mnesia:delete_table(appointment),
  
  fill_table(cat, CatFileName, fun add_cat/1, record_info(fields, cat)),
  fill_table(appointment, ApptFileName, fun add_appt/1,
    record_info(fields, appointment)).

%% @doc Fill the given table with data from given file name.
%% The AdderFunction assigns data to fields and writes it to the file;
%% RecordInfo is used when creating the table.

fill_table(TableName, FileName, AdderFunction, RecordInfo) ->
  mnesia:create_table(TableName, [{attributes, RecordInfo}]),
  
  {OpenResult, InputFile} = file:open(FileName, [read]),
  case OpenResult of
    ok ->
      TransResult = mnesia:transaction(
        fun() -> read_file(InputFile, AdderFunction) end),
        io:format("Transaction result ~p~n", [TransResult]);
    _ -> io:format("Error opening file: ~p~n", [FileName])
  end.
   
%% @doc Read a line from InputFile, and insert its contents into
%% the appropriate table by using AdderFunction.

-spec(read_file(file:io_device(), function()) -> atom()).

read_file(InputFile, AdderFunction) ->
  RawData = io:get_line(InputFile, ""),
  if
    is_list(RawData) ->
      Data = string:strip(RawData, right, $\n),
      ItemList = re:split(Data, ",", [{return, list}]),
      AdderFunction(ItemList),
      read_file(InputFile, AdderFunction);
    RawData == eof -> ok
  end.

  
%% Add a cat record; the data is in an ItemList.

-spec(add_cat(list()) -> undefined).

add_cat(ItemList) ->
  [Id, Name, OwnerName] = ItemList,
  mnesia:write(#cat{id = to_int(Id), name = Name, owner_name = OwnerName}).

%% Add an appointment record; the data is in an ItemList.

-spec(add_appt(list()) -> undefined).

add_appt(ItemList) ->
  [Id, Date, CatId] = ItemList,
  mnesia:write(#appointment{id = to_int(Id), date = to_date(Date),
    cat_id = to_int(CatId)}).

%% @doc Convert a string in form "yyyy-mm-dd" to a tuple {yyyy, mm, dd}
%% suitable for use with the calendar module.

-spec(to_date(string()) -> {integer(), integer(), integer()}).

to_date(Date) ->
  [Year, Month, Day] = re:split(Date, "-", [{return, list}]),
  [{Y, _}, {M, _}, {D, _}] = lists:map(fun string:to_integer/1,
    [Year, Month, Day]),
  {Y, M, D}.

%% @doc Convenience routine to convert a string to integer.
%% In case of an error, return zero.
%% suitable for use with the calendar module.

-spec(to_int(string()) -> integer()).

to_int(Str) ->
  {IPart, _} = string:to_integer(Str),
  case IPart of
    error -> 0;
    _ -> IPart
  end.
  

list_appointments(Year, Month, Day) ->
  When = {Year, Month, Day},
  {_Result, List} = mnesia:transaction(
    fun() ->
       qlc:e(
        qlc:q( [{Cat#cat.name, Cat#cat.owner_name} ||
          Appt <- mnesia:table(appointment),
          Cat <- mnesia:table(cat),
          Appt#appointment.date == When,
          Cat#cat.id == Appt#appointment.cat_id
        ]
        )
      )
    end
  ),
  List.

