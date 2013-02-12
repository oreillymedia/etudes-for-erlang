%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Read in a database of people, their cats, and their veterinarian
%% appointments.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(cat_db).
-export([setup/3, list_appointments/3]).
-include("cat_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @doc Set up Mnesia tables for owners, cats, and appointments
%% given their file names.

-spec(setup(string(), string(), string()) -> atom()).

setup(OwnerFileName, CatFileName, ApptFileName) ->

  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:delete_table(owner),
  mnesia:delete_table(cat),
  mnesia:delete_table(appointment),
  
  fill_table(owner, OwnerFileName,fun add_owner/1, record_info(fields, owner)),
  fill_table(cat, CatFileName, fun add_cat/1, record_info(fields, cat)),
  fill_table(appointment, ApptFileName, fun add_appt/1,
    record_info(fields, appointment)).

%% @doc Fill the given table with data from given file name.
%% The AdderFunction assigns data to fields and writes it to the file;
%% RecordInfo is used when creating the table.

fill_table(TableName, FileName, AdderFunction, RecordInfo) ->
  {Result, Message} = mnesia:create_table(TableName,
    [{attributes, RecordInfo}]),
  case Result of
    atomic ->
        {OpenResult, InputFile} = file:open(FileName, [read]),
        case OpenResult of
          ok ->
            TransResult = mnesia:transaction(
              fun() -> read_file(InputFile, AdderFunction) end),
            io:format("Transaction result ~p~n", [TransResult]);
          _ -> io:format("Error opening file: ~p~n", [FileName])
        end;
    aborted -> {error, Message}
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

  
%% Add an owner record; the data is in an ItemList.

-spec(add_owner(list()) -> undefined).

add_owner(ItemList) ->
  [Id, LastName, FirstName] = ItemList,
  mnesia:write(#owner{id = to_int(Id), last_name = LastName,
    first_name = FirstName}).

%% Add a cat record; the data is in an ItemList.

-spec(add_cat(list()) -> undefined).

add_cat(ItemList) ->
  [Id, Name, OwnerId] = ItemList,
  mnesia:write(#cat{id = to_int(Id), name = Name, owner_id = to_int(OwnerId)}).

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

to_int(Str) ->
  {IPart, _} = string:to_integer(Str),
  IPart.
  
%%summary(PhoneNumber) ->
%%  {Result, Calls} = mnesia:transaction(
%%    fun() ->
%%      qlc:e(
%%        qlc:q( [X || X <- mnesia:table(phone_call),
%%          X#phone_call.phone_number == PhoneNumber] )
%%      )
%%    end

list_appointments(Year, Month, Day) ->
  When = {Year, Month, Day},
  {_Result, List} = mnesia:transaction(
    fun() ->
     
        ApptHandle = qlc:q([Appt || Appt <- mnesia:table(appointment),
          Appt#appointment.date == When,
          Cat <- mnesia:table(cat),
          Cat#cat.id == Appt#appointment.cat_id]),
       Temp = qlc:e(ApptHandle),
       io:format("~p~n", [Temp]),
       io:format("===============~n"),
       qlc:e(
        qlc:q( [{Appt#appointment.date, Cat#cat.name,
          {Owner#owner.last_name, Owner#owner.first_name}} ||
          Appt <- ApptHandle,
          Cat <- mnesia:table(cat),
          Owner <- mnesia:table(owner),
          Cat#cat.owner_id == Owner#owner.id
        ]
        )
      )
    end
  ),
  List.
          
%%invoice(Last, First, Middle) -> 
%%  {Result, CallList} = mnesia:transaction(
%%    fun() ->
%%      qlc:e(
%%        qlc:q( [{Person, Call}   ||
%%          Person <- mnesia:table(customer),
%%          Call <- mnesia:table(phone_call),
%%          Person#customer.phone_number == Call#phone_call.phone_number,
%%          Person#customer.last_name == Last,
%%          Person#customer.first_name == First,
%%          Person#customer.middle_name == Middle
%%          ]
%%        )
 %%     )
%%    end
%%  ).

