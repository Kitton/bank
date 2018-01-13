%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module manages the interaction with the
%% database. Translation from the logical model to the physical model
%% is done here.

-module(bn_db).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour

%% Includes

%% Exports
-export([
         create/2,
         save/2,
         read/2
        ]).

%% Macro definitions

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PHYSICAL MODEL DEFNITION

-record(transfer,
        {
          id :: binary(),
          sender :: binary(),
          receiver :: binary(),
          value :: non_neg_integer(),
          currency :: binary(),
          commission = 0 :: non_neg_integer(),
          created :: integer(),
          preconsolidated = null :: integer() | null,
          consolidated = null :: integer() | null,
          failed = null :: integer() | null
        }).

-record(customer,
        {
          id :: binary(),
          name :: binary(),
          created :: non_neg_integer()
        }).

-record(account,
        {
          id :: binary(),
          customer :: binary(),
          available = 0 :: integer(),
          balance = 0 :: integer(),
          currency = <<"EUR">> :: binary(),
          created :: integer()
        }).

-type object() :: #transfer{} | #customer{} | #account{}.

%% @doc Dynamically returns information about the object's physical
%% representation
-spec fields(bn_model:object_name()) -> [{atom(), term}].
fields(transfer) ->
  [
   {id, 'REQUIRED'},
   {sender, 'REQUIRED'},
   {receiver, 'REQUIRED'},
   {value, 'REQUIRED'},
   {currency, 'REQUIRED'},
   {commission, 0},
   {created, 'REQUIRED'},
   {preconsolidated, null},
   {consolidated, null},
   {failed, null}
  ];
fields(customer) ->
  [
   {id, 'REQUIRED'},
   {name, 'REQUIRED'},
   {created, 'REQUIRED'}
  ];
fields(account) ->
  [
   {id, 'REQUIRED'},
   {customer, 'REQUIRED'},
   {available, 0},
   {balance, 0},
   {currency, <<"EUR">>},
   {created, 'REQUIRED'}
  ].

%% @doc Dynamically returns the keys of the object's physical
%% representation
-spec keys(bn_model:object_name()) -> [atom()].
keys(Object) ->
  lists:map(fun({K, _}) -> K end, fields(Object)).

%% @doc Returns the record name for the given table
-spec object_name(atom()) -> atom().
object_name(Table) ->
  case Table of
    transfers -> transfer;
    customers -> customer;
    accounts -> account
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Creates a new object in the given table. Returns error if
%% already exists
-spec create(atom(), bn_model:object()) -> {ok, bn_model:object()} | error.
create(Table, Object) ->
  Name = object_name(Table),
  Row = from_model(Name, Object),
  case ets:insert_new(Table, Row) of
    true ->
      {ok, to_model(Row)};
    false ->
      error
  end.

%% @doc Saves a new object in the given table
-spec save(atom(), bn_model:object()) -> {ok, bn_model:object()}.
save(Table, Object) ->
  Name = object_name(Table),
  Row = from_model(Name, Object),
  ets:insert(Table, Row),
  {ok, to_model(Row)}.

-spec read(atom(), bn_model:object()) -> [bn_model:object()].
read(Table, Id) ->
  Rows = ets:lookup(Table, Id),
  lists:map(fun to_model/1, Rows).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

%% @doc Takes the physical representation of an object and returns its
%% logical representation
-spec to_model(bn_db:object()) -> bn_model:object().
to_model(Record) -> 
  Name = element(1, Record),
  Keys = keys(Name),
  Values = tl(tuple_to_list(Record)),
  Proplist = lists:zip(Keys, Values),
  maps:from_list(Proplist).

%% @doc Takes the logical representation of an object and returns its
%% physical representation
-spec from_model(bn_model:object_name(), bn_model:object()) -> bn_db:object().
from_model(Name, Object) ->
  Values =
    lists:map(fun({Key, 'REQUIRED'}) ->
                  maps:get(Key, Object);
                 ({Key, Default}) ->
                  maps:get(Key, Object, Default)
              end, fields(Name)),
  list_to_tuple([Name|Values]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

%% @doc Test the translation between the logical and physical models
model_test_() ->
  TransferP = #transfer{ id = <<"id">>,
                         sender = <<"sender">>,
                         receiver = <<"receiver">>,
                         value = <<"value">>,
                         currency = <<"EUR">>,
                         created = 1234},
  CustomerP = #customer{ id = <<"id">>,
                         name = <<"name">>,
                         created = 1234},
  AccountP = #account{ id = <<"id">>,
                       customer = <<"customer">>,
                       created = 1234
                     },
  TransferL = #{ id => <<"id">>,
                 sender => <<"sender">>,
                 receiver => <<"receiver">>,
                 value => <<"value">>,
                 currency => <<"EUR">>,
                 created => 1234},
  CustomerL = #{ id => <<"id">>,
                 name => <<"name">>,
                 created => 1234},
  AccountL = #{ id => <<"id">>,
                customer => <<"customer">>,
                created => 1234
              },
  [
   ?_assertEqual(#transfer{}, from_model(transfer, to_model(#transfer{}))),
   ?_assertEqual(#customer{}, from_model(customer, to_model(#customer{}))),
   ?_assertEqual(#account{}, from_model(account, to_model(#account{}))),
   ?_assertEqual(TransferP, from_model(transfer, to_model(TransferP))),
   ?_assertEqual(CustomerP, from_model(customer, to_model(CustomerP))),
   ?_assertEqual(AccountP, from_model(account, to_model(AccountP))),
   ?_assertEqual(TransferP, from_model(transfer, TransferL)),
   ?_assertEqual(CustomerP, from_model(customer, CustomerL)),
   ?_assertEqual(AccountP, from_model(account, AccountL)),
   ?_test(try from_model(transfer, #{}) catch error:{badkey, id} -> ok end),
   ?_test(try from_model(customer, #{}) catch error:{badkey, id} -> ok end),
   ?_test(try from_model(account, #{}) catch error:{badkey, id} -> ok end)
  ].

%% Tests end
-endif.
