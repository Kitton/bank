%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module contains the bank's business logic

-module(bn_dal).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour
-behaviour(gen_server).

%% Includes

%% Exports
-export([
         start_link/0,
         stop/0,
         create_transfer/1,
         update_transfer/2,
         inc_available/3,
         dec_available/3,
         inc_balance/3,
         dec_balance/3,
         create_account/1,
         read_account/1,
         create_customer/1,
         read_customer/1,
         find_transactions/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

%% Macro definitions

-define(SERVER, ?MODULE).

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Starts the gen_server
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the gen_server
stop() ->
  gen_server:stop(?SERVER).

%% @doc Creates a new transfer
-spec create_transfer(maps:map()) -> {ok, bn_model:transfer()}.
create_transfer(Transfer) ->
  gen_server:call(?SERVER, {create_transfer, Transfer}).

%% @doc Updates an existing transfer
-spec update_transfer(Id :: bn_model:transfer_id(), Fields :: maps:map()) -> {ok, bn_model:transfer()} | error.
update_transfer(Id, Fields) ->
  gen_server:call(?SERVER, {update_transfer, Id, Fields}).

%% @doc Decreases an account's available balance
-spec dec_available(Account :: bn_model:account_id(),
                    Value :: bn_model:value(),
                    Currency :: bn_model:currency()) -> ok | error.
dec_available(Account, Value, Currency) ->
  gen_server:call(?SERVER, {dec_available, Account, Value, Currency}).

%% @doc Increases an account's available balance
-spec inc_available(Account :: bn_model:account_id(),
                    Value :: bn_model:value(),
                    Currency :: bn_model:currency()) -> ok | error.
inc_available(Account, Value, Currency) ->
  gen_server:call(?SERVER, {inc_available, Account, Value, Currency}).

%% @doc Decreases an account's real balance
-spec dec_balance(Account :: bn_model:account_id(),
                    Value :: bn_model:value(),
                    Currency :: bn_model:currency()) -> ok | error.
dec_balance(Account, Value, Currency) ->
  gen_server:call(?SERVER, {dec_balance, Account, Value, Currency}).

%% @doc Increases an account's real balance
-spec inc_balance(Account :: bn_model:account_id(),
                    Value :: bn_model:value(),
                    Currency :: bn_model:currency()) -> ok | error.
inc_balance(Account, Value, Currency) ->
  gen_server:call(?SERVER, {inc_balance, Account, Value, Currency}).

-spec create_account(maps:map()) -> {ok, bn_model:account()} | error.
create_account(Account) ->
  gen_server:call(?SERVER, {create_account, Account}).

%% @doc Reads an account (not thread safe)
-spec read_account(bn_model:account_id()) -> [bn_model:account()].
read_account(Id) ->
  bn_db:read(accounts, Id).

%% @doc Creates a new customer
-spec create_customer(maps:map()) -> {ok, bn_model:customer()}.
create_customer(Customer) ->
  gen_server:call(?SERVER, {create_customer, Customer}).

%% @doc Reads an customer (not thread safe)
-spec read_customer(bn_model:customer_id()) -> [bn_model:customer()].
read_customer(Id) ->
  bn_db:read(customers, Id).

%% @doc Returns the transactions of the given account (not thread safe)
-spec find_transactions(bn_model:account_id()) -> [bn_model:transfer()] | error.
find_transactions(Id) ->
  case bn_db:read(accounts, Id) of
    [] ->
      error;
    _ ->
      bn_db:find_transactions(Id)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour Callbacks

%% -type state() :: #{}.

%% @doc init
init([]) ->
  {ok, #{}}.

%% @doc handle_call
handle_call({create_transfer, Transfer}, _, State) ->
  Reply = create_transfer_call(Transfer),
  {reply, Reply, State};
handle_call({update_transfer, Id, Fields}, _, State) ->
  Reply = update_transfer_call(Id, Fields),
  {reply, Reply, State};
handle_call({inc_available, Account, Value, Currency}, _, State) ->
  Reply = inc_available_call(Account, Value, Currency),
  {reply, Reply, State};
handle_call({dec_available, Account, Value, Currency}, _, State) ->
  Reply = dec_available_call(Account, Value, Currency),
  {reply, Reply, State};
handle_call({inc_balance, Account, Value, Currency}, _, State) ->
  Reply = inc_balance_call(Account, Value, Currency),
  {reply, Reply, State};
handle_call({dec_balance, Account, Value, Currency}, _, State) ->
  Reply = dec_balance_call(Account, Value, Currency),
  {reply, Reply, State};
handle_call({create_account, Account}, _, State) ->
  Reply = create_account_call(Account),
  {reply, Reply, State};
handle_call({create_customer, Customer}, _, State) ->
  Reply = create_customer_call(Customer),
  {reply, Reply, State};
handle_call(_, _, State) ->
  {noreply, State}.

%% @doc handle_cast
handle_cast(_, State) ->
  {noreply, State}.

%% @doc handle_info
handle_info(Message, Table) ->
  bn_log:error(?MODULE, ?LINE, "Unexpected message ~p", [Message]),
  {noreply, Table}.

%% @doc terminate
terminate(_, _) ->
  bn_log:info(?MODULE, ?LINE, "Terminating"),    
  {noreply, ok}.

%% @doc code_change
code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions


%% Transfers

%% @doc Creates a new transfer in the DB
-spec create_transfer_call(maps:map()) -> {ok, bn_model:transfer()}.
create_transfer_call(Transfer) ->
  Id = integer_to_binary(rand:uniform(9999999999)),
  Object = Transfer#{ id => Id,
                      created => bn_time:now()},
  case bn_db:create(transfers, Object) of
    {ok, Created} ->
      {ok, Created};
    error ->
      create_transfer_call(Transfer)
  end.


-spec update_transfer_call(Id :: bn_model:transfer_id(), Fields :: maps:map()) -> {ok, bn_model:transfer()} | error.
update_transfer_call(Id, Fields) ->
  case bn_db:read(transfers, Id) of
    [] ->
      error;
    [Transfer] ->
      Updated = maps:merge(Transfer, Fields),
      bn_db:save(transfers, Updated),
      {ok, Updated}
  end.

%% Availables

%% @doc Some banks might allow their customers to go into overdraft
-define(LIMIT, 0).

%% @doc Decreases an account's available balance. Returns error if the
%% account goes under the established limit
-spec dec_available_call(Id :: bn_model:account_id(),
                         Value :: bn_model:value(),
                         Currency :: bn_model:currency()) -> ok | error.
dec_available_call(Id, TransferValue, TransferCurrency) ->
  [Account] = bn_db:read(accounts, Id),
  #{available := Available, currency := Currency} = Account,
  CER = bn_cer:cer(TransferCurrency, Currency),
  Value = round(TransferValue * CER),
  if Available - Value < ?LIMIT ->
      error;
     true ->
      bn_db:save(accounts, Account#{available => Available - Value}),
      ok
  end.

%% @doc Increases an account's available balance
-spec inc_available_call(Id :: bn_model:account_id(),
                         Value :: bn_model:value(),
                         Currency :: bn_model:currency()) -> ok.
inc_available_call(Id, TransferValue, TransferCurrency) ->
  [Account] = bn_db:read(accounts, Id),
  #{available := Available, currency := Currency} = Account,
  CER = bn_cer:cer(TransferCurrency, Currency),
  Value = round(TransferValue * CER),
  bn_db:save(accounts, Account#{available => Available + Value}),
  ok.

%% Balances

%% @doc Decreases an account's real balance. Returns error if the
%% account goes under the established limit
-spec dec_balance_call(Id :: bn_model:account_id(),
                       Value :: bn_model:value(),
                       Currency :: bn_model:currency()) -> ok | error.
dec_balance_call(Id, TransferValue, TransferCurrency) ->
  [Account] = bn_db:read(accounts, Id),
  #{balance := Balance, currency := Currency} = Account,
  CER = bn_cer:cer(TransferCurrency, Currency),
  Value = round(TransferValue * CER),
  if Balance - Value < ?LIMIT ->
      error;
     true ->
      bn_db:save(accounts, Account#{balance => Balance - Value}),
      ok
  end.

%% @doc Increases an account's real balance
-spec inc_balance_call(Id :: bn_model:account_id(),
                       Value :: bn_model:value(),
                       Currency :: bn_model:currency()) -> ok.
inc_balance_call(Id, TransferValue, TransferCurrency) ->
  [Account] = bn_db:read(accounts, Id),
  #{balance := Balance, currency := Currency} = Account,
  CER = bn_cer:cer(TransferCurrency, Currency),
  Value = round(TransferValue * CER),
  bn_db:save(accounts, Account#{balance => Balance + Value}),
  ok.


%% Accounts

%% @doc Creates a new account in the DB
-spec create_account_call(maps:map()) -> {ok, bn_model:account()} | error.
create_account_call(Account = #{customer := Customer}) ->
  case bn_db:read(customers, Customer) of
    [] ->
      error;
    _ ->
      Object = Account#{ id => gen_iban(),
                         created => bn_time:now()},
      case bn_db:create(accounts, Object) of
        {ok, Created} ->
          {ok, Created};
        error ->
          create_account_call(Account)
      end
  end.

%% @doc Randomly generates an account ID that follows the spanish IBAN
%% format for this bank (except for the two sanity-check digits, which
%% are left as XX)
-spec gen_iban() -> bn_model:account_id().
gen_iban() ->
  {ok, BankCode} = application:get_env(bank, bank_code),
  Rest = integer_to_binary(rand:uniform(9999999999999999)), %% 16 digits
  Fill = list_to_binary(lists:duplicate(16 - size(Rest), 48)), %% 48 is "0" in ASCII
  <<"ESXX", BankCode/binary, Fill/binary, Rest/binary>>.

%% Customers

%% @doc Creates a new customer in the DB
-spec create_customer_call(maps:map()) -> {ok, bn_model:customer()}.
create_customer_call(Customer) ->
  Id = integer_to_binary(rand:uniform(999999)),
  Object = Customer#{ id => Id,
                      created => bn_time:now()},
  case bn_db:create(customers, Object) of
    {ok, Created} ->
      {ok, Created};
    error ->
      create_customer_call(Customer)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

start_stop_test() ->
  ?_test(
     begin
       start_link(),
       stop()
     end).

%% Tests end
-endif.
