%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module contains the bank's business logic

-module(bn_logic).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour

%% Includes

%% Exports
-export([
         transfer/4,
         open_account/2,
         new_customer/1
        ]).

%% Macro definitions

-define(EXTERNAL_TRANSFER_MAX, 10000000). %% 1000 EUR (in centi-cents)
-define(EXTERNAL_TRANSFER_COMM, 50000). %% 5 EUR (in centi-cents)

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

-spec transfer(Sender :: bn_model:account_id(),
               Receiver :: bn_model:account_id(),
               Value :: bn_model:value(),
               Currency :: bn_model:currency()) -> {ok, bn_model:transfer()} | {error, {bn_error:error_type(), #{}}}.
transfer(Sender, Receiver, Value, Currency) ->
  Transfer = #{sender => Sender,
               receiver => Receiver,
               value => Value,
               currency => Currency},
  transfer_sm(identify_sender, Transfer, #{}).

-type transfer_sm_state() ::
        identify_sender
      | identify_receiver
      | check_value
      | commit
      | consolidate.

-spec transfer_sm(transfer_sm_state(), #{}, #{}) -> {ok, bn_model:transfer()} | {error, {bn_error:error_type(), #{}}}.
transfer_sm(identify_sender, Transfer = #{sender := Sender}, Ctx) ->
  case identify_account(Sender) of
    error ->
      {error, {sender_unidentified, #{bad_iban => Sender}}};
    {external, Bank} ->
      {error, {sender_not_local, #{bank => Bank}}};
    {ok, _} ->
      transfer_sm(identify_receiver, Transfer, Ctx)
  end;
transfer_sm(identify_receiver, Transfer = #{receiver := Receiver}, Ctx) ->
  case identify_account(Receiver) of
    error ->
      {error, {receiver_unidentified, #{bad_iban => Receiver}}};
    {external, Bank} ->
      transfer_sm(check_value, Transfer#{kind => external}, Ctx#{external => Bank});
    {ok, _} ->
      transfer_sm(check_value, Transfer#{kind => internal}, Ctx)
  end;
transfer_sm(check_value, Transfer, Ctx) ->
  #{value := Value, currency := Currency, kind := Kind} = Transfer,
  case bn_cer:available(Currency) of
    true ->
      {error, {bad_currency, #{currency => Currency}}};
    false ->
      %% NOTE (new transfers): Refactor this if new transfer kinds are allowed
      case Kind of
        internal ->
          transfer_sm(commit, Transfer#{commission => 0}, Ctx);
        external when Value =< ?EXTERNAL_TRANSFER_MAX ->
          transfer_sm(commit,
                      Transfer#{commission => ?EXTERNAL_TRANSFER_COMM},
                      Ctx);
        external ->
          {error, {bad_value, #{limit => ?EXTERNAL_TRANSFER_COMM}}}
      end
  end;
transfer_sm(commit, Transfer, Ctx) ->
  #{value := Value,
    commission := Commission,
    currency := Currency,
    sender := Sender} = Transfer,
  case bn_dal:dec_available(Sender, Value + Commission, Currency) of
    ok ->
      {ok, Created} = bn_dal:create_transfer(Transfer),
      transfer_sm(consolidate, Created, Ctx);
    error ->
      {error, {no_balance, #{}}}
  end;
transfer_sm(consolidate, Transfer = #{kind := Kind}, #{external := Bank}) ->
  case Kind of
    internal ->
      consolidate_internal(Transfer);
    external ->
      case bn_comm:call_bank(Bank, Transfer) of
        {ok, Updated} ->
          consolidate_external_out(Updated);
        {wait, PreConsolidated} ->
          {ok, PreConsolidated};
        {error, Reason} ->
          fail(Transfer),
          {error, {consolidation_failed, #{reason => Reason}}}
      end
  end.


%% @doc Opens a new account in the bank
-spec open_account(Customer :: bn_model:customer_id(),
                   Currency :: bn_model:currency()) -> {ok, bn_model:account()} |
                                                       {error, customer_unknown} |
                                                       {error, bad_currency}.
open_account(Customer, Currency) ->
  case bn_cer:available(Currency) of
    false ->
      {error, bad_currency};
    true ->
      Result =
        bn_dal:create_account(#{customer => Customer, currency => Currency}),
        case Result of
          error ->
            {error, customer_unknown};
          {ok, Account} ->
            {ok, Account}
        end
  end.
      
%% @doc Registers a new customer in the bank
-spec new_customer(Name :: binary()) -> {ok, bn_model:customer()}.
new_customer(Name) ->
  bn_dal:create_customer(#{name => Name}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
-spec identify_account(Id :: binary()) -> {local, bn_model:account()} | {external, binary()} | error.
identify_account(Id) ->
  case bn_dal:read_account(Id) of
    [Account] ->
      {local, Account};
    [] ->
      case get_bank(Id) of
        error ->
          error;
        BankCode ->
          {external, BankCode}
      end
  end.

%% @doc Returns the bank code of the given IBAN code. For this exercise, the IBAN must follow spanish format
-spec get_bank(binary()) -> binary() | error.
get_bank(Account) when size(Account) == 24 ->
  case Account of
    <<_Country:2/binary, _Control:2/binary, Bank:4/binary, _/binary>> ->
      Bank;
    _ ->
      error
  end;
get_bank(_) ->
  error.

%% @doc Consolidates an internal transfer: Increases the receiver's
%% available and balance and reduces the sender's balance
-spec consolidate_internal(bn_model:transfer()) -> {ok, bn_model:transfer()}.
consolidate_internal(Transfer) ->
  #{id := Id,
    sender := Sender,
    receiver := Receiver,
    value := Value,
    currency := Currency,
    commission := Commission } = Transfer,
  bn_dal:inc_available(Receiver, Value, Currency),
  bn_dal:inc_balance(Receiver, Value, Currency),
  bn_dal:dec_balance(Sender, Value + Commission, Currency),
  {ok, _} = bn_dal:update_transfer(Id, #{consolidated => bn_time:now()}).
  
%% @doc Consolidates an external, incoming transfer: Increases the receiver's
%% available and balance
-spec consolidate_external_in(bn_model:transfer()) -> {ok, bn_model:transfer()}.
consolidate_external_in(Transfer) ->
  #{id := Id,
    receiver := Receiver,
    value := Value,
    currency := Currency} = Transfer,
  bn_dal:inc_available(Receiver, Value, Currency),
  bn_dal:inc_balance(Receiver, Value, Currency),
  {ok, _} = bn_dal:update_transfer(Id, #{consolidated => bn_time:now()}).

%% @doc Consolidates an external, outgoing transfer: Decreases the
%% sender's balance
-spec consolidate_external_out(bn_model:transfer()) -> {ok, bn_model:transfer()}.
consolidate_external_out(Transfer) ->
  #{sender := Sender,
    value := Value,
    currency := Currency,
    commission := Commission } = Transfer,
  bn_dal:dec_balance(Sender, Value + Commission, Currency),
  {ok, Transfer}.

%% @doc Fails a transfer because it could not be consolidated with the
%% external bank
-spec fail(bn_model:transfer()) -> {ok, bn_model:transfer()}.
fail(#{id := Id}) ->
  {ok, _} = bn_dal:update_transfer(Id, #{failed => bn_time:now()}). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [].

%% Tests end
-endif.
