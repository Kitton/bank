%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Banki API

-module(bank).

%% Exports
-export([
         transfer/4,
         open_account/2,
         new_customer/1,
         get_transactions/1
        ]). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Performs a transfer between two accounts. Receiver account can
%% belong to another bank.
-spec transfer(Sender :: bn_model:account_id(),
               Receiver :: bn_model:account_id(),
               Value :: bn_model:value(),
               Currency :: bn_model:currency()) -> {ok, bn_model:transfer()} |
                                                   {error, {bn_error:error_type(), #{}}}.
transfer(Sender, Receiver, Value, Currency) ->
  bn_logic:transfer(Sender, Receiver, Value, Currency).

%% @doc Opens a new account in the bank
-spec open_account(Customer :: bn_model:customer_id(),
                   Currency :: bn_model:currency()) -> {ok, bn_model:account()} |
                                                       {error, customer_unknown} |
                                                       {error, bad_currency}.
open_account(Customer, Currency) ->
  bn_logic:open_account(Customer, Currency).

%% @doc Registers a new customer in the bank
-spec new_customer(Name :: binary()) -> {ok, bn_model:customer()}.
new_customer(Name) ->
  bn_logic:new_customer(Name).

%% @doc Returns the transactions of a given account. Returns error if
%% account does not belong to this bank.
-spec get_transactions(Id :: bn_model:account_id()) -> [bn_model:transfer()] | error.
get_transactions(Id) ->
  bn_dal:find_transactions(Id).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
