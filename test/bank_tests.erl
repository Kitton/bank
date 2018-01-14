%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module contains top-level tests for the bank application

-module(bank_tests).

%% Behaviour

%% Includes

%% Exports
-export([
         run/0
        ]).

%% Macro definitions

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

run() ->
  Tests = open_account() ++ balances() ++ transfers(),
  lists:foreach(fun(F) ->
                    F()
                end, Tests).
  
open_account() ->
  {ok, Bank} = application:get_env(bank, bank_code),
    {ok, #{id := C1}} = bank:new_customer(<<"customer_1">>),
    {ok, #{id := C2}} = bank:new_customer(<<"customer_2">>),
  [ 
    fun() ->
        {ok, #{id := IBAN}} = bank:open_account(C1, <<"EUR">>),
        <<_:4/binary, BankCode:4/binary, _/binary>> = IBAN,
        Bank = BankCode
    end,
    fun() ->
        {ok, #{id := IBAN}} = bank:open_account(C1, <<"EUR">>),
        <<_:4/binary, BankCode:4/binary, _/binary>> = IBAN,
        Bank = BankCode
    end,
    fun() ->
        {ok, #{id := IBAN}} = bank:open_account(C2, <<"EUR">>),
        <<_:4/binary, BankCode:4/binary, _/binary>> = IBAN,
        Bank = BankCode
    end
  ].

balances() ->
  {ok, #{id := C1}} = bank:new_customer(<<"customer_1">>),
  {ok, #{id := IBAN}} = bank:open_account(C1, <<"EUR">>),
  [
   fun() ->
       error = bn_dal:dec_balance(IBAN, 100, <<"EUR">>),
       ok = bn_dal:inc_balance(IBAN, 100, <<"EUR">>),
       ok = bn_dal:dec_balance(IBAN, 100, <<"EUR">>),
       error = bn_dal:dec_available(IBAN, 100, <<"EUR">>),
       ok = bn_dal:inc_available(IBAN, 100, <<"EUR">>),
       ok = bn_dal:dec_available(IBAN, 100, <<"EUR">>)
   end
  ].

transfers() ->
  {ok, #{id := C1}} = bank:new_customer(<<"customer_1">>),
  {ok, #{id := A1}} = bank:open_account(C1, <<"EUR">>),
  {ok, #{id := A2}} = bank:open_account(C1, <<"EUR">>),
  [
   fun() ->
       {error, {no_balance, _}} = bank:transfer(A1, A2, 100, <<"EUR">>)
   end,
   fun() ->
       {error, {no_balance, _}} = bank:transfer(A2, A1, 100, <<"EUR">>)
   end,
   fun() ->
       ok = bn_dal:inc_balance(A1, 100, <<"EUR">>),
       ok = bn_dal:inc_available(A1, 100, <<"EUR">>),
       {ok, #{consolidated := C}} = bank:transfer(A1, A2, 80, <<"EUR">>),
       true = C /= null,
       [#{available := 20, balance := 20}] = bn_dal:read_account(A1),
       [#{available := 80, balance := 80}] = bn_dal:read_account(A2)
   end,
   fun() ->
       {error, {no_balance, _}} = bank:transfer(A1, <<"ESXX99991">>, 20, <<"EUR">>),
       ok = bn_dal:inc_balance(A1, 500, <<"EUR">>),
       ok = bn_dal:inc_available(A1, 500, <<"EUR">>),
       {error, {consolidation_failed, _}} = bank:transfer(A1, <<"ESXX99991">>, 20, <<"EUR">>),
       [#{available := 520, balance := 520}] = bn_dal:read_account(A1)
   end
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

