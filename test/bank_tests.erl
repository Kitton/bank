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
  Tests = open_account(),
  lists:foreach(fun(F) ->
                    F()
                end, Tests).
  
open_account() ->
  {ok, Bank} = application:get_env(bank, bank_code),
    {ok, #{id := C1}} = bn_logic:new_customer(<<"customer_1">>),
    {ok, #{id := C2}} = bn_logic:new_customer(<<"customer_2">>),
  [ 
    fun() ->
        {ok, #{id := IBAN}} = bn_logic:open_account(C1, <<"EUR">>),
        <<_:4/binary, BankCode:4/binary, _/binary>> = IBAN,
        Bank = BankCode
    end,
    fun() ->
        {ok, #{id := IBAN}} = bn_logic:open_account(C1, <<"EUR">>),
        <<_:4/binary, BankCode:4/binary, _/binary>> = IBAN,
        Bank = BankCode
    end,
    fun() ->
        {ok, #{id := IBAN}} = bn_logic:open_account(C2, <<"EUR">>),
        <<_:4/binary, BankCode:4/binary, _/binary>> = IBAN,
        Bank = BankCode
    end
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

