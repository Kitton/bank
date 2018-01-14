%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module manages currencies and currency-exchange-rates

-module(bn_cer).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour

%% Includes

%% Exports
-export([
         available/1,
         cer/2
        ]).

%% Macro definitions

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Returns true if the currency is available for operation in
%% this bank, false otherwise
-spec available(bn_model:currency()) -> boolean().
available(<<"EUR">>) ->
  true;
available(_) ->
  false.

%% @doc Returns the CER for two currencies
-spec cer(From :: bn_model:currency(), To :: bn_model:currency()) -> float().
cer(_, _) ->
  1.0.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [?_test(available(<<"EUR">>)),
   ?_assertEqual(1.0, cer(<<"EUR">>, <<"EUR">>))
  ].

%% Tests end
-endif.
