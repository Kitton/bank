%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module contains the bank's business logic

-module(bn_error).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour

%% Includes

%% Exports
-export([
        ]).

%% Macro definitions

%% Type Definitions

-type error_type() ::
        sender_unidentified
      | sender_not_local
      | receiver_unidentified
      | bad_currency
      | bad_value
      | no_balance
      | consolidation_failed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Serializes an error
-spec serial_error(error_type(), #{}) -> #{}.
serial_error(sender_unidentified, _Args) ->
  #{type => sender_unidentified,
    description => <<"TBD">>,
    args => #{}};
serial_error(sender_not_local, _Args) ->
  #{type => sender_not_local,
    description => <<"TBD">>,
    args => #{}};
serial_error(receiver_unidentified, _Args) ->
  #{type => receiver_unidentified,
    description => <<"TBD">>,
    args => #{}};
serial_error(bad_currency, _Args) ->
  #{type => bad_currency,
    description => <<"TBD">>,
    args => #{}};
serial_error(bad_value, _Args) ->
  #{type => bad_value,
    description => <<"TBD">>,
    args => #{}};
serial_error(no_balance, _Args) ->
  #{type => no_balance,
    description => <<"TBD">>,
    args => #{}};
serial_error(consolidation_failed, _Args) ->
  #{type => consolidation_failed,
    description => <<"TBD">>,
    args => #{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [].

%% Tests end
-endif.
