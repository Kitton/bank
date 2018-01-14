%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module contains the definition of the logical model

-module(bn_model).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour

%% Includes

%% Exports
-export_type([
              object/0,
              object_name/0,
              transfer/0,
              transfer_id/0,
              transfer_type/0,
              customer/0,
              customer_id/0,
              account/0,
              account_id/0,
              value/0,
              currency/0
             ]).

%% Macro definitions

%% Type Definitions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOGICAL MODEL DEFINITION
-type object() :: transfer() | customer() | account().

-type object_name() :: transfer | customer | account.

-type transfer() ::
        #{
           id => transfer_id(),
           type => transfer_type(),
           sender => binary(),
           receiver => binary(),
           value => value(),
           currency => currency(),
           commission => value(),
           created => bn_time:ts(),
           preconsolidated => bn_time:ts(),
           consolidated => bn_time:ts(),
           failed => bn_time:ts()
         }.

-type transfer_id() :: binary().

-type transfer_type() :: internal | external.

-type customer() ::
        #{
           id => customer_id(),
           name => binary(),
           created => bn_time:ts()
         }.

-type customer_id() :: binary().

-type account() ::
        #{
           id => account_id(),
           customer => customer_id(),
           available => value(),
           balance => value(),
           currency => currency(),
           created => bn_time:ts()
         }.

-type account_id() :: binary().

-type value() :: non_neg_integer().

-type currency() :: binary().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [?_test(bn_time:now())].

%% Tests end
-endif.
