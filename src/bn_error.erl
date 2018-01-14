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
         error_resp/2
        ]).

%% Macro definitions

%% Type Definitions

-type error_type() :: logic_error() | api_error().

-type logic_error() ::
        sender_unidentified
      | sender_not_local
      | receiver_unidentified
      | bad_currency
      | bad_value
      | no_balance
      | consolidation_failed
      | account_not_found.

-type api_error() ::
        method_not_allowed |
        bad_request.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Serializes an error
-spec serial_error(error_type(), maps:map()) -> maps:map().
serial_error(Type, _Args) ->
  #{type => Type,
    description => <<"To be defined">>,
    args => #{}
   }.

%% @doc Cowboy dependant: Prepares an error response
error_resp(Req, {ErrorType, Args}) ->
  Map = serial_error(ErrorType, Args),
  Body = jiffy:encode(Map),
  cowboy_req:set_resp_header(<<"Content-Type">>,
                             <<"application/json">>,
                             cowboy_req:set_resp_body(Body, Req)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [].

%% Tests end
-endif.
