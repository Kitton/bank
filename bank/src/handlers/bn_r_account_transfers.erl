%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Inter-bank consolidations handler

-module(bn_r_account_transfers).


%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Includes

%% Exports
-export([init/2]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

%% Macro definitions

%% @doc Request fields
-define(FIELDS, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

init(Req, State) ->
    {cowboy_rest, Req, State}.

rest_init(Req, []) ->
  {ok, Req, #{}}.

allowed_methods(Req, State) ->
  Allowed = [<<"GET">>],
  Method = cowboy_req:method(Req),
  Req1 =
    case lists:member(Method, Allowed) of
      true ->
        Req;
      false ->
        {stop, bn_error:error_resp(Req, {method_not_allowed, #{}}), State}
    end,
  {Allowed, Req1, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET
to_json(Req, State) ->
  Id = cowboy_req:binding(account, Req),
  io:format("ACCOUNT = ~p~n", [Id]),
  case bank:get_transactions(Id) of
    error ->
      Req1 = bn_error:error_resp(Req, {account_not_found, #{id => Id}}),
      {stop, cowboy_req:reply(404, Req1), State};
    Transfers ->
      JSON = jiffy:encode(Transfers),
      {JSON, Req, State}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

%% @doc Test functions that should always succeed (ie. testing them
%% does not depend on the database or external states)
basic_test_() ->
  [].

%% Tests end
-endif.
