%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Bank Public API

-module(bank_app).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour
-behaviour(application).

%% Includes

%% Exports
-export([start/2,
         stop/1]).

%% Macro definitions
-define(SERVER, ?MODULE).

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% Application callbacks

start(_, _) ->
  bn_log:info(?MODULE, ?LINE, "Starting application"),
  %% Check for environment variables
  Port = fetch_or_set(port, 8000),
  fetch_or_set(bank_code, <<"0000">>),
  %% Start Cowboy
  Dispatch = cowboy_router:compile([
                                    {'_', [{"/interbank/consolidations/", bn_r_consolidations, []}]},
                                    {'_', [{"/transfers/", bn_r_transfers, []}]}
                                   ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
                               [{port, Port}],
                               #{env => #{dispatch => Dispatch}}
                              ),
  bank_sup:start_link().

stop(_State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

fetch_or_set(Env, Default) ->
  case application:get_env(bank, Env) of
      {ok, B} -> B;
      _ ->
        application:set_env(bank, Env, Default),
        bn_log:error(?MODULE,
                     ?LINE,
                     "Environment variable ~s not set. Using default ~p instead",
                     [Env, Default]),
      Default
    end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [].

%% Tests end
-endif.
