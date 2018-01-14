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
  R = bank_sup:start_link(),
  populate_example(),
  R.

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

%% @doc Populates the example given for the Digital Origin Challenge
populate_example() ->
  {ok, Bank} = application:get_env(bank, bank_code),
  case Bank of
    <<"000A">> -> %% Bank A
      %% Create an account for Jose with 20k EUR
      {ok, #{id := Id}} = bank:new_customer(<<"Jose">>),
      bn_db:save(accounts, #{id => <<"ESXX000A1">>, customer => Id, created => bn_time:now(), available => 20000 * 100, balance => 20000 * 100, currency => <<"EUR">>});
    <<"000B">> -> %% Bank B
      %% Create an account for Antonio with 20k EUR
      {ok, #{id := Id1}} = bank:new_customer(<<"Antonio">>),
      bn_db:save(accounts, #{id => <<"ESXX000B1">>, customer => Id1, created => bn_time:now(), available => 20000 * 100, balance => 20000 * 100, currency => <<"EUR">>}),
      %% Create an account for Antonio with 20k EUR
      {ok, #{id := Id2}} = bank:new_customer(<<"Maria">>),
      bn_db:save(accounts, #{id => <<"ESXX000B2">>, customer => Id2, created => bn_time:now(), available => 0, balance => 0, currency => <<"EUR">>});
    _ ->
      ok
  end.
      
       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [].

%% Tests end
-endif.
