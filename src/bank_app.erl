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

start(_StartType, _StartArgs) ->
  bn_log:info(?MODULE, ?LINE, "Starting application"),
  Dispatch = cowboy_router:compile([
                                    {'_', [{"/", bn_r_v, []}]}
                                   ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
                               [{port, 8001}],
                               #{env => #{dispatch => Dispatch}}
                              ),
  bank_sup:start_link().

stop(_State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [].

%% Tests end
-endif.
