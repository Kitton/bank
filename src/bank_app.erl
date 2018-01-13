%%%-------------------------------------------------------------------
%% @doc bank public API
%% @end
%%%-------------------------------------------------------------------

-module(bank_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

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

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
