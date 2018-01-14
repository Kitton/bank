%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Top level supervisor for the bank app

-module(bank_sup).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour
-behaviour(supervisor).

%% Includes

%% Exports
-export([start_link/0]).

-export([init/1]).

%% Macro definitions
-define(SERVER, ?MODULE).
-define(TABLES, [transfers, customers, accounts]).

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    {ok, { {one_for_one, 1, 1}, [
                                 {table_handler,
                                  {bn_table_handler, start_link, [table_handler, ?TABLES]},
                                  permanent,
                                  1250,
                                  worker,
                                  [bn_table_handler]},
                                 {data_access_layer,
                                  {bn_dal, start_link, []},
                                  permanent,
                                  1250,
                                  worker,
                                  [bn_table_dal]}
                                ]} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [].

%% Tests end
-endif.
