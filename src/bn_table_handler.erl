%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module implements a basic database. Data is stored in ETS
%% tables and access is done through calls to this gen_server.

-module(bn_table_handler).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour
-behaviour(gen_server).

%% Includes

%% Exports
-export([
         start_link/1,
         stop/1,
         create/2,
         delete/2,
         all/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

%% Macro definitions

%% Type Definitions

%% -type state() ::
%%         #{
%%            tables => [atom()]
%%          }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Starts the gen_server
start_link(Name) when is_atom(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []);
start_link(_) ->
  error(badarg).

%% @doc Stops the gen_server
stop(Name) ->
  gen_server:stop(Name).

%% @doc Creates a new table. Returns error if table already exists.
-spec create(atom(), atom()) -> ok | error.
create(Table, Server) ->
  gen_server:call(Server, {create, Table}).

%% @doc Deletes a table. Returns error if table does not exist.
-spec delete(atom(), atom()) -> ok | error.
delete(Table, Server) ->
  gen_server:call(Server, {delete, Table}).

%% @doc Returns all the tables handled by the server
-spec all(atom()) -> [atom()].
all(Server) ->
  gen_server:call(Server, all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour Callbacks

%% @doc init
init([]) ->
  {ok, #{tables => []}}.

%% @doc handle_call
%% Create a table
handle_call({create_table, Name}, _, State = #{tables := Tables}) ->
  Options = [
             set,
             public,
             named_table,
             {write_concurrency, false},
             {read_concurrency, true}
            ],
  try ets:new(Name, Options) of
      _ -> {reply, ok, State#{tables := [Name|Tables]}}
  catch
    error:badarg -> {reply, error, State}
  end;
%% Delete a table
handle_call({delete_table, Name}, _, State = #{tables := Tables}) ->
  try ets:delete(Name) of
      _ -> {reply, ok, State#{tables := lists:delete(Name, Tables)}}
  catch
    error:badarg -> {reply, error, State}
  end;
%% Return all tables
handle_call(all, _, State = #{tables := Tables}) ->
  {reply, Tables, State};
handle_call(_, _, State) ->
  {noreply, State}.

%% @doc handle_cast
handle_cast(_, State) ->
  {noreply, State}.

%% @doc handle_info
handle_info(Message, Table) ->
  bn_log:error(?MODULE, ?LINE, "Unexpected message ~p", [Message]),
  {noreply, Table}.

%% @doc terminate
terminate(_, _) ->
  bn_log:info(?MODULE, ?LINE, "Terminating"),    
  {noreply, ok}.

%% @doc code_change
code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

start_stop_test() ->
  ?_test(
     begin
       {ok, _} = start_link(test_server),
       ok = stop(test_server)
     end
    ).

create_tables_test() ->
  ?_test(
     begin
       Server = test_server,
       start_link(Server),

       ?assertEqual([], all(Server)),

       ?assertEqual(ok, create(table_1, Server)),
       ?assertEqual([table_1], all(Server)),

       ?assertEqual(ok, create(table_2, Server)),
       ?assertEqual([table_2, table_1], all(Server)),

       ?assertEqual(error, create(table_1, Server)),

       ?assertEqual(ok, delete(table_1, Server)),
       ?assertEqual([table_2], all(Server)),
       ?assertEqual(error, delete(table_1, Server)),

       ?assertEqual(ok, create(table_1, Server)),
       ?assertEqual([table_1, table_2], all(Server)),
       
       ?assertEqual(ok, delete(table_1, Server)),
       ?assertEqual([table_2], all(Server)),

       ?assertEqual(ok, delete(table_2, Server)),
       ?assertEqual([], all(Server)),

       stop(Server)
     end).

%% Tests end
-endif.
