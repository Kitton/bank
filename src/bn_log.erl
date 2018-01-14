%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Logging functions

-module(bn_log).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Includes

%% Exports
-export([
         info/3,
         info/4,
         error/3,
         error/4
        ]).

%% Macro definitions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Logs an info message with no arguments
info(Module, Line, Message) ->
  info(Module, Line, Message, []).

%% @doc Logs an info message
info(Module, Line, Format, Args) ->
  error_logger:info_msg("~p ~p ~p: " ++ Format ++ "~n", [Module, Line, self()] ++ Args).

%% @doc Logs an error message with no arguments
error(Module, Line, Message) ->
  bn_log:error(Module, Line, Message, []).

%% @doc Logs an error message
error(Module, Line, Format, Args) ->
  error_logger:error_msg("~p ~p ~p: " ++ Format ++ "~n", [Module, Line, self()] ++ Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

%% @doc Test functions that should always succeed (ie. testing them
%% does not depend on the database or external states)
basic_test_() ->
  [
   ?_test(
      bn_log:error(?MODULE, ?LINE, "This is an error message")
     ),
   ?_test(
      bn_log:info(?MODULE, ?LINE, "This is an info message")
     ),
   ?_test(
      bn_log:error(?MODULE, ?LINE, "This is an error message ~p", [1])
     ),
   ?_test(
      bn_log:info(?MODULE, ?LINE, "This is an info message ~p", [2])
     )
  ].

%% Tests end
-endif.
