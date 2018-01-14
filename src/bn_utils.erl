%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Utils module

-module(bn_utils).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour

%% Includes

%% Exports
-export([
         parse/3,
         to_integer/1,
         to_binary/1
        ]).            

%% Macro definitions

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Parses the required params from a map
-spec parse(maps:map(), [atom()], function()) -> {maps:map(), [atom()], [atom()]}.
parse(Map, Params, ParseFun) ->
  lists:foldl(fun(Key, {Ok, NonValid, Missing}) ->
                    case maps:get(Key, Map, null) of
                      null ->
                        {Ok, NonValid, [Key|Missing]};
                      Value ->
                        case ParseFun(Key, Value) of
                          {K, V} ->
                            {Ok#{K => V}, NonValid, Missing};
                          non_valid ->
                            {Ok, [Key|NonValid], Missing}
                        end
                    end
                end,
              {#{}, [], []},
              Params).

%% @doc Transforms to binary
-spec to_binary(atom() | list(byte()) | binary()) -> binary().
to_binary(Value) when is_binary(Value) ->
  Value;
to_binary(Value) when is_list(Value) ->
  list_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
  atom_to_binary(Value, utf8).

%% @doc Transforms to binary
-spec to_integer(integer() | string() | binary()) -> integer() | non_valid.
to_integer(Value) when is_integer(Value) ->
  Value;
to_integer(Value) when is_list(Value)  ->
  try list_to_integer(Value) catch error:badarg -> non_valid end;
to_integer(Value) when is_binary(Value)  ->
  try binary_to_integer(Value) catch error:badarg -> non_valid end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  ParseFun =
    fun (<<"a">>, X) -> {a, X};
        (<<"b">>, X) -> {b, X};
        (<<"c">>, X) -> {c, X}
    end,
  [
   ?_assertEqual({#{a => 1, b => 2, c => 3}, [], []},
                 parse(#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3}, [<<"a">>, <<"b">>, <<"c">>], ParseFun)),
   ?_assertEqual({#{a => 1, b => 2}, [], [<<"c">>]},
                 parse(#{<<"a">> => 1, <<"b">> => 2}, [<<"a">>, <<"b">>, <<"c">>], ParseFun)),
   ?_assertEqual({#{a => 1, b => 2}, [], []},
                 parse(#{<<"a">> => 1, <<"b">> => 2, <<"d">> => 3}, [<<"a">>, <<"b">>], ParseFun))
  ].

%% Tests end
-endif.
