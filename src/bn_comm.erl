%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module manages communication between banks

-module(bn_comm).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour

%% Includes

%% Exports
-export([
         consolidate/2
        ]).

%% Macro definitions

%% Type Definitions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

-spec consolidate(binary(), bn_model:transfer()) ->
                     {ok, bn_model:transfer()} |
                     {wait, bn_model:transfer()} |
                     {error, term()}.
consolidate(Bank, Transfer = #{id := Id}) ->
  io:format("~s~n", [Bank]),
  case bank_endpoint(Bank) of
    error ->
      {error, {unknown_bank, Bank}};
    Endpoint ->
      case rand:uniform(99) of
        N when N < 30 -> %% Simulated success rate of 30%
          io:format("1~n"),
          {error, connection_error};
        _ ->
          Response =
            ibrowse:send_req(Endpoint,
                             [{"Content-Type", "application/json"}],
                             post,
                             jiffy:encode(Transfer),
                             []),
          {ok, Updated} = bn_dal:update_transfer(Id, #{preconsolidated => bn_time:now()}),
          case Response of
            {error, Reason} ->
              io:format("1~n"),
              {error, Reason};
            {ok, "200", _, Body} ->
              try jiffy:decode(Body, [return_maps]) of
                  Transfer = #{<<"consolidated">> := null} ->
                  {wait, Updated};
                  #{<<"consolidated">> := C} ->
                  {ok, _} =
                    bn_dal:update_transfer(Id,
                                           #{consolidated => bn_utils:to_integer(C)})
              catch
                _:_ ->
                  io:format("1~n"),
                  {error, {bad_json, Body}}
              end;
            {ok, Code, _, _} ->
              io:format("4~n"),
              {error, {http_error, Code}}
          end
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

%% @doc Returns the bank's API endpoint
-spec bank_endpoint(binary()) -> string() | error.
bank_endpoint(<<"000A">>) ->
  "http://localhost:8001/interbank/consolidations";
bank_endpoint(<<"000B">>) ->
  "http://localhost:8002/interbank/consolidations";
bank_endpoint(_) ->
  error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

basic_test_() ->
  [
   ?_assertEqual("http://localhost:8001/interbank/consolidations", bank_endpoint(<<"000A">>)),
   ?_assertEqual("http://localhost:8002/interbank/consolidations", bank_endpoint(<<"000B">>)),
   ?_assertEqual(error, bank_endpoint(<<"ZZZZ">>)),
   ?_assertEqual({error, {unknown_bank, <<"ZZZZ">>}}, consolidate(<<"ZZZZ">>, #{id => <<"123">>}))
  ].

%% Tests end
-endif.
