%% @author Juan Luis Gamella Martin
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Inter-bank consolidations handler

-module(bn_r_consolidations).


%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Includes

%% Exports
-export([init/2]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([malformed_request/2]).
-export([process_post/2]).

%% Macro definitions

%% @doc Request fields
-define(FIELDS, [
                 {<<"type">>, true},
                 {<<"sender">>, true},
                 {<<"receiver">>, true},
                 {<<"value">>, true},
                 {<<"currency">>, true},
                 {<<"commission">>, true},
                 {<<"created">>, true},
                 {<<"preconsolidated">>, false},
                 {<<"consolidated">>, false},
                 {<<"failed">>, false}
                ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

init(Req, State) ->
    {cowboy_rest, Req, State}.

rest_init(Req, []) ->
  {ok, Req, #{}}.

allowed_methods(Req, State) ->
  Allowed = [<<"POST">>],
  Method = cowboy_req:method(Req),
  Req1 =
    case lists:member(Method, Allowed) of
      true ->
        Req;
      false ->
        {stop, bn_error:error_resp(Req, {method_not_allowed, #{}}), State}
    end,
  {Allowed, Req1, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, process_post}], Req, State}.

malformed_request(Req, State) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req),
  try jiffy:decode(Body, [return_maps]) of
      Data ->
      case bn_utils:parse(Data, ?FIELDS, fun parse/2) of
        {Transfer, [], []} ->
          {false, Req1, #{transfer => Transfer}};
        {_, NonValid, Missing} ->
          Req2 = bn_error:error_resp(Req1, {bad_request, #{non_valid => NonValid, missing => Missing}}),
          {true, Req2, State}
      end
  catch
    _:_ ->
      Req2 = bn_error:error_resp(Req1, {bad_request, #{}}),
      {true, Req2, State}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET
process_post(Req, State) ->
  #{transfer := Transfer} = State,
  case bn_logic:consolidate_external_in(Transfer) of
    {ok, Consolidated} ->
      JSON = jiffy:encode(Consolidated),
      {true, cowboy_req:set_resp_body(JSON, Req), State};
    {error, Error} ->
      Req1 = bn_error:error_resp(Req, Error),
      {stop, cowboy_req:reply(400, Req1), State}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

-spec parse(binary(), binary()) -> {atom(), term()} | non_valid.
parse(<<"type">>, Value) ->
  %% NOTE (new transfers): Refactor this if new transfer types are allowed
  case Value of
    <<"internal">> ->
      {type, internal};
    <<"external">> ->
      {type, external};
    _ ->
      non_valid
  end;
parse(<<"sender">>, Value) ->
    {sender, bn_utils:to_binary(Value)};
parse(<<"receiver">>, Value) ->
    {receiver, bn_utils:to_binary(Value)};
parse(<<"value">>, Value) ->  case bn_utils:to_integer(Value) of
    N when N > 0 ->
      {value, N};
    _ ->
      non_valid
  end;
parse(<<"currency">>, Value) ->
  {currency, bn_utils:to_binary(Value)};
parse(<<"commission">>, Value) ->
  case bn_utils:to_integer(Value) of
    N when N > 0 ->
      {commission, N};
    _ ->
      non_valid
  end;
parse(<<"created">>, Value) ->
    case bn_utils:to_integer(Value) of
    N when N >= 0 ->
      {created, N};
    _ ->
      non_valid
  end;
parse(<<"preconsolidated">>, Value) ->
    case bn_utils:to_integer(Value) of
    N when N >= 0 ->
      {preconsolidated, N};
    _ ->
      non_valid
  end;
parse(<<"consolidated">>, Value) ->
    case bn_utils:to_integer(Value) of
    N when N >= 0 ->
      {consolidated, N};
    _ ->
      non_valid
  end;
parse(<<"failed">>, Value) ->
    case bn_utils:to_integer(Value) of
    N when N >= 0 ->
      {failed, N};
    _ ->
      non_valid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

%% @doc Test functions that should always succeed (ie. testing them
%% does not depend on the database or external states)
basic_test_() ->
  [].

%% Tests end
-endif.
