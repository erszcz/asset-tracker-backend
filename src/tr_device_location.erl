%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 12:57
%%%-------------------------------------------------------------------
-module(tr_device_location).
-author("bartekgorny").
-behaviour(cowboy_http_handler).

%% API
-export([init/3, handle/2, terminate/3]).

init(_, Req, _) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {DeviceID, Req2} = get_device(Req),
    Loc = case tr_current_state:get_state(DeviceID) of
              {ok, undefined} -> <<"unknown">>;
              {ok, O} -> O
          end,
    Resp = tr_json:encode(Loc),
    {ok, Req3} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], Resp, Req2),
    {ok, Req3, State}.

terminate(_, _, _) ->
    ok.

get_device(Req) ->
    case cowboy_req:binding(device, Req) of
        undefined -> error(undefined_device, [Req]);
        Device -> Device
    end.
