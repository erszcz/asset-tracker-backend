-module(tr_devices_handler).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    State = [],
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req0, State) ->
    {IsPretty, Req1} = cowboy_req:qs_val(<<"pretty">>, Req0),
    DevicesList = get_devices(),
    Reply = get_reply(DevicesList, IsPretty),
    {Reply, Req1, State}.

%% Internal

get_devices() ->
    [this, is, my, device].

get_reply(DevicesList, <<"true">>) ->
    tr_json:pretty_encode(#{nodes => DevicesList});

get_reply(DevicesList, _) ->
    tr_json:encode(#{nodes => DevicesList}).
