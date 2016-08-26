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
    {Limit, Req2} = cowboy_req:qs_val(<<"limit">>, Req1, <<"1">>),
% TODO: Uncommenct this line to get queries from real DB
% TODO: DevicesList = get_devices(Limit),
    TestDevices = [get_sample_device(<<"rower">>),
               get_sample_device(<<"pies">>)],
    Reply = get_reply(TestDevices, IsPretty),
    {Reply, Req2, State}.

%% Internal

get_devices(LimitBin) ->
    Limit = erlang:binary_to_integer(LimitBin),
    {ok, Devices} = location_store:get_devices(),
    get_devices_with_location(Devices, Limit).

get_devices_with_location(Devices, Limit) ->
    Locations = lists:map(
                  fun(Device) ->
                          location_store:get_current_locations(Device, Limit)
                  end,
                  Devices),
    lists:zip(Devices, Locations).

get_reply(DevicesList, <<"true">>) ->
    tr_json:pretty_encode(DevicesList);

get_reply(DevicesList, _) ->
    tr_json:encode(DevicesList).

get_sample_device(Name) ->
    #{<<"display_name">> => Name,
      <<"data">> => <<"50.066902,19.929014">>,
      <<"ttl">> => <<"60">>,
      <<"published_at">> => <<"2016-08-17T10:28:03.512Z">>,
      <<"coreid">> => <<"4f0047001951343334363036">>}.
