-module(tr_devices_handler).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-define(LIMIT, 1).

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
    DevicesList = get_devices(?LIMIT),
%    TestDevices = [get_sample_device(<<"rower">>),
%               get_sample_device(<<"pies">>)],
    Reply = get_reply(DevicesList, IsPretty),
    {Reply, Req1, State}.

%% Internal

get_devices(Limit) ->
    {ok, Devices1} = location_store:get_devices(),
    Devices2 = lists:filter(
                 fun(DBName) ->
                         (DBName =/= <<225,79,184,32,62,1:4>>) and
                         (DBName =/= <<228,36,18>>) and
                         (DBName =/= <<"devices">>) and
                         (DBName =/= <<"123">>)
                 end,
                 Devices1),
    get_devices_with_location(Devices2, Limit).

get_devices_with_location(Devices, Limit) ->
    lists:map(
      fun(Device) ->
              {ok, [Location]} = location_store:get_current_locations(Device, Limit),
              DeviceName = case location_store:get_device_name(Device) of
                               {error, _} -> <<"unknown">>;
                               {ok, Name} -> Name
                           end,
              Result = maps:put(<<"display_name">>, DeviceName, Location),
              Result
      end,
      Devices).

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
