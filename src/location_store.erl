-module(location_store).
-author("Rafal Slota").

-type device_attributes() :: #{binary() => binary()}.

%% API
-export([get_devices/0, register_location/2, get_current_locations/2]).
-export([set_device_name/2, get_device_name/1]).

-spec get_devices() ->
    {ok, [binary()]} | {error, Reason :: any()}.
get_devices() ->
    couchdb_driver:list_dbs().

-spec register_location(DeviceId :: binary(), Attributes :: device_attributes()) ->
    ok | {error, Reason :: any()}.
register_location(DeviceId, Attributes) ->
    couchdb_driver:save_doc(DeviceId, Attributes).

-spec get_current_locations(DeviceId :: binary(), Limit :: non_neg_integer()) ->
    {ok, [device_attributes()]} | {error, Reason :: any()}.
get_current_locations(DeviceId, Limit) when Limit > 0 ->
    couchdb_driver:list_docs(DeviceId, Limit).


-spec set_device_name(DeviceId :: binary(), Name :: binary()) ->
    ok | {error, Reason :: term()}.
set_device_name(DeviceId, Name) ->
    couchdb_driver:save_doc(<<"devices">>, DeviceId, #{<<"name">> => Name}).


-spec get_device_name(DeviceId :: binary()) ->
    {ok, Name :: binary()} | {error, not_found} | {error, Reason :: term()}.
get_device_name(DeviceId) ->
    case couchdb_driver:get_doc(<<"devices">>, DeviceId) of
        {ok, #{<<"name">> := Name}} ->
            {ok, Name};
        {error, Reason} ->
            {error, Reason}
    end .
