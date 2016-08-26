-module(location_store).
-author("Rafal Slota").

-type device_attributes() :: #{binary() => binary()}.

%% API
-export([get_devices/0, register_location/2, get_current_locations/2]).

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

