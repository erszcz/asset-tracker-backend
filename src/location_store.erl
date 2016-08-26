-module(location_store).
-author("Rafal Slota").

%% API
-export([get_devices/0, register_location/2]).

-spec get_devices() -> [binary()].
get_devices() ->
    couchdb_driver:list_dbs().

-spec register_location(DeviceId :: binary(), Attributes :: #{binary() => term()}) ->
    ok | no_return().
register_location(DeviceId, Attributes) ->
    couchdb_driver:save_doc(DeviceId, Attributes),
    ok.