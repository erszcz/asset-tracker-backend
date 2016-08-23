%% @doc tracker public API
%% @end

-module(tracker_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%
%% API
%%

start(_StartType, _StartArgs) ->
    Routes = [
              {'_', [
                     {"/", cowboy_static, {priv_file, tracker, "index.html"}},
                     {"/device/:device/location-stream", tr_device_location_ws, []},
                     {"/static/[...]", cowboy_static, {priv_dir, tracker, "static"}}
                    ]}
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    tracker_sup:start_link().

stop(_State) ->
    ok.

%%
%% Internal functions
%%
