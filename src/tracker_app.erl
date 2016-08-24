%% @doc tracker public API
%% @end

-module(tracker_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

-export([defaults/0]).

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
    {ok, _} = cowboy:start_http(http, 100,
                                [{port, maps:get(listen_port, opts())}],
                                [{env, [{dispatch, Dispatch}]}]),
    tracker_sup:start_link(defaults()).

stop(_State) ->
    ok.

defaults() ->
    #{listen_port => 7890}.

opts() ->
    opts([]).

opts(LOverrides) ->
    Overrides = maps:from_list(LOverrides),
    Env = maps:from_list(application:get_all_env(tracker)),
    maps:merge(maps:merge(defaults(), Env), Overrides).

%%
%% Internal functions
%%
