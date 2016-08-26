%% @doc tracker public API
%% @end

-module(tracker_app).

-behaviour(application).
%% set your particle login and password to those os variables
-define(LOGIN_ENV, "LOGIN_ENV").
-define(PASS_ENV, "PASS_ENV").
%% Application callbacks
-export([start/2,
         stop/1]).

-export([opts/0]).

%%
%% API
%%

start(_StartType, _StartArgs) ->
    Routes = [
              {'_', [
                     {"/", cowboy_static, {priv_file, tracker, "index.html"}},
                     {"/device/:device/location-stream", tr_device_location_ws, []},
                     {"/device/:device/location", tr_device_location, []},
                     {"/devices", tr_devices_handler, []},
                     {"/static/[...]", cowboy_static, {priv_dir, tracker, "static"}}
                    ]}
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(http, 100,
                                [{port, maps:get(listen_port, opts())}],
                                [{env, [{dispatch, Dispatch}]}]),
    tracker_sup:start_link(opts()),
    tr_current_state:start_link().




stop(_State) ->
    ok.

defaults() ->
    #{listen_port => 7890}.

opts() ->
    opts([{login, get_login()}, {pass, get_pass()}]).

opts(LOverrides) ->
    Overrides = maps:from_list(LOverrides),
    Env = maps:from_list(application:get_all_env(tracker)),
    maps:merge(maps:merge(defaults(), Env), Overrides).

%%
%% Internal functions
%%

get_login() ->
  get_var(?LOGIN_ENV).

get_pass() ->
  get_var(?PASS_ENV).

get_var(Var) ->
  case os:getenv(Var) of
    false ->
%%      lager:error("Variable ~p is not set", [Var]),
      throw({variable_not_set, Var});
    Val ->
      Val
  end.