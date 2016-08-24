-module(tr_source_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

init(_Opts) ->
    {ok, {flags(), [source_spec()]}}.

flags() ->
    #{strategy => simple_one_for_one}.

source_spec() ->
    #{id => tr_device_location_source,
      start => {tr_device_location_source, start_link, []},
      type => worker}.
