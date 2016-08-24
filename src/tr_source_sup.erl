-module(tr_source_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

init(Opts) ->
    {ok, {flags(), [source_spec(Opts)]}}.

flags() ->
    #{strategy => simple_one_for_one}.

source_spec(Opts) ->
    #{id => tr_device_location_source,
      start => {tr_device_location_source, start_link, [Opts]},
      type => worker}.
