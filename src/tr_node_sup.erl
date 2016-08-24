-module(tr_node_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

init(Opts) ->
    {ok, {flags(), [node_spec(Opts)]}}.

flags() ->
    #{strategy => simple_one_for_one}.

node_spec(Opts) ->
    #{id => tr_device_location_node,
      start => {tr_device_location_node, start_link, [Opts]},
      restart => temporary,
      type => worker}.
