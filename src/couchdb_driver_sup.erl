-module(couchdb_driver_sup).
-author("Rafal Slota").
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
    #{
        strategy    => one_for_one
    }.


node_spec(Opts) ->
    #{
        id      => couchdb_driver,
        start   => {couchdb_driver, start_link, [Opts]},
        restart => temporary,
        type    => worker
    }.