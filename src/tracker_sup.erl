%% @doc tracker top level supervisor.
%% @end

-module(tracker_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%
%% API functions
%%

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%
%% Supervisor callbacks
%%

init(Opts) ->
    tr_device_location_node:init_mapping(),
    lager:start(),
    {ok, {flags(), [tr_node_sup(Opts),
                    tr_source_sup(Opts),
                    couchdb_driver_sup(Opts),
                    tr_turbo_subscriber(Opts)]}}.

%%
%% Internal functions
%%

flags() ->
    #{strategy => one_for_one}.

tr_node_sup(Opts) ->
    #{id => tr_node_sup,
      start => {tr_node_sup, start_link, [Opts]},
      type => supervisor}.

tr_source_sup(Opts) ->
    #{id => tr_source_sup,
      start => {tr_source_sup, start_link, [Opts]},
      type => supervisor}.

couchdb_driver_sup(Opts) ->
    #{
        id      => couchdb_driver_sup,
        start   => {couchdb_driver_sup, start_link, [Opts]},
        type    => supervisor
    }.

tr_turbo_subscriber(Opts) ->
  #{id => subscriber,
    start => {particle_subscriber, start_link, [Opts]},
    type => worker}.

