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
    {ok, {flags(), [tr_source_sup(Opts)]}}.

%%
%% Internal functions
%%

flags() ->
    #{strategy => one_for_one}.

tr_source_sup(Opts) ->
    #{id => tr_source_sup,
      start => {tr_source_sup, start_link, [Opts]},
      type => supervisor}.
