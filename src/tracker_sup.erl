%% @doc tracker top level supervisor.
%% @end

-module(tracker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%
%% API functions
%%

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%
%% Supervisor callbacks
%%

init([]) ->
    {ok, {flags(), [tr_source_sup([])]}}.

%%
%% Internal functions
%%

flags() ->
    #{strategy => one_for_one}.

tr_source_sup(Opts) ->
    #{id => tr_source_sup,
      start => {tr_source_sup, start_link, [Opts]},
      type => supervisor}.
