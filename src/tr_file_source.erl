-module(tr_file_source).
-behaviour(gen_server).

%% API
-export([spec/2,
         start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%
%% API
%%

spec(DeviceID, NodePid) ->
    #{id => {?MODULE, DeviceID},
      start => {?MODULE, start_link, [DeviceID, NodePid]},
      restart => transient,
      type => worker}.

start_link(DeviceID, NodePid) ->
    gen_server:start_link(?MODULE, [DeviceID, NodePid], []).

%% 
%% gen_server callbacks
%% 

init([DeviceID, NodePid]) ->
    Events = prepare_events("gps-event-stream-clean.txt"),
    {ok, #{device_id => DeviceID,
           node_pid => NodePid,
           events => Events,
           current => 1}, timeout()}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #{node_pid := NodePid, events := Events,
                       current := Current} = S) ->
    gen_event:notify(NodePid, process_event(lists:nth(Current, Events))),
    {noreply, S#{current := succ(Current, length(Events))}, timeout()};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%

prepare_events(FileName) ->
    {ok, Data} = file:read_file(FileName),
    re:split(Data, <<"\n">>, [trim, notempty]).

succ(N, N) -> 1;
succ(I, _) -> I + 1.

timeout() ->
    timer:seconds(1).

process_event(BEvent) ->
    Ev = tr_json:decode(BEvent),
    tr_json:encode(Ev#{<<"event">> => <<"G">>,
                       <<"published_at">> := ts()}).

ts() ->
    TS = ec_date:format_iso8601(qdate:to_date(os:timestamp())),
    iolist_to_binary(TS).
