-module(tr_db_source).
-behaviour(gen_changes).

%% API
-export([spec/2, start_link/2]).

%% gen_changes callbacks
-export([init/1, handle_call/3, handle_info/2, handle_change/2, handle_cast/2, terminate/2]).

-define(SERVER, ?MODULE).


%%
%% API
%%

spec(DeviceID, NodePid) ->
    #{id => {?MODULE, DeviceID},
        start => {?MODULE, start_link, [DeviceID, NodePid]},
        restart => transient,
        type => worker}.

start_link(DeviceID, NodePid) ->
    DBRef = couchdb_driver:db_ref(DeviceID),
    lager:info("Starting DB source: ~p", [{DeviceID, DBRef}]),
    couchdb_driver:create_db(DeviceID),
    {ok, {InfoList}} = couchbeam:db_info(couchdb_driver:db_ref(DeviceID)),
    Since0 = proplists:get_value(<<"update_seq">>, InfoList),
    Since = max(0, Since0 - 30),
    Opts = [{<<"include_docs">>, <<"true">>}, {since, Since}],
    gen_changes:start_link(?MODULE, DBRef, Opts, [DeviceID, NodePid]).

%%
%% gen_changes callbacks
%%

-type gen_changes_state() :: #{device_id => binary(), node_pid => pid()}.

-spec init(Opts :: [term()]) ->
    {ok, gen_changes_state()}.
init([DeviceID, NodePid]) ->
    {ok, #{device_id => DeviceID, node_pid => NodePid}}.

-spec handle_change(term(), gen_changes_state()) -> {noreply, gen_changes_state()} | {stop, normal, gen_changes_state()}.
handle_change({done, _LastSeq}, State) ->
    {noreply, State};
handle_change({change, {ChangeProplist}}, State = #{node_pid := NodePid}) ->
    RawDoc = proplists:get_value(<<"doc">>, ChangeProplist),
    DocMap0 = maps:from_list(RawDoc),
    DocMap1 = maps:remove(<<"_id">>, DocMap0),
    DocMap2 = maps:remove(<<"_rev">>, DocMap1),
%%    lager:info("Change: ~p", [DocMap2]),
    gen_event:notify(NodePid, process_event(DocMap2)),

    {noreply, State};

handle_change(UnknownChange, State) ->
    lager:warning("Unknown stream response: ~p", [UnknownChange]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle_call/3 callback for gen_changes server.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), pid(), gen_changes_state()) -> {reply, term(), gen_changes_state()}.
handle_call(_Req, _From, State) ->
    {reply, _Req, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle_cast/2 callback for gen_changes server.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), gen_changes_state()) -> {noreply, gen_changes_state()}.
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle_info/2 callback for gen_changes server.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), gen_changes_state()) -> {noreply, gen_changes_state()}.
handle_info({error, {closed, timeout}}, State) ->
    {stop, stream_end, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% terminate/2 callback for gen_changes server.
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), gen_changes_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


%%
%% Internal functions
%%

process_event(Event) ->
    tr_json:encode(Event).
