-module(couchdb_driver).
-author("Rafal Slota").
-behaviour(gen_server).

%% API
-export([
    start_link/1, list_dbs/0, save_doc/2, list_docs/2, db_ref/1
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-type state() :: #{
    server => term()
}.

%% API functions

start_link(_Opts) ->
    %% Just for ./rebar3 shell, release version starts those deps automatically
    ok = ensure_deps_started(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec list_dbs() -> {ok, [binary()]} | no_return().
list_dbs() ->
    case couchbeam:all_dbs(server_ref()) of
        {ok, DBList} ->
            {ok, [decode_dbname(DBName) || DBName <- DBList]};
        {error, Reason} ->
            {error, Reason}
    end.

save_doc(DBName, Doc) ->
    DBRef = db_ref(DBName),
    RawDoc = {maps:to_list(Doc)},
    case couchbeam:save_doc(DBRef, RawDoc) of
        {ok, _} ->
            ok;
        {error, not_found} ->
            {ok, _} = couchbeam:create_db(server_ref(), encode_dbname(DBName)),
            save_doc(DBName, Doc);
        {error, Reason} ->
            lager:info("save_doc error ~p", [{encode_dbname(DBName), Reason}]),
            {error, Reason}
    end.


list_docs(DBName, Limit) ->
    case couchbeam_changes:follow_once(db_ref(DBName), [{descending, true}, include_docs, {limit, Limit}]) of
        {ok, _, Docs} ->
            DocMaps = lists:map(
                fun({RawDoc}) ->
                    {DocElements} = proplists:get_value(<<"doc">>, RawDoc),
                    DocMap0 = maps:from_list(DocElements),
                    DocMap1 = maps:remove(<<"_id">>, DocMap0),
                    maps:remove(<<"_rev">>, DocMap1)
                end, Docs),
            {ok, DocMaps};
        {error, Reason} ->
            {error, Reason}
    end.


%% gen_server callbacks

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


server_ref() ->
    %% @todo: make this URL configurable
    Url = "http://localhost:5984",
    Options = [],
    couchbeam:server_connection(Url, Options).

db_ref(DBName) ->
    {ok, DBRef} = couchbeam:open_db(server_ref(), encode_dbname(DBName)),
    DBRef.


encode_dbname(Bin) ->
    list_to_binary([ 97 + Char || <<Char:4>> <= Bin]).

decode_dbname(DBName) ->
    << <<Char:4>> || Char <- [ Char - 97 || <<Char:8>> <= DBName]>>.


ensure_deps_started() ->
    case hackney:start() of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok
    end,

    case application:start(couchbeam) of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.