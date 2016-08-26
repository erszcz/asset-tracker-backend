-module(couchdb_driver).
-author("Rafal Slota").
-behaviour(gen_server).

%% API
-export([
    start_link/1, list_dbs/0, save_doc/2, save_doc/3, list_docs/2, db_ref/1, create_db/1, get_doc/1, get_doc/2
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
-define(DEVICES_DB, <<"devices">>).

%% Local types
-type simple_doc() :: #{binary() => binary()}.
-type dbname() :: binary().

%% API functions

start_link(_Opts) ->
    Res = create_db(?DEVICES_DB),
    lager:info("Create ~p", [{?DEVICES_DB, Res}]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


create_db(DBName) ->
    couchbeam:create_db(server_ref(), encode_dbname(DBName)).


-spec list_dbs() -> {ok, [dbname()]} | no_return().
list_dbs() ->
    case couchbeam:all_dbs(server_ref()) of
        {ok, DBList} ->
            {ok, [decode_dbname(DBName) || DBName <- DBList]};
        {error, Reason} ->
            {error, Reason}
    end.

save_doc(DBName, Doc) ->
    save_doc(DBName, gen_uuid(), Doc).
save_doc(DBName, UUID, Doc) ->
    DBRef = db_ref(DBName),
    RawDoc = {maps:to_list(Doc#{<<"_id">> => UUID})},
    case couchbeam:save_doc(DBRef, RawDoc) of
        {ok, _} ->
            ok;
        {error, not_found} ->
            {ok, _} = create_db(DBName),
            save_doc(DBName, UUID, Doc);
        {error, Reason} ->
            lager:error("save_doc error ~p", [{encode_dbname(DBName), Reason}]),
            {error, Reason}
    end.

get_doc(DocId) ->
    get_doc(?DEVICES_DB, DocId).
get_doc(DBName, DocId) ->
    DBRef = db_ref(DBName),
    case couchbeam:open_doc(DBRef, DocId) of
        {ok, {DocElements}} ->
            DocMap0 = maps:from_list(DocElements),
            DocMap1 = maps:remove(<<"_id">>, DocMap0),
            {ok, maps:remove(<<"_rev">>, DocMap1)};
        {error, Reason} ->
            {error, Reason}
    end.


-spec list_docs(DBName :: dbname(), Limit :: non_neg_integer()) ->
    {ok, [simple_doc()]} | {error, Reason :: any()}.
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


-spec server_ref() -> ServerRef :: term().
server_ref() ->
    %% @todo: make this URL configurable
    Url = "http://localhost:5984",
    Options = [],
    couchbeam:server_connection(Url, Options).

-spec db_ref(DBName :: dbname()) -> DBRef :: term().
db_ref(DBName) ->
    {ok, DBRef} = couchbeam:open_db(server_ref(), encode_dbname(DBName), [{recv_timeout, timer:minutes(5)}]),
    DBRef.

-spec encode_dbname(dbname()) -> binary().
encode_dbname(Bin) ->
    list_to_binary([ 97 + Char || <<Char:4>> <= Bin]).

-spec decode_dbname(binary()) -> dbname().
decode_dbname(DBName) ->
    << <<Char:4>> || Char <- [ Char - 97 || <<Char:8>> <= DBName]>>.

gen_uuid() ->
    base64:encode(crypto:strong_rand_bytes(16)).