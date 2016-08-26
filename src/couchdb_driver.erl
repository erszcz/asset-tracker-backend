-module(couchdb_driver).
-author("Rafal Slota").
-behaviour(gen_server).

%% API
-export([
    start_link/1, list_dbs/0, save_doc/2
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
%%    couchbeam_app:start([], []),
    hackney:start(),
    ok = application:start(couchbeam),
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
    {ok, DBRef} = couchbeam:open_db(server_ref(), encode_dbname(DBName)),
    DocID = gen_uuid(),
    RawDoc = {maps:to_list(Doc#{<<"_id">> => DocID})},
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


gen_uuid() ->
    base64:encode(crypto:strong_rand_bytes(32)).

encode_dbname(Bin) ->
    list_to_binary([ 97 + Char || <<Char:4>> <= Bin]).

decode_dbname(DBName) ->
    << <<Char:4>> || Char <- [ Char - 97 || <<Char:8>> <= DBName]>>.


