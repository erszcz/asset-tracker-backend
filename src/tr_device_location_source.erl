-module(tr_device_location_source).
-behaviour(gen_event).

%% API
-export([start_link/1,
         get_existing_source/1,
         init_mapping/0,
         register_sink/2]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type state() :: #{sink => pid()}.

%%
%% API
%%

start_link(_Opts) ->
    gen_event:start_link().

get_existing_source(DeviceID) ->
    case ets:lookup(tr_device_event_sources, DeviceID) of
        [] -> error({no_event_source, DeviceID});
        [{DeviceID, SourcePid}] -> SourcePid
    end.

init_mapping() ->
    ets:new(tr_device_event_sources, [public, named_table, {read_concurrency, true}]).

register_sink(DeviceID, SinkPid) ->
    Source = get_source(DeviceID),
    erlang:monitor(process, Source),
    gen_event:add_sup_handler(Source, {tr_device_location_source, SinkPid}, SinkPid).

%%
%% gen_event callbacks
%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(SinkPid) ->
    {ok, #{sink => SinkPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(Event, #{sink := Pid} = S) ->
    Pid ! {event, Event},
    {ok, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(Info, #{sink := Pid} = S) ->
    Pid ! {info, Info},
    {ok, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%

get_source(DeviceID) ->
    Pid = case supervisor:start_child(tr_source_sup, []) of
              {ok, P} -> P;
              {ok, P, _} -> P;
              {error, Reason} -> error(Reason)
          end,
    case ets:insert_new(tr_device_event_sources, {DeviceID, Pid}) of
        false -> supervisor:terminate_child(tr_source_sup, Pid),
                 get_existing_source(DeviceID);
        true -> Pid
    end.
