%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 10:16
%%%-------------------------------------------------------------------
-module(particle_subscriber).
-author("ludwikbukowski").
-define(TIMEDIFF, 10).
-define(PARTICLE, "api.particle.io").
-define(PARTICLE_HTTPS, "https://api.particle.io").
-define(TOKEN, <<"/oauth/token/">>).
-define(EVENTS, <<"/v1/devices/events/G">>).
-define(EVENTS_SSE, <<"https://api.particle.io/v1/devices/events/G/">>).
-define(LIST, <<"/v1/devices/">>).
%% API
-export([handle_info/2, handle_cast/3, handle_call/3, init/1, start_link/1, terminate/2, list_devices/1,
  handle_event/3, sse_process/1]).

%% gen_server callbacks
start_link(Opts) ->
  gen_server:start_link({local, requestor}, ?MODULE, Opts, []).

init(Args) ->
  erlang:send_after(0, self(), get_token, []),
  {ok, Args}.

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, _, State) ->
  {reply, ok, State}.

handle_info(get_token, State) ->
  {ok, User} = maps:find(login, State),
  {ok, Pass} = maps:find(pass, State),
  TokenMap = get_token(User, Pass),
  case TokenMap of
    {error, _, R} ->
      lager:error("Error during authentication!"),
      {stop, {bad_resposne, R}, State};
    Token ->
      lager:info("Successfully authenticated for user ~p!", [User]),
      erlang:send_after(0, self(), get_name, []),
      {noreply, maps:merge(State, Token)}
  end;
handle_info(request, State) ->
  try
    io:format("go~n"),
    sse_process(State),
%%    Data = get_stream(State),
%%      case Data of
%%        ok -> ok;
%%        {error, Name, R} ->
%%          lager:warning("Unknown data ~p:~p!",[Name, R]),
%%          ok;
%%        Other when is_map(Other)->
%%          {ok, DeviceId} = maps:find(<<"coreid">>, Data),
%%          case location_store:register_location(DeviceId, Data) of
%%            ok ->
%%              ok;
%%            _ ->
%%              lager:error("Error during storing data ~p", [{DeviceId, Data}])
%%          end
%%      end,
%%    erlang:send_after(?TIMEDIFF, self(), request, []),
    {noreply, State}
  catch Err:Reason ->
    lager:error("Error during sending request ~p", [{Err, Reason}]),
    {stop, {Err, Reason}, State}
  end;
handle_info(get_name, State) ->
  [Devices] = list_devices(State),
  {ok, Name} = maps:find(<<"name">>, Devices),
  %location_store:set_device_name(Name),
  erlang:send_after(?TIMEDIFF, self(), request, []),
  {noreply, State}.

terminate(Reason, State) ->
  ok.

%% internals: particle specific
get_token(User, Pass) ->
  AuthHeader = create_auth_header({"particle", "particle"}),
  Accepted = create_accepted_headers(),
  Body = url_encode([{"grant_type", "password"}, {"username", User}, {"password", Pass}]),
  Res = request(?TOKEN, "POST", AuthHeader ++ Accepted, [Body]),
  case has_status_code(Res, 200) of
    false ->
      lagger:error("Bad Response! ~p",[Res]),
      {error, badresponse, Res};
    true ->
      get_response_body_json(Res)
  end.

list_devices(State) ->
  D = accessed_request(?LIST, "GET", [], [], State),
  get_response_body_json(D).

get_stream(State) ->
  D = accessed_request(?EVENTS, "GET", [], [], State),
  {ok, {_, _, Body, _ , _}} = D,
  get_response_body_raw(D).

%% internals: encoding x-www-form-urlencoded type
url_encode(Data) ->
  url_encode(Data,"").

url_encode([],Acc) ->
  Acc;

url_encode([{Key,Value}|R],"") ->
  url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++
    edoc_lib:escape_uri(Value));

url_encode([{Key,Value}|R],Acc) ->
  url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++
    edoc_lib:escape_uri(Value)).

%% internals: http specific
has_status_code(Response, Code) when is_integer(Code) ->
  {ok, {{ResCode, _}, _, _, _, _}} = Response,
  binary_to_integer(ResCode) == Code.

get_response_body_json(Response) ->
  {ok, {_, _, Body, _ , _}} = Response,
  tr_json:decode(Body).

get_response_body_raw(Body) ->
  BodyList = binary_to_list(Body),
  Stripped = re:replace(BodyList, "\\s+", "", [global,{return,list}]),
  Start = string:str(Stripped, "{\"data\":"),
  case Start of
    0 -> {error, no_json, BodyList};
    _ ->
      Bin = list_to_binary(string:sub_string(Stripped, Start)),
      tr_json:decode(Bin)
  end.

create_auth_header({User, Password}) ->
  Basic = list_to_binary("basic " ++ base64:encode_to_string(User ++ ":"++ Password)),
  [{<<"authorization">>, Basic}].

create_accepted_headers() ->
  [{<<"Content-Type">>, <<"application/json">>},
    {<<"Accept">>, <<"application/json">>},
    {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
    {<<"Accept">>, <<"application/x-www-form-urlencoded">>}].

request(Path, Method, Headers, Body) ->
  {ok, Pid} = fusco:start_link(?PARTICLE_HTTPS, []),
  R = fusco:request(Pid, Path, Method, Headers, Body, 5000),
  fusco:disconnect(Pid),
  R.

accessed_request(Path, Method, Headers, Body, TokenMap) ->
  {ok, AccessToken} = maps:find(<<"access_token">>, TokenMap),
  AccessedPath = list_to_binary(binary_to_list(Path) ++ "?access_token=" ++ binary_to_list(AccessToken)),
  request(AccessedPath, Method, Headers, Body).

sse_process(State) ->
  {ok, AccessToken} = maps:find(<<"access_token">>, State),
  AccessedPath = binary_to_list(?EVENTS) ++ "?access_token=" ++ binary_to_list(AccessToken),
  {ok, Conn} = shotgun:open(?PARTICLE, 443, https),
  Fun = fun handle_event/3,
  Options = #{async => true, async_data => sse, async_mode => sse, handle_event => Fun},
  {ok, Ref} = shotgun:get(Conn, AccessedPath, #{}, Options).


handle_event(_Fin, _Ref, Data) ->
  io:format("received ~p~n", [Data]),
  D = get_response_body_raw(Data),
  case D of
        ok -> ok;
        {error, Name, R} ->
          lager:warning("Unknown data ~p:~p!",[Name, R]),
          ok;
        Other when is_map(Other)->
          {ok, DeviceId} = maps:find(<<"coreid">>, D),
          case location_store:register_location(DeviceId, D) of
            ok ->
              ok;
            _ ->
              lager:error("Error during storing data ~p", [{DeviceId, D}])
          end
      end.
