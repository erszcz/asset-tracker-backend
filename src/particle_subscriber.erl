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
-define(TIMEDIFF, 5000).
-define(PARTICLE, "https://api.particle.io").
-define(TOKEN, <<"/oauth/token/">>).
-define(EVENTS, <<"/v1/devices/events/G/">>).
-define(LIST, <<"/v1/devices/">>).
%% API
-export([handle_info/2, handle_cast/3, handle_call/3, init/1, start_link/1, terminate/2]).

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
      lager:info("Error during authentication!~n"),
      {stop, {bad_resposne, R}, State};
    Token ->
%%      lager:info("Successfully authenticated!~n"),
      erlang:send_after(?TIMEDIFF, self(), request, []),
      {noreply, maps:merge(State, Token)}
  end;
handle_info(request, State) ->
  try
    Data = get_stream(State),
    io:format("request data ~n~p", [Data]),
    erlang:send_after(?TIMEDIFF, self(), request, []),
    {noreply, State}
  catch Err:R ->
  %  lager:error("Error during sending request ~p~n", [{Err, R}],
      {stop, {Err, R}, State}
  end.

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
      io:format("Bad Response!~n~p",[Res]),
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
  case Body of
    <<":ok\n\n\n">> ->
      ok;
    _ ->
      get_response_body_raw(D)
  end.

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

get_response_body_raw(Response) ->
  {ok, {_, _, Body, _ , _}} = Response,
  BodyList = binary_to_list(Body),
  Start = string:str(BodyList, "{"),
  End = string:str(BodyList, "}"),
  case {Start, End} of
    {0, 0} -> {error, no_json, BodyList};
    _ ->
      Bin = list_to_binary(string:sub_string(BodyList, Start, End)),
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
  {ok, Pid} = fusco:start_link(?PARTICLE, []),
  R = fusco:request(Pid, Path, Method, Headers, Body, 5000),
  fusco:disconnect(Pid),
  R.

accessed_request(Path, Method, Headers, Body, TokenMap) ->
  {ok, AccessToken} = maps:find(<<"access_token">>, TokenMap),
  AccessedPath = list_to_binary(binary_to_list(Path) ++ "?access_token=" ++ binary_to_list(AccessToken)),
  request(AccessedPath, Method, Headers, Body).






