-module(nats).
-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1, start_link/2, subscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {host, port, sock, state}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(atom() | list()) ->
                        {ok, pid()} |
                        {error, term()}.
start_link(Name) when is_atom(Name) ->
    start_link(Name, []);

start_link(Opts) when is_list(Opts) ->
    start_link(?MODULE, Opts).

-spec start_link(atom(), list()) -> {ok, pid()} | {error, term()}.
start_link(undefined, Opts) when is_list(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []);

start_link(Name, Opts) when is_atom(Name), is_list(Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Opts], []).

subscribe(NameOrPid, Channel) ->
    gen_server:call(NameOrPid, {sub, Channel}, 2000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%    {ok, State, Timeout} |
%%    ignore                             |
%%    {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Opts]) ->
    State = init_state(Opts),
    case connect(State) of
        State1 when is_record(State1, state) ->
            {ok, State1};
        Err ->
            {stop, Err}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%    {reply, Reply, State, Timeout} |
%%    {noreply, State} |
%%    {noreply, State, Timeout} |
%%    {stop, Reason, Reply, State} |
%%    {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({sub, Subject}, _From, #state{sock=Sock, state=connected}=State) ->
    gen_tcp:send(Sock, nats_proto:sub_request(Subject, "1")),
    inet:setopts(Sock, [{active, once}]),
    {reply, ok, State};

handle_call(_Msg, _From, State = #state{}) ->
    {reply, unknown_message, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%    {noreply, State, Timeout} |
%%    {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%    {noreply, State, Timeout} |
%%    {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, #state{sock=Sock, state=connecting}=State) ->
    case parse_data(Data) of
        {info, _} ->
            gen_tcp:send(State#state.sock, nats_proto:connect_response()),
            inet:setopts(Sock, [{active, once}])
    end,
    {noreply, State#state{state=connected}};

handle_info({tcp, Sock, Data}, #state{sock=Sock, state=connected}=State) ->
    case parse_data(Data) of
        ping ->
            gen_tcp:send(State#state.sock, nats_proto:pong_response()),
            inet:setopts(Sock, [{active, once}]);
        {msg, Subject, _Sid, _ReplyTo, Body} ->
            io:format("received from topic ~p message ~p~n", [Subject, Body]),
            inet:setopts(Sock, [{active, once}]);
        _ ->
            inet:setopts(Sock, [{active, once}])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_state(Opts) ->
    URL = proplists:get_value(url, Opts, "nats://localhost:4222"),
    {ok, {nats, _, Host, Port, "/", _}} = http_uri:parse(URL, [{scheme_defaults, [{nats, 4222}]}]),
    #state{
        host = Host,
        port = Port,
        state = connecting
    }.

connect(#state{host=Host, port=Port}=State) ->
    case connect_socket(Host, Port) of
        {ok, Sock} ->
           inet:setopts(Sock, [{active, once}]),
           State#state{sock=Sock};
        Err ->
            Err
    end.

connect_socket(Host, Port) ->
    SockOpts = [binary, {active, false}, {keepalive, true}, {nodelay, true}],
    gen_tcp:connect(Host, Port, SockOpts).

parse_data(Data) ->
    case binary:split(Data, <<"\r\n">>) of
        [Cmd, Rest] ->
            case binary:split(Cmd, <<" ">>, [global]) of
                [<<"INFO">>, Info, <<>>] ->
                    {info, parse_info(Info)};
                [<<"PING">>] ->
                    ping;
                [<<"MSG">>, Subject, Sid, _Bytes] ->
                    {msg, Subject, Sid, unknown, Rest};
                [<<"MSG">>, Subject, Sid, ReplyTo, _Bytes] ->
                    {msg, Subject, Sid, ReplyTo, Rest}
            end
    end.

parse_info(Raw) ->
    jsx:decode(Raw, [{labels, atom}]).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_state_test() ->
    State = init_state([]),
    ?assertEqual(State#state.host, "localhost"),
    ?assertEqual(State#state.port, 4222).

init_state_with_url_test() ->
    State = init_state([{url, "nats://nats.example.com:6222"}]),
    ?assertEqual(State#state.host, "nats.example.com"),
    ?assertEqual(State#state.port, 6222).

parse_data_with_info_test() ->
    InfoMsg = <<"INFO {\"server_id\":\"1e7z\",\"version\":\"0.6.6\",\"go\":\"go1.4.2\",\"host\":\"0.0.0.0\",\"port\":4222,\"auth_required\":false,\"ssl_required\":false,\"max_payload\":1048576} \r\n">>,
    Expected = {info , [{server_id,<<"1e7z">>}, {version,<<"0.6.6">>}, {go,<<"go1.4.2">>}, {host,<<"0.0.0.0">>}, {port,4222}, {auth_required,false}, {ssl_required,false}, {max_payload,1048576}]},
    ?assertEqual(Expected, parse_data(InfoMsg)).

parse_data_with_ping_test() ->
    PingMsg = <<"PING\r\n">>,
    ?assertEqual(ping, parse_data(PingMsg)).

parse_data_with_msg_test() ->
    Msg = <<"MSG FOO.BAR 9 11\r\nHello World\r\n">>,
    Expected = {msg,<<"FOO.BAR">>,<<"9">>,unknown,<<"Hello World\r\n">>},
    ?assertEqual(Expected, parse_data(Msg)).

parse_data_with_msg_with_reply_to_test() ->
    Msg = <<"MSG FOO.BAR 9 INBOX.34 11\r\nHello World\r\n">>,
    Expected = {msg,<<"FOO.BAR">>,<<"9">>,<<"INBOX.34">>,<<"Hello World\r\n">>},
    ?assertEqual(Expected, parse_data(Msg)).
-endif.
