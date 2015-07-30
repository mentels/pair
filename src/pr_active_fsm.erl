-module(pr_active_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, prepare/2, send/2, wait_ack/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(RECV_MILIS_TIMEOUT, 3000).

-record(state, {port :: inet:port_number(),
                ip :: inet:ip_address(),
                peer_ip :: inet:ip_address(),
                it :: integer(),
                iterations :: non_neg_integer(),
                intf_name :: string(),
                base_mac :: string(),
                pair_no :: non_neg_integer(),
                sock :: inet:socket()}).

-define(NO_ARP, "No ARP entry for").

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, Opts) ->
    gen_fsm:start_link(?MODULE, [Port, Opts], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Port, Opts]) ->
    State0 = prepare_config(Port, Opts),
    State1 = open_socket(State0),
    clean_arp(State1),
    {ok, prepare, State1, _Timeout = 0}.

prepare(timeout, #state{it = N} = State) when N =:= State#state.iterations ->
    lager:info("[active] Finished"),
    {stop, normal, State};
prepare(timeout, State) ->
    reconfigure_networking(State),
    clean_arp(State),
    {next_state, send, State, 0}.


send(timeout, #state{sock = Sock} = State) ->
    lager:info("[active] Sending data..."),
    ok = gen_udp:send(Sock, State#state.peer_ip, State#state.port,
                      Data = data(State)),
    lager:info("[active] Sent ~p", [Data]),
    {next_state, wait_ack, State, 0}.

wait_ack(timeout, #state{sock = Sock, it = N} = State) ->
    ExpectedAck = expected_ack(N),
    case gen_udp:recv(Sock, 100, ?RECV_MILIS_TIMEOUT) of
        {ok, {Addr, Port, ExpectedAck = Packet}} ->
            lager:info("[active] Received ack ~p from ~p:~p",
                       [Packet, Addr, Port]);
        {ok, {Addr, Port, Packet}} ->
            lager:warning("[active][bad_ack] Incorrect ack ~p from ~p:~p",
                       [Packet, Addr, Port]);
        {error, timeout} ->
            lager:warning("[active][timeout] Socket timeout. "
                          "Recevied no ack for iteration ~p", [N])
    end,
    {next_state, prepare, State#state{it = N + 1}, 0}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

prepare_config(Port, Opts) ->
    PairNo = proplists:get_value(pair_no, Opts),
    Ip = proplists:get_value(ip, Opts),
    {ok, ParsedIp} = inet:parse_ipv4_address(Ip),
    PeerIp = proplists:get_value(peer_ip, Opts),
    {ok, ParsedPeerIp} = inet:parse_ipv4_address(PeerIp),
    #state{port = Port,
           ip = ParsedIp,
           peer_ip = ParsedPeerIp,
           it = 0,
           iterations = proplists:get_value(iterations, Opts, 1),
           intf_name = proplists:get_value(intf_name, Opts),
           base_mac = base_mac(PairNo, active),
           pair_no = PairNo}.

open_socket(State) ->
    SockOpts = [{ip, State#state.ip}, {active, false}, binary],
    {ok, Sock} = gen_udp:open(State#state.port, SockOpts),
    State#state{sock = Sock}.

clean_arp(#state{peer_ip = PeerIP0}) ->
    PeerIP1 = inet:ntoa(PeerIP0),
    Cmd = io_lib:format("arp -d ~s", [PeerIP1]),
    case os:cmd(Cmd) of
        [] ->
            ok;
        NoArpOrError ->
            ?NO_ARP == string:substr(NoArpOrError, 1, length(?NO_ARP))
                orelse
                throw(io_lib:format("Arp cleaning error: ~s", [NoArpOrError]))
    end.

base_mac(PairNo, InitState) when PairNo =< 16#FFFF - 1 ->
    Mac0 = integer_to_list(PairNo, 16),
    MissingZeros = 4 - length(Mac0),
    Mac1 = [$0 || _ <- lists:seq(1, MissingZeros)] ++ Mac0,
    Mac2 = Mac1 ++ case InitState of
                       passive ->
                           "0001";
                       active ->
                           "0002"
                   end, %% ++ (_Iteration = "0001");
    format_base_mac(Mac2, []);
base_mac(_, _) ->
    throw(pair_no_out_of_range).

format_base_mac([], Acc) ->
    Acc;
format_base_mac(Mac, Acc) ->
    {Byte, T} = lists:split(2, Mac),
    format_base_mac(T, Acc ++ Byte ++ ":").

reconfigure_networking(#state{base_mac = Mac0, it = N, intf_name = Intf}) ->
    Mac1 = format_mac(Mac0, N),
    set_mac(Mac1, Intf),
    lager:info("[active] Changed MAC to: ~s", [Mac1]).

format_mac(BaseMac, It) ->
    ItPart0 = integer_to_list(It, 16),
    MissingZeros = 4 - length(ItPart0),
    ItPart1 = [$0 || _ <- lists:seq(1, MissingZeros)] ++ ItPart0,
    {First, Second} = lists:split(2, ItPart1),
    BaseMac ++ First ++ ":" ++ Second.

set_mac(Mac, Intf) ->
    Cmd = io_lib:format("ip link set ~s address ~s",[Intf, Mac]),
    [] = os:cmd(Cmd).

data(#state{it = N, iterations = Its}) when Its =:= N + 1->
    list_to_binary(io_lib:format("finish/~p", [N]));
data(#state{it = N}) ->
    list_to_binary(io_lib:format("data/~p", [N])).

expected_ack(It) ->
    list_to_binary(io_lib:format("ack/~p", [It])).
