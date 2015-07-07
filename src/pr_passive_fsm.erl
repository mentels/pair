-module(pr_passive_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, config/2, recv/2, send_ack/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {port :: inet:port_number(),
                ip :: inet:ip_address(),
                peer_ip :: inet:ip_address(),
                intf_name :: string(),
                it :: integer(),
                base_mac :: string(),
                pair_no :: non_neg_integer(),
                sock :: inet:socket(),
                last_packet :: binary()}).

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
    {ok, recv, State1, _Timeout = 0}.

recv(timeout, #state{sock = Sock} = State) ->
    lager:info("[passive] Waiting for data..."),
    {ok, {Addr, Port, Packet}} = gen_udp:recv(Sock, 100),
    lager:info("[passive] Received ~p from ~p:~p", [Packet, Addr, Port]),
    {next_state, config, State#state{last_packet = Packet}, 0}.

config(timeout, State) ->
    reconfigure_networking(State),
    clean_arp(State),
    {next_state, send_ack, State, 0}.

send_ack(timeout, #state{sock = Sock, it = N, last_packet = LastPacket} = State) ->
    ok = gen_udp:send(Sock, State#state.peer_ip, State#state.port, <<"ack">>),
    lager:info("[passive] Sent ack"),
    case LastPacket of
        <<"finish">> ->
            {stop, normal, State};
        _ ->
            {next_state, recv, State#state{it = N + 1}, 0}
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    lager:info("[passive] Finished"),
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
           intf_name = proplists:get_value(intf_name, Opts),
           it = 0,
           base_mac = base_mac(PairNo, passive),
           pair_no = PairNo,
           last_packet = <<>>}.

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
    lager:info("[passive] Changed MAC to: ~s", [Mac1]).

format_mac(BaseMac, It) ->
    ItPart0 = integer_to_list(It, 16),
    MissingZeros = 4 - length(ItPart0),
    ItPart1 = [$0 || _ <- lists:seq(1, MissingZeros)] ++ ItPart0,
    {First, Second} = lists:split(2, ItPart1),
    BaseMac ++ First ++ ":" ++ Second.

set_mac(Mac, Intf) ->
    Cmd = io_lib:format("ip link set ~s address ~s",[Intf, Mac]),
    [] = os:cmd(Cmd).
