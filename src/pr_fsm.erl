-module(pr_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).
-compile([export_all]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, active/2, passive/2, terminate/3, handle_event/3,
         handle_sync_event/4, handle_info/3]).

%% ------------------------------------------------------------------
%% Types
%% ------------------------------------------------------------------

-record(state, {port :: inet:port_number(),
                ip :: inet:ip_address(),
                peer_ip :: inet:ip_address(),
                it :: non_neg_integer(),
                iterations :: non_neg_integer(),
                intf_name :: string(),
                base_mac :: string(),
                pair_no :: non_neg_integer(),
                sock :: inet:socket()}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port, Opts) ->
    gen_fsm:start_link(?MODULE, [Port, Opts], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Port, Opts]) ->
    PairNo = proplists:get_value(pair_no, Opts),
    InitState = proplists:get_value(state, Opts),
    State0 = #state{port = Port,
                    ip = proplists:get_value(ip, Opts),
                    peer_ip = proplists:get_value(peer_ip, Opts),
                    it = 0,
                    iterations = proplists:get_value(iterations, Opts, 1),
                    intf_name = proplists:get_value(intf_name, Opts),
                    base_mac = base_mac(PairNo, InitState),
                    pair_no = PairNo},
    State1 = open_socket(State0),
    {ok, InitState, State1, _Timeout = 0}.

active(timeout, #state{it = N} = State) when N > State#state.iterations ->
    lager:info("Finishing in active state"),
    {stop, normal, State};
active(timeout, #state{it = N, sock = Sock,
                       base_mac = Mac0, intf_name = Intf} = State) ->
    gen_udp:send(Sock, State#state.peer_ip, State#state.port, Data = data()),
    Mac1 = format_mac(Mac0, N),
    set_mac(Mac1, Intf),
    lager:info("Sent ~p", [Data]),
    lager:info("Changed MAC to: ~s", [Mac1]),
    {next_state, passive, State#state{it = N + 1}, 0}.

passive(timeout, #state{it = N} = State) when N > State#state.iterations ->
    lager:info("Finising in passive state"),
    {stop, normal, State};
passive(timeout, #state{it = N, sock = Sock,
                        base_mac = Mac0, intf_name = Intf} = State) ->
    {ok, {Addr, Port, Packet}} = gen_udp:recv(Sock, 100),
    Mac1 = format_mac(Mac0, N),
    set_mac(Mac1, Intf),
    lager:info("Received ~p from ~p:~p", [Packet, Addr, Port]),
    lager:info("Changed MAC to: ~p",[Mac1]),
    {next_state, active, State#state{it = N + 1}, 0}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{sock = Sock}) ->
    ok = gen_udp:close(Sock).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

data() ->
    <<"hello world">>.

base_mac(PairNo, InitState) when PairNo =< 16#FFFF - 1 ->
    Mac0 = case InitState of
               active ->
                   integer_to_list(PairNo + 1, 16);
               passive ->
                   integer_to_list(PairNo, 16)
           end,
    MissingZeros = 4 - length(Mac0),
    Mac1 = [$0 || _ <- lists:seq(1, MissingZeros)] ++ Mac0,
    Mac2 = Mac1 ++ (_Unused = "0000"), %% ++ (_Iteration = "0001");
    format_base_mac(Mac2, []);
base_mac(_, _) ->
    throw(pair_no_out_of_range).

format_base_mac([], Acc) ->
    Acc;
format_base_mac(Mac, Acc) ->
    {Byte, T} = lists:split(2, Mac),
    format_base_mac(T, Acc ++ Byte ++ ":").

format_mac(BaseMac, It) ->
    ItPart0 = integer_to_list(It, 16),
    MissingZeros = 4 - length(ItPart0),
    ItPart1 = [$0 || _ <- lists:seq(1, MissingZeros)] ++ ItPart0,
    {First, Second} = lists:split(2, ItPart1),
    BaseMac ++ First ++ ":" ++ Second.

set_mac(Mac, Intf) ->
    Cmd = io_lib:format("ip link set ~s address ~s",[Intf, Mac]),
    [] = os:cmd(Cmd).

base_ip(PairNo, InitState) when PairNo =< 16#FFFF - 1 ->
    Ip0 = case InitState of
              active ->
                  integer_to_list(PairNo + 1, 16);
              passive ->
                  integer_to_list(PairNo, 16)
          end,
    MissingZeros = 4 - length(Ip0),
    Ip1 = [$0 || _ <- lists:seq(1, MissingZeros)] ++ Ip0,
    {FirstOct, SecondOct} = lists:split(2, Ip1),
    integer_to_list(list_to_integer(FirstOct, 16)) ++ "." ++
        integer_to_list(list_to_integer(SecondOct, 16)) ++ ".";
base_ip(_, _) ->
    throw(pair_no_out_of_range).

format_ip(BaseIp, It) ->
    ItPart0 = integer_to_list(It, 16),
    MissingZeros = 4 - length(ItPart0),
    ItPart1 = [$0 || _ <- lists:seq(1, MissingZeros)] ++ ItPart0,
    {FirstOct, SecondOct} = lists:split(2, ItPart1),
    BaseIp ++ integer_to_list(list_to_integer(FirstOct, 16)) ++ "." ++
        integer_to_list(list_to_integer(SecondOct, 16)).

open_socket(State) ->
    SockOpts = [{ip, State#state.ip}, {active, false}, binary],
    {ok, Sock} = gen_udp:open(State#state.port, SockOpts),
    State#state{sock = Sock}.



