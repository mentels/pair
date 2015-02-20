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

-export([init/1, active/2, passive/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% Types
%% ------------------------------------------------------------------

-record(state, {port :: inet:port_number(),
                ip :: inet:ip_address(),
                peer_ip :: inet:ip_address(),
                iterations :: non_neg_integer(),
                pair_no :: non_neg_integer()}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port, Opts) ->
    gen_fsm:start_link(?MODULE, [Port, Opts], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Port, Opts]) ->
    State = #state{port = Port,
                   ip = proplists:get_value(ip, Opts),
                   peer_ip = proplists:get_value(peer_ip, Opts),
                   iterations = proplists:get_value(interations, Opts, 1),
                   pair_no = proplists:get_value(pair_no, Opts)},
    {ok, proplists:get_value(state, Opts), State, _Timeout = 0}.

active(timeout, #state{iterations = 0} = State) ->
    lager:info("Finishing in active state"),
    {stop, normal, State};
active(timeout, #state{iterations = N} = State) ->
    SockOpts = [{ip, State#state.ip}, {active, false}, binary],
    {ok, Sock} = gen_udp:open(State#state.port, SockOpts),
    %% NewAddressing = get_new_addressing(State)
    gen_udp:send(Sock, State#state.peer_ip, State#state.port, Data = data()),
    gen_udp:close(Sock),
    lager:info("Send ~p~n", [Data]),
    {next_state, passive, State#state{iterations = N - 1}, 0}.

passive(timeout, #state{iterations = 0} = State) ->
    lager:info("Finising in passive state"),
    {stop, normal, State};
passive(timeout, #state{iterations = N} = State) ->
    SockOpts = [{ip, State#state.ip}, {active, false}, binary],
    {ok, Sock} = gen_udp:open(State#state.port, SockOpts),
    {ok, {Addr, Port, Packet}} = gen_udp:recv(Sock, 100),
    lager:info("Received ~p from ~p:~p", [Packet, Addr, Port]),
    {next_state, active, State#state{iterations = N - 1}}.

state_name(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

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



