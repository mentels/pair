-module(pr_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

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
                iterations :: non_neg_integer()}).

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
                   iterations = proplists:get_value(interations, Opts, 1)},
    {ok, proplists:get_value(state, Opts), State, _Timeout = 0}.

active(timeout, #state{iterations = 0} = State) ->
    lager:info("Finishing in active state"),
    {stop, normal, State};
active(timeout, #state{iterations = N} = State) ->
    SockOpts = [{ip, State#state.ip}, {active, false}, binary],
    {ok, Sock} = gen_udp:open(State#state.port, SockOpts),
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
    {ok, Addr, Port, Packet} = gen_udp:recv(Sock, 100),
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
