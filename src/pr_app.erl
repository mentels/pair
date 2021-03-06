-module(pr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(OPTS, [port, state, ip, peer_ip, iterations, intf_name, pair_no]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    try read_opts() of
        [{port, Port}, {state, State} | Rest] = Opts ->
            lager:info("[~s] Loaded opts: ~p~n", [State, Opts]),
            pr_sup:start_link(Port, State, Rest)
    catch
        _:Reason ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

read_opts() ->
    [begin
         case application:get_env(pair, O) of
             {ok, V}  ->
                 {O, V};
             undefined ->
                 throw({undefined_opt, O})
         end
     end|| O <- ?OPTS].










