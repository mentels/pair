-module(pr_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port, State, Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, State, Opts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, State, Opts]) ->
    Mod = case State of
              passive ->
                  pr_passive_fsm;
              active ->
                  pr_active_fsm
          end,
    ChildSpec = {pr_fsm,
                 {Mod, start_link, [Port, Opts]},
                 temporary, 5000, worker, [Mod]},
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.
