-module(pr_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port, Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Opts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, Opts]) ->
    ChildSpec = {pr_fsm,
                 {pr_fsm, start_link, [Port, Opts]},
                 temporary, 5000, worker, [pr_fsm]},
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.

