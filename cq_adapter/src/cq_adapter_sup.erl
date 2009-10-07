-module(cq_adapter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  Users = {cq_users_adapter, {cq_users_adapter, start_link, []},
           Restart, Shutdown, Type, [cq_users_adapter]},
  VHosts = {cq_vhosts_adapter, {cq_vhosts_adapter, start_link, []},
           Restart, Shutdown, Type, [cq_vhosts_adapter]},
  Exchanges = {cq_exchanges_adapter, {cq_exchanges_adapter, start_link, []},
               Restart, Shutdown, Type, [cq_exchanges_adapter]},

  {ok, {SupFlags, [Users, VHosts, Exchanges]}}.
