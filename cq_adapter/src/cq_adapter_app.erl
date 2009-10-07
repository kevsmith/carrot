-module(cq_adapter_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
  application:start(cq_adapter).

start(_StartType, _StartArgs) ->
  cq_adapter_sup:start_link().

stop(_State) ->
  ok.
