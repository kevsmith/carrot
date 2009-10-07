%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the cqapi application.

-module(cqapi_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cqapi.
start(_Type, _StartArgs) ->
    cqapi_deps:ensure(),
    cqapi_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cqapi.
stop(_State) ->
    ok.
