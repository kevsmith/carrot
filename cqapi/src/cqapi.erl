%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(cqapi).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start() -> ok
%% @doc Start the cqapi server.
start() ->
    cqapi_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(cqapi).

%% @spec stop() -> ok
%% @doc Stop the cqapi server.
stop() ->
    Res = application:stop(cqapi),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
