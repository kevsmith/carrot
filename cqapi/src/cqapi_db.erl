-module(cqapi_db).

-export([new_account/1]).

new_account() ->
  ok.

%% Internal functions
next_counter(CounterName) ->
