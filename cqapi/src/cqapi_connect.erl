-module(cqapi_connect).

-export([connect/1]).

connect(Node) ->
  case lists:member(Node, erlang:nodes()) of
    true ->
      ok;
    false ->
      case net_kernel:connect(Node) of
        true ->
          ok;
        false ->
          throw({error, {connect_failed, Node}})
      end
  end.
