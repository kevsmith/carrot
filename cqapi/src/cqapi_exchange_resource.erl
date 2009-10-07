-module(cqapi_exchange_resource).

-export([init/1, resource_exists/2, allowed_methods/2, is_authorized/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([from_json/2, delete_resource/2]).

-include("cqapi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #resource_state{}}.

allowed_methods(ReqData, State) ->
  {['PUT', 'DELETE'], ReqData, State}.

is_authorized(ReqData, State) ->
  cqapi_digest_auth:handle_auth(?AUTH_REALM, ?AUTH_FUN, ReqData, State).

resource_exists(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  Exchange = wrq:path_info(exchange, ReqData),
  Exists = lists:member(list_to_binary(VHost), cq_vhosts_adapter:list(Node)) andalso
           exchange_exists(list_to_binary(Exchange), cq_exchanges_adapter:list(Node, list_to_binary(VHost))),
  {Exists, ReqData, State}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json},
    {"text/plain", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.

delete_resource(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  Exchange = wrq:path_info(exchange, ReqData),
  case cq_exchanges_adapter:del(Node, list_to_binary(VHost), list_to_binary(Exchange)) of
    ok ->
      {true, ReqData, State};
    Err ->
      error_logger:error_msg("Error deleting exchange (~p/~p): ~p", [VHost, Exchange, Err]),
      {false, ReqData, State}
  end.

from_json(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  Exchange = wrq:path_info(exchange, ReqData),
  Type = exchange_type(mochijson2:decode(wrq:req_body(ReqData))),
  case cq_exchanges_adapter:add(Node, list_to_binary(VHost), list_to_binary(Exchange), Type) of
    {exchange, _, _, _, _, _} ->
      {true, ReqData, State};
    Err ->
      error_logger:error_msg("Error create exchange (~p/~p): ~p", [VHost, Exchange, Err]),
      {false, ReqData, State}
  end.

%% Internal functions
exchange_type(<<"fanout">>) ->
  fanout;
exchange_type(<<"direct">>) ->
  direct;
exchange_type(<<"topic">>) ->
  topic;
exchange_type(<<"headers">>) ->
  headers.

exchange_exists(_Name, []) ->
  false;
exchange_exists(Name, [{Name, _Type}|_]) ->
  true;
exchange_exists(Name, [_|T]) ->
  exchange_exists(Name, T).
