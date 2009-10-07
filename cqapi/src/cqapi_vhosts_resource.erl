-module(cqapi_vhosts_resource).

-export([init/1, is_authorized/2, resource_exists/2, content_types_provided/2]).
-export([generate_etag/2, to_json/2]).

-include("cqapi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #resource_state{}}.

is_authorized(ReqData, State) ->
  cqapi_digest_auth:handle_auth(?AUTH_REALM, ?AUTH_FUN, ReqData, State).

resource_exists(ReqData, #resource_state{node=Node}=State) ->
  {true, ReqData, State#resource_state{data=cq_vhosts_adapter:list(Node)}}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json},
    {"text/plain", to_json}], ReqData, State}.

generate_etag(ReqData, #resource_state{data=VHosts}=State) ->
  {mochihex:to_hex(erlang:md5(VHosts)), ReqData, State}.

to_json(ReqData, #resource_state{data=VHosts}=State) ->
  {mochijson2:encode(VHosts), ReqData, State}.
