-module(cqapi_perms_resource).

-export([init/1, allowed_methods/2, is_authorized/2, resource_exists/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([to_json/2]).

-include("cqapi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #resource_state{}}.

is_authorized(ReqData, State) ->
  cqapi_digest_auth:handle_auth(?AUTH_REALM, ?AUTH_FUN, ReqData, State).

resource_exists(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  Exists = lists:member(list_to_binary(VHost), cq_vhosts_adapter:list(Node)),
  {Exists, ReqData, State}.

allowed_methods(ReqData, State) ->
  {['GET', 'HEAD'], ReqData, State}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json},
    {"text/plain", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.

to_json(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  Perms = case cq_vhosts_adapter:list_perms(Node, list_to_binary(VHost)) of
            [] ->
              [];
            RawPerms ->
              format_perms(RawPerms)
          end,
  {mochijson2:encode(Perms), ReqData, State}.

%% Internal functions
format_perms(Perms) ->
  format_perms(Perms, []).

format_perms([], Accum) ->
  {struct, lists:reverse(Accum)};
format_perms([{User, Config, Write, Read}|T], Accum) ->
  format_perms(T, [{User, {struct, [{config, Config},
                                    {write, Write},
                                    {read, Read}]}}|Accum]).