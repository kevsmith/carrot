-module(cqapi_user_resource).

-export([init/1, is_authorized/2, resource_exists/2, content_types_provided/2]).
-export([content_types_accepted/2, allowed_methods/2]).
-export([from_json/2, to_json/2, delete_resource/2]).

-include("cqapi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #resource_state{}}.

is_authorized(ReqData, State) ->
  cqapi_digest_auth:handle_auth(?AUTH_REALM, ?AUTH_FUN, ReqData, State).

resource_exists(ReqData, #resource_state{node=Node}=State) ->
  User = wrq:path_info(user, ReqData),
  Exists = not(cq_users_adapter:find(Node, list_to_binary(User)) =:= {error, not_found}),
  {Exists, ReqData, State}.

allowed_methods(ReqData, State) ->
  {['GET', 'HEAD', 'PUT', 'DELETE'], ReqData, State}.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json},
    {"text/plain", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.

to_json(ReqData, State) ->
  UserName = wrq:path_info(user, ReqData),
  {mochijson2:encode(list_to_binary(UserName)), ReqData, State}.

from_json(ReqData, #resource_state{node=Node}=State) ->
  UserName = wrq:path_info(user, ReqData),
  Password = mochijson2:decode(wrq:req_body(ReqData)),
  case cq_users_adapter:find(Node, list_to_binary(UserName)) of
    {error, not_found} ->
      ok = cq_users_adapter:add(Node, list_to_binary(UserName), Password),
      {true, ReqData, State};
    _ ->
      {false, ReqData, State}
  end.

delete_resource(ReqData, #resource_state{node=Node}=State) ->
  UserName = wrq:path_info(user, ReqData),
  case cq_users_adapter:del(Node, list_to_binary(UserName)) of
    ok ->
      {true, ReqData, State};
    Err ->
      error_logger:error_msg("Error deleting user ~p: ~p", [UserName, Err]),
      {false, ReqData, State}
  end.
