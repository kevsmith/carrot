%% Copyright (c) 2009 Hypothetical Labs, Inc.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

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
