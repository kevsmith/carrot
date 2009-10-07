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

-module(cqapi_perm_resource).

-export([init/1, allowed_methods/2, resource_exists/2, is_authorized/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([to_json/2, from_json/2, delete_resource/2]).

-include("cqapi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #resource_state{}}.

allowed_methods(ReqData, State) ->
  {['HEAD', 'GET', 'PUT', 'DELETE'], ReqData, State}.

is_authorized(ReqData, State) ->
  cqapi_digest_auth:handle_auth(?AUTH_REALM, ?AUTH_FUN, ReqData, State).

resource_exists(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  User = wrq:path_info(user, ReqData),
  Exists = lists:member(list_to_binary(VHost), cq_vhosts_adapter:list(Node)) andalso
             not(cq_users_adapter:find(Node, list_to_binary(User)) =:= {error, not_found}),
  {Exists, ReqData, State}.


content_types_provided(ReqData, State) ->
  {[{"application/json", to_json},
    {"text/plain", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.

delete_resource(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  User = wrq:path_info(user, ReqData),
  case cq_vhosts_adapter:clear_perms(Node, list_to_binary(VHost), list_to_binary(User)) of
    ok ->
      {true, ReqData, State};
    Err ->
      error_logger:error_msg("Error deleting perm (~p/~p): ~p", [VHost, User, Err]),
      {false, ReqData, State}
  end.

to_json(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  User = wrq:path_info(user, ReqData),
  Perms = case cq_vhosts_adapter:list_perms(Node, list_to_binary(VHost), list_to_binary(User)) of
            [{_, Config, Write, Read}] ->
              {struct, [{config, Config}, {write, Write}, {read, Read}]};
            [] ->
              []
          end,
  {mochijson2:encode(Perms), ReqData, State}.

from_json(ReqData, #resource_state{node=Node}=State) ->
  VHost = cqapi_util:fixup_vhost(wrq2:path_info(vhost, ReqData, "/")),
  User = wrq:path_info(user, ReqData),
  {struct, Perms} = mochijson2:decode(wrq:req_body(ReqData)),
  io:format("Perms: ~p~n", [Perms]),
  ConfigPerm = proplists:get_value(<<"config">>, Perms, <<"">>),
  WritePerm = proplists:get_value(<<"write">>, Perms, <<"">>),
  ReadPerm = proplists:get_value(<<"read">>, Perms, <<"">>),
  case cq_vhosts_adapter:set_perms(Node, list_to_binary(VHost), list_to_binary(User),
                                   ConfigPerm, WritePerm, ReadPerm) of
    ok ->
      {true, ReqData, State};
    Err ->
      error_logger:error_msg("Error setting perms (~p/~p ~p): ~p", [VHost, User, [ConfigPerm,
                                                                                  WritePerm,
                                                                                  ReadPerm], Err]),
      {false, ReqData, State}
  end.
