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
