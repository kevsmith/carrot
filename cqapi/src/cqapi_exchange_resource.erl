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
