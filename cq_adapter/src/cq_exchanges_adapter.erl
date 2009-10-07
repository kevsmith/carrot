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

-module(cq_exchanges_adapter).

-behaviour(gen_server).

%% API
-export([start_link/0, add/4, del/3, list/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Node, VHost, Name, Type) when is_binary(VHost),
                            is_binary(Name),
                            is_atom(Type) ->
  gen_server:call({?SERVER, Node}, {add, VHost, Name, Type}).

del(Node, VHost, Name) when is_binary(VHost),
                      is_binary(Name) ->
  check_exchange(Name),
  gen_server:call({?SERVER, Node}, {del, VHost, Name}).

list(Node, VHost) when is_binary(VHost) ->
  gen_server:call({?SERVER, Node}, {list, VHost}).

init([]) ->
  {ok, #state{}}.

handle_call({add, VHost, Name, Type}, _From, State) ->
  ExchangeName = rabbit_misc:r(VHost, exchange, Name),
  {reply, rabbit_exchange:declare(ExchangeName, Type, true, false, []), State};

handle_call({del, VHost, Name}, _From, State) ->
  ExchangeName = rabbit_misc:r(VHost, exchange, Name),
  {reply, rabbit_exchange:delete(ExchangeName, false), State};

handle_call({list, VHost}, _From, State) ->
  Exchanges = rabbit_exchange:list(VHost),
  NameTypes = [{Name, Type} || {exchange, {resource, _, _, Name}, Type, _, _, _} <- Exchanges],
  {reply, NameTypes, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
check_exchange(<<>>=Name) ->
  throw({error, permanent_exchange, Name});
check_exchange(<<"amq.direct">>=Name) ->
  throw({error, permanent_exchange, Name});
check_exchange(<<"amq.fanout">>=Name) ->
  throw({error, permanent_exchange, Name});
check_exchange(<<"amq.topic">>=Name) ->
  throw({error, permanent_exchange, Name});
check_exchange(<<"amq.headers">>=Name) ->
  throw({error, permanent_exchange, Name});
check_exchange(<<"amq.match">>=Name) ->
  throw({error, permanent_exchange, Name});
check_exchange(_) ->
  ok.
