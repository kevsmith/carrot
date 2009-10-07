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

-module(cq_vhosts_adapter).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% VHosts API
-export([add/2, del/2, list/1]).

%% Permissions API
-export([set_perms/6, clear_perms/3, list_perms/2, list_perms/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Node, VHost) when is_binary(VHost) ->
  gen_server:call({?SERVER, Node}, {add, VHost}).

del(Node, VHost) when is_binary(VHost) ->
  gen_server:call({?SERVER, Node}, {del, VHost}).

list(Node) ->
  gen_server:call({?SERVER, Node}, list).

set_perms(Node, VHost, User, ConfigurePerm, WritePerm, ReadPerm) when is_binary(VHost),
                                                                      is_binary(User),
                                                                      is_binary(ConfigurePerm),
                                                                      is_binary(WritePerm),
                                                                      is_binary(ReadPerm) ->
  gen_server:call({?SERVER, Node}, {set_perms, User, VHost, ConfigurePerm, WritePerm, ReadPerm}).

clear_perms(Node, VHost, User) when is_binary(VHost),
                                    is_binary(User) ->
  gen_server:call({?SERVER, Node}, {clear_perms, User, VHost}).

list_perms(Node, VHost) when is_binary(VHost) ->
  gen_server:call({?SERVER, Node}, {list_perms, VHost}).

list_perms(Node, VHost, User) when is_binary(VHost),
                                   is_binary(User) ->
  Perms = list_perms(Node, VHost),
  [{U, P1, P2, P3} || {U, P1, P2, P3} <- Perms,
                      U =:= User].

%% gen_server callbacks

init([]) ->
  {ok, #state{}}.

handle_call({add, VHost}, _From, State) ->
  {reply, rabbit_access_control:add_vhost(VHost), State};

handle_call({del, VHost}, _From, State) ->
  {reply, rabbit_access_control:delete_vhost(VHost), State};

handle_call(list, _From, State) ->
  {reply, rabbit_access_control:list_vhosts(), State};

handle_call({set_perms, User, VHost, ConfigurePerm, WritePerm, ReadPerm}, _From, State) ->
  {reply, rabbit_access_control:set_permissions(User, VHost, ConfigurePerm,
                                                WritePerm, ReadPerm), State};

handle_call({clear_perms, User, VHost}, _From, State) ->
  {reply, rabbit_access_control:clear_permissions(User, VHost), State};

handle_call({list_perms, VHost}, _From, State) ->
  {reply, rabbit_access_control:list_vhost_permissions(VHost), State};

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
