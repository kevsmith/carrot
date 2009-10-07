-module(cq_users_adapter).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% User API
-export([auth/3, add/3, del/2, find/2, list/1, change/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

auth(Node, User, Password) when is_binary(User), is_binary(Password) ->
  gen_server:call({?SERVER, Node}, {auth, User, Password}).

add(Node, User, Password) when is_binary(User), is_binary(Password) ->
  gen_server:call({?SERVER, Node}, {add, User, Password}).

del(Node, User) when is_binary(User) ->
  gen_server:call({?SERVER, Node}, {del, User}).

list(Node) ->
  gen_server:call({?SERVER, Node}, list).

find(Node, User) when is_binary(User) ->
  gen_server:call({?SERVER, Node}, {find, User}).

change(Node, User, Password) when is_binary(User), is_binary(Password) ->
  gen_server:call({?SERVER, Node}, {change, User, Password}).

init([]) ->
  {ok, #state{}}.

handle_call({auth, User, Password}, _From, State) ->
  try
    {reply, rabbit_access_control:user_pass_login(User, Password), State}
  catch
    exit: Error ->
      begin
        error_logger:info_msg("Auth error (~p, ~p): ~p~n", [User, Password, Error]),
        {reply, {error, auth_failed}, State}
      end
  end;

handle_call({add, User, Password}, _From, State) ->
  {reply, rabbit_access_control:add_user(User, Password), State};

handle_call({del, User}, _From, State) ->
  {reply, rabbit_access_control:delete_user(User), State};

handle_call(list, _From, State) ->
  {reply, rabbit_access_control:list_users(), State};

handle_call({find, User}, _From, State) ->
  {reply, rabbit_access_control:lookup_user(User), State};

handle_call({change, User, Password}, _From, State) ->
  {reply, rabbit_access_control:change_password(User, Password), State};

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
