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
