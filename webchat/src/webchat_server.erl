-module(webchat_server).
-behaviour(gen_server).

-export([start_link/0, add_client/2, remove_client/1, broadcast/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_client(Pid, Username) ->
    gen_server:call(?MODULE, {add_client, Pid, Username}).

remove_client(Pid) ->
    gen_server:cast(?MODULE, {remove_client, Pid}).

broadcast(Username, Message) ->
    gen_server:cast(?MODULE, {broadcast, Username, Message}).

init([]) ->
    {ok, #{}}.

handle_call({add_client, Pid, Username}, _From, Clients) ->
    monitor(process, Pid),
    {reply, ok, Clients#{Pid => Username}}.

handle_cast({remove_client, Pid}, Clients) ->
    {noreply, maps:remove(Pid, Clients)};
handle_cast({broadcast, Username, Message}, Clients) ->
    Msg = jsx:encode(#{
        type => <<"message">>,
        username => Username,
        message => Message
    }),
    [Pid ! {text, Msg} || {Pid, _} <- maps:to_list(Clients)],
    {noreply, Clients}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, Clients) ->
    {noreply, maps:remove(Pid, Clients)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.