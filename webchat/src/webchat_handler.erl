-module(webchat_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Json}, State) ->
    Msg = jsx:decode(Json, [return_maps]),
    case Msg of
        #{<<"type">> := <<"join">>, <<"username">> := Username} ->
            webchat_server:add_client(self(), Username),
            {ok, State};
        #{<<"type">> := <<"message">>, <<"username">> := Username, <<"message">> := Message} ->
            webchat_server:broadcast(Username, Message),
            {ok, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({text, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.