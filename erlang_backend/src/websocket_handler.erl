-module(websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {[{text, <<"Hello!">>}], State}.

websocket_handle({text, Message}, State) ->
    ParsedBody = jsx:decode(Message),
    case ParsedBody of
    #{<<"type">> := <<"user-created">>, <<"username">> := Username} ->
        %{ok, UserId} = sh:create_session(Username),
        %io:format("User created: ~s, UserID: ~w~n", [Username, UserId]);
        io:format("User created: ~s~n", [Username]),
        Response = jsx:encode(#{<<"type">> => <<"user-joined">>, <<"message">> => <<"New user entered the chatroom">>}),
        io:format("Response: ~s~n", [Response]),
           {reply, {text, Response}, State};
    #{<<"type">> := <<"send-message">>, <<"username">> := Username, <<"message">> := MessageText} ->
        %% Handle send-message message
        io:format("Received message from ~s: ~s~n", [Username, MessageText]),
        Response = jsx:encode(#{<<"type">> => <<"chat-message">>, <<"username">> => Username, <<"message">> => MessageText}),
        io:format("Response: ~s~n", [Response]),    
            {reply, {text, Response}, State};
    _ ->
        %% Handle other types of messages
        io:format("Received unknown message: ~s~n", [Message])
end,
{ok, State}.

websocket_info({log, Text}, State) ->
    {[{text, Text}], State};
websocket_info(_Info, State) ->
    {ok, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
