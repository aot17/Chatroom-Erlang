-module(websocket_handler).
-record(state, {ip = 0}).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        idle_timeout => 3000000}}.

websocket_init(State) ->
    gproc:reg({p, l, my_chatroom}),
    {ok, State}.

websocket_handle({text, Message}, State) ->
    ParsedBody = jsx:decode(Message),
    case ParsedBody of
    #{<<"type">> := <<"user-created">>, <<"username">> := Username} ->
        session_handler:add_participant(Username),
        Participants = session_handler:get_participants(),
        Response = jsx:encode(#{<<"type">> => <<"user-joined">>,
            <<"username">> => Username,
            <<"message">> => <<"joined the chatroom">>,
            <<"participants">> => Participants}),
        io:format("User created: ~s~n", [Username]),
        io:format("Response: ~s~n", [Response]),
        io:format("Values in my_chatroom: ~p~n", [gproc:lookup_values({p, l, my_chatroom})]),
            gproc:send({p, l, my_chatroom}, Response),
            {ok, State};
    #{<<"type">> := <<"send-message">>, <<"username">> := Username, <<"message">> := MessageText} ->
        %% Handle send-message message
        io:format("Received message from ~s: ~s~n", [Username, MessageText]),
        Response = jsx:encode(#{<<"type">> => <<"chat-message">>, <<"username">> => Username, <<"message">> => MessageText}),
        io:format("Response: ~s~n", [Response]),
        io:format("Values in my_chatroom: ~p~n", [gproc:lookup_values({p, l, my_chatroom})]),
            gproc:send({p, l, my_chatroom}, Response),    
            {ok, State}
    end;  
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(Response, State) ->
    {[{text, Response}], State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

