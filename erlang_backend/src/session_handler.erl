-module(session_handler).
-export([create_session/2, get_session/1, update_session/2, delete_session/1, init/0]).

-define(SESSION_STORE, session_store).

% Create a session
create_session(UserId, SessionData) ->
    ets:insert_new(?SESSION_STORE, {UserId, SessionData}),
    ok.

% Retrieve a session
get_session(UserId) ->
    case ets:lookup(?SESSION_STORE, UserId) of
        [] -> not_found;
        [{_, SessionData}] -> SessionData
    end.

% Update a session
update_session(UserId, NewSessionData) ->
    case ets:lookup(?SESSION_STORE, UserId) of
        [] -> not_found;
        _ -> 
            ets:insert(?SESSION_STORE, {UserId, NewSessionData}),
            ok
    end.

% Delete a session
delete_session(UserId) ->
    ets:delete(?SESSION_STORE, UserId),
    ok.

init() ->
    ets:new(?SESSION_STORE, [named_table, set, public]),
    ok.
