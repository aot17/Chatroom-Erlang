-module(sh).
-export([create_session/1, get_session/1, update_session/2, delete_session/1, init/0, increment_counter/0, post_message/2, get_messages/1]).

-define(SESSION_STORE, session_store).
-define(SESSION_HANDLER, sh).

% Counter to generate UserId
-record(state, {counter}).
-record(session, {user_id, username, counter, messages}).

% Create a session with a counter-based UserId
create_session(UserName) ->
    CounterState = increment_counter(),
    UserId = CounterState#state.counter,
    InitialSession = #session{user_id = UserId, username = UserName, counter = 0, messages = []},
    ets:insert_new(?SESSION_STORE, {UserId, InitialSession}),
    {ok, UserId}.


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

% Initialize the session store and counter
init() ->
    ets:new(?SESSION_STORE, [named_table, set, public]),
    CounterState = #state{counter = 0},
    ets:insert(?SESSION_STORE, {state, CounterState}),  % Insert initial state
    {ok, CounterState}.

% Increment counter and return the updated state
increment_counter() ->
    case ets:lookup(?SESSION_STORE, state) of
        [] -> not_found;
        [{state, State}] ->
            NewCounter = State#state.counter + 1,
            NewState = State#state{counter = NewCounter},
            ets:insert(?SESSION_STORE, {state, NewState}),
            NewState
    end.


post_message(UserId, Message) ->
    case ets:lookup(?SESSION_STORE, UserId) of
        [] -> not_found;
        [{UserId, Session}] ->
            NewCounter = Session#session.counter + 1,
            UpdatedMessages = [{content, Message} | Session#session.messages],
            UpdatedSession = Session#session{counter = NewCounter, messages = UpdatedMessages},
            ets:insert(?SESSION_STORE, {UserId, UpdatedSession}),
            {ok, NewCounter}
    end.

% Retrieve messages for a user
get_messages(UserId) ->
    case ets:lookup(?SESSION_STORE, UserId) of
        [] -> not_found;
        [{UserId, Session}] ->
            Messages = Session#session.messages,
            {ok, Messages}
    end.