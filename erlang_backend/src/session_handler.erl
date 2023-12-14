-module(session_handler).
-export([add_participant/1, remove_participant/1, get_participants/0, init/0]).

-define(PARTICIPANTS_STORE, participants_store).

% Add a participant to the chatroom
add_participant(Username) ->
    ets:insert(?PARTICIPANTS_STORE, {Username}),
    ok.

% Remove a participant from the chatroom
remove_participant(Username) ->
    ets:delete(?PARTICIPANTS_STORE, Username),
    ok.

% Retrieve the list of participants
get_participants() ->
    Participants = ets:tab2list(?PARTICIPANTS_STORE),
    [Username || {Username} <- Participants].

% Initialize the ets table for participants
init() ->
    ets:new(?PARTICIPANTS_STORE, [named_table, set, public]),
    ok.
