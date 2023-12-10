%%%-------------------------------------------------------------------
%% @doc my_app public API
%% @end
%%%-------------------------------------------------------------------

-module(my_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/sessions", websocket_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}),
        io:format("Cowboy started on port 8080 ~n"
    ),
    my_app_sup:start_link().

stop(_State) ->
    io:format("listener stopped"),
    ok = cowboy:stop_listener(my_http_listener).

%% internal functions
