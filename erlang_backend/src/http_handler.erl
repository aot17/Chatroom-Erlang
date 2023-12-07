-module(http_handler).
-export([init/2, handle/2, terminate/3]).

init(Req, Opts) ->
    {ok, Req, Opts}.

handle(Req, State) ->
    io:format("Received request~n"),
    RespBody = <<"Hello from Cowboy!">>,
    {ok, Req2} = cowboy_req:reply(200, 
                                   #{<<"content-type">> => <<"text/plain">>}, 
                                   RespBody, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
