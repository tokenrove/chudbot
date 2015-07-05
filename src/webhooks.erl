-module(webhooks).
-export([init/2]).

init(Req, Opts) ->
    %% {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
    %% {_, Lang} = lists:keyfind(lang, 1, KeyValues).
    {ok, Data, Req2} = cowboy_req:body(Req),
    irc:announce(whereis(irc), io_lib:fwrite("~p: ~p", [Opts, Data])),
    Rep = cowboy_req:reply(200,
                           [{<<"content-type">>, <<"text/plain">>}],
                           <<"ok">>,
                           Req2),
    {ok, Rep, {}}.
