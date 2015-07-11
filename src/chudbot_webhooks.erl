-module(chudbot_webhooks).
-export([init/2]).

init(Req, Opts) ->
    case cowboy_req:has_body(Req) of
        true -> handle_request_with_body(Req, Opts);
        _ -> {ok, cowboy_req:reply(200, [], <<>>, Req), {}}
    end.

handle_request_with_body(Req, Opts) ->
    %% {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
    %% {_, Lang} = lists:keyfind(lang, 1, KeyValues).
    io:format("HTTP request (~p): ~p~n", [Opts, Req]),
    {ok, Data, Req2} = cowboy_req:body(Req),
    M = case Opts of
            [trello] -> chudbot_trello:handle_request(Data, Req2);
            [github] -> handle_github_request(Data, Req2);
            [buildbot] -> handle_github_request(Data, Req2);
            _ -> io_lib:fwrite("Unknown request: ~p", Data)
        end,
    chudbot_irc:announce(M),
    Rep = cowboy_req:reply(204, [], <<>>, Req2),
    {ok, Rep, {}}.

handle_github_request(Body, _Req) ->
    io_lib:fwrite("github: ~p", [Body]).

handle_buildbot_request(Body, _Req) ->
    io_lib:fwrite("buildbot: ~p", [Body]).
