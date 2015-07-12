-module(chudbot_webhooks).
-export([init/2]).

%% Some dumb clients (e.g. buildbot) think that 204 is a failure code,
%% so send 200 with an empty body..
empty_reply(Req) ->
    {ok, cowboy_req:reply(200, [], <<>>, Req), {}}.

init(Req, Opts) ->
    case cowboy_req:has_body(Req) of
        true -> handle_request_with_body(Req, Opts);
        _ -> empty_reply(Req)
    end.

handle_request_with_body(Req, Opts) ->
    {Type, Subtype, _} = cowboy_req:parse_header(<<"content-type">>, Req),
    ContentType = {Type, Subtype},
    {Data,Req2} = case ContentType of
                      {<<"application">>, <<"x-www-form-urlencoded">>} ->
                          {ok, KeyValues, R} = cowboy_req:body_qs(Req),
                          {KeyValues, R};
                      {<<"application">>, <<"json">>} ->
                          {ok, Body, R} = cowboy_req:body(Req),
                          {jiffy:decode(Body, [return_maps]), R}
                  end,
    M = case Opts of
            [trello] -> chudbot_trello:handle_request(Data, ContentType);
            [github] ->
                Event = cowboy_req:header(<<"x-github-event">>, Req2),
                handle_github_request(Event, Data, ContentType);
            [buildbot] -> handle_buildbot_request(Data, ContentType);
            _ -> io:format("Unexpected request (~p) ~p", [Opts, Req])
        end,
    chudbot_irc:announce(M),
    empty_reply(Req2).

handle_github_request(Event, Json, {<<"application">>, <<"json">>}) ->
    #{<<"sender">> := #{<<"login">> := Subject}} = Json,
    Object = case Json of
                 #{<<"repository">> := #{<<"name">> := X}} -> X;
                 #{<<"organization">> := #{<<"login">> := X}} -> X
             end,
    Desc = case Event of
               <<"push">> ->
                   [Subject, " pushed ", integer_to_list(length(maps:get(<<"commits">>, Json, []))),
                    " commits to ", Object];
               _ -> [Subject, $ , Event, "'d", " on ", Object]
           end,
    ["(github) " | Desc].

handle_buildbot_request([{<<"packets">>, ToDecode}],
                        {<<"application">>, <<"x-www-form-urlencoded">>}) ->
    ExtractEvent = fun (J) ->
                           case maps:get(<<"event">>, J, undefined) of
                               undefined -> "unknown event";
                               E -> binary_to_list(E)
                           end
                   end,
    Json = jiffy:decode(ToDecode, [return_maps]),
    ["(buildbot) ", string:join(lists:map(ExtractEvent, Json), ", ")].
