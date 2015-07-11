-module(chudbot_trello).
-export([handle_request/2]).

handle_request(Body, _Req) ->
    Json = jiffy:decode(Body, [return_maps]),
    Action = maps:get(<<"action">>, Json),
    Model = maps:get(<<"model">>, Json),
    io_lib:fwrite("trello: ~s: ~s ~s",
                  [maps:get(<<"name">>, Model),
                   pp_who_dunnit(Action),
                   pp_action(Action)]).

subscribe_webhook(Model, Key, Token, BaseUrl) ->
    ContentType = "application/json",
    Uri = lists:flatten(["https://trello.com/1/tokens/", Token, "/webhooks/?key=", Key]),
    Body = jiffy:encode(#{description => "Chudbot watching boards",
                          'callbackURL' => (BaseUrl ++ "/chudbot/trello"),
                          'idModel' => Model}),
    {ok, {{_, Status, _}, _, _}} = httpc:request(post, [Uri, [], ContentType, Body], [], []),
    Status.

pp_who_dunnit(Action) ->
    maps:get(<<"fullName">>, maps:get(<<"memberCreator">>, Action)).

pp_action(Action) ->
    pp_action(maps:get(<<"type">>, Action), maps:get(<<"data">>, Action)).

pp_action(<<"updateCheckItemStateOnCard">>, Data) ->
    Card = maps:get(<<"name">>, maps:get(<<"card">>, Data)),
    Item = maps:get(<<"checkItem">>, Data),
    Name = maps:get(<<"name">>, Item),
    Verb = [maps:get(<<"state">>, Item), "d"],
    [Verb, " ", $\", Name, $\", " on ", $\", Card, $\"];
pp_action(<<"addMemberToCard">>, Data) ->
    Card = maps:get(<<"name">>, maps:get(<<"card">>, Data)),
    Name = maps:get(<<"idMember">>, Data),
    ["added ",Name," to ",$\",Card,$\"];
pp_action(<<"removeMemberFromCard">>, Data) ->
    Card = maps:get(<<"name">>, maps:get(<<"card">>, Data)),
    Name = maps:get(<<"idMember">>, Data),
    ["removed ",Name," from ",$\",Card,$\"];
pp_action(<<"createCard">>, Data) ->
    Card = maps:get(<<"name">>, maps:get(<<"card">>, Data)),
    ["created ",$\",Card,$\"];
pp_action(<<"updateCard">>, Data) ->
    Card = maps:get(<<"name">>, maps:get(<<"card">>, Data)),
    ["updated ",$\",Card,$\"];
pp_action(S, Data) ->
    Card = maps:get(<<"name">>, maps:get(<<"card">>, Data)),
    [S, "'d", " on ", $\",Card,$\"].
