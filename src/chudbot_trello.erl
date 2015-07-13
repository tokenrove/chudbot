-module(chudbot_trello).
-export([handle_request/2, start/0, list_boards/1, subscribe_webhook/3]).

%%% Trello API stuff

start() ->
    application:ensure_all_started(ssl),
    inets:start().

list_boards(Config) ->
    Key = proplists:get_value(key, Config),
    Token = proplists:get_value(token, Config),
    Uri = lists:flatten(["https://trello.com/1/members/my/boards?key=", Key, "&token=", Token]),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(Uri),
    lists:map(fun (M) -> {maps:get(<<"name">>, M), maps:get(<<"id">>,M)} end,
              jiffy:decode(Body, [return_maps])).

subscribe_webhook(Model, Config, BaseUrl) ->
    Key = proplists:get_value(key, Config),
    Token = proplists:get_value(token, Config),
    ContentType = "application/json",
    Uri = lists:flatten(["https://trello.com/1/tokens/", Token, "/webhooks/?key=", Key]),
    Body = jiffy:encode(#{description => "Chudbot watching boards",
                          'callbackURL' => (BaseUrl ++ "/chudbot/trello"),
                          'idModel' => Model}),
    {ok, {{_, Status, _}, _, _}} = httpc:request(post, [Uri, [], ContentType, Body], [], []),
    Status.

%%% Dealing with notifications

handle_request(Json, {<<"application">>, <<"json">>}) ->
    #{<<"action">> := Action, <<"model">> := Model} = Json,
    io_lib:fwrite("(trello) ~s: ~s ~s",
                  [maps:get(<<"name">>, Model),
                   pp_who_dunnit(Action),
                   pp_action(Action)]).

pp_who_dunnit(#{<<"memberCreator">> := #{<<"fullName">> := N}}) -> N;
pp_who_dunnit(_) -> "unknown".

pp_action(#{<<"data">> := Data, <<"type">> := Type}) ->
    Card = case Data of
               #{<<"card">> := #{<<"name">> := N}} -> N;
               _ -> undefined
           end,
    pp_action(Type, Data, Card).

pp_action(S, _Data, undefined) ->
    [S, "'d"];
pp_action(<<"updateCheckItemStateOnCard">>, #{<<"checkItem">> := Item}, Card) ->
    #{<<"name">> := Name, <<"state">> := Verb} = Item,
    [Verb, "d ", $\", Name, $\", " on ", $\", Card, $\"];
pp_action(<<"addMemberToCard">>, #{<<"idMember">> := Name}, Card) ->
    ["added ",Name," to ",$\",Card,$\"];
pp_action(<<"removeMemberFromCard">>, #{<<"idMember">> := Name}, Card) ->
    ["removed ",Name," from ",$\",Card,$\"];
pp_action(<<"createCard">>, _Data, Card) ->
    ["created ",$\",Card,$\"];
pp_action(<<"updateCard">>, #{<<"listAfter">> := #{<<"name">> := After},
                              <<"listBefore">> := _},
          Card) ->
    ["moved ",$\",Card,$\", " to ", After];
pp_action(<<"updateCard">>, _Data, Card) ->
    ["updated ",$\",Card,$\"];
pp_action(S, _Data, Card) ->
    [S, "'d", " on ", $\",Card,$\"].
