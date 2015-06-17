
init(Config, Irc) ->
    Dispatch = proplists:get_value(dispatch, Config),
    Port = proplists:get_value(port, Config),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{dispatch, Dispatch}]),
    ???.
