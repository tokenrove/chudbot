%% @private
-module(chudbot_app).
-author('Julian Squires <julian@cipht.net>').
-behaviour(application).
-export([start/2,stop/1]).

-spec start(_Type, _StartArgs) -> {'ok', pid()} | {'error', _}.
start(_Type, _StartArgs) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, IrcConfig} = file:consult(filename:join([code:priv_dir(App), "irc.conf"])),
    {ok, WebHooksConfig} = file:consult(filename:join([code:priv_dir(App), "webhooks.conf"])),
    %% lists:foreach(fun code:ensure_loaded/1, []),
    {ok, Irc} = irc:start(IrcConfig),
    {ok, WebHooks} = webhooks:start(Irc, WebHooksConfig),
    chudbot_sup:start_link().

-spec stop(_State) -> ok.
stop(_State) -> ok.
