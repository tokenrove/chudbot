%% @private
-module(chudbot_app).
-author('Julian Squires <julian@cipht.net>').
-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, IrcConfig} = file:consult(filename:join([code:priv_dir(App), "irc.config"])),
    {ok, WebHooksConfig} = file:consult(filename:join([code:priv_dir(App), "webhooks.config"])),
    application:ensure_all_started(cowboy),
    lists:foreach(fun code:ensure_loaded/1, [webhooks]),
    chudbot_sup:start_link(IrcConfig, WebHooksConfig).

stop(_State) -> ok.
