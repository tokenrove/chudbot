-module(chudbot_sup).
-behavior(supervisor).
-export([start_link/2]).
-export([init/1]).

start_link(IrcConfig, WebHooksConfig) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [IrcConfig, WebHooksConfig]).

init([IrcConfig, WebHooksConfig]) ->
    Dispatch = cowboy_router:compile(proplists:get_value(dispatch, WebHooksConfig)),
    Port = proplists:get_value(port, WebHooksConfig),
    Children = [{irc, {irc, start_link, [IrcConfig]},
                permanent, 2000, worker, [irc]},
                {http, {cowboy, start_http,
                        [http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]]},
                 permanent, 2000, worker, [cowboy]}],
    RestartStrategy = {one_for_one, 10, 30},
    {ok, {RestartStrategy, Children}}.
