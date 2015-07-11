-module(chudbot_delayed_dispatch).
-export([start/2]).

start(Socket, Interval) ->
    Pid = spawn(fun () -> loop(Socket, Interval) end),
    fun(M) -> Pid ! M end.

loop(Socket, Interval) ->
    receive
        %% Flush messages first?
        terminate -> ok;
        X -> gen_tcp:send(Socket, X),
             timer:sleep(Interval + 10*iolist_size(X)),
             loop(Socket, Interval)
    end.
