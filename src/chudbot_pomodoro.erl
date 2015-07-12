-module(chudbot_pomodoro).
-export([start/0]).

start() ->
    case whereis(?MODULE) of
        undefined ->
            Pid = spawn(fun () -> loop(not_started) end),
            register(?MODULE, Pid),
            Pid ! start;
        Pid -> Pid ! start
    end.

loop(not_started) ->
    receive
        start ->
            chudbot_irc:say("Started a 25 minute pomodoro."),
            loop({pomodoro, erlang:send_after(25*60*1000, self(), done)});
        terminate -> ok
    end;
loop(State={pomodoro, TRef}) ->
    receive
        start ->
            chudbot_irc:say("A pomodoro is already running (" ++
                                pp_timer(TRef) ++ " remaining)."),
            loop(State);
        done ->
            chudbot_irc:say("Time's up!  Take a five minute break."),
            loop({break, erlang:send_after(5*60*1000, self(), done), false});
        terminate ->
            chudbot_irc:say("Interrupting a pomodoro."),
            ok
    end;
loop({break, TRef, Queued}) ->
    receive
        start ->
            chudbot_irc:say("It's still break time (" ++
                                pp_timer(TRef) ++
                                " remaining), but I've queued up another pomodoro."),
            loop({break, TRef, true});
        done ->
            chudbot_irc:say("Break's over!"),
            case Queued of
                true -> self() ! start;
                _ -> ok
            end,
            loop(not_started);
        terminate ->
            chudbot_irc:say("Interrupting a break."),
            ok
    end.

pp_timer(T) ->
    case erlang:read_timer(T) of
        false -> "none";
        I when is_integer(I) ->
            io_lib:fwrite("~.10B:~2.10.0B.~4.10.0B",
                          [I div (60*1000), (I div 1000) rem 60, I rem 1000])
    end.
