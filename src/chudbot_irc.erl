%%% @doc A minimal IRC client.  Just enough to connect, join a
%%%      channel, and send notices.
%%% @end
%%%
%%% Features intentionally omitted:
%%%  - multiple channels
%%%
%%% TODO:
%%%  - flood protection (queue notices and send_after)
%%%  - handle nick in use
%%%  - handle problem joining channel

-module(chudbot_irc).
-author('Julian Squires <julian@cipht.net>').
-behavior(gen_fsm).
-export([start_link/1,announce/1,shutdown/0]).
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3,code_change/4]).
-export([connecting/2,connected/2,idle/2]).
-export([parse_msg/1]).
%% tests
-export([sanitize_test/0]).

-define(DEFAULT_PORT, 6667).

-record(config,
        {
          server :: string(),
          port :: integer(),
          channel :: binary(),
          nick :: binary(),
          user :: string(),
          real_name :: string(),
          masters :: [binary()]
        }).
-record(state,
        {
          config :: #config{},
          sender,
          socket :: inet:socket(),
          nick :: binary()
        }).

%%% API

start_link(Config) ->
    gen_fsm:start_link({local,?MODULE}, ?MODULE, Config, []).

announce(Msg) ->
    gen_fsm:send_event(whereis(?MODULE), {notice, Msg}).

shutdown() ->
    gen_fsm:sync_send_all_state_event(whereis(?MODULE), stop).

%%% GEN_FSM BEHAVIOR

init(Config) ->
    {next_state, StateName, StateData} = connect(#state{config=config_of_plist(Config)}),
    {ok, StateName, StateData}.

connect(#state{config=Config}) ->
    {ok, Socket} = gen_tcp:connect(Config#config.server, Config#config.port,
                                   [binary,
                                    {packet, line},
                                    {active, true}]),
    Sender = chudbot_delayed_dispatch:start(Socket, 500),
    Nick = Config#config.nick,
    gen_tcp:send(Socket, [<<"NICK ">>, Nick, <<"\r\n">>]),
    gen_tcp:send(Socket, [<<"USER ">>, Config#config.user, <<" 8 * ">>,
                          Config#config.real_name, <<"\r\n">>]),
    {next_state, connecting, #state{config=Config, socket=Socket, sender=Sender, nick=Nick}}.

connecting({_, {welcome, _}}, S=#state{config=Config, sender=Send}) ->
    Send([<<"JOIN ">>, Config#config.channel, <<"\r\n">>]),
    {next_state, connected, S};
connecting({_, {nickname_in_use, _}}, S=#state{nick=Nick, sender=Send}) ->
    Send([<<"NICK ">>, Nick, <<"_\r\n">>]),
    {next_state, connecting, S#state{nick = <<Nick/bytes, <<"_">>/bytes>>}};
connecting(What, S) ->
    unexpected([connecting, What]),
    {next_state, connecting, S}.

connected({_, {join, _}}, S) ->
    {next_state, idle, S};
connected(What, S) ->
    unexpected([connected, What]),
    {next_state, connected, S}.

idle({notice, Msg}, S=#state{config=Config, sender=Send}) ->
    Channel = Config#config.channel,
    Msgs = sanitize(iolist_to_binary(Msg), max_msg_len(Channel)),
    lists:foreach(fun(M) -> Send(fmt_notice(Channel, M)) end,
                  Msgs),
    {next_state, idle, S};

idle(W={{user, Them, _}, {privmsg, Target, Msg}},
     S=#state{config=Config, sender=Send, nick=Nick}) ->
    NotMyMasterMsg = <<"You're not my master.">>,
    IsMaster = lists:member(Them, Config#config.masters),
    {MsgTarget, Command} = target_and_msg(Msg),
    if IsMaster, Target == Nick ->
            handle_command(Msg, Them, S);
       IsMaster, MsgTarget == Nick ->
            %% we assume Target == Config#config.channel
            handle_command(Command, Config#config.channel, S);
       Target == Nick ->
            Send(fmt_privmsg(Them, NotMyMasterMsg));
       Target == Config#config.channel, MsgTarget == Nick ->
            Send(fmt_privmsg(Target, [Them, <<": ">>, NotMyMasterMsg]));
       true ->
            unexpected([idle, W, Target, IsMaster, MsgTarget])
    end,
    {next_state, idle, S};

idle(What, S) ->
    unexpected([idle, What]),
    {next_state, idle, S}.

handle_event(Event, StateName, StateData) ->
    unexpected([handle_event, StateName, Event]),
    {stop, unexpected_event, StateData}.

handle_sync_event(shutdown, _From, _StateName, StateData) ->
    {stop, shutdown, StateData};
handle_sync_event(Event, From, StateName, StateData) ->
    unexpected([StateName, From, Event]),
    {stop, unexpected_event, StateData}.

handle_info({tcp, Socket, Recv}, StateName, StateData) ->
    Msg = parse_msg(Recv),
    case Msg of
        %% Handle a ping at any time.
        {_Prefix, {ping, Rest}} ->
            gen_tcp:send(Socket, [<<"PONG ">>, Rest, <<"\r\n">>]),
            {next_state, StateName, StateData};
        _ -> ?MODULE:StateName(Msg, StateData)
    end;

handle_info({tcp_closed, _Socket}, _StateName, StateData) ->
    F = StateData#state.sender,
    F(terminate),
    connect(StateData#state{socket=undefined, sender=undefined});

handle_info(What, StateName, StateData) ->
    unexpected([handle_info, StateName, What]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #state{socket=undefined}) ->
    ok;
terminate(Reason, _StateName, #state{socket=Socket, sender=Send}) ->
    Send(terminate),
    gen_tcp:send(Socket, io_lib:fwrite("QUIT :~p\r\n", [Reason])),
    gen_tcp:close(Socket),
    ok.

code_change(_OldVersion, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%% INTERNAL FUNCTIONS

unexpected(What) ->
    io:format("Unexpected: ~p~n", [What]).

%% defmacro opportunity
config_of_plist(Config) ->
    #config{server = proplists:get_value(server, Config),
            port = proplists:get_value(port, Config, ?DEFAULT_PORT),
            channel = binary:list_to_bin(proplists:get_value(channel, Config)),
            nick = binary:list_to_bin(proplists:get_value(nick, Config)),
            user = proplists:get_value(user, Config),
            real_name = proplists:get_value(real_name, Config),
            masters = lists:map(fun(M) -> binary:list_to_bin(M) end,
                                proplists:get_value(masters, Config))}.

strip_nl(M) when size(M) > 2 ->
    Len = size(M),
    case {binary:at(M, Len-2), binary:at(M, Len-1)} of
        {$\r,$\n} -> binary:part(M, 0, Len-2);
        {_, $\n} -> binary:part(M, 0, Len-1);
        _ -> M
    end;
strip_nl(<<$\n>>) -> <<>>;
strip_nl(M) -> M.


parse_msg(<<":", Msg/bytes>>) ->
    %% read prefix til space
    [Prefix,Rest] = binary:split(strip_nl(Msg), <<" ">>),
    %% determine if this is a user or a server; if it contains a !
    case binary:split(Prefix, <<"!">>) of
        [Server] -> {{server, Server}, parse_kwd(Rest)};
        [Nick, User] -> {{user, Nick, User}, parse_kwd(Rest)}
    end;
parse_msg(Msg) -> {none, parse_kwd(strip_nl(Msg))}.

parse_kwd(Msg) ->
    [Keyword,Rest] = binary:split(Msg, <<" ">>),
    case Keyword of
        <<"JOIN">> -> {join, Rest};
        <<"PING">> -> {ping, Rest};
        <<"PRIVMSG">> -> parse_privmsg(privmsg, Rest);
        <<"NOTICE">> -> parse_privmsg(notice, Rest);
        <<"001">> -> {welcome, Rest};
        <<"002">> -> {your_host, Rest};
        <<"003">> -> {created, Rest};
        <<"004">> -> {my_info, Rest};
        <<"005">> -> {bounce, Rest};
        %% -->8-- omitted --8<--
        <<"331">> -> {no_topic, Rest};
        <<"332">> -> {topic, Rest};
        <<"263">> -> {try_again, Rest};
        <<"403">> -> {no_such_channel, Rest};
        <<"404">> -> {cannot_send_to_channel, Rest};
        <<"432">> -> {erroneous_nickname, Rest};
        <<"433">> -> {nickname_in_use, Rest};
        <<"4",_/bytes>> -> {error, Keyword, Rest};
        <<"5",_/bytes>> -> {error, Keyword, Rest};
        _ -> {ignored, Keyword, Rest}
    end.

parse_privmsg(Kwd, Rest) ->
    [Destination, M] = binary:split(Rest, <<" ">>),
    possible_ctcp(Kwd, Destination, M).

possible_ctcp(Kwd, Destination, M) when byte_size(M) > 2 ->
    case {binary:at(M, 0), binary:at(M, size(M)-1)} of
        {1,1} ->
            {ctcp, Destination, binary:part(M, 1, size(M)-2)};
        _ -> {Kwd, Destination, M}
    end;
possible_ctcp(Kwd, Destination, M) ->
    {Kwd, Destination, M}.

split1(Bin, Size, Rest) when byte_size(Bin) > Size ->
    H = binary:part(Bin, {0, Size}),
    T = binary:part(Bin, {Size, byte_size(Bin)-Size}),
    split1(T, Size, [H | Rest]);
split1(Bin, _Size, Rest) -> [Bin | Rest].

split_by_size(Bin, Size) ->
    lists:reverse(split1(Bin, Size, [])).


%% Sanitization of an incoming message; we want to remove newlines and
%% break long messages into smaller pieces.
-spec sanitize(binary(), integer()) -> [iolist()].
sanitize(Msg, MaxLen) ->
    Chunks = split_by_size(Msg, MaxLen),
    lists:map(fun (X) -> binary:split(X, [<<$\r>>,<<$\n>>,<<"\r\n">>], [global,trim]) end,
              Chunks).

max_msg_len(Channel) ->
    502 - byte_size(Channel).

sanitize_test() ->
    [[<<"foo">>,<<"bar">>]] = sanitize(<<"foo\r\nbar">>, 510),
    [[]] = sanitize(<<"\n">>, 510),
    [[<<0:3240>>], [<<0:3240>>], [<<0:1712>>]] = sanitize(<<0:8192>>, 405),
    ok.

target_and_msg(<<":", Rest/bytes>>) ->
    target_and_msg(Rest);
target_and_msg(M) ->
    case binary:split(M, <<": ">>) of
        [_NoTarget] -> {none, M};
        [Target, Msg] -> {Target, Msg}
    end.

fmt_notice(Channel, Msg) ->
    [<<"NOTICE ">>, Channel, <<" :">>, Msg, <<"\r\n">>].

fmt_privmsg(Channel, Msg) ->
    [<<"PRIVMSG ">>, Channel, <<" :">>, Msg, <<"\r\n">>].

address(Target, #state{sender=Send}, Msg) ->
    Send(fmt_privmsg(Target, Msg)).

handle_command(<<"pomodoro">>, _Target, _State) ->
    chudbot_pomodoro:start();
handle_command(Command, Target, State) ->
    address(Target, State, io_lib:fwrite("Not sure how to handle ~p", [Command])).
