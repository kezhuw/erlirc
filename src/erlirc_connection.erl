-module(erlirc_connection).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(UNDEFINED, 0).
-define(INITED, 1).
-define(PASSED, 2).
-define(NAMED, 3).
-define(DONE, 4).

-record(state, {
        socket                      :: inet:socket(),
        status      = ?UNDEFINED    :: integer(),
        nickname    = <<"*">>       :: binary(),
        username    = <<>>          :: binary(),
        usermode    = 0             :: integer(),
        realname    = <<>>          :: binary()
    }).

-define(TIMEOUT, 30000).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    {ok, Server} = application:get_env(server),
    put(server_prefix, list_to_binary([":", Server, " "])),
    erlang:send_after(?TIMEOUT, self(), timeout),
    {ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
    {reply, unexpected_request, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Packet}, #state{status=Status, socket=Socket} = State) ->
    Result = case erlirc_message:ignore_prefix(Packet) of
        {'PASS', [_Password|_]} when Status =:= ?INITED  ->
            {continue, State#state{status=?PASSED}};
        {'NICK', [Nickname]} ->
            if
                Status >= ?NAMED ->
                    {continue, State#state{nickname=Nickname}};
                true ->
                    {continue, State#state{status=?NAMED, nickname=Nickname}}
            end;
        {'USER', [Username, Usermode, _Unused, Realname |_]} ->
            case Status of
                ?NAMED ->
                    {user, State#state{
                            status = ?DONE,
                            username = Username,
                            usermode = Usermode,
                            realname = Realname}};
                _ ->
                    Message = erlirc_message:notice(<<"*">>, "unexpected USER command before NICK\r\n"),
                    gen_tcp:send(Socket, Message),
                    {continue, State}
            end;
        {Command, _ } when Command =:= 'SERVER'; Command =:= 'SERVICE' ->
            Message = erlirc_message:notice(
                State#state.nickname,
                ["unimplemented command ", atom_to_list(Command), "\r\n"]),
            gen_tcp:send(Socket, Message),
            {continue, State};
        _ ->
            Message = erlirc_message:notice(
                State#state.nickname,
                ["unsupported message in registration: ", Packet]),
            gen_tcp:send(Socket, Message),
            {continue, State}
    end,
    case Result of
        {continue, State1} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State1};
        {user, State1} ->
            erlirc_user_sup:new(Socket, [
                    {nickname, State1#state.nickname},
                    {username, State1#state.username},
                    {usermode, State1#state.usermode},
                    {realname, State1#state.realname}]),
            {stop, normal, State1}
    end;
handle_info({shoot, Socket}, #state{socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}, {packet, line}, {packet_size, 512}]),
    {noreply, State#state{status=?INITED}};
handle_info({tcp_error, Socket, Reason}, #state{socket=Socket} = State) ->
    {stop, {shutdown, {tcp_error, Reason}}, State};
handle_info({tcp_closed, Socket}, #state{socket=Socket} = State) ->
    {stop, {shutdown, tcp_closed}, State};
handle_info(timeout, State) ->
    {stop, {shutdown, timeout}, State};
handle_info(Msg, State) ->
    error_logger:error_msg("~s received unexpected message: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #state{socket=Socket}) ->
    case Reason of
        normal ->
            ok;
        _ ->
            gen_tcp:close(Socket)
    end.
