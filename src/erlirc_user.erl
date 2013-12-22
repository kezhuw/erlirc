-module(erlirc_user).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(user, {
        nickname,
        username,
        usermode,
        realname,
        channels        = [],
        server,
        socket
        }).

start_link(Infos) ->
    gen_server:start_link(?MODULE, Infos, []).

init(Infos) ->
    try user_info(Infos) of
        {ok, User} ->
            {ok, User,  1000}
    catch
        _:_ -> {stop, ill}
    end.

user_info(Infos) ->
    Socket = proplists:get_value(socket, Infos),
    Nickname = binary:copy(proplists:get_value(nickname, Infos)),
    Username = binary:copy(proplists:get_value(username, Infos)),
    Realname = binary:copy(proplists:get_value(realname, Infos)),
    Usermode = case catch binary_to_integer(proplists:get_value(usermode, Infos)) of
        {'EXIT', _Error} ->
            0;
        Value ->
            Value
    end,
    {ok, Server} = application:get_env(server),
    put(server_prefix, list_to_binary([":", Server, " "])),
    {ok, #user{
            socket = Socket,
            server = Server,
            nickname = Nickname,
            username = Username,
            usermode = Usermode,
            realname = Realname
        }}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({message, Message}, #user{socket = Socket} = User) ->
    gen_tcp:send(Socket, Message),
    {noreply, User};
handle_info({message, Nickname, Message}, #user{socket = Socket,
                                                nickname = Nickname} = User) ->
    gen_tcp:send(Socket, Message),
    {noreply, User};
handle_info({tcp, Socket, Packet},
            #user{socket = Socket, nickname = Nickname} = User) ->
    [Message] = binary:split(Packet, <<"\r\n">>, [trim]),
    io:format("received from ~s: ~p~n", [Nickname, Message]),
    gen_tcp:send(Socket, Packet),
    inet:setopts(Socket, [{active, once}]),
    {noreply, User};
handle_info({tcp_closed, Socket}, #user{socket = Socket} = User) ->
    {stop, {shutdown, tcp_closed}, User};
handle_info({tcp_error, Socket, Reason}, #user{socket = Socket} = User) ->
    {stop, {shutdown, {tcp_error, Reason}}, User};
handle_info({shoot, Socket}, #user{socket = Socket} = User) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, User};
handle_info(Msg, User) ->
    error_logger:error_msg("~s received unexpected message: ~p~n", [?MODULE, Msg]),
    {noreply, User}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #user{socket = Socket}) ->
    case Reason of
        normal ->
            ok;
        _ ->
            gen_tcp:close(Socket)
    end.
