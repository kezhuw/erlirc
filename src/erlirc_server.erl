-module(erlirc_server).
-export([new_connection/1]).

new_connection(Socket) ->
    Pid = spawn(
        fun() ->
            receive
                shoot ->
                    io:format("Serving Connection: ~p~n", [Socket]),
                    loop(Socket)
            end
        end),
    gen_tcp:controlling_process(Socket, Pid),
    Pid ! shoot,
    ok.

loop(Socket) ->
    inet:setopts(Socket, [{active, true}, {packet, line}]),
    receive
        {tcp, Socket, Msg} ->
            io:format("Recv: ~p~n", [Msg]),
            ?MODULE:loop(Socket);
        {tcp_error, Socket, Reason} ->
            io:format("Error: ~p~n", [Reason]);
        {tcp_closed, Socket} ->
            io:format("Closed: ~p~n", [Socket])
    end.
