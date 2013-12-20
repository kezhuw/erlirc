-module(erlirc_acceptor).
-export([start_link/1]).

start_link(Listener) ->
    Pid = proc_lib:spawn_link(fun() -> loop(Listener) end),
    {ok, Pid}.

loop(Listener) ->
    {ok, Socket} = gen_tcp:accept(Listener),
    erlirc_connection_sup:new(Socket),
    ?MODULE:loop(Listener).
