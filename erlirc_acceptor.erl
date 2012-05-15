-module(erlirc_acceptor).

-export([start_link/2]).

-include("erlirc_define.hrl").

start_link(Listener, Server) when is_pid(Server) ->
	%% Pid = spawn_link(?MODULE, acceptor, [Listener, Server]),
	Pid = spawn_link(fun() -> accept(Listener, Server) end),
	{ok, Pid}.

accept(Listener, Server) ->
	case gen_tcp:accept(Listener) of
		{ok, Socket} ->
			ok = gen_tcp:controlling_process(Socket, Server),
			Server ! ?NEWSOCKET(Socket),
			accept(Listener, Server);
		{error, _Reason} ->
			%% supervisor will restart it
			ok
	end.
