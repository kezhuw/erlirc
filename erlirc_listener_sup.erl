-module(erlirc_listener_sup).
-behaviour(supervisor).

-export([start_link/2]).

%% callback exports
-export([init/1]).

-define(ACCEPTOR_MODULE, erlirc_acceptor).

start_link(Server, NAcceptor) when is_integer(NAcceptor), NAcceptor >= 1 ->
	supervisor:start_link(?MODULE, {Server, NAcceptor}).

init({Server, NAcceptor}) ->
	IRCPort = 6667,
	{ok, Listener} = gen_tcp:listen(IRCPort, [{active, false}, {packet, line}]),
	Acceptors = [child_spec(Seqi, Listener, Server)
			|| Seqi <- lists:seq(1, NAcceptor)],
	{ok, {{one_for_one, 30, 3600}, Acceptors}}.

child_spec(Seqi, Listener, Server) ->
	Func = {?ACCEPTOR_MODULE, start_link, [Listener, Server]},
	Id = {?ACCEPTOR_MODULE, Seqi},
	{Id, Func, permanent, brutal_kill, worker, []}.
