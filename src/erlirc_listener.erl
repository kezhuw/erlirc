-module(erlirc_listener).
-export([start_link/2]).

-behavior(supervisor).
-export([init/1]).

start_link(Port, NAcceptor) ->
    supervisor:start_link(?MODULE, [Port, NAcceptor]).

init([Port, NAcceptor]) ->
    io:format("Try listen: ~p~n", [Port]),
    {ok, Listener} = gen_tcp:listen(Port, [{active, false}]),
    io:format("Listening on: ~p~n", [Port]),
    Acceptors = [acceptor_spec(Listener, Port, I)
        || I <- lists:seq(1, NAcceptor)],
    {ok, {{one_for_one, 30, 3600}, Acceptors}}.

acceptor_spec(Listener, Port, I) ->
    {{acceptor, Port, I},
     {erlirc_acceptor, start_link, [Listener]},
     permanent, brutal_kill, worker,
     [erlirc_acceptor]}.
