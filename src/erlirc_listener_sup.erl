-module(erlirc_listener_sup).
-export([start_link/2]).

-behaviour(supervisor).
-export([init/1]).

start_link(Ports, NAcceptor) when Ports =/= [], is_integer(NAcceptor), NAcceptor >= 1 ->
    supervisor:start_link(?MODULE, [Ports, NAcceptor]).

init([Ports, NAcceptor]) ->
    Listeners = [listener_spec(Port, NAcceptor) || Port <- Ports],
    {ok, {{one_for_one, 30, 3600}, Listeners}}.

listener_spec(Port, NAcceptor) ->
    {{listener, Port},
     {erlirc_listener, start_link, [Port, NAcceptor]},
     permanent, infinity, supervisor,
     [erlirc_listener]}.
