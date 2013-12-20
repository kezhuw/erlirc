-module(erlirc_sup).
-export([start_link/1]).

-behavior(supervisor).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(Args) ->
    {_, Ports} = proplists:lookup(ports, Args),
    {_, NAcceptor} = proplists:lookup(nacceptor, Args),
    Specs = [
        {erlirc_connection_sup,
            {erlirc_connection_sup, start_link, []},
            permanent, infinity, supervisor,
            [erlirc_connection_sup]},
        {erlirc_listener_sup,
            {erlirc_listener_sup, start_link, [Ports, NAcceptor]},
            permanent, infinity, supervisor,
            [erlirc_listener_sup]}
    ],
    {ok, {{rest_for_one, 30, 3600}, Specs}}.
