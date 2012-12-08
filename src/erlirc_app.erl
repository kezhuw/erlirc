-module(erlirc_app).

-behavior(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    Env = application:get_all_env(),
    Ports = proplists:get_value(ports, Env, [6667]),
    NAcceptor = proplists:get_value(nacceptor, Env, 20),
    erlirc_sup:start_link([{ports, Ports}, {nacceptor, NAcceptor}]).

stop(_State) ->
    ok.
