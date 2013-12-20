-module(erlirc_connection_sup).
-export([start_link/0, new/1]).

-behaviour(supervisor).
-export([init/1]).

-define(SELF, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SELF}, ?MODULE, {}).

init(_) ->
    {ok, {{simple_one_for_one, 30, 3600},
          [{connection,
            {erlirc_connection, start_link, []},
            temporary,
            1200,
            worker,
            [erlirc_connection]}]}}.

new(Socket) ->
    {ok, Pid} = supervisor:start_child(?SELF, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! {shoot, Socket},
    ok.
