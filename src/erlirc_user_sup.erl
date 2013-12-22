-module(erlirc_user_sup).
-export([start_link/0]).
-export([new/2]).

-behaviour(supervisor).
-export([init/1]).

-define(NAME, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?NAME}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 30, 3600},
            [{erlirc_user,
                    {erlirc_user, start_link, []},
                    temporary, 1200, worker,
                    [erlirc_user]}]}}.

new(Socket, Infos) ->
    {ok, Pid} = supervisor:start_child(?NAME, [[{socket, Socket} | Infos]]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! {shoot, Socket},
    ok.
