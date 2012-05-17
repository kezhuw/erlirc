-module(erlirc_client_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	ClientSpec = {erlirc_client, {erlirc_client, start_link, []}, temporary, 2000, worker, []},
	{ok, {{simple_one_for_one, 0, 0}, [ClientSpec]}}.
