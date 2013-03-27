-module(erlirc_incoming).
-export([start_link/1]).

-behaviour(gen_fsm).
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         code_change/4,
         terminate/3]).
-export([idle/2]).

start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

init(Socket) ->
    {ok, idle, Socket, 0}.

idle(timeout, Socket) ->
    receive
        {shoot, Socket} ->
            inet:setopts(Socket, [{active, once}, {packet, line}])
    end,
    {next_state, ready, Socket}.

handle_info({tcp, Socket, Packet}, ready, Socket) ->
    io:format("Recv: ~p~n", [Packet]),
    inet:setopts(Socket, [{active, once}]),
    {next_state, ready, Socket};
handle_info({tcp_error, Socket, _Why} = Reason, _StateName, Socket) ->
    {stop, {shutdown, Reason}, Socket};
handle_info({tcp_closed, Socket} = Reason, _, Socket) ->
    {stop, {shutdown, Reason}, Socket}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, non_sync_event_handler, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, Socket) ->
    gen_tcp:close(Socket).
