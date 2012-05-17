-module(erlirc_client_handler).
-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state,
	{clients,
	 server,
	 self
	}).

-include("erlirc_define.hrl").

start_link(?NEWSOCKET(_Socket) = Cmd, Server) ->
	gen_server:start_link(?MODULE, {Cmd, Server}, []).

init({?NEWSOCKET(Socket), Server}) ->
	Dict = dict:from_list([{Socket, placeholder}]),
	{ok, #state{clients=Dict, server=Server}}.

handle_call(_Cmd, _From, State) ->
	{reply, ok, State}.

handle_cast(?NEWSOCKET(Socket), #state{clients=Clients} = State) ->
	Clients1 = dict:store(Socket, placeholder, Clients),
	%% TODO application level keepalive
	inet:setopts(Socket, [{active, once}, {keepalive, true}, {nodelay, true}]),
	{noreply, State#state{clients=Clients1}}.

handle_info({tcp_closed, Socket}, State = #state{clients = Clients, server = Server}) ->
	Clients1 = try dict:fetch(Socket, Clients) of
		placeholder ->
			gen_server:cast(Server, ?DELSOCKET(self())),
			dict:erase(Socket, Clients);
		Nickname ->
			gen_server:cast(Server, ?DELCLIENT(Nickname, self())),
			dict:erase(Socket, Clients)
	catch
		_:_ ->
			Clients
	end,
	{noreply, State#state{clients=Clients1}};
handle_info({keepalive, _Socket}, State) ->
	{noreply, State}.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, #state{clients=Clients, server=Server}) ->
	delete_clients(dict:to_list(Clients), Server).

delete_clients([{_Socket, placeholder}|T], Server) ->
	gen_server:cast(Server, ?DELSOCKET(self())),
	delete_clients(T, Server);
delete_clients([{_Socket, Nickname}|T], Server) ->
	gen_server:cast(Server, ?DELCLIENT(Nickname, self())),
	delete_clients(T, Server);
delete_clients([], _Server) ->
	done.
