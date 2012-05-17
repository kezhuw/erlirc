-module(erlirc_client_srv).
-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("erlirc_define.hrl").
-include("erlirc_record.hrl").

-define(TABNAME, erlirc_user_table_name).

-record(state, {
	users,
	hmanager
	}).

start_link(Creator, Maximum) ->
	gen_server:start_link(?MODULE, {Creator, Maximum}, []).

init({Creator, Maximum}) ->
	{ok, #state{users=ets:new(?TABNAME, [set, private]),
			hmanager=erlirc_hmanager:new(Creator, Maximum)}}.

transfer_client(?NEWCLIENT(Nickname, Socket) = Cmd, Users, Manager) ->
	case erlirc_hmanager:retain_handler(Manager) of
		{ok, Handler, Manager1} ->
			ets:insert(Users, {Nickname, placeholder}),
			gen_tcp:controlling_process(Socket, Handler),
			gen_server:cast(Handler, Cmd),
			{ok, Manager1};
		{error, Reason} ->
			gen_tcp:close(Socket),
			{Reason, Manager}
	end.

handle_call({newclient, Nickname, _Socket} = Cmd, _From, #state{users=Users, hmanager=Manager} = State) ->
	case ets:lookup(Users, Nickname) of
		[] ->
			{Result, Manager1} = transfer_client(Cmd, Users, Manager),
			{reply, Result, State#state{hmanager=Manager1}};
		_ ->
			{reply, conflict, State}
	end;
handle_call({nickname, Nickname}, _From, #state{users=Users} = State) ->
	case ets:lookup(Users, Nickname) of
		[] ->
			ets:insert(Users, {Nickname, placeholder}),
			{reply, ok, State};
		_ ->
			{reply, conflict, State}
	end;
handle_call({getclient, Nickname}, _From, #state{users=Users} = State) ->
	case ets:lookup(Users, Nickname) of
		[] ->
			{reply, nonexist_nickname, State};
		[placeholder] ->
			{reply, uninitialized_nickname, State};
		[#state{} = Userinf] ->
			{reply, Userinf, State}
	end.

handle_cast(?NEWSOCKET(Socket) = Cmd, #state{hmanager=Manager} = State) ->
	case erlirc_hmanager:retain_handler(Manager) of
		{ok, Handler, Manager1} ->
			gen_tcp:controlling_process(Socket, Handler),
			gen_server:cast(Handler, Cmd),
			{noreply, State#state{hmanager=Manager1}};
		{error, _Reason} ->
			gen_tcp:close(Socket),
			{noreply, State}
	end;
handle_cast({delsocket, Handler}, #state{hmanager=Manager} = State) ->
	Manager1 = erlirc_hmanager:release_handler(Handler, Manager),
	{noreply, State#state{hmanager=Manager1}}; 
handle_cast({setclient, Nickname, Userinf}, #state{users=Users} = State) ->
	case ets:lookup(Users, Nickname) of
		[] ->
			{stop, nonexist_nickname, State};
		_ ->
			ets:insert(Users, {Nickname, Userinf}),
			{noreply, State}
	end; 
handle_cast({delclient, Nickname, Handler}, #state{users=Users, hmanager=Manager} = State) ->
	ets:delete(Users, Nickname),
	Manager1 = erlirc_hmanager:release_handler(Handler, Manager),
	{noreply, State#state{hmanager=Manager1}}.

handle_info(_Info, State) ->
	{ok, State}.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, #state{users=Users}) ->
	ets:delete(Users).
