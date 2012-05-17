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

handle_call({newclient, Nickname, Socket} = Cmd, _From, #state{users=Users, hmanager=Manager} = State) ->
	case ets:lookup(Users, Nickname) of
		[] ->
			ets:insert(Users, {Nickname, placeholder}),
			{Handler, Manager1} = erlirc_hmanager:retain_handler(Manager),
			gen_tcp:controlling_process(Socket, Handler),
			gen_server:cast(Handler, Cmd),
			{reply, ok, State#state{hmanager=Manager1}};
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
	{Handler, Manager1} = erlirc_hmanager:retain_handler(Manager),
	gen_tcp:controlling_process(Socket, Handler),
	gen_server:cast(Handler, Cmd),
	{noreply, State#state{hmanager=Manager1}}; 
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
