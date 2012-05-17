-module(erlirc_hmanager).
-export([new/1, new/2, retain_handler/1, release_handler/2]).

-record(manager, {
	creator,
	maximum,
	idle = [],
	hmap = dict:new()
	}).

-type manager() :: #manager{}.
-type handler() :: term().

-spec new(Creator) -> Manager when
	Creator :: function(),
	Manager :: manager().

new(Creator) ->
	new(Creator, 50).

-spec new(Creator, Maximum) -> Manager when
	Creator :: function(),
	Maximum :: integer(),
	Manager :: manager().

new(Creator, Maximum)
	when is_function(Creator), is_integer(Maximum), Maximum >= 1 ->
	#manager{creator=Creator, maximum=Maximum}.

-spec retain_handler(Manager) ->
	{ok, Handler, Manager1} | {error, Reason} when
	Manager :: manager(),
	Manager1 :: manager(),
	Handler :: handler(),
	Reason :: term().

retain_handler(#manager{creator=Creator, maximum=Maximum, idle=[], hmap=Hmap} = Manager) ->
	case Creator() of
		{ok, Handler} ->
			Hmap1 = dict:store(Handler, Maximum-1, Hmap),
			Idle = if
				Maximum =:= 1 ->
					[];
				true ->
					[Handler]
			end,
			{ok, Handler, Manager#manager{idle=Idle, hmap=Hmap1}};
		{error, _Reason} = Error ->
			Error;
		Error ->
			{error, Error}
	end;
retain_handler(#manager{idle=[Handler|Ridle]=Idle, hmap=Hmap} = Manager) ->
	N = dict:fetch(Handler, Hmap) - 1,
	Hmap1 = dict:store(Handler, N, Hmap),
	Idle1 = case N of
		0 ->
			Ridle;
		_ ->
			Idle
	end,
	{Handler, Manager#manager{idle=Idle1, hmap=Hmap1}}.

-spec release_handler(Handler, Manager) -> Manager1 when
	Handler :: term(),
	Manager :: manager(),
	Manager1 :: manager().

release_handler(Handler, #manager{idle=Idle, hmap=Hmap} = Manager) ->
	case dict:fetch(Handler, Hmap) of
		0 ->
			Idle1 = [Handler|Idle],
			Hmap1 = dict:store(Handler, 1, Hmap),
			Manager#manager{idle=Idle1, hmap=Hmap1};
		N ->
			Hmap1 = dict:store(Handler, N+1, Hmap),
			Manager#manager{hmap=Hmap1}
	end.
