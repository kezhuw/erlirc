-module(erlirc_hmanager).
-export([new/1, new/2, retain_handler/1, release_handler/2]).

-record(manager, {
	creator,
	maximum,
	idle = [],
	hmap = dict:new()
	}).

new(Creator) ->
	new(Creator, 50).

new(Creator, Maximum)
	when is_function(Creator), is_integer(Maximum), Maximum >= 1 ->
	#manager{creator=Creator, maximum=Maximum}.

retain_handler(#manager{creator=Creator, maximum=Maximum, idle=[], hmap=Hmap} = Manager) ->
	Handler = Creator(),
	N = Maximum-1,
	Hmap1 = dict:store(Handler, N, Hmap),
	Idle = case N of
		0 ->
			[];
		_ ->
			[Handler]
	end,
	{Handler, Manager#manager{idle=Idle, hmap=Hmap1}};
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
