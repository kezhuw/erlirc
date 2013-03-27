{application, erlirc,
 [{description, "IRC server deamon written in Erlang"},
  {vsn, "0.0.0"},
  {modules, [erlirc_app, erlirc_sup,
             erlirc_listener_sup, erlirc_listener, erlirc_acceptor,
	     erlirc_incoming_sup, erlirc_incoming
	    ]},
  {applications, [kernel, stdlib]},
  {env, [
         {ports, [6667]},
	 {nacceptor, 20}
	]},
  {mod, {erlirc_app, []}}
 ]}.
