-define(NEWSOCKET(Socket), {newsocket, Socket}).
-define(DELSOCKET(Handler), {delsocket, Handler}).

-define(NEWCLIENT(Nickname, Socket), {newclient, Nickname, Socket}).
-define(DELCLIENT(Nickname, Handler), {delclient, Nickname, Handler}).
