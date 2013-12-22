-module(erlirc_message).
-export([parse/1,
         ignore_prefix/1,
         notice/2]).

-export([commands/0]).

commands() ->
    [
        'PASS', 'NICK', 'USER', 'SERVER', 'SERVICE', 'QUIT', 'SQUIT',
        'JOIN', 'PART', 'MODE', 'TOPIC', 'NAMES', 'LIST', 'INVITE', 'KICK',
        'PRIVMSG', 'NOTICE'
    ].

parse(<<"$:", Packet1/binary>> = Packet) when is_binary(Packet) ->
    case binary:split(Packet1, <<"$ ">>) of
        [Prefix, Packet2] ->
            [Packet3] = binary:split(Packet2, <<"\r\n">>, [trim]),
            case noprefix_parse(Packet3) of
                {error, _} = Error ->
                    Error;
                {Command, Parameters} ->
                    {Prefix, Command, Parameters}
            end;
        _ ->
            {error, <<"empty message">>}
    end;
parse(Packet) when is_binary(Packet) ->
    [Packet1] = binary:split(Packet, <<"\r\n">>, [trim]),
    noprefix_parse(Packet1).

ignore_prefix(Packet) ->
    case parse(Packet) of
        {error, _} = Error ->
            Error;
        {_Prefix, Command, Parameters} ->
            {Command, Parameters};
        {_Command, _Parameters} = Message ->
            Message
    end.

noprefix_parse(Packet) ->
    [Command, Packet1] = binary:split(Packet, <<$ >>),
    try binary_to_existing_atom(Command, utf8) of
        CmdAtom ->
            {CmdAtom, params(Packet1)}
    catch
        error:badarg ->
            {error, [<<"unrecognized command: ">>, Command]}
    end.

params(Packet) ->
    case binary:split(Packet, <<" :">>) of
        [Packet] ->
            re:split(Packet, <<" +">>, [trim, {return, binary}]);
        [Init, Tail] ->
            re:split(Init, <<" +">>, [trim, {return, binary}]) ++ [Tail]
    end.

notice(Nickname, Msg) ->
    [get(server_prefix), Nickname, " :", Msg].
