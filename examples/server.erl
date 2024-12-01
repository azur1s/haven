-module(server).
-export([main/1]).

server() ->
    {ok, LSock} = gen_tcp:listen(3000, [binary, {packet, 0}, {active, false}]),
    loop(LSock).

loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = do_recv(Sock, []),
    ok = gen_tcp:close(Sock),
    io:format("~s~n", [Bin]),
    loop(LSock).

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

main(_) ->
    io:format("Server started~n"),
    server().