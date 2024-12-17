-module(server).
-export([main/1, loop/1, do_recv/2, recv/2, close/1, accept/1, listen/1, println/1, l_to_b/1]).

l_to_b(Bs) ->
  list_to_binary(Bs).
println(Msg) ->
  io:format("~s~n", [Msg]).
listen(Port) ->
  gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]).
accept(Sckt) ->
  gen_tcp:accept(Sckt).
close(Sckt) ->
  gen_tcp:close(Sckt).
recv(Sckt, Len) ->
  gen_tcp:recv(Sckt, Len).
do_recv(Sckt, Bs) ->
  {Res1, B2} = recv(Sckt, 0),
  Ok3 = ok,
  _if4 = Res1 == Ok3,
  if
    _if4 == true -> do_recv(Sckt, B2);
    true -> {Ok3, l_to_b(Bs)}
  end.
loop(Srvr) ->
  {_1, Sckt2} = accept(Srvr),
  {_3, Bin4} = do_recv(Sckt2, []),
  close(Sckt2),
  println(Bin4),
  loop(Srvr).
server() ->
  {_1, Srvr2} = listen(3000),
  loop(Srvr2).
main(_) ->
  println("Server started").