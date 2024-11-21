-module(t).
-export([main/1]).

main(Args) ->
    Id = fun (X) -> X end,
    Num = 42,
    Id(Num),
    ok.