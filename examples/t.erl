-module(t).
-export([main/1]).

main(Args) ->
    Test = 34 =:= 35,
    Test,
    ok.