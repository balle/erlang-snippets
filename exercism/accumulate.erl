-module(accumulate).
-export([accumulate/2]).

accumulate([], _) -> [];
accumulate([Head|Tail], Fun) -> [Fun(Head)|accumulate(Tail, Fun)].

