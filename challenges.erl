-module(challenges).
-export([last_but_one/1, find_last_but_one/1]).

% geklaut
last_but_one([]) ->
     "undefined";
last_but_one([_|[]]) -> "undefined";
last_but_one([P,_|[]]) -> P;
last_but_one([_,S|T]) -> last_but_one([S|T]).


% selbst geschrieben - klappt net
find_last_but_one(List) ->
    [X|Tail] = List,
    case length(Tail) of
	1 -> X;
        _ -> find_last_but_one(List)
    end.
