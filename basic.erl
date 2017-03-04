-module(basic).
-export([mirror/1, number/1, word_counter/1, count_to_ten/0, count_to_ten/1]).

mirror(Anything) -> Anything.

number(one) -> 1;
number(two) -> 2;
number(three) -> 3.

% recursive count words
list_len([]) -> 0;

list_len(List) ->
    [_|Tail] = List,
    1 + list_len(Tail).

word_counter(Sentence) ->
    List = re:split(Sentence, " "),
    list_len(List).


% recursive count to 10
count_to_ten() ->
  count_to_ten(1).

count_to_ten(Val) when Val < 10 ->
  io:format("~w ", [Val]),
  count_to_ten(Val + 1);

count_to_ten(_) ->
  io:format("10~n", []).
