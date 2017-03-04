-module(buy).
-export([price_per_item/2, calc_sum/2]).

%Pricelist = [
%  {apple, 0.5},
%  {milk, 1.2},
%  {bread, 3.5},
%  {beer, 1.0}
%].

%Buylist = [
%  {1, bread},
%  {3, milk},
%  {6, beer}
%].


price_per_item(Item, Pricelist) -> lists:last([V || {K, V} <- Pricelist, K == Item]).

calc_sum(Shoppings, Pricelist) ->
    lists:foldl(fun(X, Sum) -> X + Sum end,
    0,
    [ Amount * price_per_item(Item, Pricelist) || {Amount, Item} <- Shoppings]).
