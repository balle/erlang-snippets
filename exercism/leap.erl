-module(leap).
-export([is_leap_year/1]).

dividable_by(Input, Number) ->
    Rest = Input rem Number,

    if Rest == 0 ->
      true;
    Rest /= 0 ->
      false
    end.

dividable_by_four(Number) ->
    dividable_by(Number, 4).

is_leap_year(Year) ->
    dividable_by(Year, 4) and dividable_by(Year, 400) and dividable_by(Year, 100).





