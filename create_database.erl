-module(create_database).
-include_lib("stdlib/include/qlc.hrl").
-include("log.hrl").
-export([init/0]).

init() ->
    mnesia:create_table(log, [{attributes, record_info(fields, log)}]).
