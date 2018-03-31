-module(log_parser).
-include_lib("stdlib/include/qlc.hrl").
-include("log.hrl").
-export([init_db/0, start/1, show_all_logs/0]).

-spec start(Filename::string()) -> none().
-spec show_all_logs() -> list().
-spec match_to_string(Line::string(), Matches::list()) -> list().
-spec parse_syslog(Line::string()) -> list() | erlang:throw(nomatch).


init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(log, [{attributes, record_info(fields, log)}]),
    mnesia:stop().


start(File) ->
    %%mnesia:start(),
    lists:foreach(fun(Line) -> try persist_data( parse_syslog(Line) )
                               catch
                                   throw:nomatch -> io:format("NOMATCH: ~s~n", [Line])
                               end
		  end, read_file(File)).   
    %%mnesia:sync_log,
    %%mnesia:stop().


parse_syslog(Line) ->
    %% Mar 17 16:23:19 writeordie /bsd: /tmp force dirty (dangling 164 inflight 0)
    %% Mar 17 16:25:57 writeordie /bsd: urtwn0 detached
    {ok, MatchSyslog} = re:compile("^(?<Month>\\w{3})\\s(?<Day>\\d{2})\\s(?<Hour>\\d{2}):(?<Minute>\\d{2}):(?<Second>\\d{2})\\s(?<Host>\\w+?)\\s(?<Command>.+?)(\\[(?<Pid>\\d+?)\\])?:\\s(?<Message>.+)$"),

    case re:run(Line, MatchSyslog) of
        {match, ParsedLine} -> match_to_string(Line, ParsedLine);
	 nomatch -> throw(nomatch)
    end.


resolve_match(_, {-1, 0}) ->
    "";

resolve_match(Line, {Start, Length}) ->
    string:sub_string(Line, Start+1, Start+Length).


match_to_string(Line, [_|MatchSyslog]) ->
    ResolveMatch = fun(Match) -> resolve_match(Line, Match) end,
    [ResolveMatch(X) || X <- MatchSyslog].


read_file(File) ->
    {ok, FileHandle} = file:open(File, [read]), 
    Data = get_lines(FileHandle),
    file:close(FileHandle),
    Data.


get_lines(FileHandle) ->
    Buffer = [],
    get_lines(FileHandle, Buffer).

get_lines(FileHandle, Buffer) ->
    Line = io:get_line(FileHandle, ""),

    if Line /= eof ->
        get_lines(FileHandle, Buffer ++ [string:strip(Line, right, $\n)]);
    true ->
        Buffer
    end.


persist_data([_, Month, Day, Hour, Minute, Second, Host, Command, Pid, Message]) ->
    io:format("Writing ~s ~s ~s ~s ~s ~s ~s ~s ~s~n[", [Month, Day, Hour, Minute, Second, Host, Command, Pid, Message]),
    F = fun() -> 
        Row = #log{month=Month, day=Day, hour=Hour, minute=Minute, second=Second, host=Host, command=Command, pid=Pid, message=Message},
	mnesia:write(Row)
    end,	
    mnesia:transaction(F).


show_all_logs() ->
    SelectAll = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(log)])) end,
    {atomic, Val} = mnesia:transaction(SelectAll),
    Val.
