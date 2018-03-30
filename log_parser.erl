-module(log_parser).
-include_lib("stdlib/include/qlc.hrl").
-include("log.hrl").
-export([init_db/0, start/1]).

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(log, [{attributes, record_info(fields, log)}]),
    mnesia:stop().

start(File) ->
    mnesia:start(),
    Data = read_file(File),
    ParseSyslog = fun(Line) ->
        %% Mar 17 16:23:19 writeordie /bsd: /tmp force dirty (dangling 164 inflight 0)
        %% Mar 17 16:25:57 writeordie /bsd: urtwn0 detached
        {ok, MatchSyslog} = re:compile("^(?<Month>\\w{3})\\s(?<Day>\\d{2})\\s(?<Hour>\\d{2}):(?<Minute>\\d{2}):(?<Second>\\d{2})\\s(?<Host>\\w+?)\\s(?<Command>.+?)(\[(?<Pid>\\d+?)\])?:\\s(?<Message>.+)$"),

        case re:run(Line, MatchSyslog) of
            {match, ParsedLine} -> persist_data(ParsedLine);
	    nomatch -> []
        end
    end,

    lists:foreach(ParseSyslog, Data),
    mnesia:stop().

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

%% TODO: mnesia:write fails with "no transaction"
persist_data([_, Month, Day, Hour, Minute, Second, Host, Command, Pid, Message]) ->
    F = fun() -> 
        Row = #log{month=Month, day=Day, hour=Hour, minute=Minute, second=Second, host=Host, command=Command, pid=Pid, message=Message},
	mnesia:write(Row)
    end,	
    mnesia:transaction(F).



