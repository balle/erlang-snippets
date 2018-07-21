-module(log_parser).
-include_lib("stdlib/include/qlc.hrl").
-include("log.hrl").
-export([init_db/0, start/1, show_all_logs/0, parse_line/1, persist_data/0, process_file/1]).

-spec start(Filename::string()) -> none().
-spec show_all_logs() -> list().
-spec match_to_string(Line::string(), Matches::list()) -> list().


init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(log, [{attributes, record_info(fields, log)}]),
    mnesia:stop().


start(File) ->
    %%mnesia:start(),
    process_flag(trap_exit, true),

    %% Mar 17 16:23:19 writeordie /bsd: /tmp force dirty (dangling 164 inflight 0)
    %% May  9 22:33:35 writeordie pkg_add: Added chocolate-doom-3.0.0
    {ok, MatchSyslog} = re:compile("^(?<Month>\\w{3})\\s{1,2}(?<Day>\\d{1,2})\\s(?<Hour>\\d{2}):(?<Minute>\\d{2}):(?<Second>\\d{2})\\s(?<Host>\\w+?)\\s(?<Command>.+?)(\\[(?<Pid>\\d+?)\\])?:\\s(?<Message>.+)$"),

    keep_alive(node(), syslog_parser, parse_line, [MatchSyslog]),
    keep_alive(node(), data_persister, persist_data, []),

    spawn(log_parser, process_file, [File]).

    %%mnesia:sync_log,
    %%mnesia:stop().


on_exit(Pid, Fun) ->
    spawn(fun() ->
        Ref= monitor(process, Pid),
	receive
	    {'DOWN', Ref, process, Pid, Why} -> Fun(Why)
	end
    end).


keep_alive(Node, Name, Fun, Args) ->
    register(Name, Pid = spawn(Node, log_parser, Fun, Args)),
    on_exit(Pid, fun(Why) ->
        error_logger:error_msg("Process ~s [~s] died: ~s~n", [Name, Pid, Why]), 
        keep_alive(Node, Name, Fun, Args) end).


process_file(File) ->
    lists:foreach(fun(Line) -> whereis(syslog_parser) ! Line end, read_file(File)).   
    

parse_line(Match) ->
    Persister_PID = whereis(data_persister),

    receive
        Line ->        
            case re:run(Line, Match) of
                {match, ParsedLine} -> Persister_PID ! match_to_string(Line, ParsedLine);
        	nomatch -> error_logger:warning_msg("NOMATCH: ~s~n", [Line])
            end,
    
            parse_line(Match)
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


persist_data() ->
    receive
        [Month, Day, Hour, Minute, Second, Host, Command, Pid, _, Message] ->
            error_logger:info_msg("Writing ~s ~s ~s ~s ~s ~s ~s ~s ~s~n[", [Month, Day, Hour, Minute, Second, Host, Command, Pid, Message]),
            F = fun() -> 
                Row = #log{month=Month, day=Day, hour=Hour, minute=Minute, second=Second, host=Host, command=Command, pid=Pid, message=Message},
        	mnesia:write(Row)
            end,	
      	    mnesia:transaction(F),
    	    persist_data()
    end.


show_all_logs() ->
    SelectAll = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(log)])) end,
    {atomic, Val} = mnesia:transaction(SelectAll),
    Val.
