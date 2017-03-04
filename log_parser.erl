-module(log_parser).
-export([parse_syslog/1]).
-include_lib("stdlib/include/qlc.hrl").
-include("log.hrl").

parse_syslog(Line) ->
    %% Feb 26 13:30:14 openbsd apmd: cannot set hw.perfpolicy
    %% Feb 26 13:32:56 openbsd pulseaudio[14084]: [(null)] main.c: Daemon startup failed.
    {ok, MatchSyslog} = re:compile("^(?<Month>\w{3}) (?<Day>\d{2}) (?<Hour>\d{2}):(?<Minute>\d{2}):(?<Second>\d{2}) (?<Host>\w+?) (<Command>\w+?)(\[(?<Pid>\d+?)\])?: (<?Message>.+)$"),
    {match, ParsedLine} = re:run(Line, MatchSyslog),
    persist_data(ParsedLine).

persist_data([Month, Day, Hour, Minute, Second, Host, Command, Pid, Message]) ->
    Row = #log{month=Month, day=Day, hour=Hour, minute=Minute, second=Second, host=Host, command=Command, pid=Pid, message=Message},
    mnesia:write(Row).

    
