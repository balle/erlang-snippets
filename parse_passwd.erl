-module(parse_passwd).
-export([read_passwd/0, parse_line/1]).

read_passwd() ->
    {ok, File} = file:open("/etc/passwd", [read]), 
    Data = get_lines(File),
    file:close(File),
    Data.

get_lines(File) ->
    Buffer = [],
    get_lines(File, Buffer).

get_lines(File, Buffer) ->
    Line = io:get_line(File, ""),

    if Line /= eof ->
        get_lines(File, Buffer ++ [string:strip(Line, right, $\n)]);
    true ->
        Buffer
    end.

parse_line(Line) ->
    [User, Pass, UID, GID, Comment, Home, Shell] = re:split(Line, ":"),
    io:format("User ~s UID ~s GID ~s Home ~s Shell ~s~n", [User, UID, GID, Home, Shell]).



