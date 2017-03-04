-module(translate).
-export([loop/0]).

loop() ->
    receive
	"muh" ->
	    io:format("Its a cow talking~n"),
	    loop();
	"maeh" ->
	    io:format("Got sheep?~n"),
	    loop();
	_ ->
	    io:format("Huh?~n"),
	    loop()
     end.
