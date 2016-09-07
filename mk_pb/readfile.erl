-module(readfile).

-export([run/1]).

 

run(Name) -> io:format("~w~n",[file:read_file(Name)]).
