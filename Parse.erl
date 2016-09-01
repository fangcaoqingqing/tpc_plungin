-module(Parse).
-export([parse/2]).
parse(<<Len:32/little, LeftBin:Len/binary>> = Data, State = #state_rcv{datasize = DataSize}) ->
    io:format("datasize:~w~n",[DataSize]),
    <<Uid:32/integer, Random:32/integer>> = LeftBin,
    io:format("qmsg_response decode result : ~p", [{"ts_qmsg.erl  parse **##", Uid, Random, "##**"}])，
