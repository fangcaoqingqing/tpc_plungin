-module(ts_qmsg).
-author('neiyong@staff.weibo.com').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_qmsg.hrl").

-include("ts_macros.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         subst/2,
         parse/2,
         parse_bidi/2,
         dump/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%% @spec decode_buffer(Buffer::binary(),Session::record(raw)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer, #qmsg_request{}) ->
    Buffer.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #qmsg_request{}.
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request
%% Args:    #jabber
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#qmsg_request{uid = StrUid, data = Data}, #state_rcv{session = S})->
    _MsgBin = list_to_binary(Data),
    ?LOGF("get_message ~p",Data,?DEB),
%    {ok,F}=file:read_file("/root/tsung_plugin_demo/mk_pb/nclogin",read),
%    {ok,F}=file:read_file("/root/tsung_plugin_demo/mk_pb/nclogin"),
    F = <<10,24,53,55,98,53,53,52,99,50,101,52,98,48,53,55,97,48,53,101,49,48,52,53,52,56,18,64,52,69,84,73,105,71,119,80,67,76,120,98,117,99,67,76,110,82,99,81,89,85,117,122,87,71,76,53,113,73,113,49,65,107,69,55,100,48,80,50,119,84,67,51,85,114,66,50,77,69,103,51,88,78,97,73,52,109,83,57,87,88,110,79,24,1>>,
    ?LOGF("get_message F~p",F,?DEB),
    Uid = case is_list(StrUid) of
              true -> list_to_integer(StrUid);
              false -> StrUid
          end,
    ReqBody = <<Uid:32/little, F/binary>>,
    BodyLen = byte_size(ReqBody),
    ReqBin = <<BodyLen:32/little, ReqBody/binary>>,
    AllLen = byte_size(ReqBin),
    TranBin = <<AllLen:32/little, ReqBody/binary>>,
    io:format("ts_qmsg.erl  get_messge  ReqBin!!!~w~n",[ReqBin]),
    ?LOGF("qmsg_request encode result : ~p", [{"ts_qmsg.erl  get_message   Uid, Data, "}], ?DEB),
    {TranBin, S}.

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: Parse the response data
%% Returns: {NewState, Opts, Close}
%%----------------------------------------------------------------------
parse(closed, State) ->
    ?LOGF("qmsg_response got closed", [], ?DEB),
    {State#state_rcv{ack_done = true, datasize = 0}, [], true};
parse(<<Len:32/big, LeftBin/binary>> = Data, State = #state_rcv{datasize = DataSize}) ->
    io:format("data:~w~n",[Data]),
    io:format("datasize:~w~n",[DataSize]),
    <<Uid:32/integer, Random:32/integer>> = LeftBin,

    ?LOGF("qmsg_response decode result : ~p", [{"ts_qmsg.erl  parse **##", Uid, Random, "##**"}], ?DEB),

    AckResult =
    case Random of
        Num when Num > 0 ->
            true;
        0 ->
            false
    end,

    NewDataSize = DataSize + Len + 4,
    {State#state_rcv{ack_done = AckResult, acc = [], datasize = NewDataSize}, [], false};
parse(Data, State) ->
    %%io:format("ts_qmsg.erl parse :parse Data ~w~n",[Data]),
    %%io:format("ts_qmsg.erl parse :parse State ~w~n",[State]),
    ?LOGF("qmsg_response got unmatched data : ~p", [Data], ?DEB),
    {State, [], false}.

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

dump(A,B) ->
    ts_plugin:dump(A,B).

parse_config(Element, Conf) ->
    ts_config_qmsg:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(_,[], Param, _Host) ->
    Param;
add_dynparams(true, {DynVars, _Session}, OldReq, _Host) ->
    subst(OldReq, DynVars);
add_dynparams(_Subst, _DynData, Param, _Host) ->
    Param.

%%----------------------------------------------------------------------
%% Function: subst/1
%%----------------------------------------------------------------------
subst(Req = #qmsg_request{uid = Uid, data = Data}, DynVars) ->
    NewData = ts_search:subst(Data, DynVars),
    NewUid = ts_search:subst(Uid, DynVars),
    ?LOGF("subst data result : ~p", [{NewUid, NewData}], ?DEB),
    Req#qmsg_request{uid = NewUid, data = NewData}.
